library(shiny)
library(ggplot2)
library(tibble)
library(dplyr)
library(purrr)
library(magrittr)
library(modelr)
library(DBI)
library(RPostgreSQL)
library(stringr)

dbConn <- dbConnect(RPostgreSQL::PostgreSQL(), dbname = "upisdb_lite", host=Sys.getenv("PG_HOST"), port=5432,
                    user=Sys.getenv("PG_USER"), password=Sys.getenv("PG_PASS"))

source("dbFunctions.R")


standardize <- function(var, m=0, stdev=1) {
  m+stdev*(var-mean(var, na.rm=TRUE))/(sd(var, na.rm=TRUE))
}

drop.na <- function(data) data[!is.na(data)]

server <- function(input, output, session) {

  svef <- reactive({
    input$text_size  # we want to refresh all plots when this property changes
    tryCatch({
      if(input$tabs == "joins") {
        finalFilter <- if(input$base_filter == "") "true" else input$base_filter
        if(!is.null(input$facet_filter_val) && input$facet_filter_var != "") {
          finalFilter <- paste0("(", finalFilter, ") AND (", input$facet_filter_var, " IN (",
                                paste(paste0("'", input$facet_filter_val, "'"), collapse=", "), "))")
          result <- dbGetColumn(c(input$var_x, input$var_y, input$facet_col, input$facet_filter_var), finalFilter)
        } else {
          result <- dbGetColumn(c(input$var_x, input$var_y, input$facet_col), finalFilter) %>% add_column(facet="")
        }
        colnames(result) <- c("x", "y", "color", "facet")
      } else {
        if(str_match(input$var_x, "2...") == str_match(input$var_y, "2...")) {
          result <- dbGetColumn(c(input$var_x, input$var_y), input$base_filter)
        } else {
          colx <- dbGetColumn(input$var_x, input$base_filter)[[1]]
          coly <- dbGetColumn(input$var_y, input$base_filter)[[1]]
          length(colx) <- max(length(colx), length(coly))
          length(coly) <- max(length(colx), length(coly))
          result <- data.frame(x=colx, y=coly)
        }
        colnames(result) <- c("x", "y")
      }
      if(input$x_standardize) result$x %<>% standardize()
      if(input$y_standardize) result$y %<>% standardize()
      tibble(result)
    },
      warning = function(w) {
        print(w)
        tibble()
      } ,
      error = function(e) {
        print(e)
        tibble()
      }
    )
  })
  svef %<>% debounce(750)

  output$x_stats_text <- renderText({
    data <- svef()
    paste("Srednja vrednost:", mean(data$x, na.rm = TRUE), "<br>Standardna devijacija:", sd(data$x, na.rm = TRUE))
  })
  output$common_stats_text <- renderText({
    data <- svef()
    count.x <- sum(!is.na(data$x))
    count.y <- sum(!is.na(data$y))
    paste("<b>Ukupno tačaka:", count.x, if(count.x != count.y) paste0("/ ", count.y) else "", "</b>")
  })
  output$y_stats_text <- renderText({
    data <- svef()
    paste("Srednja vrednost:", mean(data$y, na.rm = TRUE), "<br>Standardna devijacija:", sd(data$y, na.rm = TRUE))
  })


  output$facet <- renderPlot({
    data <- svef()
    if(count(data) == 0) return(ggplot(data))

    col <- input$facet_col
    pos <- if(input$facet_jitter) "jitter" else "identity"
    ggplot(data, aes(x, y, color=color)) + geom_point(alpha=input$facet_alpha, position=pos) +
      facet_grid(rows = "facet") +
      guides(colour = guide_legend(override.aes = list(alpha = 1, size = 3))) + scale_color_discrete(name=col) +
      scale_color_viridis_d(option = "turbo") + xlab(input$var_x) + ylab(input$var_y)
  })
  output$facet.ui <- renderUI(plotOutput("facet", height=input$plot_height))

  output$boxplot <- renderPlot({
    data <- svef()
    if(count(data) == 0) return(ggplot(data))

    if(data$x %>% unique %>% length > input$boxplot_intervals) {
      x_factors <- data$x %>% cut(input$boxplot_intervals)
    } else {
      x_factors <- data$x %>% round(digits = 2) %>% as.factor
    }
    ggplot(data, aes(x_factors, y)) + geom_boxplot(outlier.alpha = input$boxplot_outlier_alpha,
                                                      varwidth = input$boxplot_varwidth) +
      facet_grid(rows = "facet") + guides(colour = guide_legend(override.aes = list(alpha = 1))) +
      scale_color_discrete(name=col) + xlab(input$var_x) + ylab(input$var_y)
  })
  output$boxplot.ui <- renderUI(plotOutput("boxplot", height=input$plot_height))

  output$distributions <- renderPlot({
    data <- svef()
    ggplot(data) + geom_freqpoly(aes(x, color=input$var_x), binwidth=input$distributions_binwidth) +
      geom_freqpoly(aes(y, color=input$var_y), binwidth=input$distributions_binwidth) + scale_color_discrete(name = "Varijabla")
  })
  output$distributions.ui <- renderUI(plotOutput("distributions", height = input$plot_height))

  output$ks_text <- renderText({
    data <- svef()
    test <- ks.test(drop.na(data$x), drop.na(data$y))

    counts <- colSums(!is.na(data))
    paired.wilcox <- counts[1] == counts[2]
    wilcox <- wilcox.test(data$x, data$y, paired = paired.wilcox, conf.int = TRUE)
    paste("<b>Statistike</b><br>D =", test$statistic[[1]], "<br>", "p ", if(test$p.value == 0) "< 1e-16" else paste("=", test$p.value),
          '<hr style="margin:4px;border-top:1px solid #bbb">W = ', wilcox$statistic[[1]],
          "<br>p ", if(test$p.value == 0) "< 1e-16" else paste("=", test$p.value))
  })

  output$distributions1s <- renderPlot({
    data <- svef()
    f <- paste0("r", input$ks1s_dist)
    args <- list(count(data)[[1]], input$ks1s_param1, input$ks1s_param2)
    length(args) <- length(formals(f))
    dist <- data.frame(x=do.call(f, args))
    ggplot(data) + geom_freqpoly(aes(x, color=input$var_x), binwidth=input$distributions1s_binwidth) +
      geom_freqpoly(aes(x, color=input$ks1s_dist), data=dist, binwidth=input$distributions_binwidth) + scale_color_discrete(name = "Varijabla")
  })
  output$distributions1s.ui <- renderUI(plotOutput("distributions1s", height = input$plot_height))

  output$ks1s_text <- renderText({
    data <- svef()
    test <- ks.test(data$x, paste0("p", input$ks1s_dist), input$ks1s_param1, input$ks1s_param2)
    paste("<b>Statistike</b><br>D =", test$statistic[[1]], "<br>", "p", if(test$p.value == 0) "< 1e-16" else paste("=", test$p.value))
  })

  # This is where model is built
  model <- reactive({
    if(input$tabs == "models") { # We'll build a model only if tab 3 is showing
      data <- svef()
      lm(y ~ x, data)
    } else {
      NA
    }
  })

  output$model_regline <- renderPlot({
    data <- svef()
    if(count(data) == 0) return(ggplot(data))

    pos <- if(input$model_rl_jitter) "jitter" else "identity"
    grid <- data %>% data_grid(x) %>% add_predictions(model())

    ggplot(data, aes(x, y)) + geom_point(alpha=input$model_rl_alpha, position=pos) +
      geom_line(aes(grid[[1]], grid[[2]]), data=grid, color="red", size=1.5) +
      xlab(input$var_x) + ylab(input$var_y)
  })
  output$model_regline.ui <- renderUI(plotOutput("model_regline", height=input$plot_height))

  output$regression_table <- renderTable(bind_cols(Values=c("Intercept", "x"), summary(model())$coefficients))
    
  output$regression_text <- renderText({
    summ <- summary(model())
    paste("R^2 =", summ$r.squared)
  })

  output$model_freqpoly <- renderPlot({
    data <- svef() %>% add_residuals(model())
    if(count(data) == 0) return(ggplot(data))

    ggplot(data, aes(resid)) + geom_freqpoly(binwidth=input$model_freq_binwidth)
  })
  output$model_freqpoly.ui <- renderUI(plotOutput("model_freqpoly", height=input$plot_height))

  output$model_resid <- renderPlot({
    data <- svef() %>% add_residuals(model())
    if(count(data) == 0) return(ggplot(data))

    pos <- if(input$model_res_jitter) "jitter" else "identity"
    ggplot(data, aes(x, resid)) + geom_ref_line(h=0) +
      geom_point(position=pos, alpha=input$model_res_alpha) + xlab(input$var_x)
  })
  output$model_resid.ui <- renderUI(plotOutput("model_resid", height=input$plot_height))

  output$model_diagplots <- renderPlot({
    model <- model()
    par(mfrow=c(2,2))
    textScale <- input$text_size/2
    plot(model, cex.lab=textScale, cex.axis=textScale-0.2, cex.main=textScale, cex.sub=textScale, cex.id = textScale, cex.caption = textScale)
  })
  output$model_diagplots.ui <- renderUI(plotOutput("model_diagplots", height = 2*input$plot_height))

  observe({
    join_choices <- dbColumnValues(input$facet_filter_var)
    updateSelectInput(session, "facet_filter_val", choices = join_choices, selected = join_choices[1])
  })

  observe({
    textElement <- element_text(size=rel(input$text_size))
    smallTextElement <- element_text(size=rel(input$text_size-0.3))
    theme_update(text = textElement, axis.title.x = textElement, axis.title.y = textElement, legend.title = textElement,
                 legend.text = smallTextElement, axis.text.x = smallTextElement, axis.text.y = smallTextElement,
                 legend.key.height = unit(input$text_size/80, "npc"), legend.key.width = unit(input$text_size/120, "npc"))
  })

  # this triggers every time btn_swap is changed
  observeEvent(input$btn_swap, {
    selx <- isolate(input$var_x) # if we isolate input variable, this block won't react on it
    sely <- isolate(input$var_y)
    cols <- allNumericColumns()
    updateSelectizeInput(session, "var_x", choices = cols, selected = sely)
    updateSelectizeInput(session, "var_y", choices = cols, selected = selx)
  })
}

ui <- fluidPage(

  # Application title
  titlePanel("Upis u srednje škole"),

  fluidRow(
    column(5,
           selectizeInput("var_x", "X", choices = allNumericColumns(), selected = "ucenici2020.matematika_p"),
           checkboxInput("x_standardize", "Standardizovano?"),
           htmlOutput("x_stats_text")),
    column(2,
           actionButton("btn_swap", "Zameni", style="margin-top: 26px"),
           htmlOutput("common_stats_text")),
    column(5,
           selectizeInput("var_y", "Y", choices = allNumericColumns(), selected = "ucenici2020.matematika"),
           checkboxInput("y_standardize", "Standardizovano?"),
           htmlOutput("y_stats_text"))
  ),

  fluidRow(
    column(12, textAreaInput("base_filter", "Filter", width="100%", rows=1))
  ),

  tabsetPanel(id="tabs",

              tabPanel("Grafikoni", value="joins",
                       fluidRow(
                         wellPanel(style="min-height:110px",
                           tags$div(class="row-fluid",
                             tags$div(class="col-sm-6",
                               selectInput("facet_filter_var", "Odvoji grafikone po", choices = allCharacterColumns(), selected = "smerovi2020.podrucje")
                             ),
                             tags$div(class="col-sm-6",
                               selectInput("facet_filter_val", "za vrednosti", choices = dbColumnValues("smerovi2020.podrucje"), selected = "gimnazija", multiple = TRUE)
                             )
                           )
                         )
                       ),
                       h3("Box plot"),
                       fluidRow(
                         column(9, uiOutput("boxplot.ui", height = 600)),
                         column(3, wellPanel(
                           sliderInput("boxplot_outlier_alpha", "Transparentnost štrčaka (outlier alpha):",
                                       min = 0, max = 1, value = 0.07),
                           sliderInput("boxplot_intervals", "Maksimalan broj intervala", min=2, max = 30, step=1, value = 20),
                           checkboxInput("boxplot_varwidth", "Proporcionalna širina", value = TRUE),
                         ))
                       ),
                       h3("Scatter plot"),
                       fluidRow(
                         column(9, uiOutput("facet.ui", height = 600)),
                         column(3, wellPanel(
                           sliderInput("facet_alpha", "Transparentnost (alpha):",
                                       min = 0, max = 1, value = 0.07),
                           checkboxInput("facet_jitter", "Jitter", value = TRUE),
                           selectInput("facet_col", "Boja", choices = allCharacterColumns(), selected = "smerovi2020.podrucje")
                         ))
                       )
              ),

              tabPanel("Testovi saglasnosti", value="ks",
                       h3("Jedan uzorak"),
                       fluidRow(
                         column(9, uiOutput("distributions1s.ui", height = 600)),
                         column(3, wellPanel(
                           textInput("ks1s_dist", "Distribucija", value = "norm"),
                           sliderInput("ks1s_param1", "Parametar 1", min=0, max = 15, step=0.1, value = 2.5),
                           sliderInput("ks1s_param2", "Parametar 2", min=0, max = 15, step=0.1, value = 1),
                           sliderInput("distributions1s_binwidth", "Binwidth", min = 0.01, max = 5, step=0.01, value = 0.33),
                           htmlOutput("ks1s_text")
                         ))
                       ),

                       h3("Dva uzorka"),
                       fluidRow(
                         column(9, uiOutput("distributions.ui", height = 600)),
                         column(3, wellPanel(
                           sliderInput("distributions_binwidth", "Binwidth", min = 0.01, max = 5, step=0.01, value = 0.33),
                           htmlOutput("ks_text")
                         ))
                       )
              ),

              tabPanel("Linearna regresija", value="models",

                       h3("Regresiona linija"),

                       fluidRow(
                         column(9, uiOutput("model_regline.ui")),
                         column(3,  wellPanel(
                           sliderInput("model_rl_alpha", "Transparentnost (alpha):",
                                       min = 0, max = 1, value = 0.04),
                           checkboxInput("model_rl_jitter", "Jitter", value = TRUE),
                           tableOutput("regression_table"),
                           htmlOutput("regression_text")
                         ))
                       ),

                       h3("Dijagnostički plotovi"),
                       fluidRow(
                         column(12, uiOutput("model_diagplots.ui"))
                       ),

                       h3("Broj reziduala"),

                       fluidRow(
                         column(9, uiOutput("model_freqpoly.ui")),
                         column(3, wellPanel(
                           sliderInput("model_freq_binwidth", "Binwidth",
                                       min = 0.01, max = 3, value = 0.33)
                         ))
                       ),

                       h3("Distribucija reziduala"),

                       fluidRow(
                         column(9, uiOutput("model_resid.ui")),
                         column(3, wellPanel(
                           sliderInput("model_res_alpha", "Transparentnost (alpha):",
                                       min = 0, max = 1, value = 0.05),
                           checkboxInput("model_res_jitter", "Jitter", value = TRUE)
                         ))
                       )
              )
  ),

  sliderInput("plot_height", "Visina plotova", 100, 1600, 400),
  sliderInput("text_size", "Veličina teksta", 1, 7, 4, 0.1)
)

shinyApp(ui = ui, server = server, options = list(host="0.0.0.0", port=4000, display.mode="showcase"))
