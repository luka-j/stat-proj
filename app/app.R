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

dbConn <- dbConnect(RPostgreSQL::PostgreSQL(), dbname = "upisdb_lite", host="localhost", port=5432,
                    user=Sys.getenv("PG_USER"), password=Sys.getenv("PG_PASS"))

source("app/dbFunctions.R")


standardize <- function(var, m=0, stdev=1) {
  m+stdev*(var-mean(var))/(sd(var))
}

#
# =======================================================
# STEP 2: DOING STUFF
# =======================================================
#

server <- function(input, output, session) {

  svef <- reactive({
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
        result <- dbGetColumn(c(input$var_x, input$var_y), input$base_filter)
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
    paste("Srednja vrednost:", mean(data$x), "<br>Standardna devijacija:", sd(data$x))
  })
  output$common_stats_text <- renderText({
    data <- svef()
    paste("<b>Ukupno tačaka:", count(data), "</b>")
  })
  output$y_stats_text <- renderText({
    data <- svef()
    paste("Srednja vrednost:", mean(data$y), "<br>Standardna devijacija:", sd(data$y))
  })


  output$facet <- renderPlot({
    data <- svef()
    if(count(data) == 0) return(ggplot(data))

    col <- input$facet_col
    # this is "unused" but we need a way to register it as a reactive dependency
    filt_var <- input$facet_filter_var
    filt_val <- input$facet_filter_val
    pos <- if(input$facet_jitter) "jitter" else "identity"
    ggplot(data, aes(x, y, color=color)) + geom_point(alpha=input$facet_alpha, position=pos) +
      facet_grid(rows = "facet") +
      guides(colour = guide_legend(override.aes = list(alpha = 1))) + scale_color_discrete(name=col) +
      xlab(input$var_x) + ylab(input$var_y)
  })
  output$facet.ui <- renderUI(plotOutput("facet", height=input$plot_height))

  #
  # ---------------------------------------
  # Tab 3: Linear model
  # ---------------------------------------
  #

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


  #
  # ---------------------------------------
  # Misc: observers
  # These get fired whenever something happens that changes variables they use
  # ---------------------------------------
  #

  observe({
    join_choices <- dbColumnValues(input$facet_filter_var)
    updateSelectInput(session, "facet_filter_val", choices = join_choices, selected = join_choices[1])
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


#
# =======================================================
# STEP 3: LAYING EVERYTHING OUT
# =======================================================
#
ui <- fluidPage(

  # Application title
  titlePanel("Statistika projekat"),

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

              tabPanel("Tačkasti grafikoni", value="joins",
                       fluidRow(
                         column(9, uiOutput("facet.ui", height = 600)),
                         column(3, wellPanel(
                           sliderInput("facet_alpha", "Transparentnost (alpha):",
                                       min = 0, max = 1, value = 0.07),
                           checkboxInput("facet_jitter", "Jitter", value = TRUE),
                           selectInput("facet_filter_var", "Prikaži samo ako je", choices = allCharacterColumns(), selected = "smerovi2020.podrucje"),
                           selectInput("facet_filter_val", "jedan od", choices = dbColumnValues("smerovi2020.podrucje"), selected = "gimnazija", multiple = TRUE),
                           selectInput("facet_col", "Boja", choices = allCharacterColumns(), selected = "smerovi2020.podrucje")
                         ))
                       )
              ),

              tabPanel("Linearna regresija", value="models",

                       h3("Regression line"),

                       fluidRow(
                         column(9, uiOutput("model_regline.ui")),
                         column(3,  wellPanel(
                           sliderInput("model_rl_alpha", "Transparentnost (alpha):",
                                       min = 0, max = 1, value = 0.04),
                           checkboxInput("model_rl_jitter", "Jitter", value = TRUE)
                         ))
                       ),

                       h3("Residual count"),

                       fluidRow(
                         column(9, uiOutput("model_freqpoly.ui")),
                         column(3, wellPanel(
                           sliderInput("model_freq_binwidth", "Binwidth",
                                       min = 0.01, max = 3, value = 0.33)
                         ))
                       ),

                       h3("Residual distribution"),

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

  sliderInput("plot_height", "Visina plotova", 100, 1600, 400)
)

#
# Finally, run the app!
#
shinyApp(ui = ui, server = server, options = c(port=4000))