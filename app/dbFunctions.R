library(magrittr)

allColumns <- function() {
  dbListTables(dbConn) %>% lapply(function(x) paste(x, dbListFields(dbConn, x), sep=".")) %>% unlist()
}

allColumnsByType <- function(tableName, types) {
  q <- dbSendQuery(dbConn, paste("SELECT * from ", tableName, " limit 1"))
  res <- dbColumnInfo(q) %>% filter(Sclass %in% types) %>% select(name)
  dbClearResult(q)
  res %>% lapply(function(x) paste(tableName, x, sep="."))
}

numericColumns <- function(tableName) allColumnsByType(tableName, c("double", "integer"))
characterColumns <- function(tableName) allColumnsByType(tableName, "character")
logicalColumns <- function(tableName) allColumnsByType(tableName, "logical")

allNumericColumns <- function() lapply(dbListTables(dbConn), numericColumns) %>% unlist(use.names = FALSE)
allCharacterColumns <- function() lapply(dbListTables(dbConn), characterColumns) %>% unlist(use.names = FALSE)

dbColumnValues <- function(colName) {
  print(paste("Getting values for", colName))
  spl <- str_split(colName, "\\.")[[1]]
  table <- spl[1]
  col <- spl[2]
  dbGetQuery(dbConn, paste("SELECT DISTINCT", col, "FROM", table))
}

dbGetColumn <- function(colName, filter) {
  if(grepl("(UPDATE)|(DELETE)|(DROP)|(CREATE)|(ALTER)|(RENAME)|(TRUNCATE)|(INSERT)|(MERGE)", filter, TRUE))
    return(NULL)
  table <- str_split(colName[1], "\\.")[[1]][1]
  query <- paste("SELECT", toString(colName), "FROM", table)
  year <- str_sub(table, start=-4)
  filter %<>% str_replace_all("os\\.", paste0("os", year, "."))
  filter %<>% str_replace_all("smerovi\\.", paste0("smerovi", year, "."))
  filter %<>% str_replace_all("ucenici\\.", paste0("ucenici", year, "."))

  if(str_starts(table, "ucenici")) {
    year <- str_sub(table, -4, -1)
    query <- paste0(query, " JOIN smerovi", year, " ON ", table, ".upisana_id=smerovi", year, ".id JOIN os", year, " ON ",
                    table, ".osnovna_id=os", year, ".id")
  }
  if(filter != "") {
    query <- paste(query, "WHERE", filter)
  }
  query %<>% tolower()
  print(paste("Executing query:", query))
  dbGetQuery(dbConn, query)
}