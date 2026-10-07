library(DBI)
library(RPostgres)

db_connect <- function() {
  dbConnect(
    RPostgres::Postgres(),
    host     = Sys.getenv("DB_HOST", "127.0.0.1"),
    port     = as.integer(Sys.getenv("DB_PORT", "5432")),
    dbname   = Sys.getenv("DB_NAME", "ngram_db"),
    user     = Sys.getenv("DB_USER"),
    password = Sys.getenv("DB_PASSWORD")
  )
}

close_connect <- function(con) {
  if (!is.null(con) && dbIsValid(con)) {
    dbDisconnect(con)
    return(TRUE)
  }
  return(FALSE)
}