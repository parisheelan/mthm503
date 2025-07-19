readRenviron(".Renviron.R")
Sys.getenv("PGRUSER")

conn <- DBI::dbConnect(
  RPostgres::Postgres(),
  dbname = Sys.getenv("PGRDATABASE"),
  host = Sys.getenv("PGRHOST"),
  user = Sys.getenv("PGRUSER"),
  password = Sys.getenv("PGRPASSWORD"),
  port = Sys.getenv("PGRPORT")
)

DBI::dbIsValid(conn)

tables <- DBI::dbListTables(conn)
tables