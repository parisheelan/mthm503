get_db_connection <- function() {
  is_ci <- Sys.getenv("CI") == "true"

  if (is_ci) {
    # CI path: return SQLite connection (in-memory)
    DBI::dbConnect(RSQLite::SQLite(), here::here("mock_data", "mockdb.sqlite"))
  } else {
    # Production path: return Postgres connection
    DBI::dbConnect(
      RPostgres::Postgres(),
      dbname = Sys.getenv("PGRDATABASE"),
      host = Sys.getenv("PGRHOST"),
      user = Sys.getenv("PGRUSER"),
      password = Sys.getenv("PGRPASSWORD"),
      port = Sys.getenv("PGRPORT")
    )
  }
}

load_data <- function() {
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
  
  casualties <- DBI::dbReadTable(conn, "stats19_casualties")
  accidents <- DBI::dbReadTable(conn, "stats19_accidents")
  vehicles <- DBI::dbReadTable(conn, "stats19_vehicles")
  names(casualties)
  names(accidents)
  names(vehicles)
  
  DBI::dbDisconnect(conn)
  return c
}
