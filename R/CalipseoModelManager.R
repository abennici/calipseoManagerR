#' Calipseo Model Manager
#'
#' This R6 class handles connection and SQL metadata management for the Calipseo data model.
#' It allows listing tables available in the database and loading individual tables as R6 objects.
#'
#' @format R6 class
#'
#' @section Methods:
#' \describe{
#'   \item{\code{new(pool)}}{Create a new manager using a database connection pool.}
#'   \item{\code{list_tables()}}{List all tables in the database.}
#'   \item{\code{load_table(table_name)}}{Load a specific SQL table as a \code{CalipseoTable} object.}
#' }
#'
#' @examples
#' \dontrun{
#' pool <- pool::dbPool(DBI::dbConnect(...))
#' manager <- CalipseoModelManager$new(pool)
#' manager$list_tables()
#' table_obj <- manager$load_table("my_table")
#' }
#'
#' @export

CalipseoModelManager <- R6::R6Class("CalipseoModelManager",
                                    private = list(
                                      pool = NULL,
                                      db_name = NULL
                                    ),
                                    public = list(
                                      initialize = function(pool, db_name) {
                                        private$pool <- pool
                                        private$db_name <- db_name
                                      },

                                      list_tables = function() {
                                        DBI::dbGetQuery(private$pool, "SHOW TABLES")
                                      },

                                      load_table = function(table_name) {
                                        CalipseoTable$new(pool = private$pool, db_name = private$db_name, table_name = table_name)
                                      }
                                    )
)
