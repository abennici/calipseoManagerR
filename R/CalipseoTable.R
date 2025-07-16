#' Calipseo SQL Table Handler
#'
#' This R6 class represents a SQL table from the Calipseo model.
#' It provides functions to manage data entries and generate SQL insert statements.
#'
#' @format R6 class
#'
#' @section Methods:
#' \describe{
#'   \item{\code{new(pool, table_name)}}{Create a table object connected to SQL.}
#'   \item{\code{TABLE_INFO()}}{Return SQL metadata of the table.}
#'   \item{\code{FOREIGN_KEYS()}}{Return foreign keys related to this table.}
#'   \item{\code{ADD_ENTRY(...)}}{Add a new row entry.}
#'   \item{\code{VIEW_ENTRIES()}}{View current staged entries (data.table).}
#'   \item{\code{INSERT_STATEMENT()}}{Generate SQL insert statements from staged entries.}
#'   \item{\code{CLEAR_STATEMENT()}}{Generate SQL to clear the table (TRUNCATE).}
#' }
#'
#' @examples
#' \dontrun{
#' pool <- pool::dbPool(DBI::dbConnect(...))
#' table_obj <- CalipseoTable$new(pool, "my_table")
#' table_obj$ADD_ENTRY(name = "Test", quantity = 42)
#' table_obj$INSERT_STATEMENT()
#' }
#'
#' @export

CalipseoTable <- R6Class("CalipseoTable",
                         public = list(
                           pool = NULL,
                           name = NULL,
                           structure = NULL,
                           dataset = NULL,
                           entries = NULL,
                           foreign_keys = NULL,
                           index_field = NULL,

                           initialize = function(pool, table_name) {
                             self$pool <- pool
                             self$name <- table_name
                             self$structure <- dbGetQuery(pool, sprintf("DESCRIBE `%s`", table_name))
                             self$dataset <- self$structure
                             self$entries <- data.table(1)[, (self$structure$Field) := NA][, V1 := NULL][.0]

                             fk_query <- paste0(
                               "SELECT RefCons.constraint_name as 'name', RefCons.constraint_schema as 'schema', ",
                               "RefCons.table_name as 'table', KeyCol.column_name as 'column', ",
                               "KeyCol.constraint_schema as 'referenced_schema', ",
                               "RefCons.referenced_table_name as 'referenced_table', ",
                               "KeyCol.referenced_column_name as 'referenced_column' ",
                               "FROM information_schema.referential_constraints RefCons ",
                               "JOIN information_schema.key_column_usage KeyCol ",
                               "ON RefCons.constraint_schema = KeyCol.table_schema ",
                               "AND RefCons.table_name = KeyCol.table_name ",
                               "AND RefCons.constraint_name = KeyCol.constraint_name ",
                               sprintf("WHERE RefCons.constraint_schema = DATABASE() AND RefCons.Table_name = '%s';", table_name)
                             )
                             self$foreign_keys <- dbGetQuery(pool, fk_query)

                             idx <- self$structure$Field[self$structure$Extra == "auto_increment"]
                             self$index_field <- ifelse(length(idx) == 0, NA, idx)
                           },

                           ADD_ENTRY = function(...) {
                             args <- list(...)
                             row <- as.list(rep(NA, nrow(self$structure)))
                             names(row) <- self$structure$Field

                             for (colname in names(args)) {
                               if (colname %in% names(row)) {
                                 row[[colname]] <- args[[colname]]
                               }
                             }

                             if (!is.na(self$index_field) && is.null(args[[self$index_field]])) {
                               row[[self$index_field]] <- self$MAX_INDEX() + 1
                             }

                             self$entries <- rbindlist(list(self$entries, as.data.table(row)), fill = TRUE)
                           },

                           REMOVE_ENTRY = function(row_to_remove) {
                             self$entries <- self$entries[-row_to_remove, ]
                           },

                           CLEAR_ENTRIES = function() {
                             self$entries <- data.table(1)[, (self$structure$Field) := NA][, V1 := NULL][.0]
                           },

                           VIEW_ENTRIES = function() {
                             return(self$entries)
                           },

                           TABLE_INFO = function() {
                             return(self$dataset)
                           },

                           MANDATORY_FIELDS = function() {
                             return(subset(self$dataset, Null == "NO")$Field)
                           },

                           VIEW_DB_TABLE = function() {
                             dbGetQuery(self$pool, sprintf("SELECT * FROM `%s`", self$name))
                           },

                           FOREIGN_KEYS = function() {
                             return(self$foreign_keys)
                           },

                           MAX_INDEX = function() {
                             if (is.na(self$index_field)) {
                               return(nrow(self$entries))
                             }
                             sql <- sprintf("SELECT MAX(`%s`) AS max_val FROM `%s`", self$index_field, self$name)
                             max_db <- dbGetQuery(self$pool, sql)$max_val
                             max_db <- ifelse(is.na(max_db), 0, max_db)
                             return(max_db + nrow(self$entries))
                           },

                           INSERT_STATEMENT = function(execute = FALSE, file = NULL) {
                             sql_note <- sprintf("--\n-- Insert data for %s\n--\n", self$name)
                             columns <- paste0("`", self$structure$Field, "`", collapse = ",")
                             sql_insert <- sprintf("INSERT INTO `%s` (%s) VALUES\n", self$name, columns)

                             sql_values <- apply(self$entries, 1, function(row) {
                               paste0("(", paste0(mapply(function(value, type) {
                                 if (is.na(value) || value == "null") {
                                   return("NULL")
                                 } else if (grepl('^varchar|^timestamp|^date', type)) {
                                   return(sprintf("'%s'", value))
                                 } else {
                                   return(as.character(value))
                                 }
                               }, row, self$structure$Type), collapse = ","), ")")
                             })

                             sql_body <- paste(sql_values, collapse = ",\n")
                             sql <- paste0(sql_note, sql_insert, sql_body, ";\n\n")

                             if (execute) {
                               dbExecute(self$pool, sql)
                               self$CLEAR_ENTRIES()
                             }

                             if (!is.null(file)) {
                               writeLines(sql, file)
                             }

                             return(sql)
                           },

                           CLEAR_STATEMENT = function(execute = FALSE, file = NULL) {
                             sql <- sprintf(
                               "--\n-- Clear data for %s\n--\n\nSET FOREIGN_KEY_CHECKS = 0;\nTRUNCATE `%s`;\nSET FOREIGN_KEY_CHECKS = 1;\n\n",
                               self$name, self$name
                             )

                             if (execute) {
                               statements <- unlist(strsplit(sql, ";"))
                               statements <- statements[nzchar(statements)]
                               for (stmt in statements) {
                                 dbExecute(self$pool, paste0(stmt, ";"))
                               }
                             }

                             if (!is.null(file)) {
                               writeLines(sql, file)
                             }

                             return(sql)
                           }
                         )
)
