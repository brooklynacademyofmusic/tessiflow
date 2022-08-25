#' sqlite_upsert
#'
#' @param table string, right now only `jobs` is valid
#' @param data data.frame of data to update, rows will be matched on flow_name, job_name, and start_time
#' @param con database connection
#' @importFrom dplyr copy_to
#' @importFrom checkmate assert_names assert_data_frame assert_choice
#' @return invisibly
sqlite_upsert <- function(table = "jobs", data, con = tessiflow$db) {
  assert_choice(table, c("jobs","performance"))
  flows_log_open()
  
  matching_cols <- DBI::dbGetQuery(con,
    "SELECT m.name table_name, ii.name column_name
    FROM sqlite_schema AS m,
         pragma_index_list(m.name) AS il,
         pragma_index_info(il.name) AS ii
         WHERE m.type='table'") %>% 
    filter(table_name == table) %>% 
    select(column_name) %>% 
    unlist
  
  assert_names(names(data),must.include = matching_cols)
  assert_data_frame(data, min.rows = 1)
  
  data <- dplyr::select(data, intersect(names(data), DBI::dbListFields(con, table))) %>%
    dplyr::ungroup() %>%
    dplyr::mutate_if(~ !is.double(.) && !is.integer(.), as.character)
  
  copy_to(con, data, "data", overwrite = TRUE, temporary = TRUE)
  
  data_names <- paste0("\"", names(data), "\"")
  
  sql <- paste(
    "INSERT INTO", table, "(", paste(data_names, collapse = ", "), ")",
    "SELECT", paste(data_names, collapse = ", "),
    "FROM data f WHERE true",
    "ON CONFLICT (", paste(matching_cols, collapse = ", "), ")",
    "DO UPDATE SET", paste(paste0(data_names, " = excluded.", data_names), collapse = ", ")
  )
  
  DBI::dbExecute(con, sql)
}
