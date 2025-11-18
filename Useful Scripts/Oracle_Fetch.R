# --- Package Management --- #
# List of required packages
required_packages <- c("httr", "jsonlite", "dplyr", "rlang")

# Check, install if missing, and load
for (pkg in required_packages) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    install.packages(pkg)
  }
  library(pkg, character.only = TRUE)
}

#' Fetch, Filter, and Sort, from ORACLE API #

#' This function connects to an API, applies filters, sorts the results, selects specific columns, automatically handles all pagination, and returns a single,clean data frame with uppercase column names by default.

#' @param base_url The complete web address (URL) of the API data table.
#' @param ... A series of named arguments to be used as filters.
#'   (e.g., `obs_year = 2024`, `region = "MHI"`).
#' @param sort_by The name of the column to sort the results by.
#' @param sort_order The sort direction: "asc" (default) or "desc". 
#' @param uppercase_cols Logical. If TRUE (default), converts all column names to uppercase. Set to FALSE to keep original names.

fetch_data = function(base_url, ..., sort_by = NULL, sort_order = "asc",
                      records_per_page = 1000,
                      pause_between_requests = 0.5,
                      uppercase_cols = TRUE) {
  
  #### --------------- Part 1: Prepare the API Query --------------- ####
  
  # Collect all filter choices into a single list.
  # Collect all filter choices into a single list.
  filter_expressions <- enquos(...)
  filters <- list()
  
  
  # Convert database syntax into intuitive R language
  operator_map <- list(`==`='$eq', `!=`='$ne', `>`='$gt', `>=`='$gte', `<`='$lt', `<=`='$lte')
  if(length(filter_expressions) > 0) {
    for (expr_q in filter_expressions) {
      expr <- quo_get_expr(expr_q)
      if (is_call(expr)) {
        operator <- call_name(expr); column <- as_name(call_args(expr)[[1]]); value <- eval_tidy(call_args(expr)[[2]])
        api_operator <- operator_map[[operator]]
        if (!is.null(api_operator)) {
          if (api_operator == "$eq") {
            filters[[column]] <- value
          } else {
            # If a filter for this column already exists, add to it (lets you filter for in between parameters)
            if (column %in% names(filters) && is.list(filters[[column]])) {
              filters[[column]][[api_operator]] <- value
            } else {
              # Otherwise, create a new filter.
              filters[[column]] <- set_names(list(value), api_operator)
            }
          }
        }
      }
    }
  }
  
  # This list will hold all filtering and sorting rules for the choice filter
  json_query_list = list()
  if (length(filters) > 0) {
    json_query_list = filters
  }
  if (!is.null(sort_by)) {
    sort_list = list()
    sort_list[[sort_by]] = sort_order
    json_query_list[['$orderby']] = sort_list
  }
  
  # This list holds the final parameters that will be attached to the URL.
  final_params = list(limit = records_per_page)
  if (length(json_query_list) > 0) {
    final_params$q = toJSON(json_query_list, auto_unbox = TRUE)
  }
  
  
  
  #### -------- Part 2: Loop Through Pages to Fetch All Data -------- ####
  
  # Create an empty list to store the data frame from each page we retrieve
  all_pages_list = list()
  
  
  # Initialize the record offset at 0. The offset tells the API how many records to skip, which is how we get to the next "page" of data.
  current_offset <- 0
  
  # Gives Confirmation of filter applied
  cat("Starting data fetch from:", base_url, "\n")
  if('q' %in% names(final_params)) cat("Applying filter and sort:", final_params$q, "\n")
  
  # Begin a loop that will continue fetching pages until a until all records are fetched
  while (TRUE) {
    final_params$offset = current_offset
    response = GET(url = base_url, query = final_params)
    
    # Tells if the API call was successful or not
    if (http_status(response)$category != "Success") {
      warning("API call failed... while trying to fetch record number ", current_offset + 1)
      return(NULL)
    }
    
    parsed_content = fromJSON(content(response, "text", encoding = "UTF-8"))
    
    # Checks if there are data records that match the filtered parameters, if not, then reports
    if (length(parsed_content$items) == 0) {
      if (current_offset == 0) cat("No records matched the specified filter.\n")
      else cat("No more data found. Fetch complete.\n")
      break
    }
    
    # Print a progress/status update to the console.
    all_pages_list[[length(all_pages_list) + 1]] = parsed_content$items
    cat("Fetched records", current_offset + 1, "to", current_offset + nrow(parsed_content$items), "\n")
    
    # Exits the loop if there are no more API pages
    if (parsed_content$hasMore == FALSE) {
      cat("No more records found. Fetch complete.\n")
      break
    }
    
    # Increments the offset by the page size to prepare to get new data on the next page request
    current_offset = current_offset + records_per_page
    
    # Brief pause between pages to not overload the database
    Sys.sleep(pause_between_requests)
  }
  
  
  #### ------- Part 3: Combine, Finalize, and Return the Data ------- ####
  
  # Provides progress/status update 
  cat("Combining all pages...\n")
  
  # Combines all the pages into one dataframe
  final_df = bind_rows(all_pages_list)
  
  # Provides number of records fetched (this number could be used to QC to ensure all data was fetched)
  cat("Done. Total records retrieved:", nrow(final_df), "\n")
  
  # Convert column names to uppercase
  if (uppercase_cols) {
    colnames(final_df) = toupper(colnames(final_df))
  }
  return(final_df)
}


