# The load_data.R file contains a function that reads a CSV file and returns a data frame. 
# The function uses different read functions based on the file size and then cleans the 
# column names using the clean_names function from the janitor package. 
# The function also allows for filtering the data based on the primary_market column. 
# The default value for the primary_market_filter parameter is NULL, which means that no filtering
# is applied by default. Both file_path and primary_market_filter are parameters of the function
# which take strings as input. file_path is relative to the project root directory. See
# a running *.Rmd file for an example.

load_data <- function(file_path, primary_market_filter = NULL) {
  # Get file size in bytes
  file_size <- file.info(file_path)$size
  
  # Convert file size to megabytes
  file_size_mb <- file_size / 1024^2
  
  # Use different read functions based on file size
  if (file_size_mb < 500) {
    # Read the CSV file with the 'read_csv' function from the 'readr' package
    df <- readr::read_csv(file_path)
    
    # Print a message to the console on a new line
    cat("\nUsing readr::read_csv")
  } else {
    # Read the CSV file with the 'fread' function from the 'data.table' package
    df <- data.table::fread(file_path)
    
    # Print a message to the console on a new line
    cat("\nUsing data.table::fread")
  }
  
  # Start with the 'df' data frame
  df |>
    # Use the 'clean_names' function from the 'janitor' package to clean
    # the column names
    janitor::clean_names() |>
    # Use the 'mutate' function to transform the data
    mutate(
      # Convert 'product_id' to factor
      product_id = as.factor(product_id),
      # Convert 'rank_number' to integer
      rank_number = as.integer(rank_number),
      # Convert 'store_id' to factor
      store_id = as.factor(store_id),
      # Convert 'count' to integer
      count = as.integer(count),
      # Convert 'count_total' to integer
      count_total = as.integer(count_total),
      # Convert 'primary_market' to factor
      primary_market = as.factor(primary_market),
      # Convert 'currency' to factor
      currency = as.factor(currency),
      # Convert 'product_type' to factor
      product_type = as.factor(product_type)
    ) ->
    # Assign the result to 'df'
    df
  
  # If 'primary_market_filter' is not NULL, filter the data by currency
  if (!is.null(primary_market_filter)) {
    df <- df %>%
      filter(primary_market == primary_market_filter)
  }
  
  # Return the data frame
  return(df)
}