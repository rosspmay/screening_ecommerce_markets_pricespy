# Flag markets where normalized total consumer loss exceeds upper bounds
# Author: Analysis script
# Date: 2026-01-20

library(dplyr)
library(tidyr)
library(readr)

# Define the configurations for each case
cases <- data.frame(
  region = c("gb", "gb", "se", "se"),
  product = c("freezers", "headphones", "freezers", "headphones"),
  cl_folder = c("cl_results_gb_freezers", "cl_results_gb_headphones", 
                "cl_results_se_freezers", "cl_results_se_headphones"),
  upper_folder = c("upper_pi_bounds_gb_freezers", "upper_pi_bounds_gb_headphones",
                   "upper_pi_bounds_se_freezers", "upper_pi_bounds_se_headphones"),
  upper_file = c("tcl_upper_gb_freez.csv", "tcl_upper_gb_hp.csv",
                 "tcl_upper_se_freez.csv", "tcl_upper_se_hp.csv"),
  stringsAsFactors = FALSE
)

# Function to process a single market file (a_markets or na_markets)
process_market_file <- function(file_path, upper_bounds, region, product, market_type) {
  # Read the market data
  market_data <- read_csv(file_path, show_col_types = FALSE)
  
  # Check if file is empty or has no data rows
  if (nrow(market_data) == 0) {
    return(data.frame())
  }
  
  # Normalize tot_consum_loss by mean of tot_consum_loss
  market_data <- market_data %>%
    mutate(
      normalized_tcl = tot_consum_loss / mean(tot_consum_loss, na.rm = TRUE),
      region = region,
      product = product,
      market_type = market_type
    )
  
  # Merge with upper bounds
  market_data <- market_data %>%
    left_join(upper_bounds, by = c("mark_size" = "ms"))
  
  # Flag markets where normalized TCL exceeds upper bound
  market_data <- market_data %>%
    mutate(
      flagged = normalized_tcl > tcl_upper,
      excess_amount = normalized_tcl - tcl_upper,
      excess_pct = ((normalized_tcl - tcl_upper) / tcl_upper) * 100
    )
  
  return(market_data)
}

# Initialize list to store all results
all_results <- list()
flagged_summary <- list()

# Process each case
for (i in 1:nrow(cases)) {
  region <- cases$region[i]
  product <- cases$product[i]
  cl_folder <- cases$cl_folder[i]
  upper_folder <- cases$upper_folder[i]
  upper_file <- cases$upper_file[i]
  
  cat(sprintf("\n=== Processing: %s %s ===\n", toupper(region), product))
  
  # Read upper bounds
  upper_bounds_path <- file.path("output", upper_folder, upper_file)
  upper_bounds <- read_csv(upper_bounds_path, show_col_types = FALSE)
  
  # Process a_markets
  a_markets_path <- file.path("output", cl_folder, "a_markets.csv")
  if (file.exists(a_markets_path)) {
    a_results <- process_market_file(a_markets_path, upper_bounds, region, product, "a_markets")
    
    if (nrow(a_results) > 0) {
      # Save detailed results for a_markets
      output_file <- file.path("output", cl_folder, "a_markets_flagged.csv")
      write_csv(a_results, output_file)
      cat(sprintf("  A-markets: %d markets, %d flagged (%.1f%%)\n", 
                  nrow(a_results), sum(a_results$flagged), 
                  100 * sum(a_results$flagged) / nrow(a_results)))
      
      all_results[[paste0(region, "_", product, "_a")]] <- a_results
    }
  }
  
  # Process na_markets
  na_markets_path <- file.path("output", cl_folder, "na_markets.csv")
  if (file.exists(na_markets_path)) {
    na_results <- process_market_file(na_markets_path, upper_bounds, region, product, "na_markets")
    
    if (nrow(na_results) > 0) {
      # Save detailed results for na_markets
      output_file <- file.path("output", cl_folder, "na_markets_flagged.csv")
      write_csv(na_results, output_file)
      cat(sprintf("  NA-markets: %d markets, %d flagged (%.1f%%)\n", 
                  nrow(na_results), sum(na_results$flagged), 
                  100 * sum(na_results$flagged) / nrow(na_results)))
      
      all_results[[paste0(region, "_", product, "_na")]] <- na_results
    }
  }
}

# Combine all results
combined_results <- bind_rows(all_results)

# Save combined detailed results
write_csv(combined_results, "output/all_markets_flagged_detailed.csv")
cat(sprintf("\n=== Overall Results ===\n"))
cat(sprintf("Total markets analyzed: %d\n", nrow(combined_results)))
cat(sprintf("Total flagged markets: %d (%.1f%%)\n", 
            sum(combined_results$flagged), 
            100 * sum(combined_results$flagged) / nrow(combined_results)))

# Create summary statistics
summary_stats <- combined_results %>%
  group_by(region, product, market_type) %>%
  summarise(
    total_markets = n(),
    flagged_markets = sum(flagged),
    flagged_pct = 100 * sum(flagged) / n(),
    avg_normalized_tcl = mean(normalized_tcl),
    avg_tcl_upper = mean(tcl_upper, na.rm = TRUE),
    max_excess_amount = max(excess_amount[flagged], na.rm = TRUE),
    avg_excess_amount = mean(excess_amount[flagged], na.rm = TRUE),
    avg_excess_pct = mean(excess_pct[flagged], na.rm = TRUE),
    .groups = "drop"
  )

# Save summary statistics
write_csv(summary_stats, "output/flagged_markets_summary.csv")
cat("\nSummary by region, product, and market type:\n")
print(summary_stats)

# Create a list of flagged markets for further analysis
flagged_markets <- combined_results %>%
  filter(flagged) %>%
  select(region, product, market_type, product_id, mark_size, 
         tot_consum_loss, normalized_tcl, tcl_upper, 
         excess_amount, excess_pct) %>%
  arrange(region, product, market_type, desc(excess_pct))

# Save flagged markets list
write_csv(flagged_markets, "output/flagged_markets_for_analysis.csv")
cat(sprintf("\nFlagged markets saved to: output/flagged_markets_for_analysis.csv\n"))
cat(sprintf("Number of flagged markets for pricing pattern analysis: %d\n", nrow(flagged_markets)))

# Create market size distribution of flagged markets
ms_distribution <- flagged_markets %>%
  group_by(region, product, market_type, mark_size) %>%
  summarise(
    n_flagged = n(),
    avg_excess_pct = mean(excess_pct),
    .groups = "drop"
  ) %>%
  arrange(region, product, market_type, mark_size)

write_csv(ms_distribution, "output/flagged_markets_by_market_size.csv")

# Create visual summary table
visual_summary <- combined_results %>%
  group_by(region, product) %>%
  summarise(
    a_markets_total = sum(market_type == "a_markets"),
    a_markets_flagged = sum(market_type == "a_markets" & flagged),
    a_markets_flagged_pct = 100 * sum(market_type == "a_markets" & flagged) / sum(market_type == "a_markets"),
    na_markets_total = sum(market_type == "na_markets"),
    na_markets_flagged = sum(market_type == "na_markets" & flagged),
    na_markets_flagged_pct = 100 * sum(market_type == "na_markets" & flagged) / sum(market_type == "na_markets"),
    .groups = "drop"
  )

write_csv(visual_summary, "output/flagged_markets_visual_summary.csv")
cat("\nVisual summary:\n")
print(visual_summary)

cat("\n=== Analysis Complete ===\n")
cat("Output files created:\n")
cat("  1. output/all_markets_flagged_detailed.csv - All markets with flagging status\n")
cat("  2. output/flagged_markets_summary.csv - Summary statistics by region/product/type\n")
cat("  3. output/flagged_markets_for_analysis.csv - List of flagged markets for pricing analysis\n")
cat("  4. output/flagged_markets_by_market_size.csv - Distribution by market size\n")
cat("  5. output/flagged_markets_visual_summary.csv - Quick visual summary\n")
cat("  6. Individual *_flagged.csv files in each cl_results_* folder\n")
