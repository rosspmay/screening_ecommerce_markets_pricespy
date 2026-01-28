# Generate Mixed Markets Summary
# This script reads all mixed_markets.csv files, combines them, and creates a summary table

library(dplyr)
library(tidyr)
library(readr)

# Define the configurations for each case
cases <- data.frame(
  region = c("gb", "gb", "se", "se"),
  product = c("freezers", "headphones", "freezers", "headphones"),
  cl_folder = c("cl_results_gb_freezers", "cl_results_gb_headphones", 
                "cl_results_se_freezers", "cl_results_se_headphones"),
  stringsAsFactors = FALSE
)

# Read and combine all mixed_markets files
mixed_results <- list()

cat("\n=== Reading Mixed Markets Files ===\n")

for (i in 1:nrow(cases)) {
  region <- cases$region[i]
  product <- cases$product[i]
  cl_folder <- cases$cl_folder[i]
  
  mixed_markets_path <- file.path("output", cl_folder, "mixed_markets.csv")
  
  if (file.exists(mixed_markets_path)) {
    mixed_data <- read_csv(mixed_markets_path, show_col_types = FALSE)
    
    if (nrow(mixed_data) > 0) {
      mixed_data <- mixed_data %>%
        mutate(
          region = region,
          product = product
        )
      
      mixed_results[[paste0(region, "_", product)]] <- mixed_data
      cat(sprintf("  %s %s: %d mixed markets\n", toupper(region), product, nrow(mixed_data)))
    }
  } else {
    cat(sprintf("  %s %s: file not found\n", toupper(region), product))
  }
}

# Combine all mixed markets
all_mixed_markets <- bind_rows(mixed_results)

# Create summary table for mixed markets
mixed_summary <- all_mixed_markets %>%
  group_by(region, product) %>%
  mutate(
    normalised_tot_consum_loss = tot_consum_loss / mean(tot_consum_loss)
  ) %>%
  summarise(
    total_mixed_markets = n(),
    avg_tot_consum_loss = mean(tot_consum_loss),
    median_tot_consum_loss = median(tot_consum_loss),
    avg_normalised_tot_consum_loss = mean(normalised_tot_consum_loss),
    median_normalised_tot_consum_loss = median(normalised_tot_consum_loss),
    min_tot_consum_loss = min(tot_consum_loss),
    max_tot_consum_loss = max(tot_consum_loss),
    avg_mark_size = mean(mark_size),
    min_mark_size = min(mark_size),
    max_mark_size = max(mark_size),
    .groups = "drop"
  ) %>%
  rename(
    country = region,
    `product category` = product,
    `total mixed markets` = total_mixed_markets,
    `avg CPG` = avg_tot_consum_loss,
    `median CPG` = median_tot_consum_loss,
    `avg normalised CPG` = avg_normalised_tot_consum_loss,
    `median normalised CPG` = median_normalised_tot_consum_loss,
    `min CPG` = min_tot_consum_loss,
    `max CPG` = max_tot_consum_loss,
    `avg market size` = avg_mark_size,
    `min market size` = min_mark_size,
    `max market size` = max_mark_size
  )

# Save combined mixed markets data
write_csv(all_mixed_markets, "output/all_mixed_markets_combined.csv")
write_csv(all_mixed_markets, "assets/all_mixed_markets_combined.csv")

# Save summary table
write_csv(mixed_summary, "output/mixed_markets_summary.csv")
write_csv(mixed_summary, "assets/mixed_markets_summary.csv")

# Display results
cat("\n=== Mixed Markets Summary ===\n")
print(mixed_summary)
cat(sprintf("\nTotal mixed markets across all categories: %d\n", nrow(all_mixed_markets)))

cat("\n=== Files Saved ===\n")
cat("  - output/all_mixed_markets_combined.csv\n")
cat("  - output/mixed_markets_summary.csv\n")
cat("  - assets/all_mixed_markets_combined.csv\n")
cat("  - assets/mixed_markets_summary.csv\n")
cat("\nDone!\n")
