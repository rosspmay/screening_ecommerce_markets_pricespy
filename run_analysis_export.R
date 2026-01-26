#!/usr/bin/env Rscript

# Script to run the analysis and export all tables and graphs to assets folder
setwd("/home/rom/pricespy")

# Load required libraries
library(dplyr)
library(tidyr)
library(readr)
library(ggplot2)
library(knitr)

set.seed(123)

cat("==============================================\n")
cat("Starting Analysis Export for Word Document\n")
cat("==============================================\n\n")

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
      cat(sprintf("  NA-markets: %d markets, %d flagged (%.1f%%)\n", 
                  nrow(na_results), sum(na_results$flagged), 
                  100 * sum(na_results$flagged) / nrow(na_results)))
      
      all_results[[paste0(region, "_", product, "_na")]] <- na_results
    }
  }
}

# Combine all results
combined_results <- bind_rows(all_results)

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

# Create a list of flagged markets for further analysis
flagged_markets <- combined_results %>%
  filter(flagged) %>%
  select(region, product, market_type, product_id, mark_size, 
         tot_consum_loss, normalized_tcl, tcl_upper, 
         excess_amount, excess_pct) %>%
  arrange(region, product, market_type, desc(excess_pct))

# Create market size distribution of flagged markets
ms_distribution <- flagged_markets %>%
  group_by(region, product, market_type, mark_size) %>%
  summarise(
    n_flagged = n(),
    avg_excess_pct = mean(excess_pct),
    .groups = "drop"
  ) %>%
  arrange(region, product, market_type, mark_size)

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

cat("\n==============================================\n")
cat("Exporting Tables and Data to Assets Folder\n")
cat("==============================================\n\n")

# Format tables for Word document with proper capitalization and labels
summary_stats_formatted <- summary_stats %>%
  mutate(
    region = toupper(region),
    product = tools::toTitleCase(product),
    market_type = ifelse(market_type == "a_markets", "A", "NA")
  ) %>%
  rename(
    Country = region,
    `Product Category` = product,
    `Market Type` = market_type,
    `Total Markets` = total_markets,
    `Flagged Markets` = flagged_markets,
    `Flagged %` = flagged_pct,
    `Avg Normalised CPG` = avg_normalized_tcl,
    `Avg CPG Upper` = avg_tcl_upper,
    `Max Excess Amount` = max_excess_amount,
    `Avg Excess Amount` = avg_excess_amount,
    `Avg Excess %` = avg_excess_pct
  )

visual_summary_formatted <- visual_summary %>%
  mutate(
    region = toupper(region),
    product = tools::toTitleCase(product)
  ) %>%
  rename(
    Country = region,
    `Product Category` = product,
    `A Markets Total` = a_markets_total,
    `A Markets Flagged` = a_markets_flagged,
    `A Markets Flagged %` = a_markets_flagged_pct,
    `NA Markets Total` = na_markets_total,
    `NA Markets Flagged` = na_markets_flagged,
    `NA Markets Flagged %` = na_markets_flagged_pct
  )

flagged_markets_formatted <- flagged_markets %>%
  mutate(
    region = toupper(region),
    product = tools::toTitleCase(product),
    market_type = ifelse(market_type == "a_markets", "A", "NA")
  ) %>%
  rename(
    Country = region,
    `Product Category` = product,
    `Market Type` = market_type,
    `Product ID` = product_id,
    `Market Size` = mark_size,
    CPG = tot_consum_loss,
    `Normalised CPG` = normalized_tcl,
    `CPG Upper` = tcl_upper,
    `Excess Amount` = excess_amount,
    `Excess %` = excess_pct
  )

ms_distribution_formatted <- ms_distribution %>%
  mutate(
    region = toupper(region),
    product = tools::toTitleCase(product),
    market_type = ifelse(market_type == "a_markets", "A", "NA")
  ) %>%
  rename(
    Country = region,
    `Product Category` = product,
    `Market Type` = market_type,
    `Market Size` = mark_size,
    `Number Flagged` = n_flagged,
    `Avg Excess %` = avg_excess_pct
  )

# Export all data tables to assets folder
write_csv(summary_stats, "assets/flagged_markets_summary.csv")
cat("✓ Exported: assets/flagged_markets_summary.csv\n")

write_csv(visual_summary, "assets/flagged_markets_visual_summary.csv")
cat("✓ Exported: assets/flagged_markets_visual_summary.csv\n")

write_csv(flagged_markets, "assets/flagged_markets_for_analysis.csv")
cat("✓ Exported: assets/flagged_markets_for_analysis.csv\n")

write_csv(ms_distribution, "assets/flagged_markets_by_market_size.csv")
cat("✓ Exported: assets/flagged_markets_by_market_size.csv\n")

write_csv(summary_stats_formatted, "assets/table1_summary_statistics.csv")
cat("✓ Exported: assets/table1_summary_statistics.csv\n")

write_csv(visual_summary_formatted, "assets/table2_visual_summary.csv")
cat("✓ Exported: assets/table2_visual_summary.csv\n")

write_csv(head(flagged_markets_formatted, 10), "assets/table3_top_flagged_markets.csv")
cat("✓ Exported: assets/table3_top_flagged_markets.csv\n")

# Export full combined results for reference
write_csv(combined_results, "assets/all_markets_combined_results.csv")
cat("✓ Exported: assets/all_markets_combined_results.csv\n")

cat("\n==============================================\n")
cat("Generating and Exporting Graphs\n")
cat("==============================================\n\n")

# Plot 1: Flagged markets by region and product
# Create formatted labels
summary_stats_plot <- summary_stats %>%
  mutate(
    region_product = case_when(
      region == "gb" & product == "freezers" ~ "GB Freezers",
      region == "gb" & product == "headphones" ~ "GB Headphones",
      region == "se" & product == "freezers" ~ "SE Freezers",
      region == "se" & product == "headphones" ~ "SE Headphones"
    ),
    market_type_label = ifelse(market_type == "a_markets", "A", "NA")
  )

p1 <- ggplot(summary_stats_plot, aes(x = region_product, y = flagged_pct, fill = market_type_label)) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_manual(values = c("A" = "#F8766D", "NA" = "#00BFC4")) +
  labs(title = "Percentage of Flagged Markets by Country, Product Category, and Market Type",
       x = "Country & Product Category",
       y = "Flagged %",
       fill = "Market Type") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave("assets/figure1_flagged_pct_by_region_product.png", plot = p1, width = 10, height = 6, dpi = 300)
cat("✓ Exported: assets/figure1_flagged_pct_by_region_product.png\n")

# Plot 2: Distribution of excess percentages for flagged markets
if (nrow(flagged_markets) > 0) {
  # Create formatted labels
  flagged_markets_plot <- flagged_markets %>%
    mutate(
      region_product = case_when(
        region == "gb" & product == "freezers" ~ "GB Freezers",
        region == "gb" & product == "headphones" ~ "GB Headphones",
        region == "se" & product == "freezers" ~ "SE Freezers",
        region == "se" & product == "headphones" ~ "SE Headphones"
      ),
      market_type_label = ifelse(market_type == "a_markets", "A", "NA")
    )
  
  p2 <- ggplot(flagged_markets_plot, aes(x = region_product, y = excess_pct, color = market_type_label)) +
    geom_point(size = 3) +
    scale_color_manual(values = c("A" = "#F8766D", "NA" = "#00BFC4")) +
    labs(title = "Excess % Above CPG Upper Bound for Flagged Markets",
         x = "Country & Product Category",
         y = "Excess % Above Upper Bound",
         color = "Market Type") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  ggsave("assets/figure2_excess_pct_flagged_markets.png", plot = p2, width = 10, height = 6, dpi = 300)
  cat("✓ Exported: assets/figure2_excess_pct_flagged_markets.png\n")
}

# Plot 3: Normalized TCL vs Upper Bound for all markets
# Create formatted labels
combined_results_plot <- combined_results %>%
  mutate(
    region_product = case_when(
      region == "gb" & product == "freezers" ~ "GB Freezers",
      region == "gb" & product == "headphones" ~ "GB Headphones",
      region == "se" & product == "freezers" ~ "SE Freezers",
      region == "se" & product == "headphones" ~ "SE Headphones"
    )
  )

p3 <- ggplot(combined_results_plot, aes(x = tcl_upper, y = normalized_tcl, color = flagged)) +
  geom_point(alpha = 0.6) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "black") +
  scale_color_manual(values = c("FALSE" = "#619CFF", "TRUE" = "#F8766D"),
                     labels = c("FALSE" = "Not Flagged", "TRUE" = "Flagged")) +
  labs(title = "Normalised CPG vs Upper Bound",
       x = "Upper Bound (CPG Upper)",
       y = "Normalised CPG",
       color = "Status") +
  facet_wrap(~ region_product, scales = "free") +
  theme_minimal()

ggsave("assets/figure3_normalized_tcl_vs_upper_bound.png", plot = p3, width = 12, height = 8, dpi = 300)
cat("✓ Exported: assets/figure3_normalized_tcl_vs_upper_bound.png\n")

# Plot 4: Distribution by market size
p4 <- ggplot(combined_results_plot, aes(x = as.factor(mark_size), fill = flagged)) +
  geom_bar(position = "fill") +
  scale_fill_manual(values = c("FALSE" = "#619CFF", "TRUE" = "#F8766D"),
                    labels = c("FALSE" = "Not Flagged", "TRUE" = "Flagged")) +
  labs(title = "Proportion of Flagged Markets by Market Size",
       x = "Market Size",
       y = "Proportion",
       fill = "Status") +
  facet_wrap(~ region_product) +
  theme_minimal()

ggsave("assets/figure4_proportion_flagged_by_market_size.png", plot = p4, width = 12, height = 8, dpi = 300)
cat("✓ Exported: assets/figure4_proportion_flagged_by_market_size.png\n")

cat("\n==============================================\n")
cat("Creating Summary Report\n")
cat("==============================================\n\n")

# Create a summary report text file
sink("assets/analysis_summary_report.txt")

cat("===============================================\n")
cat("MARKET COLLUSION ANALYSIS - SUMMARY REPORT\n")
cat("===============================================\n\n")
cat("Date Generated:", format(Sys.Date(), "%B %d, %Y"), "\n\n")

cat("OVERALL FINDINGS\n")
cat("----------------\n")
cat(sprintf("Total markets analyzed: %d\n", nrow(combined_results)))
cat(sprintf("Total flagged markets: %d (%.1f%%)\n\n", 
            sum(combined_results$flagged), 
            100 * sum(combined_results$flagged) / nrow(combined_results)))

cat("BREAKDOWN BY REGION AND PRODUCT\n")
cat("--------------------------------\n\n")

for (i in 1:nrow(visual_summary)) {
  cat(sprintf("%s %s:\n", toupper(visual_summary$region[i]), visual_summary$product[i]))
  cat(sprintf("  A-markets: %d total, %d flagged (%.1f%%)\n", 
              visual_summary$a_markets_total[i],
              visual_summary$a_markets_flagged[i],
              visual_summary$a_markets_flagged_pct[i]))
  cat(sprintf("  NA-markets: %d total, %d flagged (%.1f%%)\n\n", 
              visual_summary$na_markets_total[i],
              visual_summary$na_markets_flagged[i],
              visual_summary$na_markets_flagged_pct[i]))
}

cat("\nKEY OBSERVATIONS\n")
cat("----------------\n")
cat("• Flagged markets exhibit anomalously high consumer losses\n")
cat("• Markets exceed upper prediction interval bounds\n")
cat("• Candidates for further pricing pattern analysis\n")
cat("• Potential indicators of market coordination\n\n")

cat("\nEXPORTED FILES\n")
cat("--------------\n\n")
cat("Tables:\n")
cat("  • table1_summary_statistics.csv - Detailed statistics by region/product/type\n")
cat("  • table2_visual_summary.csv - Overview by region and product\n")
cat("  • table3_top_flagged_markets.csv - Top 10 flagged markets\n")
cat("  • flagged_markets_for_analysis.csv - All flagged markets\n")
cat("  • flagged_markets_by_market_size.csv - Distribution by market size\n\n")

cat("Figures:\n")
cat("  • figure1_flagged_pct_by_region_product.png\n")
cat("  • figure2_excess_pct_flagged_markets.png\n")
cat("  • figure3_normalized_tcl_vs_upper_bound.png\n")
cat("  • figure4_proportion_flagged_by_market_size.png\n\n")

cat("METHODOLOGY NOTES\n")
cat("-----------------\n")
cat("• Markets normalized by mean total consumer loss within groups\n")
cat("• Flagging based on exceeding upper prediction intervals\n")
cat("• Separate analysis for a_markets and na_markets\n")
cat("• Four region-product combinations analyzed\n\n")

cat("===============================================\n")

sink()

cat("✓ Exported: assets/analysis_summary_report.txt\n")

cat("\n==============================================\n")
cat("EXPORT COMPLETE!\n")
cat("==============================================\n\n")
cat("All tables, graphs, and data have been exported to the 'assets' folder.\n")
cat("You can now import these into your Word document.\n\n")

# Print summary statistics to console
cat("\nSUMMARY STATISTICS TABLE:\n")
print(summary_stats)

cat("\n\nVISUAL SUMMARY TABLE:\n")
print(visual_summary)

cat("\n\nTOP 10 FLAGGED MARKETS:\n")
print(head(flagged_markets, 10))

cat("\n==============================================\n")
cat("Analysis script completed successfully!\n")
cat("==============================================\n")
