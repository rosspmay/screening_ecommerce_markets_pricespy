# Temporary script to calculate average and median of CPG (tcl) 
# for mixed markets in Sweden freezers

# Load the data
data <- read.csv("output/cl_results_se_freezers/mixed_markets.csv")

# Calculate average and median of tot_consum_loss
average_cpg <- mean(data$tot_consum_loss, na.rm = TRUE)
median_cpg <- median(data$tot_consum_loss, na.rm = TRUE)

# Print results
cat("Statistics for CPG (tot_consum_loss) in Sweden Freezers Mixed Markets:\n")
cat("=================================================================\n")
cat(sprintf("Average: %.2f\n", average_cpg))
cat(sprintf("Median: %.2f\n", median_cpg))
cat(sprintf("Number of observations: %d\n", nrow(data)))
