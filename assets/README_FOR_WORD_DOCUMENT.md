# Market Collusion Analysis - Assets for Word Document

## Overview
This folder contains all tables, figures, and data exports from the market collusion analysis, ready to be imported into your Word document.

---

## Analysis Summary

**Total markets analyzed:** 192  
**Total flagged markets:** 8 (4.2%)

### Breakdown by Region and Product

- **GB Freezers**
  - A-markets: 2 total, 0 flagged (0%)
  - NA-markets: 15 total, 0 flagged (0%)

- **GB Headphones**
  - A-markets: 37 total, 2 flagged (5.4%)
  - NA-markets: 22 total, 1 flagged (4.5%)

- **SE Freezers**
  - A-markets: 0 total
  - NA-markets: 8 total, 1 flagged (12.5%)

- **SE Headphones**
  - A-markets: 40 total, 2 flagged (5.0%)
  - NA-markets: 68 total, 2 flagged (2.9%)

---

## Files in This Folder

### Summary Tables (for main text)

1. **table1_summary_statistics.csv**
   - Detailed statistics by region, product, and market type
   - Includes: total markets, flagged counts, percentages, averages
   - **Use for:** Main results table in your document

2. **table2_visual_summary.csv**
   - Overview by region and product (combining a_markets and na_markets)
   - **Use for:** Quick summary table

3. **table3_top_flagged_markets.csv**
   - Top 10 flagged markets by excess percentage
   - **Use for:** Highlighting specific anomalous markets

### Detailed Data Files

4. **flagged_markets_for_analysis.csv**
   - All 8 flagged markets with full details
   - Includes: product_id, market size, consumer loss metrics, excess amounts

5. **flagged_markets_by_market_size.csv**
   - Distribution of flagged markets by market size
   - Shows how many flagged markets per size category

6. **flagged_markets_summary.csv**
   - Same as table1 (duplicate for reference)

7. **flagged_markets_visual_summary.csv**
   - Same as table2 (duplicate for reference)

8. **all_markets_combined_results.csv**
   - Complete dataset with all 192 markets
   - Includes flagging status, normalized values, and excess measures

### Figures (High Resolution PNG, 300 DPI)

9. **figure1_flagged_pct_by_region_product.png**
   - Bar chart showing percentage of flagged markets by region, product, and market type
   - **Use for:** Main visual showing flagging rates across all categories

10. **figure2_excess_pct_flagged_markets.png**
    - Scatter plot showing excess percentage above upper bound for each flagged market
    - **Use for:** Showing severity of anomalies in flagged markets

11. **figure3_normalized_tcl_vs_upper_bound.png**
    - Scatter plot with facets for each region-product combination
    - Shows normalized TCL vs upper bound with diagonal reference line
    - **Use for:** Visualizing which markets exceed bounds

12. **figure4_proportion_flagged_by_market_size.png**
    - Stacked bar chart showing proportion of flagged vs non-flagged by market size
    - **Use for:** Examining if certain market sizes are more prone to flagging

### Additional Resources

13. **analysis_summary_report.txt**
    - Text summary of all findings
    - **Use for:** Quick reference while writing

---

## Suggested Word Document Structure

### 1. Introduction
- Brief overview of the analysis purpose
- Mention of 4 region-product combinations analyzed

### 2. Methodology
- Normalization approach (dividing by mean TCL)
- Flagging criterion (exceeding upper prediction bounds)
- Explanation of excess measures

### 3. Results

#### 3.1 Overall Findings
- Insert text: "8 flagged markets (4.2%) out of 192 total"
- **Insert:** table2_visual_summary.csv

#### 3.2 Detailed Statistics
- **Insert:** table1_summary_statistics.csv
- **Insert:** figure1_flagged_pct_by_region_product.png

#### 3.3 Flagged Markets Analysis
- **Insert:** table3_top_flagged_markets.csv
- **Insert:** figure2_excess_pct_flagged_markets.png

#### 3.4 Distribution Analysis
- **Insert:** figure3_normalized_tcl_vs_upper_bound.png
- **Insert:** figure4_proportion_flagged_by_market_size.png

### 4. Discussion
- Interpretation of flagged markets
- SE Freezers showing highest flagging rate (12.5%)
- Consistency of flagging rates across headphone markets (2.9-5.4%)

### 5. Conclusion
- Flagged markets are candidates for pricing pattern analysis
- Potential indicators of market coordination

---

## Notes

- All CSV files can be opened in Excel and copied into Word as tables
- PNG files are high resolution (300 DPI) suitable for publication
- The "NaN" values in visual_summary table indicate categories with no markets (e.g., SE Freezers a_markets)
- The "-Inf" values in summary_stats indicate groups with no flagged markets

---

## Methodology Details (for Methods section)

### Normalization Formula
normalized_tcl = tot_consum_loss / mean(tot_consum_loss)

### Flagging Criterion
flagged = TRUE if normalized_tcl > tcl_upper, FALSE otherwise

### Excess Measures
- Excess Amount = normalized_tcl - tcl_upper
- Excess Percentage = ((normalized_tcl - tcl_upper) / tcl_upper) × 100

---

Generated: January 26, 2026
