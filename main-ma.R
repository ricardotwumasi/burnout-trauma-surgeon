# Load required libraries with checks
required_packages <- c("tidyverse", "metafor", "rje", "meta", "dplyr")
lapply(required_packages, function(pkg) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    install.packages(pkg)
  }
  library(pkg, character.only = TRUE)
})

# Read and prepare the data
data <- read.csv("meta_trauma_extraction.csv") %>%
  mutate(
    mbi_used = ifelse(mbi == "Y", "MBI Studies", "Non-MBI Studies"),
    events = round(burnout_prevalence * sample_size)
  )

# Calculate effect sizes for all studies
data_es <- escalc(
  measure = "PLO",
  xi = data$events,
  ni = data$sample_size,
  data = data,
  slab = paste(data$study_name, data$year, sep=", ")
)

# Overall meta-analysis
res_overall <- rma(yi, vi, data=data_es, method="REML", test="knha")

# Subgroup analysis for MBI studies
data_mbi <- data %>% filter(mbi == "Y")
data_es_mbi <- escalc(
  measure = "PLO",
  xi = round(data_mbi$burnout_prevalence * data_mbi$sample_size),
  ni = data_mbi$sample_size,
  data = data_mbi,
  slab = paste(data_mbi$study_name, data_mbi$year, sep=", ")
)
res_mbi <- rma(yi, vi, data=data_es_mbi, method="REML", test="knha")

# Function to create formatted output for forest plot
format_result <- function(res) {
  est <- transf.ilogit(res$b[1])
  ci_lb <- transf.ilogit(res$ci.lb)
  ci_ub <- transf.ilogit(res$ci.ub)
  sprintf("%.3f [%.3f, %.3f]", est, ci_lb, ci_ub)
}

# Create enhanced forest plot
par(mar = c(5,4,3,2) + 0.1)
forest(res_overall,
       transf = transf.ilogit,
       xlab = "Proportion of Burnout",
       refline = transf.ilogit(res_overall$b[1]),
       xlim = c(-0.5, 2),
       alim = c(0, 1),
       at = seq(0, 1, by=0.2),
       header = "Study",
       mlab = paste("Random-Effects Model"),
       ilab = paste0(formatC(weights(res_overall), format="f", digits=1), "%"),
       ilab.xpos = 1.2,
       cex = 0.75)

# Add weight column header
text(1, res_overall$k + 2, "Weight (%)", cex = 0.75, pos = 4)

# Cumulative meta-analysis with back-transformation
cum_res <- cumul(res_overall, order=data$year)
cum_table <- data.frame(
  study = cum_res$slab,
  estimate = transf.ilogit(cum_res$estimate),
  ci.lb = transf.ilogit(cum_res$ci.lb),
  ci.ub = transf.ilogit(cum_res$ci.ub)
)

# Create cumulative forest plot with back-transformed proportions
forest(cum_res,
       transf = transf.ilogit,
       xlab = "Cumulative Proportion",
       xlim = c(-0.5, 2),
       alim = c(0, 1),
       at = seq(0, 1, by=0.2),
       header = "Study",
       mlab = "Cumulative Estimate",
       cex = 0.75,
       refline = 0.6)

# Publication bias assessment
funnel(res_overall, xlab = "Proportion (logit scale)")
title("Funnel Plot of Standard Error by Logit Proportion")

# Egger's test
eggers_test <- regtest(res_overall)

# Trim and fill analysis
tf <- trimfill(res_overall)
tf_result <- format_result(tf)

# Print detailed results
cat("\nMeta-Analysis Results:\n")
cat("Overall Proportion (back-transformed):", format_result(res_overall), "\n")
cat("MBI Studies Proportion:", format_result(res_mbi), "\n")
cat("Heterogeneity (I²):", sprintf("%.1f%%", res_overall$I2), "\n")
cat("Q-statistic:", sprintf("%.2f (p = %.4f)", res_overall$QE, res_overall$QEp), "\n")
cat("Egger's test p-value:", sprintf("%.4f", eggers_test$pval), "\n")
cat("Trim and fill adjusted proportion:", tf_result, "\n")
