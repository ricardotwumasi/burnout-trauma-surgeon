# Create moderator variable for MBI status
data_es$mbi_status <- ifelse(data$mbi == "Y", 1, 0)

# Conduct mixed-effects meta-regression
meta_reg <- rma(yi, vi, mods = ~ mbi_status, data=data_es, method="REML", test="knha")

# Print results
print(summary(meta_reg))

# Calculate predicted values for each subgroup
# Non-MBI studies (mbi_status = 0)
pred_non_mbi <- predict(meta_reg, newmods = c(0))
# MBI studies (mbi_status = 1)
pred_mbi <- predict(meta_reg, newmods = c(1))

# Back transform the predictions and CIs
non_mbi_prop <- transf.ilogit(c(pred_non_mbi$pred, pred_non_mbi$ci.lb, pred_non_mbi$ci.ub))
mbi_prop <- transf.ilogit(c(pred_mbi$pred, pred_mbi$ci.lb, pred_mbi$ci.ub))

# Create summary table
results_table <- data.frame(
  Subgroup = c("Non-MBI Studies", "MBI Studies"),
  Proportion = c(non_mbi_prop[1], mbi_prop[1]),
  CI_Lower = c(non_mbi_prop[2], mbi_prop[2]),
  CI_Upper = c(non_mbi_prop[3], mbi_prop[3])
)

# Print formatted results
cat("\nSubgroup Analysis Results:\n")
cat("Non-MBI Studies: ", sprintf("%.3f [%.3f, %.3f]",
    results_table$Proportion[1], results_table$CI_Lower[1], results_table$CI_Upper[1]), "\n")
cat("MBI Studies: ", sprintf("%.3f [%.3f, %.3f]",
    results_table$Proportion[2], results_table$CI_Lower[2], results_table$CI_Upper[2]), "\n")
cat("\nTest of Moderators (MBI vs Non-MBI):\n")
cat("QM = ", sprintf("%.2f", meta_reg$QM), "\n")
cat("p-value = ", sprintf("%.4f", meta_reg$QMp), "\n")

# Additional heterogeneity statistics for subgroups
# Separate analyses for each subgroup
mbi_studies <- data_es[data_es$mbi_status == 1, ]
non_mbi_studies <- data_es[data_es$mbi_status == 0, ]

res_mbi <- rma(yi, vi, data=mbi_studies, method="REML", test="knha")
res_non_mbi <- rma(yi, vi, data=non_mbi_studies, method="REML", test="knha")

cat("\nHeterogeneity within subgroups:\n")
cat("MBI Studies I² = ", sprintf("%.1f%%", res_mbi$I2), "\n")
cat("Non-MBI Studies I² = ", sprintf("%.1f%%", res_non_mbi$I2), "\n")
