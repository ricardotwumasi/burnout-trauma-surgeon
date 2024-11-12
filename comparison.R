# Load required libraries with checks
required_packages <- c("tidyverse")
lapply(required_packages, function(pkg) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    install.packages(pkg)
  }
  library(pkg, character.only = TRUE)
})

# Load required libraries
library(tidyverse)

# Read data
burnout_data <- read.csv("burnout_comparison_updated.csv")

# Create indicator for studies with CIs and format specialty labels
burnout_data <- burnout_data %>%
  mutate(
    has_ci = !is.na(CI_Lower),
    # Create combined specialty and author label
    specialty_label = case_when(
      Paper != "" ~ paste0(Specialty, "\n(", Paper, ")"),
      Specialty == "Trauma" ~ "Trauma Surgery\n(Kirdar-Smith et al., (this study))",
      TRUE ~ Specialty
    )
  )

# Create forest plot
forest_plot <- ggplot(burnout_data, aes(y = reorder(specialty_label, Burnout_Prevelance))) +
  # Add points and error bars
  geom_point(aes(x = Burnout_Prevelance, shape = has_ci), size = 3) +
  geom_errorbarh(
    aes(xmin = CI_Lower, xmax = CI_Upper, x = Burnout_Prevelance),
    height = 0.2,
    data = burnout_data %>% filter(has_ci)
  ) +
  # Add reference line for trauma estimate
  geom_vline(xintercept = 0.60, linetype = "dashed", color = "red", alpha = 0.5) +
  # Labels and formatting
  labs(
    title = "Burnout Prevalence Across Surgical Specialties",
    subtitle = "Comparison of Systematic Reviews and Meta-Analyses",
    x = "Prevalence",
    y = NULL,
    caption = "Note: Point estimates without confidence intervals shown as triangles"
  ) +
  theme_minimal() +
  theme(
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank(),
    axis.text = element_text(size = 10),
    axis.text.y = element_text(size = 9, lineheight = 0.9),
    axis.title = element_text(size = 12, face = "bold"),
    plot.title = element_text(size = 14, face = "bold"),
    legend.position = "bottom",
    legend.box = "vertical",
    legend.margin = margin(t = 10),
    plot.margin = margin(5.5, 5.5, 5.5, 5.5, "pt")
  ) +
  scale_x_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.1)) +
  scale_shape_manual(values = c(17, 19),
                    labels = c("No confidence interval reported", "Confidence interval reported"),
                    name = "Study precision")

# Print the plot
print(forest_plot)

# Save the plot if needed (uncomment to use)
# ggsave("burnout_forest_plot.pdf", forest_plot, width = 12, height = 8, units = "inches")
