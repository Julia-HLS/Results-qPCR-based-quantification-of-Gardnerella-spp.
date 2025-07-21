###########################################################################
# Clear the workspace
rm(list = ls())

# Set the working directory
setwd("D:/USER/Julia/Schule/2021_22Fhnw/BA/Auswertung/publicRepo")
getwd()

# Load necessary libraries
library(tidyverse)
library(readxl)
library(ggpubr)
library(gridExtra)
library(ggplot2)
library(readr)
library(FSA)
library(openxlsx)
library(lawstat)

# Import dataset 
mastertable <- read_excel("2025_mastertable_adjusted_allTimepoints.xlsx", 
                          sheet = "mastertable", col_types = c("text", "numeric",
                                                               "numeric", "numeric", "numeric", 
                                                               "numeric", "numeric", "numeric", 
                                                               "numeric", "numeric", "numeric",
                                                               "numeric", #SD
                                                               "numeric", "numeric", "numeric", "numeric",
                                                               "text", "text",
                                                               "numeric", "text",
                                                               "text", "text", "text", 
                                                               "text", "text", "text",
                                                               "text", "text", "text",
                                                               "text", "text", "text",
                                                               "text", "text", "text",
                                                               "text", "text", "text"))
# Drop samples with negative GV_status
mastertable <- mastertable %>% filter(GV_status != "negative")

# Filter data 
mastertable <- mastertable %>%
  filter(
    protection %in% c("yes", "no"),
    !is.na(GV_log_copies_swab),
    !is.na(Body_site)
  ) %>%
  mutate(
    protection = factor(protection, levels = c("yes", "no"), labels = c("Yes", "No")),
    Body_site = factor(Body_site, levels = c("vaginal_fluid", "penile_skin"), labels = c("Vaginal", "Penile skin"))
  )

# Brunner-Munzel-Test
vaginal_data <- filter(mastertable, Body_site == "Vaginal")
penile_data <- filter(mastertable, Body_site == "Penile skin")

bm_vaginal <- brunner.munzel.test(
  x = vaginal_data$GV_log_copies_swab[vaginal_data$protection == "Yes"],
  y = vaginal_data$GV_log_copies_swab[vaginal_data$protection == "No"]
)

bm_penile <- brunner.munzel.test(
  x = penile_data$GV_log_copies_swab[penile_data$protection == "Yes"],
  y = penile_data$GV_log_copies_swab[penile_data$protection == "No"]
)

# Format p-values
p_vag <- format.pval(bm_vaginal$p.value, digits = 3)
p_pen <- format.pval(bm_penile$p.value, digits = 3)

# Prepare P-value as dataframe (1/facet)
p_text_df <- data.frame(
  Body_site = factor(c("Vaginal", "Penile skin"), levels = c("Vaginal", "Penile skin")),
  label = c(paste0("Brunner-Munzel p = ", p_vag),
            paste0("Brunner-Munzel p = ", p_pen)),
  x = 1.5,
  y = 10.6
)

# Make sure Body_site factor order is enforced in main data too
mastertable$Body_site <- factor(mastertable$Body_site, levels = c("Vaginal", "Penile skin"))

# Open PNG graphics device
png("Plots/Vag_Pen_Gspp_load_protection_withoutNegative.png", width = 1800, height = 1800, res = 300)

# Plot
ggplot(mastertable, aes(x = protection, y = GV_log_copies_swab)) +
  geom_boxplot(aes(fill = protection), width = 0.4, outlier.shape = NA, color = "black") +
  geom_jitter(width = 0.2, alpha = 0.5, size = 1.5) +
  scale_fill_manual(values = c("Yes" = "mediumspringgreen", "No" = "tomato")) +
  facet_wrap(~Body_site, strip.position = "top") +
  theme_classic() +
  labs(
    x = "Contraceptive use",
    y = expression( "Log10 DNA copy number (" *italic("Gardnerella spp.")* ")" ),
    title = expression(italic("Gardnerella spp.") * " load by protection status - without negatives")
  ) +
  theme(
    legend.position = "none",
    axis.title = element_text(face = "bold"),
    axis.text.x = element_text(size = 12),
    axis.text.y = element_text(size = 12)
  ) +
  geom_text(data = p_text_df,
            aes(x = x, y = y, label = label),
            inherit.aes = FALSE,
            fontface = "bold",
            size = 4.5)

# Close device
dev.off()
