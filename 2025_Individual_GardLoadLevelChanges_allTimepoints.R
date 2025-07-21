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

# Filter: All timepoints, non-NA GV_log_copies_swab, relevant body sites
filtered_data <- mastertable %>%
  filter(
    timepoint %in% c("t0", "t1", "t2"),
    Body_site %in% c("vaginal_fluid", "penile_skin"),
    !is.na(GV_log_copies_swab)
  ) %>%
  mutate(
    Body_site = factor(Body_site,
                       levels = c("vaginal_fluid", "penile_skin"),
                       labels = c("Vaginal", "Penile skin")),
    couple_no = factor(couple_no),
    timepoint = factor(timepoint, levels = c("t0", "t1", "t2"))
  )

#open PNG-Grafikgerät
png("Plots/Individual_GardLoadLevelChange.png", width = 2700, height = 1200, res = 300)

# Vaginal plot
plot_vaginal <- filtered_data %>%
  filter(Body_site == "Vaginal") %>%
  ggplot(aes(x = timepoint, y = GV_log_copies_swab, color = couple_no, group = couple_no)) +
  geom_line(linewidth = 0.8, alpha = 0.7) +
  geom_point(size = 3) +
  labs(
    title = expression(italic("Gardnerella spp.") * " load: vaginal swabs"),
    x = "Timepoint",
    y = expression( "Log10 DNA copy number (" *italic("Gardnerella spp.")* ")" )
  ) +
  theme_minimal() +
  theme(
    axis.title = element_text(face = "bold"),
    axis.text = element_text(size = 12)
  )

# Penile plot
plot_penile <- filtered_data %>%
  filter(Body_site == "Penile skin") %>%
  ggplot(aes(x = timepoint, y = GV_log_copies_swab, color = couple_no, group = couple_no)) +
  geom_line(linewidth = 0.8, alpha = 0.7) +
  geom_point(size = 3) +
  labs(
    title = expression(italic("Gardnerella spp.") * " load: penile skin swabs"),
    x = "Timepoint",
    y = expression( "Log10 DNA copy number (" *italic("Gardnerella spp.")* ")" )
  ) +
  theme_minimal() +
  theme(
    axis.title = element_text(face = "bold"),
    axis.text = element_text(size = 12)
  )

# Combined with shared legend
ggarrange(plot_vaginal, plot_penile,
          ncol = 2,
          common.legend = TRUE,
          legend = "right"
          )
dev.off()