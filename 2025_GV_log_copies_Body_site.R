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

mastertable <- read_excel("_2025_database_JG.xlsx", 
                   sheet = "mastertable", col_types = c("text", "numeric",
                                                        "numeric", "numeric", "numeric", 
                                                        "numeric", "numeric", "numeric", 
                                                        "numeric", "numeric", "numeric", 
                                                        "numeric", "numeric", "numeric", "numeric",
                                                        "text", "text",
                                                        "numeric", "text",
                                                        "text", "text", "text", 
                                                        "text", "text", "text",
                                                        "text", "text", "text",
                                                        "text", "text", "text",
                                                        "text", "text", "text",
                                                        "text", "text"))
#View(mastertable)

# Create the first basic boxplot
ggplot(mastertable, aes(x = Body_site, y = GV_log_copies_swab)) +
  geom_boxplot(outlier.shape = NA) +
  geom_jitter(width = 0.2, alpha = 0.5) +
  labs(
    title = "GV log copies per swab by Sample Type",
    x = "Sample Type",
    y = "Log10 GV copies/swab"
  )


# Ensure Body_site is a factor with correct order and labels
mastertable <- mastertable %>%
  mutate(Body_site = factor(Body_site, 
                              levels = c("vaginal_fluid", "penile_skin", "rectal"),
                              labels = c("Vaginal", "Penile skin", "Rectal")))

# Plot
ggplot(mastertable, aes(x = Body_site, y = GV_log_copies_swab, fill = Body_site)) +
  geom_boxplot(width = 0.4, outlier.shape = NA, color = "black") +
  geom_jitter(width = 0.2, alpha = 0.5, size = 1.5) +
  scale_fill_manual(values = c("Vaginal" = "palevioletred", 
                               "Penile skin" = "paleturquoise4", 
                               "Rectal" = "gold")) +
  theme_classic() +
  labs(
    x = "Body site",
    y = "Log10 Gardnerella spp. copies/swab"
  ) +
  theme(
    legend.position = "none",
    axis.title = element_text(face = "bold"),
    axis.text.x = element_text(angle = 0, hjust = 0.5),
    axis.text.y = element_text(size = 12), 
    axis.text.x.bottom  = element_text(size = 12)
  )

##### Add the statistics

# Define the pairwise comparisons to show
comparisons <- list(
  c("Vaginal", "Penile skin"),
  c("Vaginal", "Rectal"),
  c("Penile skin", "Rectal")
)

#open PNG-Grafikgerät
png("Plots/Gspp_BodySite.png", width = 1800, height = 1800, res = 300)

# Plot with statistical comparisons
ggplot(mastertable, aes(x = Body_site, y = GV_log_copies_swab, fill = Body_site)) +
  geom_boxplot(width = 0.4, outlier.shape = NA, color = "black") +
  geom_jitter(width = 0.2, alpha = 0.5, size = 1.5) +
  scale_fill_manual(values = c("Vaginal" = "palevioletred", 
                               "Penile skin" = "paleturquoise4", 
                               "Rectal" = "gold")) +
  theme_classic() +
  labs(
    title = expression(italic("Gardnerella spp.") * " DNA copies per body site"),
    x = "Body site",
    y = expression( "Log10 DNA copy number (" *italic("Gardnerella spp.")* ")" )
  ) +
  stat_compare_means(comparisons = comparisons, 
                     method = "wilcox.test", 
                     label = "p.format", 
                     label.y = c(8.8, 9.6, 10.4)) +
  stat_compare_means(method = "kruskal.test", 
                     label.y = 10.6, 
                     label = "p.format") +
  theme(
    legend.position = "none",
    axis.title = element_text(face = "bold"),
    axis.text.x = element_text(angle = 0, hjust = 0.5, size = 12),
    axis.text.y = element_text(size = 12)
  )

dev.off()