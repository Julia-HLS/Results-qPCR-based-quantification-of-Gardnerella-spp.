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

# Import dataset 

mastertable <- read_excel("2025_mastertable_adjustedMale.xlsx", 
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


# Ensure Body_site is a factor with correct order and labels, rectal swabs are filtered out
mastertable <- mastertable %>%
  filter(
    Body_site %in% c("vaginal_fluid", "penile_skin"),
    !is.na(frequency_of_sexual_intercourse),
    frequency_of_sexual_intercourse != "not_available"
  ) %>%
  mutate(
    Body_site = factor(Body_site, 
                       levels = c("vaginal_fluid", "penile_skin"),
                       labels = c("Vaginal", "Penile skin")
    ),
    frequency_of_sexual_intercourse =factor(frequency_of_sexual_intercourse,
                                            levels = c("never", "rarely", "occasionally", "somewhat_frequent", "frequent", "very_frequent"),
                                            labels = c("Never", "Rarely", "Occasionally", "Somewhat frequent", "Frequent", "Very frequent")
    )
  ) %>%
  mutate(frequency_grouped = case_when(
    frequency_of_sexual_intercourse %in% c("Never", "Rarely", "Occasionally") ~ "Never/ Rare",
    TRUE ~ as.character(frequency_of_sexual_intercourse)
  ),
  frequency_grouped = factor(frequency_grouped,
                             levels = c("Never/ Rare", "Somewhat frequent", "Frequent", "Very frequent"))    
  )

#open PNG-Grafikgerät
png("Plots/adjustedMale_Gspp_load_frequency_sex_bp.png", width = 2700, height = 1200, res = 300)

# Create Plot
Copies_Frequency_plot <- ggplot(mastertable, aes(x = frequency_grouped, y = GV_log_copies_swab, fill = Body_site)) +
  geom_boxplot(position = position_dodge(width = 0.8),
               outlier.shape = NA, color = "black") + 
  
  geom_jitter(position = position_jitterdodge(jitter.width = 0.2, dodge.width = 0.75),
              alpha = 0.5, size = 1.5) +
  scale_fill_manual(values = c("Vaginal" = "palevioletred", 
                               "Penile skin" = "paleturquoise4")) +
  theme_classic() +
  labs(
    title = expression(italic("Gardnerella spp.") * " DNA copies in relation to frequency of sexual intercourse - adjusted male"),
    
    x = "Frequency of sexual intercourse",
    y = expression( "Log10 DNA copy number (" *italic("Gardnerella spp.")* ")" ),
    fill = "Body site"
  ) +
  theme(
    legend.position = "none",
    axis.title = element_text(face = "bold"),
    axis.text.x = element_text(angle = 0, hjust = 0.5),
    axis.text.y = element_text(size = 12), 
    axis.text.x.bottom  = element_text(size = 12)
  )

##### Add the statistics

# separate data by body site
vaginal <- filter(mastertable, Body_site == "Vaginal")
penile <- filter(mastertable, Body_site == "Penile skin")

# Kruskal-Wallis tests
kw_vag <- kruskal.test(GV_log_copies_swab ~ frequency_of_sexual_intercourse, data = vaginal)
kw_pen <- kruskal.test(GV_log_copies_swab ~ frequency_of_sexual_intercourse, data = penile)

# format P-values
p_vag <- paste0("Vaginal: p = ", format.pval(kw_vag$p.value, digits = 3))
p_pen <- paste0("Penile skin: p = ", format.pval(kw_pen$p.value, digits = 3))

# add P-values manually

Copies_Frequency_plot +
  
  annotate("text", x = 1, y = 9.5, label = p_vag, color = "deeppink2", size = 4.5, fontface = "bold") +
  annotate("text", x = 1, y = 9, label = p_pen, color = "deepskyblue3", size = 4.5, fontface = "bold") +
  theme(
    legend.position = "right",
    axis.title = element_text(face = "bold"),
    axis.text = element_text(size = 12)
  )

# Dunn-Test with BH-correction for both body sites
dunn_vag <- dunnTest(GV_log_copies_swab ~ frequency_grouped, data = vaginal, method = "bh")
dunn_pen <- dunnTest(GV_log_copies_swab ~ frequency_grouped, data = penile, method = "bh")

# extract results
results_vag <- dunn_vag$res %>% 
  mutate(Body_site = "Vaginal")

results_pen <- dunn_pen$res %>% 
  mutate(Body_site = "Penile skin")

# combine
results_combined <- bind_rows(results_vag, results_pen)

# columnname
results_combined <- results_combined %>%
  rename(
    Comparison = Comparison,
    Z = Z,
    Unadjusted_p = P.unadj,
    Adjusted_p = P.adj
  ) %>%
  mutate(
    Comparison = gsub("_", " ", Comparison),
    Adjusted_p = signif(Adjusted_p, digits = 3)
  ) %>%
  select(Body_site, Comparison, Adjusted_p)

# view(results_combined)

dev.off()

# Turn results combined into xlsx file
workbook <- createWorkbook()
addWorksheet(workbook, "Dunn_results_adjustedMale")
writeData(workbook, "Dunn_results_adjustedMale", results_combined)

# save to Excel file
saveWorkbook(workbook, "Plots/DunnTest_results_adjustedMale.xlsx", overwrite = TRUE)