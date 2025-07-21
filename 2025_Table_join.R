# Step 1 - Import Data

#Improting libraries
library(dplyr)
library(readxl)
library(tidyverse)
library(openxlsx) #for exporting exel sheet

# Clear the workspace
rm(list = ls())

# Set the working directory
setwd("D:/USER/Julia/Schule/2021_22Fhnw/BA/Auswertung/publicRepo")
getwd()

# Import excel sheet working_qPCR
df_qPCR <- read_excel("_2025_database_JG.xlsx", 
                 sheet = "working_qPCR")
#View(df_qPCR)

# Import excel sheet JG_FJ_Metadata (2)
df_Metadata <- read_excel("_2025_database_JG.xlsx", 
                      sheet = "JG_FJ_Metadata (2)")
#View(df_Metadata)

# Step 2 - Cleaning Data df_qPCR

# 2.1 Standardize column content (character)
ugly_col <- c("GV_ct_1", "GV_ct_2", "GV_ct_3")

df_qPCR[ugly_col] <- lapply(df_qPCR[ugly_col], function(x) as.character(x))

# 2.2 turn all undetermined outputs into "0"
df_qPCR$GV_ct_1[df_qPCR$GV_ct_1 == "Undetermined"] <- "0"
df_qPCR$GV_ct_2[df_qPCR$GV_ct_2 == "Undetermined"] <- "0"
df_qPCR$GV_ct_3[df_qPCR$GV_ct_3 == "Undetermined"] <- "0"

# 2.3 turn columns numeric
df_qPCR[ugly_col] <- lapply(df_qPCR[ugly_col], function(x) as.numeric(x))

# 2.4 turn all N/As in numeric cols into 0
numeric_col <- c("GV_ct_1", "GV_ct_2", "GV_ct_3",
                 "GV_copies1", "GV_copies2", "GV_copies3",
                 "GV_ct_mean", "GV_copies_mean", "GV_copies_swab", "GV_copies_1ul",
                 "GV_log_copies_swab", "GV_log_copies_1ul",
                 "Qubit DNA conc. (ng/µl)")
df_qPCR[numeric_col] <- lapply(df_qPCR[numeric_col], function(x) {
  x[is.na(x)] <- 0
  x
})

# In column GV_status turn DIV/0 into negative
df_qPCR <- df_qPCR %>%
  mutate(GV_status = replace_na(GV_status, "negative"))

#View(df_qPCR)

# Step 3 - Merging df_qPCR and df_Metadata
mastertable <- merge(df_qPCR, df_Metadata, by = "JG_ID", all = FALSE)
#view(mastertable)

# Step 4 - Turn mastertable into xlsx file
workbook <- createWorkbook()
addWorksheet(workbook, "mastertable")
writeData(workbook, sheet = "mastertable", mastertable)

# Step 5 - save to excel file
saveWorkbook(workbook, "2025_mastertable.xlsx", overwrite = TRUE)