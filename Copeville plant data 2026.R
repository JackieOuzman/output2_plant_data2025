################################################################################
# Copeville Plant Data 2026 - Sandy Soils II
#
# Purpose: Read the collated "wide format" Copeville plant data (which has
# stacked header rows for units/date/inside-outside/north-south/comments),
# split those header rows out into a metadata lookup table, pivot the actual
# plot data into long/tidy format, join the metadata + trial design fieldbook
# back on, calculate days since sowing, and write out a tidy long CSV.
#
# Output: collated Copeville data May2026_afterR.csv
################################################################################

library(ggplot2)
library(readxl)
library(tidyverse)
library(lubridate)
library(data.table)
library(stringr)
library(dplyr)
library(readr)

################################################################################
## Define the directory and file paths
################################################################################
dir     <- "//fs1-cbr.nexus.csiro.au/{af-sandysoils-ii}"

site_number <- "2._SSO2_Copeville-Farley"
site_name <- "Copeville-Farley"

headDir <- paste0(dir, "/work/Output-2/Site-Data/", site_number)

folder <- "/Jackies_working"
compiled_folder <- "/2025/"

file <- "collated Copeville data step 2.xlsx"
worksheet <- "collated wide format"

path_file <- paste0(headDir, folder, compiled_folder, file)

################################################################################
## Read the wide data
## Rows 4-9 of the spreadsheet are a "stacked header": row 4 = variable name
## (the column header R reads in), rows 5-9 = units / date / inside_outside_row
## / North_south / Comments metadata for that variable. skip = 3 brings all of
## this in as one dataframe, with the metadata rows sitting in as extra "PlotID"
## values (e.g. "units", "date") instead of real plot IDs.
################################################################################
df_wide_results <- read_xlsx(path_file, sheet = worksheet, skip = 3)

## Split the metadata rows from the real plot data rows
meta_labels <- c("units", "date", "inside_outside_row", "North_south", "Comments")

df_metadata_wide <- df_wide_results %>% filter(PlotID %in% meta_labels)
df_data_wide     <- df_wide_results %>% filter(!PlotID %in% meta_labels)

################################################################################
## Build the metadata lookup table: one row per Variable, with units/date/
## inside_outside_row/North_south/Comments as columns
################################################################################
df_metadata_long <- df_metadata_wide %>%
  pivot_longer(
    cols = -PlotID,
    names_to = "Variable",
    values_to = "Value"
  ) %>%
  pivot_wider(
    names_from = PlotID,
    values_from = Value
  )

# Convert the Excel serial date into a real Date
df_metadata_long <- df_metadata_long %>%
  mutate(date = as.Date(as.numeric(date), origin = "1899-12-30"))

################################################################################
## Pivot the real plot data into long format, then join the metadata on
################################################################################
df_data_long <- df_data_wide %>%
  pivot_longer(cols = -PlotID, names_to = "Variable", values_to = "Value")

df_long_results <- df_data_long %>%
  left_join(df_metadata_long, by = "Variable")

################################################################################
## Get the sowing date (a single trial-wide value, not a per-variable one)
## from the "look up tables" sheet, and calculate days since sowing
################################################################################
df_dates <- read_xlsx(path_file, sheet = "look up tables")

sowing_date_raw <- df_dates %>%
  filter(str_detect(str_to_lower(Variable), "sowing")) %>%
  mutate(Date = as.Date(as.numeric(Date), origin = "1899-12-30")) %>%
  pull(Date)

df_long_results <- df_long_results %>%
  mutate(
    sowing_date = sowing_date_raw,
    days_since_sowing = as.numeric(date - sowing_date)
  )

################################################################################
## Bring in the plot-level trial design metadata (Row/Block/Treatment etc.)
## from the fieldbook, joined on PlotID
################################################################################
metadata <- read.csv(paste0(headDir,
                            "/1. SSO2_Trial design_MetaData/2025/SSO2_Copeville_2025_Core_FieldBook.csv"))

metadata <- metadata %>%
  select(
    PlotID,
    experiment,
    Year,
    Row,
    Block,
    Wholeplot,
    RippingTreatment,
    Nutrient.Treatment,
    Treatment.Combo,
    RippingTreatment.1,
    Nutrient.Treatment.1,
    Treatment.Description
  ) %>%
  rename(RippingTreatment_name = RippingTreatment.1,
         Nutrient.Treatment_name = Nutrient.Treatment.1)

df_long_results <- left_join(df_long_results, metadata, by = "PlotID")

################################################################################
## Reorder columns: plot metadata -> variable metadata -> value
################################################################################
df_long_results <- df_long_results %>%
  select(PlotID,
         experiment, Year, Row, Block, Wholeplot,
         RippingTreatment, Nutrient.Treatment, Treatment.Combo,
         RippingTreatment_name, Nutrient.Treatment_name, Treatment.Description,
         Variable, units, inside_outside_row, North_south, Comments,
         date, sowing_date, days_since_sowing,
         Value)

################################################################################
## Add remaining placeholder columns (still TBC / pending from Kenton)
################################################################################
df_long_results <- df_long_results %>%
  mutate(
    After_Phenology_stage = "Kenton will provide",
    period_since_sowing = "TBC"
  )

## Value comes out of pivot_longer as character - convert to numeric
## (check the NA count below matches expected genuine missing data)
df_long_results <- df_long_results %>%
  mutate(Value = as.numeric(Value))

sum(is.na(df_long_results$Value))

df_long_results %>% 
  filter(Variable == "Yield Machine" & is.na(Value)) %>% 
  select(PlotID, Variable, Value)

################################################################################
## Save the tidy long dataset
################################################################################
file_save <- "collated Copeville data May2026_afterR.csv"
path_file_save <- paste0(headDir, folder, compiled_folder, file_save)

write.csv(df_long_results, path_file_save, row.names = FALSE)
