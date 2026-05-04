library(ggplot2)
library(readxl)
library(tidyverse)
library(lubridate)
library(data.table)
library(stringr)
library(dplyr)
library(readr)

################################################################################
########################            Define the directory              ##########
################################################################################
dir     <- "//fs1-cbr.nexus.csiro.au/{af-sandysoils-ii}"

site_number <- "2._SSO2_Copeville-Farley"
site_name <- "Copeville-Farley"

headDir <- paste0(dir, "/work/Output-2/Site-Data/", site_number)

soils_folder <- "/Jackies_working"
compiled_folder <- "/2025/"

file <- "collated Copeville data step 2.xlsx"
worksheet <- "collated wide format"

path_file <- paste0(headDir, soils_folder, compiled_folder,file )

df_wide_results <- read_xlsx(path_file, sheet = worksheet, skip = 3) 
str(df_wide_results)

### get the dates

df_dates <- read_xlsx(path_file, sheet = "look up tables")  

df_dates <- df_dates %>%
  select(Variable, Date) %>%
  filter(row_number() <= which(Variable == "Harvest Index"))

df_dates <- df_dates %>%
  mutate(Date = format(as.Date(as.numeric(Date), origin = "1899-12-30"), "%d - %m - %Y"))

################################################################################
## Pivot longer

df_long_results <- df_wide_results %>%
  pivot_longer(
    cols = -PlotID,
    names_to = "Variable",
    values_to = "Value"
  )




################################################################################
#Add some metdata
 

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



df_long_results <- left_join(df_long_results, metadata)

names(df_long_results)

### reorder the clms
df_long_results <- df_long_results %>%
  select(PlotID, 
         experiment, Year, Row, Block, Wholeplot, 
         RippingTreatment, Nutrient.Treatment, Treatment.Combo, 
         RippingTreatment_name, Nutrient.Treatment_name, Treatment.Description,
         everything())

#### add some extra clms

df_long_results <- df_long_results %>%
  mutate(
    After_Phenology_stage = "Kenton will provide",
    sowing_date = "TBC",
    period_since_sowing = "TBC",
    days_since_sowing = "TBC"
  )


### get the dates of sampling


df_dates
unique(df_long_results$Variable)

df_long_results <- df_long_results %>%
  left_join(df_dates, by = "Variable")
names(df_long_results)

df_long_results <- df_long_results %>%
mutate(sowing_date = df_dates$Date[df_dates$Variable == "sowing date"])

### day since sowing

df_long_results <- df_long_results %>%
  mutate(
    days_since_sowing = as.numeric(
      as.Date(trimws(Date), "%d - %m - %Y") - 
        as.Date(trimws(sowing_date), "%d - %m - %Y")
    )
  )
file_save <- "collated Copeville data May2026_afterR.csv"
path_file_save <- paste0(headDir, soils_folder, compiled_folder,file_save )

write.csv(df_long_results, path_file_save, row.names = FALSE)


