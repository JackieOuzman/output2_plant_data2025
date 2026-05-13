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

site_number <- "6. SS02_Bowmans-Roberts"
site_name <- "Bowmans-Roberts"

headDir <- paste0(dir, "/work/Output-2/Site-Data/", site_number)

folder <- "/Jackies_working/"
#compiled_folder <- "/2025/"

file <- "collated plant data 22_01_2026.xlsx"
worksheet <- "collated wide format"

path_file <- paste0(headDir, folder, file )

df_wide_results <- read_xlsx(path_file, sheet = worksheet, skip = 2) 
str(df_wide_results)

### get the dates

df_dates <- read_xlsx(path_file, sheet = "look up tables")  

df_dates <- df_dates %>%
  select(Variable, Date) %>%
  filter(row_number() <= which(Variable == "END"))

df_dates <- df_dates %>%
  mutate(Date = format(as.Date(as.numeric(Date), origin = "1899-12-30"), "%d - %m - %Y"))

################################################################################
## Pivot longer
names(df_wide_results)
df_wide_results <- df_wide_results %>% rename(PlotID = "Plot ID")


df_long_results <- df_wide_results %>%
  pivot_longer(
    cols = -PlotID,
    names_to = "Variable",
    values_to = "Value"
  )




################################################################################
#Add some metdata
 

metadata <- read.csv(paste0(headDir,
                            "/1. SSO2_Trial design_MetaData/2025/SSO2_2025_Bowmans2025Core_FieldBook.csv"))

names(metadata)

metadata <- metadata %>%
  select(
    PlotID = plot,
    Row =x,
    Bay =y,
    experiment,
    Block = block,
    Ripping_main,
    Nutrient,
    Treatment.Combo,
    Ripping_main_lab,
    Nutrition_sub_lab
  ) %>%
  rename(
    RippingTreatment_name = Ripping_main_lab,
    Nutrient.Treatment_name = Nutrition_sub_lab
  )

names(metadata)

df_long_results <- left_join(df_long_results, metadata)

names(df_long_results)

### reorder the clms
df_long_results <- df_long_results %>%
  select(PlotID, 
         experiment, 
         #Year, 
         Row,
         Bay,    
         Block, 
         #Wholeplot, 
         RippingTreatment = Ripping_main, 
         Nutrient.Treatment = Nutrient, 
         Treatment.Combo, 
         RippingTreatment_name, 
         Nutrient.Treatment_name, 
         #Treatment.Description,
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
unique(df_dates$Variable)
unique(df_long_results$Variable)

##### Problem here if this returns 2 which is not a NA
df_dates %>% count(Variable) %>% filter(n > 1)



df_long_results <- df_long_results %>%
  left_join(df_dates, by = "Variable")
names(df_long_results)

#df_dates[df_dates$Variable == "Sowing date", ]

df_long_results <- df_long_results %>%
  mutate(sowing_date = df_dates$Date[df_dates$Variable == "Sowing date"][1])

### day since sowing


df_long_results <- df_long_results %>%
  mutate(
    days_since_sowing = as.numeric(
      as.Date(trimws(Date), "%d - %m - %Y") - 
        as.Date(trimws(sowing_date), "%d - %m - %Y")
    )
  )
file_save <- "collated Bowmans data May2026_afterR.csv"
path_file_save <- paste0(headDir, folder, file_save )

write.csv(df_long_results, path_file_save, row.names = FALSE)


