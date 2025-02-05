#===============================================================================
# Greater Manchester Gentrification Index
# 
# Authors: Sophie Flinders and Adam Almeida
#===============================================================================

###################### Load packages

library(tidyverse)
library(data.table)   # import data
library(stringr)      # for Greater Manchester filter
library(sf)           # for st_read
library(ggplot2)     
library(dplyr)        
library(janitor)      # for clean names
library(plyr)         # for join all
library(ggpubr)       # correlation test
library(ggrepel)      # repel labels
library(tidygeocoder) # transform postcodes to lat lon
library(remotes)
library(readxl)
library(missForest)
library(ggalt)        # for geom_dumbbell
library(forcats)
library(PostcodesioR)
library(scales)
library(extrafont)
library(showtext)     # render chart fonts
library(systemfonts)  # register fonts for charts

###################### set working directory

setwd(dir = "************")

###################### load data

#### maps and lookups
lsoa_map11 <- st_read('LSOA_2011_EW_BFE_V3.shp')
ward_convert <- fread(file= 'LSOA_(2021)_to_Ward_(2023)_to_LAD_(2023).csv')
lsoa11_ward <- fread(file= 'LSOA11_WD20_LAD20_EW_LU_v2.csv')
ward <- st_read('bdline_essh_gb/Data/GB/district_borough_unitary_ward_region.shp')
pc2lsoa <- fread(file = 'postcode_to_lsoa.csv') #postcode to lsoa11nm
pc2lsoa2 <- fread(file = 'postcode_OA_LSOA_MSOA_lookup.csv') #postcode to lsoa11nm
la_shape <- st_read('Local_Authority_Districts_December_2011_GB_BGC.shp')

#### measures
churn <- fread(file = 'hh_churn_lsoa11_2023.csv')
rmd <- fread(file = 'MEP_RMD_DORM_to2023/RMD_97-23/RMD_out_LSOA_releasedata.csv')
ethn_13 <- fread(file = 'MEP_RMD_DORM_to2023/MEP_LSOA/MEP_lsoa11_2013.csv')
ethn_22 <- fread(file = 'MEP_RMD_DORM_to2023/MEP_LSOA/MEP_lsoa11_2022.csv')
hp <- fread(file = 'house_price.csv')
hp_avg <- read.csv(file = 'hp_clean.csv') %>% select(-1) #imputed and cleaned dataset
wage12 <- read_excel(path = 'wage1112.xls', sheet = "Total weekly income", skip = 4)
wage20 <- read_excel(path = 'wage20.xlsx', sheet = "Total annual income", skip = 4)

###################### set up filter for Greater Manchester

great_manc <- c("Manchester", "Wigan", "Bolton", "Bury", "Rochdale", "Oldham", "Salford", "Trafford", "Tameside", "Stockport")
GM_WARDS <- c("MANCHESTER", "WIGAN", "BOLTON", "BURY", "ROCHDALE", "OLDHAM", "SALFORD", "TRAFFORD", "TAMESIDE", "STOCKPORT")

# get Greater Manchester LSOA codes and names
great_manc_lsoacd <- lsoa_map11 %>% 
  st_drop_geometry() %>% 
  filter(str_detect(LSOA11NM, paste(great_manc, collapse = "|"))) %>% 
  select(LSOA11CD,LSOA11NM)

###################### prepare data

#### RMD measure
rmd <- rmd %>%
  row_to_names(row_number = 1)

# standardise LSOA code column
names(rmd)[1] <- "LSOA11CD"

# remove redundant columns
rmd <- rmd %>%
  select(-c(2:17))

# convert values to numeric
rmd <- rmd %>% 
  mutate_at(2:12, ~ as.numeric(.), na.rm=TRUE)

# change column naming convention
colnames(rmd)[2:12] <- paste("out", colnames(rmd[,c(2:12)]), sep = "")

# get gentrification measure
rmd <- rmd %>%
  rowwise() %>%
  dplyr::mutate(rmd_avg = mean(c_across(out2013:out2023), na.rm = TRUE)) %>%
  ungroup() %>% 
  dplyr::mutate(rmd_z = (rmd_avg - mean(rmd_avg, na.rm = TRUE)) / sd(rmd_avg, na.rm = TRUE)) 

# select relevant columns
rmd <- rmd %>% 
  select(LSOA11CD, rmd_avg, rmd_z)


#### map
names(pc2lsoa)[1] <- "postcode"

lsoa_map <- lsoa_map %>%
  select(-c(3))

lsoa_map11 <- lsoa_map11 %>%
  select(c(1,2,9))


#### population churn measure

# standardise LSOA code column
names(churn)[1] <- "LSOA11CD"

# select relevant columns
churn <- churn %>%
  select(c(1,18))


#### change in BAME population measure

# standardise LSOA code column
names(ethn_13)[1] <- "LSOA11CD"
names(ethn_22)[1] <- "LSOA11CD"

# set BAME ethnicities
ethn_13$bame_13 <- ethn_13$aao + ethn_13$abd + ethn_13$acn + ethn_13$ain + ethn_13$apk + ethn_13$baf + ethn_13$bca + ethn_13$oxx + ethn_13$unknown
ethn_22$bame_22 <- ethn_22$aao + ethn_22$abd + ethn_22$acn + ethn_22$ain + ethn_22$apk + ethn_22$baf + ethn_22$bca + ethn_22$oxx + ethn_22$unknown

# join 2013 and 2022 data
ethn <- ethn_13 %>% left_join(ethn_22, by = join_by(LSOA11CD)) %>% 
  dplyr::mutate(bame_ppt = bame_22 - bame_13) %>% 
  select(LSOA11CD, bame_22, bame_13, bame_ppt)


#### house price measure
# impute values for Greater Manchester only because it takes to long to impute values for all of England

# clean
hp <- hp %>% clean_names()
colnames(hp)[1:50] <- gsub("year_ending_", "hp", colnames(hp)[1:50])

# replace ":" with NULL for columns 2 to 50
hp[, 2:50] <- lapply(hp[, 2:50], function(x) ifelse(grepl(":", x), NA, x))

# replace , with NULL for columns 2 to 50
hp[, 2:50] <- lapply(hp[, 2:50], function(x) gsub(",", "", x))

# convert columns 2 to 50 to numeric
hp[, 2:50] <- lapply(hp[, 2:50], function(x) as.numeric(as.character(x)))

# filter for greater manchester
hp_gm <- hp %>%
  filter(str_detect(lsoa, paste(great_manc, collapse = "|")))

# remove LSOA column to prepare for missForest imputation
hp_imp <- hp_gm %>% select(-lsoa)

# calculate the total number of NAs in the entire dataframe
total_na <- sum(is.na(hp_imp))
total_na2013 <- sum(is.na(hp_imp$hpmar_2013))
total_na2023 <- sum(is.na(hp_imp$hpmar_2023))

# calculate the total number of elements in the dataframe
total_elements <- prod(dim(hp_imp))  # Number of rows * Number of columns

# calculate the percentage of NAs overall
na_percentage_overall <- (total_na / total_elements) * 100

# view the result - 3.41%
na_percentage_overall

# impute missing hp values
set.seed(124)  # for reproducibility
imputed_data <- missForest(hp_imp,
                           maxiter = 10,
                           ntree = 300,
                           variablewise = FALSE,
                           decreasing = FALSE,
                           verbose = TRUE,
                           mtry = floor(sqrt(ncol(hp_imp))),
                           replace = TRUE,
                           classwt = NULL,
                           cutoff = NULL,
                           strata = NULL,
                           sampsize = NULL,
                           nodesize = NULL,
                           maxnodes = NULL,
                           xtrue = NA,
                           parallelize = "no")

# check results
imputed_hp <- imputed_data$ximp
imputation_error <- imputed_data$OOBerror
print(imputed_hp)
print(imputation_error)

# NRMSE = 0.223285

# rejoin LSOA codes
lsoa_column <- hp_gm %>% select(lsoa)
imputed_hp$LSOA11NM <- lsoa_column

# create the house price measure
hp_avg <- imputed_hp %>%
  dplyr::mutate(hp_change = (hpmar_2023 - hpmar_2013)/hpmar_2013) %>%
  select(LSOA11NM, hp_change, hpmar_2013,
         hpmar_2023) %>%
  dplyr::mutate(hp_change_z = (hp_change - mean(hp_change, na.rm = TRUE)) / sd(hp_change, na.rm = TRUE))

# save and load this instead of running the imputation again
write.csv(x = hp_avg, file = "hp_clean.csv")


#### wage measure *at MSOA level*

# clean names
wage12 <- wage12 %>% clean_names()
wage20 <- wage20 %>% clean_names()

# join
wage1220 <- wage12 %>% left_join(wage20, by = join_by(msoa_code, msoa_name, 
                                                      local_authority_code, 
                                                      local_authority_name, 
                                                      region_code, 
                                                      region_name)) %>% 
  select(1,2,7,11) %>% 
  dplyr::mutate(annual_income_2012 = total_weekly_income*52,
         annual_income_2020 = total_annual_income) %>% 
  select(-4) %>% 
  dplyr::mutate(wage_change = (annual_income_2020-annual_income_2012)/annual_income_2012,
                wage_change_z = (wage_change - mean(wage_change, na.rm = TRUE)) / sd(wage_change, na.rm = TRUE))

###################### merge gentrification measure data

eng <- join_all(list(churn, 
                     rmd,
                     ethn,
                     hp_avg,
                     great_manc_lsoacd), by='LSOA11CD', type='left') %>% 
  select(-8)  #remove LSOA name duplicate column

# isolate msoa code to join wage data
eng <- eng %>%
  dplyr::mutate(msoa_name = str_sub(LSOA11NM, 1, -2))     

# join wage
eng <- eng %>%
  left_join(wage1220, by = "msoa_name")

# remove redundant columns
eng <- eng %>% 
  select(LSOA11CD, 
         LSOA11NM,
         msoa_code,
         bame_22,
         bame_13,
         bame_ppt,
         rmd_avg,
         rmd_z,
         hpmar_2013,
         hpmar_2023,
         hp_change,
         hp_change_z,
         annual_income_2012,
         annual_income_2020,
         wage_change,
         wage_change_z,
         chn2013) 

# get z-scores churn and ethnicity measures
eng <- eng %>% 
  dplyr::mutate(bame_z = (bame_ppt - mean(bame_ppt, na.rm = TRUE)) / sd(bame_ppt, na.rm = TRUE),
         churn_z = (chn2013 - mean(chn2013, na.rm = TRUE)) / sd(chn2013, na.rm = TRUE),
         bame_z_neg = bame_z*(-1)) 

#### filter England for Manchester 
manc <- eng %>%
  filter(str_detect(LSOA11NM, paste(great_manc, collapse = "|")))

###################### get Gentrification Index data set

# combine measures in weighted average as set out in the methodology
gent <- manc %>% 
  dplyr::mutate(
    gi = (0.2*bame_z_neg + 0.1*rmd_z + 0.1*hp_change_z + 0.2*wage_change_z + 0.4*churn_z)
  )

# scale gentrification score between 0 and 1 and multiply by 100 for final index score
gent$gi_n <- (scale(gent$gi, center = min(gent$gi, na.rm = T), scale = max(gent$gi, na.rm = T) - min(gent$gi, na.rm = T)))*100

# save
write.csv(gent, file = 'Manchester Gentrification/gent_index.csv')


###################### analysis

# load Gentrification Index database
gent <- read.csv(file = 'Manchester Gentrification/gent_index.csv')

# create a new bin variable
gent$gi_n_bin <- cut(gent$gi_n, breaks = seq(0, 100, by = 10), include.lowest = TRUE, labels = FALSE)

#### summary tables

# join wards
to_ward <- lsoa11_ward %>% select(1,4,5, 6)

# summary data table
manc_sum <- gent %>%
  left_join(to_ward) %>% 
  st_drop_geometry()

# ward level summary table
ward_level <- manc_sum %>%
  group_by(WD20NM) %>% 
  dplyr::mutate(
    ward_gi_n = mean(gi_n),
    ward_rmd = mean(rmd_avg),
    ward_bame = mean(bame_ppt),
    ward_hp = mean(hp_change),
    ward_wage = mean(wage_change),
    ward_churn = mean(chn2013),
    lsoa_n = n_distinct(LSOA11CD)) %>% 
  ungroup() %>% 
  select(WD20NM, LAD20NM, ward_gi_n, ward_rmd, ward_bame, ward_hp, ward_wage, ward_churn, lsoa_n) %>%
  distinct()

# borough level summary table
borough_level <- manc_sum %>% 
  group_by(LAD20NM) %>% 
  dplyr::mutate(
    b_gi = mean(gi_n),
    b_rmd = mean(rmd_avg),
    b_poc = mean(bame_ppt),
    b_hp = mean(hp_change),
    b_wage = mean(wage_change),
    b_churn = mean(chn2013),
    lsoa_n = n_distinct(LSOA11CD),
    ward_n = n_distinct(WD20NM),
    min = min(gi_n),
    max = max(gi_n)) %>% 
  ungroup() %>% 
  select(LAD20NM,  
         b_gi,b_rmd,b_poc,b_hp,b_wage,b_churn,lsoa_n,ward_n,min,max) %>% 
  distinct()

# Greater Manchester stats summary
gm_sum <- gent 

# select the relevant columns
selected_columns <- gm_sum[, c("gi_n", "bame_ppt", "rmd_avg", "hp_change", "wage_change", "chn2013")]

# calculate specific summary statistics
summary_table <- data.frame(
  Variable = c("gi_n", "bame_ppt", "rmd_avg", "hp_change", "wage_change", "chn2013"),
  Min = sapply(selected_columns, min),
  Lower_Quartile = sapply(selected_columns, function(x) quantile(x, 0.25)),
  Median = sapply(selected_columns, median),
  Upper_Quartile = sapply(selected_columns, function(x) quantile(x, 0.75)),
  Max = sapply(selected_columns, max),
  Mean = sapply(selected_columns, mean)
)

# save
write.csv(manc_sum, file = 'lsoa_summary.csv')
write.csv(ward_level, file = 'ward_summary.csv')
write.csv(LA_level, file = 'borough_summary.csv')
write.csv(summary_table, file = "gm_summary_stats.csv", row.names = FALSE)
