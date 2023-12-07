###Import data

install.packages("readxl")
library("readxl")

# Set working directory

setwd("C:\\Users\\hanna\\PycharmProjects\\INFO698\\Data\\Tables")

# Import final Indian Reservations table

indian_reservations <- read_xlsx("indian_reservations.xlsx", sheet = 1)

# Make table into data frame

df_res <- as.data.frame(indian_reservations)

# Import NPL site boundary tables

ic_boundaries <- read_xlsx("ic_boundaries.xlsx", sheet = 1)
ou_boundaries <- read_xlsx("ou_boundaries.xlsx", sheet = 1)
site_boundaries <- read_xlsx("site_boundaries.xlsx", sheet = 1)

# Make tables into data frames

df_IC <- as.data.frame(ic_boundaries)
df_OU <- as.data.frame(ou_boundaries)
df_SITE <- as.data.frame(site_boundaries)

# Import reservation vs sites distances, reservation vs OU distances, and reservation vs IC distances

ic_distance <- read_xlsx("ic_distance.xlsx", sheet = 1)
ou_distance <- read_xlsx("ou_distance.xlsx", sheet = 1)
site_distance <- read_xlsx("site_distance.xlsx", sheet = 1)

# Make tables into data frames

df_IC_dist <- as.data.frame(ic_distance)
df_OU_dist <- as.data.frame(ou_distance)
df_SITE_dist <- as.data.frame(site_distance)

### Merge data sets

# Merge df_IC and df_IC_dist
IC <- merge(df_IC_dist, df_IC, by.x = "IN_FID", by.y = "OBJECTID", all.x = TRUE, all.y = FALSE)


# Merge df_OU and df_IC_dist
OU <- merge(df_OU_dist, df_OU, by.x = "IN_FID", by.y = "OBJECTID", all.x = TRUE, all.y = FALSE)

# Merge df_SITES and df_site_dist
SITE <- merge(df_SITE_dist, df_SITE, by.x = "IN_FID", by.y = "OBJECTID", all.x = TRUE, all.y = FALSE)

# Combine the three tables

all_site_dist <- rbind(IC, OU, SITE)

# Round latitude and longitude to 4 decimal places

all_site_dist <- all_site_dist %>% mutate(across(c('Cen_x', 'Cen_y'), round, 4)) %>% mutate(key = paste(SITE_NAME, SITE_FEA_4))


### Remove duplicates

library("dplyr")
options(scipen = 9999)

# Use slicing to remove duplicates

site_max <- all_site_dist %>% group_by(SITE_NAME) %>% slice(which.max(GIS_AREA))
  
all_site_dist_final <- all_site_dist %>% filter(key %in% site_max$key)

# List original field names

colnames(all_site_dist)

# NEAR_FID, NEAR_DIST, NEAR_RANK, REGION_COD, EPA_PROGRA, EPA_ID, SITE_NAME, NPL_STATUS, FEDERAL_FA, LAST_CHANG, ORIGINAL_C, STREET_ADD, ADDR_COMME, CITY_NAME, COUNTY, STATE_CODE, ZIP_CODE, SITE_CONTA, PRIMARY_TE, SITE_CON_1, URL_ALIAS_, FEATURE_IN, FEATURE__1, CLEARED_PU, HORIZ_COLL, TIER_ACCUR, GIS_AREA, GIS_AREA_U, Shape_Leng, Shape_Area, Cen_x, Cen_y

# Group by coordinates and site name and tally

remove_duplicates <- all_site_dist_final %>% group_by(NEAR_FID, NEAR_DIST, NEAR_RANK, key, SITE_NAME, GIS_AREA, EPA_ID) %>% tally()

# See how many duplicates are remaining

remaining_duplicates <- remove_duplicates %>% group_by(SITE_NAME) %>% tally() %>% filter(n != 10)


#### Download file

write.csv(remove_duplicates, "site_dist.csv")
