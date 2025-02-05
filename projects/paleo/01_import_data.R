## Imports data from csv table and stores them as rds files.
source('source/paleo.R')

library(stringr)
library(dplyr)

#### Europe ####
recon_summary <- read.csv("/mnt/shared/data/paleo/processed/pratap_thesis/recon_meta_eu.csv")
recon_data <- read.csv("/mnt/shared/data/paleo/processed/pratap_thesis/recon_data_eu.csv")

### Assign recon_id ####
# Add first four letters in recon_id from selected columns
# year from investigators 
extract_year <- function(investigators) {
  sapply(str_extract_all(investigators, "\\d{4}"), toString)
}

# From each rest of element
first_four <- function(x) {
  substr(x, 1, 4)
}


data_id <- recon_summary %>%
  mutate(loc_part = first_four(location),
         #frac_part = first_four(fractions),
         var_part = first_four(proxy),
         year_part = extract_year(investigators),
         recon_id = paste(loc_part, year_part, var_part, sep = "_") #frac_part, var_part, 
  ) %>%
  dplyr::select(recon_id)

head(data_id)

# Add recon_id to meta data
recon_summary <- cbind(data_id, recon_summary) %>%
  dplyr::select(-investigators)%>%
  filter(time_step != "varying")%>%
  filter(variable != "Temp")

recon_summary

### Add recon_id in main reconstruction data ###
additional_columns <- recon_summary %>%
  dplyr::select(recon_id, location)

final_recon_data <- recon_data %>%
  left_join(additional_columns, by = "location")%>%
  filter(variable != "Temp")

######################
final_recon <- final_recon_data %>%
  filter(time >= 1958) %>%   
  # filter(time >= 1959 & time <= 2019) %>%
  #Remove rows where the recon_id appears less than 5 times
  group_by(recon_id) %>%
  filter(n() >= 5) %>%
  ungroup() %>%
  dplyr::select(recon_id, time, value)  %>%
  mutate(time = ifelse((time - floor(time)) >= 0.5, ceiling(time), floor(time))) %>%
  mutate(value = round(value, 2))

final_recon_df <- data.frame(final_recon)

final_recon_df

############

# save(recon_summary, paste0(PATH_SAVE_PALEO, "recon_summary.Rdata"))
# saveRDS(recon_data, paste0(PATH_SAVE_PALEO, "recon_data.rds"))

save(recon_summary, file = "~/shared/data_projects/ithaca/paleo/recon_summary.Rdata")
saveRDS(final_recon_df, file = "~/shared/data_projects/ithaca/paleo/recon_data.rds")



######################################################################################################


#### North America ####

rm(list = ls(all.names = TRUE))

## Imports data from csv table and stores them as rds files.
source('source/paleo.R')

library(stringr)
library(dplyr)

recon_summary_nam <- read.csv("/mnt/shared/data/paleo/processed/pratap_thesis/recon_meta_nam_p.csv")
recon_data_nam <- read.csv("/mnt/shared/data/paleo/processed/pratap_thesis/recon_data_nam_p.csv")

### Assign recon_id ####
# Add first four letters in recon_id from selected columns
# year from investigators 
extract_year_nam <- function(investigators) {
  sapply(str_extract_all(investigators, "\\d{4}"), toString)
}

# From each rest of element
first_four_nam <- function(x) {
  substr(x, 1, 4)
}


data_id_nam <- recon_summary_nam %>%
  mutate(loc_part = first_four_nam(location),
         #frac_part = first_four(fractions),
         var_part = first_four_nam(proxy),
         year_part = extract_year_nam(investigators),
         recon_id = paste(loc_part, year_part, var_part, sep = "_") #frac_part, var_part, 
  ) %>%
  dplyr::select(recon_id)

head(data_id_nam)

# Add recon_id to meta data
recon_summary_nam <- cbind(data_id_nam, recon_summary_nam) %>%
  dplyr::select(-investigators)%>%
  filter(time_step != "varying")%>%
  filter(variable != "Temp")

recon_summary_nam


### Add recon_id in main reconstruction data ###
additional_columns_nam <- recon_summary_nam %>%
  dplyr::select(recon_id, location)

final_recon_data_nam <- recon_data_nam %>%
  left_join(additional_columns_nam, by = "location")%>%
  filter(variable != "Temp")

######################

final_recon_nam <- final_recon_data_nam %>%
  # filter(time >= 1959 & time <= 2019) %>%
  #Remove rows where the recon_id appears less than 5 times
  group_by(recon_id) %>%
  filter(n() >= 5) %>%
  ungroup() %>%
  dplyr::select(recon_id, time, value)  %>%
  #mutate(time = ceiling(time)) %>%
  mutate(time = ifelse((time - floor(time)) >= 0.5, ceiling(time), floor(time))) %>%
  mutate(value = round(value, 2))%>%
  as.data.frame()

final_recon_nam


############

# save(recon_summary, paste0(PATH_SAVE_PALEO, "recon_summary.Rdata"))
# saveRDS(recon_data, paste0(PATH_SAVE_PALEO, "recon_data.rds"))

save(recon_summary_nam, file = "~/shared/data_projects/ithaca/paleo/recon_summary_nam_p.Rdata")
saveRDS(final_recon_nam, file = "~/shared/data_projects/ithaca/paleo/recon_data_nam_p.rds")


### Analysis #####
# load("~/shared/data_projects/ithaca/paleo/recon_summary.Rdata")

final_recon_rds_nam <- readRDS(file = "~/shared/data_projects/ithaca/paleo/recon_data_nam_p.rds")

results_nam <- final_recon_rds_nam %>%
  # filter(time >= 1959 & time <= 2019) %>%
  filter(time >= 1958) %>%   
  group_by(recon_id) %>%
  dplyr::summarize(
    mean_value = mean(value, na.rm = TRUE),
    sd_value = sd(value, na.rm = TRUE),
    cv = sd_value / mean_value,
    acf_lag1 = acf(value, lag.max = 1)$acf[2],
    slope = coef(lm(value ~ time))[2]) %>%
  as.data.frame()

results_nam

