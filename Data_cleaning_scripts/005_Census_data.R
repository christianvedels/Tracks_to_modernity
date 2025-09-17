# Census data
#
# Date updated:   2025-01-14
# Auhtor:         Christian Vedel, Tom Görges
# Purpose:        Cleans census data

# ==== Libraries ====
library(tidyverse)
library(hisco)

# ==== Load data ====
load("../Data not redistributable/All_raw_data_for_project.Rdata")

hisco_incomes = read_csv("Data/HISCO_coded_income.csv")

# ==== Select variables ====
clean_census = popdata %>% 
  select(
    Year,
    GIS_ID,
    Pop,
    Pop_f,
    Pop_m,
    Age_mean,
    Age_1_4,
    Age_15_24_f, Age_25_34_f, Age_35_44_f,
    Born_different_county,
    hisco_1st_digit0:hisco_1st_digit9
  ) %>% 
  mutate(
    Child_women_ratio = Age_1_4/(Age_15_24_f + Age_25_34_f + Age_35_44_f),
    Manufacturing_789 = hisco_1st_digit7 + hisco_1st_digit8 + hisco_1st_digit9,
    Farming = hisco_1st_digit6
  ) %>% 
  select(
    -c(Age_1_4, Age_15_24_f, Age_25_34_f, Age_35_44_f) # Deselect used stuff
  ) %>% 
  rename(
    hisco_major0 = hisco_1st_digit0,
    hisco_major1 = hisco_1st_digit1,
    hisco_major2 = hisco_1st_digit2,
    hisco_major3 = hisco_1st_digit3,
    hisco_major4 = hisco_1st_digit4,
    hisco_major5 = hisco_1st_digit5,
    hisco_major6 = hisco_1st_digit6,
    hisco_major7 = hisco_1st_digit7,
    hisco_major8 = hisco_1st_digit8,
    hisco_major9 = hisco_1st_digit9
  )

# ==== HISCO codes ====
path = "../Data not redistributable/LL_hisco_codes_clean.csv"
hisco_full = read_csv2(path)

key_gis_id_row_id = read_csv("../Data not redistributable/RowID_GIS_ID_key.csv")

# Convert to ses
hisco = hisco_full %>%
  mutate(
    RowID = paste0(Year,pa_id) %>% as.numeric()
  ) %>%
  left_join(key_gis_id_row_id, by = "RowID") %>% 
  select(hisco1:hisco5, GIS_ID, Year) %>% 
  mutate(
    hisco1 = as.numeric(hisco1),
    hisco2 = as.numeric(hisco2),
    hisco3 = as.numeric(hisco3),
    hisco4 = as.numeric(hisco4),
    hisco5 = as.numeric(hisco5),
  )



# Calculate HISCAM for each HISCO code
hisco = hisco %>% mutate(
  hiscam1 = hisco_to_ses(hisco1, ses = "hiscam_u1"),
  hiscam2 = hisco_to_ses(hisco2, ses = "hiscam_u1"),
  hiscam3 = hisco_to_ses(hisco3, ses = "hiscam_u1"),
  hiscam4 = hisco_to_ses(hisco4, ses = "hiscam_u1"),
  hiscam5 = hisco_to_ses(hisco5, ses = "hiscam_u1")
)

# Add HISCO incomes
hisco_incomes %>% 
  mutate(hisco_1 = as.numeric(hisco_1)) %>%
  group_by(hisco_1) %>% # Weighted by count 'Antal'
  summarise(
    income = weighted.mean(Gns._Indtægt_kr, `Antal (1916)`, na.rm = TRUE)
  ) -> hisco_income_mean

hisco = hisco %>%
  left_join(hisco_income_mean, by = c("hisco1" = "hisco_1")) %>%
  rename(hisco_income_1 = income) %>%
  left_join(hisco_income_mean, by = c("hisco2" = "hisco_1")) %>%
  rename(hisco_income_2 = income) %>%
  left_join(hisco_income_mean, by = c("hisco3" = "hisco_1")) %>%
  rename(hisco_income_3 = income) %>%
  left_join(hisco_income_mean, by = c("hisco4" = "hisco_1")) %>%
  rename(hisco_income_4 = income) %>%
  left_join(hisco_income_mean, by = c("hisco5" = "hisco_1")) %>%
  rename(hisco_income_5 = income)

# Aggregate at the parish level
mean0 = function(x){
  if(all(is.na(x))) return(NA)
  mean(x, na.rm = TRUE)
}

hisco = hisco %>% 
  # sample_n(10000) %>% 
  rowwise() %>% 
  mutate(
    hiscam_avg = mean0(c(hiscam1, hiscam2, hiscam3, hiscam4, hiscam5)),
    income_avg = mean0(c(hisco_income_1, hisco_income_2, hisco_income_3, hisco_income_4, hisco_income_5))
  ) %>% 
  group_by(Year, GIS_ID) %>% 
  summarise(
    hiscam_avg = mean0(hiscam_avg),
    income_avg = mean0(income_avg)
  )

# ==== Join on ses scores ====
clean_census = clean_census %>% 
  left_join(hisco, by = c("Year", "GIS_ID"))

clean_census %>% 
  write_csv2("Data/Census_data.csv")  
