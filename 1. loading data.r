# load packages
pacman::p_load(dplyr, tidyr, haven, purrr, tableone, broom, writexl, stringr)

# set working directory
setwd("C:/Users/vl22683/OneDrive - University of Bristol/Documents/Publications/DHS and violence paper/data/")

# load raw data
cambodia_2010_individual_raw <- read_dta("Cambodia 2010.DTA")
cambodia_2010_individual_clean <- cambodia_2010_individual_raw %>% mutate(country = "cambodia")

cambodia_2014_individual_raw <- read_dta("Cambodia 2014.DTA")
cambodia_2014_individual_clean <- cambodia_2014_individual_raw %>% mutate(country = "cambodia")

cambodia_2021_individual_raw <- read_dta("Cambodia 2021-22.DTA")
cambodia_2021_individual_clean <- cambodia_2021_individual_raw %>% mutate(country = "cambodia")

philippines_2013_individual_raw <- read_dta("Philippines 2013.DTA")
philippines_2013_individual_clean <- philippines_2013_individual_raw %>% mutate(country = "philippines")

philippines_2017_individual_raw <- read_dta("Philippines 2017.DTA")
philippines_2017_individual_clean <- philippines_2017_individual_raw %>% mutate(country = "philippines")

philippines_2022_individual_raw <- read_dta("Philippines 2022.DTA")
philippines_2022_individual_clean <- philippines_2022_individual_raw %>% mutate(country = "philippines")

# variables to clean
vars_to_clean <- c("v000", "v006", "v007", "v009", "v010", "v012", "v013", "d113", "v106", "v130", "v137", "v140", "v150", "v155", "v213", "v228", "v501", "v502", "v511", "v525", "v531", "v717", "v761", "s826f", "v744a", "v744b", "v744c", "v744d", "v744e", "v850a", "v783", "v781", "v828", "d104", "d106", "d107", "d108", "d128")

# variables to keep
vars_to_keep <- c("v000", "v006", "v007", "v009", "v010", "v012", "v013", "d113", "v106", "v130", "v137", "v140", "v150", "v155", "v213", "v228", "v501", "v502", "v511", "v525", "v531", "v717", "v761", "s826f", "v744a", "v744b", "v744c", "v744d", "v744e", "v850a", "v783", "v781", "v828", "country", "d104", "d106", "d107", "d108", "d128")

# dataframe list
southeast_asia_dfs <- c("cambodia_2010_individual_clean", "cambodia_2014_individual_clean", "cambodia_2021_individual_clean",
                        "philippines_2013_individual_clean", "philippines_2017_individual_clean", "philippines_2022_individual_clean")

# convert variables to numeric
for (df_name in southeast_asia_dfs) {
  df <- get(df_name)
  df <- df %>%
    mutate(across(any_of(vars_to_clean), as.numeric))
  assign(df_name, df)
}

# keep variables of interest
for (df_name in southeast_asia_dfs) {
  df <- get(df_name)
  df <- df %>%
    select(any_of(vars_to_keep))
  assign(df_name, df)
}

# combine dataframes
southeast_asia_combined <- bind_rows(
  cambodia_2010_individual_clean,
  cambodia_2014_individual_clean,
  cambodia_2021_individual_clean,
  philippines_2013_individual_clean,
  philippines_2017_individual_clean,
  philippines_2022_individual_clean
)

# standardise religion variable
southeast_asia_combined <- southeast_asia_combined %>%
  mutate(v130_standard = case_when(
    # Cambodia
    country == "cambodia" & v130 == 1 ~ "buddhist",
    country == "cambodia" & v130 == 2 ~ "muslim",
    country == "cambodia" & v130 == 3 ~ "christian",
    country == "cambodia" & v130 == 95 ~ "no religion",
    country == "cambodia" & v130 == 96 ~ "other",
    country == "cambodia" & v130 == 99 ~ "missing",
    
    # Philippines
    country == "philippines" & v130 == 1 ~ "roman catholic",
    country == "philippines" & v130 == 2 ~ "protestant",
    country == "philippines" & v130 == 3 ~ "iglesia ni cristo",
    country == "philippines" & v130 == 4 ~ "aglipay",
    country == "philippines" & v130 == 5 ~ "muslim",
    country == "philippines" & v130 == 6 ~ "other christian",
    country == "philippines" & v130 %in% c(7, 97) ~ "no religion",
    country == "philippines" & v130 == 96 ~ "other",
    country == "philippines" & v130 == 99 ~ "missing",
    
    # any other values
    TRUE ~ "missing"
  ))

# binary hiv testing outcome
southeast_asia_combined <- southeast_asia_combined %>%
  filter(!is.na(v781)) %>%
  mutate(v781_binary = case_when(
    v781 == 1 ~ 1,  
    v781 == 0 ~ 0,  
    TRUE ~ NA_real_ 
  )) %>%
  filter(!is.na(v781_binary))

# lowercase country
analysis_data <- southeast_asia_combined %>%
  mutate(country = tolower(as.character(country))) %>%
  filter(!is.na(v781)) %>%
  mutate(
    v781_binary = dplyr::case_when(
      v781 == 1 ~ 1,
      v781 == 0 ~ 0,
      TRUE ~ NA_real_
    )
  ) %>%
  filter(!is.na(v781_binary))

# standardise household relationship variable
southeast_asia_combined <- southeast_asia_combined %>%
  mutate(v150_standard = case_when(
    v150 == 1 ~ "head",
    v150 == 2 ~ "spouse",
    v150 %in% c(3, 4, 11) ~ "child",
    v150 %in% c(5, 6, 7, 8, 10, 15) ~ "other relative",
    v150 == 12 ~ "not related",
    TRUE ~ "missing"
  ))

# binary occupation variable
southeast_asia_combined <- southeast_asia_combined %>%
  mutate(v717_standard = case_when(
    v717 == 0 ~ "not employed",
    v717 %in% c(1, 2, 3, 4, 7, 8, 9) ~ "employed",
    v717 == 98 ~ "don't know",
    TRUE ~ "missing"
  ))

# create three age cat
southeast_asia_combined <- southeast_asia_combined %>%
  mutate(
    age_3cat = case_when(
      v013 %in% 1:2 ~ "15-24",
      v013 %in% 3:5 ~ "25-39",
      v013 %in% 6:7 ~ "40+",
      TRUE ~ NA_character_
    ))

# make violence experiences binary
southeast_asia_combined <- southeast_asia_combined %>%
  mutate(
    emotional_violence_bin    = factor(case_when(d104 == 1 ~ 1, d104 == 0 ~ 0, TRUE ~ NA_real_), levels = c(0, 1), labels = c("No", "Yes")),
    less_severe_violence_bin  = factor(case_when(d106 == 1 ~ 1, d106 == 0 ~ 0, TRUE ~ NA_real_), levels = c(0, 1), labels = c("No", "Yes")),
    severe_violence_bin       = factor(case_when(d107 == 1 ~ 1, d107 == 0 ~ 0, TRUE ~ NA_real_), levels = c(0, 1), labels = c("No", "Yes")),
    sexual_violence_bin       = factor(case_when(d108 == 1 ~ 1, d108 == 0 ~ 0, TRUE ~ NA_real_), levels = c(0, 1), labels = c("No", "Yes")),
    any_violence = factor(case_when(
      emotional_violence_bin == "Yes" | less_severe_violence_bin == "Yes" |
      severe_violence_bin == "Yes" | sexual_violence_bin == "Yes" ~ 1,
      emotional_violence_bin == "No" & less_severe_violence_bin == "No" &
      severe_violence_bin == "No" & sexual_violence_bin == "No" ~ 0,
      TRUE ~ NA_real_
    ), levels = c(0, 1), labels = c("No", "Yes"))
  )

# create number of children cat
southeast_asia_combined <- southeast_asia_combined %>%
  mutate(
    v137_cat = case_when(
      v137 == 0 ~ "0",
      v137 == 1 ~ "1",
      v137 == 2 ~ "2",
      v137 >= 3 ~ "3+",
      TRUE ~ NA_character_
    ),
    v137_cat = factor(v137_cat, levels = c("0", "1", "2", "3+"))
  )

# create binary acceptability questions
southeast_asia_combined <- southeast_asia_combined %>%
  mutate(
    justifies_dv_condom_bin         = case_when(s826f  == 1 ~ 1, s826f  == 0 ~ 0, TRUE ~ NA_real_),
    beating_justified_out_bin       = case_when(v744a  == 1 ~ 1, v744a  == 0 ~ 0, TRUE ~ NA_real_),
    beating_justified_neglect_bin   = case_when(v744b  == 1 ~ 1, v744b  == 0 ~ 0, TRUE ~ NA_real_),
    beating_justified_argue_bin     = case_when(v744c  == 1 ~ 1, v744c  == 0 ~ 0, TRUE ~ NA_real_),
    beating_justified_refuse_sex_bin= case_when(v744d  == 1 ~ 1, v744d  == 0 ~ 0, TRUE ~ NA_real_),
    beating_justified_burn_food_bin = case_when(v744e  == 1 ~ 1, v744e  == 0 ~ 0, TRUE ~ NA_real_),
    can_refuse_sex_bin              = case_when(v850a  == 1 ~ 1, v850a  == 0 ~ 0, TRUE ~ NA_real_),
    beating_justified_bin = case_when(
      beating_justified_out_bin == 1 | beating_justified_neglect_bin == 1 |
      beating_justified_argue_bin == 1 | beating_justified_refuse_sex_bin == 1 |
      beating_justified_burn_food_bin == 1 ~ 1,
      beating_justified_out_bin == 0 & beating_justified_neglect_bin == 0 &
      beating_justified_argue_bin == 0 & beating_justified_refuse_sex_bin == 0 &
      beating_justified_burn_food_bin == 0 ~ 0,
      TRUE ~ NA_real_
    )
  )

# convert confounders to factors
southeast_asia_combined <- southeast_asia_combined %>%
  mutate(
    v013 = factor(v013, levels = 1:7,
                  labels = c("15-19", "20-24", "25-29", "30-34", "35-39", "40-44", "45-49")),
    v140 = factor(v140, levels = c(1, 2, 7),
                  labels = c("urban", "rural", "not de jure resident")),
    v213 = factor(v213, levels = c(0, 1),
                  labels = c("no or unsure", "yes"))
  )

# tab
table(southeast_asia_combined$v130_standard, southeast_asia_combined$country, useNA = "ifany")
table(southeast_asia_combined$v150_standard, southeast_asia_combined$country, useNA = "ifany")
table(southeast_asia_combined$v717_standard, southeast_asia_combined$country, useNA = "ifany")

# save data
write_xlsx(southeast_asia_combined, "../data/southeast_asia_combined_dataset.xlsx")
saveRDS(southeast_asia_combined, "../data/southeast_asia_combined_dataset.rds")
