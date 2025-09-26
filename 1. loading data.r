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

indonesia_2012_individual_raw <- read_dta("Indonesia 2012.DTA")
indonesia_2012_individual_clean <- indonesia_2012_individual_raw %>% mutate(country = "indonesia")

indonesia_2017_individual_raw <- read_dta("Indonesia 2017.DTA")
indonesia_2017_individual_clean <- indonesia_2017_individual_raw %>% mutate(country = "indonesia")

myanmar_2015_individual_raw <- read_dta("Myanmar 2015-16.DTA")
myanmar_2015_individual_clean <- myanmar_2015_individual_raw %>% mutate(country = "myanmar")

philippines_2013_individual_raw <- read_dta("Philippines 2013.DTA")
philippines_2013_individual_clean <- philippines_2013_individual_raw %>% mutate(country = "philippines")

philippines_2017_individual_raw <- read_dta("Philippines 2017.DTA")
philippines_2017_individual_clean <- philippines_2017_individual_raw %>% mutate(country = "philippines")

philippines_2022_individual_raw <- read_dta("Philippines 2022.DTA")
philippines_2022_individual_clean <- philippines_2022_individual_raw %>% mutate(country = "philippines")

timorleste_2016_individual_raw <- read_dta("Timor Leste 2016.DTA")
timorleste_2016_individual_clean <- timorleste_2016_individual_raw %>% mutate(country = "timor leste")

# variables to clean
vars_to_clean <- c("v000", "v006", "v007", "v009", "v010", "v012", "v013", "v106", "v130", "v137", "v140", "v155", "v213", "v228", "v501", "v502", "v511", "v525", "v531", "v761", "s826f", "v744a", "v744b", "v744c", "v744d", "v744e", "v850a", "v783", "v781", "v828", "d104", "d106", "d107", "d108", "d128")

# variables to keep
vars_to_keep <- c("v000", "v006", "v007", "v009", "v010", "v012", "v013", "v106", "v130", "v137", "v140", "v155", "v213", "v228", "v501", "v502", "v511", "v525", "v531", "v761", "s826f", "v744a", "v744b", "v744c", "v744d", "v744e", "v850a", "v783", "v781", "v828", "country", "d104", "d106", "d107", "d108", "d128")

# dataframe list
southeast_asia_dfs <- c("cambodia_2010_individual_clean", "cambodia_2014_individual_clean", "cambodia_2021_individual_clean",
                        "indonesia_2012_individual_clean", "indonesia_2017_individual_clean", "myanmar_2015_individual_clean",
                        "philippines_2013_individual_clean", "philippines_2017_individual_clean", "philippines_2022_individual_clean",
                        "timorleste_2016_individual_clean")

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

# standardise religion variable
for (df_name in southeast_asia_dfs) {
  df <- get(df_name)
  
  country_name <- unique(df$country)[1]
  
  df <- df %>%
    mutate(v130_standard = case_when(
      # Cambodia, Indonesia, Myanmar
      country_name %in% c("cambodia", "indonesia", "myanmar") & v130 == 1 ~ "buddhist",
      country_name %in% c("cambodia", "indonesia", "myanmar") & v130 == 2 ~ "muslim",
      country_name %in% c("cambodia", "indonesia", "myanmar") & v130 == 3 ~ "christian",
      country_name %in% c("cambodia", "indonesia", "myanmar") & v130 == 95 ~ "no religion",
      country_name %in% c("cambodia", "indonesia", "myanmar") & v130 == 96 ~ "other",
      country_name %in% c("cambodia", "indonesia", "myanmar") & v130 == 99 ~ "missing",
      
      # Philippines
      country_name == "philippines" & v130 %in% c(1, 2, 3, 4, 6) ~ "christian", # catholic, protestant, iglesia ni cristo, aglipay, other christian
      country_name == "philippines" & v130 == 5 ~ "muslim",
      country_name == "philippines" & v130 %in% c(7, 97) ~ "no religion",
      country_name == "philippines" & v130 == 96 ~ "other",
      country_name == "philippines" & v130 == 99 ~ "missing",
      
      # Timor Leste
      country_name == "timor leste" & v130 == 1 ~ "christian", # roman catholic
      country_name == "timor leste" & v130 == 2 ~ "muslim",
      country_name == "timor leste" & v130 == 3 ~ "christian", # protestant
      country_name == "timor leste" & v130 == 4 ~ "other", # hindu
      country_name == "timor leste" & v130 == 96 ~ "other",
      
      # any other values
      TRUE ~ "missing"
    ))
  
  assign(df_name, df)
}

# combine dataframes
southeast_asia_combined <- bind_rows(
  cambodia_2010_individual_clean,
  cambodia_2014_individual_clean,
  cambodia_2021_individual_clean,
  indonesia_2012_individual_clean,
  indonesia_2017_individual_clean,
  myanmar_2015_individual_clean,
  philippines_2013_individual_clean,
  philippines_2017_individual_clean,
  philippines_2022_individual_clean,
  timorleste_2016_individual_clean
)

# save data
write_xlsx(southeast_asia_combined, "../code/southeast_asia_combined_dataset.xlsx")