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
  
  # Get country from the dataframe
  country_name <- unique(df$country)[1]
  
  df <- df %>%
    mutate(v130_standard = case_when(
      # For Cambodia, Indonesia, Myanmar (standard coding)
      country_name %in% c("cambodia", "indonesia", "myanmar") & v130 == 1 ~ "buddhist",
      country_name %in% c("cambodia", "indonesia", "myanmar") & v130 == 2 ~ "muslim",
      country_name %in% c("cambodia", "indonesia", "myanmar") & v130 == 3 ~ "christian",
      country_name %in% c("cambodia", "indonesia", "myanmar") & v130 == 95 ~ "no religion",
      country_name %in% c("cambodia", "indonesia", "myanmar") & v130 == 96 ~ "other",
      country_name %in% c("cambodia", "indonesia", "myanmar") & v130 == 99 ~ "missing",
      
      # For Philippines (different coding)
      country_name == "philippines" & v130 %in% c(1, 2, 3, 4, 6) ~ "christian", # catholic, protestant, iglesia ni cristo, aglipay, other christian
      country_name == "philippines" & v130 == 5 ~ "muslim",
      country_name == "philippines" & v130 %in% c(7, 97) ~ "no religion",
      country_name == "philippines" & v130 == 96 ~ "other",
      country_name == "philippines" & v130 == 99 ~ "missing",
      
      # For Timor Leste (different coding)
      country_name == "timor leste" & v130 == 1 ~ "christian", # roman catholic
      country_name == "timor leste" & v130 == 2 ~ "muslim",
      country_name == "timor leste" & v130 == 3 ~ "christian", # protestant
      country_name == "timor leste" & v130 == 4 ~ "other", # hindu
      country_name == "timor leste" & v130 == 96 ~ "other",
      
      # Default for any other values
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

# violence variables
violence_vars <- c("s826f", "v744a", "v744b", "v744c", "v744d", "v744e", "v850a", "d104", "d106", "d107", "d108", "d128")

# binary hiv testing outcome
analysis_data <- southeast_asia_combined %>%
  filter(!is.na(v781)) %>%
  mutate(v781_binary = case_when(
    v781 == 1 ~ 1,  # Yes, tested
    v781 == 0 ~ 0,  # No, not tested
    TRUE ~ NA_real_ # Missing for other values
  )) %>%
  filter(!is.na(v781_binary))

# convert vars from numeric to factors
analysis_data <- analysis_data %>%
  mutate(across(any_of(violence_vars), ~ as.factor(.x)))

# logistic regressions adjusting for country
violence_models <- lapply(violence_vars, function(var) {
  if(var %in% names(analysis_data) && sum(!is.na(analysis_data[[var]])) > 0) {
    formula_str <- paste("v781_binary ~", var, "+ country")
    glm(as.formula(formula_str), data = analysis_data, family = binomial)
  }
})

# store results
names(violence_models) <- violence_vars
violence_results <- lapply(violence_models, function(model) {
  if(!is.null(model)) {
    tidy(model, exponentiate = TRUE, conf.int = TRUE)
  }
})

# variable and level labels
variable_labels <- data.frame(
  violence_variable = c("s826f", "v744a", "v744b", "v744c", "v744d", "v744e", "v850a", "d104", "d106", "d107", "d108", "d128"),
  variable_label = c(
    "Justifies DV: wife ask use condom",
    "Wife beating justified: goes out without telling husband",
    "Wife beating justified: neglects children", 
    "Wife beating justified: argues with husband",
    "Wife beating justified: refuses sex with husband",
    "Wife beating justified: burns the food",
    "Can respondent refuse sex",
    "Experienced any emotional violence",
    "Experienced any less severe violence",
    "Experienced any severe violence", 
    "Experienced any sexual violence",
    "Ever told anyone else about violence"
  )
)

level_labels <- data.frame(
  level = c("0", "1", "8", "9"),
  level_description = c("No", "Yes", "Don't know", "Missing")
)

# results tables
final_results <- bind_rows(violence_results, .id = "violence_variable") %>%
  rename(
    odds_ratio = estimate,
    OR_lower_CI = conf.low,
    OR_upper_CI = conf.high
  ) %>%
  mutate(
    level = case_when(
      term == "(Intercept)" ~ "Reference",
      str_detect(term, "^d\\d{4}$") ~ str_extract(term, "\\d$"),  
      TRUE ~ str_extract(term, "\\d+$")
    )
  ) %>%
  left_join(variable_labels, by = "violence_variable") %>%
  left_join(level_labels, by = "level") %>%
  mutate(
    level_description = ifelse(level == "Reference", "Reference (No)", level_description)
  ) %>%
  select(violence_variable, variable_label, term, level, level_description, 
         odds_ratio, OR_lower_CI, OR_upper_CI, p.value) %>%
  arrange(violence_variable, term)

# save results
write_xlsx(final_results, "violence_hiv_testing_odds_ratios.xlsx")

# check the marriage variable
table(analysis_data$v502, useNA = "ifany")

# store levels of marriage variable
v502_levels <- unique(analysis_data$v502[!is.na(analysis_data$v502)])

# list for results
stratified_results <- list()

# regression for levels of marriage variable
for(level in v502_levels) {
  level_data <- analysis_data %>% filter(v502 == level)
  
  level_models <- lapply(violence_vars, function(var) {
    if(var %in% names(level_data) && sum(!is.na(level_data[[var]])) > 0) {
      formula_str <- paste("v781_binary ~", var, "+ country")
      glm(as.formula(formula_str), data = level_data, family = binomial)
    }
  })
  
  names(level_models) <- violence_vars
  level_results <- lapply(level_models, function(model) {
    if(!is.null(model)) {
      tidy(model, exponentiate = TRUE, conf.int = TRUE)
    }
  })
  
  level_final <- bind_rows(level_results, .id = "violence_variable") %>%
    rename(
      odds_ratio = estimate,
      OR_lower_CI = conf.low,
      OR_upper_CI = conf.high
    ) %>%
    mutate(
      level = case_when(
        term == "(Intercept)" ~ "Reference",
        str_detect(term, "^d\\d{4}$") ~ str_extract(term, "\\d$"),
        TRUE ~ str_extract(term, "\\d+$") 
      )
    ) %>%
    left_join(variable_labels, by = "violence_variable") %>%
    left_join(level_labels, by = "level") %>%
    mutate(
      level_description = ifelse(level == "Reference", "Reference (No)", level_description)
    ) %>%
    filter(!str_detect(term, "country")) %>%
    select(violence_variable, variable_label, term, level, level_description, 
           odds_ratio, OR_lower_CI, OR_upper_CI, p.value) %>%
    arrange(violence_variable, term)
  
  sheet_name <- case_when(
    level == 0 ~ "Never_married",
    level == 1 ~ "Currently_married", 
    level == 2 ~ "Formerly_married",
    TRUE ~ paste0("Level_", level)
  )
  
  stratified_results[[sheet_name]] <- level_final
}

# save to excel
write_xlsx(stratified_results, "violence_hiv_by_marital_status.xlsx")

test test 