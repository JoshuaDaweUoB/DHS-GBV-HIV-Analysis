# load packages
pacman::p_load(dplyr, tidyr, haven, purrr, tableone, broom, writexl, stringr, readxl)

# set working directory
setwd("C:/Users/vl22683/OneDrive - University of Bristol/Documents/Publications/DHS and violence paper/data/")

# load clean data
southeast_asia_combined <- read_xlsx("../code/southeast_asia_combined_dataset.xlsx")

# violence variables
violence_vars <- c("s826f", "v744a", "v744b", "v744c", "v744d", "v744e", "v850a", "d104", "d106", "d107", "d108", "d128")

# binary hiv testing outcome
analysis_data <- southeast_asia_combined %>%
  filter(!is.na(v781)) %>%
  mutate(v781_binary = case_when(
    v781 == 1 ~ 1,  
    v781 == 0 ~ 0,  
    TRUE ~ NA_real_ 
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

# descriptive statistics
descriptive_stats <- map_dfr(violence_vars, function(var) {
  if(var %in% names(analysis_data)) {
    if(str_detect(var, "^d\\d{3}$")) {
      analysis_data %>%
        mutate(var_level = ifelse(!!sym(var) == TRUE, "1", "0")) %>%
        group_by(var_level) %>%
        summarise(
          n_level = n(),
          n_hiv_tested = sum(v781_binary == 1, na.rm = TRUE),
          prop_hiv_tested = mean(v781_binary == 1, na.rm = TRUE),
          .groups = 'drop'
        ) %>%
        mutate(
          violence_variable = var,
          level = var_level,
          percent_hiv_tested = round(prop_hiv_tested * 100, 1)
        ) %>%
        select(violence_variable, level, n_level, n_hiv_tested, percent_hiv_tested)
    } else {
      analysis_data %>%
        group_by(!!sym(var)) %>%
        summarise(
          n_level = n(),
          n_hiv_tested = sum(v781_binary == 1, na.rm = TRUE),
          prop_hiv_tested = mean(v781_binary == 1, na.rm = TRUE),
          .groups = 'drop'
        ) %>%
        mutate(
          violence_variable = var,
          level = as.character(!!sym(var)),
          percent_hiv_tested = round(prop_hiv_tested * 100, 1)
        ) %>%
        select(violence_variable, level, n_level, n_hiv_tested, percent_hiv_tested)
    }
  }
})

# reference categories
reference_rows <- descriptive_stats %>%
  filter(level == "0") %>%
  mutate(
    term = "Reference",
    level = "0",
    odds_ratio = NA,
    OR_lower_CI = NA,
    OR_upper_CI = NA,
    p.value = NA
  ) %>%
  left_join(variable_labels, by = "violence_variable") %>%
  left_join(level_labels, by = "level") %>%
  select(violence_variable, variable_label, term, level, level_description,
         n_level, n_hiv_tested, percent_hiv_tested,
         odds_ratio, OR_lower_CI, OR_upper_CI, p.value)

# exposed categories
odds_ratio_rows <- bind_rows(violence_results, .id = "violence_variable") %>%
  rename(
    odds_ratio = estimate,
    OR_lower_CI = conf.low,
    OR_upper_CI = conf.high
  ) %>%
  mutate(
    level = case_when(
      term == "(Intercept)" ~ "Reference",
      str_detect(term, "TRUE$") ~ "1", 
      str_detect(term, "FALSE$") ~ "0",
      str_detect(term, "^d\\d{4}$") ~ str_extract(term, "\\d$"),  
      TRUE ~ str_extract(term, "\\d+$")
    )
  ) %>%
  filter(!str_detect(term, "country")) %>% 
  filter(term != "(Intercept)") %>% 
  left_join(variable_labels, by = "violence_variable") %>%
  left_join(level_labels, by = "level") %>%
  left_join(descriptive_stats, by = c("violence_variable", "level")) %>%
  select(violence_variable, variable_label, term, level, level_description,
         n_level, n_hiv_tested, percent_hiv_tested,
         odds_ratio, OR_lower_CI, OR_upper_CI, p.value)

# combine reference and exposed
final_results <- bind_rows(reference_rows, odds_ratio_rows) %>%
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
  
  # descriptive statistics
  level_descriptive_stats <- map_dfr(violence_vars, function(var) {
    if(var %in% names(level_data)) {
      if(str_detect(var, "^d\\d{3}$")) {
        level_data %>%
          mutate(var_level = ifelse(!!sym(var) == TRUE, "1", "0")) %>%
          group_by(var_level) %>%
          summarise(
            n_level = n(),
            n_hiv_tested = sum(v781_binary == 1, na.rm = TRUE),
            prop_hiv_tested = mean(v781_binary == 1, na.rm = TRUE),
            .groups = 'drop'
          ) %>%
          mutate(
            violence_variable = var,
            level = var_level,
            percent_hiv_tested = round(prop_hiv_tested * 100, 1)
          ) %>%
          select(violence_variable, level, n_level, n_hiv_tested, percent_hiv_tested)
      } else {
        level_data %>%
          group_by(!!sym(var)) %>%
          summarise(
            n_level = n(),
            n_hiv_tested = sum(v781_binary == 1, na.rm = TRUE),
            prop_hiv_tested = mean(v781_binary == 1, na.rm = TRUE),
            .groups = 'drop'
          ) %>%
          mutate(
            violence_variable = var,
            level = as.character(!!sym(var)),
            percent_hiv_tested = round(prop_hiv_tested * 100, 1)
          ) %>%
          select(violence_variable, level, n_level, n_hiv_tested, percent_hiv_tested)
      }
    }
  })
  
  # reference categories
  level_reference_rows <- level_descriptive_stats %>%
    filter(level == "0") %>%
    mutate(
      term = "Reference",
      level = "0",
      odds_ratio = NA,
      OR_lower_CI = NA,
      OR_upper_CI = NA,
      p.value = NA
    ) %>%
    left_join(variable_labels, by = "violence_variable") %>%
    left_join(level_labels, by = "level") %>%
    select(violence_variable, variable_label, term, level, level_description,
           n_level, n_hiv_tested, percent_hiv_tested,
           odds_ratio, OR_lower_CI, OR_upper_CI, p.value)
  
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
  
  # exposed categories
  level_odds_ratio_rows <- bind_rows(level_results, .id = "violence_variable") %>%
    rename(
      odds_ratio = estimate,
      OR_lower_CI = conf.low,
      OR_upper_CI = conf.high
    ) %>%
    mutate(
      level = case_when(
        term == "(Intercept)" ~ "Reference",
        str_detect(term, "TRUE$") ~ "1", 
        str_detect(term, "FALSE$") ~ "0",
        str_detect(term, "^d\\d{4}$") ~ str_extract(term, "\\d$"),
        TRUE ~ str_extract(term, "\\d+$") 
      )
    ) %>%
    filter(!str_detect(term, "country")) %>% 
    filter(term != "(Intercept)") %>% 
    left_join(variable_labels, by = "violence_variable") %>%
    left_join(level_labels, by = "level") %>%
    left_join(level_descriptive_stats, by = c("violence_variable", "level")) %>%
    select(violence_variable, variable_label, term, level, level_description,
           n_level, n_hiv_tested, percent_hiv_tested,
           odds_ratio, OR_lower_CI, OR_upper_CI, p.value)
  
  # combine exposed and reference categories
  level_final <- bind_rows(level_reference_rows, level_odds_ratio_rows) %>%
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
