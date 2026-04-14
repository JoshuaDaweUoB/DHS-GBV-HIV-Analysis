# load packages
pacman::p_load(dplyr, tidyr, haven, purrr, tableone, broom, writexl, stringr, openxlsx)

# set working directory
setwd("C:/Users/vl22683/OneDrive - University of Bristol/Documents/Publications/DHS and violence paper/data/")

## frequency table

# total sample with hiv testing data
nrow(southeast_asia_combined)

# total sample with violence data
southeast_asia_combined <- southeast_asia_combined %>%
  filter(!is.na(beating_justified_bin) | !is.na(any_violence))
nrow(southeast_asia_combined)

justified_only <- sum(!is.na(southeast_asia_combined$beating_justified_bin) & is.na(southeast_asia_combined$any_violence))
any_violence_only <- sum(!is.na(southeast_asia_combined$any_violence) & is.na(southeast_asia_combined$beating_justified_bin))
both <- sum(!is.na(southeast_asia_combined$any_violence) & !is.na(southeast_asia_combined$beating_justified_bin))
justified_only
any_violence_only
both
sum(is.na(southeast_asia_combined$any_violence) & is.na(southeast_asia_combined$beating_justified_bin))

# load clean data
southeast_asia_combined <- readRDS("../data/southeast_asia_combined_dataset.rds")
table(southeast_asia_combined$beating_justified_bin)

# violence variables
violence_vars <- c("s826f", "v744a", "v744b", "v744c", "v744d", "v744e", "v850a", "d104", "d106", "d107", "d108", "d128", "any_violence", "beating_justified_bin")

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

# any wife-beating justified (v744a–v744e)
just_vars <- c("v744a","v744b","v744c","v744d","v744e")

analysis_data <- analysis_data %>%
  rowwise() %>%
  mutate(
    beating_justified_bin = {
      sx <- trimws(as.character(c_across(all_of(just_vars))))
      any_yes <- any(sx %in% c("1","Yes","TRUE","True","T"), na.rm = TRUE)
      any_obs <- any(sx %in% c("0","1","No","Yes","FALSE","False","TRUE","True","T","F"), na.rm = TRUE)
      if (any_yes) "Yes" else if (any_obs) "No" else NA_character_
    }
  ) %>%
  ungroup() %>%
  mutate(beating_justified_bin = factor(beating_justified_bin, levels = c("No","Yes")))

# Ensure country is lower-case consistent
analysis_data <- analysis_data %>% mutate(country = tolower(as.character(country)))

# Define countries for output and display names
target_countries <- c("philippines", "cambodia")

# Helper: normalize a variable to character "0"/"1" for counting
to_01_char <- function(x) {
  if (is.logical(x)) {
    y <- ifelse(x, "1", "0")
  } else if (is.numeric(x)) {
    y <- ifelse(x == 1, "1",
                ifelse(x == 0, "0", NA_character_))
  } else {
    # factor/character – trim and map common encodings
    sx <- trimws(as.character(x))
    y <- ifelse(sx %in% c("1","Yes","TRUE","True","T"), "1",
                ifelse(sx %in% c("0","No","FALSE","False","F"), "0", NA_character_))
  }
  y
}

# Helper to compute n and percent for 0/1 levels of a single variable within a dataset
freq_01 <- function(df, var) {
  vv <- to_01_char(df[[var]])
  total_n <- sum(!is.na(vv))
  n0 <- sum(vv == "0", na.rm = TRUE)
  n1 <- sum(vv == "1", na.rm = TRUE)
  p0 <- if (total_n > 0) round(100 * n0 / total_n, 1) else 0
  p1 <- if (total_n > 0) round(100 * n1 / total_n, 1) else 0
  tibble(
    level = c("No", "Yes"),
    n = c(n0, n1),
    pct = c(p0, p1),
    total_n = total_n
  )
}

variable_labels <- tibble::tribble(
  ~violence_variable, ~variable_label,
  "s826f", "Justifies DV: wife ask use condom",
  "v744a", "Wife beating justified: goes out without telling husband",
  "v744b", "Wife beating justified: neglects children",
  "v744c", "Wife beating justified: argues with husband",
  "v744d", "Wife beating justified: refuses sex with husband",
  "v744e", "Wife beating justified: burns the food",
  "v850a", "Can respondent refuse sex",
  "d104",  "Experienced any emotional violence",
  "d106",  "Experienced any less severe violence",
  "d107",  "Experienced any severe violence",
  "d108",  "Experienced any sexual violence",
  "d128",  "Ever told anyone else about violence",
  "any_violence", "Any violent experiences",
  "beating_justified_bin", "Beating justified any reason"
)

# Build the table
violence_freq_rows <- purrr::map_dfr(violence_vars, function(var) {
  # Label for the violence variable
  vlabel <- variable_labels %>%
    filter(violence_variable == var) %>%
    pull(variable_label) %>%
    { if(length(.)==0) var else . }

  # Per-country data frames (fill zeros if country absent)
  per_country <- purrr::map(target_countries, function(cty) {
    df_cty <- analysis_data %>% filter(country == cty)
    if (nrow(df_cty) == 0) {
      tibble(level = c("No", "Yes"), n = c(0, 0), pct = c(0, 0), total_n = 0)
    } else {
      freq_01(df_cty, var)
    }
  })

  # Overall
  overall <- freq_01(analysis_data, var)

  tibble(
    `Violence exposure` = c(vlabel, vlabel),
    Level = c("No", "Yes"),
    `Philippines, n (%)` = sprintf("%d (%.1f%%)", per_country[[1]]$n, per_country[[1]]$pct),
    `Cambodia, n (%)`    = sprintf("%d (%.1f%%)", per_country[[2]]$n, per_country[[2]]$pct),
    `Overall, n (%)`     = sprintf("%d (%.1f%%)", overall$n, overall$pct)
  )
})

# Order rows by the template order from variable_labels
violence_freq_rows <- violence_freq_rows %>%
  mutate(order = match(`Violence exposure`, variable_labels$variable_label)) %>%
  arrange(order, desc(Level)) %>%
  select(-order)

# Save to Excel
writexl::write_xlsx(violence_freq_rows, "violence_exposure_frequencies.xlsx")

## regressions

# load clean data
southeast_asia_combined <- readRDS("../data/southeast_asia_combined_dataset.rds")

# create workbook for results
violence_results <- loadWorkbook("violence_ORs.xlsx")

# stratify data by marriage type
married_data <- southeast_asia_combined %>% filter(marital_status_3cat == "Married")
nrow(married_data)
never_married_data <- southeast_asia_combined %>% filter(marital_status_3cat == "Never married")
nrow(never_married_data)
separated_data <- southeast_asia_combined %>% filter(marital_status_3cat == "Separated")
nrow(separated_data)

# exposures
exposures <- c("any_violence", "emotional_violence_bin", "sexual_violence_bin", "less_severe_violence_bin", "severe_violence_bin", "any_physical_violence_bin")
exposure_labels <- c("Any violence", "Emotional violence", "Sexual violence", "Less severe violence", "Severe violence", "Any physical violence")

# confounders
confounder_vars_stratified <- c("household_head", "religion_4cat", "employed_bin", "residence_3cat", "children_under5_4cat")  

# levels
table(southeast_asia_combined$household_head, useNA = "always")
table(southeast_asia_combined$religion_4cat, useNA = "always")
table(southeast_asia_combined$employed_bin, useNA = "always")
table(southeast_asia_combined$residence_3cat, useNA = "always")
table(southeast_asia_combined$children_under5_4cat, useNA = "always")

# Build formula using confounder_vars
build_formula <- function(exposure, confounders) {
  confounder_str <- paste(confounders, collapse = " + ")
  as.formula(paste("v781_binary ~", exposure, "+", confounder_str))
}

## experiences of violence

# Philippines
violence_exp_philippines <- data.frame(
  Exposure = exposure_labels,

  Married_No_n = NA_integer_,
  Married_No_HIV_n_pct = NA_character_,
  Married_Yes_n = NA_integer_,
  Married_Yes_HIV_n_pct = NA_character_,
  Married_Unadj_OR_CI = NA_character_,
  Married_OR_CI = NA_character_,

  NeverMarried_No_n = NA_integer_,
  NeverMarried_No_HIV_n_pct = NA_character_,
  NeverMarried_Yes_n = NA_integer_,
  NeverMarried_Yes_HIV_n_pct = NA_character_,
  NeverMarried_Unadj_OR_CI = NA_character_,
  NeverMarried_OR_CI = NA_character_,

  Separated_No_n = NA_integer_,
  Separated_No_HIV_n_pct = NA_character_,
  Separated_Yes_n = NA_integer_,
  Separated_Yes_HIV_n_pct = NA_character_,
  Separated_Unadj_OR_CI = NA_character_,
  Separated_OR_CI = NA_character_,

  stringsAsFactors = FALSE
)

# Married exposed and HIV tested
for (i in seq_along(exposures)) {

  d <- married_data[
    married_data$country == "philippines" &
    !is.na(married_data[[exposures[i]]]) &
    !is.na(married_data$v781_binary),
  ]

  # no
  d_no <- d[d[[exposures[i]]] == "No", ]
  n_no <- nrow(d_no)
  hiv_no <- sum(d_no$v781_binary == 1)

  if (n_no > 0) {
    violence_exp_philippines$Married_No_n[i] <- n_no
    violence_exp_philippines$Married_No_HIV_n_pct[i] <-
      sprintf("%d (%.1f%%)", hiv_no, 100 * hiv_no / n_no)
  }

  # yes
  d_yes <- d[d[[exposures[i]]] == "Yes", ]
  n_yes <- nrow(d_yes)
  hiv_yes <- sum(d_yes$v781_binary == 1)

  if (n_yes > 0) {
    violence_exp_philippines$Married_Yes_n[i] <- n_yes
    violence_exp_philippines$Married_Yes_HIV_n_pct[i] <-
      sprintf("%d (%.1f%%)", hiv_yes, 100 * hiv_yes / n_yes)
  }
}

# Married unadjusted
for (i in seq_along(exposures)) {
  model <- glm(as.formula(paste0("v781_binary ~ ", exposures[i])),
              data = married_data[married_data$country == "philippines", ],
              family = binomial())
  res <- tidy(model, exponentiate = TRUE, conf.int = TRUE)
  term <- paste0(exposures[i], "Yes")
  row <- res[res$term == term, ]
  if (nrow(row) > 0) {
    violence_exp_philippines$Married_Unadj_OR_CI[i] <- sprintf("%.2f (%.2f–%.2f)",
    row$estimate, row$conf.low, row$conf.high)
  }
}

# Married adjusted
for (i in seq_along(exposures)) {
  model <- glm(build_formula(exposures[i], confounder_vars_stratified), data = married_data[married_data$country == "philippines", ], family = binomial())
  res <- tidy(model, exponentiate = TRUE, conf.int = TRUE)
  term <- paste0(exposures[i], "Yes")
  row <- res[res$term == term, ]
  if (nrow(row) > 0) {
    or_str <- sprintf("%.2f (%.2f–%.2f)", row$estimate, row$conf.low, row$conf.high)
    violence_exp_philippines$Married_OR_CI[i] <- or_str
  }
}

# Never married unadjusted
for (i in seq_along(exposures)) {
  model <- glm(as.formula(paste0("v781_binary ~ ", exposures[i])),
              data = never_married_data[never_married_data$country == "philippines", ],
              family = binomial())
  res <- tidy(model, exponentiate = TRUE, conf.int = TRUE)
  term <- paste0(exposures[i], "Yes")
  row <- res[res$term == term, ]
  if (nrow(row) > 0) {
    violence_exp_philippines$NeverMarried_Unadj_OR_CI[i] <- sprintf("%.2f (%.2f–%.2f)",
    row$estimate, row$conf.low, row$conf.high)
  }
}

# Never married adjusted
for (i in seq_along(exposures)) {
  model <- glm(build_formula(exposures[i], confounder_vars_stratified), data = never_married_data[never_married_data$country == "philippines", ], family = binomial())
  res <- tidy(model, exponentiate = TRUE, conf.int = TRUE)
  term <- paste0(exposures[i], "Yes")
  row <- res[res$term == term, ]
  if (nrow(row) > 0) {
    or_str <- sprintf("%.2f (%.2f–%.2f)", row$estimate, row$conf.low, row$conf.high)
    violence_exp_philippines$NeverMarried_OR_CI[i] <- or_str
  }
}

# Never married exposed and HIV tested
for (i in seq_along(exposures)) {
  d <- never_married_data[
    never_married_data$country == "philippines" &
    !is.na(never_married_data[[exposures[i]]]) &
    !is.na(never_married_data$v781_binary),
  ]

  # no
  d_no <- d[d[[exposures[i]]] == "No", ]
  n_no <- nrow(d_no)
  hiv_no <- sum(d_no$v781_binary == 1)

  if (n_no > 0) {
    violence_exp_philippines$NeverMarried_No_n[i] <- n_no
    violence_exp_philippines$NeverMarried_No_HIV_n_pct[i] <-
      sprintf("%d (%.1f%%)", hiv_no, 100 * hiv_no / n_no)
  }

  # yes
  d_yes <- d[d[[exposures[i]]] == "Yes", ]
  n_yes <- nrow(d_yes)
  hiv_yes <- sum(d_yes$v781_binary == 1)

  if (n_yes > 0) {
    violence_exp_philippines$NeverMarried_Yes_n[i] <- n_yes
    violence_exp_philippines$NeverMarried_Yes_HIV_n_pct[i] <-
      sprintf("%d (%.1f%%)", hiv_yes, 100 * hiv_yes / n_yes)
  }
}

# Separated exposed and HIV tested
for (i in seq_along(exposures)) {

  d <- separated_data[
    separated_data$country == "philippines" &
    !is.na(separated_data[[exposures[i]]]) &
    !is.na(separated_data$v781_binary),
  ]

  d_no <- d[d[[exposures[i]]] == "No", ]
  d_yes <- d[d[[exposures[i]]] == "Yes", ]

  if (nrow(d_no) > 0) {
    violence_exp_philippines$Separated_No_n[i] <- nrow(d_no)
    violence_exp_philippines$Separated_No_HIV_n_pct[i] <-
      sprintf("%d (%.1f%%)",
              sum(d_no$v781_binary == 1),
              100 * sum(d_no$v781_binary == 1) / nrow(d_no))
  }

  if (nrow(d_yes) > 0) {
    violence_exp_philippines$Separated_Yes_n[i] <- nrow(d_yes)
    violence_exp_philippines$Separated_Yes_HIV_n_pct[i] <-
      sprintf("%d (%.1f%%)",
              sum(d_yes$v781_binary == 1),
              100 * sum(d_yes$v781_binary == 1) / nrow(d_yes))
  }
}


# Separated unadjusted
for (i in seq_along(exposures)) {
  model <- glm(as.formula(paste0("v781_binary ~ ", exposures[i])),
              data = separated_data[separated_data$country == "philippines", ],
              family = binomial())
  res <- tidy(model, exponentiate = TRUE, conf.int = TRUE)
  term <- paste0(exposures[i], "Yes")
  row <- res[res$term == term, ]
  if (nrow(row) > 0) {
    violence_exp_philippines$Separated_Unadj_OR_CI[i] <- sprintf("%.2f (%.2f–%.2f)",
    row$estimate, row$conf.low, row$conf.high)
  }
}

# Separated adjusted
for (i in seq_along(exposures)) {
  model <- glm(build_formula(exposures[i], confounder_vars_stratified), data = separated_data[separated_data$country == "philippines", ], family = binomial())
  res <- tidy(model, exponentiate = TRUE, conf.int = TRUE)
  term <- paste0(exposures[i], "Yes")
  row <- res[res$term == term, ]
  if (nrow(row) > 0) {
    or_str <- sprintf("%.2f (%.2f–%.2f)", row$estimate, row$conf.low, row$conf.high)
    violence_exp_philippines$Separated_OR_CI[i] <- or_str
  }
}

# cambodia

violence_exp_cambodia <- data.frame(
  Exposure = exposure_labels,
  Married_No_n = NA_integer_,
  Married_No_HIV_n_pct = NA_character_,
  Married_Yes_n = NA_integer_,
  Married_Yes_HIV_n_pct = NA_character_,
  Married_Unadj_OR_CI = NA_character_,
  Married_OR_CI = NA_character_,

  NeverMarried_No_n = NA_integer_,
  NeverMarried_No_HIV_n_pct = NA_character_,
  NeverMarried_Yes_n = NA_integer_,
  NeverMarried_Yes_HIV_n_pct = NA_character_,
  NeverMarried_Unadj_OR_CI = NA_character_,
  NeverMarried_OR_CI = NA_character_,

  Separated_No_n = NA_integer_,
  Separated_No_HIV_n_pct = NA_character_,
  Separated_Yes_n = NA_integer_,
  Separated_Yes_HIV_n_pct = NA_character_,
  Separated_Unadj_OR_CI = NA_character_,
  Separated_OR_CI = NA_character_,

  stringsAsFactors = FALSE
)

# Married No / Yes counts and HIV tested — Cambodia
for (i in seq_along(exposures)) {

  d <- married_data[
    married_data$country == "cambodia" &
    !is.na(married_data[[exposures[i]]]) &
    !is.na(married_data$v781_binary),
  ]

  # NO
  d_no <- d[d[[exposures[i]]] == "No", ]
  if (nrow(d_no) > 0) {
    violence_exp_cambodia$Married_No_n[i] <- nrow(d_no)
    violence_exp_cambodia$Married_No_HIV_n_pct[i] <-
      sprintf("%d (%.1f%%)",
              sum(d_no$v781_binary == 1),
              100 * sum(d_no$v781_binary == 1) / nrow(d_no))
  }

  # YES
  d_yes <- d[d[[exposures[i]]] == "Yes", ]
  if (nrow(d_yes) > 0) {
    violence_exp_cambodia$Married_Yes_n[i] <- nrow(d_yes)
    violence_exp_cambodia$Married_Yes_HIV_n_pct[i] <-
      sprintf("%d (%.1f%%)",
              sum(d_yes$v781_binary == 1),
              100 * sum(d_yes$v781_binary == 1) / nrow(d_yes))
  }
}

# Married unadjusted
for (i in seq_along(exposures)) {
  model <- glm(as.formula(paste0("v781_binary ~ ", exposures[i])),
              data = married_data[married_data$country == "cambodia", ],
              family = binomial())
  res <- tidy(model, exponentiate = TRUE, conf.int = TRUE)
  term <- paste0(exposures[i], "Yes")
  row <- res[res$term == term, ]
  if (nrow(row) > 0) {
    violence_exp_cambodia$Married_Unadj_OR_CI[i] <- sprintf("%.2f (%.2f–%.2f)",
    row$estimate, row$conf.low, row$conf.high)
  }
}

# Married adjusted
for (i in seq_along(exposures)) {
  model <- glm(build_formula(exposures[i], confounder_vars_stratified), data = married_data[married_data$country == "cambodia", ], family = binomial())
  res <- tidy(model, exponentiate = TRUE, conf.int = TRUE)
  term <- paste0(exposures[i], "Yes")
  row <- res[res$term == term, ]
  if (nrow(row) > 0) {
    or_str <- sprintf("%.2f (%.2f–%.2f)", row$estimate, row$conf.low, row$conf.high)
    violence_exp_cambodia$Married_OR_CI[i] <- or_str
  }
}

# Never married No / Yes counts and HIV tested — Cambodia
for (i in seq_along(exposures)) {

  d <- never_married_data[
    never_married_data$country == "cambodia" &
    !is.na(never_married_data[[exposures[i]]]) &
    !is.na(never_married_data$v781_binary),
  ]

  d_no <- d[d[[exposures[i]]] == "No", ]
  d_yes <- d[d[[exposures[i]]] == "Yes", ]

  if (nrow(d_no) > 0) {
    violence_exp_cambodia$NeverMarried_No_n[i] <- nrow(d_no)
    violence_exp_cambodia$NeverMarried_No_HIV_n_pct[i] <-
      sprintf("%d (%.1f%%)",
              sum(d_no$v781_binary == 1),
              100 * sum(d_no$v781_binary == 1) / nrow(d_no))
  }

  if (nrow(d_yes) > 0) {
    violence_exp_cambodia$NeverMarried_Yes_n[i] <- nrow(d_yes)
    violence_exp_cambodia$NeverMarried_Yes_HIV_n_pct[i] <-
      sprintf("%d (%.1f%%)",
              sum(d_yes$v781_binary == 1),
              100 * sum(d_yes$v781_binary == 1) / nrow(d_yes))
  }
}

# Never married unadjusted
for (i in seq_along(exposures)) {
  model <- glm(as.formula(paste0("v781_binary ~ ", exposures[i])),
              data = never_married_data[never_married_data$country == "cambodia", ],
              family = binomial())
  res <- tidy(model, exponentiate = TRUE, conf.int = TRUE)
  term <- paste0(exposures[i], "Yes")
  row <- res[res$term == term, ]
  if (nrow(row) > 0) {
    violence_exp_cambodia$NeverMarried_Unadj_OR_CI[i] <- sprintf("%.2f (%.2f–%.2f)",
    row$estimate, row$conf.low, row$conf.high)
  }
}

# Never married adjusted
for (i in seq_along(exposures)) {
  try({
    model <- glm(build_formula(exposures[i], confounder_vars_stratified), 
                 data = never_married_data[never_married_data$country == "cambodia", ], 
                 family = binomial())
    res <- tidy(model, exponentiate = TRUE, conf.int = TRUE)
    term <- paste0(exposures[i], "Yes")
    row <- res[res$term == term, ]
    if (nrow(row) > 0) {
      or_str <- sprintf("%.2f (%.2f–%.2f)", row$estimate, row$conf.low, row$conf.high)
      violence_exp_cambodia$NeverMarried_OR_CI[i] <- or_str
    }
  }, silent = TRUE)
}

# Separated No / Yes counts and HIV tested — Cambodia
for (i in seq_along(exposures)) {

  d <- separated_data[
    separated_data$country == "cambodia" &
    !is.na(separated_data[[exposures[i]]]) &
    !is.na(separated_data$v781_binary),
  ]

  d_no <- d[d[[exposures[i]]] == "No", ]
  d_yes <- d[d[[exposures[i]]] == "Yes", ]

  if (nrow(d_no) > 0) {
    violence_exp_cambodia$Separated_No_n[i] <- nrow(d_no)
    violence_exp_cambodia$Separated_No_HIV_n_pct[i] <-
      sprintf("%d (%.1f%%)",
              sum(d_no$v781_binary == 1),
              100 * sum(d_no$v781_binary == 1) / nrow(d_no))
  }

  if (nrow(d_yes) > 0) {
    violence_exp_cambodia$Separated_Yes_n[i] <- nrow(d_yes)
    violence_exp_cambodia$Separated_Yes_HIV_n_pct[i] <-
      sprintf("%d (%.1f%%)",
              sum(d_yes$v781_binary == 1),
              100 * sum(d_yes$v781_binary == 1) / nrow(d_yes))
  }
}

# Separated unadjusted
for (i in seq_along(exposures)) {
  model <- glm(as.formula(paste0("v781_binary ~ ", exposures[i])),
              data = separated_data[separated_data$country == "cambodia", ],
              family = binomial())
  res <- tidy(model, exponentiate = TRUE, conf.int = TRUE)
  term <- paste0(exposures[i], "Yes")
  row <- res[res$term == term, ]
  if (nrow(row) > 0) {
    violence_exp_cambodia$Separated_Unadj_OR_CI[i] <- sprintf("%.2f (%.2f–%.2f)",
    row$estimate, row$conf.low, row$conf.high)
  }
}

# Separated adjusted
for (i in seq_along(exposures)) {
  model <- glm(build_formula(exposures[i], confounder_vars_stratified), data = separated_data[separated_data$country == "cambodia", ], family = binomial())
  res <- tidy(model, exponentiate = TRUE, conf.int = TRUE)
  term <- paste0(exposures[i], "Yes")
  row <- res[res$term == term, ]
  if (nrow(row) > 0) {
    or_str <- sprintf("%.2f (%.2f–%.2f)", row$estimate, row$conf.low, row$conf.high)
    violence_exp_cambodia$Separated_OR_CI[i] <- or_str
  }
}

## acceptability of violence

# Acceptability exposures and labels
acceptability_exposures <- c(
  "justifies_dv_condom_bin",
  "can_refuse_sex_bin",
  "beating_justified_out_bin",
  "beating_justified_neglect_bin",
  "beating_justified_argue_bin",
  "beating_justified_refuse_sex_bin",
  "beating_justified_burn_food_bin",
  "beating_justified_bin"
)

acceptability_labels <- c(
  justifies_dv_condom_bin = "Justifies DV (condom)",
  can_refuse_sex_bin = "Can refuse sex",
  beating_justified_out_bin = "Beating justified: out",
  beating_justified_neglect_bin = "Beating justified: neglect",
  beating_justified_argue_bin = "Beating justified: argue",
  beating_justified_refuse_sex_bin = "Beating justified: refuse sex",
  beating_justified_burn_food_bin = "Beating justified: burn food",
  beating_justified_bin = "Any beating justified"
)

prop.table(table(married_data$v781_binary, married_data$can_refuse_sex_bin), margin = 2)

# philippines

violence_acc_philippines <- data.frame(
  Exposure = acceptability_labels,
  Married_No_n = NA_integer_,
  Married_No_HIV_n_pct = NA_character_,
  Married_Yes_n = NA_integer_,
  Married_Yes_HIV_n_pct = NA_character_,
  Married_Unadj_OR_CI = NA_character_,
  Married_OR_CI = NA_character_,

  NeverMarried_No_n = NA_integer_,
  NeverMarried_No_HIV_n_pct = NA_character_,
  NeverMarried_Yes_n = NA_integer_,
  NeverMarried_Yes_HIV_n_pct = NA_character_,
  NeverMarried_Unadj_OR_CI = NA_character_,
  NeverMarried_OR_CI = NA_character_,

  Separated_No_n = NA_integer_,
  Separated_No_HIV_n_pct = NA_character_,
  Separated_Yes_n = NA_integer_,
  Separated_Yes_HIV_n_pct = NA_character_,
  Separated_Unadj_OR_CI = NA_character_,
  Separated_OR_CI = NA_character_,

  stringsAsFactors = FALSE
)

# Married No / Yes counts and HIV tested — Philippines (acceptability)
for (i in seq_along(acceptability_exposures)) {

  d <- married_data[
    married_data$country == "philippines" &
    !is.na(married_data[[acceptability_exposures[i]]]) &
    !is.na(married_data$v781_binary),
  ]

  # NO
  d_no <- d[d[[acceptability_exposures[i]]] == "No", ]
  if (nrow(d_no) > 0) {
    violence_acc_philippines$Married_No_n[i] <- nrow(d_no)
    violence_acc_philippines$Married_No_HIV_n_pct[i] <-
      sprintf("%d (%.1f%%)",
              sum(d_no$v781_binary == 1),
              100 * sum(d_no$v781_binary == 1) / nrow(d_no))
  }

  # YES
  d_yes <- d[d[[acceptability_exposures[i]]] == "Yes", ]
  if (nrow(d_yes) > 0) {
    violence_acc_philippines$Married_Yes_n[i] <- nrow(d_yes)
    violence_acc_philippines$Married_Yes_HIV_n_pct[i] <-
      sprintf("%d (%.1f%%)",
              sum(d_yes$v781_binary == 1),
              100 * sum(d_yes$v781_binary == 1) / nrow(d_yes))
  }
}

# Married unadjusted
for (i in seq_along(acceptability_exposures)) {
  try({
    model <- glm(as.formula(paste0("v781_binary ~ ", acceptability_exposures[i])),
                 data = married_data[married_data$country == "philippines", ],
                 family = binomial())
    res <- tidy(model, exponentiate = TRUE, conf.int = TRUE)
    term <- paste0(acceptability_exposures[i], "Yes")
    row <- res[res$term == term, ]
    if (nrow(row) > 0) {
      violence_acc_philippines$Married_Unadj_OR_CI[i] <-
        sprintf("%.2f (%.2f–%.2f)", row$estimate, row$conf.low, row$conf.high)
    }
  }, silent = TRUE)
}

# Married adjusted
for (i in seq_along(acceptability_exposures)) {
  try({
    model <- glm(build_formula(acceptability_exposures[i], confounder_vars_stratified), 
                 data = married_data[married_data$country == "philippines", ], 
                 family = binomial())
    res <- tidy(model, exponentiate = TRUE, conf.int = TRUE)
    term <- paste0(acceptability_exposures[i], "Yes")
    row <- res[res$term == term, ]
    if (nrow(row) > 0) {
      or_str <- sprintf("%.2f (%.2f–%.2f)", row$estimate, row$conf.low, row$conf.high)
      violence_acc_philippines$Married_OR_CI[i] <- or_str
    }
  }, silent = TRUE)
}

# Never married No / Yes — Philippines (acceptability)
for (i in seq_along(acceptability_exposures)) {

  d <- never_married_data[
    never_married_data$country == "philippines" &
    !is.na(never_married_data[[acceptability_exposures[i]]]) &
    !is.na(never_married_data$v781_binary),
  ]

  d_no <- d[d[[acceptability_exposures[i]]] == "No", ]
  d_yes <- d[d[[acceptability_exposures[i]]] == "Yes", ]

  if (nrow(d_no) > 0) {
    violence_acc_philippines$NeverMarried_No_n[i] <- nrow(d_no)
    violence_acc_philippines$NeverMarried_No_HIV_n_pct[i] <-
      sprintf("%d (%.1f%%)",
              sum(d_no$v781_binary == 1),
              100 * sum(d_no$v781_binary == 1) / nrow(d_no))
  }

  if (nrow(d_yes) > 0) {
    violence_acc_philippines$NeverMarried_Yes_n[i] <- nrow(d_yes)
    violence_acc_philippines$NeverMarried_Yes_HIV_n_pct[i] <-
      sprintf("%d (%.1f%%)",
              sum(d_yes$v781_binary == 1),
              100 * sum(d_yes$v781_binary == 1) / nrow(d_yes))
  }
}

# Never married unadjusted
for (i in seq_along(acceptability_exposures)) {
  try({
    model <- glm(as.formula(paste0("v781_binary ~ ", acceptability_exposures[i])),
                 data = never_married_data[never_married_data$country == "philippines", ],
                 family = binomial())
    res <- tidy(model, exponentiate = TRUE, conf.int = TRUE)
    term <- paste0(acceptability_exposures[i], "Yes")
    row <- res[res$term == term, ]
    if (nrow(row) > 0) {
      violence_acc_philippines$NeverMarried_Unadj_OR_CI[i] <-
        sprintf("%.2f (%.2f–%.2f)", row$estimate, row$conf.low, row$conf.high)
    }
  }, silent = TRUE)
}

# Never married adjusted
for (i in seq_along(acceptability_exposures)) {
  try({
    model <- glm(build_formula(acceptability_exposures[i], confounder_vars_stratified), 
                 data = never_married_data[never_married_data$country == "philippines", ], 
                 family = binomial())
    res <- tidy(model, exponentiate = TRUE, conf.int = TRUE)
    term <- paste0(acceptability_exposures[i], "Yes")
    row <- res[res$term == term, ]
    if (nrow(row) > 0) {
      or_str <- sprintf("%.2f (%.2f–%.2f)", row$estimate, row$conf.low, row$conf.high)
      violence_acc_philippines$NeverMarried_OR_CI[i] <- or_str
    }
  }, silent = TRUE)
}

# Separated No / Yes — Philippines (acceptability)
for (i in seq_along(acceptability_exposures)) {
  d <- separated_data[
    separated_data$country == "philippines" &
    !is.na(separated_data[[acceptability_exposures[i]]]) &
    !is.na(separated_data$v781_binary),
  ]

  d_no <- d[d[[acceptability_exposures[i]]] == "No", ]
  d_yes <- d[d[[acceptability_exposures[i]]] == "Yes", ]

  if (nrow(d_no) > 0) {
    violence_acc_philippines$Separated_No_n[i] <- nrow(d_no)
    violence_acc_philippines$Separated_No_HIV_n_pct[i] <-
      sprintf("%d (%.1f%%)",
              sum(d_no$v781_binary == 1),
              100 * sum(d_no$v781_binary == 1) / nrow(d_no))
  }

  if (nrow(d_yes) > 0) {
    violence_acc_philippines$Separated_Yes_n[i] <- nrow(d_yes)
    violence_acc_philippines$Separated_Yes_HIV_n_pct[i] <-
      sprintf("%d (%.1f%%)",
              sum(d_yes$v781_binary == 1),
              100 * sum(d_yes$v781_binary == 1) / nrow(d_yes))
  }
}


# Separated unadjusted
for (i in seq_along(acceptability_exposures)) {
  try({
    model <- glm(as.formula(paste0("v781_binary ~ ", acceptability_exposures[i])),
                 data = separated_data[separated_data$country == "philippines", ],
                 family = binomial())
    res <- tidy(model, exponentiate = TRUE, conf.int = TRUE)
    term <- paste0(acceptability_exposures[i], "Yes")
    row <- res[res$term == term, ]
    if (nrow(row) > 0) {
      violence_acc_philippines$Separated_Unadj_OR_CI[i] <-
        sprintf("%.2f (%.2f–%.2f)", row$estimate, row$conf.low, row$conf.high)
    }
  }, silent = TRUE)
}

# Separated adjusted
for (i in seq_along(acceptability_exposures)) {
  try({
    model <- glm(build_formula(acceptability_exposures[i], confounder_vars_stratified), 
                 data = separated_data[separated_data$country == "philippines", ], 
                 family = binomial())
    res <- tidy(model, exponentiate = TRUE, conf.int = TRUE)
    term <- paste0(acceptability_exposures[i], "Yes")
    row <- res[res$term == term, ]
    if (nrow(row) > 0) {
      or_str <- sprintf("%.2f (%.2f–%.2f)", row$estimate, row$conf.low, row$conf.high)
      violence_acc_philippines$Separated_OR_CI[i] <- or_str
    }
  }, silent = TRUE)
}

# cambodia

violence_acc_cambodia <- data.frame(
  Exposure = acceptability_labels,
  Married_No_n = NA_integer_,
  Married_No_HIV_n_pct = NA_character_,
  Married_Yes_n = NA_integer_,
  Married_Yes_HIV_n_pct = NA_character_,
  Married_Unadj_OR_CI = NA_character_,
  Married_OR_CI = NA_character_,

  NeverMarried_No_n = NA_integer_,
  NeverMarried_No_HIV_n_pct = NA_character_,
  NeverMarried_Yes_n = NA_integer_,
  NeverMarried_Yes_HIV_n_pct = NA_character_,
  NeverMarried_Unadj_OR_CI = NA_character_,
  NeverMarried_OR_CI = NA_character_,

  Separated_No_n = NA_integer_,
  Separated_No_HIV_n_pct = NA_character_,
  Separated_Yes_n = NA_integer_,
  Separated_Yes_HIV_n_pct = NA_character_,
  Separated_Unadj_OR_CI = NA_character_,
  Separated_OR_CI = NA_character_,

  stringsAsFactors = FALSE
)

# Married No / Yes — Cambodia (acceptability)
for (i in seq_along(acceptability_exposures)) {

  d <- married_data[
    married_data$country == "cambodia" &
    !is.na(married_data[[acceptability_exposures[i]]]) &
    !is.na(married_data$v781_binary),
  ]

  d_no <- d[d[[acceptability_exposures[i]]] == "No", ]
  d_yes <- d[d[[acceptability_exposures[i]]] == "Yes", ]

  if (nrow(d_no) > 0) {
    violence_acc_cambodia$Married_No_n[i] <- nrow(d_no)
    violence_acc_cambodia$Married_No_HIV_n_pct[i] <-
      sprintf("%d (%.1f%%)",
              sum(d_no$v781_binary == 1),
              100 * sum(d_no$v781_binary == 1) / nrow(d_no))
  }

  if (nrow(d_yes) > 0) {
    violence_acc_cambodia$Married_Yes_n[i] <- nrow(d_yes)
    violence_acc_cambodia$Married_Yes_HIV_n_pct[i] <-
      sprintf("%d (%.1f%%)",
              sum(d_yes$v781_binary == 1),
              100 * sum(d_yes$v781_binary == 1) / nrow(d_yes))
  }
}

# Married unadjusted
for (i in seq_along(acceptability_exposures)) {
  try({
    model <- glm(as.formula(paste0("v781_binary ~ ", acceptability_exposures[i])),
                 data = married_data[married_data$country == "cambodia", ],
                 family = binomial())
    res <- tidy(model, exponentiate = TRUE, conf.int = TRUE)
    term <- paste0(acceptability_exposures[i], "Yes")
    row <- res[res$term == term, ]
    if (nrow(row) > 0) {
      violence_acc_cambodia$Married_Unadj_OR_CI[i] <-
        sprintf("%.2f (%.2f–%.2f)", row$estimate, row$conf.low, row$conf.high)
    }
  }, silent = TRUE)
}

# Married adjusted
for (i in seq_along(acceptability_exposures)) {
  try({
    model <- glm(build_formula(acceptability_exposures[i], confounder_vars_stratified), 
                 data = married_data[married_data$country == "cambodia", ], 
                 family = binomial())
    res <- tidy(model, exponentiate = TRUE, conf.int = TRUE)
    term <- paste0(acceptability_exposures[i], "Yes")
    row <- res[res$term == term, ]
    if (nrow(row) > 0) {
      or_str <- sprintf("%.2f (%.2f–%.2f)", row$estimate, row$conf.low, row$conf.high)
      violence_acc_cambodia$Married_OR_CI[i] <- or_str
    }
  }, silent = TRUE)
}

# Never married No / Yes — Cambodia (acceptability)
for (i in seq_along(acceptability_exposures)) {

  d <- never_married_data[
    never_married_data$country == "cambodia" &
    !is.na(never_married_data[[acceptability_exposures[i]]]) &
    !is.na(never_married_data$v781_binary),
  ]

  d_no <- d[d[[acceptability_exposures[i]]] == "No", ]
  d_yes <- d[d[[acceptability_exposures[i]]] == "Yes", ]

  if (nrow(d_no) > 0) {
    violence_acc_cambodia$NeverMarried_No_n[i] <- nrow(d_no)
    violence_acc_cambodia$NeverMarried_No_HIV_n_pct[i] <-
      sprintf("%d (%.1f%%)",
              sum(d_no$v781_binary == 1),
              100 * sum(d_no$v781_binary == 1) / nrow(d_no))
  }

  if (nrow(d_yes) > 0) {
    violence_acc_cambodia$NeverMarried_Yes_n[i] <- nrow(d_yes)
    violence_acc_cambodia$NeverMarried_Yes_HIV_n_pct[i] <-
      sprintf("%d (%.1f%%)",
              sum(d_yes$v781_binary == 1),
              100 * sum(d_yes$v781_binary == 1) / nrow(d_yes))
  }
}

# Never married unadjusted
for (i in seq_along(acceptability_exposures)) {
  try({
    model <- glm(as.formula(paste0("v781_binary ~ ", acceptability_exposures[i])),
                 data = never_married_data[never_married_data$country == "cambodia", ],
                 family = binomial())
    res <- tidy(model, exponentiate = TRUE, conf.int = TRUE)
    term <- paste0(acceptability_exposures[i], "Yes")
    row <- res[res$term == term, ]
    if (nrow(row) > 0) {
      violence_acc_cambodia$NeverMarried_Unadj_OR_CI[i] <-
        sprintf("%.2f (%.2f–%.2f)", row$estimate, row$conf.low, row$conf.high)
    }
  }, silent = TRUE)
}

# Never married adjusted
for (i in seq_along(acceptability_exposures)) {
  try({
    model <- glm(build_formula(acceptability_exposures[i], confounder_vars_stratified), 
                 data = never_married_data[never_married_data$country == "cambodia", ], 
                 family = binomial())
    res <- tidy(model, exponentiate = TRUE, conf.int = TRUE)
    term <- paste0(acceptability_exposures[i], "Yes")
    row <- res[res$term == term, ]
    if (nrow(row) > 0) {
      or_str <- sprintf("%.2f (%.2f–%.2f)", row$estimate, row$conf.low, row$conf.high)
      violence_acc_cambodia$NeverMarried_OR_CI[i] <- or_str
    }
  }, silent = TRUE)
}

# Separated No / Yes — Cambodia (acceptability)
for (i in seq_along(acceptability_exposures)) {

  d <- separated_data[
    separated_data$country == "cambodia" &
    !is.na(separated_data[[acceptability_exposures[i]]]) &
    !is.na(separated_data$v781_binary),
  ]

  d_no <- d[d[[acceptability_exposures[i]]] == "No", ]
  d_yes <- d[d[[acceptability_exposures[i]]] == "Yes", ]

  if (nrow(d_no) > 0) {
    violence_acc_cambodia$Separated_No_n[i] <- nrow(d_no)
    violence_acc_cambodia$Separated_No_HIV_n_pct[i] <-
      sprintf("%d (%.1f%%)",
              sum(d_no$v781_binary == 1),
              100 * sum(d_no$v781_binary == 1) / nrow(d_no))
  }

  if (nrow(d_yes) > 0) {
    violence_acc_cambodia$Separated_Yes_n[i] <- nrow(d_yes)
    violence_acc_cambodia$Separated_Yes_HIV_n_pct[i] <-
      sprintf("%d (%.1f%%)",
              sum(d_yes$v781_binary == 1),
              100 * sum(d_yes$v781_binary == 1) / nrow(d_yes))
  }
}

# Separated unadjusted
for (i in seq_along(acceptability_exposures)) {
  try({
    model <- glm(as.formula(paste0("v781_binary ~ ", acceptability_exposures[i])),
                 data = separated_data[separated_data$country == "cambodia", ],
                 family = binomial())
    res <- tidy(model, exponentiate = TRUE, conf.int = TRUE)
    term <- paste0(acceptability_exposures[i], "Yes")
    row <- res[res$term == term, ]
    if (nrow(row) > 0) {
      violence_acc_cambodia$Separated_Unadj_OR_CI[i] <-
        sprintf("%.2f (%.2f–%.2f)", row$estimate, row$conf.low, row$conf.high)
    }
  }, silent = TRUE)
}

# Separated adjusted
for (i in seq_along(acceptability_exposures)) {
  try({
    model <- glm(build_formula(acceptability_exposures[i], confounder_vars_stratified), 
                 data = separated_data[separated_data$country == "cambodia", ], 
                 family = binomial())
    res <- tidy(model, exponentiate = TRUE, conf.int = TRUE)
    term <- paste0(acceptability_exposures[i], "Yes")
    row <- res[res$term == term, ]
    if (nrow(row) > 0) {
      or_str <- sprintf("%.2f (%.2f–%.2f)", row$estimate, row$conf.low, row$conf.high)
      violence_acc_cambodia$Separated_OR_CI[i] <- or_str
    }
  }, silent = TRUE)
}

# Save results
violence_results <- createWorkbook()

addWorksheet(violence_results, "Philippines_Experience")
addWorksheet(violence_results, "Cambodia_Experience")
addWorksheet(violence_results, "Philippines_Acceptability")
addWorksheet(violence_results, "Cambodia_Acceptability")

writeData(violence_results, sheet = "Philippines_Experience", violence_exp_philippines)
writeData(violence_results, sheet = "Cambodia_Experience", violence_exp_cambodia)
writeData(violence_results, sheet = "Philippines_Acceptability", violence_acc_philippines)
writeData(violence_results, sheet = "Cambodia_Acceptability", violence_acc_cambodia)

saveWorkbook(violence_results, "violence_ORs.xlsx", overwrite = TRUE)

# format for manuscript

# table 2

phil_exp_counts <-
  violence_exp_philippines %>%
  pivot_longer(
    cols = c(
      Married_No_n, Married_No_HIV_n_pct,
      Married_Yes_n, Married_Yes_HIV_n_pct,
      NeverMarried_No_n, NeverMarried_No_HIV_n_pct,
      NeverMarried_Yes_n, NeverMarried_Yes_HIV_n_pct,
      Separated_No_n, Separated_No_HIV_n_pct,
      Separated_Yes_n, Separated_Yes_HIV_n_pct
    ),
    names_to = c("Marital", "Status", ".value"),
    names_pattern = "(Married|NeverMarried|Separated)_(No|Yes)_(.*)"
  )

phil_exp_or_unadj <-
  violence_exp_philippines %>%
  pivot_longer(
    cols = c(Married_Unadj_OR_CI, NeverMarried_Unadj_OR_CI, Separated_Unadj_OR_CI),
    names_to = "Marital",
    names_pattern = "(Married|NeverMarried|Separated)_Unadj_OR_CI",
    values_to = "Unadj_OR"
  )

phil_exp_or_adj <-
  violence_exp_philippines %>%
  pivot_longer(
    cols = c(Married_OR_CI, NeverMarried_OR_CI, Separated_OR_CI),
    names_to = "Marital",
    names_pattern = "(Married|NeverMarried|Separated)_OR_CI",
    values_to = "Adj_OR"
  )

phil_exp_table <-
  phil_exp_counts %>%
  left_join(phil_exp_or_unadj, by = c("Exposure", "Marital")) %>%
  left_join(phil_exp_or_adj, by = c("Exposure", "Marital")) %>%
  mutate(
    Unadj_OR = ifelse(Status == "No", "ref.", Unadj_OR),
    Adj_OR = ifelse(Status == "No", "ref.", Adj_OR),
    Status = factor(Status, levels = c("No", "Yes"))
  ) %>%
  arrange(Exposure, Status) %>%
  pivot_wider(
    names_from = Marital,
    values_from = c(n, HIV_n_pct, Unadj_OR, Adj_OR),
    names_glue = "{Marital}_{.value}"
  ) %>%
  mutate(Violence_type = ifelse(duplicated(Exposure), "No", Exposure)) %>%
  select(
    Violence_type,
    Married_n, Married_HIV_n_pct, Married_Unadj_OR, Married_Adj_OR,
    NeverMarried_n, NeverMarried_HIV_n_pct, NeverMarried_Unadj_OR, NeverMarried_Adj_OR,
    Separated_n, Separated_HIV_n_pct, Separated_Unadj_OR, Separated_Adj_OR
  )

View(phil_exp_table)

# table 3

cam_exp_counts <-
  violence_exp_cambodia %>%
  pivot_longer(
    cols = c(
      Married_No_n, Married_No_HIV_n_pct,
      Married_Yes_n, Married_Yes_HIV_n_pct,
      NeverMarried_No_n, NeverMarried_No_HIV_n_pct,
      NeverMarried_Yes_n, NeverMarried_Yes_HIV_n_pct,
      Separated_No_n, Separated_No_HIV_n_pct,
      Separated_Yes_n, Separated_Yes_HIV_n_pct
    ),
    names_to = c("Marital", "Status", ".value"),
    names_pattern = "(Married|NeverMarried|Separated)_(No|Yes)_(.*)"
  )

cam_exp_or_unadj <-
  violence_exp_cambodia %>%
  pivot_longer(
    cols = c(Married_Unadj_OR_CI, NeverMarried_Unadj_OR_CI, Separated_Unadj_OR_CI),
    names_to = "Marital",
    names_pattern = "(Married|NeverMarried|Separated)_Unadj_OR_CI",
    values_to = "Unadj_OR"
  )

cam_exp_or_adj <-
  violence_exp_cambodia %>%
  pivot_longer(
    cols = c(Married_OR_CI, NeverMarried_OR_CI, Separated_OR_CI),
    names_to = "Marital",
    names_pattern = "(Married|NeverMarried|Separated)_OR_CI",
    values_to = "Adj_OR"
  )

cam_exp_table <-
  cam_exp_counts %>%
  left_join(cam_exp_or_unadj, by = c("Exposure", "Marital")) %>%
  left_join(cam_exp_or_adj, by = c("Exposure", "Marital")) %>%
  mutate(
    Unadj_OR = ifelse(Status == "No", "ref.", Unadj_OR),
    Adj_OR = ifelse(Status == "No", "ref.", Adj_OR),
    Status = factor(Status, levels = c("No", "Yes"))
  ) %>%
  arrange(Exposure, Status) %>%
  pivot_wider(
    names_from = Marital,
    values_from = c(n, HIV_n_pct, Unadj_OR, Adj_OR),
    names_glue = "{Marital}_{.value}"
  ) %>%
  mutate(Violence_type = ifelse(duplicated(Exposure), "No", Exposure)) %>%
  select(
    Violence_type,
    Married_n, Married_HIV_n_pct, Married_Unadj_OR, Married_Adj_OR,
    NeverMarried_n, NeverMarried_HIV_n_pct, NeverMarried_Unadj_OR, NeverMarried_Adj_OR,
    Separated_n, Separated_HIV_n_pct, Separated_Unadj_OR, Separated_Adj_OR
  )

# table 4

phil_acc_counts <-
  violence_acc_philippines %>%
  pivot_longer(
    cols = c(
      Married_No_n, Married_No_HIV_n_pct,
      Married_Yes_n, Married_Yes_HIV_n_pct,
      NeverMarried_No_n, NeverMarried_No_HIV_n_pct,
      NeverMarried_Yes_n, NeverMarried_Yes_HIV_n_pct,
      Separated_No_n, Separated_No_HIV_n_pct,
      Separated_Yes_n, Separated_Yes_HIV_n_pct
    ),
    names_to = c("Marital", "Status", ".value"),
    names_pattern = "(Married|NeverMarried|Separated)_(No|Yes)_(.*)"
  )

phil_acc_or_unadj <-
  violence_acc_philippines %>%
  pivot_longer(
    cols = c(Married_Unadj_OR_CI, NeverMarried_Unadj_OR_CI, Separated_Unadj_OR_CI),
    names_to = "Marital",
    names_pattern = "(Married|NeverMarried|Separated)_Unadj_OR_CI",
    values_to = "Unadj_OR"
  )

phil_acc_or_adj <-
  violence_acc_philippines %>%
  pivot_longer(
    cols = c(Married_OR_CI, NeverMarried_OR_CI, Separated_OR_CI),
    names_to = "Marital",
    names_pattern = "(Married|NeverMarried|Separated)_OR_CI",
    values_to = "Adj_OR"
  )

phil_acc_table <-
  phil_acc_counts %>%
  left_join(phil_acc_or_unadj, by = c("Exposure", "Marital")) %>%
  left_join(phil_acc_or_adj, by = c("Exposure", "Marital")) %>%
  mutate(
    Unadj_OR = ifelse(Status == "No", "ref.", Unadj_OR),
    Adj_OR = ifelse(Status == "No", "ref.", Adj_OR),
    Status = factor(Status, levels = c("No", "Yes"))
  ) %>%
  arrange(Exposure, Status) %>%
  pivot_wider(
    names_from = Marital,
    values_from = c(n, HIV_n_pct, Unadj_OR, Adj_OR),
    names_glue = "{Marital}_{.value}"
  ) %>%
  mutate(Violence_type = ifelse(duplicated(Exposure), "No", Exposure)) %>%
  select(
    Violence_type,
    Married_n, Married_HIV_n_pct, Married_Unadj_OR, Married_Adj_OR,
    NeverMarried_n, NeverMarried_HIV_n_pct, NeverMarried_Unadj_OR, NeverMarried_Adj_OR,
    Separated_n, Separated_HIV_n_pct, Separated_Unadj_OR, Separated_Adj_OR
  )

# table 5

cam_acc_counts <-
  violence_acc_cambodia %>%
  pivot_longer(
    cols = c(
      Married_No_n, Married_No_HIV_n_pct,
      Married_Yes_n, Married_Yes_HIV_n_pct,
      NeverMarried_No_n, NeverMarried_No_HIV_n_pct,
      NeverMarried_Yes_n, NeverMarried_Yes_HIV_n_pct,
      Separated_No_n, Separated_No_HIV_n_pct,
      Separated_Yes_n, Separated_Yes_HIV_n_pct
    ),
    names_to = c("Marital", "Status", ".value"),
    names_pattern = "(Married|NeverMarried|Separated)_(No|Yes)_(.*)"
  )

cam_acc_or_unadj <-
  violence_acc_cambodia %>%
  pivot_longer(
    cols = c(Married_Unadj_OR_CI, NeverMarried_Unadj_OR_CI, Separated_Unadj_OR_CI),
    names_to = "Marital",
    names_pattern = "(Married|NeverMarried|Separated)_Unadj_OR_CI",
    values_to = "Unadj_OR"
  )

cam_acc_or_adj <-
  violence_acc_cambodia %>%
  pivot_longer(
    cols = c(Married_OR_CI, NeverMarried_OR_CI, Separated_OR_CI),
    names_to = "Marital",
    names_pattern = "(Married|NeverMarried|Separated)_OR_CI",
    values_to = "Adj_OR"
  )

cam_acc_table <-
  cam_acc_counts %>%
  left_join(cam_acc_or_unadj, by = c("Exposure", "Marital")) %>%
  left_join(cam_acc_or_adj, by = c("Exposure", "Marital")) %>%
  mutate(
    Unadj_OR = ifelse(Status == "No", "ref.", Unadj_OR),
    Adj_OR = ifelse(Status == "No", "ref.", Adj_OR),
    Status = factor(Status, levels = c("No", "Yes"))
  ) %>%
  arrange(Exposure, Status) %>%
  pivot_wider(
    names_from = Marital,
    values_from = c(n, HIV_n_pct, Unadj_OR, Adj_OR),
    names_glue = "{Marital}_{.value}"
  ) %>%
  mutate(Violence_type = ifelse(duplicated(Exposure), "No", Exposure)) %>%
  select(
    Violence_type,
    Married_n, Married_HIV_n_pct, Married_Unadj_OR, Married_Adj_OR,
    NeverMarried_n, NeverMarried_HIV_n_pct, NeverMarried_Unadj_OR, NeverMarried_Adj_OR,
    Separated_n, Separated_HIV_n_pct, Separated_Unadj_OR, Separated_Adj_OR
  )
  
wb <- createWorkbook()

addWorksheet(wb, "Philippines_Experience")
addWorksheet(wb, "Cambodia_Experience")
addWorksheet(wb, "Philippines_Acceptability")
addWorksheet(wb, "Cambodia_Acceptability")

writeData(wb, "Philippines_Experience", phil_exp_table)
writeData(wb, "Cambodia_Experience", cam_exp_table)
writeData(wb, "Philippines_Acceptability", phil_acc_table)
writeData(wb, "Cambodia_Acceptability", cam_acc_table)

saveWorkbook(wb, "violence_tables_formatted.xlsx", overwrite = TRUE)


