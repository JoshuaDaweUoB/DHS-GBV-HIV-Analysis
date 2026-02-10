## association between violence and hiv testing ##

# load packages
pacman::p_load(dplyr, tidyr, haven, purrr, tableone, broom, writexl, stringr, openxlsx)

# set working directory
setwd("C:/Users/vl22683/OneDrive - University of Bristol/Documents/Publications/DHS and violence paper/data/")

# load clean data
southeast_asia_combined <- readRDS("../data/southeast_asia_combined_dataset.rds")

# values for flowchart
table(southeast_asia_combined$v781, useNA = "always")
table(southeast_asia_combined$any_violence, useNA = "always")
table(southeast_asia_combined$beating_justified_bin, useNA = "always")
table(southeast_asia_combined$any_violence, southeast_asia_combined$beating_justified_bin, useNA = "always")
sum(!is.na(southeast_asia_combined$any_violence) & is.na(southeast_asia_combined$beating_justified_bin))

justified_only <- sum(!is.na(southeast_asia_combined$beating_justified_bin) & is.na(southeast_asia_combined$any_violence))
any_violence_only <- sum(!is.na(southeast_asia_combined$any_violence) & is.na(southeast_asia_combined$beating_justified_bin))
both <- sum(!is.na(southeast_asia_combined$any_violence) & !is.na(southeast_asia_combined$beating_justified_bin))
justified_only
any_violence_only
both
sum(is.na(southeast_asia_combined$any_violence) & is.na(southeast_asia_combined$beating_justified_bin))

# create workbook for results
violence_results <- loadWorkbook("violence_ORs.xlsx")

# stratify data by marriage type
married_data <- southeast_asia_combined %>% filter(v502 == 1)
never_married_data <- southeast_asia_combined %>% filter(v502 == 0)
separated_data <- southeast_asia_combined %>% filter(v502 == 2)

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
  "beating_justified_out_bin",
  "beating_justified_neglect_bin",
  "beating_justified_argue_bin",
  "beating_justified_refuse_sex_bin",
  "beating_justified_burn_food_bin",
  "can_refuse_sex_bin",
  "beating_justified_bin"
)
acceptability_labels <- c(
  "Justifies DV (condom)",
  "Beating justified: out",
  "Beating justified: neglect",
  "Beating justified: argue",
  "Beating justified: refuse sex",
  "Beating justified: burn food",
  "Can refuse sex",
  "Any beating justified"
)

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
