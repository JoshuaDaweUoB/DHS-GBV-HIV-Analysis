## association between violence and hiv testing ##

# load packages
pacman::p_load(dplyr, tidyr, haven, purrr, tableone, broom, writexl, stringr, openxlsx)

# set working directory
setwd("C:/Users/vl22683/OneDrive - University of Bristol/Documents/Publications/DHS and violence paper/data/")

# load clean data
southeast_asia_combined <- readRDS("../data/southeast_asia_combined_dataset.rds")

# create workbook for results
violence_results <- loadWorkbook("violence_ORs.xlsx")

# stratify data by marriage type
married_data <- southeast_asia_combined %>% filter(v502 == 1)
never_married_data <- southeast_asia_combined %>% filter(v502 == 0)
separated_data <- southeast_asia_combined %>% filter(v502 == 2)

# exposures
exposures <- c("any_violence", "emotional_violence_bin", "sexual_violence_bin", "less_severe_violence_bin", "severe_violence_bin")
exposure_labels <- c("Any violence", "Emotional violence", "Sexual violence", "Less severe violence", "Severe violence")

# confounders
confounder_vars_stratified <- c("household_head", "religion", "employed_bin", "residence_3cat", "children_under5_4cat")  

# Build formula using confounder_vars
build_formula <- function(exposure, confounders) {
  confounder_str <- paste(confounders, collapse = " + ")
  as.formula(paste("v781_binary ~", exposure, "+", confounder_str))
}

## experiences of violence

# Philippines

violence_exp_philippines <- data.frame(
  Exposure = exposure_labels,
  Married_OR_CI = NA_character_,
  NeverMarried_OR_CI = NA_character_,
  Separated_OR_CI = NA_character_,
  stringsAsFactors = FALSE
)

# Married
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

# Never married
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

# Separated
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

# save results
writeData(violence_results, sheet = "Philippines_Experience", violence_exp_philippines)

# cambodia

violence_exp_cambodia <- data.frame(
  Exposure = exposure_labels,
  Married_OR_CI = NA_character_,
  NeverMarried_OR_CI = NA_character_,
  Separated_OR_CI = NA_character_,
  stringsAsFactors = FALSE
)

# Married
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

# Never married
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

# Separated
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

# save results
writeData(violence_results, sheet = "Cambodia_Experience", violence_exp_cambodia)

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
  Married_OR_CI = NA_character_,
  NeverMarried_OR_CI = NA_character_,
  Separated_OR_CI = NA_character_,
  stringsAsFactors = FALSE
)

# Married
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

# Never married
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
      acceptability_resviolence_acc_philippinesults$NeverMarried_OR_CI[i] <- or_str
    }
  }, silent = TRUE)
}

# Separated
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

# Save results
writeData(violence_results, sheet = "Philippines_Acceptability", violence_acc_philippines)

# cambodia

violence_acc_cambodia <- data.frame(
  Exposure = acceptability_labels,
  Married_OR_CI = NA_character_,
  NeverMarried_OR_CI = NA_character_,
  Separated_OR_CI = NA_character_,
  stringsAsFactors = FALSE
)

# Married
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

# Never married
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

# Separated
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
writeData(violence_results, sheet = "Cambodia_Acceptability", violence_acc_cambodia)
saveWorkbook(violence_results, "violence_ORs.xlsx", overwrite = TRUE)




# any beating justified
model <- glm(build_formula("beating_justified_bin", confounder_vars_stratified), data = separated_married_data[separated_married_data$country == "philippines", ], family = binomial())
summary(model)
print(res$term)
exp(cbind(OR = coef(model), confint(model)))



















# Cambodia

# emotional violence
model <- glm(v781_binary ~ d104, data = married_data[married_data$country == "cambodia", ], family = binomial())
exp(cbind(OR = coef(model), confint(model)))

# emotional violence
model <- glm(build_formula("d104", confounder_vars_stratified), data = married_data[married_data$country == "cambodia", ], family = binomial())
exp(cbind(OR = coef(model), confint(model)))

# less severe violence
model <- glm(build_formula("d106", confounder_vars_stratified), data = married_data[married_data$country == "cambodia", ], family = binomial())
exp(cbind(OR = coef(model), confint(model)))

# severe violence
model <- glm(build_formula("d107", confounder_vars_stratified), data = married_data[married_data$country == "cambodia", ], family = binomial())
exp(cbind(OR = coef(model), confint(model)))

# sexual violence
model <- glm(build_formula("d108", confounder_vars_stratified), data = married_data[married_data$country == "cambodia", ], family = binomial())
exp(cbind(OR = coef(model), confint(model)))

# any violence
model <- glm(build_formula("any_violence", confounder_vars_stratified), data = married_data[married_data$country == "cambodia", ], family = binomial())
exp(cbind(OR = coef(model), confint(model)))

## acceptability of violence
acceptability_vars <- c("s826f", "v744a", "v744b", "v744c", "v744d", "v744e", "v850a", "beating_justified_bin")

# Philippines

# any beating justified
model <- glm(build_formula("beating_justified_bin", confounder_vars_stratified), data = married_data[married_data$country == "philippines", ], family = binomial())
summary(model)
print(res$term)
exp(cbind(OR = coef(model), confint(model)))

# wife beating justified if she goes out without telling husband
model <- glm(build_formula("beating_justified_out_bin", confounder_vars_stratified), data = married_data[married_data$country == "philippines", ], family = binomial())
summary(model)
exp(cbind(OR = coef(model), confint(model)))

# wife beating justified if she neglects the children
model <- glm(build_formula("beating_justified_neglect_bin", confounder_vars_stratified), data = married_data[married_data$country == "philippines", ], family = binomial())
summary(model)
exp(cbind(OR = coef(model), confint(model)))

# wife beating justified if she goes out without telling husband
model <- glm(build_formula("beating_justified_argue_bin", confounder_vars_stratified), data = married_data[married_data$country == "philippines", ], family = binomial())
summary(model)
exp(cbind(OR = coef(model), confint(model)))

# wife beating justified if she refuses to have sex with husband
model <- glm(build_formula("beating_justified_refuse_sex_bin", confounder_vars_stratified), data = married_data[married_data$country == "philippines", ], family = binomial())
summary(model)
exp(cbind(OR = coef(model), confint(model)))

# wife beating justified if she burns the food
model <- glm(build_formula("beating_justified_burn_food_bin", confounder_vars_stratified), data = married_data[married_data$country == "philippines", ], family = binomial())
summary(model)
exp(cbind(OR = coef(model), confint(model)))

# can respondent refuse sex 
model <- glm(build_formula("can_refuse_sex_bin", confounder_vars_stratified), data = married_data[married_data$country == "philippines", ], family = binomial())
summary(model)
exp(cbind(OR = coef(model), confint(model)))


# Cambodia

# any beating justified
model <- glm(build_formula("beating_justified_bin", confounder_vars_stratified), data = married_data[married_data$country == "cambodia", ], family = binomial())
summary(model)
exp(cbind(OR = coef(model), confint(model)))

# wife beating justified if she goes out without telling husband
model <- glm(build_formula("beating_justified_out_bin", confounder_vars_stratified), data = married_data[married_data$country == "cambodia", ], family = binomial())
summary(model)
exp(cbind(OR = coef(model), confint(model)))

# wife beating justified if she neglects the children
model <- glm(build_formula("beating_justified_neglect_bin", confounder_vars_stratified), data = married_data[married_data$country == "cambodia", ], family = binomial())
summary(model)
exp(cbind(OR = coef(model), confint(model)))

# wife beating justified if she goes out without telling husband
model <- glm(build_formula("beating_justified_argue_bin", confounder_vars_stratified), data = married_data[married_data$country == "cambodia", ], family = binomial())
summary(model)
exp(cbind(OR = coef(model), confint(model)))

# wife beating justified if she refuses to have sex with husband
model <- glm(build_formula("beating_justified_refuse_sex_bin", confounder_vars_stratified), data = married_data[married_data$country == "cambodia", ], family = binomial())
summary(model)
exp(cbind(OR = coef(model), confint(model)))

# wife beating justified if she burns the food
model <- glm(build_formula("beating_justified_burn_food_bin", confounder_vars_stratified), data = married_data[married_data$country == "cambodia", ], family = binomial())
summary(model)
exp(cbind(OR = coef(model), confint(model)))

# can respondent refuse sex 
model <- glm(build_formula("can_refuse_sex_bin", confounder_vars_stratified), data = married_data[married_data$country == "cambodia", ], family = binomial())
summary(model)
exp(cbind(OR = coef(model), confint(model)))
