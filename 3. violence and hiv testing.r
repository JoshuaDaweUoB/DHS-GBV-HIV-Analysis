## association between violence and hiv testing ##

# load packages
pacman::p_load(dplyr, tidyr, haven, purrr, tableone, broom, writexl, stringr, openxlsx)

# load clean data
southeast_asia_combined <- readRDS("../data/southeast_asia_combined_dataset.rds")

# create workbook for results
violence_results <- loadWorkbook("violence_ORs.xlsx")

# violence variables
violence_vars <- c("d104", "d106", "d107", "d108", "d128", "any_violence")
acceptability_vars <- c("s826f", "v744a", "v744b", "v744c", "v744d", "v744e", "v850a", "beating_justified_bin")

# stratify data by marriage type
married_data <- southeast_asia_combined %>% filter(v502 == 1)
never_married_data <- southeast_asia_combined %>% filter(v502 == 0)
separated_data <- southeast_asia_combined %>% filter(v502 == 2)

for (var in exposures) {
  cat("\n", var, ":\n")
  print(table(married_data[married_data$country == "philippines", var], useNA = "ifany"))
}

# confounders
confounder_vars_stratified <- c("v150_standard", "v130_standard", "v717_standard", "v140", "v137_cat")  

# Build formula using confounder_vars
build_formula <- function(exposure, confounders) {
  confounder_str <- paste(confounders, collapse = " + ")
  as.formula(paste("v781_binary ~", exposure, "+", confounder_str))
}

## experiences of violence

# Philippines

exposures <- c("emotional_violence_bin", "less_severe_violence_bin", "severe_violence_bin", "sexual_violence_bin", "any_violence")
exposure_labels <- c("Emotional violence", "Less severe violence", "Severe violence", "Sexual violence", "Any violence")

results <- data.frame(
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
    results$Married_OR_CI[i] <- or_str
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
    results$NeverMarried_OR_CI[i] <- or_str
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
    results$Separated_OR_CI[i] <- or_str
  }
}

# save results
addWorksheet(violence_results, "philippines")
writeData(violence_results, "philippines", results)

# cambodia

results <- data.frame(
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
    results$Married_OR_CI[i] <- or_str
  }
}

# Never married
for (i in seq_along(exposures)) {
  model <- glm(build_formula(exposures[i], confounder_vars_stratified), data = never_married_data[never_married_data$country == "cambodia", ], family = binomial())
  res <- tidy(model, exponentiate = TRUE, conf.int = TRUE)
  term <- paste0(exposures[i], "Yes")
  row <- res[res$term == term, ]
  if (nrow(row) > 0) {
    or_str <- sprintf("%.2f (%.2f–%.2f)", row$estimate, row$conf.low, row$conf.high)
    results$NeverMarried_OR_CI[i] <- or_str
  }
}

# Separated
for (i in seq_along(exposures)) {
  model <- glm(build_formula(exposures[i], confounder_vars_stratified), data = separated_data[separated_data$country == "cambodia", ], family = binomial())
  res <- tidy(model, exponentiate = TRUE, conf.int = TRUE)
  term <- paste0(exposures[i], "Yes")
  row <- res[res$term == term, ]
  if (nrow(row) > 0) {
    or_str <- sprintf("%.2f (%.2f–%.2f)", row$estimate, row$conf.low, row$conf.high)
    results$Separated_OR_CI[i] <- or_str
  }
}

# save results
addWorksheet(violence_results, "cambodia")
writeData(violence_results, "cambodia", results)
saveWorkbook(violence_results, "violence_ORs.xlsx", overwrite = TRUE)





























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
