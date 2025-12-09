# load packages
pacman::p_load(dplyr, tidyr, haven, purrr, tableone, broom, writexl, stringr, readxl)

# set working directory
setwd("C:/Users/vl22683/OneDrive - University of Bristol/Documents/Publications/DHS and violence paper/data/")

# load clean data
southeast_asia_combined <- read_xlsx("../data/southeast_asia_combined_dataset.xlsx")

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

### association between experiences of violence and thinking beatings are justified ###

# --- Define exposures and labels ---
exposures <- c("v850a", "d128", "d104", "d106", "d107", "d108")
exposure_labels <- c(
  v850a = "Can respondent refuse sex",
  d128  = "Experienced any less severe violence",
  d104  = "Experienced any severe violence",
  d106  = "Experienced any sexual violence",
  d107  = "Ever told anyone else about violence",
  d108  = "Experienced any emotional violence"
)

justified_vars <- c("beating_justified_bin", "v744a","v744b","v744c","v744d","v744e")

# frequency table
setNames(lapply(exposures, \(v) table(analysis_data[[v]], useNA = "ifany")), exposures)
setNames(lapply(justified_vars, \(v) table(analysis_data[[v]], useNA = "ifany")), justified_vars)

analysis_data <- analysis_data %>%
  mutate(
    v850a = case_when(
      v850a == 1 ~ "Yes",
      v850a == 0 ~ "No",
      TRUE ~ NA_character_
    ),
    across(c(d128, d104, d106, d107, d108),
           ~ case_when(
             .x == TRUE ~ "Yes",
             .x == FALSE ~ "No",
             TRUE ~ NA_character_
           ))
  )

# frequency table
setNames(lapply(exposures, \(v) table(analysis_data[[v]], useNA = "ifany")), exposures)
lapply(exposures, function(v) {
  cat("\n\n=== Crosstab:", v, "===\n")
  print(table(analysis_data[[v]], analysis_data$beating_justified_bin,
              useNA = "ifany"))
})

# ------------------------
# Helper: format OR string
# ------------------------
fmt_or <- function(model) {
  if (is.null(model)) return(NA_character_)
  
  row <- broom::tidy(model, exponentiate = TRUE, conf.int = TRUE) %>%
    filter(term != "(Intercept)") %>% 
    slice(1)
  
  if (nrow(row) == 0) return(NA_character_)
  
  sprintf("%.2f (%.2f–%.2f)", row$estimate, row$conf.low, row$conf.high)
}

# ----------------------------------------------------------
# Build OR table for one country (or overall if no filter)
# ----------------------------------------------------------
build_outcome_table <- function(country_filter = NULL, label = "Overall") {
  
  df <- if (!is.null(country_filter)) {
    analysis_data %>% filter(tolower(country) == tolower(country_filter))
  } else analysis_data
  
  map_dfr(exposures, function(var) {
    
    # exposure and outcome (already Yes/No/NA)
    level_data <- df %>%
      transmute(
        exposure = !!sym(var),
        outcome  = beating_justified_bin
      ) %>%
      filter(!is.na(exposure) & !is.na(outcome))
    
    # simple counts
    ds <- level_data %>%
      count(exposure, outcome) %>%
      group_by(exposure) %>%
      summarise(
        n_total = sum(n),
        n_outcome = sum(n[outcome == "Yes"]),
        .groups = "drop"
      )
    
    # logistic regression (unadjusted)
    model <- tryCatch(
      if (n_distinct(level_data$exposure) > 1)
        glm(I(outcome == "Yes") ~ exposure, 
            data = level_data, family = binomial())
      else NULL,
      error = function(e) NULL
    )
    
    or_str <- fmt_or(model)
    
    # n (%) formatter
    n_str <- function(row) {
      if (nrow(row) == 0) return(NA_character_)
      pct <- 100 * row$n_outcome / row$n_total
      sprintf("%d (%.1f%%)", row$n_outcome, pct)
    }
    
    ds_yes <- ds %>% filter(exposure == "Yes")
    ds_no  <- ds %>% filter(exposure == "No")
    
    tibble(
      `Violence experience` = exposure_labels[var],
      Exposure = c("Yes", "No"),
      !!paste0(label, " - Beatings are justified, n (%)") :=
        c(n_str(ds_yes), n_str(ds_no)),
      !!paste0(label, " - Odds ratio (95% CI)") :=
        c(or_str, NA_character_)
    )
    
  })
}

# ---------------------
# Build tables
# ---------------------
table_ph  <- build_outcome_table("philippines", "Philippines")
table_kh  <- build_outcome_table("cambodia", "Cambodia")
table_all <- build_outcome_table(NULL, "Overall")

# ---------------------
# Merge
# ---------------------
final_table <- table_ph %>%
  full_join(table_kh, by = c("Violence experience", "Exposure")) %>%
  full_join(table_all, by = c("Violence experience", "Exposure")) %>%
  arrange(factor(`Violence experience`, levels = exposure_labels),
          factor(Exposure, levels = c("Yes","No")))

# ---------------------
# Save
# ---------------------
write_xlsx(final_table, "beating_justified_by_exposure.xlsx")

## association between violence and hiv testing ##

# ----------------------------
# Load data
# ----------------------------
southeast_asia_combined <- read_xlsx("../data/southeast_asia_combined_dataset.xlsx")

# ----------------------------
# Config: variables and labels
# ----------------------------

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

# binary hiv testing outcome
analysis_data <- southeast_asia_combined %>%
  filter(!is.na(v781)) %>%
  mutate(v781_binary = case_when(
    v781 == 1 ~ 1,  
    v781 == 0 ~ 0,  
    TRUE ~ NA_real_ 
  )) %>%
  filter(!is.na(v781_binary))

# any wife-beating justified (v744a–v744e) → derive as "0"/"1"
just_vars <- c("v744a","v744b","v744c","v744d","v744e")

analysis_data <- analysis_data %>%
  mutate(across(all_of(just_vars), ~ as.numeric(as.character(.x)))) %>%   # ensure numeric
  mutate(
    beating_justified_bin =
      case_when(
        rowSums(across(all_of(just_vars), ~ .x == 1), na.rm = TRUE) > 0 ~ 1,  # any Yes
        rowSums(across(all_of(just_vars), ~ .x %in% c(0,1)), na.rm = TRUE) > 0 ~ 0,  # at least observed 0/1
        TRUE ~ NA_real_                                                    # all missing/don't know
      ),
    beating_justified_bin = factor(beating_justified_bin, levels = c(0,1),
                                   labels = c("No","Yes"))
  )

# define violence variable
violence_vars <- c(
  "s826f",                     # Justifies DV: wife ask use condom (0/1/8/9)
  "v744a","v744b","v744c","v744d","v744e",  # Wife beating justified items (0/1/8/9)
  "v850a",                     # Can respondent refuse sex (0/1/8/9 or 0/1)
  "d104","d106","d107","d108","d128",        # Logical TRUE/FALSE
  "beating_justified_bin"
)

variable_labels <- tibble::tribble(
  ~violence_variable, ~variable_label,
  "d104",  "Experienced any emotional violence",
  "d106",  "Experienced any less severe violence",
  "d107",  "Experienced any severe violence",
  "d108",  "Experienced any sexual violence",
  "d128",  "Ever told anyone else about violence",
  "s826f", "Justifies DV: wife ask use condom",
  "v744a", "Wife beating justified: goes out without telling husband",
  "v744b", "Wife beating justified: neglects children",
  "v744c", "Wife beating justified: argues with husband",
  "v744d", "Wife beating justified: refuses sex with husband",
  "v744e", "Wife beating justified: burns the food",
  "v850a", "Can respondent refuse sex",
  "beating_justified_bin", "Wife beating justified (any reason)"
)

level_labels <- tibble::tribble(
  ~level, ~level_description,
  "0", "No",
  "1", "Yes",
  "8", "Don't know",
  "9", "Missing"
)

# convert vars from numeric to factors
analysis_data <- analysis_data %>%
  mutate(across(any_of(violence_vars), ~ as.factor(.x)))

# ----------------------------
# Stratified models by v502
# ----------------------------
# v502: 0=Never married, 1=Currently married, 2=Formerly married (adjust if needed)
v502_levels <- sort(unique(analysis_data$v502[!is.na(analysis_data$v502)]))
sheet_names <- c(`0`="Never_married", `1`="Currently_married", `2`="Formerly_married")

# Helper: tidy model to OR CI
fmt_results <- function(model) {
  broom::tidy(model, exponentiate = TRUE, conf.int = TRUE)
}

# Build descriptive stats for one predictor inside a stratum
desc_one <- function(level_data, var) {
  if (!var %in% names(level_data)) return(NULL)

  if (str_detect(var, "^d\\d{3}$")) {
    # logical → map TRUE/FALSE to "1"/"0"
    level_data %>%
      mutate(var_level = ifelse(.data[[var]] == TRUE, "1",
                                ifelse(.data[[var]] == FALSE, "0", NA_character_))) %>%
      filter(!is.na(var_level)) %>%
      group_by(var_level) %>%
      summarise(
        n_level = n(),
        n_hiv_tested = sum(v781_binary == 1, na.rm = TRUE),
        prop_hiv_tested = mean(v781_binary == 1, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      mutate(
        violence_variable = var,
        level = var_level,
        percent_hiv_tested = round(100 * prop_hiv_tested, 1)
      ) %>%
      select(violence_variable, level, n_level, n_hiv_tested, percent_hiv_tested)
  } else {
    # categorical (0/1/8/9 or strings)
    level_data %>%
      filter(!is.na(.data[[var]])) %>%
      group_by(level = as.character(.data[[var]])) %>%
      summarise(
        n_level = n(),
        n_hiv_tested = sum(v781_binary == 1, na.rm = TRUE),
        prop_hiv_tested = mean(v781_binary == 1, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      mutate(
        violence_variable = var,
        percent_hiv_tested = round(100 * prop_hiv_tested, 1)
      ) %>%
      select(violence_variable, level, n_level, n_hiv_tested, percent_hiv_tested)
  }
}

# Fit one model safely inside a stratum
fit_one <- function(level_data, var) {
  if (!var %in% names(level_data)) return(NULL)

  model_df <- level_data %>%
    select(v781_binary, country, !!rlang::sym(var)) %>%
    drop_na() %>%
    mutate(country = droplevels(as.factor(country)))

  # predictor as factor
  model_df[[var]] <- droplevels(as.factor(model_df[[var]]))

  # Need ≥ 2 exposure levels
  if (nrow(model_df) == 0 || dplyr::n_distinct(model_df[[var]]) < 2) return(NULL)

  use_country <- dplyr::n_distinct(model_df$country) >= 2
  fml <- as.formula(paste("v781_binary ~", var, if (use_country) "+ country" else ""))

  tryCatch(glm(fml, data = model_df, family = binomial()),
           error = function(e) NULL)
}

# Build results per stratum (sheet)
stratified_results <- list()

for (level in v502_levels) {
  level_data <- analysis_data %>% filter(v502 == level)

  # Descriptive stats
  level_descriptive_stats <- purrr::map_dfr(violence_vars, ~ desc_one(level_data, .x))

  # Reference rows (level "0")
  level_reference_rows <- level_descriptive_stats %>%
    filter(level == "0") %>%
    mutate(
      term = "Reference",
      odds_ratio = NA_real_, OR_lower_CI = NA_real_, OR_upper_CI = NA_real_, p.value = NA_real_
    ) %>%
    left_join(variable_labels, by = "violence_variable") %>%
    left_join(level_labels, by = "level") %>%
    select(violence_variable, variable_label, term, level, level_description,
           n_level, n_hiv_tested, percent_hiv_tested,
           odds_ratio, OR_lower_CI, OR_upper_CI, p.value)

  # Models
  level_models <- lapply(violence_vars, function(v) fit_one(level_data, v))
  names(level_models) <- violence_vars
  level_models <- Filter(Negate(is.null), level_models)

  # Tidy and prepare exposed rows
  level_results <- lapply(level_models, fmt_results)

  level_odds_ratio_rows <- if (length(level_results)) {
    bind_rows(level_results, .id = "violence_variable") %>%
      rename(odds_ratio = estimate, OR_lower_CI = conf.low, OR_upper_CI = conf.high) %>%
      mutate(
        level = case_when(
          term == "(Intercept)" ~ "Reference",
          str_detect(term, "TRUE$")  ~ "1",
          str_detect(term, "FALSE$") ~ "0",
          TRUE ~ str_extract(term, "\\d+$")    # captures 1/8/9 from v744*, s826f, v850a
        )
      ) %>%
      filter(term != "(Intercept)") %>%
      filter(!str_detect(term, "^country")) %>%
      left_join(variable_labels, by = "violence_variable") %>%
      left_join(level_labels,    by = "level") %>%
      left_join(level_descriptive_stats, by = c("violence_variable","level")) %>%
      select(violence_variable, variable_label, term, level, level_description,
             n_level, n_hiv_tested, percent_hiv_tested,
             odds_ratio, OR_lower_CI, OR_upper_CI, p.value)
  } else {
    tibble()
  }

  # Combine
  level_final <- bind_rows(level_reference_rows, level_odds_ratio_rows) %>%
    arrange(violence_variable, term)

  sheet_name <- unname(ifelse(as.character(level) %in% names(sheet_names),
                              sheet_names[as.character(level)],
                              paste0("Level_", level)))

  stratified_results[[sheet_name]] <- level_final
}

# Write workbook (one sheet per marital status)
write_xlsx(stratified_results, "violence_hiv_by_marital_status.xlsx")

# Ensure country format
analysis_data <- analysis_data %>% dplyr::mutate(country = tolower(as.character(country)))

# v502 sheet names
sheet_names <- c(`0`="Never_married", `1`="Currently_married", `2`="Formerly_married")

# Helper to build one workbook for a given country
build_country_workbook <- function(cty) {
  df_country <- analysis_data %>% dplyr::filter(country == tolower(cty))
  v502_levels <- sort(unique(df_country$v502[!is.na(df_country$v502)]))
  wb <- list()

  for (level in v502_levels) {
    level_data <- df_country %>% dplyr::filter(v502 == level)

    # Descriptive
    level_descriptive_stats <- purrr::map_dfr(violence_vars, ~ desc_one(level_data, .x))

    # Reference rows
    level_reference_rows <- level_descriptive_stats %>%
      dplyr::filter(level == "0") %>%
      dplyr::mutate(
        term = "Reference",
        odds_ratio = NA_real_, OR_lower_CI = NA_real_, OR_upper_CI = NA_real_, p.value = NA_real_
      ) %>%
      dplyr::left_join(variable_labels, by = "violence_variable") %>%
      dplyr::left_join(level_labels, by = "level") %>%
      dplyr::select(violence_variable, variable_label, term, level, level_description,
                    n_level, n_hiv_tested, percent_hiv_tested,
                    odds_ratio, OR_lower_CI, OR_upper_CI, p.value)

    # Models: reuse fit_one but without country adjustment (country is constant here)
    fit_one_country <- function(level_data, var) {
      if (!var %in% names(level_data)) return(NULL)
      model_df <- level_data %>%
        dplyr::select(v781_binary, !!rlang::sym(var)) %>%
        tidyr::drop_na()
      model_df[[var]] <- droplevels(as.factor(model_df[[var]]))
      if (nrow(model_df) == 0 || dplyr::n_distinct(model_df[[var]]) < 2) return(NULL)
      tryCatch(glm(stats::as.formula(paste("v781_binary ~", var)),
                   data = model_df, family = binomial()),
               error = function(e) NULL)
    }

    level_models <- lapply(violence_vars, function(v) fit_one_country(level_data, v))
    names(level_models) <- violence_vars
    level_models <- Filter(Negate(is.null), level_models)

    level_results <- lapply(level_models, function(m) broom::tidy(m, exponentiate = TRUE, conf.int = TRUE))

    level_odds_ratio_rows <- if (length(level_results)) {
      dplyr::bind_rows(level_results, .id = "violence_variable") %>%
        dplyr::rename(odds_ratio = estimate, OR_lower_CI = conf.low, OR_upper_CI = conf.high) %>%
        dplyr::mutate(
          level = dplyr::case_when(
            term == "(Intercept)" ~ "Reference",
            stringr::str_detect(term, "TRUE$")  ~ "1",
            stringr::str_detect(term, "FALSE$") ~ "0",
            TRUE ~ stringr::str_extract(term, "\\d+$")
          )
        ) %>%
        dplyr::filter(term != "(Intercept)") %>%
        dplyr::left_join(variable_labels, by = "violence_variable") %>%
        dplyr::left_join(level_labels,    by = "level") %>%
        dplyr::left_join(level_descriptive_stats, by = c("violence_variable","level")) %>%
        dplyr::select(violence_variable, variable_label, term, level, level_description,
                      n_level, n_hiv_tested, percent_hiv_tested,
                      odds_ratio, OR_lower_CI, OR_upper_CI, p.value)
    } else {
      tibble::tibble()
    }

    level_final <- dplyr::bind_rows(level_reference_rows, level_odds_ratio_rows) %>%
      dplyr::arrange(violence_variable, term)

    sheet_name <- unname(ifelse(as.character(level) %in% names(sheet_names),
                                sheet_names[as.character(level)],
                                paste0("Level_", level)))
    wb[[sheet_name]] <- level_final
  }

  wb
}

# Build and write separate workbooks
wb_kh <- build_country_workbook("cambodia")
wb_ph <- build_country_workbook("philippines")

writexl::write_xlsx(wb_kh, "violence_hiv_by_marital_status_cambodia.xlsx")
writexl::write_xlsx(wb_ph, "violence_hiv_by_marital_status_philippines.xlsx")

# ---- Format helpers ----
fmt_or_ci <- function(or, lo, hi) {
  ifelse(is.na(or), "", sprintf("%.2f (%.2f–%.2f)", or, lo, hi))
}

format_sheet <- function(df) {
  # keep only 0/1 rows, build strings, order by label then No/Yes
  df %>%
    dplyr::filter(level %in% c("0","1")) %>%
    dplyr::mutate(
      `Violence type` = variable_labels$variable_label[
        match(violence_variable, variable_labels$violence_variable)
      ],
      Level = ifelse(level == "1", "Yes", "No"),
      `Exposed, n` = n_level,
      `HIV tested, n (%)` = sprintf("%d (%.1f%%)", n_hiv_tested, percent_hiv_tested),
      `Odds ratio (95% CI)` = dplyr::if_else(
        Level == "No", "ref.", fmt_or_ci(odds_ratio, OR_lower_CI, OR_upper_CI)
      )
    ) %>%
    dplyr::select(`Violence type`, Level, `Exposed, n`, `HIV tested, n (%)`, `Odds ratio (95% CI)`) %>%
    dplyr::arrange(
      factor(`Violence type`, levels = variable_labels$variable_label),
      factor(Level, levels = c("No","Yes"))
    )
}

# ---- Apply to the country workbooks you already built (wb_kh, wb_ph) ----
wb_kh_fmt <- lapply(wb_kh, format_sheet)
wb_ph_fmt <- lapply(wb_ph, format_sheet)

writexl::write_xlsx(wb_kh_fmt, "violence_hiv_by_marital_status_cambodia_formatted.xlsx")
writexl::write_xlsx(wb_ph_fmt, "violence_hiv_by_marital_status_philippines_formatted.xlsx")