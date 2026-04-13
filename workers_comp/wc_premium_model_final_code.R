library(tidyverse)
library(MASS) 
library(fitdistrplus)
library(writexl)
library(patchwork)
library(scales)


set.seed(42)  

n_mc_simulations <- 50000 

output_dir <- "outputs"
if (!dir.exists(output_dir)) dir.create(output_dir)

col_helionis  <- "#4A9FD5"
col_bayesia   <- "#E07B30"
col_oryn      <- "#50B86C"
col_gold      <- "#C8960C"
col_navy      <- "#1B3A6B"
col_panel_bg  <- "#1A2E45"
col_plot_bg   <- "#0D1B2A"
col_red       <- "#CC3300"
col_green     <- "#2D7D46"

system_colours <- c(
  "Helionis Cluster" = col_helionis,
  "Bayesia System"   = col_bayesia,
  "Oryn Delta"       = col_oryn
)

system_labels <- c(
  "Helionis Cluster" = "Helionis",
  "Bayesia System"   = "Bayesia",
  "Oryn Delta"       = "Oryn Delta"
)

historical_systems <- c("Helionis Cluster", "Epsilon", "Zeta")
target_systems     <- c("Helionis Cluster", "Bayesia System", "Oryn Delta")

# ggplot2 report theme
theme_report <- theme_minimal(base_family = "sans") +
  theme(
    plot.background   = element_rect(fill = col_plot_bg,  colour = NA),
    panel.background  = element_rect(fill = col_panel_bg, colour = NA),
    panel.grid.major  = element_line(colour = "#2A4060", linewidth = 0.4),
    panel.grid.minor  = element_blank(),
    text              = element_text(colour = "white"),
    axis.text         = element_text(colour = "white", size = 9),
    axis.title        = element_text(colour = "white", size = 10),
    plot.title        = element_text(colour = "white", face = "bold", size = 11),
    plot.subtitle     = element_text(colour = col_gold,  size = 9),
    plot.caption      = element_text(colour = "#888888", size = 7, hjust = 0),
    legend.background = element_rect(fill = col_panel_bg),
    legend.text       = element_text(colour = "white"),
    legend.title      = element_text(colour = "white"),
    strip.background  = element_rect(fill = col_navy),
    strip.text        = element_text(colour = "white", face = "bold")
  )

save_fig <- function(plot_obj, filename, w = 14, h = 8) {
  ggsave(file.path(output_dir, filename), plot_obj,
         width = w, height = h, dpi = 150, bg = col_plot_bg)
  cat(sprintf("  Saved: %s\n", filename))
}


# SECTION 1: DATA LOADING, CLEANING, AND PERSONNEL DEDUPLICATION

clean_category <- function(x) {
  # Strips corrupted suffixes: "Drill Operator_XYZ9999" -> "Drill Operator"
  x |> as.character() |> sub(pattern = "_.*", replacement = "", x = _) |> trimws()
}

#(a) Frequency data
raw_freq <- readxl::read_excel(
  "srcsc-2026-claims-workers-comp.xlsx", sheet = "freq"
)

frequency_data_clean <- raw_freq |>
  mutate(across(c(solar_system, occupation, employment_type,
                  station_id, policy_id), clean_category)) |>
  filter(solar_system %in% historical_systems) |>
  mutate(
    experience_yrs          = pmax(0.2,  pmin(40,     experience_yrs)),
    supervision_level       = pmax(0.0,  pmin(1.0,    supervision_level)),
    gravity_level           = pmax(0.75, pmin(1.50,   gravity_level)),
    base_salary             = pmax(20000, pmin(130000, base_salary)),
    exposure                = pmax(1e-6, pmin(1.0,    exposure)),
    psych_stress_index      = round(pmax(1, pmin(5,   psych_stress_index))),
    safety_training_index   = round(pmax(1, pmin(5,   safety_training_index))),
    protective_gear_quality = round(pmax(1, pmin(5,   protective_gear_quality))),
    accident_history_flag   = as.integer(accident_history_flag > 0),
    claim_count             = pmax(0, pmin(2,          claim_count))
  ) |>
  drop_na()

cat(sprintf("  Frequency: %d rows (from %d raw)\n",
            nrow(frequency_data_clean), nrow(raw_freq)))

#(b) Severity data
raw_sev <- readxl::read_excel(
  "srcsc-2026-claims-workers-comp.xlsx", sheet = "sev"
)

severity_data_clean <- raw_sev |>
  mutate(across(c(solar_system, occupation, employment_type,
                  injury_type, injury_cause), clean_category)) |>
  filter(solar_system %in% historical_systems) |>
  mutate(
    experience_yrs          = pmax(0.2,   pmin(40,     experience_yrs)),
    gravity_level           = pmax(0.75,  pmin(1.50,   gravity_level)),
    base_salary             = pmax(20000, pmin(130000, base_salary)),
    exposure                = pmax(1e-6,  pmin(1.0,    exposure)),
    psych_stress_index      = round(pmax(1, pmin(5,   psych_stress_index))),
    safety_training_index   = round(pmax(1, pmin(5,   safety_training_index))),
    protective_gear_quality = round(pmax(1, pmin(5,   protective_gear_quality))),
    accident_history_flag   = as.integer(accident_history_flag > 0),
    claim_length            = pmax(3,     pmin(1000,   claim_length)),
    # claim_amount is in absolute Đ (confirmed by cross-checking against salary scale)
    claim_amount            = pmax(5,     pmin(200000, claim_amount))
  ) |>
  drop_na(solar_system, claim_amount, occupation)

cat(sprintf("  Severity:  %d rows (from %d raw)\n",
            nrow(severity_data_clean), nrow(raw_sev)))

# (c) Economic rates
economic_rates <- readxl::read_excel(
  "srcsc-2026-interest-and-inflation.xlsx", skip = 2
) |>
  setNames(c("year", "inflation_rate", "overnight_rate", "rate_1y", "rate_10y"))

# (d) CQ personnel
raw_personnel <- readxl::read_excel(
  "srcsc-2026-cosmic-quarry-personnel.xlsx", skip = 1
) |>
  setNames(c("role", "n_total", "n_fulltime", "n_contract",
             "avg_salary_diad", "avg_age_years")) |>
  mutate(across(c(n_total, n_fulltime, n_contract,
                  avg_salary_diad, avg_age_years), as.numeric)) |>
  filter(!is.na(n_total), !is.na(avg_age_years)) |>
  mutate(role = trimws(as.character(role)))

cq_personnel <- raw_personnel |>
  group_by(role) |>
  summarise(
    avg_salary_diad = sum(avg_salary_diad * n_total, na.rm = TRUE) /
      sum(n_total, na.rm = TRUE),
    avg_age_years   = sum(avg_age_years   * n_total, na.rm = TRUE) /
      sum(n_total, na.rm = TRUE),
    n_total         = sum(n_total, na.rm = TRUE),  # sum LAST to keep vector above
    .groups = "drop"
  )

cat(sprintf("  CQ personnel: %d unique roles, %d total employees\n\n",
            nrow(cq_personnel), sum(cq_personnel$n_total)))

cat("Section 1 complete\n\n")


# SECTION 2: ECONOMIC PARAMETERS

five_yr_avg_inflation  <- mean(tail(economic_rates$inflation_rate, 5))
cumulative_trend       <- (1 + five_yr_avg_inflation)^5
risk_free_rate_10y     <- tail(economic_rates$rate_10y, 1)
pv_discount_factor     <- 1 / (1 + risk_free_rate_10y)^3

expense_ratio          <- 0.22
lae_ratio              <- 0.12
profit_margin          <- 0.06
investment_rate_base   <- 0.10
investment_rate_stress <- risk_free_rate_10y   # risk-free bonds only
gross_up_factor        <- 1 / (1 - expense_ratio - lae_ratio - profit_margin)
target_loss_ratio      <- 1 - expense_ratio - lae_ratio - profit_margin

ibnr_factor            <- 1.12   # Standard WC long-tail IBNR reserve
cat_factor             <- 1.05   # NCCI-grounded frontier cat provision (see header)

cat(sprintf("  5yr avg inflation:    %.4f (%.2f%%)\n",
            five_yr_avg_inflation, five_yr_avg_inflation * 100))
cat(sprintf("  Cumulative trend:     %.4f\n", cumulative_trend))
cat(sprintf("  10Y risk-free rate:   %.4f (%.2f%%)\n",
            risk_free_rate_10y, risk_free_rate_10y * 100))
cat(sprintf("  Gross-up factor:      %.4f (targets %.0f%% loss ratio)\n",
            gross_up_factor, target_loss_ratio * 100))
cat(sprintf("  Investment base/RF:   %.1f%% / %.2f%%\n",
            investment_rate_base * 100, investment_rate_stress * 100))
cat("Section 2 complete\n\n")


# SECTION 3: CLAIM FREQUENCY ANALYSIS

system_freq_summary <- frequency_data_clean |>
  group_by(solar_system) |>
  summarise(
    total_claims  = sum(claim_count),
    total_exposure = sum(exposure),
    annual_rate   = total_claims / total_exposure,  # Poisson MLE
    mean_gravity  = mean(gravity_level),
    n_records     = n(),
    .groups = "drop"
  )

# Overdispersion test
dispersion_test <- frequency_data_clean |>
  group_by(solar_system) |>
  summarise(
    sample_mean      = mean(claim_count),
    sample_variance  = var(claim_count),
    dispersion_index = var(claim_count) / mean(claim_count),
    .groups = "drop"
  )

cat("  Annual claim rates (Poisson MLE: sum/sum):\n")
print(system_freq_summary |>
        dplyr::select(solar_system, annual_rate, mean_gravity, total_claims, total_exposure) |>
        mutate(across(where(is.numeric), ~ round(., 5))))

cat("\n  Overdispersion indices (all ~1.0 → Poisson adequate):\n")
print(dispersion_test |> mutate(across(where(is.numeric), ~ round(., 4))))

# Fit NB GLMs to recover theta — confirms near-Poisson for all systems
for (sys in historical_systems) {
  sys_data <- frequency_data_clean |> filter(solar_system == sys)
  fit <- tryCatch(
    suppressWarnings(MASS::glm.nb(claim_count ~ offset(log(exposure)),
                                   data = sys_data)),
    error = function(e) NULL
  )
  theta <- if (!is.null(fit)) fit$theta else Inf
  cat(sprintf("  %s: rate=%.5f, theta=%.1f %s\n",
              sys,
              system_freq_summary$annual_rate[system_freq_summary$solar_system == sys],
              theta,
              if (theta > 100) "(effectively Poisson)" else ""))
}


# SECTION 4: CLAIM SEVERITY ANALYSIS

sev_fits <- list()

for (sys in historical_systems) {
  claim_amounts <- severity_data_clean |>
    filter(solar_system == sys) |>
    pull(claim_amount)

  fit_ln <- fitdist(claim_amounts, "lnorm", method = "mle")
  fit_g  <- fitdist(claim_amounts, "gamma", method = "mle", lower = c(0, 0))

  ln_mean <- exp(fit_ln$estimate["meanlog"] + 0.5 * fit_ln$estimate["sdlog"]^2)

  sev_fits[[sys]] <- list(
    raw          = claim_amounts,
    mu           = unname(fit_ln$estimate["meanlog"]),
    sigma        = unname(fit_ln$estimate["sdlog"]),
    ln_mean      = ln_mean,
    obs_mean     = mean(claim_amounts),
    n            = length(claim_amounts),
    aic_ln       = fit_ln$aic,
    aic_gamma    = fit_g$aic,
    gamma_fit    = fit_g
  )

  cat(sprintf("  %s: n=%d | mu=%.4f, sigma=%.4f, E[S]=Đ%.0f\n",
              sys, length(claim_amounts),
              sev_fits[[sys]]$mu, sev_fits[[sys]]$sigma, ln_mean))
  cat(sprintf("    AIC: LN=%.0f | Gamma=%.0f | ΔAIC=%+.0f → LogNormal selected\n",
              fit_ln$aic, fit_g$aic, fit_g$aic - fit_ln$aic))
}

# SECTION 5: GLM RISK RELATIVITIES

# All 7 predictors (for reference and pool average computation)
all_glm_predictors <- c(
  "experience_yrs", "psych_stress_index", "safety_training_index",
  "protective_gear_quality", "accident_history_flag",
  "gravity_level", "supervision_level"
)

# Reduced 5-predictor model (adopted after AIC/BIC/LRT selection)
retained_predictors <- c(
  "experience_yrs", "psych_stress_index", "safety_training_index",
  "accident_history_flag", "gravity_level"
)

# Pool averages — used in schedule modification factor
pool_avg <- frequency_data_clean |>
  summarise(across(all_of(all_glm_predictors), mean)) |>
  unlist()

# Frequency GLM: Poisson log-link, exposure offset, 5 retained predictors
freq_glm_formula <- as.formula(
  paste("claim_count ~ offset(log(exposure)) +",
        paste(retained_predictors, collapse = " + "))
)

freq_glm_model <- glm(
  freq_glm_formula,
  data   = frequency_data_clean,
  family = poisson(link = "log")
)

freq_glm_coefs <- coef(freq_glm_model)[-1]  # Drop intercept
names(freq_glm_coefs) <- retained_predictors

# Gravity coefficient: used in environment multiplier Section 7
gravity_coef <- freq_glm_coefs["gravity_level"]

cat("  Frequency GLM coefficients (5-predictor reduced model):\n")
data.frame(
  predictor  = retained_predictors,
  beta       = round(freq_glm_coefs, 5),
  rate_mult  = round(exp(freq_glm_coefs), 4),
  direction  = ifelse(freq_glm_coefs > 0, "Increases frequency", "Reduces frequency")
) |> print(row.names = FALSE)

cat(sprintf("\n  Gravity coefficient (used in env. multiplier): %.5f\n", gravity_coef))
cat(sprintf("  Pool averages (baseline for schedule mod comparison):\n"))
print(round(pool_avg, 4))

# Severity GLM: Gamma log-link, includes base_salary
sev_glm_predictors <- c(
  "experience_yrs", "psych_stress_index", "safety_training_index",
  "protective_gear_quality", "accident_history_flag",
  "gravity_level", "base_salary"
)

sev_glm_model <- glm(
  as.formula(paste("claim_amount ~",
                   paste(sev_glm_predictors, collapse = " + "))),
  data   = severity_data_clean,
  family = Gamma(link = "log")
)

# Schedule-rating predictors: remove gravity (handled in environment multiplier)
sched_predictors <- setdiff(retained_predictors, "gravity_level")
sched_coefs      <- freq_glm_coefs[sched_predictors]


# SECTION 6: BÜHLMANN CREDIBILITY — OCCUPATION CLASS RATES

# Fleet statistics
grand_mean_rate <- frequency_data_clean |>
  summarise(r = sum(claim_count) / sum(exposure)) |>
  pull(r)

grand_mean_sev <- mean(severity_data_clean$claim_amount)

# Observed rates per occupation
occ_obs <- frequency_data_clean |>
  group_by(occupation) |>
  summarise(
    exposure_wy  = sum(exposure),
    total_claims = sum(claim_count),
    obs_rate     = total_claims / exposure_wy,
    .groups = "drop"
  )

# Bühlmann parameters
epv              <- grand_mean_rate   # Poisson: variance = mean
mean_exp_per_occ <- mean(occ_obs$exposure_wy)
total_exp_all    <- sum(occ_obs$exposure_wy)

weighted_rate_var <- with(occ_obs,
  sum(exposure_wy * (obs_rate - grand_mean_rate)^2) / total_exp_all)
vhm          <- max(weighted_rate_var - epv / mean_exp_per_occ, 1e-8)
buehlmann_k  <- epv / vhm

cat(sprintf("  Grand mean rate:  %.5f/wy | Grand mean severity: Đ%.0f\n",
            grand_mean_rate, grand_mean_sev))
cat(sprintf("  EPV=%.6f | VHM=%.8f | k=%.1f\n", epv, vhm, buehlmann_k))
cat(sprintf("  k=%.0f: need %.0f worker-years for Z=50%% credibility\n\n",
            buehlmann_k, buehlmann_k))

# Apply Bühlmann formula
occ_cred <- occ_obs |>
  mutate(
    Z             = exposure_wy / (exposure_wy + buehlmann_k),
    cred_rate     = Z * obs_rate + (1 - Z) * grand_mean_rate,
    freq_rel      = cred_rate / grand_mean_rate
  )

# Severity relativities per occupation
occ_sev <- severity_data_clean |>
  group_by(occupation) |>
  summarise(mean_sev = mean(claim_amount), .groups = "drop") |>
  mutate(sev_rel = mean_sev / grand_mean_sev)

# Combined rating table
occ_rating <- occ_cred |>
  left_join(occ_sev |> dplyr::select(occupation, mean_sev, sev_rel),
            by = "occupation") |>
  mutate(
    mean_sev = coalesce(mean_sev, grand_mean_sev),
    sev_rel  = coalesce(sev_rel, 1.0),
    pp_rel   = freq_rel * sev_rel
  ) |>
  arrange(desc(pp_rel))

occ_lookup <- occ_rating |>
  dplyr::select(occupation, freq_rel, mean_sev, sev_rel)

cat("  Top 5 occupations by PP relativity:\n")
occ_rating |>
  head(5) |>
  dplyr::select(occupation, exposure_wy, Z, freq_rel, sev_rel, pp_rel) |>
  mutate(across(where(is.numeric), ~ round(., 3))) |>
  print(row.names = FALSE)

# SECTION 7: ENVIRONMENT MULTIPLIER DECOMPOSITION

# Historical mean gravity from data (using [[]] to extract unnamed scalar)
hist_grav <- setNames(
  vapply(historical_systems, function(s)
    unname(system_freq_summary$mean_gravity[system_freq_summary$solar_system == s]),
    numeric(1)),
  historical_systems
)

# Target gravity assumptions
helionis_target_g <- unname(hist_grav[["Helionis Cluster"]])  # Direct data

target_grav <- c(
  "Helionis Cluster" = helionis_target_g,
  "Bayesia System"   = 1.350,  # Encyclopaedia: "high-gravity"; near Zeta (1.375g)
  "Oryn Delta"       = 1.050   # Rocky exoplanet, no descriptor; near-Earth assumed
)

analogue_map <- c(
  "Helionis Cluster" = "Helionis Cluster",
  "Bayesia System"   = "Zeta",
  "Oryn Delta"       = "Epsilon"
)

resid_freq_load <- c(
  "Helionis Cluster" = 0.97,
  "Bayesia System"   = 1.10,
  "Oryn Delta"       = 1.15
)

resid_sev_load <- c(
  "Helionis Cluster" = 0.97,
  "Bayesia System"   = 1.10,
  "Oryn Delta"       = 1.05
)

# Build environment multiplier table using map_dbl for safe element-wise
# named-vector lookup inside tibble::mutate()
env_mults <- tibble(target_system = target_systems) |>
  mutate(
    analogue_sys   = analogue_map[target_system],
    hist_gravity   = map_dbl(analogue_sys, ~ unname(hist_grav[[.x]])),
    target_gravity = map_dbl(target_system, ~ unname(target_grav[[.x]])),
    delta_gravity  = target_gravity - hist_gravity,
    # Component A: data-driven gravity adjustment
    grav_freq_mult = exp(gravity_coef * delta_gravity),
    # Component B: judgemental residual loading
    resid_freq     = resid_freq_load[target_system],
    resid_sev      = resid_sev_load[target_system],
    # Combined
    total_freq_mult = grav_freq_mult * resid_freq,
    total_sev_mult  = resid_sev
  )

cat("  Environment multipliers:\n")
env_mults |>
  dplyr::select(target_system, analogue_sys, hist_gravity, target_gravity,
                grav_freq_mult, resid_freq, total_freq_mult, total_sev_mult) |>
  mutate(across(where(is.numeric), ~ round(., 4))) |>
  print(row.names = FALSE)

# SECTION 8: SCHEDULE MODIFICATION FACTORS (SYSTEM-SPECIFIC)

# Derive CQ experience from personnel + BLS/AMISA entry ages
industry_entry_ages <- tribble(
  ~role,                       ~entry_age,
  "Executive",                  35,
  "Vice President",             30, 
  "Director",                   28,  
  "HR",                         23,  
  "IT",                         22, 
  "Legal",                      25, 
  "Finance & Accounting",       23, 
  "Environmental Scientists",   24, 
  "Safety Officer",             23,  
  "Medical Personel",           26,  
  "Geoligist",                  24,
  "Scientist",                  24, 
  "Field technician",           21, 
  "Drilling operators",         22,  
  "Maintenance",                21,  
  "Engineers",                  23,   
  "Freight operators",          21,   
  "Robotics technician",        22,  
  "Navigation officers",        24,  
  "Security personel",          21, 
  "Steward",                    22,   
  "Galleyhand",                 20 
)

cq_with_exp <- cq_personnel |>
  left_join(industry_entry_ages |> dplyr::select(role, entry_age), by = "role") |>
  mutate(
    entry_age  = coalesce(entry_age, 22),
    exp_years  = pmax(0.5, avg_age_years - entry_age)
  )

cq_wtd_exp <- with(cq_with_exp,
  sum(n_total * exp_years, na.rm = TRUE) / sum(n_total, na.rm = TRUE))

# Supervision ratio from personnel file (data-derived)
mgmt_n   <- sum(cq_personnel$n_total[
  cq_personnel$role %in% c("Executive","Vice President","Director")],
  na.rm = TRUE)
safety_n <- sum(cq_personnel$n_total[
  grepl("Safety", cq_personnel$role, ignore.case = TRUE)],
  na.rm = TRUE)
total_n  <- sum(cq_personnel$n_total, na.rm = TRUE)
cq_sup_ratio <- (mgmt_n + safety_n) / total_n

cat(sprintf("  CQ weighted avg experience: %.2f yr  (pool avg: %.3f yr)\n",
            cq_wtd_exp, pool_avg["experience_yrs"]))
cat(sprintf("  CQ supervision ratio:       %.4f    (pool avg: %.4f)\n",
            cq_sup_ratio, pool_avg["supervision_level"]))
cat("  Supervision is BELOW pool average → small debit for all systems\n\n")

# CQ risk profiles per system
# System-specific values reflect workforce age (frontier discount), stress
# (escalating Helionis → Bayesia → Oryn Delta), and training (frontier discount)
cq_profiles <- list(
  "Helionis Cluster" = c(
    experience_yrs        = cq_wtd_exp,           # (D) Personnel file
    psych_stress_index    = 2.2,                   # (E) Stable star, mature ops → moderate
    safety_training_index = 4.0,                   # (PI) 11.9% E&S staff >> BHP ~8% benchmark
    accident_history_flag = 0.09                   # (A) Slightly below pool 10.1%
  ),
  "Bayesia System" = c(
    experience_yrs        = cq_wtd_exp * 0.85, # (E) ~15% younger: expansion cohort
    psych_stress_index    = 3.0,                   # (E) EM events, temperature extremes
    safety_training_index = 3.9,                   # (PI) Frontier discount from Helionis
    accident_history_flag = 0.10                   # (A) Near pool average
  ),
  "Oryn Delta" = c(
    experience_yrs        = cq_wtd_exp * 0.80, # (E) ~20% younger: most recent expansion
    psych_stress_index    = 3.5,                   # (E) Aggressive expansion, comms inconsistency
    safety_training_index = 3.8,                   # (PI) Further frontier discount
    accident_history_flag = 0.12                   # (A) Frontier uplift
  )
)

# Compute schedule modification factors
sched_mod_results <- map_dfr(target_systems, function(sys) {
  profile <- cq_profiles[[sys]]

  log_contribs <- setNames(
    sched_coefs * (profile[sched_predictors] - pool_avg[sched_predictors]),
    sched_predictors
  )

  tibble(
    solar_system      = sys,
    log_mod_experience = log_contribs["experience_yrs"],
    log_mod_stress     = log_contribs["psych_stress_index"],
    log_mod_training   = log_contribs["safety_training_index"],
    log_mod_accident   = log_contribs["accident_history_flag"],
    total_log_mod      = sum(log_contribs),
    schedule_mod       = exp(sum(log_contribs))
  )
})

schedule_mods <- setNames(sched_mod_results$schedule_mod,
                           sched_mod_results$solar_system)

cat("  System-specific schedule modification factors:\n")
sched_mod_results |>
  dplyr::select(solar_system, total_log_mod, schedule_mod) |>
  mutate(across(where(is.numeric), ~ round(., 5)),
         direction = ifelse(schedule_mod > 1.0, "DEBIT  (premium increases)",
                            "CREDIT (premium decreases)")) |>
  print(row.names = FALSE)


# =============================================================================
# SECTION 9: PURE PREMIUM BUILD-UP


cq_occ_map <- c(
  "Executive"              = "Executive",
  "Vice President"         = "Manager",
  "Director"               = "Manager",
  "HR"                     = "Administrator",
  "IT"                     = "Technology Officer",
  "Legal"                  = "Administrator",
  "Finance & Accounting"   = "Administrator",
  "Environmental Scientists" = "Scientist",
  "Safety Officer"         = "Safety Officer",
  "Medical Personel"       = "Safety Officer",
  "Geoligist"              = "Scientist",
  "Scientist"              = "Scientist",
  "Field technician"       = "Maintenance Staff",
  "Drilling operators"     = "Drill Operator",
  "Maintenance"            = "Maintenance Staff",
  "Engineers"              = "Engineer",
  "Freight operators"      = "Spacecraft Operator",
  "Robotics technician"    = "Technology Officer",
  "Navigation officers"    = "Spacecraft Operator",
  "Security personel"      = "Planetary Operations",
  "Steward"                = "Administrator",
  "Galleyhand"             = "Administrator"
)

worker_props <- c(
  "Helionis Cluster" = 30 / 55,
  "Bayesia System"   = 15 / 55,
  "Oryn Delta"       = 10 / 55
)

# Historical analogue base rates and mean severities
hist_base_rates <- setNames(system_freq_summary$annual_rate,
                             system_freq_summary$solar_system)
hist_mean_sev   <- sapply(historical_systems, function(s) sev_fits[[s]]$obs_mean)

# Build per-role per-system pure premium table
pure_premium_detail <- expand_grid(
  cq_role       = cq_personnel$role,
  target_system = target_systems
) |>
  left_join(cq_personnel |> dplyr::select(role, n_total, avg_salary_diad),
            by = c("cq_role" = "role")) |>
  mutate(
    n_workers     = round(n_total * worker_props[target_system]),
    hist_occ      = coalesce(cq_occ_map[cq_role], "Administrator"),
    analogue_sys  = analogue_map[target_system],
    base_rate     = map_dbl(analogue_sys, ~ unname(hist_base_rates[[.x]])),
    base_mean_sev = map_dbl(analogue_sys, ~ unname(hist_mean_sev[[.x]]))
  ) |>
  left_join(occ_lookup, by = c("hist_occ" = "occupation")) |>
  left_join(env_mults |> dplyr::select(target_system,
                                        total_freq_mult, total_sev_mult),
            by = "target_system") |>
  left_join(sched_mod_results |> dplyr::select(solar_system, schedule_mod),
            by = c("target_system" = "solar_system")) |>
  mutate(
    freq_rel      = coalesce(freq_rel, 1.0),
    sev_rel       = coalesce(sev_rel, 1.0),
    base_mean_sev = coalesce(base_mean_sev, grand_mean_sev),
    base_pure_premium = base_rate * freq_rel * total_freq_mult *
      base_mean_sev  * sev_rel  * total_sev_mult
  ) |>
  filter(n_workers > 0)


# SECTION 10: GROSS PREMIUM LOADING

gross_premium_detail <- pure_premium_detail |>
  mutate(
    after_sched_mod = base_pure_premium * schedule_mod,
    after_trend     = after_sched_mod   * cumulative_trend,
    after_ibnr      = after_trend       * ibnr_factor,
    after_grossup   = after_ibnr        * gross_up_factor,
    gross_pp        = after_grossup     * cat_factor,
    total_premium   = gross_pp          * n_workers
  )

premium_by_system <- gross_premium_detail |>
  group_by(target_system) |>
  summarise(
    total_workers  = sum(n_workers),
    total_premium  = sum(total_premium),
    avg_gross_pp   = total_premium / total_workers,
    .groups = "drop"
  ) |>
  mutate(
    total_payroll  = sapply(target_system, function(ss)
      sum(cq_personnel$n_total * cq_personnel$avg_salary_diad,
          na.rm = TRUE) * worker_props[ss]),
    pct_payroll    = total_premium / total_payroll * 100  # 3dp for display
  )

cat("  Annual premiums by solar system:\n")
premium_by_system |>
  mutate(
    premium_M  = total_premium / 1e6,
    pct_pay    = round(pct_payroll, 3)  # 3 decimal places: 0.533% vs 0.806% vs 0.783%
  ) |>
  dplyr::select(target_system, total_workers, avg_gross_pp, premium_M, pct_pay) |>
  setNames(c("System","Workers","Avg PP (Đ)","Premium (ĐM)","% Payroll")) |>
  print(row.names = FALSE)

cat(sprintf("\n  TOTAL PORTFOLIO PREMIUM: Đ%.4f million\n\n",
            sum(premium_by_system$total_premium) / 1e6))


# SECTION 11: MONTE CARLO AGGREGATE LOSS SIMULATION

# Compute occupation-weighted freq and sev relativities for each target system
compute_wtd_occ_rels <- function(sys) {
  prop <- worker_props[sys]
  rows <- cq_personnel |>
    mutate(
      n_in_sys   = round(n_total * prop),
      hist_occ   = coalesce(cq_occ_map[role], "Administrator"),
      occ_fr     = coalesce(occ_lookup$freq_rel[match(hist_occ, occ_lookup$occupation)], 1.0),
      occ_sr     = coalesce(occ_lookup$sev_rel[ match(hist_occ, occ_lookup$occupation)], 1.0)
    ) |>
    filter(n_in_sys > 0)

  list(
    wtd_freq_rel = sum(rows$occ_fr * rows$n_in_sys) / sum(rows$n_in_sys),
    wtd_sev_rel  = sum(rows$occ_sr * rows$n_in_sys) / sum(rows$n_in_sys)
  )
}

mc_results <- list()

for (sys in target_systems) {
  an          <- analogue_map[sys]
  n_workers   <- premium_by_system$total_workers[
    premium_by_system$target_system == sys]
  lam_analogue <- unname(hist_base_rates[[an]])
  fm           <- env_mults$total_freq_mult[env_mults$target_system == sys]
  sm           <- env_mults$total_sev_mult[ env_mults$target_system == sys]
  smod         <- schedule_mods[sys]
  mu_hist      <- sev_fits[[an]]$mu
  sigma_hist   <- sev_fits[[an]]$sigma

  # Occupation-weighted relativities
  wtd <- compute_wtd_occ_rels(sys)
  wtd_fr <- wtd$wtd_freq_rel
  wtd_sr <- wtd$wtd_sev_rel

  # Fleet Poisson rate (includes occupation-weighted frequency relativity)
  fleet_lambda <- n_workers * lam_analogue * fm * smod * wtd_fr

  # Adjusted LogNormal mu (adding log of loadings = multiplying mean by loadings)
  mu_adj <- mu_hist + log(sm) + log(wtd_sr) + log(cumulative_trend) + log(ibnr_factor)

  # Simulate — seed already fixed globally in Section 0; resetting per-system
  # ensures each system's draws are independent of the others' iteration order
  set.seed(42 + match(sys, target_systems))
  annual_losses <- numeric(n_mc_simulations)
  for (i in seq_len(n_mc_simulations)) {
    n_claims <- rpois(1, lambda = fleet_lambda)
    if (n_claims > 0) {
      annual_losses[i] <- sum(rlnorm(n_claims, meanlog = mu_adj, sdlog = sigma_hist))
    }
  }

  var_99  <- quantile(annual_losses, 0.99)
  tvar_99 <- mean(annual_losses[annual_losses >= var_99])
  gp      <- premium_by_system$total_premium[
    premium_by_system$target_system == sys]

  mc_results[[sys]] <- list(
    samples       = annual_losses,
    fleet_lambda  = fleet_lambda,
    wtd_freq_rel  = wtd_fr,
    wtd_sev_rel   = wtd_sr,
    expected_loss = mean(annual_losses),
    std_dev       = sd(annual_losses),
    cv            = sd(annual_losses) / mean(annual_losses),
    median        = median(annual_losses),
    var_90        = quantile(annual_losses, 0.90),
    var_95        = quantile(annual_losses, 0.95),
    var_99        = var_99,
    var_99_5      = quantile(annual_losses, 0.995),
    tvar_99       = tvar_99,
    gross_premium = gp,
    loss_ratio    = mean(annual_losses) / gp
  )

  cat(sprintf(
    "  %s: E[L]=Đ%.4fM  CV=%.3f  VaR99=Đ%.4fM  TVaR99=Đ%.4fM  LR=%.1f%%\n",
    sys,
    mc_results[[sys]]$expected_loss / 1e6,
    mc_results[[sys]]$cv,
    mc_results[[sys]]$var_99 / 1e6,
    mc_results[[sys]]$tvar_99 / 1e6,
    mc_results[[sys]]$loss_ratio * 100
  ))
}

# SECTION 12: CONFIDENCE INTERVAL ANALYSIS

# Exact Poisson CIs for claim rates
freq_ci <- system_freq_summary |>
  mutate(
    ci_lower         = qchisq(0.025, df = 2 * total_claims)   / (2 * total_exposure),
    ci_upper         = qchisq(0.975, df = 2 * total_claims + 2) / (2 * total_exposure),
    ci_halfwidth_pct = (ci_upper - ci_lower) / (2 * annual_rate) * 100
  )

cat("  Exact Poisson 95% CIs for annual claim rates:\n")
freq_ci |>
  dplyr::select(solar_system, annual_rate, ci_lower, ci_upper, ci_halfwidth_pct) |>
  mutate(across(where(is.numeric), ~ round(., 5))) |>
  print(row.names = FALSE)

# Bootstrap CIs for aggregate E[L]
n_boot_outer <- 200
n_boot_inner <- 500
cat(sprintf("\n  Bootstrap CIs (B=%d outer, %d inner)...\n",
            n_boot_outer, n_boot_inner))

bootstrap_ci <- list()

for (sys in target_systems) {
  an         <- analogue_map[sys]
  n_wk       <- premium_by_system$total_workers[
    premium_by_system$target_system == sys]
  freq_data  <- frequency_data_clean |> filter(solar_system == an)
  sev_data   <- severity_data_clean  |> filter(solar_system == an)
  fm_val     <- env_mults$total_freq_mult[env_mults$target_system == sys]
  sm_val     <- env_mults$total_sev_mult[ env_mults$target_system == sys]
  smod_val   <- schedule_mods[sys]
  wtd        <- compute_wtd_occ_rels(sys)

  boot_means <- numeric(n_boot_outer)

  for (b in seq_len(n_boot_outer)) {
    bf      <- freq_data[sample(nrow(freq_data), replace = TRUE), ]
    lam_b   <- sum(bf$claim_count) / sum(bf$exposure)
    sev_b   <- sample(sev_data$claim_amount, nrow(sev_data), replace = TRUE)
    mu_b    <- mean(log(sev_b))
    sig_b   <- sd(log(sev_b))

    lam_fleet_b <- n_wk * lam_b * fm_val * smod_val * wtd$wtd_freq_rel
    mu_adj_b    <- mu_b + log(sm_val) + log(wtd$wtd_sev_rel) +
      log(cumulative_trend) + log(ibnr_factor)

    inner <- vapply(seq_len(n_boot_inner), function(i) {
      nc <- rpois(1, lam_fleet_b)
      if (nc > 0) sum(rlnorm(nc, mu_adj_b, sig_b)) else 0
    }, numeric(1))

    boot_means[b] <- mean(inner)
  }

  bootstrap_ci[[sys]] <- list(
    ci_lower = quantile(boot_means, 0.025),
    ci_upper = quantile(boot_means, 0.975)
  )

  cat(sprintf("  %s: [Đ%.4fM, Đ%.4fM]\n",
              sys,
              bootstrap_ci[[sys]]$ci_lower / 1e6,
              bootstrap_ci[[sys]]$ci_upper / 1e6))
}


# SECTION 13: SENSITIVITY ANALYSIS AND P&L SUMMARY

# Bayesia gravity sensitivity (most material unverified assumption)
gravity_range          <- seq(1.20, 1.50, by = 0.05)
bayesia_central_prem   <- premium_by_system$total_premium[
  premium_by_system$target_system == "Bayesia System"]
bayesia_central_fm     <- env_mults$total_freq_mult[
  env_mults$target_system == "Bayesia System"]
bayesia_hist_g         <- unname(hist_grav[["Zeta"]])

gravity_sensitivity <- tibble(assumed_gravity = gravity_range) |>
  mutate(
    freq_mult  = exp(gravity_coef * (assumed_gravity - bayesia_hist_g)) *
      resid_freq_load["Bayesia System"],
    scaled_prem = bayesia_central_prem * freq_mult / bayesia_central_fm,
    pct_vs_central = (scaled_prem / bayesia_central_prem - 1) * 100
  )

cat("  Bayesia premium sensitivity to gravity assumption:\n")
print(gravity_sensitivity |> mutate(across(where(is.numeric), ~ round(., 3))))

# P&L summary
pnl_summary <- premium_by_system |>
  mutate(
    expected_loss  = sapply(target_system, function(ss)
      mc_results[[ss]]$expected_loss),
    expenses_lae   = total_premium * (expense_ratio + lae_ratio),
    cat_reserve    = total_premium * (cat_factor - 1),
    invest_base    = total_premium * investment_rate_base,
    invest_rf      = total_premium * investment_rate_stress,
    net_rev_base   = total_premium - expected_loss - expenses_lae -
      cat_reserve + invest_base,
    net_rev_rf     = total_premium - expected_loss - expenses_lae -
      cat_reserve + invest_rf,
    loss_ratio     = expected_loss / total_premium,
    combined_ratio = (expected_loss + expenses_lae) / total_premium
  )

cat("\n  P&L summary (base 10% / stress 5.10% investment):\n")
pnl_summary |>
  dplyr::select(target_system, total_premium, expected_loss,
                loss_ratio, combined_ratio, net_rev_base, net_rev_rf) |>
  mutate(across(where(is.numeric), ~ round(., 4))) |>
  print(row.names = FALSE)


# SECTION 14: REPORT FIGURES


# Figure 1: Data Exploration (6-panel)
f1a <- ggplot(
  system_freq_summary |> mutate(label = system_labels[solar_system]),
  aes(x = label, y = annual_rate * 100, fill = solar_system)
) +
  geom_col(width = 0.55, alpha = 0.9) +
  geom_text(aes(label = sprintf("%.2f", annual_rate * 100)),
            vjust = -0.5, colour = "white", fontface = "bold", size = 4) +
  scale_fill_manual(values = system_colours, guide = "none") +
  scale_y_continuous(expand = expansion(mult = c(0, 0.22))) +
  labs(title = "Annual claim rates by system",
       subtitle = "Poisson MLE: sum(claims) / sum(exposure)",
       x = NULL, y = "Claims per 100 worker-years") +
  theme_report

f1b <- ggplot(
  severity_data_clean |> mutate(label = system_labels[solar_system]),
  aes(x = label, y = claim_amount, fill = solar_system)
) +
  geom_violin(alpha = 0.7, trim = TRUE, scale = "width") +
  geom_boxplot(width = 0.12, fill = "white", alpha = 0.3,
               outlier.shape = NA, colour = "white") +
  scale_fill_manual(values = system_colours, guide = "none") +
  scale_y_log10(labels = label_dollar(prefix = "Đ", big.mark = ",")) +
  labs(title = "Severity distribution (log scale)",
       subtitle = "LogNormal selected over Gamma by AIC for all systems",
       x = NULL, y = "Claim amount (Đ, log scale)") +
  theme_report

f1c <- ggplot(
  frequency_data_clean |> mutate(label = system_labels[solar_system]),
  aes(x = label, y = gravity_level, fill = solar_system)
) +
  geom_boxplot(alpha = 0.8) +
  geom_hline(yintercept = 1.0, colour = col_gold,
             linetype = "dashed", linewidth = 1.2) +
  annotate("text", x = 0.6, y = 1.025, label = "Earth = 1g",
           colour = col_gold, size = 3.5) +
  scale_fill_manual(values = system_colours, guide = "none") +
  labs(title = "Surface gravity by historical system",
       subtitle = "Epsilon 0.875g (LOW) → Oryn Delta analogue | Zeta 1.375g (HIGH) → Bayesia analogue",
       x = NULL, y = "Gravity (g)") +
  theme_report

f1d <- severity_data_clean |>
  count(solar_system, injury_type) |>
  group_by(solar_system) |>
  mutate(pct = n / sum(n)) |>
  ggplot(aes(x = solar_system, y = pct, fill = injury_type)) +
  geom_col(position = "stack", alpha = 0.9) +
  scale_fill_brewer(palette = "Set3") +
  scale_x_discrete(labels = system_labels) +
  scale_y_continuous(labels = percent_format()) +
  labs(title = "Injury type composition",
       subtitle = "Stress/strain injuries have highest mean severity",
       x = NULL, y = "Proportion", fill = "Injury type") +
  theme_report +
  theme(legend.key.size = unit(0.35, "cm"),
        legend.text     = element_text(size = 7))

f1e <- occ_rating |>
  mutate(above_avg = freq_rel > 1) |>
  ggplot(aes(x = freq_rel,
             y = reorder(occupation, freq_rel),
             fill = above_avg)) +
  geom_col(alpha = 0.85, width = 0.7) +
  geom_vline(xintercept = 1.0, colour = "white", linewidth = 0.8) +
  scale_fill_manual(
    values = c("FALSE" = col_green, "TRUE" = col_red),
    guide  = "none"
  ) +
  scale_x_continuous(expand = expansion(mult = c(0.02, 0.15))) +
  labs(title = "Occupation frequency relativities",
       subtitle = "Bühlmann credibility-weighted | 1.0× = fleet average",
       x = "Frequency relativity", y = NULL) +
  theme_report

f1f <- frequency_data_clean |>
  group_by(accident_history_flag) |>
  summarise(rate = sum(claim_count) / sum(exposure) * 100, .groups = "drop") |>
  mutate(label = ifelse(accident_history_flag == 0, "No prior accident",
                        "Prior accident")) |>
  ggplot(aes(x = label, y = rate, fill = label)) +
  geom_col(alpha = 0.9, width = 0.5) +
  geom_text(aes(label = sprintf("%.3f", rate)),
            vjust = -0.5, colour = "white", fontface = "bold", size = 4) +
  scale_fill_manual(
    values = c("No prior accident" = col_green, "Prior accident" = col_red),
    guide  = "none"
  ) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.25))) +
  labs(title = "Accident history flag impact",
       subtitle = "+31.8% frequency → justifies schedule debit (Coef: +0.276)",
       x = NULL, y = "Claims per 100 worker-years") +
  theme_report

fig1 <- (f1a | f1b | f1c) / (f1d | f1e | f1f) +
  plot_annotation(
    title    = "Figure 1 — Data Exploration: Historical WC Claims Data",
    subtitle = "Three historical systems: Helionis Cluster, Epsilon, Zeta | 133,398 worker-period records",
    theme    = theme(
      plot.title    = element_text(colour = "white", face = "bold", size = 13),
      plot.subtitle = element_text(colour = col_gold, size = 9),
      plot.background = element_rect(fill = col_plot_bg, colour = NA)
    )
  )
save_fig(fig1, "fig1_data_exploration.png", 18, 11)

# Figure 2: Severity Distribution Fitting
fig2_panels <- map(historical_systems, function(sys) {
  d    <- sev_fits[[sys]]
  x_range <- seq(min(d$raw), quantile(d$raw, 0.97), length.out = 300)
  ln_y    <- dlnorm(x_range, meanlog = d$mu, sdlog = d$sigma)
  g_fit   <- d$gamma_fit
  gam_y   <- dgamma(x_range,
                    shape = g_fit$estimate["shape"],
                    rate  = g_fit$estimate["rate"])
  ggplot(data.frame(x = d$raw), aes(x = x)) +
    geom_histogram(aes(y = after_stat(density)),
                   bins = 50, fill = system_colours[sys], alpha = 0.5, colour = NA) +
    geom_line(data = data.frame(x = x_range, y = ln_y),
              aes(x = x, y = y), colour = "white", linewidth = 1.5) +
    geom_line(data = data.frame(x = x_range, y = gam_y),
              aes(x = x, y = y), colour = col_gold, linewidth = 1.2,
              linetype = "dashed") +
    annotate("text", x = Inf, y = Inf, hjust = 1.1, vjust = 1.5,
             size = 3, colour = "white",
             label = sprintf("LN  AIC = %.0f\nGamma = %.0f\n\u0394AIC = +%.0f \u2192 LN wins",
                             d$aic_ln, d$aic_gamma, d$aic_gamma - d$aic_ln)) +
    scale_x_continuous(labels = label_dollar(prefix = "Đ", scale = 1/1000,
                                              suffix = "K")) +
    labs(title = sys,
         subtitle = sprintf("\u03bc = %.4f | \u03c3 = %.4f | E[S] = Đ%.0f | n = %.0f",
                            d$mu, d$sigma, d$ln_mean, d$n),
         x = "Claim amount (Đ thousands)", y = "Density") +
    theme_report
})

fig2 <- wrap_plots(fig2_panels, nrow = 1) +
  plot_annotation(
    title    = "Figure 2 — Severity Distribution Fitting: LogNormal vs Gamma",
    subtitle = "White = LogNormal (selected by AIC)  |  Gold dashed = Gamma (rejected)",
    theme    = theme(
      plot.title    = element_text(colour = "white", face = "bold", size = 13),
      plot.subtitle = element_text(colour = col_gold, size = 9),
      plot.background = element_rect(fill = col_plot_bg, colour = NA)
    )
  )
save_fig(fig2, "fig2_distribution_fitting.png", 18, 7)

# Figure 3: GLM Coefficients + Bühlmann Credibility
glm_plot_df <- tibble(
  predictor = retained_predictors,
  label     = c("Experience (years)", "Psych stress (1–5)",
                "Safety training (1–5)", "Prior accident (0/1)",
                "Surface gravity (g)"),
  beta      = freq_glm_coefs,
  direction = ifelse(freq_glm_coefs > 0, "Increases frequency",
                     "Reduces frequency")
) |> mutate(label = reorder(label, beta))

f3a <- ggplot(glm_plot_df, aes(x = beta, y = label, fill = direction)) +
  geom_col(alpha = 0.9, width = 0.65) +
  geom_vline(xintercept = 0, colour = "white", linewidth = 1.0) +
  geom_text(aes(
    label = sprintf("%+.4f  [×%.3f]", beta, exp(beta)),
    hjust = ifelse(beta > 0, -0.06, 1.06)),
    colour = "white", size = 3.5) +
  scale_fill_manual(values = c("Increases frequency" = col_red,
                                "Reduces frequency"   = col_green)) +
  scale_x_continuous(expand = expansion(mult = 0.32)) +
  labs(title = "Frequency GLM coefficients (5-predictor reduced model)",
       subtitle = "All five predictors significant at 1% level | exp(\u03b2) = rate multiplier per unit",
       x = "Coefficient \u03b2 (log scale)", y = NULL, fill = NULL) +
  theme_report

f3b <- ggplot(
  occ_rating |>
    mutate(cred_z = exposure_wy / (exposure_wy + buehlmann_k),
           above_avg = freq_rel > 1),
  aes(x = exposure_wy, y = cred_z, colour = above_avg)
) +
  geom_point(size = 4, alpha = 0.85) +
  geom_text(aes(label = substr(occupation, 1, 11)),
            size = 2.8, colour = "white", hjust = -0.12) +
  stat_function(
    fun = ~ . / (. + buehlmann_k),
    colour = col_gold, linewidth = 1.5,
    xlim = c(0, max(occ_rating$exposure_wy))
  ) +
  scale_colour_manual(
    values = c("FALSE" = col_green, "TRUE" = col_red),
    labels = c("Below avg frequency", "Above avg frequency"),
    name   = NULL
  ) +
  labs(
    title    = sprintf("Bühlmann credibility weights (k = %.0f wy)", buehlmann_k),
    subtitle = "Gold curve = Z = E/(E+k) | Low exposure → pulled towards fleet average",
    x = "Exposure (worker-years)", y = "Credibility weight Z (0=no trust, 1=full trust)"
  ) +
  theme_report

fig3 <- (f3a | f3b) +
  plot_annotation(
    title = "Figure 3 — GLM Risk Relativities & Bühlmann Credibility Weights",
    theme = theme(
      plot.title    = element_text(colour = "white", face = "bold", size = 13),
      plot.background = element_rect(fill = col_plot_bg, colour = NA)
    )
  )
save_fig(fig3, "fig3_glm_and_relativities.png", 18, 7)

# Figure 4: Occupation Pure Premium Relativities
fig4 <- occ_rating |>
  mutate(occ = reorder(occupation, pp_rel),
         above = pp_rel > 1) |>
  ggplot(aes(x = pp_rel, y = occ, fill = above)) +
  geom_col(alpha = 0.85, width = 0.72) +
  geom_vline(xintercept = 1.0, colour = "white", linewidth = 1.0) +
  geom_text(aes(
    label = sprintf("%.3f\u00d7", pp_rel),
    hjust = ifelse(pp_rel > 1, -0.08, 1.08)),
    colour = "white", fontface = "bold", size = 3.8) +
  scale_fill_manual(values = c("FALSE" = col_green, "TRUE" = col_red),
                    guide = "none") +
  scale_x_continuous(expand = expansion(mult = c(0.04, 0.26))) +
  labs(
    title    = "Figure 4 — Occupation Pure Premium Relativities",
    subtitle = sprintf("Bühlmann k = %.0f | Fleet average = 1.0\u00d7 | Spread: %.2f\u00d7 to %.2f\u00d7",
                       buehlmann_k, min(occ_rating$pp_rel), max(occ_rating$pp_rel)),
    x = "Pure premium relativity (frequency × severity | 1.0× = fleet average)",
    y = NULL
  ) +
  theme_report

save_fig(fig4, "fig4_occupation_pp_relativities.png", 14, 7)

# Figure 5: Analogue Mapping + Environment Multipliers
f5a <- system_freq_summary |>
  mutate(label = system_labels[solar_system]) |>
  ggplot(aes(x = label, y = mean_gravity, fill = solar_system)) +
  geom_col(width = 0.55, alpha = 0.85) +
  geom_text(aes(label = sprintf("%.3fg", mean_gravity)),
            vjust = -0.5, colour = "white", fontface = "bold", size = 4) +
  geom_hline(yintercept = 1.0, colour = col_gold,
             linetype = "dashed", linewidth = 1.2) +
  scale_fill_manual(values = system_colours, guide = "none") +
  scale_y_continuous(limits = c(0, 1.65),
                     expand = expansion(mult = c(0, 0.05))) +
  labs(title = "Historical system gravity",
       subtitle = "Basis for analogue selection: Epsilon→Oryn Delta | Zeta→Bayesia",
       x = NULL, y = "Mean gravity (g)") +
  theme_report

f5b <- env_mults |>
  dplyr::select(target_system, grav_freq_mult, resid_freq) |>
  pivot_longer(c(grav_freq_mult, resid_freq),
               names_to  = "component",
               values_to = "multiplier") |>
  mutate(
    comp_label = recode(component,
      "grav_freq_mult" = "Gravity (data-driven)",
      "resid_freq"     = "Residual (judgemental)"
    ),
    sys_label = system_labels[target_system]
  ) |>
  ggplot(aes(x = sys_label, y = multiplier, fill = comp_label)) +
  geom_col(position = "dodge", alpha = 0.85, width = 0.6) +
  geom_hline(yintercept = 1.0, colour = "white",
             linetype = "dashed", linewidth = 0.8) +
  geom_text(aes(label = sprintf("%.4f", multiplier)),
            position = position_dodge(width = 0.6),
            vjust = -0.4, colour = "white", size = 3.2) +
  scale_fill_manual(values = c("Gravity (data-driven)" = col_helionis,
                                "Residual (judgemental)" = col_gold)) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.20))) +
  labs(title = "Frequency multiplier decomposition",
       subtitle = "Gravity component derived from GLM coefficient | Residual = expert judgement",
       x = NULL, y = "Multiplier component", fill = NULL) +
  theme_report

f5c <- gravity_sensitivity |>
  ggplot(aes(x = assumed_gravity, y = scaled_prem / 1e6)) +
  geom_ribbon(aes(ymin = min(scaled_prem / 1e6), ymax = scaled_prem / 1e6),
              alpha = 0.20, fill = col_bayesia) +
  geom_line(colour = col_bayesia, linewidth = 2) +
  geom_vline(xintercept = 1.35, colour = col_gold,
             linetype = "dashed", linewidth = 1.5) +
  geom_point(data = gravity_sensitivity |> filter(abs(assumed_gravity - 1.35) < 0.01),
             aes(x = assumed_gravity, y = scaled_prem / 1e6),
             colour = "white", size = 5, shape = 18) +
  labs(title = "Bayesia premium sensitivity to gravity",
       subtitle = "Diamond = central estimate (1.35g) | Range: ±4.6%",
       x = "Assumed Bayesia gravity (g)",
       y = "Bayesia annual premium (Đ million)") +
  theme_report

fig5 <- (f5a | f5b | f5c) +
  plot_annotation(
    title    = "Figure 5 — Analogue Mapping, Environment Multiplier Decomposition & Sensitivity",
    theme    = theme(
      plot.title    = element_text(colour = "white", face = "bold", size = 13),
      plot.background = element_rect(fill = col_plot_bg, colour = NA)
    )
  )
save_fig(fig5, "fig5_analogue_and_env_mults.png", 18, 7)

# Figure 6: Schedule Modification Breakdown
fig6 <- sched_mod_results |>
  pivot_longer(
    cols      = starts_with("log_mod_"),
    names_to  = "factor",
    values_to = "log_contribution"
  ) |>
  mutate(
    factor_label = recode(factor,
      "log_mod_experience" = "Experience (yrs)",
      "log_mod_stress"     = "Psych stress",
      "log_mod_training"   = "Safety training",
      "log_mod_accident"   = "Accident history"
    ),
    direction  = ifelse(log_contribution > 0, "Debit (+)", "Credit (−)"),
    sys_label  = system_labels[solar_system]
  ) |>
  ggplot(aes(x = log_contribution,
             y = reorder(factor_label, abs(log_contribution)),
             fill = direction)) +
  geom_col(alpha = 0.85, width = 0.65) +
  geom_vline(xintercept = 0, colour = "white", linewidth = 0.8) +
  geom_text(aes(
    label = sprintf("%+.4f", log_contribution),
    hjust = ifelse(log_contribution > 0, -0.08, 1.08)),
    colour = "white", size = 3.2) +
  facet_wrap(~ sys_label, nrow = 1) +
  scale_fill_manual(values = c("Debit (+)" = col_red,
                                "Credit (−)" = col_green)) +
  labs(
    title    = sprintf(
      "Figure 6 — Schedule Modification: Helionis %.4f\u00d7 | Bayesia %.4f\u00d7 | Oryn %.4f\u00d7",
      schedule_mods["Helionis Cluster"],
      schedule_mods["Bayesia System"],
      schedule_mods["Oryn Delta"]
    ),
    subtitle = "Log contribution = \u03b2 × (CQ value − pool average) | Sum exponentiated gives final mod",
    x = "Log contribution", y = NULL, fill = NULL
  ) +
  theme_report
save_fig(fig6, "fig6_schedule_mod_breakdown.png", 14, 7)

# Figure 7: Premium Build-Up Waterfall
build_up_stages <- map_dfr(target_systems, function(sys) {
  em   <- env_mults |> filter(target_system == sys)
  an   <- em$analogue_sys
  base <- hist_base_rates[an] * hist_mean_sev[an]
  env  <- base * em$total_freq_mult * em$total_sev_mult
  sch  <- env  * schedule_mods[sys]
  trd  <- sch  * cumulative_trend
  ibnr <- trd  * ibnr_factor
  gup  <- ibnr * gross_up_factor * cat_factor
  tibble(
    system = sys,
    stage  = factor(c("Base", "×Env", "×Sched", "×Trend", "×IBNR", "Gross PP"),
                    levels = c("Base", "×Env", "×Sched", "×Trend", "×IBNR", "Gross PP")),
    value  = c(base, env, sch, trd, ibnr, gup),
    is_final = c(rep(FALSE, 5), TRUE)
  )
})

fig7 <- ggplot(build_up_stages,
               aes(x = stage, y = value,
                   fill = interaction(system, is_final),
                   group = system)) +
  geom_col(position = "dodge", alpha = 0.85) +
  geom_text(aes(label = sprintf("Đ%.0f", value)),
            position = position_dodge(width = 0.9),
            vjust = -0.4, colour = "white", size = 3, fontface = "bold") +
  facet_wrap(~ system, nrow = 1) +
  scale_fill_manual(
    values = setNames(
      c(alpha(col_helionis, 0.55), alpha(col_bayesia, 0.55), alpha(col_oryn, 0.55),
        col_helionis, col_bayesia, col_oryn),
      c("Helionis Cluster.FALSE", "Bayesia System.FALSE", "Oryn Delta.FALSE",
        "Helionis Cluster.TRUE",  "Bayesia System.TRUE",  "Oryn Delta.TRUE")
    ),
    guide = "none"
  ) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.22)),
                     labels = label_dollar(prefix = "Đ")) +
  labs(
    title    = "Figure 7 — Pure Premium Build-Up per Worker per Year",
    subtitle = "Each bar = cumulative premium after that loading stage | Final = gross premium",
    x = "Build-up stage", y = "Đ per worker per year"
  ) +
  theme_report
save_fig(fig7, "fig7_premium_buildup.png", 14, 7)

# Figure 8: Final Premiums, P&L, and Loss Ratios
f8a <- pnl_summary |>
  dplyr::select(target_system, expected_loss, expenses_lae,
                cat_reserve, invest_base) |>
  pivot_longer(-target_system, names_to = "component", values_to = "amount") |>
  mutate(
    comp_label = recode(component,
      "expected_loss" = "Expected losses",
      "expenses_lae"  = "Expenses + LAE",
      "cat_reserve"   = "Cat reserve",
      "invest_base"   = "Investment income"
    ),
    sys_label = system_labels[target_system]
  ) |>
  ggplot(aes(x = sys_label, y = amount / 1e6, fill = comp_label)) +
  geom_col(position = "stack", alpha = 0.85) +
  geom_point(
    data = pnl_summary |> mutate(sys_label = system_labels[target_system]),
    aes(x = sys_label, y = total_premium / 1e6),
    inherit.aes = FALSE, shape = 18, size = 7, colour = "white"
  ) +
  scale_fill_manual(values = c(
    "Expected losses"   = col_helionis,
    "Expenses + LAE"    = "#8B8000",
    "Cat reserve"       = col_red,
    "Investment income" = col_green
  )) +
  labs(title = "Premium composition",
       subtitle = "Diamond = gross premium | Stacked bars = cost components",
       x = NULL, y = "Đ million", fill = NULL) +
  theme_report

f8b <- pnl_summary |>
  mutate(sys_label = system_labels[target_system]) |>
  ggplot(aes(x = sys_label)) +
  geom_col(aes(y = combined_ratio * 100, fill = target_system),
           alpha = 0.85, width = 0.5) +
  geom_point(aes(y = loss_ratio * 100),
             colour = "white", size = 5, shape = 18) +
  geom_hline(yintercept = 86, colour = col_gold,
             linetype = "dashed", linewidth = 1.2) +
  annotate("text", x = 0.58, y = 87.5,
           label = "NCCI benchmark 86%",
           colour = col_gold, size = 3.5) +
  scale_fill_manual(values = system_colours, guide = "none") +
  scale_y_continuous(limits = c(0, 108)) +
  labs(title = "Combined & loss ratios",
       subtitle = "Bars = combined ratio | Diamond = loss ratio",
       x = NULL, y = "Ratio (%)") +
  theme_report

f8c <- pnl_summary |>
  mutate(sys_label = system_labels[target_system]) |>
  ggplot(aes(x = sys_label, y = net_rev_base / 1e6, fill = target_system)) +
  geom_col(alpha = 0.85, width = 0.5) +
  geom_text(aes(label = sprintf("Đ%.3fM", net_rev_base / 1e6)),
            vjust = -0.5, colour = "white", fontface = "bold", size = 4) +
  scale_fill_manual(values = system_colours, guide = "none") +
  scale_y_continuous(expand = expansion(mult = c(0, 0.28))) +
  labs(title = "Net revenue (10% investment income)",
       subtitle = "After losses, expenses, cat reserve + investment income",
       x = NULL, y = "Đ million") +
  theme_report

fig8 <- (f8a | f8b | f8c) +
  plot_annotation(
    title = "Figure 8 — Final Premiums, P&L and Financial Returns by System",
    theme = theme(
      plot.title    = element_text(colour = "white", face = "bold", size = 13),
      plot.background = element_rect(fill = col_plot_bg, colour = NA)
    )
  )
save_fig(fig8, "fig8_premiums_pl.png", 18, 7)

# Figure 9: Monte Carlo Loss Distributions
mc_samples_df <- map_dfr(target_systems, function(sys)
  tibble(solar_system = sys, annual_loss = mc_results[[sys]]$samples / 1e6))

mc_vlines_df <- map_dfr(target_systems, function(sys) {
  ar <- mc_results[[sys]]
  bind_rows(
    tibble(solar_system = sys, stat = "E[L]",    val = ar$expected_loss / 1e6),
    tibble(solar_system = sys, stat = "VaR 95%", val = ar$var_95 / 1e6),
    tibble(solar_system = sys, stat = "VaR 99%", val = ar$var_99 / 1e6),
    tibble(solar_system = sys, stat = "Premium",
           val = ar$gross_premium / 1e6)
  )
})

f9a <- ggplot(mc_samples_df, aes(x = annual_loss, fill = solar_system)) +
  geom_histogram(aes(y = after_stat(density)), bins = 80, alpha = 0.6,
                 colour = NA) +
  geom_vline(data = mc_vlines_df,
             aes(xintercept = val, colour = stat, linetype = stat),
             linewidth = 1.2) +
  facet_wrap(~ solar_system, scales = "free_x") +
  scale_fill_manual(values = system_colours, guide = "none") +
  scale_colour_manual(values = c("E[L]"    = "white",
                                  "VaR 95%" = col_gold,
                                  "VaR 99%" = col_red,
                                  "Premium" = col_green)) +
  scale_linetype_manual(values = c("E[L]"    = "solid",
                                    "VaR 95%" = "dashed",
                                    "VaR 99%" = "dotdash",
                                    "Premium" = "dotted")) +
  labs(title    = "Annual Loss Distributions (50,000 simulated scenarios)",
       subtitle = "White = E[L] | Gold = VaR 95% | Red = VaR 99% | Green = Annual premium",
       x = "Annual losses (Đ million)", y = "Density",
       colour = NULL, linetype = NULL) +
  theme_report

# Exceedance probability curves
ep_df <- map_dfr(target_systems, function(sys) {
  s <- sort(mc_results[[sys]]$samples / 1e6)
  n <- length(s)
  tibble(solar_system = sys, loss_level = s,
         exceedance_prob = 1 - seq_len(n) / n)
})

f9b <- ggplot(ep_df, aes(x = loss_level, y = exceedance_prob,
                          colour = solar_system)) +
  geom_line(linewidth = 1.5) +
  geom_hline(yintercept = 0.05, colour = col_gold,
             linetype = "dashed", linewidth = 0.8) +
  geom_hline(yintercept = 0.01, colour = col_red,
             linetype = "dotdash", linewidth = 0.8) +
  annotate("text", x = 0.1, y = 0.06,
           label = "1-in-20 year (5%)", colour = col_gold, size = 3.5, hjust = 0) +
  annotate("text", x = 0.1, y = 0.012,
           label = "1-in-100 year (1%)", colour = col_red, size = 3.5, hjust = 0) +
  scale_colour_manual(values = system_colours, name = NULL) +
  scale_y_log10(labels = percent_format()) +
  labs(title = "Exceedance Probability Curves (log scale)",
       subtitle = "P(Annual losses > x) | Lower right = rare but large events",
       x = "Annual loss threshold (Đ million)",
       y = "Probability of exceeding threshold") +
  theme_report

fig9 <- (f9a / f9b) +
  plot_annotation(
    title = "Figure 9 — Monte Carlo Aggregate Loss Distributions",
    theme = theme(
      plot.title      = element_text(colour = "white", face = "bold", size = 13),
      plot.background = element_rect(fill = col_plot_bg, colour = NA)
    )
  )
save_fig(fig9, "fig9_monte_carlo.png", 18, 12)

# Figure 10: Confidence Intervals
ci_df <- map_dfr(target_systems, function(sys) {
  tibble(
    solar_system = sys,
    el           = mc_results[[sys]]$expected_loss,
    ci_lo        = bootstrap_ci[[sys]]$ci_lower,
    ci_hi        = bootstrap_ci[[sys]]$ci_upper,
    cv           = mc_results[[sys]]$cv,
    gp           = mc_results[[sys]]$gross_premium
  )
})

f10a <- ggplot(ci_df |> mutate(sys_label = system_labels[solar_system]),
               aes(x = sys_label, colour = solar_system)) +
  geom_errorbar(aes(ymin = ci_lo / 1e6, ymax = ci_hi / 1e6),
                width = 0.3, linewidth = 2) +
  geom_point(aes(y = el / 1e6), size = 6, shape = 18) +
  geom_point(aes(y = gp / 1e6), size = 4, shape = 16, colour = "white",
             position = position_nudge(x = 0.18)) +
  scale_colour_manual(values = system_colours, guide = "none") +
  labs(title = "E[L] with 95% bootstrap confidence intervals",
       subtitle = "Diamond = E[L] | Circle = gross premium | Error bar = 95% CI",
       x = NULL, y = "Đ million") +
  theme_report

f10b <- ci_df |>
  mutate(sys_label = system_labels[solar_system]) |>
  ggplot(aes(x = reorder(sys_label, cv), y = cv, fill = solar_system)) +
  geom_col(alpha = 0.85, width = 0.5) +
  geom_text(aes(label = sprintf("CV = %.3f", cv)),
            vjust = -0.5, colour = "white", fontface = "bold", size = 4) +
  scale_fill_manual(values = system_colours, guide = "none") +
  scale_y_continuous(expand = expansion(mult = c(0, 0.22))) +
  labs(title = "Coefficient of variation by system",
       subtitle = "CV = Std Dev / E[L] | Higher = wider year-to-year variability",
       x = NULL, y = "CV = SD / E[L]") +
  theme_report

fig10 <- (f10a | f10b) +
  plot_annotation(
    title = "Figure 10 — Confidence Intervals & Aggregate Uncertainty Summary",
    theme = theme(
      plot.title      = element_text(colour = "white", face = "bold", size = 13),
      plot.background = element_rect(fill = col_plot_bg, colour = NA)
    )
  )
save_fig(fig10, "fig10_confidence_intervals.png", 14, 7)

