## ACTL4001 Assignment
## RStudio - Version 2024.04.1+748 (2024.04.1+748)

################################################################################
## Package Setup 
################################################################################

### Libraries
# List of packages used for the assignment
packages <- c('dplyr', 'tidyverse', 'readxl', 'corrplot', 'stats', 'goftest', 
              'caTools', 'glmnet', 'MASS', 'janitor', 'moments', 'purrr')

# Install packages if not already installed
for (p in packages) {
  if (!requireNamespace(p)) {
    install.packages(p)
  }
}

# Apply all packages
lapply(packages, require, character.only = TRUE)

################################################################################
## Data Setup 
################################################################################

## Setting seed to replicate results
set.seed(1)

# Setting up data for models

## CHANGE WORKING DIRECTORY ACCORDINGLY
setwd("~/Desktop/ACTL4001/Case_Study/SOA_2026_Case_Study_Materials")

raw_e_f_freq <- read_excel("srcsc-2026-claims-equipment-failure.xlsx", 1)
raw_e_f_sev <- read_excel("srcsc-2026-claims-equipment-failure.xlsx", 2)
cosmic_quarry_inventory <- read_excel("srcsc-2026-cosmic-quarry-inventory.xlsx")
interest_inflation <- read_excel("srcsc-2026-interest-and-inflation.xlsx")
interest_inflation <- interest_inflation[5:17,1:5]

clean_equipment_data <- function(df) {
  df %>%
    row_to_names(row_number = 1) %>%
    rename(equipment = 1) %>%
    pivot_longer(
      cols = -equipment, 
      names_to = "solar_system", 
      values_to = "count"
    ) %>%
    mutate(count = as.numeric(count)) %>%
    pivot_wider(
      names_from = equipment, 
      values_from = count
    ) %>%
    clean_names()
}

equipment <- clean_equipment_data(cosmic_quarry_inventory[3:9, 1:4])
equipment <- cbind(equipment, equipment_sum = rowSums(equipment[-1]))
equipment <- equipment[order(equipment$solar_system), ]
risk_index <- clean_equipment_data(cosmic_quarry_inventory[49:55, 1:4])
risk_index <- cbind(risk_index, mean_risk = rowMeans(risk_index[-1]))
risk_index <- risk_index[order(risk_index$solar_system), ]

## String cleaning functions
clean_id <- function(ids) {
  return(str_replace(ids,"_.*",""))
}

clean_string <- function(strings) {
  return(str_replace(strings,"[^A-Za-z ]+$",""))
}

# Mapping zeta to Oryn delta and epsilon to Bayesian system
new_names <- setNames((c("Helionis Cluster","Oryn Delta", "Bayesian System")), c("Helionis Cluster", "Zeta","Epsilon"))

# Cleaning frequency table
e_f_freq <- na.omit(unique(raw_e_f_freq) %>%
  mutate(
    policy_id = if_else(duplicated(clean_id(policy_id)), NA, clean_id(policy_id)),
    equipment_id = clean_id(equipment_id),
    equipment_type = clean_string(equipment_type),
    solar_system = new_names[clean_string(solar_system)],
    equipment_age = if_else(between(equipment_age, 0, 20), equipment_age, NA),
    maintenance_int = if_else(between(maintenance_int, 100, 5000), maintenance_int, NA),
    usage_int = if_else(between(usage_int, 0, 24), usage_int, NA),
    exposure = if_else(between(exposure, 0, 1), exposure, NA),
    claim_count = if_else(claim_count %in% 0:3, claim_count, NA)
  ))

# Cleaning severity
e_f_sev <- na.omit(unique(raw_e_f_sev) %>%
  mutate(
    claim_id = clean_id(claim_id),
    claim_seq = if_else(claim_seq %in% 0:3, claim_seq, NA),
    policy_id = clean_id(policy_id),
    equipment_id = clean_id(equipment_id),
    equipment_type = clean_string(equipment_type),
    equipment_age = if_else(between(equipment_age, 0, 20), equipment_age, NA),
    solar_system = new_names[clean_string(solar_system)],
    maintenance_int = if_else(between(maintenance_int, 100, 5000), maintenance_int, NA),
    usage_int = if_else(between(usage_int, 0, 24), usage_int, NA),
    exposure = if_else(between(exposure, 0, 1), exposure, NA),
    claim_amount = if_else(claim_amount >= 0,  claim_amount, NA)
  ))

# Aggregate severity by claim_id and update claim_seq
e_f_sev <- e_f_sev %>%
  group_by(claim_id, policy_id, equipment_id, equipment_type, equipment_age, 
           solar_system, maintenance_int, usage_int, exposure) %>%
  summarize(claim_amount = sum(claim_amount), claim_seq = max(claim_seq), 
    .groups = 'drop') %>% 
  arrange(policy_id, claim_id) %>%
  group_by(policy_id) %>%
  mutate(claim_seq = row_number()) %>%
  ungroup() %>%
  relocate(claim_seq, .after = claim_id)


################################################################################
## Constants
################################################################################

inflation_rate <- 0.0239
risk_free_rate <- 0.0474
fixed_expenses <- 500000
variable_exp_ratio <- 0.07
## Includes 
profit_load <- 0.05

## Judgement based selection
cred_Z <- c(
  "Helionis Cluster" = 1.00,
  "Bayesian System" = 0.60,
  "Oryn Delta" = 0.50
)

deductible_values <- e_f_sev %>% 
  group_by(solar_system) %>%
  summarise(
    deductibles = quantile(claim_amount,0.1),
  ) %>% dplyr::select(deductibles)

deductible <- deductible_values$deductibles
names(deductible) <- c("Bayesian System", "Helionis Cluster", "Oryn Delta")

################################################################################
## Exploratory Data Analysis
################################################################################

## EDA for claim frequency
freq_summary <- e_f_freq %>% 
  group_by(solar_system) %>%
  summarise(
    mean_exposure = mean(exposure),
    mean_maintenance = mean(maintenance_int),
    total_claims = sum(claim_count),
    mean_freq = mean(claim_count),
    sd_freq = sd(claim_count)
  )

## EDA for claim severity
sev_summary <- e_f_sev %>% 
  group_by(solar_system) %>%
  summarise(
    mean_exposure = mean(exposure),
    mean_maintenance = mean(maintenance_int),
    total_claim_amount = sum(claim_amount),
    mean_claim_amount = mean(claim_amount),
    sd_claim_amount = sd(claim_amount),
    skew = skewness(claim_amount),
    kurtosis = kurtosis(claim_amount),
    VaR = quantile(claim_amount,0.95),
    ES = mean(claim_amount[claim_amount > quantile(claim_amount,0.95)]),
    coefficient_var = sd(claim_amount) / mean(claim_amount)
  )

equipment_summary <- e_f_sev %>% 
  group_by(equipment_type) %>%
  summarise(
    mean_exposure = mean(exposure),
    mean_maintenance = mean(maintenance_int),
    total_claim_amount = sum(claim_amount),
    mean_claim_amount = mean(claim_amount),
    sd_claim_amount = sd(claim_amount),
    skew = skewness(claim_amount),
    kurtosis = kurtosis(claim_amount),
    VaR = quantile(claim_amount,0.95),
    ES = mean(claim_amount[claim_amount > quantile(claim_amount,0.95)]),
    coefficient_var = sd(claim_amount) / mean(claim_amount),
  )

## Severity Plot
ggplot(e_f_sev, aes(x = claim_amount, colour = solar_system)) +
  geom_density(size = 0.5) + 
  labs(title = "Density of Claim Amounts by Solar System",
       x = "Claim Amount", y = "Density", colour = "Solar System") +
  scale_colour_discrete(labels = c("Bayesian System" = "Epsilon (Bayesian System)", 
                                   "Oryn Delta" = "Zeta (Oryn Delta)")) +
  theme_minimal(base_size = 11) + 
  coord_cartesian(xlim = c(8000, 1000000))

## Frequency Plot
ggplot(filter(e_f_freq, claim_count > 0), aes(x = factor(claim_count), fill = equipment_type)) +
  geom_bar(position = position_dodge(preserve = "single")) + 
  labs(title = "Histogram of Claim Counts by Solar System (> 0)", x = "Claim Amount",
    y = "Count", fill = "Solar System") +
  scale_fill_discrete(labels = c("Bayesian System" = "Epsilon (Bayesian System)", 
                                   "Oryn Delta" = "Zeta (Oryn Delta)")) +
    theme_minimal(base_size = 15)

## Claim Severity by equipment type
ggplot(e_f_sev, aes(x = claim_amount, colour = equipment_type)) +
  geom_density(size = 0.5) + 
  labs(title = "Density of Claim Amounts by Equipment type",
       x = "Claim Amount", y = "Density", colour = "Solar System") +
  theme_minimal(base_size = 11) + 
  coord_cartesian(xlim = c(8000, 1000000))

ggplot(filter(e_f_freq, claim_count > 0), aes(x = factor(solar_system), fill = equipment_type)) +
  # Use position = "stack" to segment equipment types within one bar
  geom_bar(position = "stack") + 
  labs(
    title = "Claim Counts by Equipment",
    x = "Solar System", # Updated for consistency
    y = "Total Count",
    fill = "Equipment Type" # Updated to match your fill variable
  ) +
  scale_fill_discrete(labels = c(
    "Bayesian System" = "Epsilon (Bayesian System)", 
    "Oryn Delta" = "Zeta (Oryn Delta)"
  )) +
  theme_minimal(base_size = 15)

################################################################################
## Premium Modellling 
################################################################################
subset_freq <- as.data.frame(e_f_freq[c(3:9)])
subset_sev <- as.data.frame(e_f_sev[c(5:11)])

freq_split <- sample.split(subset_freq, SplitRatio = 0.8)
sev_split <- sample.split(subset_sev, SplitRatio = 0.8)

freq_train <- subset(subset_freq, freq_split == TRUE)
freq_test <- subset(subset_freq, freq_split == FALSE)

sev_train <- subset(subset_sev, sev_split == TRUE)
sev_test <- subset(subset_sev, sev_split == FALSE)

X_freq_train <- model.matrix(claim_count ~ . - exposure - 1, data = freq_train)
X_freq_test  <- model.matrix(claim_count ~ . - exposure - 1, data = freq_test)


X_sev_train <- model.matrix(claim_amount ~ . - exposure - 1, data = sev_train)
X_sev_test  <- model.matrix(claim_amount ~ . - exposure - 1, data = sev_test)

## Frequency Model
freq_lasso <- cv.glmnet(X_freq_train, freq_train$claim_count, family = "poisson", 
                        offset = log(freq_train$exposure), alpha = 1)
freq_lambda <- freq_lasso$lambda.min
freq_coefs <- coef(freq_lasso, s = freq_lambda)

preds_test_freq <- predict(freq_lasso, newx = X_freq_test, 
                           newoffset = log(freq_test$exposure), 
                           s = "lambda.min", type = "response")
test_mse_freq_lasso <- mean((preds_test_freq - freq_test$claim_count)^2)

## Severity model
sev_lasso <- cv.glmnet(X_sev_train, sev_train$claim_amount, 
                       family = Gamma(link = "log"), 
                       alpha = 1)
sev_lambda <- sev_lasso$lambda.min
sev_coefs <- coef(sev_lasso, s = sev_lambda)

preds_test_sev <- predict(sev_lasso, newx = X_sev_test, s = "lambda.min", type = "response")
test_mse_sev_lasso <- mean((preds_test_sev - sev_test$claim_amount)^2)


## Setting up full data prediction
X_freq_full <- model.matrix(claim_count ~ . - exposure - 1, 
                            data = as.data.frame(e_f_freq[c(3:9)]))
X_sev_full  <- model.matrix(claim_amount ~ . - exposure - 1, 
                            data = as.data.frame(e_f_sev[c(5:11)]))

pred_freq_full <- predict(freq_lasso, newx = X_freq_full, 
                          newoffset = log(e_f_freq$exposure), s = "lambda.min", 
                          type = "response")
pred_sev_full  <- predict(sev_lasso, newx = X_freq_full, s = "lambda.min", 
                          type = "response")

e_f_full <- e_f_freq[c(3:9)]
e_f_full$pred_freq <- as.numeric(pred_freq_full)
e_f_full$pred_sev <- as.numeric(pred_sev_full)
e_f_full$pred_pure_prem <- e_f_full$pred_freq * e_f_full$pred_sev

## Incorporating deductible into expected premiums
ded_factors_emp <- e_f_sev %>%
  group_by(solar_system) %>%
  summarise(
    ded_factor = sum(pmax(claim_amount - deductible[solar_system], 0)) /
      sum(claim_amount),
    .groups = "drop"
  )

## Setting names
ded_factors <- setNames(ded_factors_emp$ded_factor, ded_factors_emp$solar_system)

## Applying deductible factor
e_f_full <- e_f_full %>%
  mutate(
    pred_pure_prem = pred_pure_prem * ded_factors[solar_system]
  )


grand_mean_pp <- mean(e_f_full$pred_pure_prem)

glm_pp <- e_f_full %>%
  group_by(solar_system) %>%
  summarise(glm_pure_prem = mean(pred_pure_prem), .groups = "drop")

glm_pp <- glm_pp %>%
  mutate(
    Z = cred_Z[solar_system],
    cred_pp = Z * glm_pure_prem + (1 - Z) * grand_mean_pp
  )

# Reorder glm_pp to match equipment ordering
glm_pp <- glm_pp %>%
  arrange(solar_system) %>%
  mutate(
    eq_count  = equipment$equipment_sum,
    mean_risk = risk_index$mean_risk,
    eq_growth = c(0.25, 0.25, 0.15)
  )

## Setting up long and short term premiums
projection_years <- 1:10

# Cumulative inflation factors (average inflation over 10 years assumption)
cum_inflation <- cumprod(1 + rep(0.0246, 10)) 

# Create the list of summaries
premium_summaries_list <- lapply(projection_years, function(y) {
  
  # Initialising rates
  current_cum_inflation <- cum_inflation[y]
  current_rf_rate <- if(y > 1) 0.051 else 0.0474 
  
  # Applying equipment growth factors 
  
  system_growth_factors <- c("Bayesian System" = 0.25, 
                             "Helionis Cluster" = 0.25, 
                             "Oryn Delta" = 0.15)
  # Calculate the summary for Year 'y'
  glm_pp %>%
    dplyr::mutate(
      year = y,
      # Apply growth factor
      eq_count_adj = round(eq_count * (1 + system_growth_factors[solar_system] * y / 10)),
      # Apply inflation and rf rate
      inflated_el = cred_pp * current_cum_inflation,
      pv_el = (inflated_el * eq_count_adj) / (1 + current_rf_rate)^y,
      gross_premium = (pv_el * (1 + mean_risk) + (fixed_expenses / 3)) /
        (1 - variable_exp_ratio - profit_load)
    ) %>%
    dplyr::select(year, solar_system, eq_count_adj, glm_pure_prem, Z, 
                  cred_pp, inflated_el, pv_el, gross_premium)
})

short_term_premiums <- premium_summaries_list[[1]]$gross_premium
long_term_premiums <- premium_summaries_list

total_lt_prems <- numeric(10)
h_total_lt <- 0 
b_total_lt <- 0 
o_total_lt <- 0 
for (i in 1:10) {
  total_lt_prems[i] <- sum(long_term_premiums[[i]]$gross_premium)
  h_total_lt <- h_total_lt + long_term_premiums[[i]]$gross_premium["Helionis Cluster"]
  b_total_lt <- b_total_lt + long_term_premiums[[i]]$gross_premium["Bayesian System"]
  o_total_lt <- o_total_lt + long_term_premiums[[i]]$gross_premium["Oryn Delta"]
}


# Shows how sensitive Bayesian/Oryn premiums are to the credibility judgment
z_vals <- c(0.30, 0.50, 0.70, 0.90)

sensitivity_Z <- lapply(z_vals, function(z) {
  glm_pp %>%
    mutate(
      Z_test = ifelse(solar_system == "Helionis Cluster", 1.0, z),
      cred_pp_t = Z_test * glm_pure_prem + (1 - Z_test) * grand_mean_pp,
      pv_el_t = cred_pp_t * (1 + inflation_rate) / (1 + risk_free_rate) * eq_count,
      gp_t = (pv_el_t * (1 + mean_risk) + fixed_expenses / 3) /
        (1 - variable_exp_ratio - profit_load)
    ) %>%
    summarise(Z = z, total_gross_premium = sum(gp_t))
}) %>% bind_rows()

## Senstitivity analysis for credibility
print(sensitivity_Z %>% mutate(total_gross_premium = 
                                 sprintf("Đ%.2fM", total_gross_premium / 1e6)))


################################################################################
## Capital Modelling + Distribution Modelling
################################################################################

## Modelling distributions by solar system and aggregate data
e_f_sev_h <- filter(e_f_sev, solar_system == "Helionis Cluster")
e_f_freq_h <- filter(e_f_freq, solar_system == "Helionis Cluster")

e_f_sev_o <- filter(e_f_sev, solar_system == "Oryn Delta")
e_f_freq_o <- filter(e_f_freq, solar_system == "Oryn Delta")

e_f_sev_b <- filter(e_f_sev, solar_system == "Bayesian System")
e_f_freq_b <- filter(e_f_freq, solar_system == "Bayesian System")

distribution_modelling <- function(e_f_sev, e_f_freq) {
  # Severity setup
  loss <- e_f_sev$claim_amount
  n_sev <- length(loss)
  
  # Parameters
  exp_lambda <- 1 / mean(loss)
  ln_mu      <- mean(log(loss))
  ln_sd      <- sd(log(loss)) * sqrt((n_sev - 1) / n_sev)
  par_lambda <- min(loss)
  par_alpha  <- n_sev / sum(log(loss) - log(par_lambda))
  
  gamma_fit <- tryCatch(fitdistr(loss / 1000, densfun = "gamma"), error = function(e) NULL)
  g_shape   <- if(!is.null(gamma_fit)) gamma_fit$estimate["shape"] else NA
  g_rate    <- if(!is.null(gamma_fit)) gamma_fit$estimate["rate"] / 1000 else NA
  
  # Likelihoods
  ll_sev <- c(
    Exponential = sum(dexp(loss, exp_lambda, log = TRUE)),
    LogNormal   = sum(dlnorm(loss, ln_mu, ln_sd, log = TRUE)),
    Gamma       = if(!is.na(g_shape)) sum(dgamma(loss, g_shape, g_rate, log = TRUE)) else -Inf,
    Pareto      = n_sev*(log(par_alpha) + par_alpha*log(par_lambda)) - (par_alpha + 1)*sum(log(loss))
  )
  
  k_sev <- c(1, 2, 2, 2)
  
  # Severity Summary Table
  sev_summary <- data.frame(
    Dist    = c("Exponential", "LogNormal", "Gamma", "Pareto"),
    AIC     = 2 * k_sev - 2 * ll_sev,
    BIC     = k_sev * log(n_sev) - 2 * ll_sev,
    AD_Stat = c(
      ad.test(loss, "pexp", rate = exp_lambda)$statistic,
      ad.test(loss, "plnorm", ln_mu, ln_sd)$statistic,
      if(!is.na(g_shape)) ad.test(loss, "pgamma", g_shape, g_rate)$statistic else NA,
      ad.test(loss, function(x) 1-(par_lambda/x)^par_alpha)$statistic
    ),
    KS_Stat = c(
      suppressWarnings(ks.test(loss, "pexp", rate = exp_lambda)$statistic),
      suppressWarnings(ks.test(loss, "plnorm", ln_mu, ln_sd)$statistic),
      if(!is.na(g_shape)) suppressWarnings(ks.test(loss, "pgamma", g_shape, g_rate)$statistic) else NA,
      suppressWarnings(ks.test(loss, function(x) 1-(par_lambda/x)^par_alpha)$statistic)
    )
  )
  
  ## Frequency models
  freq <- e_f_freq$claim_count
  n_f  <- length(freq)
  m_f  <- mean(freq)
  v_f  <- var(freq)
  
  # Poisson 
  ll_pois <- sum(dpois(freq, m_f, log = TRUE))
  
  if (v_f > m_f) {
    r_nb  <- m_f^2 / (v_f - m_f)
    p_nb  <- m_f / v_f
    ll_nb <- sum(dnbinom(freq, size = r_nb, prob = p_nb, log = TRUE))
  } else {
    r_nb  <- NA
    p_nb  <- NA
    ll_nb <- -Inf 
  }
  
  freq_summary <- data.frame(
    Dist = c("Poisson", "NegBinomial"),
    AIC  = c(2*1 - 2*ll_pois, 2*2 - 2*ll_nb),
    BIC  = c(log(n_f)*1 - 2*ll_pois, log(n_f)*2 - 2*ll_nb)
  )

  
  # Return the summaries
  return(list(
    sev_metrics  = sev_summary,
    freq_metrics = freq_summary,
    severity = list(
      Exponential = list(rate = exp_lambda),
      LogNormal   = list(mu = ln_mu, sd = ln_sd),
      Gamma       = list(shape = g_shape, rate = g_rate),
      Pareto      = list(alpha = par_alpha, lambda = par_lambda)
    ),
    frequency = list(
      Poisson     = list(lambda = m_f),
      NegBinomial = list(size = r_nb, prob = p_nb)
    )
  ))
}

## Loss distributions for all the data and then separated into solar systems
full <- distribution_modelling(e_f_sev, e_f_freq)
hp <- distribution_modelling(e_f_sev_h, e_f_freq_h)
op <- distribution_modelling(e_f_sev_o, e_f_freq_o)
bp <- distribution_modelling(e_f_sev_b, e_f_freq_b)

full_param_summary <- do.call(rbind, lapply(full$severity, unlist))
hp_param_summary <- do.call(rbind, lapply(hp$severity, unlist))
op_param_summary <- do.call(rbind, lapply(op$severity, unlist))
bp_param_summary <- do.call(rbind, lapply(bp$severity, unlist))

full_param_summary <- do.call(rbind, lapply(full$frequency, unlist))
hp_param_summary_f <- do.call(rbind, lapply(hp$frequency, unlist))
op_param_summary_f <- do.call(rbind, lapply(op$frequency, unlist))
bp_param_summary_f <- do.call(rbind, lapply(bp$frequency, unlist))



x_val <-seq(min(e_f_sev$claim_amount), max(e_f_sev$claim_amount), length = NROW(e_f_sev)) 

dpareto <- function(x, alpha, lambda){
  rep(alpha * lambda^alpha, length(x))/x^(alpha + 1)
}

densities <- data.frame(
  x = x_val,
  Pareto = dpareto(x_val, full$severity$Pareto$alpha, full$severity$Pareto$lambda),
  Exponential = dexp(x_val, full$severity$Exponential$rate),
  Gamma = dgamma(x_val, full$severity$Gamma$shape, full$severity$Gamma$rate),
  LogNormal = dlnorm(x_val, full$severity$LogNormal$mu, full$severity$LogNormal$sd)
)

dens_long <- densities %>%
  tidyr::pivot_longer(-x, names_to = "Distribution", values_to = "Density")

# Plot
ggplot(densities, aes(x = e_f_sev$claim_amount)) +
  geom_histogram(aes(y = ..density..), bins = 100, fill = "grey", color = "black", 
                 alpha = 0.5) +
  geom_line(data = dens_long, aes(x = x, y = Density, color = Distribution), 
            size = 0.5) +
  labs(title = "Histogram of Claim amount with Fitted Densities", x = "Claims", y = "Density") +
  coord_cartesian(xlim = range(c(0,1000000)), ylim = c(0,1.2e-05)) +
  theme_minimal()

## Monte Carlo Capital Modeling  
rescale_nb_size <- function(nb_params, target_mean) {
  # E[NB] = size * (1-p)/p  =>  size = target_mean * p / (1-p)
  list(
    size = target_mean * nb_params$prob / (1 - nb_params$prob),
    prob = nb_params$prob
  )
}

system_nb_mean <- sapply(list(hp, bp ,op), function(p) {
  nb <- p$frequency$NegBinomial
  nb$size * (1 - nb$prob) / nb$prob
})

system_total_nb_mean <- mean(system_nb_mean)
cred_nb_means <- cred_Z * system_nb_mean + (1 - cred_Z) * system_total_nb_mean

bp_nb <- rescale_nb_size(bp$frequency$NegBinomial, cred_nb_means["Bayesian System"])
hp_nb <- rescale_nb_size(hp$frequency$NegBinomial, cred_nb_means["Helionis Cluster"])
op_nb <- rescale_nb_size(op$frequency$NegBinomial, cred_nb_means["Oryn Delta"])

bp_ln <- bp$severity$LogNormal
hp_ln <- hp$severity$LogNormal
op_ln <- op$severity$LogNormal


## Monte carlo simulation for capital model
monte_carlo_sim <- function(n_sims = 10000, years = 1, shock_prob = 0.01, 
                            shock_factor = 1.75, inflation_rate = 0, rf_rate = 0) {
  
  final_losses <- numeric(n_sims)
  
  for (i in 1:n_sims) {
    path_losses <- 0

    for (y in 1:years) {
      joint_shock <- if (runif(1) < shock_prob) shock_factor else 1

      t <- if (years == 1) 0 else y 
      calc_net_loss <- function(sys_name, nb_p, ln_p) {
        eq_count <-  equipment$equipment_sum[equipment$solar_system == sys_name] * (1 + 0.25 * t / 10)
        counts <- rnbinom(round(eq_count), nb_p$size * joint_shock, nb_p$prob)
        total_claims <- sum(counts)
        
        if (total_claims == 0) return(0)
        
        # Simulate ground-up claims
        ground_up_claims <- rlnorm(total_claims, ln_p$mu, ln_p$sd)
        # Apply deductible: Net = max(0, Claim - Deductible)
        return(sum(pmax(0, ground_up_claims - deductible[sys_name])))
      }
      y_loss <- calc_net_loss("Bayesian System", bp_nb, bp_ln) +
        calc_net_loss("Helionis Cluster", hp_nb, hp_ln) +
        calc_net_loss("Oryn Delta", op_nb, op_ln)
      
      path_losses <- path_losses + (y_loss * (1 + inflation_rate)^t) / (1 + rf_rate)^t
    }
    final_losses[i] <- path_losses
  }
  return(final_losses)
}

## Summarise monte carlo results
simulation_summary <- function(n_sims = 10000, years = 1, shock_prob = 0.01, 
                                   shock_factor = 1.75, inflation_rate = 0,
                                   rf_rate = 0) {
  
  sim_losses <- monte_carlo_sim(n_sims, years, shock_prob = shock_prob, 
                                shock_factor = shock_factor, 
                                inflation_rate = inflation_rate, 
                                rf_rate = rf_rate)
  
  exp_loss <- mean(sim_losses)
  std_dev <- sd(sim_losses)
  var <- quantile(sim_losses, c(0.95, 0.99, 0.995))
  es_val <- sapply(var, function(x) mean(sim_losses[sim_losses > x]))

  # Create Summary Table
  summary_table <- data.frame(
    metric = c("Expected Loss", "Std Deviation", "VaR 95%", "VaR 99%", 
               "VaR 99.5%", "ES 95%", "ES 99%", "ES 99.5%", "Capital 99%"),
    value = c(exp_loss, std_dev, var[1], var[2], var[3], es_val[1], es_val[2], 
               es_val[3], es_val[2] - exp_loss)
    ) %>%
    mutate(reg_val = value,
           value = sprintf("%.2f", value / 1e6))
  return(summary_table)
}

## Short term capital
st_capital <- simulation_summary(n_sims = 100000, years = 1, 
                                 inflation_rate = 0.0239, rf_rate = 0.0474)
## Long term capital
lt_capital <- simulation_summary(n_sims = 10000, years = 10, 
                                 inflation_rate = 0.0239, rf_rate = 0.0474)


# Define base parameters
base_params <- list(
  shock_prob = 0.01,
  shock_factor = 2,
  inflation_rate = 0.0239, # Set a non-zero base to allow % changes
  rf_rate = 0.0474
)

shifts <- c(-0.5, -0.25, -0.1, 0, 0.1, 0.25, 0.5)
params_to_test <- names(base_params)

# 2. Run Sensitivity Analysis
sensitivity_results <- map_df(params_to_test, function(p_name) {
  map_df(shifts, function(s) {

    # Adjust parameters
    current_params <- base_params
    current_params[[p_name]] <- current_params[[p_name]] * (1 + s)

    # Call monte carlo simulation
    res <- do.call(simulation_summary, c(list(n_sims = 10000), current_params))

    # Formatting cleanly
    cap_val <- res %>%
      filter(metric == "Capital 99%") %>%
      pull(value) %>%
      as.numeric()

    data.frame(
      parameter = p_name,
      shift = s,
      capital_99 = cap_val,
      sd = sd(res$reg_val)
    )
  })
})

# Generate plot
ggplot(sensitivity_results, aes(x = shift, y = capital_99, color = parameter)) +
  geom_line(size = 1) +
  geom_point() +
  scale_x_continuous(labels = scales::percent) +
  labs(
    title = "Sensitivity Analysis: Impact on 99% Capital",
    subtitle = "Parameters adjusted by +/- 10, 25, 50%",
    x = "Percentage Change from Base",
    y = "Capital Requirement (Millions)"
  ) +
  theme_minimal(base_size = 12) +
  theme(legend.position = "bottom")
