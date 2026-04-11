## ACTL4001 Assignment
## RStudio - Version 2024.04.1+748 (2024.04.1+748)

#################################### Setup #####################################

### Libraries
# List of packages used for the assignment
packages <- c('dplyr', 'tidyverse', 'readxl', 'corrplot', 'fitdistrplus', 
              'xgboost', 'goftest', 'ggpubr', 'caTools', 'randomForest', 
              'glmnet', 'MASS', 'janitor')

# Install packages if not already installed
for (p in packages) {
  if (!requireNamespace(p)) {
    install.packages(p)
  }
}

# Apply all packages
lapply(packages, require, character.only = TRUE)

### Data setup + Cleaning
setwd("~/Desktop/ACTL4001/Case_Study/SOA_2026_Case_Study_Materials")

raw_e_f_freq <- read_excel("srcsc-2026-claims-equipment-failure.xlsx", 1)
raw_e_f_sev <- read_excel("srcsc-2026-claims-equipment-failure.xlsx", 2)
cosmic_quarry_inventory <- read_excel("srcsc-2026-cosmic-quarry-inventory.xlsx")
interest_inflation <- read_excel("srcsc-2026-interest-and-inflation.xlsx")

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

# Mapping zeta to oryn delta and epsilon to bayesian system
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

##################################### EDA ######################################

## EDA for claim frequency
freq_summary <- e_f_freq %>% 
  group_by(solar_system) %>%
  summarise(
    mean_exposure = mean(exposure),
    mean_maintenance = mean(maintenance_int),
    total_claims = sum(claim_count),
    mean_freq = mean(claim_count)
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
    VaR = quantile(claim_amount,0.95),
    ES = mean(claim_amount[claim_amount > quantile(claim_amount,0.95)]),
    coefficient_var = sd(claim_amount) / mean(claim_amount)
  )

##################################### MLE ######################################

## Modelling distributions using MLE
n <- NROW(e_f_sev)
loss <- e_f_sev$claim_amount
empirical <- ecdf(loss)
mean <- mean(loss)

exp_lambda <- 1/mean

ln_mu <- sum(log(loss)) / n
ln_sd <- sqrt(sum((log(loss) - ln_mu)^2) / n)

pp_ln <- data.frame(
  theoretical_prob = plnorm(loss, ln_mu, ln_sd),
  empirical_prob = empirical(loss)
)

par_lambda <- min(loss)
par_alpha <- n / sum(log(loss) - log(par_lambda))

scale <- 1000
gamma_mle <- fitdistr(loss / scale, densfun = "gamma")
gamma_shape <- gamma_mle$estimate["shape"]
gamma_rate <- gamma_mle$estimate["rate"] / scale
gamma_mle$estimate["rate"] / scale

x_val <-seq(min(loss), max(loss), length = n) 

# Cumulative Distribution Function
ppareto <- function(x, alpha, lambda){
  rep(1, length(x)) - (rep(lambda, length(x)) / x)^alpha
}

# Probability Distribution Function
dpareto <- function(x, alpha, lambda){
  rep(alpha * lambda^alpha, length(x))/x^(alpha + 1)
}

densities <- data.frame(
  x = x_val,

  Exponential = dexp(x_val, exp_lambda),
  Gamma = dgamma(x_val, gamma_shape, gamma_rate),
  LogNormal = dlnorm(x_val, ln_mu, ln_sd)
)

dens_long <- densities %>%
  tidyr::pivot_longer(-x, names_to = "Distribution", values_to = "Density")

# Plot
ggplot(densities, aes(x = loss)) +
  geom_histogram(aes(y = ..density..), bins = 100, fill = "grey", color = "black", 
                 alpha = 0.5) +
  geom_line(data = dens_long, aes(x = x, y = Density, color = Distribution), 
            size = 0.5) +
  labs(title = "Histogram of Losses with Fitted Densities", x = "Claims", y = "Density") +
  coord_cartesian(xlim = c(8000,1000000)) +
  theme_minimal()



ggplot(e_f_sev, aes(x = claim_amount, colour = solar_system)) +
  geom_density(size = 0.5) + labs(title = "Density of Claim Amounts by Solar System",
    x = "Claim Amount",y = "Density", colour = "Solar System") +
   theme_minimal()

################################ AIC & BIC ####################################

d <- 1
exp_loglik <- n * log(exp_lambda) - exp_lambda * sum(loss)
AIC_exp <- 2 * d - 2 * exp_loglik
BIC_exp <- 1 * log(n) - 2 * exp_loglik

d <- 2
pareto_loglik <- n * (log(par_alpha) + par_alpha * log(par_lambda)) - 
  (par_alpha + 1) * sum(log(loss))
AIC_par <- 2 * d - 2 * pareto_loglik
BIC_par <- d * log(n) - 2 * pareto_loglik

ln_loglik <- -n * (log(ln_sd) + log(sqrt(2 * pi))) - sum(log(loss) + 
                                                           1 / (2 * ln_sd^2) * (log(loss) - ln_mu)^2)
AIC_ln <- 2 * d - 2 * ln_loglik
BIC_ln <- d * log(n) - 2 * ln_loglik

gamma_loglik <- n * (gamma_shape * log(gamma_rate) - log(gamma(gamma_shape))) + 
  sum((gamma_shape - 1) * log(loss) - gamma_rate * loss)
AIC_gamma <- 2 * d - 2 * gamma_loglik
BIC_gamma <- d * log(n) - 2 * gamma_loglik

############################## Test Statistics #################################

ad_exp <- ad.test(loss, null = pexp, rate = exp_lambda)
ad_lnorm <- ad.test(loss, null = plnorm, ln_mu, ln_sd)
ad_gamma <- ad.test(loss, null = pgamma, gamma_shape, gamma_rate)
ad_pareto <- ad.test(loss, null = ppareto, par_alpha, par_lambda)

ks_exp <- ks.test(loss, "pexp", rate = exp_lambda)
ks_lnorm <- ks.test(loss, "plnorm", ln_mu, ln_sd)
ks_gamma <- ks.test(loss, "pgamma", gamma_shape, gamma_rate)
ks_pareto <- ks.test(loss, ppareto, par_alpha, par_lambda)

AIC <- c(AIC_exp, AIC_gamma, AIC_ln, AIC_par)
BIC <- c(BIC_exp, BIC_gamma, BIC_ln, BIC_par)
ad <- c(ad_exp$statistic, ad_gamma$statistic, ad_lnorm$statistic, ad_pareto$statistic)
ks <- c(ks_exp$statistic, ks_gamma$statistic, ks_lnorm$statistic, ks_pareto$statistic)

as.data.frame(cbind(AIC, BIC, ad, ks))

# Log normal has the best stats

n <- NROW(e_f_freq)
frequency <- e_f_freq$claim_count
empirical_freq <- ecdf(frequency)
mean_freq <- mean(frequency)
var_freq <- var(frequency)
## Method of moments for negative binomial

# Poisson
lambda_pois <- mean_freq

# NB
r_nb <- mean_freq^2 / (var_freq - mean_freq)
p_nb <- mean_freq / var_freq


# Log likelihoods
pois_loglik <- -n * lambda_pois - sum(log(factorial(frequency))) + 
  log(lambda_pois) * sum(frequency)

nb_loglik <- sum(log(gamma(frequency + r_nb)) - log(factorial(frequency)) + 
                   frequency * log(1 - p_nb)) + n * (r_nb * log(p_nb) - log(gamma(r_nb)))


## AIC & BIC, since difference is AIC is > 5, we will choose nb despite it having
## a slightly worse BIC
d <- 1
AIC_pois <- 2 * d - 2 * pois_loglik
BIC_pois <- d * log(n) - 2 * pois_loglik

d <- 2
AIC_nb <- 2 * d - 2 * nb_loglik
BIC_nb <- d * log(n) - 2 * nb_loglik


mean_nb <- r_nb * (1 - p_nb) / p_nb
var_nb <- r_nb * (1 - p_nb) / p_nb^2

mean_ln <- exp(ln_mu + ln_sd^2 / 2)
var_ln <- (exp(ln_sd^2) - 1) * exp(2 * ln_mu + ln_sd^2)

## Aggregate loss summary

agg_exp <- mean_nb * mean_ln
agg_var <- mean_nb * var_ln + var_nb * mean_ln^2

##################################### GLM ######################################

# Setting up data for models
set.seed(1)

subset_freq <- as.data.frame(e_f_freq[c(3:9)])
subset_sev <- as.data.frame(e_f_sev[c(5:11)])

freq_split <- sample.split(subset_freq, SplitRatio = 0.8)
sev_split <- sample.split(subset_sev, SplitRatio = 0.8)

freq_train <- subset(subset_freq, freq_split == TRUE)
freq_test <- subset(subset_freq, freq_split == FALSE)

X_freq_train <- model.matrix(claim_count ~ . - 1, data = freq_train)
y_freq_train <- freq_train$claim_count
X_freq_test  <- model.matrix(claim_count ~ . - 1, data = freq_test)

sev_train <- subset(subset_sev, sev_split == TRUE)
sev_test <- subset(subset_sev, sev_split == FALSE)

X_sev_train <- model.matrix(claim_amount ~ . - 1, data = sev_train)
y_sev_train <- sev_train$claim_amount
X_sev_test  <- model.matrix(claim_amount ~ . - 1, data = sev_test)

## Frequency model
freq_lasso <- cv.glmnet(X_freq_train, y_freq_train, family = "poisson", 
                        offset = log(freq_train$exposure), alpha = 1)

preds_test_freq <- predict(freq_lasso, newx = X_freq_test, newoffset = log(freq_test$exposure), 
                           s = "lambda.min", type = "response")
test_mse_freq_lasso <- mean((preds_test_freq - freq_test$claim_count)^2)

## Severity model
sev_lasso <- cv.glmnet(X_sev_train, y_sev_train, 
                       family = Gamma(link = "log"), 
                       alpha = 1)

preds_test_sev <- predict(sev_lasso, newx = X_freq_test, s = "lambda.min", type = "response")
test_mse_sev_lasso <- mean((preds_test_sev - sev_test$claim_amount)^2)

# Assigning best model
best_freq <- preds_test_freq 
best_sev <- preds_test_sev

# Rough estimate for pure premium + potential option for gross premium
pure_premium <- mean(best_freq) * mean(best_sev)
variance_agg_loss <- mean(best_freq) * var(best_sev) + 
  var(best_freq) * mean(best_sev)^2
premium <- pure_premium + sqrt(variance_agg_loss) * 0.2



e_f_glm <- rename(cbind(freq_test[, -ncol(freq_test)], preds_test_freq), 
                  claim_count = lambda.min)
e_f_glm <- rename(cbind(e_f_glm, preds_test_sev), 
                  claim_amount = lambda.min)

e_f_glm$pure_premium <- e_f_glm$claim_amount * e_f_glm$claim_count

## Summary for glm frequency severity
freq_summary_glm <- e_f_glm %>% 
  group_by(solar_system) %>%
  summarise(
    mean_exposure = mean(exposure),
    mean_maintenance = mean(maintenance_int),
    total_claims = sum(claim_count),
    mean_freq = mean(claim_count)
  )

## Summary for glm claim severity
sev_summary_glm <- e_f_glm %>% 
  filter(claim_count != 0) %>% 
  group_by(solar_system) %>%
  summarise(
    mean_exposure = mean(exposure),
    mean_maintenance = mean(maintenance_int),
    total_claim_amount = sum(claim_amount),
    mean_claim_amount = mean(claim_amount),
    sd_claim_amount = sd(claim_amount),
    VaR = quantile(claim_amount,0.95),
    ES = mean(claim_amount[claim_amount > quantile(claim_amount,0.95)]),
    coefficient_var = sd(claim_amount) / mean(claim_amount)
  )


inflation_rate <- 0.0377
risk_free_rate <- 0.0311
fixed_expenses <- 500000
variable_exp_ratio <- 0.10
profit_load <- 0.10


# 6. Monte Carlo Capital Modeling (Tail Behavior) 
n_sims <- 10000
total_losses <- numeric(n_sims)

for (i in 1:n_sims) {
  # Galaxy wide shocks
  shock_factor <- if (runif(1) < 0.01) 5 else 1
  
  # Using estimated distributions
  sim_claims <- rpois(4730, e_f_glm$claim_count * shock_factor)
  sim_costs <- sum(rlnorm(sum(sim_claims),ln_mu,ln_sd))
  
  total_losses[i] <- sim_costs
}

# Monte Carlo Capital summary
expected_loss <- mean(total_losses)

# Required capital to survive 1 in 100 year event
var_99 <- quantile(total_losses, 0.99) 

# Solar System Premium Summary
system_summary <- e_f_glm %>%
  group_by(solar_system) %>%
  summarize(total_el = mean(pure_premium) * (1 + inflation_rate))
system_summary <- cbind(system_summary, eq_count = equipment$equipment_sum, 
                        mean_risk = risk_index$mean_risk)

premium_summary <- system_summary %>%
  mutate(
    pv_el = total_el / (1 + risk_free_rate) * eq_count,
    gross_premium = (pv_el * (1 + mean_risk) + fixed_expenses / 3) / 
      (1 - variable_exp_ratio - profit_load)
  )

print(premium_summary)


