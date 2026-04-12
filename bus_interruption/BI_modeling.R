########## PRELIM A: Download, libraries, clean and structuring data ############ 
library(readxl)
bus.interupt.freq <- read_excel("Downloads/SOA_2026_Case_Study_Materials/Fixed dataset.xlsx", 
                                                      sheet = "freq")
View(bus.interupt.freq)

bus.interupt.sev <- read_excel("Downloads/SOA_2026_Case_Study_Materials/Fixed dataset.xlsx", 
                                                      sheet = "sev")
View(bus.interupt.sev)

library(dplyr)

#mutate column 
bus.interupt.freq$solar_system[grepl("Zeta", bus.interupt.freq$solar_system)] <- "Zeta"
bus.interupt.freq$solar_system[grepl("Epsilon", bus.interupt.freq$solar_system)] <- "Epsilon"
bus.interupt.freq$solar_system[grepl("Helionis Cluster", bus.interupt.freq$solar_system)] <- "Helionis Cluster"

# fixing data to the dictionary cap
filtered.bus.interupt.freq <- bus.interupt.freq %>%
  filter(
    production_load >= 0 & production_load <= 1,
    energy_backup_score %in% 1:5,
    supply_chain_index >= 0 & supply_chain_index <= 1,
    avg_crew_exp >= 1 & avg_crew_exp <= 30,
    maintenance_freq >= 0 & maintenance_freq <= 6,
    safety_compliance %in% 1:5,
    exposure >= 0 & exposure <= 1,
    claim_count >= 0 & claim_count <= 4
  ) %>%
  select(-policy_id, -station_id)
  
library(dplyr)

#mutate column 
bus.interupt.sev$solar_system[grepl("Zeta", bus.interupt.sev$solar_system)] <- "Zeta"
bus.interupt.sev$solar_system[grepl("Epsilon", bus.interupt.sev$solar_system)] <- "Epsilon"
bus.interupt.sev$solar_system[grepl("Helionis Cluster", bus.interupt.sev$solar_system)] <- "Helionis Cluster"



library(dplyr)

filtered.bus.interupt.sev <- bus.interupt.sev %>%
  mutate(
    claim_amount = pmax(28000, pmin(claim_amount, 1426000))
  ) %>%
  filter(
    production_load >= 0 & production_load <= 1,
    energy_backup_score %in% 1:5,
    safety_compliance %in% 1:5,
    exposure >= 0 & exposure <= 1
  ) %>%
  select(-policy_id, -station_id, -claim_id)
  


################################################################################
############################## PRELIM B: EDA ################################### 

naive.glm.severity <- glm(claim_amount ~ . , data = filtered.bus.interupt.sev, family = Gamma(link = "log"))

summary(naive.glm.severity)


mean(filtered.bus.interupt.sev$claim_amount, na.rm = TRUE)
var(filtered.bus.interupt.sev$claim_amount, na.rm = TRUE)
#[1] 1174300 very heavy tailed

# Splitting into solar systems for any further analysis

solar_list <- filtered.bus.interupt.freq %>%
  group_by(solar_system) %>%
  group_split(.keep = FALSE)

names(solar_list) <- unique(filtered.bus.interupt.freq$solar_system)

naive.glm.frequency <- glm(claim_count ~ . , offset = exposure, data = filtered.bus.interupt.freq, family = "poisson")


###### Comparing claim frequency across solar systems
library(ggplot2)
filtered.bus.interupt.sev %>%
  filter(!is.na(solar_system)) %>%
  # Remove the 'clumped' outliers to see the actual distribution
  filter(claim_amount < 1250000) %>% 
  ggplot(aes(x = claim_amount, fill = solar_system)) +
  geom_histogram(
    aes(y = after_stat(count)), 
    alpha = 0.5, 
    position = "identity", 
    bins = 40, 
    color = "black"
  ) +
  # Using a standard scale now since we removed the extreme tail
  scale_x_continuous(labels = scales::comma) + 
  theme_classic() +
  labs(
    title = "Distribution of Attritional Claim Amounts",
    subtitle = "Excluding claims > $1.25M to show distribution shape",
    x = "Claim Amount ($)",
    y = "Frequency"
  ) +
  theme(
    legend.position.inside = c(0.15, 0.75), # Moved to top-left where it's emptier
    legend.background = element_rect(color = "black")
  )

library(dplyr)
filtered.bus.interupt.sev %>%
  filter(claim_amount == 1426000) %>%
  group_by(solar_system) %>%
  tally() # or count(solar_system)
count_matrix <- filtered.bus.interupt.freq %>%
  filter(!is.na(solar_system)) %>% 
  group_by(solar_system, claim_count) %>%
  tally() %>%
  pivot_wider(names_from = claim_count, values_from = n, values_fill = 0) %>%
  # Use dplyr:: explicitly here
  dplyr::select(solar_system, any_of(as.character(0:5))) 

count_matrix

# --- SOLAR SYSTEM FREQUENCY PROFILE ---

system_frequency_profile <- filtered.bus.interupt.freq %>%
  filter(!is.na(solar_system)) %>%
  group_by(solar_system) %>%
  summarise(
    avg_maintenance = mean(maintenance_freq, na.rm = TRUE),
    avg_backup_score = mean(energy_backup_score, na.rm = TRUE),
    avg_supply_index = mean(supply_chain_index, na.rm = TRUE),
    avg_crew_exp = mean(avg_crew_exp, na.rm = TRUE),
    avg_compliance = mean(safety_compliance, na.rm = TRUE),
    avg_exposure = mean(exposure, na.rm = TRUE),
    total_records = n(),
    claim_frequency = mean(claim_count, na.rm = TRUE)
  )

View(system_frequency_profile)

# --- SOLAR SYSTEM SEVERITY PROFILE ---
system_severity_profile <- filtered.bus.interupt.sev %>%
  filter(!is.na(solar_system)) %>%
  group_by(solar_system) %>%
  summarise(
    avg_claim_amount = mean(claim_amount, na.rm = TRUE),
    median_claim_amount = median(claim_amount, na.rm = TRUE),
    # Count how many claims hit $1.426M policy limit
    count_limit_hits = sum(claim_amount >= 1426000, na.rm = TRUE),
    # Percentage of claims that are "Total Losses"
    prop_limit_hits = mean(claim_amount >= 1426000, na.rm = TRUE),
    avg_exposure = mean(exposure, na.rm = TRUE),
    avg_backup_score = mean(energy_backup_score, na.rm = TRUE),
    avg_production_load = mean(production_load, na.rm = TRUE),
    avg_compliance = mean(safety_compliance, na.rm = TRUE),
    total_claims = n()
  )

View(system_severity_profile)


# Findings: Epsilon and Zeta has similar claim distribution, HC has significantly lower 
# claim frequency by half for each category

# VIF test and correlation matrix for multicollinearity 
library(car)
temp_glm <- glm(claim_count ~ solar_system + production_load + energy_backup_score + 
                  supply_chain_index + avg_crew_exp + maintenance_freq + 
                  safety_compliance + offset(log(exposure)), 
                data = filtered.bus.interupt.freq, family = poisson)
vif(temp_glm)







################################################################################
######################## PART 0: CLAIM FREQUENCY FITTING #######################
hist(filtered.bus.interupt.freq$claim_count)
var(filtered.bus.interupt.freq$claim_count)
mean(filtered.bus.interupt.freq$claim_count)

# candidate: negative bin or ZINB
install.packages("glmmTMB") 
library(glmmTMB)

nb_model <- glmmTMB(
  claim_count ~ solar_system + production_load + energy_backup_score + 
    supply_chain_index + avg_crew_exp + maintenance_freq + safety_compliance + offset(log(exposure)),
  data = filtered.bus.interupt.freq,
  family = nbinom2
)

summary(nb_model)


# log-link function by default, the raw coefficients are on a log scale. 
# To make them useful for a business report or a pricing model, must exponentiate
# them to get Rate Ratios.
hist(residuals(nb_model))


# Option 2: Constant Zero-Inflation (Simplest start)
zinb_model_basic <- glmmTMB(
  claim_count ~ solar_system + production_load + energy_backup_score + 
    supply_chain_index + avg_crew_exp + maintenance_freq + 
    safety_compliance + offset(log(exposure)),
  ziformula = ~ 1,      # Models a constant probability of structural zeros
  data = filtered.bus.interupt.freq,
  family = nbinom1
)
summary(zinb_model_basic)
anova(nb_model, zinb_model_basic) # win for ZINB, nbinom1 better than 2 
#even with lower AIC BIC as KS test passed

# Check the residuals of the NEW model
library(DHARMa)
res_zinb <- simulateResiduals(zinb_model_basic)
plot(res_zinb)



######################## PART 0B: CLAIM SEVERITY FITTING #######################


# gamma distribution
sev_data <- filtered.bus.interupt.sev %>% filter(claim_amount > 0)

# 2. Fit a Gamma GLM for Severity
#  use a log link to ensure costs stay positive
sev_model <- glm(claim_amount ~ solar_system + production_load + energy_backup_score + 
                   safety_compliance + offset(log(exposure)), 
                 data = filtered.bus.interupt.sev, 
                 family = Gamma(link = "log"))

summary(sev_model)


avg_severity <- mean(predict(sev_model, type = "response"))
print(avg_severity)




########################### PART 3: Simulation #################################

zinb_model_final <- zinb_model_basic
# 1. Create a clean version of the data that matches what the model used
model_data <- na.omit(filtered.bus.interupt.freq[, all.vars(formula(zinb_model_final))])

# 2. Add the predictions to this clean dataset
model_data$predicted_freq <- predict(zinb_model_final, type = "response")

# 3. Now run pricing summary on the data
pricing_summary <- model_data %>%
  group_by(solar_system) %>%
  summarise(
    Total_Exposure = sum(exposure),
    Total_Predicted_Claims = sum(predicted_freq),
    Expected_Frequency = Total_Predicted_Claims / Total_Exposure
  )

print(pricing_summary)

# --- STEP 2: MONTE CARLO SIMULATION ---
set.seed(42) 
n_sims <- 20000
portfolio_losses <- numeric(n_sims)

# Dispersion from  Gamma model output 
gamma_dispersion <- 0.708287 

# Loop to simulate the portfolio 10,000 times
for (i in 1:n_sims) {
  
  # A. Simulate Total Claims for the portfolio (Frequency)
  # This uses the specific risk factors from frequency model
  sim_counts <- simulate(zinb_model_final, nsim = 1)$sim_1
  total_claims <- sum(sim_counts)
  
  # B. Simulate the Cost of those Claims (Severity)
  if (total_claims > 0) {
    # Sample individual claim amounts from a Gamma distribution
    # Shape = 1/Dispersion, Scale = Mean * Dispersion
    shape_param <- 1 / gamma_dispersion
    scale_param <- avg_severity * gamma_dispersion
    
    individual_costs <- rgamma(n = total_claims, shape = shape_param, scale = scale_param)
    portfolio_losses[i] <- sum(individual_costs)
  } else {
    portfolio_losses[i] <- 0
  }
}

# --- STEP 3: EXTRACT RESULTS FOR THE REPORT ---
expected_agg_loss <- mean(portfolio_losses)
var_99 <- quantile(portfolio_losses, 0.99) # 1-in-100 year event
var_99.5 <- quantile(portfolio_losses, 0.995) # solvency II 

max_loss <- max(portfolio_losses)

print(paste("Expected Aggregate Loss (Item 3a): $", round(expected_agg_loss, 2)))
print(paste("1-in-100 Year Stress Test (Item 3b): $", round(var_99, 2)))

# PART 3a: Ruin

c_0 <- var_99 
pi <- 0.05
# --- SURPLUS PROCESS SIMULATION (CRAMER-LUNDBERG) ---
set.seed(2026)

# 1. Define the Parameters from your previous steps
initial_capital <- var_99   # c0: VaR 99% (Solvency Buffer)
annual_premium <- 22981624562    # pi: Annual Returns (with 30% loading)
avg_severity <- 1532208          # Yi: Mean claim cost
annual_freq <- 10500             # N(t): Approx. claims per year from ZINB model

# 2. Simulation Settings
time_horizon <- 10                # 1 year (Short-term vs. Long-term)
n_steps <- 1000                  # Granularity of the simulation

# 3. Generate the "Jumps" (Claims)
# We use a Poisson process for the timing of claims
num_claims <- rpois(1, lambda = annual_freq * time_horizon)
claim_times <- sort(runif(num_claims, 0, time_horizon))
# We use your Gamma parameters for the severity of each jump
claim_sizes <- rgamma(num_claims, shape = 1/0.708287, scale = avg_severity * 0.708287)

# 4. Calculate the Surplus Path
time_points <- seq(0, time_horizon, length.out = n_steps)
surplus <- initial_capital + (annual_premium * time_points)

# Subtract claims as they occur
for(i in 1:num_claims) {
  surplus[time_points >= claim_times[i]] <- surplus[time_points >= claim_times[i]] - claim_sizes[i]
}

# 5. Plot the result for your Pricing Report (Section 3)
plot(time_points, surplus / 1e9, type="l", col="blue", lwd=2,
     main="Galaxy General: 1-Year Surplus Process (Cramer-Lundberg)",
     xlab="Time (Years)", ylab="Surplus (Billions $)",
     ylim=c(min(surplus/1e9) - 1, max(surplus/1e9) + 1))
abline(h = initial_capital / 1e9, col="red", linetype="dashed") # Initial Capital line
legend("bottomright", legend=c("Surplus C(t)", "Initial Capital c0"), 
       col=c("blue", "red"), lty=1:2, cex=0.8)


expected_agg_loss <- mean(portfolio_losses)
var_99 <- quantile(portfolio_losses, 0.99) # 1-in-100 year event
var_99.5 <- quantile(portfolio_losses, 0.995) # solvency II 

max_loss <- max(portfolio_losses)

print(paste("Expected Aggregate Loss (Item 3a): $", round(expected_agg_loss, 2)))
print(paste("1-in-100 Year Stress Test (Item 3b): $", round(var_99, 2)))

# PART 3a: Ruin

c_0 <- var_99 
pi <- 0.05
# --- SURPLUS PROCESS SIMULATION (CRAMER-LUNDBERG) ---
set.seed(2026)

# 1. Define the Parameters from your previous steps
initial_capital <- var_99   # c0: VaR 99% (Solvency Buffer)
annual_premium <- 22981624562    # pi: Annual Returns (with 30% loading)
avg_severity <- 1532208          # Yi: Mean claim cost
annual_freq <- 10500             # N(t): Approx. claims per year from ZINB model

# 2. Simulation Settings
time_horizon <- 10                # 1 year (Short-term vs. Long-term)
n_steps <- 1000                  # Granularity of the simulation

# 3. Generate the "Jumps" (Claims)
# We use a Poisson process for the timing of claims
num_claims <- rpois(1, lambda = annual_freq * time_horizon)
claim_times <- sort(runif(num_claims, 0, time_horizon))
# We use your Gamma parameters for the severity of each jump
claim_sizes <- rgamma(num_claims, shape = 1/0.708287, scale = avg_severity * 0.708287)

# 4. Calculate the Surplus Path
time_points <- seq(0, time_horizon, length.out = n_steps)
surplus <- initial_capital + (annual_premium * time_points)

# Subtract claims as they occur
for(i in 1:num_claims) {
  surplus[time_points >= claim_times[i]] <- surplus[time_points >= claim_times[i]] - claim_sizes[i]
}

# 5. Plot the result for your Pricing Report (Section 3)
plot(time_points, surplus / 1e9, type="l", col="blue", lwd=2,
     main="Galaxy General: 1-Year Surplus Process (Cramer-Lundberg)",
     xlab="Time (Years)", ylab="Surplus (Billions $)",
     ylim=c(min(surplus/1e9) - 1, max(surplus/1e9) + 1))
abline(h = initial_capital / 1e9, col="red", linetype="dashed") # Initial Capital line
legend("bottomright", legend=c("Surplus C(t)", "Initial Capital c0"), 
       col=c("blue", "red"), lty=1:2, cex=0.8)

# --- PART 4: BÜHLMANN CREDIBILITY & COMPETITIVE PREMIUMS ---

# 1. Variance Components for Credibility (K-Factor)
# EVPV: Expected Value of Process Variance (Within-system risk)
# VHM: Variance of Hypothetical Means (Between-system risk)
evpv <- mean(avg_severity^2 * 0.708287)   # Stability of costs within a system
vhm  <- var(predict(sev_model, type="response")) # How much risk varies by system

k_factor <- evpv / vhm
n_years  <- 1 # We use the 1-year data we have
z_factor <- n_years / (n_years + k_factor)

# 2. Competitive Premium Generation (Item 1b & 1c)
# Setting a 22% Loading for market competitiveness
comp_loading <- 0.22 
base_premium <- expected_agg_loss / (1 - comp_loading)
per_unit_base <- base_premium / 97545 # Assuming ~97k systems

# 3. Example Scenarios for the Report Table
# Individual experience examples: 0 claims, 1 average claim, 1 catastrophic claim
safe_system_exp <- 0
avg_system_exp  <- avg_severity
cat_system_exp  <- 1400000 # The $1.4M spike

cred_prem_safe <- (z_factor * safe_system_exp + (1 - z_factor) * avg_severity) / (1-comp_loading)
cred_prem_avg  <- (z_factor * avg_system_exp + (1 - z_factor) * avg_severity) / (1-comp_loading)
cred_prem_cat  <- (z_factor * cat_system_exp + (1 - z_factor) * avg_severity) / (1-comp_loading)

# --- FINAL OUTPUTS FOR REPORT SECTIONS 1, 2, & 3 ---

print("--- ITEM 2: IMPORTANT STATISTICS ---")
print(paste("Expected Value (Portfolio): $", round(expected_agg_loss, 2)))
print(paste("Portfolio Variance: ", round(var(portfolio_losses), 2)))
print(paste("Tail Behavior (1-in-200 / Solvency II): $", round(var_99.5, 2)))
print(paste("Bühlmann Credibility Factor (Z):", round(z_factor, 4)))

print("--- ITEM 1: SHORT & LONG-TERM RANGES ---")
print(paste("Long-Term Target Returns (Premium): $", round(base_premium, 2)))
print(paste("Short-Term Net Revenue (Expected Profit): $", round(base_premium - expected_agg_loss, 2)))
print(paste("Stress-Test Net Revenue (at VaR 99): $", round(base_premium - var_99, 2)))

print("--- COMPETITIVE PREMIUM EXAMPLES (BÜHLMANN) ---")
print(paste("Safe System (0 claims): $", round(cred_prem_safe / 97545, 2)))
print(paste("Average System: $", round(cred_prem_avg / 97545, 2)))
print(paste("Catastrophic System ($1.4M hit): $", round(cred_prem_cat / 97545, 2)))



#best case scenario
best_case_10pct <- quantile(portfolio_losses, 0.10)
smooth_ops_mean <- mean(portfolio_losses[portfolio_losses < best_case_10pct])

# --- PART 8: TAIL RISK SIMULATION (VaR & ES 99%) ---

set.seed(42)
n_sims <- 100000 # High simulation count for stable tail metrics

# 1. Prepare Annual Frequency Parameters
# Your model was trained on 10-year data, so we annualize the predictions
annual_pred_freq <- predict(zinb_model_final, type = "response") / 10
total_annual_lambda <- sum(annual_pred_freq)

# 2. Prepare Severity Parameters (from your Gamma GLM)
# avg_severity is the mean; gamma_dispersion is the 1/shape
shape_param <- 1 / 0.708287 
scale_param <- avg_severity * 0.708287

# --- 1-YEAR SIMULATION ---
sim_1yr_losses <- replicate(n_sims, {
  # Simulate total claims for the fleet this year (Poisson approx for aggregate)
  n_claims <- rpois(1, lambda = total_annual_lambda)
  if(n_claims > 0) {
    sum(rgamma(n_claims, shape = shape_param, scale = scale_param))
  } else { 0 }
})

# --- 10-YEAR SIMULATION ---
sim_10yr_losses <- replicate(n_sims, {
  # Total claims over 10 years = sum of 10 annual lambdas
  n_claims <- rpois(1, lambda = total_annual_lambda * 10)
  if(n_claims > 0) {
    sum(rgamma(n_claims, shape = shape_param, scale = scale_param))
  } else { 0 }
})

# 3. Helper Function for Expected Shortfall (ES)
calc_es <- function(losses, alpha) {
  var_val <- quantile(losses, alpha)
  mean(losses[losses >= var_val])
}

# 4. Extract Final Metrics (Convert to ÐM)
results_tail <- data.frame(
  Metric = c("VaR 99%", "ES 99%"),
  Annual_DM = c(quantile(sim_1yr_losses, 0.99) / 1e7, calc_es(sim_1yr_losses, 0.99) / 1e7),
  LongTerm_DM = c(quantile(sim_10yr_losses, 0.99) / 1e7, calc_es(sim_10yr_losses, 0.99) / 1e7),
 test = c(quantile(sim_1yr_losses, 0.95) / 1e7)
)


print(results_tail)






