##### FREQUENCY MODELLING #####

library(glmmTMB)
# Fitting ZINB with constant zero-inflation (ziformula = ~ 1)
zinb_model_basic <- glmmTMB(
  claim_count ~ solar_system + production_load + energy_backup_score + 
    supply_chain_index + avg_crew_exp + maintenance_freq + 
    safety_compliance + offset(log(exposure)),
  ziformula = ~ 1,      
  data = filtered.bus.interupt.freq,
  family = nbinom1 # nbinom1 was selected over nbinom2 based on AIC/BIC
)
summary(zinb_model_basic)

### SEVERITY MODELLING ###
# Pre-processing: Bounding claims by deductible and policy limit
filtered.bus.interupt.sev <- bus.interupt.sev %>%
  mutate(claim_amount = pmax(28000, pmin(claim_amount, 1426000))) %>%
  filter(claim_amount > 0)

# Fitting Gamma GLM for Severity
sev_model <- glm(claim_amount ~ solar_system + production_load + 
                   energy_backup_score + safety_compliance + offset(log(exposure)), 
                 data = filtered.bus.interupt.sev, 
                 family = Gamma(link = "log"))

avg_severity <- mean(predict(sev_model, type = "response"))

##### SIMULATIONS #####
set.seed(42) 
n_sims <- 20000
portfolio_losses <- numeric(n_sims)
gamma_dispersion <- 0.708287 

for (i in 1:n_sims) {
  # 1. Simulate frequency from ZINB
  sim_counts <- simulate(zinb_model_basic, nsim = 1)$sim_1
  total_claims <- sum(sim_counts)
  
  # 2. Simulate severity from Gamma
  if (total_claims > 0) {
    shape_param <- 1 / gamma_dispersion
    scale_param <- avg_severity * gamma_dispersion
    
    # Generate costs and sum for aggregate loss
    individual_costs <- rgamma(n = total_claims, shape = shape_param, scale = scale_param)
    portfolio_losses[i] <- sum(individual_costs)
  } else {
    portfolio_losses[i] <- 0
  }
}

# Extracting key metrics
expected_agg_loss <- mean(portfolio_losses)
var_99 <- quantile(portfolio_losses, 0.99)


##### PREMIUM PROJECTION #####
# 1. Variance Components for Credibility 
evpv <- mean(avg_severity^2 * 0.708287) 
vhm  <- var(predict(sev_model, type="response")) 

k_factor <- evpv / vhm
n_years  <- 1 
z_factor <- n_years / (n_years + k_factor)

# 2. Competitive Premium Generation (incorporating 22% Loading)
comp_loading <- 0.22 
base_premium <- expected_agg_loss / (1 - comp_loading)

# 3. Example Scenarios providing the "Paradox" logic
# Individual experience: 0 claims, 1 average claim, 1 catastrophic claim ($1.4M limit hit)
safe_system_exp <- 0
avg_system_exp  <- avg_severity
cat_system_exp  <- 1400000 # Capped near the $1.426M limit

# Applying Buhlmann Credibility Formula: [Z * Experience + (1-Z) * Portfolio Mean] / (1 - Loading)
cred_prem_safe <- (z_factor * safe_system_exp + (1 - z_factor) * avg_severity) / (1-comp_loading)
cred_prem_avg  <- (z_factor * avg_system_exp + (1 - z_factor) * avg_severity) / (1-comp_loading)
cred_prem_cat  <- (z_factor * cat_system_exp + (1 - z_factor) * avg_severity) / (1-comp_loading)

# Outputting per-unit results to demonstrate the limit-censoring effect
print(paste("Average System Premium:", round(cred_prem_avg / 97545, 2)))
print(paste("Catastrophic System Premium:", round(cred_prem_cat / 97545, 2)))




