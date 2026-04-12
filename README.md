
# 2026 SOA Case Study: The Pricing Frontier
Team: 4001 Crusaders

Members: Niklesh Anantha-Siva, Armaan Banga, Toby Mufford, Huy Pham, Vatsal Sharma

## Project Summary

The 4001 Crusaders actuarial team at Galaxy General Insurance Company (GGIC) have been given the opportunity to develop an insurance portfolio for Cosmic Quarry Mining Corporation (CQMC). The proposed insurance products account for the risks associated with CQMC's operations which are reflected in each of the hazards covered:

| Hazard | Annual Premium | Expected Loss | Loss Ratio | Net Revenue |
|---|---|---|---|---|
| Equipment Failure | Đ44.61M | Đ26.73M | 59.99% | Đ15.88M |
| Cargo Loss | Đ660.07M | Đ519.60M | 78.72% | Đ107.46M |
| Workers' Compensation | Đ17.01M | Đ8.82M | 51.80% | Đ3.26M |
| Business Interruption | Đ206.25M | Đ160.88M | 78.00% | Đ46.37M |

The following page will navigate each of the 4 hazards separately, exploring how the unique risks and challenges associated with each of them influenced our modelling choices and premium calculation. 

## Cargo Loss
### Data Exploration
Cargo loss presents an extremely challenging and risky hazard from both a frequency and severity perspective. Historical shipment claims reveal that 18.23% of shipments resulted in at least 1 claim with claim counts ranging up to 5 per shipment. This is coupled with the high claim severity, with the average and median claim loss being Đ7.8M and Đ382k respectively. The histogram of claim size severity also illustrates how it has a highly skewed bimodal distribution. 

![My plot](cargo_loss/bimodal_cargo_loss.png)

The most notable covariate influencing claim severity size was cargo type, with gold and platinum cargo shipments specifically having a disproportionate impact on the higher claim sizes relative to other cargo types. The plot below reveals the average claim size of gold and platinum shipments were Đ10M and Đ43M respectively, much larger than the remaining cargo types whose average is less than Đ1M. A further analysis into the tail risk reveals that 88.5% of the 5% largest claims were gold cargo shipments despite all cargo types having an equal distribution. Platinum was the other cargo type making up the remaining 11.4%.

![My plot](cargo_loss/cargo_type_cargo_loss.png)

### Product Offering

In light of these discoveries, our modelling and subsequent product design were carefully selected to control these tail risks and uncertainties.
Our coverage benefit structure thus incorporated a Đ100k deductible to prevent attritional claims from inflating premiums. The policy limit was set at Đ1M per shipment to truncate the maximum payout, reducing the impact of very extreme losses. This still provides substantial coverage with 75% of the predicted claims falling within this limit. Without these strict controls, the severity profile appears too extreme for an affordable and sustainable product.
Exclusions included
*Cargo shipments carrying gold are excluded from cover. This was necessary given the unavailability of the gold extraction forecast plans which created too much pricing uncertainty given its disproportionately higher claim size
*Platinum is covered up to a 50,000kg weight cap given its strong association with higher claim severity but to a much lesser extent than gold 
*Each insured vessel is subject to a maximum number of insured shipments by system. Helionis system vessels can be insured for their first 4 shipments, Bayesia vessels for their first 3, and Oryn Delta vessels for their first 2. This controlled the size of the portfolio whilst still meeting the volume needs of cosmic mining corporations expansionary mining plans 
*Cargo shipments exceeding the recommended maximum cargo weight are excluded  


### Modelling
The modelling procedure involved first utilising a GLM model to predict claim severity and frequency. Frequency was modelled using a poison distributed log-link GLM keeping only significant variables at a 5% significance level. Regarding the severity model, it was realised that a singular GLM model would fail to capture the bimodal distribution that we observed. Thus, a 2 layered GLM model was chosen consisting of 3 GLM models. The first was a classifier GLM model which predicted whether a covariate profile would result in a big or small claim. Đ1.8M was the threshold chosen to split the claim sizes based on the previously observed graph to effectively split the claim size distribution into unimodal distributions. Then, 2 separate GLM’s trained on small and large claim sizes respectively were used to predict claim severity. Based on a train-test split which split 80% of the data into a training dataset and 20% into a testing, it was revealed that this 2 layered GLM approach was superior, seeing an MSE reduction of 650k relative to a single GLM model.

```r
cargo_freq_model<-glm(claim_count~cargo_type+route_risk+pilot_experience+container_type,data=table_cargo_freq,offset=log(exposure),family=poisson(link="log"))

cargo_classifier_model<-glm(claim_relativity~cargo_type+weight+route_risk+solar_radiation,data=table_cargo_sev,family=binomial)

table_cargo_sev_small<-table_cargo_sev %>%
  filter(claim_relativity==0)

table_cargo_sev_large<-table_cargo_sev %>%
  filter(claim_relativity==1)


cargo_sev_small_model<-glm(claim_amount~cargo_type+weight+route_risk+solar_radiation+debris_density,data=table_cargo_sev_small,family=Gamma(link="log"))
sev_dispersion_small<-summary(cargo_sev_small_model)$dispersion

cargo_sev_large_model<-glm(claim_amount~cargo_type+route_risk+solar_radiation,data=table_cargo_sev_large,family=Gamma(link="log"))
sev_dispersion_large<-summary(cargo_sev_large_model)$dispersion
```



Once the GLM models were built, 10000 Monte-Carlo simulations were used to estimate the portfolio claim size. Firstly a synthetic shipment portfolio dataset utilising the limited fleet dataset we had. We modelled the number of shipments as a binomial random variable with parameters chosen to be in line with the maximum number of shipments allowed by the policy and the expected shipments to be equal to the number of shipments needed in each solar system to meet the CQMC long-term mining production goals. Covariate based data was sampled from the historical dataset with slight assumption changes being made based off the qualitative descriptions we had regarding solar systems attributes. For example, regarding how we sampled the route risk covariate, we utilised the baseline probabilities implied by the dataset and then increased probabilities of higher route risks for solar systems like Oryn to reflect its risk associated with fluctuating gravitational gradients.

```r
route_risk_draw<-function(solar_system, length){
  if(solar_system=="Helionis Cluster") {
    return(sample(c(1,2,3,4,5),length, replace=TRUE, prob=c(0.10-0.05,0.2-0.05,0.4-0.1,0.2+0.1,0.1+0.1)))}
  if(solar_system=="Bayesia") {
    return(sample(c(1,2,3,4,5),length, replace=TRUE, prob=c(0.1+0.1,0.2+0.05,0.4,0.2-0.1,0.1-0.05)))}
  if(solar_system=="Oryn Delta") {
    return(sample(c(1,2,3,4,5),length, replace=TRUE, prob=c(0.1-0.05,0.2-0.05,0.4-0.15,0.2+0.1,0.1+0.15)))}}

```


Once the shipment portfolio was created, simulations were run utilising the poison and gamma distribution implied by our frequency and severity GLM modelling. This allowed claims to follow a distribution based on their covariate compositions as opposed to fitting a gamma distribution on the overall claim severity profile which would fail to differentiate the covariate risk.

```r
n_sim <- 10000
n_rows <- nrow(practice_data_freq_helionis)
simulated_total_short_helionis <- numeric(n_sim)

simulated_total_short_helionis_deepspace   <- numeric(n_sim)
simulated_total_short_helionis_dockarc     <- numeric(n_sim)
simulated_total_short_helionis_hardseal    <- numeric(n_sim)
simulated_total_short_helionis_longhaul    <- numeric(n_sim)
simulated_total_short_helionis_quantumcrate<- numeric(n_sim)

for (k in seq_len(n_sim)) {
  number_claims <- rpois(n_rows, practice_data_freq_helionis$lambda)
  idx <- which(number_claims > 0)
  if (length(idx) > 0) {
    shapes <- rep(practice_data_freq_helionis$shape[idx], number_claims[idx])
    scales <- rep(practice_data_freq_helionis$scale[idx], number_claims[idx])
    container <- rep(practice_data_freq_helionis$container_type[idx], number_claims[idx])
    claim_size <- rgamma(length(shapes), shape = shapes, scale = scales)
    claim_size_limit <- pmax(0, pmin(claim_size, 1e6) - 1e5)   
    simulated_total_short_helionis[k] <- sum(claim_size_limit)
    simulated_total_short_helionis_deepspace[k] <-
      sum(claim_size_limit[container == "DeepSpace Haulbox"])
    simulated_total_short_helionis_dockarc[k] <-
      sum(claim_size_limit[container == "DockArc Freight Case"])
    simulated_total_short_helionis_hardseal[k] <-
      sum(claim_size_limit[container == "HardSeal Transit Crate"])
    simulated_total_short_helionis_longhaul[k] <-
      sum(claim_size_limit[container == "LongHaul Vault Canister"])
    simulated_total_short_helionis_quantumcrate[k] <-
      sum(claim_size_limit[container == "QuantumCrate Module"])
  }
}

```
### Premium
Our premium derivation started by calculating the pure premium which was the expected loss in our simulation which was then adjusted using buhlmann credibility which blended the portfolio experience with container type level data to stabilise our premium estimates. We also added stress loading and volatility loading factors to ensure our premium reflects uncertainty and risk associated with our simulations and assumptions. Cost of capital on our reserves and operational expense were further added on, leading to our final premium of Đ660M for coverage across all 3 solar systems which on average produced a Đ107M return.

| Metric | Helionis | Bayesia | Oryn | Total |
|---|---:|---:|---:|---:|
| Vessels insured | 1,160 | 1,128 | 774 | 3,062 |
| Pure premium | Đ286,026,916 | Đ152,013,507 | Đ81,563,628 | Đ519,604,051 |
| Buhlmann credibility premium | Đ283,213,208 | Đ150,635,565 | Đ80,125,874 | Đ513,974,647 |
| Stress loading | Đ309,720,579 | Đ174,939,198 | Đ91,074,367 | Đ575,734,144 |
| PV discounting of claims | Đ307,504,089 | Đ173,687,260 | Đ90,422,601 | Đ571,613,950 |
| Volatility loading | Đ329,054,526 | Đ189,506,772 | Đ101,908,464 | Đ620,469,762 |
| Cost of capital | Đ332,004,766 | Đ191,612,802 | Đ103,445,634 | Đ627,063,202 |
| Operational expenses | Đ349,478,701 | Đ201,697,686 | Đ108,890,141 | Đ660,066,528 |
| Final premium | Đ349,478,701 | Đ201,697,686 | Đ108,890,141 | Đ660,066,528 |
| Premium per vessel | Đ301,275 | Đ178,810 | Đ140,685 | Đ215,567 |
| Loss ratio | 81.8% | 75.4% | 74.9% | 78.7% |

## Business Interruption
### Data Exploration
Business Interruption (BI) presents a low-frequency, high-severity risk profile, characterised by significant tail-risk despite a high volume of non-claims.  Claim frequency is defined by a significant zero-inflation with mean of 0.10 against a variance of 0.17, where the massive peak at zero renders standard Poisson distributions inadequate and necessitates either a hurdle model or zero-inflated model approach. On the severity side, the average claim amount is approximately $1,174,300; the distribution is heavily negatively skewed with a pronounced peak at the upper policy threshold.
![Frequency Plot](bus_interruption/Frequency.png)
![Severity Plot](bus_interruption/Severity.png)
The most significant operational drivers appear to be the energy backup score, supply chain index, and maintenance frequency consistently emerge as significant predictors across initial modelling testing, indicating that these variables are the primary levers for managing risk volatility. A comparative segment analysis reveals a distinction in risk exposure between the analysed regions. Epsilon and Zeta exhibit nearly identical frequency and severity profiles, whereas the Helionis Cluster (HC) serves as a low-risk outlier, recording roughly 50% the total claim volume and frequency of its counterparts. This favourable performance in HC is correlated with superior operational efficiency, including the highest energy system efficiency, unit sourcing proportions, and compliance scores, even though it reports lower crew experience and maintenance turnover. While the overall company operating margin remains resilient at 31%, the data suggests that while HC benefits from lower exposure levels, the lack of a clear severity pattern across systems implies that extreme loss events remain a latent threat.

