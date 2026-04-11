#Cargo loss code

#-----------------------------------------EDA and data cleaning---------------------------------------#


#libraries and data loading

setwd("~/university/Actuary/actl 4001/Assignment files")
library(dplyr)
library(readxl)
library(ggplot2)
library(tidyverse)
library(randomForest)
table_cargo_sev <- read_excel("srcsc-2026-claims-cargo.xlsx", sheet = "sev")
table_cargo_freq<-read_excel("srcsc-2026-claims-cargo.xlsx", sheet = "freq")


str(table_cargo_sev)
summary(table_cargo_sev)


str(table_cargo_freq)
summary(table_cargo_freq)



#removing NA's and other incorrect datapoitn and converting to correct data type
table_cargo_sev<-na.omit(table_cargo_sev)
table_cargo_freq<-na.omit(table_cargo_freq)



large_claim_amount<-table_cargo_sev %>%
  arrange(desc(claim_amount)) %>%
  select(-c(pilot_experience, shipment_id, claim_seq))







# % of entries outside the stipulated range- results show not many values would be taken out hence why removal of non-sensible values were removed rather than imputed
mean(table_cargo_sev$claim_amount>900000000)
mean(table_cargo_sev$weight>250000)
mean(table_cargo_sev$distance>100)
mean(table_cargo_sev$transit_duration>60)
mean(table_cargo_sev$pilot_experience>30)
mean(table_cargo_sev$vessel_age>50)
mean(table_cargo_sev$solar_radiation>1)
mean(table_cargo_sev$debris_density>1)
mean(table_cargo_sev$exposure>1)



#removing negative values (removes 458 entries)
table_cargo_sev <- table_cargo_sev %>%
  filter(route_risk %in% 1:5,cargo_value>0,weight>=0,distance>=0, transit_duration>=0, pilot_experience>=0, vessel_age>0, solar_radiation>=0,debris_density>=0, claim_amount>0)


table_cargo_freq<-table_cargo_freq %>%
  filter(route_risk %in% 1:5,cargo_value>0,weight>=0,distance>=0, transit_duration>=0, pilot_experience>=0, vessel_age>0, solar_radiation>=0,debris_density>=0, exposure>0,claim_count>=0,claim_count<=5)



#aligning consistency in names (removing the things after the underscore- this code was built with assistance of chatgpt)

table_cargo_sev <- table_cargo_sev %>%
  mutate(across(where(is.character), ~ sub("_.*", "", .)))

table_cargo_freq <- table_cargo_freq %>%
  mutate(across(where(is.character), ~ sub("_.*", "", .)))


unique(table_cargo_sev$cargo_type)
unique(table_cargo_freq$cargo_type)





#converting character variable to factors
factor_columns<-c("route_risk", "claim_id","policy_id", "shipment_id", "cargo_type", "container_type")
factor_columns_2<-c("route_risk", "policy_id", "shipment_id", "cargo_type", "container_type")
table_cargo_sev[factor_columns] <- lapply(table_cargo_sev[factor_columns], as.factor)
table_cargo_freq[factor_columns_2] <- lapply(table_cargo_freq[factor_columns_2], as.factor)


summary(table_cargo_sev)


#further filtering data for only sensible values
table_cargo_sev<-table_cargo_sev %>%
  filter(distance<=100, transit_duration<=60,vessel_age<=50,solar_radiation<=1, debris_density<=1,pilot_experience<=30,weight<=250000,cargo_value<=10^9, claim_amount<=8*10^8)



table_cargo_freq<-table_cargo_freq %>%
  filter(distance<=100, transit_duration<=60,vessel_age<=50,solar_radiation<=1, debris_density<=1,exposure<=1,pilot_experience<=30,weight<=250000,cargo_value<=8*10^8)



#checking
str(table_cargo_freq)
summary(table_cargo_freq)


str(table_cargo_sev)
summary(table_cargo_sev)






#histogram
ggplot(table_cargo_sev, aes(x = claim_amount)) +
  geom_histogram(bins = 30, fill = "blue") +
  labs(title = "Claim Severity Distribution", x = "Claim Size",y = "Frequency") 

#histogram on log scale 
ggplot(table_cargo_sev, aes(x = claim_amount)) +
  geom_histogram(bins = 30, fill = "steelblue", colour="white") +
  scale_x_log10() +
  labs(title = "Claim Severity distribution on log scale", x = "Claim Size", y = "Frequency")+
  geom_vline(xintercept=10^6,color="red",linetype="dotted")


#container type barplot
table_cargo_sev %>%
  group_by(cargo_type)%>%
  summarise(avg_claim=mean(claim_amount))%>%
  ggplot()+
  aes(x=cargo_type)+
  aes(y=avg_claim)+
  geom_col()



#drivers of tail
#Value at risk and expected shortfall
tail_event_VAR<-quantile(table_cargo_sev$claim_amount,0.95)
tail_event_ES<-mean(table_cargo_sev$claim_amount[table_cargo_sev$claim_amount>=tail_event_VAR])


table_cargo_sev_tail<-table_cargo_sev%>%
  mutate(tail_0.95=claim_amount>tail_event_VAR)
summary(table_cargo_sev_tail)

table_new<-table_cargo_sev %>%
  filter(weight<=250000,cargo_type!="gold")

quantile(table_new$claim_amount, probs = seq(0.1, 0.9, by = 0.1))

table_cargo_sev_tail<-table_cargo_sev%>%
  mutate(tail_0.95=claim_amount>tail_event_VAR)


factor_variables<-c("route_risk", "cargo_type", "container_type")
for (i in factor_variables){
  x<-table_cargo_sev_tail %>%
    group_by(.data[[i]]) %>%
    summarise(total_count=n(),tail_count=sum(tail_0.95), tail_proportion=sum(tail_0.95)/n())
  print(x)
}

# we see gold cargo is the one associated with the high tail risk with 1/3 of the worse 5% events being associated with it



#correlation graphs 
table_cargo_sev_tibble<-as_tibble(table_cargo_sev)
table_cargo_sev_tibble

ggplot(data=table_cargo_sev_tibble)+
  aes(x=cargo_value)+
  aes(y=claim_amount)+
  geom_point()

cor(table_cargo_sev$cargo_value, table_cargo_sev$claim_amount)





#understanding dataset better

quantile(table_cargo_sev$claim_amount, probs = seq(0.1, 0.9, by = 0.1))

table_cargo_sev %>%
  filter(cargo_type!="gold") %>%
  summarise(deciles=quantile(claim_amount,probs = seq(0.1, 0.9, by = 0.1)))

table_cargo_freq%>%
  group_by(claim_count)%>%
  summarise(count=n(),prob=n()/nrow(table_cargo_freq))



#---------------------------------Modelling frequency and severity using train test partition-------------------------------------------------#

set.seed(3)

test_index_freq<-sample(1:nrow(table_cargo_freq),0.2*nrow(table_cargo_freq)) #this randomly chooses 20% of the positions of the dataset
val_data_freq<-table_cargo_freq[test_index_freq,]%>%
  select(-c("policy_id","shipment_id"))
train_data_freq<-table_cargo_freq[-test_index_freq,]%>%
  select(-c("policy_id","shipment_id"))





#adding a claim relativitry virbale which is 1 if it is above 1.8 million
table_cargo_sev<-table_cargo_sev %>%
  mutate(claim_relativity=as.numeric(claim_amount>1.8*10^6))


test_index_sev<-sample(1:nrow(table_cargo_sev),0.2*nrow(table_cargo_sev)) #this randomly chooses 20% of the positions of the dataset
val_data_sev<-table_cargo_sev[test_index_sev,]%>%
  select(-c("claim_id","claim_seq","policy_id","shipment_id"))  #we use the randomly chosen rows as our test dataset
train_data_sev<-table_cargo_sev[-test_index_sev,]%>%
  select(-c("claim_id","claim_seq","policy_id","shipment_id"))



#training
#linear 
#freq
lm_naive_model_freq<-lm(claim_count/exposure~cargo_type+cargo_value+weight+route_risk+distance+transit_duration+pilot_experience+vessel_age+container_type+solar_radiation+debris_density,data=train_data_freq)
summary(lm_naive_model_freq)


#sev
lm_naive_model_sev<-lm(claim_amount~cargo_type+cargo_value+weight+route_risk+distance+transit_duration+pilot_experience+vessel_age+container_type+solar_radiation+debris_density,data=train_data_sev)
summary(lm_naive_model_sev)





#GLM full
#freq
glm_naive_model_freq<-glm(claim_count~cargo_type+cargo_value+weight+route_risk+distance+transit_duration+pilot_experience+vessel_age+container_type+solar_radiation+debris_density,data=train_data_freq,offset=log(exposure),family=poisson(link="log"))
summary(glm_naive_model_freq)


#sev
glm_naive_model_sev<-glm(claim_amount~cargo_type+cargo_value+weight+route_risk+distance+transit_duration+pilot_experience+vessel_age+container_type+solar_radiation+debris_density,data=train_data_sev,family=Gamma(link="log"))
summary(glm_naive_model_sev)




#GLM SIGNIFICANT
glm_sig_model_freq<-glm(claim_count~route_risk+pilot_experience+container_type+solar_radiation+debris_density,data=train_data_freq,offset=log(exposure),family=poisson(link="log"))
summary(glm_sig_model_freq)


glm_sig_model_sev<-glm(claim_amount~cargo_type+cargo_value+weight+route_risk+solar_radiation+debris_density,data=train_data_sev,family=Gamma(link="log"))
summary(glm_sig_model_sev)



#log glm 
glm_log_naive_model_sev<-glm(log(claim_amount)~cargo_type+cargo_value+weight+route_risk+distance+transit_duration+pilot_experience+vessel_age+container_type+solar_radiation+debris_density,data=train_data_sev,family=gaussian())
summary(glm_log_naive_model_sev)





#2 part model
#size determination model
glm_size<-glm(claim_relativity~cargo_type+cargo_value+weight+route_risk+distance+transit_duration+pilot_experience+vessel_age+container_type+solar_radiation+debris_density,data=train_data_sev,family=binomial)
summary(glm_size)

train_data_sev_small<-train_data_sev %>%
  filter(claim_relativity==0)
train_data_sev_large<-train_data_sev %>%
  filter(claim_relativity==1)



#small claim model
glm_small<-glm(claim_amount~cargo_type+cargo_value+weight+route_risk+distance+transit_duration+pilot_experience+vessel_age+container_type+solar_radiation+debris_density,data=train_data_sev_small,family=Gamma(link="log"))
summary(glm_small)


#large claim model
glm_large<-glm(claim_amount~cargo_type+cargo_value+weight+route_risk+distance+transit_duration+pilot_experience+vessel_age+container_type+solar_radiation+debris_density,data=train_data_sev_large,family=Gamma(link="log"))

summary(glm_large)




#significnatr predictors 2 part model
glm_size_sig<-glm(claim_relativity~cargo_type+cargo_value+weight+route_risk+solar_radiation+debris_density,data=train_data_sev,family=binomial)
glm_small_sig<-glm(claim_amount~cargo_type+weight+route_risk+solar_radiation+debris_density,data=train_data_sev_small,family=Gamma(link="log"))
glm_large_sig<-glm(claim_amount~cargo_type+cargo_value+route_risk+solar_radiation,data=train_data_sev_large,family=Gamma(link="log"))





#testing
lm_freq_pred<-predict(lm_naive_model_freq,newdata=val_data_freq)
lm_sev_pred<-predict(lm_naive_model_sev, newdata=val_data_sev)
glm_freq_pred<-predict(glm_naive_model_freq, newdata=val_data_freq, type="response")
glm_sev_pred<-predict(glm_naive_model_sev, newdata=val_data_sev, type="response")
glm_sig_freq_pred<-predict(glm_sig_model_freq, newdata=val_data_freq, type="response")
glm_sig_sev_pred<-predict(glm_sig_model_sev, newdata=val_data_sev, type="response")











#RMSE
sqrt(mean((lm_freq_pred-val_data_freq$claim_count)^2))
sqrt(mean((lm_sev_pred-val_data_sev$claim_amount)^2))

sqrt(mean((glm_freq_pred-val_data_freq$claim_count)^2))
sqrt(mean((glm_sev_pred-val_data_sev$claim_amount)^2))

sqrt(mean((glm_sig_freq_pred-val_data_freq$claim_count)^2))
sqrt(mean((glm_sig_sev_pred-val_data_sev$claim_amount)^2))











#2 part model testing (naive)
#checking classification model on its own
glm_size_pred<-predict(glm_size, newdata=val_data_sev,type="response")
size_pred<-ifelse(glm_size_pred>0.5,1,0)
size_actual<-val_data_sev$claim_relativity
accuracy<-mean(size_actual==size_pred)


#overall claim prediction
val_data_sev<-val_data_sev %>%
  mutate(predict_prob=predict(glm_size, newdata=val_data_sev,type="response"),
         size_pred=ifelse(predict_prob>0.5,1,0))

overall_pred <- rep(NA, nrow(val_data_sev))

small_idx <- which(val_data_sev$size_pred == 0)
large_idx <- which(val_data_sev$size_pred == 1)

overall_pred[small_idx] <- predict(glm_small,
                                   newdata = val_data_sev[small_idx, ],
                                   type = "response")

overall_pred[large_idx] <- predict(glm_large,
                                   newdata = val_data_sev[large_idx, ],
                                   type = "response")



sqrt(mean((overall_pred-val_data_sev$claim_amount)^2))



#2 part model testing (signinfcant)
glm_size_pred<-predict(glm_size_sig, newdata=val_data_sev,type="response")
size_pred<-ifelse(glm_size_pred>0.5,1,0)
size_actual<-val_data_sev$claim_relativity
accuracy<-mean(size_actual==size_pred)


#overall claim prediction
val_data_sev<-val_data_sev %>%
  mutate(predict_prob=predict(glm_size_sig, newdata=val_data_sev,type="response"),
         size_pred=ifelse(predict_prob>0.5,1,0))

overall_pred <- rep(NA, nrow(val_data_sev))

small_idx <- which(val_data_sev$size_pred == 0)
large_idx <- which(val_data_sev$size_pred == 1)

overall_pred[small_idx] <- predict(glm_small_sig,
                                   newdata = val_data_sev[small_idx, ],
                                   type = "response")

overall_pred[large_idx] <- predict(glm_large_sig,
                                   newdata = val_data_sev[large_idx, ],
                                   type = "response")



sqrt(mean((overall_pred-val_data_sev$claim_amount)^2))







#significant predictors 2 part model- without value predictor (worse results but better interpretability)
glm_size_sig<-glm(claim_relativity~cargo_type+weight+route_risk+solar_radiation+debris_density,data=train_data_sev,family=binomial)
glm_small_sig<-glm(claim_amount~cargo_type+weight+route_risk+solar_radiation+debris_density,data=train_data_sev_small,family=Gamma(link="log"))
glm_large_sig<-glm(claim_amount~cargo_type+route_risk+solar_radiation,data=train_data_sev_large,family=Gamma(link="log"))

glm_size_pred<-predict(glm_size_sig, newdata=val_data_sev,type="response")
size_pred<-ifelse(glm_size_pred>0.5,1,0)
size_actual<-val_data_sev$claim_relativity
accuracy<-mean(size_actual==size_pred)


#overall claim prediction
val_data_sev<-val_data_sev %>%
  mutate(predict_prob=predict(glm_size_sig, newdata=val_data_sev,type="response"),
         size_pred=ifelse(predict_prob>0.5,1,0))

overall_pred <- rep(NA, nrow(val_data_sev))

small_idx <- which(val_data_sev$size_pred == 0)
large_idx <- which(val_data_sev$size_pred == 1)

overall_pred[small_idx] <- predict(glm_small_sig,
                                   newdata = val_data_sev[small_idx, ],
                                   type = "response")

overall_pred[large_idx] <- predict(glm_large_sig,
                                   newdata = val_data_sev[large_idx, ],
                                   type = "response")



sqrt(mean((overall_pred-val_data_sev$claim_amount)^2))






#---------------------------------------Simulations using chosen model (which is the 2 layered severity model)-----------------------------------------------------#

# Building GLM models on full dataset

glm_sev_interpretation<-glm(claim_amount~cargo_type+weight+route_risk+solar_radiation+debris_density,data=table_cargo_sev,family=Gamma(link="log"))
summary(glm_sev_interpretation)


View(table_cargo_sev)
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


summary(cargo_classifier_model)
summary(cargo_sev_small_model)
summary(cargo_sev_large_model)




#Summary statistics determining whether we see any trends between cargo type and other covariates (inputs for modelling)
table_cargo_freq %>%
  group_by(cargo_type) %>%
  summarise(number=length(cargo_type)) %>%
  print(n=50)
#cargo type equally distributed 




table_cargo_freq %>%
  group_by(container_type) %>%
  summarise(min=min(weight)) %>%
  print(n=50)
  #results shows cargo freq is pretty equally distributed among container type entries 


table_cargo_freq %>%
  group_by(container_type, cargo_type) %>%
  summarise(number=length(cargo_type)) %>%
  print(n=50)



table_cargo_freq %>%
  group_by(container_type) %>%
  summarise(mean=mean(cargo_value)) %>%
  print(n=50)


table_cargo_freq%>%
  group_by(route_risk) %>%
  summarise(no.=n()/nrow(table_cargo_freq))


quantile(table_cargo_freq$debris_density,0.35)

quantile(table_cargo_freq$solar_radiation,0.35)


summary(cargo_sev_large_model)
summary(cargo_sev_small_model)
summary(cargo_freq_model)
summary(cargo_classifier_model)






#modelling inflation and interest data
#forecasting inflation and itnerest rates- still work in progress
table_external<-read_excel("srcsc-2026-interest-and-inflation.xlsx",skip=2) # the skip=2 makes sure that it only read starting form line 3 since the tbale only starts on row 3
head(table_external)

table_external<-table_external %>%
  mutate(real_interest=`1-Year Risk Free Annual Spot Rate`-`Inflation`)



mean_return=mean(table_external$real_interest)
lag_return <- table_external$real_interest[-length(table_external$real_interest)]          
current_return <- table_external$real_interest[-1]                 

A <- sum((lag_return - mean_return) * (current_return - mean_return)) / sum((lag_return - mean_return)^2)
A


future_real<-rep(0,10)
future_real[1]<-A*table_external$real_interest[length(table_external$real_interest)]+(1-A)*mean_return
for (j in 2:10) {
  future_real[j]=A*future_real[j-1]+(1-A)*mean_return
}

future_inflation<-pmax(rnorm(10,mean=0.025,sd=0.019),0.0022)

future_interest<-pmax(future_real+future_inflation,0.0015)


future_interest
#here, interest represents the cumulative interest rate at the middle of the year (e.g. so interest[2] is cumulative interest rate at time 1.5 (half way in year 2))
interest<-rep(0,10)
interest[1]<-(1+future_interest[1])^0.5
for (i in 2:10) {
  interest[i]<-((future_interest[i-1]+1)^0.5)*(interest[i-1])*((1+future_interest[i])^0.5)
}




#here, inflation represents the accumulated inflation rate that should be applied. we assume the inflation take place at end of year so 2175 will take same price as 2174, only in 1 years time (beginning of 2176 data) will inflation hit
inflation<-rep(0,10)
inflation[1]<-1
for (i in 2:10) {
  inflation[i]<-inflation[i-1]*(1+future_inflation[i-1])
}





#developing fleet data for synthetic dataset
solar_system<-c(rep("Helionis Cluster",58),rep("Bayesia",56),rep("Oryn Delta",39),rep("Helionis Cluster",116),rep("Bayesia",113),rep("Oryn Delta",77),rep("Helionis Cluster",580),rep("Bayesia",564),rep("Oryn Delta",387),rep("Helionis Cluster",232),rep("Bayesia",226),rep("Oryn Delta",155),rep("Helionis Cluster",174),rep("Bayesia",169),rep("Oryn Delta",116))
container_type<-c(rep("DeepSpace Haulbox",153),rep("DockArc Freight Case",306),rep("HardSeal Transit Crate",1531),rep("LongHaul Vault Canister",613),rep("QuantumCrate Module",459))
fleet_data<-data.frame(solar_system,container_type)






#functions needed for simulation which will give covariate outputs relevant to the particular solar system/container type when building shipment simulation dataset

weight_draw<-function(container_type, length, cargo_type){
  if(cargo_type=="platinum") {
    return(pmin(sample(table_cargo_freq$weight[table_cargo_freq$cargo_type=="platinum"],size=length, replace=TRUE),50000))
  }
  if(container_type=="DeepSpace Haulbox") {
    return(pmin(sample(table_cargo_freq$weight[table_cargo_freq$container_type=="DeepSpace Haulbox"],size=length, replace=TRUE),25000))}
  if(container_type=="DockArc Freight Case") {
    return(pmin(sample(table_cargo_freq$weight[table_cargo_freq$container_type=="DockArc Freight Case"],size=length, replace=TRUE),50000))}
  if(container_type=="HardSeal Transit Crate") {
    return(pmin(sample(table_cargo_freq$weight[table_cargo_freq$container_type=="HardSeal Transit Crate"],size=length, replace=TRUE),100000))}
  if(container_type=="LongHaul Vault Canister") {
    return(pmin(sample(table_cargo_freq$weight[table_cargo_freq$container_type=="LongHaul Vault Canister"],size=length, replace=TRUE),150000))}
  if(container_type=="QuantumCrate Module") {
    return(pmin(sample(table_cargo_freq$weight[table_cargo_freq$container_type=="QuantumCrate Module"],size=length, replace=TRUE),250000))}
}





route_risk_draw<-function(solar_system, length){
  if(solar_system=="Helionis Cluster") {
    return(sample(c(1,2,3,4,5),length, replace=TRUE, prob=c(0.10-0.05,0.2-0.05,0.4-0.1,0.2+0.1,0.1+0.1)))}
  if(solar_system=="Bayesia") {
    return(sample(c(1,2,3,4,5),length, replace=TRUE, prob=c(0.1+0.1,0.2+0.05,0.4,0.2-0.1,0.1-0.05)))}
  if(solar_system=="Oryn Delta") {
    return(sample(c(1,2,3,4,5),length, replace=TRUE, prob=c(0.1-0.05,0.2-0.05,0.4-0.15,0.2+0.1,0.1+0.15)))}}
#of claims, 10%, 20%, 40%, 20% and 10% are route risk 1,2,3,4,5 respectively. adjusted to account for risk in description


debris_density_draw<-function(solar_system, length){
  if(solar_system=="Helionis Cluster") {
    return(pmax(sample(table_cargo_freq$debris_density, size=length, replace=TRUE),0.2))} #0.2 is approximately 1st tercile of debris desnit, forcing as min given high density
  if(solar_system=="Bayesia") {
    return(sample(table_cargo_freq$debris_density,size=length, replace=TRUE))}
  if(solar_system=="Oryn Delta") {
    return(sample(table_cargo_freq$debris_density,size=length, replace=TRUE))}
}


solar_radiation_draw<-function(solar_system, length){
  if(solar_system=="Bayesia") {
    return(pmax(sample(table_cargo_freq$solar_radiation, size=length, replace=TRUE),0.19))} #0.19 is approximately 1st tercile of solar, forcing as min given high density
  if(solar_system=="Helionis Cluster") {
    return(sample(table_cargo_freq$solar_radiation,size=length, replace=TRUE))}
  if(solar_system=="Oryn Delta") {
    return(sample(table_cargo_freq$solar_radiation,size=length, replace=TRUE))}
}




#creation of synthetic dataset 

helionis_fleet_data<-fleet_data%>%
  filter(solar_system=="Helionis Cluster")

bayesia_fleet_data<-fleet_data %>%
  filter(solar_system=="Bayesia")


oryn_fleet_data<-fleet_data%>%
  filter(solar_system=="Oryn Delta")





#----------------------------------------------------short term simulations-------------------------------------------------#


#helionis short-term simulation
#creating shipment dataset
cargo_type_syn<-c()
cargo_value_syn<-c()
weight_syn<-c()
route_risk_syn<-c()
distance_syn<-c()
transit_duration_syn<-c()
pilot_experience_syn<-c()
vessel_age_syn<-c()
solar_radiation_syn<-c()
debris_density_syn<-c()
exposure_syn<-c()
container_type_syn<-c()






for (i in 1:nrow(helionis_fleet_data)){
  shipment_no<-rbinom(1,4,3.3/4)
  if(shipment_no==0)next #ships current iteration of the for loop if 0
  
  container_type_syn<-c(container_type_syn,rep(helionis_fleet_data$container_type[i],shipment_no))
  cargo_type_sampled<-sample(c("supplies","titanium","rare earths","cobalt", "lithium", "platinum"), shipment_no, replace=TRUE, prob=c(1/6,1/6,1/6,1/6,1/6,1/6))
  cargo_weight_sampled<-rep(0,shipment_no)
  for (l in 1:shipment_no) {
    cargo_type_current<-cargo_type_sampled[l]
    cargo_weight_sampled[l]<-weight_draw(container_type=helionis_fleet_data$container_type[i],length=1,cargo_type=cargo_type_current)
  }
  cargo_type_syn<-c(cargo_type_syn,cargo_type_sampled)
  weight_syn<-c(weight_syn,cargo_weight_sampled)
  route_risk_syn<-c(route_risk_syn,route_risk_draw(solar_system=helionis_fleet_data$solar_system[i],length=shipment_no))
  distance_syn<-c(distance_syn,sample(table_cargo_freq$distance,size=shipment_no, replace=TRUE))
  transit_duration_syn<-c(transit_duration_syn,sample(table_cargo_freq$transit_duration,size=shipment_no, replace=TRUE))
  pilot_experience_syn<-c(pilot_experience_syn,sample(table_cargo_freq$pilot_experience,size=shipment_no, replace=TRUE))
  vessel_age_syn<-c(vessel_age_syn,sample(table_cargo_freq$vessel_age,size=shipment_no, replace=TRUE))
  solar_radiation_syn<-c(solar_radiation_syn,solar_radiation_draw(solar_system=helionis_fleet_data$solar_system[i],length=shipment_no))
  debris_density_syn<-c(debris_density_syn,debris_density_draw(solar_system=helionis_fleet_data$solar_system[i],length=shipment_no))
  exposure_syn<-c(exposure_syn,sample(table_cargo_freq$exposure,size=shipment_no, replace=TRUE))
}
practice_data_freq_helionis<-data.frame(container_type=as.factor(container_type_syn),cargo_type=as.factor(cargo_type_syn), weight=weight_syn, route_risk=as.factor(route_risk_syn), distance=distance_syn, transit_duration=transit_duration_syn, pilot_experience=pilot_experience_syn, vessel_age=vessel_age_syn, solar_radiation=solar_radiation_syn, debris_density=debris_density_syn, exposure=exposure_syn)




#introducing glm prediction
predicted_freq<-predict(cargo_freq_model,newdata=practice_data_freq_helionis, type="response")
predicted_relativity_prob<-predict(cargo_classifier_model, newdata=practice_data_freq_helionis,type="response")
predicted_relativity<-rbinom(nrow(practice_data_freq_helionis),1,predicted_relativity_prob)



predicted_sev<- rep(NA, nrow(practice_data_freq_helionis))

small_idx <- which(predicted_relativity == 0)
large_idx <- which(predicted_relativity == 1)

predicted_sev[small_idx] <- predict(cargo_sev_small_model,
                                    newdata = practice_data_freq_helionis[small_idx, ],
                                    type = "response")

predicted_sev[large_idx] <- predict(cargo_sev_large_model,
                                    newdata = practice_data_freq_helionis[large_idx, ],
                                    type = "response")



practice_data_freq_helionis<-practice_data_freq_helionis%>%
  mutate(predicted_freq=predicted_freq, predicted_sev=predicted_sev) 

sev_dispersion<-ifelse(predicted_relativity==0,sev_dispersion_small,sev_dispersion_large)

practice_data_freq_helionis<-practice_data_freq_helionis%>%
  mutate(lambda=predicted_freq,shape=1/sev_dispersion, scale=predicted_sev*sev_dispersion)


summary(practice_data_freq_helionis)









#trialling expected loss simulations for different deductibles/policy limits

deductible<-rep(seq(0,100000,by=25000),each=5)
policy_limit<-rep(seq(10^6,1.8*10^6,by=2*10^5),times=5)
average_loss<-rep(0,length(deductible))


for (g in 1:length(deductible)){
  n_sim <- 5000
  n_rows <- nrow(practice_data_freq_helionis)
  simulated_total_short_helionis <- numeric(n_sim)
  for (k in seq_len(n_sim)) {
    number_claims <- rpois(n_rows, practice_data_freq_helionis$lambda)
    idx <- which(number_claims > 0)
    if (length(idx) > 0) {
      # Repeat parameters according to number of claims
      shapes <- rep(practice_data_freq_helionis$shape[idx], number_claims[idx])
      scales <- rep(practice_data_freq_helionis$scale[idx], number_claims[idx])
      claim_size <- rgamma(length(shapes), shape = shapes, scale = scales)
      claim_size_limit <- pmax(0, pmin(claim_size, policy_limit[g]) - deductible[g])
      simulated_total_short_helionis[k] <- sum(claim_size_limit)
    }
  }
  average_loss[g]<-mean(simulated_total_short_helionis)
}

average_loss

policy_structure<-data.frame(Deductible=deductible, Policy_limit=policy_limit, Expected_loss=average_loss)








#actual simulation (by container type)
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

mean(simulated_total_short_helionis)



#adding Buhlmann credibiltiy
mean(simulated_total_short_helionis_deepspace)
mean(simulated_total_short_helionis_dockarc)
mean(simulated_total_short_helionis_hardseal)
mean(simulated_total_short_helionis_longhaul)
mean(simulated_total_short_helionis_quantumcrate)


J <- 5

w_helionis_deepspace <- 58
w_helionis_dockarc <- 116
w_helionis_hardseal <- 580
w_helionis_longhaul <- 232
w_helionis_quantumcrate <- 174
w_helionis_vector <- c(w_helionis_deepspace, w_helionis_dockarc, w_helionis_hardseal, w_helionis_longhaul, w_helionis_quantumcrate)
w_helionis <- sum(w_helionis_vector)


Y_helionis_deepspace <- simulated_total_short_helionis_deepspace / w_helionis_deepspace
Y_helionis_dockarc <- simulated_total_short_helionis_dockarc / w_helionis_dockarc
Y_helionis_hardseal <- simulated_total_short_helionis_hardseal / w_helionis_hardseal
Y_helionis_longhaul <- simulated_total_short_helionis_longhaul / w_helionis_longhaul
Y_helionis_quantumcrate <- simulated_total_short_helionis_quantumcrate / w_helionis_quantumcrate


mean_helionis_deepspace <- mean(Y_helionis_deepspace)
mean_helionis_dockarc <- mean(Y_helionis_dockarc)
mean_helionis_hardseal <- mean(Y_helionis_hardseal)
mean_helionis_longhaul <- mean(Y_helionis_longhaul)
mean_helionis_quantumcrate <- mean(Y_helionis_quantumcrate)
mean_helionis_vector <- c(mean_helionis_deepspace, mean_helionis_dockarc, mean_helionis_hardseal, mean_helionis_longhaul, mean_helionis_quantumcrate)
mean_helionis <- weighted.mean(mean_helionis_vector, w_helionis_vector)


s_helionis <- mean(c(var(Y_helionis_deepspace),var(Y_helionis_dockarc),var(Y_helionis_hardseal),var(Y_helionis_longhaul),var(Y_helionis_quantumcrate)))
a_helionis <- max((w_helionis / (w_helionis^2 - sum(w_helionis_vector^2))) *(sum(w_helionis_vector * (mean_helionis_vector - mean_helionis)^2) - (J - 1) * s_helionis), 0)
z_helionis <- w_helionis_vector / (w_helionis_vector + (s_helionis / a_helionis))
buhlmann_helionis <- w_helionis_vector * z_helionis * mean_helionis_vector + (1 - z_helionis) * mean_helionis

buhlmann_helionis










#bayesia short-term simulation
#creating shipment dataset
cargo_type_syn<-c()
cargo_value_syn<-c()
weight_syn<-c()
route_risk_syn<-c()
distance_syn<-c()
transit_duration_syn<-c()
pilot_experience_syn<-c()
vessel_age_syn<-c()
solar_radiation_syn<-c()
debris_density_syn<-c()
exposure_syn<-c()
container_type_syn<-c()


for (i in 1:nrow(bayesia_fleet_data)){
  shipment_no<-rbinom(1,3,2.2/3)
  if(shipment_no==0)next #ships current iteration of the for loop if 0
  container_type_syn<-c(container_type_syn,rep(bayesia_fleet_data$container_type[i],shipment_no))
  cargo_type_sampled<-sample(c("supplies","titanium","rare earths","cobalt", "lithium", "platinum"), shipment_no, replace=TRUE, prob=c(1/6,1/6,1/6,1/6,1/6,1/6))
  cargo_weight_sampled<-rep(0,shipment_no)
  for (l in 1:shipment_no) {
    cargo_type_current<-cargo_type_sampled[l]
    cargo_weight_sampled[l]<-weight_draw(container_type=bayesia_fleet_data$container_type[i],length=1,cargo_type=cargo_type_current)
  }
  cargo_type_syn<-c(cargo_type_syn,cargo_type_sampled)
  weight_syn<-c(weight_syn,cargo_weight_sampled)
  route_risk_syn<-c(route_risk_syn,route_risk_draw(solar_system=bayesia_fleet_data$solar_system[i],length=shipment_no))
  distance_syn<-c(distance_syn,sample(table_cargo_freq$distance,size=shipment_no, replace=TRUE))
  transit_duration_syn<-c(transit_duration_syn,sample(table_cargo_freq$transit_duration,size=shipment_no, replace=TRUE))
  pilot_experience_syn<-c(pilot_experience_syn,sample(table_cargo_freq$pilot_experience,size=shipment_no, replace=TRUE))
  vessel_age_syn<-c(vessel_age_syn,sample(table_cargo_freq$vessel_age,size=shipment_no, replace=TRUE))
  solar_radiation_syn<-c(solar_radiation_syn,solar_radiation_draw(solar_system=bayesia_fleet_data$solar_system[i],length=shipment_no))
  debris_density_syn<-c(debris_density_syn,debris_density_draw(solar_system=bayesia_fleet_data$solar_system[i],length=shipment_no))
  exposure_syn<-c(exposure_syn,sample(table_cargo_freq$exposure,size=shipment_no, replace=TRUE))
}
practice_data_freq_bayesia<-data.frame(container_type=as.factor(container_type_syn),cargo_type=as.factor(cargo_type_syn), weight=weight_syn, route_risk=as.factor(route_risk_syn), distance=distance_syn, transit_duration=transit_duration_syn, pilot_experience=pilot_experience_syn, vessel_age=vessel_age_syn, solar_radiation=solar_radiation_syn, debris_density=debris_density_syn, exposure=exposure_syn)




#introducing glm prediction
predicted_freq<-predict(cargo_freq_model,newdata=practice_data_freq_bayesia, type="response")
predicted_relativity_prob<-predict(cargo_classifier_model, newdata=practice_data_freq_bayesia,type="response")
predicted_relativity<-rbinom(nrow(practice_data_freq_bayesia),1,predicted_relativity_prob)



predicted_sev<- rep(NA, nrow(practice_data_freq_bayesia))

small_idx <- which(predicted_relativity == 0)
large_idx <- which(predicted_relativity == 1)

predicted_sev[small_idx] <- predict(cargo_sev_small_model,
                                    newdata = practice_data_freq_bayesia[small_idx, ],
                                    type = "response")

predicted_sev[large_idx] <- predict(cargo_sev_large_model,
                                    newdata = practice_data_freq_bayesia[large_idx, ],
                                    type = "response")



practice_data_freq_bayesia<-practice_data_freq_bayesia%>%
  mutate(predicted_freq=predicted_freq, predicted_sev=predicted_sev) 

sev_dispersion<-ifelse(predicted_relativity==0,sev_dispersion_small,sev_dispersion_large)

practice_data_freq_bayesia<-practice_data_freq_bayesia%>%
  mutate(lambda=predicted_freq,shape=1/sev_dispersion, scale=predicted_sev*sev_dispersion)










#actual simulation (by container type)
n_sim <- 10000
n_rows <- nrow(practice_data_freq_bayesia)


simulated_total_short_bayesia <- numeric(n_sim)


simulated_total_short_bayesia_deepspace   <- numeric(n_sim)
simulated_total_short_bayesia_dockarc     <- numeric(n_sim)
simulated_total_short_bayesia_hardseal    <- numeric(n_sim)
simulated_total_short_bayesia_longhaul    <- numeric(n_sim)
simulated_total_short_bayesia_quantumcrate<- numeric(n_sim)

for (k in seq_len(n_sim)) {
  
  number_claims <- rpois(n_rows, practice_data_freq_bayesia$lambda)
  idx <- which(number_claims > 0)
  
  if (length(idx) > 0) {
    shapes <- rep(practice_data_freq_bayesia$shape[idx], number_claims[idx])
    scales <- rep(practice_data_freq_bayesia$scale[idx], number_claims[idx])
    container <- rep(practice_data_freq_bayesia$container_type[idx], number_claims[idx])
    
    claim_size <- rgamma(length(shapes), shape = shapes, scale = scales)
    claim_size_limit <- pmax(0, pmin(claim_size, 1e6) - 1e5)
    
    simulated_total_short_bayesia[k] <- sum(claim_size_limit)
    
    simulated_total_short_bayesia_deepspace[k] <-
      sum(claim_size_limit[container == "DeepSpace Haulbox"])
    simulated_total_short_bayesia_dockarc[k] <-
      sum(claim_size_limit[container == "DockArc Freight Case"])
    simulated_total_short_bayesia_hardseal[k] <-
      sum(claim_size_limit[container == "HardSeal Transit Crate"])
    simulated_total_short_bayesia_longhaul[k] <-
      sum(claim_size_limit[container == "LongHaul Vault Canister"])
    simulated_total_short_bayesia_quantumcrate[k] <-
      sum(claim_size_limit[container == "QuantumCrate Module"])
  }
}





#adding buhlmann credibility
mean(simulated_total_short_bayesia_deepspace)
mean(simulated_total_short_bayesia_dockarc)
mean(simulated_total_short_bayesia_hardseal)
mean(simulated_total_short_bayesia_longhaul)
mean(simulated_total_short_bayesia_quantumcrate)





J <- 5

w_bayesia_deepspace <- 58
w_bayesia_dockarc <- 116
w_bayesia_hardseal <- 580
w_bayesia_longhaul <- 232
w_bayesia_quantumcrate <- 174
w_bayesia_vector <- c(w_bayesia_deepspace, w_bayesia_dockarc, w_bayesia_hardseal, w_bayesia_longhaul, w_bayesia_quantumcrate)
w_bayesia <- sum(w_bayesia_vector)


Y_bayesia_deepspace <- simulated_total_short_bayesia_deepspace / w_bayesia_deepspace
Y_bayesia_dockarc <- simulated_total_short_bayesia_dockarc / w_bayesia_dockarc
Y_bayesia_hardseal <- simulated_total_short_bayesia_hardseal / w_bayesia_hardseal
Y_bayesia_longhaul <- simulated_total_short_bayesia_longhaul / w_bayesia_longhaul
Y_bayesia_quantumcrate <- simulated_total_short_bayesia_quantumcrate / w_bayesia_quantumcrate


mean_bayesia_deepspace <- mean(Y_bayesia_deepspace)
mean_bayesia_dockarc <- mean(Y_bayesia_dockarc)
mean_bayesia_hardseal <- mean(Y_bayesia_hardseal)
mean_bayesia_longhaul <- mean(Y_bayesia_longhaul)
mean_bayesia_quantumcrate <- mean(Y_bayesia_quantumcrate)
mean_bayesia_vector <- c(mean_bayesia_deepspace, mean_bayesia_dockarc, mean_bayesia_hardseal, mean_bayesia_longhaul, mean_bayesia_quantumcrate)
mean_bayesia <- weighted.mean(mean_bayesia_vector, w_bayesia_vector)


s_bayesia <- mean(c(var(Y_bayesia_deepspace), var(Y_bayesia_dockarc), var(Y_bayesia_hardseal), var(Y_bayesia_longhaul), var(Y_bayesia_quantumcrate)))
a_bayesia <- max((w_bayesia / (w_bayesia^2 - sum(w_bayesia_vector^2))) * (sum(w_bayesia_vector * (mean_bayesia_vector - mean_bayesia)^2) - (J - 1) * s_bayesia), 0)
z_bayesia <- w_bayesia_vector / (w_bayesia_vector + (s_bayesia / a_bayesia))
buhlmann_bayesia <- w_bayesia_vector * z_bayesia * mean_bayesia_vector + (1 - z_bayesia) * mean_bayesia

buhlmann_bayesia











#oryn short-term simulation
#creating synthetic shipment dataset
cargo_type_syn<-c()
cargo_value_syn<-c()
weight_syn<-c()
route_risk_syn<-c()
distance_syn<-c()
transit_duration_syn<-c()
pilot_experience_syn<-c()
vessel_age_syn<-c()
solar_radiation_syn<-c()
debris_density_syn<-c()
exposure_syn<-c()
container_type_syn<-c()


for (i in 1:nrow(oryn_fleet_data)){
  shipment_no<-rbinom(1,2,1.5/2)
  if(shipment_no==0)next #ships current iteration of the for loop if 0
  
  container_type_syn<-c(container_type_syn,rep(oryn_fleet_data$container_type[i],shipment_no))
  cargo_type_sampled<-sample(c("supplies","titanium","rare earths","cobalt", "lithium", "platinum"), shipment_no, replace=TRUE, prob=c(1/6,1/6,1/6,1/6,1/6,1/6))
  cargo_weight_sampled<-rep(0,shipment_no)
  for (l in 1:shipment_no) {
    cargo_type_current<-cargo_type_sampled[l]
    cargo_weight_sampled[l]<-weight_draw(container_type=oryn_fleet_data$container_type[i],length=1,cargo_type=cargo_type_current)
  }
  cargo_type_syn<-c(cargo_type_syn,cargo_type_sampled)
  weight_syn<-c(weight_syn,cargo_weight_sampled)
  route_risk_syn<-c(route_risk_syn,route_risk_draw(solar_system=oryn_fleet_data$solar_system[i],length=shipment_no))
  distance_syn<-c(distance_syn,sample(table_cargo_freq$distance,size=shipment_no, replace=TRUE))
  transit_duration_syn<-c(transit_duration_syn,sample(table_cargo_freq$transit_duration,size=shipment_no, replace=TRUE))
  pilot_experience_syn<-c(pilot_experience_syn,sample(table_cargo_freq$pilot_experience,size=shipment_no, replace=TRUE))
  vessel_age_syn<-c(vessel_age_syn,sample(table_cargo_freq$vessel_age,size=shipment_no, replace=TRUE))
  solar_radiation_syn<-c(solar_radiation_syn,solar_radiation_draw(solar_system=oryn_fleet_data$solar_system[i],length=shipment_no))
  debris_density_syn<-c(debris_density_syn,debris_density_draw(solar_system=oryn_fleet_data$solar_system[i],length=shipment_no))
  exposure_syn<-c(exposure_syn,sample(table_cargo_freq$exposure,size=shipment_no, replace=TRUE))
}
practice_data_freq_oryn<-data.frame(container_type=as.factor(container_type_syn),cargo_type=as.factor(cargo_type_syn), weight=weight_syn, route_risk=as.factor(route_risk_syn), distance=distance_syn, transit_duration=transit_duration_syn, pilot_experience=pilot_experience_syn, vessel_age=vessel_age_syn, solar_radiation=solar_radiation_syn, debris_density=debris_density_syn, exposure=exposure_syn)





#introducing glm prediction
predicted_freq<-predict(cargo_freq_model,newdata=practice_data_freq_oryn, type="response")
predicted_relativity_prob<-predict(cargo_classifier_model, newdata=practice_data_freq_oryn,type="response")
predicted_relativity<-rbinom(nrow(practice_data_freq_oryn),1,predicted_relativity_prob)



predicted_sev<- rep(NA, nrow(practice_data_freq_oryn))

small_idx <- which(predicted_relativity == 0)
large_idx <- which(predicted_relativity == 1)

predicted_sev[small_idx] <- predict(cargo_sev_small_model,
                                    newdata = practice_data_freq_oryn[small_idx, ],
                                    type = "response")

predicted_sev[large_idx] <- predict(cargo_sev_large_model,
                                    newdata = practice_data_freq_oryn[large_idx, ],
                                    type = "response")



practice_data_freq_oryn<-practice_data_freq_oryn%>%
  mutate(predicted_freq=predicted_freq, predicted_sev=predicted_sev) 

sev_dispersion<-ifelse(predicted_relativity==0,sev_dispersion_small,sev_dispersion_large)

practice_data_freq_oryn<-practice_data_freq_oryn%>%
  mutate(lambda=predicted_freq,shape=1/sev_dispersion, scale=predicted_sev*sev_dispersion)





#actual simulation (by container type)

n_sim <- 10000
n_rows <- nrow(practice_data_freq_oryn)


simulated_total_short_oryn <- numeric(n_sim)


simulated_total_short_oryn_deepspace<-numeric(n_sim)
simulated_total_short_oryn_dockarc<-numeric(n_sim)
simulated_total_short_oryn_hardseal<- numeric(n_sim)
simulated_total_short_oryn_longhaul<-numeric(n_sim)
simulated_total_short_oryn_quantumcrate<-numeric(n_sim)

for (k in seq_len(n_sim)) {
  
  number_claims <- rpois(n_rows, practice_data_freq_oryn$lambda)
  idx <- which(number_claims > 0)
  
  if (length(idx) > 0) {
    
    
    shapes <- rep(practice_data_freq_oryn$shape[idx], number_claims[idx])
    scales <- rep(practice_data_freq_oryn$scale[idx], number_claims[idx])
    container <- rep(practice_data_freq_oryn$container_type[idx], number_claims[idx])
    
    claim_size <- rgamma(length(shapes), shape = shapes, scale = scales)
    claim_size_limit <- pmax(0, pmin(claim_size, 1e6) - 1e5)
    
    
    simulated_total_short_oryn[k] <- sum(claim_size_limit)
    
    
    simulated_total_short_oryn_deepspace[k] <-
      sum(claim_size_limit[container == "DeepSpace Haulbox"])
    simulated_total_short_oryn_dockarc[k] <-
      sum(claim_size_limit[container == "DockArc Freight Case"])
    simulated_total_short_oryn_hardseal[k] <-
      sum(claim_size_limit[container == "HardSeal Transit Crate"])
    simulated_total_short_oryn_longhaul[k] <-
      sum(claim_size_limit[container == "LongHaul Vault Canister"])
    simulated_total_short_oryn_quantumcrate[k] <-
      sum(claim_size_limit[container == "QuantumCrate Module"])
  }
}



#adding buhlmann credibility
mean(simulated_total_short_oryn_deepspace)
mean(simulated_total_short_oryn_dockarc)
mean(simulated_total_short_oryn_hardseal)
mean(simulated_total_short_oryn_longhaul)
mean(simulated_total_short_oryn_quantumcrate)



J <- 5

w_oryn_deepspace <- 58
w_oryn_dockarc <- 116
w_oryn_hardseal <- 580
w_oryn_longhaul <- 232
w_oryn_quantumcrate <- 174
w_oryn_vector <- c(w_oryn_deepspace, w_oryn_dockarc, w_oryn_hardseal, w_oryn_longhaul, w_oryn_quantumcrate)
w_oryn <- sum(w_oryn_vector)


Y_oryn_deepspace <- simulated_total_short_oryn_deepspace / w_oryn_deepspace
Y_oryn_dockarc <- simulated_total_short_oryn_dockarc / w_oryn_dockarc
Y_oryn_hardseal <- simulated_total_short_oryn_hardseal / w_oryn_hardseal
Y_oryn_longhaul <- simulated_total_short_oryn_longhaul / w_oryn_longhaul
Y_oryn_quantumcrate <- simulated_total_short_oryn_quantumcrate / w_oryn_quantumcrate


mean_oryn_deepspace <- mean(Y_oryn_deepspace)
mean_oryn_dockarc <- mean(Y_oryn_dockarc)
mean_oryn_hardseal <- mean(Y_oryn_hardseal)
mean_oryn_longhaul <- mean(Y_oryn_longhaul)
mean_oryn_quantumcrate <- mean(Y_oryn_quantumcrate)
mean_oryn_vector <- c(mean_oryn_deepspace, mean_oryn_dockarc, mean_oryn_hardseal, mean_oryn_longhaul, mean_oryn_quantumcrate)
mean_oryn <- weighted.mean(mean_oryn_vector, w_oryn_vector)


s_oryn <- mean(c(var(Y_oryn_deepspace), var(Y_oryn_dockarc), var(Y_oryn_hardseal), var(Y_oryn_longhaul), var(Y_oryn_quantumcrate)))
a_oryn <- max((w_oryn / (w_oryn^2 - sum(w_oryn_vector^2))) * (sum(w_oryn_vector * (mean_oryn_vector - mean_oryn)^2) - (J - 1) * s_oryn), 0)
z_oryn <- w_oryn_vector / (w_oryn_vector + (s_oryn / a_oryn))
buhlmann_oryn <- w_oryn_vector * z_oryn * mean_oryn_vector + (1 - z_oryn) * mean_oryn

buhlmann_oryn







#-------------------------------------------------long-term simulations----------------------------------------------------#
#helionis

n_sim <- 10000
n_rows <- nrow(practice_data_freq_helionis)

simulated_total_10_years_helionis <- numeric(n_sim)

for (k in 1:n_sim) {
  
  # Initialize total claims for this simulation
  total_claims_simulation <- 0
  
  for (m in 1:10) {
    number_claims <- rpois(n_rows, practice_data_freq_helionis$lambda)
    
    shapes <- rep(practice_data_freq_helionis$shape, number_claims)
    scales <- rep(practice_data_freq_helionis$scale, number_claims)
    
    claim_size <- rgamma(length(shapes), shape = shapes, scale = scales)
    claim_size_limit <- pmax(0, pmin(claim_size, 1e6) - 1e5)
    
    total_claims_simulation <- total_claims_simulation + sum(claim_size_limit)
  }
  
  simulated_total_10_years_helionis[k] <- total_claims_simulation * inflation[m] / interest[m]
}

simulated_total_10_years_helionis








#bayesia

n_sim <- 10000
n_rows <- nrow(practice_data_freq_bayesia)

simulated_total_10_years_bayesia <- numeric(n_sim)

for (k in 1:n_sim) {
  total_claims_simulation <- 0
  for (m in 1:10) {
    number_claims <- rpois(n_rows, practice_data_freq_bayesia$lambda)
    
    shapes <- rep(practice_data_freq_bayesia$shape, number_claims)
    scales <- rep(practice_data_freq_bayesia$scale, number_claims)

    claim_size <- rgamma(length(shapes), shape = shapes, scale = scales)
    claim_size_limit <- pmax(0, pmin(claim_size, 1e6) - 1e5)
    
    total_claims_simulation <- total_claims_simulation + sum(claim_size_limit)
  }
  
  simulated_total_10_years_bayesia[k] <- total_claims_simulation * inflation[m] / interest[m]
}

simulated_total_10_years_bayesia




#oryn
n_sim <- 10000
n_rows <- nrow(practice_data_freq_oryn)

simulated_total_10_years_oryn <- numeric(n_sim)
for (k in 1:n_sim) {
  total_claims_simulation <- 0
  
  for (m in 1:10) {
    number_claims <- rpois(n_rows, practice_data_freq_oryn$lambda)
    
    shapes <- rep(practice_data_freq_oryn$shape, number_claims)
    scales <- rep(practice_data_freq_oryn$scale, number_claims)

    claim_size <- rgamma(length(shapes), shape = shapes, scale = scales)
    claim_size_limit <- pmax(0, pmin(claim_size, 1e6) - 1e5)

    total_claims_simulation <- total_claims_simulation + sum(claim_size_limit)
  }

  simulated_total_10_years_oryn[k] <- total_claims_simulation * inflation[m] / interest[m]
}

simulated_total_10_years_oryn













#---------------------------------------------------Stress test simulations--------------------------------------------#




#stress test 1 (worst case scenario)-input route risk, solar radiation, debris concentration to be highest possible value
#helionis
#create synthetic shipment dataset
cargo_type_syn<-c()
cargo_value_syn<-c()
weight_syn<-c()
route_risk_syn<-c()
distance_syn<-c()
transit_duration_syn<-c()
pilot_experience_syn<-c()
vessel_age_syn<-c()
solar_radiation_syn<-c()
debris_density_syn<-c()
exposure_syn<-c()
container_type_syn<-c()


for (i in 1:nrow(helionis_fleet_data)){
  shipment_no<-rbinom(1,4,3.3*1.1/4)
  if(shipment_no==0)next #ships current iteration of the for loop if 0
  container_type_syn<-c(container_type_syn,rep(helionis_fleet_data$container_type[i],shipment_no))
  cargo_type_sampled<-sample(c("supplies","titanium","rare earths","cobalt", "lithium", "platinum"), shipment_no, replace=TRUE, prob=c(1/6,1/6,1/6,1/6,1/6,1/6))
  cargo_weight_sampled<-rep(0,shipment_no)
  for (l in 1:shipment_no) {
    cargo_type_current<-cargo_type_sampled[l]
    cargo_weight_sampled[l]<-weight_draw(container_type=helionis_fleet_data$container_type[i],length=1,cargo_type=cargo_type_current)
  }
  cargo_type_syn<-c(cargo_type_syn,cargo_type_sampled)
  weight_syn<-c(weight_syn,cargo_weight_sampled)
  route_risk_syn<-c(route_risk_syn,rep(5,shipment_no))
  distance_syn<-c(distance_syn,sample(table_cargo_freq$distance,size=shipment_no, replace=TRUE))
  transit_duration_syn<-c(transit_duration_syn,sample(table_cargo_freq$transit_duration,size=shipment_no, replace=TRUE))
  pilot_experience_syn<-c(pilot_experience_syn,sample(table_cargo_freq$pilot_experience,size=shipment_no, replace=TRUE))
  vessel_age_syn<-c(vessel_age_syn,sample(table_cargo_freq$vessel_age,size=shipment_no, replace=TRUE))
  solar_radiation_syn<-c(solar_radiation_syn,rep(1,shipment_no))
  debris_density_syn<-c(debris_density_syn,rep(1,shipment_no))
  exposure_syn<-c(exposure_syn,sample(table_cargo_freq$exposure,size=shipment_no, replace=TRUE))
}
practice_data_freq_helionis<-data.frame(container_type=as.factor(container_type_syn),cargo_type=as.factor(cargo_type_syn), weight=weight_syn, route_risk=as.factor(route_risk_syn), distance=distance_syn, transit_duration=transit_duration_syn, pilot_experience=pilot_experience_syn, vessel_age=vessel_age_syn, solar_radiation=solar_radiation_syn, debris_density=debris_density_syn, exposure=exposure_syn)




#introducing glm prediction
predicted_freq<-predict(cargo_freq_model,newdata=practice_data_freq_helionis, type="response")
predicted_relativity_prob<-predict(cargo_classifier_model, newdata=practice_data_freq_helionis,type="response")
predicted_relativity<-rbinom(nrow(practice_data_freq_helionis),1,predicted_relativity_prob)



predicted_sev<- rep(NA, nrow(practice_data_freq_helionis))

small_idx <- which(predicted_relativity == 0)
large_idx <- which(predicted_relativity == 1)

predicted_sev[small_idx] <- predict(cargo_sev_small_model,
                                    newdata = practice_data_freq_helionis[small_idx, ],
                                    type = "response")

predicted_sev[large_idx] <- predict(cargo_sev_large_model,
                                    newdata = practice_data_freq_helionis[large_idx, ],
                                    type = "response")



practice_data_freq_helionis<-practice_data_freq_helionis%>%
  mutate(predicted_freq=predicted_freq, predicted_sev=predicted_sev) 

sev_dispersion<-ifelse(predicted_relativity==0,sev_dispersion_small,sev_dispersion_large)

practice_data_freq_helionis<-practice_data_freq_helionis%>%
  mutate(lambda=predicted_freq,shape=1/sev_dispersion, scale=predicted_sev*sev_dispersion)





n_sim <- 10000
n_rows <- nrow(practice_data_freq_helionis)
simulated_total_short_helionis_shock <- numeric(n_sim)
for (k in seq_len(n_sim)) {
  number_claims <- rpois(n_rows, practice_data_freq_helionis$lambda)
  idx <- which(number_claims > 0)
  if (length(idx) > 0) {
    # Repeat parameters according to number of claims
    shapes <- rep(practice_data_freq_helionis$shape[idx], number_claims[idx])
    scales <- rep(practice_data_freq_helionis$scale[idx], number_claims[idx])
    claim_size <- rgamma(length(shapes), shape = shapes, scale = scales)
    claim_size_limit <- pmax(0, pmin(claim_size, 1e6) - 1e5)
    simulated_total_short_helionis_shock[k] <- sum(claim_size_limit)
  }
}

simulated_total_short_helionis_shock

mean(simulated_total_short_helionis_shock)
mean(simulated_total_short_helionis)






#bayesia
#create synthetic shipment dataset
cargo_type_syn<-c()
cargo_value_syn<-c()
weight_syn<-c()
route_risk_syn<-c()
distance_syn<-c()
transit_duration_syn<-c()
pilot_experience_syn<-c()
vessel_age_syn<-c()
solar_radiation_syn<-c()
debris_density_syn<-c()
exposure_syn<-c()
container_type_syn<-c()


for (i in 1:nrow(bayesia_fleet_data)){
  shipment_no<-rbinom(1,3,2.2*1.1/3)
  if(shipment_no==0)next #ships current iteration of the for loop if 0
  container_type_syn<-c(container_type_syn,rep(bayesia_fleet_data$container_type[i],shipment_no))
  cargo_type_sampled<-sample(c("supplies","titanium","rare earths","cobalt", "lithium", "platinum"), shipment_no, replace=TRUE, prob=c(1/6,1/6,1/6,1/6,1/6,1/6))
  cargo_weight_sampled<-rep(0,shipment_no)
  for (l in 1:shipment_no) {
    cargo_type_current<-cargo_type_sampled[l]
    cargo_weight_sampled[l]<-weight_draw(container_type=bayesia_fleet_data$container_type[i],length=1,cargo_type=cargo_type_current)
  }
  cargo_type_syn<-c(cargo_type_syn,cargo_type_sampled)
  weight_syn<-c(weight_syn,cargo_weight_sampled)
  route_risk_syn<-c(route_risk_syn,rep(5,shipment_no))
  distance_syn<-c(distance_syn,sample(table_cargo_freq$distance,size=shipment_no, replace=TRUE))
  transit_duration_syn<-c(transit_duration_syn,sample(table_cargo_freq$transit_duration,size=shipment_no, replace=TRUE))
  pilot_experience_syn<-c(pilot_experience_syn,sample(table_cargo_freq$pilot_experience,size=shipment_no, replace=TRUE))
  vessel_age_syn<-c(vessel_age_syn,sample(table_cargo_freq$vessel_age,size=shipment_no, replace=TRUE))
  solar_radiation_syn<-c(solar_radiation_syn,rep(1,shipment_no))
  debris_density_syn<-c(debris_density_syn,rep(1,shipment_no))
  exposure_syn<-c(exposure_syn,sample(table_cargo_freq$exposure,size=shipment_no, replace=TRUE))
}
practice_data_freq_bayesia<-data.frame(container_type=as.factor(container_type_syn),cargo_type=as.factor(cargo_type_syn), weight=weight_syn, route_risk=as.factor(route_risk_syn), distance=distance_syn, transit_duration=transit_duration_syn, pilot_experience=pilot_experience_syn, vessel_age=vessel_age_syn, solar_radiation=solar_radiation_syn, debris_density=debris_density_syn, exposure=exposure_syn)


#introducing glm prediction
predicted_freq<-predict(cargo_freq_model,newdata=practice_data_freq_bayesia, type="response")
predicted_relativity_prob<-predict(cargo_classifier_model, newdata=practice_data_freq_bayesia,type="response")
predicted_relativity<-rbinom(nrow(practice_data_freq_bayesia),1,predicted_relativity_prob)


predicted_sev<- rep(NA, nrow(practice_data_freq_bayesia))

small_idx <- which(predicted_relativity == 0)
large_idx <- which(predicted_relativity == 1)

predicted_sev[small_idx] <- predict(cargo_sev_small_model,
                                    newdata = practice_data_freq_bayesia[small_idx, ],
                                    type = "response")

predicted_sev[large_idx] <- predict(cargo_sev_large_model,
                                    newdata = practice_data_freq_bayesia[large_idx, ],
                                    type = "response")


practice_data_freq_bayesia<-practice_data_freq_bayesia%>%
  mutate(predicted_freq=predicted_freq, predicted_sev=predicted_sev) 

sev_dispersion<-ifelse(predicted_relativity==0,sev_dispersion_small,sev_dispersion_large)

practice_data_freq_bayesia<-practice_data_freq_bayesia%>%
  mutate(lambda=predicted_freq,shape=1/sev_dispersion, scale=predicted_sev*sev_dispersion)


n_sim <- 10000
n_rows <- nrow(practice_data_freq_bayesia)
simulated_total_short_bayesia_shock <- numeric(n_sim)
for (k in seq_len(n_sim)) {
  number_claims <- rpois(n_rows, practice_data_freq_bayesia$lambda)
  idx <- which(number_claims > 0)
  if (length(idx) > 0) {
    # Repeat parameters according to number of claims
    shapes <- rep(practice_data_freq_bayesia$shape[idx], number_claims[idx])
    scales <- rep(practice_data_freq_bayesia$scale[idx], number_claims[idx])
    claim_size <- rgamma(length(shapes), shape = shapes, scale = scales)
    claim_size_limit <- pmax(0, pmin(claim_size, 1e6) - 1e5)
    simulated_total_short_bayesia_shock[k] <- sum(claim_size_limit)
  }
}

simulated_total_short_bayesia_shock











#oryn
#create sythetic shipment dataset
cargo_type_syn<-c()
cargo_value_syn<-c()
weight_syn<-c()
route_risk_syn<-c()
distance_syn<-c()
transit_duration_syn<-c()
pilot_experience_syn<-c()
vessel_age_syn<-c()
solar_radiation_syn<-c()
debris_density_syn<-c()
exposure_syn<-c()
container_type_syn<-c()


for (i in 1:nrow(oryn_fleet_data)){
  shipment_no<-rbinom(1,2,1.5*1.1/2)
  if(shipment_no==0)next #skip current iteration if 0
  
  container_type_syn<-c(container_type_syn,rep(oryn_fleet_data$container_type[i],shipment_no))
  cargo_type_sampled<-sample(c("supplies","titanium","rare earths","cobalt", "lithium", "platinum"), shipment_no, replace=TRUE, prob=c(1/6,1/6,1/6,1/6,1/6,1/6))
  cargo_weight_sampled<-rep(0,shipment_no)
  for (l in 1:shipment_no) {
    cargo_type_current<-cargo_type_sampled[l]
    cargo_weight_sampled[l]<-weight_draw(container_type=oryn_fleet_data$container_type[i],length=1,cargo_type=cargo_type_current)
  }
  cargo_type_syn<-c(cargo_type_syn,cargo_type_sampled)
  weight_syn<-c(weight_syn,cargo_weight_sampled)
  route_risk_syn<-c(route_risk_syn,rep(5,shipment_no))
  distance_syn<-c(distance_syn,sample(table_cargo_freq$distance,size=shipment_no, replace=TRUE))
  transit_duration_syn<-c(transit_duration_syn,sample(table_cargo_freq$transit_duration,size=shipment_no, replace=TRUE))
  pilot_experience_syn<-c(pilot_experience_syn,sample(table_cargo_freq$pilot_experience,size=shipment_no, replace=TRUE))
  vessel_age_syn<-c(vessel_age_syn,sample(table_cargo_freq$vessel_age,size=shipment_no, replace=TRUE))
  solar_radiation_syn<-c(solar_radiation_syn,rep(1,shipment_no))
  debris_density_syn<-c(debris_density_syn,rep(1,shipment_no))
  exposure_syn<-c(exposure_syn,sample(table_cargo_freq$exposure,size=shipment_no, replace=TRUE))
}

practice_data_freq_oryn<-data.frame(
  container_type=as.factor(container_type_syn),
  cargo_type=as.factor(cargo_type_syn),
  weight=weight_syn,
  route_risk=as.factor(route_risk_syn),
  distance=distance_syn,
  transit_duration=transit_duration_syn,
  pilot_experience=pilot_experience_syn,
  vessel_age=vessel_age_syn,
  solar_radiation=solar_radiation_syn,
  debris_density=debris_density_syn,
  exposure=exposure_syn
)

# GLM prediction
predicted_freq<-predict(cargo_freq_model,newdata=practice_data_freq_oryn, type="response")
predicted_relativity_prob<-predict(cargo_classifier_model, newdata=practice_data_freq_oryn,type="response")
predicted_relativity<-rbinom(nrow(practice_data_freq_oryn),1,predicted_relativity_prob)

predicted_sev<- rep(NA, nrow(practice_data_freq_oryn))

small_idx <- which(predicted_relativity == 0)
large_idx <- which(predicted_relativity == 1)

predicted_sev[small_idx] <- predict(cargo_sev_small_model,
                                    newdata = practice_data_freq_oryn[small_idx, ],
                                    type = "response")

predicted_sev[large_idx] <- predict(cargo_sev_large_model,
                                    newdata = practice_data_freq_oryn[large_idx, ],
                                    type = "response")

practice_data_freq_oryn<-practice_data_freq_oryn %>%
  mutate(predicted_freq=predicted_freq, predicted_sev=predicted_sev)

sev_dispersion<-ifelse(predicted_relativity==0,sev_dispersion_small,sev_dispersion_large)

practice_data_freq_oryn<-practice_data_freq_oryn %>%
  mutate(lambda=predicted_freq, shape=1/sev_dispersion, scale=predicted_sev*sev_dispersion)

n_sim <- 10000
n_rows <- nrow(practice_data_freq_oryn)
simulated_total_short_oryn_shock <- numeric(n_sim)
for (k in seq_len(n_sim)) {
  number_claims <- rpois(n_rows, practice_data_freq_oryn$lambda)
  idx <- which(number_claims > 0)
  if (length(idx) > 0) {
    shapes <- rep(practice_data_freq_oryn$shape[idx], number_claims[idx])
    scales <- rep(practice_data_freq_oryn$scale[idx], number_claims[idx])
    claim_size <- rgamma(length(shapes), shape = shapes, scale = scales)
    claim_size_limit <- pmax(0, pmin(claim_size, 1e6) - 1e5)
    simulated_total_short_oryn_shock[k] <- sum(claim_size_limit)
  }
}

simulated_total_short_oryn_shock








#stress test 2 -moderate scenario 
#helionis
#create synthetic shipment dataset
cargo_type_syn<-c()
cargo_value_syn<-c()
weight_syn<-c()
route_risk_syn<-c()
distance_syn<-c()
transit_duration_syn<-c()
pilot_experience_syn<-c()
vessel_age_syn<-c()
solar_radiation_syn<-c()
debris_density_syn<-c()
exposure_syn<-c()
container_type_syn<-c()


for (i in 1:nrow(helionis_fleet_data)){
  shipment_no<-rbinom(1,4,3.3*1.05/4)
  if(shipment_no==0)next #ships current iteration of the for loop if 0
  container_type_syn<-c(container_type_syn,rep(helionis_fleet_data$container_type[i],shipment_no))
  cargo_type_sampled<-sample(c("supplies","titanium","rare earths","cobalt", "lithium", "platinum"), shipment_no, replace=TRUE, prob=c(1/6,1/6,1/6,1/6,1/6,1/6))
  cargo_weight_sampled<-rep(0,shipment_no)
  for (l in 1:shipment_no) {
    cargo_type_current<-cargo_type_sampled[l]
    cargo_weight_sampled[l]<-weight_draw(container_type=helionis_fleet_data$container_type[i],length=1,cargo_type=cargo_type_current)
  }
  cargo_type_syn<-c(cargo_type_syn,cargo_type_sampled)
  weight_syn<-c(weight_syn,cargo_weight_sampled)
  route_risk_syn<-c(route_risk_syn,sample(c(3,4,5), shipment_no, replace=TRUE, prob=c(1/3,1/3,1/3)))
  distance_syn<-c(distance_syn,sample(table_cargo_freq$distance,size=shipment_no, replace=TRUE))
  transit_duration_syn<-c(transit_duration_syn,sample(table_cargo_freq$transit_duration,size=shipment_no, replace=TRUE))
  pilot_experience_syn<-c(pilot_experience_syn,sample(table_cargo_freq$pilot_experience,size=shipment_no, replace=TRUE))
  vessel_age_syn<-c(vessel_age_syn,sample(table_cargo_freq$vessel_age,size=shipment_no, replace=TRUE))
  solar_radiation_syn<-c(solar_radiation_syn,solar_radiation_draw(solar_system=helionis_fleet_data$solar_system[i],length=shipment_no))
  debris_density_syn<-c(debris_density_syn,debris_density_draw(solar_system=helionis_fleet_data$solar_system[i],length=shipment_no))
  exposure_syn<-c(exposure_syn,sample(table_cargo_freq$exposure,size=shipment_no, replace=TRUE))
}
practice_data_freq_helionis<-data.frame(container_type=as.factor(container_type_syn),cargo_type=as.factor(cargo_type_syn), weight=weight_syn, route_risk=as.factor(route_risk_syn), distance=distance_syn, transit_duration=transit_duration_syn, pilot_experience=pilot_experience_syn, vessel_age=vessel_age_syn, solar_radiation=solar_radiation_syn, debris_density=debris_density_syn, exposure=exposure_syn)




#introducing glm prediction
predicted_freq<-predict(cargo_freq_model,newdata=practice_data_freq_helionis, type="response")
predicted_relativity_prob<-predict(cargo_classifier_model, newdata=practice_data_freq_helionis,type="response")
predicted_relativity<-rbinom(nrow(practice_data_freq_helionis),1,predicted_relativity_prob)



predicted_sev<- rep(NA, nrow(practice_data_freq_helionis))

small_idx <- which(predicted_relativity == 0)
large_idx <- which(predicted_relativity == 1)

predicted_sev[small_idx] <- predict(cargo_sev_small_model,
                                    newdata = practice_data_freq_helionis[small_idx, ],
                                    type = "response")

predicted_sev[large_idx] <- predict(cargo_sev_large_model,
                                    newdata = practice_data_freq_helionis[large_idx, ],
                                    type = "response")



practice_data_freq_helionis<-practice_data_freq_helionis%>%
  mutate(predicted_freq=predicted_freq, predicted_sev=predicted_sev) 

sev_dispersion<-ifelse(predicted_relativity==0,sev_dispersion_small,sev_dispersion_large)

practice_data_freq_helionis<-practice_data_freq_helionis%>%
  mutate(lambda=predicted_freq,shape=1/sev_dispersion, scale=predicted_sev*sev_dispersion)





n_sim <- 10000
n_rows <- nrow(practice_data_freq_helionis)
simulated_total_short_helionis_moderate <- numeric(n_sim)
for (k in seq_len(n_sim)) {
  number_claims <- rpois(n_rows, practice_data_freq_helionis$lambda)
  idx <- which(number_claims > 0)
  if (length(idx) > 0) {
    # Repeat parameters according to number of claims
    shapes <- rep(practice_data_freq_helionis$shape[idx], number_claims[idx])
    scales <- rep(practice_data_freq_helionis$scale[idx], number_claims[idx])
    claim_size <- rgamma(length(shapes), shape = shapes, scale = scales)
    claim_size_limit <- pmax(0, pmin(claim_size, 1e6) - 1e5)
    simulated_total_short_helionis_moderate[k] <- sum(claim_size_limit)
  }
}

simulated_total_short_helionis_moderate

mean(simulated_total_short_helionis_moderate)
mean(simulated_total_short_helionis)






#bayesia
#creating synthetic shipment dataset
cargo_type_syn<-c()
cargo_value_syn<-c()
weight_syn<-c()
route_risk_syn<-c()
distance_syn<-c()
transit_duration_syn<-c()
pilot_experience_syn<-c()
vessel_age_syn<-c()
solar_radiation_syn<-c()
debris_density_syn<-c()
exposure_syn<-c()
container_type_syn<-c()


for (i in 1:nrow(bayesia_fleet_data)){
  shipment_no<-rbinom(1,3,2.2*1.05/3)
  if(shipment_no==0)next #ships current iteration of the for loop if 0
  container_type_syn<-c(container_type_syn,rep(bayesia_fleet_data$container_type[i],shipment_no))
  cargo_type_sampled<-sample(c("supplies","titanium","rare earths","cobalt", "lithium", "platinum"), shipment_no, replace=TRUE, prob=c(1/6,1/6,1/6,1/6,1/6,1/6))
  cargo_weight_sampled<-rep(0,shipment_no)
  for (l in 1:shipment_no) {
    cargo_type_current<-cargo_type_sampled[l]
    cargo_weight_sampled[l]<-weight_draw(container_type=bayesia_fleet_data$container_type[i],length=1,cargo_type=cargo_type_current)
  }
  cargo_type_syn<-c(cargo_type_syn,cargo_type_sampled)
  weight_syn<-c(weight_syn,cargo_weight_sampled)
  route_risk_syn<-c(route_risk_syn,sample(c(3,4,5), shipment_no, replace=TRUE, prob=c(1/3+0.22,1/3-0.07,1/3-0.15)))
  distance_syn<-c(distance_syn,sample(table_cargo_freq$distance,size=shipment_no, replace=TRUE))
  transit_duration_syn<-c(transit_duration_syn,sample(table_cargo_freq$transit_duration,size=shipment_no, replace=TRUE))
  pilot_experience_syn<-c(pilot_experience_syn,sample(table_cargo_freq$pilot_experience,size=shipment_no, replace=TRUE))
  vessel_age_syn<-c(vessel_age_syn,sample(table_cargo_freq$vessel_age,size=shipment_no, replace=TRUE))
  solar_radiation_syn<-c(solar_radiation_syn,solar_radiation_draw(solar_system=bayesia_fleet_data$solar_system[i],length=shipment_no))
  debris_density_syn<-c(debris_density_syn,debris_density_draw(solar_system=bayesia_fleet_data$solar_system[i],length=shipment_no))
  exposure_syn<-c(exposure_syn,sample(table_cargo_freq$exposure,size=shipment_no, replace=TRUE))
}
practice_data_freq_bayesia<-data.frame(container_type=as.factor(container_type_syn),cargo_type=as.factor(cargo_type_syn), weight=weight_syn, route_risk=as.factor(route_risk_syn), distance=distance_syn, transit_duration=transit_duration_syn, pilot_experience=pilot_experience_syn, vessel_age=vessel_age_syn, solar_radiation=solar_radiation_syn, debris_density=debris_density_syn, exposure=exposure_syn)


#introducing glm prediction
predicted_freq<-predict(cargo_freq_model,newdata=practice_data_freq_bayesia, type="response")
predicted_relativity_prob<-predict(cargo_classifier_model, newdata=practice_data_freq_bayesia,type="response")
predicted_relativity<-rbinom(nrow(practice_data_freq_bayesia),1,predicted_relativity_prob)


predicted_sev<- rep(NA, nrow(practice_data_freq_bayesia))

small_idx <- which(predicted_relativity == 0)
large_idx <- which(predicted_relativity == 1)

predicted_sev[small_idx] <- predict(cargo_sev_small_model,
                                    newdata = practice_data_freq_bayesia[small_idx, ],
                                    type = "response")

predicted_sev[large_idx] <- predict(cargo_sev_large_model,
                                    newdata = practice_data_freq_bayesia[large_idx, ],
                                    type = "response")


practice_data_freq_bayesia<-practice_data_freq_bayesia%>%
  mutate(predicted_freq=predicted_freq, predicted_sev=predicted_sev) 

sev_dispersion<-ifelse(predicted_relativity==0,sev_dispersion_small,sev_dispersion_large)

practice_data_freq_bayesia<-practice_data_freq_bayesia%>%
  mutate(lambda=predicted_freq,shape=1/sev_dispersion, scale=predicted_sev*sev_dispersion)


n_sim <- 10000
n_rows <- nrow(practice_data_freq_bayesia)
simulated_total_short_bayesia_moderate <- numeric(n_sim)
for (k in seq_len(n_sim)) {
  number_claims <- rpois(n_rows, practice_data_freq_bayesia$lambda)
  idx <- which(number_claims > 0)
  if (length(idx) > 0) {
    # Repeat parameters according to number of claims
    shapes <- rep(practice_data_freq_bayesia$shape[idx], number_claims[idx])
    scales <- rep(practice_data_freq_bayesia$scale[idx], number_claims[idx])
    claim_size <- rgamma(length(shapes), shape = shapes, scale = scales)
    claim_size_limit <- pmax(0, pmin(claim_size, 1e6) - 1e5)
    simulated_total_short_bayesia_moderate[k] <- sum(claim_size_limit)
  }
}

simulated_total_short_bayesia_moderate











#oryn
#create synthetic shipment dataset
cargo_type_syn<-c()
cargo_value_syn<-c()
weight_syn<-c()
route_risk_syn<-c()
distance_syn<-c()
transit_duration_syn<-c()
pilot_experience_syn<-c()
vessel_age_syn<-c()
solar_radiation_syn<-c()
debris_density_syn<-c()
exposure_syn<-c()
container_type_syn<-c()


for (i in 1:nrow(oryn_fleet_data)){
  shipment_no<-rbinom(1,2,1.05*1.5/2)
  if(shipment_no==0)next #skip current iteration if 0
  
  container_type_syn<-c(container_type_syn,rep(oryn_fleet_data$container_type[i],shipment_no))
  cargo_type_sampled<-sample(c("supplies","titanium","rare earths","cobalt", "lithium", "platinum"), shipment_no, replace=TRUE, prob=c(1/6,1/6,1/6,1/6,1/6,1/6))
  cargo_weight_sampled<-rep(0,shipment_no)
  for (l in 1:shipment_no) {
    cargo_type_current<-cargo_type_sampled[l]
    cargo_weight_sampled[l]<-weight_draw(container_type=oryn_fleet_data$container_type[i],length=1,cargo_type=cargo_type_current)
  }
  cargo_type_syn<-c(cargo_type_syn,cargo_type_sampled)
  weight_syn<-c(weight_syn,cargo_weight_sampled)
  route_risk_syn<-c(route_risk_syn,sample(c(3,4,5), shipment_no, replace=TRUE, prob=c(1/3,1/3,1/3)))
  distance_syn<-c(distance_syn,sample(table_cargo_freq$distance,size=shipment_no, replace=TRUE))
  transit_duration_syn<-c(transit_duration_syn,sample(table_cargo_freq$transit_duration,size=shipment_no, replace=TRUE))
  pilot_experience_syn<-c(pilot_experience_syn,sample(table_cargo_freq$pilot_experience,size=shipment_no, replace=TRUE))
  vessel_age_syn<-c(vessel_age_syn,sample(table_cargo_freq$vessel_age,size=shipment_no, replace=TRUE))
  solar_radiation_syn<-c(solar_radiation_syn,solar_radiation_draw(solar_system=oryn_fleet_data$solar_system[i],length=shipment_no))
  debris_density_syn<-c(debris_density_syn,debris_density_draw(solar_system=oryn_fleet_data$solar_system[i],length=shipment_no))
  exposure_syn<-c(exposure_syn,sample(table_cargo_freq$exposure,size=shipment_no, replace=TRUE))
}

practice_data_freq_oryn<-data.frame(
  container_type=as.factor(container_type_syn),
  cargo_type=as.factor(cargo_type_syn),
  weight=weight_syn,
  route_risk=as.factor(route_risk_syn),
  distance=distance_syn,
  transit_duration=transit_duration_syn,
  pilot_experience=pilot_experience_syn,
  vessel_age=vessel_age_syn,
  solar_radiation=solar_radiation_syn,
  debris_density=debris_density_syn,
  exposure=exposure_syn
)

# GLM prediction
predicted_freq<-predict(cargo_freq_model,newdata=practice_data_freq_oryn, type="response")
predicted_relativity_prob<-predict(cargo_classifier_model, newdata=practice_data_freq_oryn,type="response")
predicted_relativity<-rbinom(nrow(practice_data_freq_oryn),1,predicted_relativity_prob)

predicted_sev<- rep(NA, nrow(practice_data_freq_oryn))

small_idx <- which(predicted_relativity == 0)
large_idx <- which(predicted_relativity == 1)

predicted_sev[small_idx] <- predict(cargo_sev_small_model,
                                    newdata = practice_data_freq_oryn[small_idx, ],
                                    type = "response")

predicted_sev[large_idx] <- predict(cargo_sev_large_model,
                                    newdata = practice_data_freq_oryn[large_idx, ],
                                    type = "response")

practice_data_freq_oryn<-practice_data_freq_oryn %>%
  mutate(predicted_freq=predicted_freq, predicted_sev=predicted_sev)

sev_dispersion<-ifelse(predicted_relativity==0,sev_dispersion_small,sev_dispersion_large)

practice_data_freq_oryn<-practice_data_freq_oryn %>%
  mutate(lambda=predicted_freq, shape=1/sev_dispersion, scale=predicted_sev*sev_dispersion)

n_sim <- 10000
n_rows <- nrow(practice_data_freq_oryn)
simulated_total_short_oryn_moderate <- numeric(n_sim)
for (k in seq_len(n_sim)) {
  number_claims <- rpois(n_rows, practice_data_freq_oryn$lambda)
  idx <- which(number_claims > 0)
  if (length(idx) > 0) {
    shapes <- rep(practice_data_freq_oryn$shape[idx], number_claims[idx])
    scales <- rep(practice_data_freq_oryn$scale[idx], number_claims[idx])
    claim_size <- rgamma(length(shapes), shape = shapes, scale = scales)
    claim_size_limit <- pmax(0, pmin(claim_size, 1e6) - 1e5)
    simulated_total_short_oryn_moderate[k] <- sum(claim_size_limit)
  }
}

simulated_total_short_oryn_moderate




#-----------------------------premium calculations and simulation statistics-------------------------------------------#

#Premiums

#helionis

buhlmann_helionis_premium<-sum(buhlmann_helionis)
expected_pv_loss_helionis<-(buhlmann_helionis_premium+0.12*(mean(simulated_total_short_helionis_shock)-mean(simulated_total_short_helionis))+0.18*(mean(simulated_total_short_helionis_moderate)-mean(simulated_total_short_helionis)))/interest[1]


standard_dev_helionis<-sd(simulated_total_short_helionis)
cost_of_capital_helionis<-0.0746*(as.numeric(quantile(simulated_total_short_helionis,0.995))-mean(simulated_total_short_helionis)) 


premium_helionis<-(expected_pv_loss_helionis+1.5*standard_dev_helionis+cost_of_capital_helionis)/0.95





#bayesia


buhlmann_bayesia_premium <- sum(buhlmann_bayesia)
expected_pv_loss_bayesia <- (buhlmann_bayesia_premium + 0.1 * (mean(simulated_total_short_bayesia_shock) - mean(simulated_total_short_bayesia))+0.2 * (mean(simulated_total_short_bayesia_moderate) - mean(simulated_total_short_bayesia))) / interest[1]

standard_dev_bayesia <- sd(simulated_total_short_bayesia)
cost_of_capital_bayesia <- 0.0746 * (as.numeric(quantile(simulated_total_short_bayesia, 0.995)) - mean(simulated_total_short_bayesia))

premium_bayesia <- (expected_pv_loss_bayesia + 1.5 * standard_dev_bayesia + cost_of_capital_bayesia) / 0.95






#oryn
buhlmann_oryn_premium <- sum(buhlmann_oryn)
expected_pv_loss_oryn <- (buhlmann_oryn_premium + 0.12 * (mean(simulated_total_short_oryn_shock) - mean(simulated_total_short_oryn))+0.18 * (mean(simulated_total_short_oryn_moderate) - mean(simulated_total_short_oryn))) / interest[1]

standard_dev_oryn <- sd(simulated_total_short_oryn)
cost_of_capital_oryn <- 0.0746 * (as.numeric(quantile(simulated_total_short_oryn, 0.995)) - mean(simulated_total_short_oryn))

premium_oryn <- (expected_pv_loss_oryn + 1.5 * standard_dev_oryn + cost_of_capital_oryn) / 0.95


premium<-premium_helionis+premium_bayesia+premium_oryn


#random helpful stat-summary of my short-term


quantile(practice_data_freq_helionis$predicted_sev, probs = seq(0.1, 0.9, by = 0.1))

quantile(practice_data_freq_bayesia$predicted_sev, probs = seq(0.1, 0.9, by = 0.1))

quantile(practice_data_freq_oryn$predicted_sev, probs = seq(0.1, 0.9, by = 0.1))

quantile(table_cargo_sev$claim_amount, probs = seq(0.1, 0.9, by = 0.1))


simulated_total_short_helionis
simulated_total_short_bayesia
simulated_total_short_oryn

mean(simulated_total_short_helionis)
mean(simulated_total_short_bayesia)
mean(simulated_total_short_oryn)
min(table_cargo_sev$claim_amount)





#returns,costs, net revenue etc.
#short-term
#costs

simulated_total_short<-simulated_total_short_helionis+simulated_total_short_bayesia+simulated_total_short_oryn


  #expected value
mean(simulated_total_short_helionis)
mean(simulated_total_short_bayesia)
mean(simulated_total_short_oryn)
mean(simulated_total_short)


   #standard deviation
sd(simulated_total_short_helionis)
sd(simulated_total_short_bayesia)
sd(simulated_total_short_oryn)
sd(simulated_total_short)

  #var
quantile(simulated_total_short_helionis,probs=c(0.95,0.99,0.995))
quantile(simulated_total_short_bayesia,probs=c(0.95,0.99,0.995))
quantile(simulated_total_short_oryn,probs=c(0.95,0.99,0.995))
quantile(simulated_total_short, probs=c(0.95,0.99,0.995))

  #expected shortfall
mean(simulated_total_short_helionis[simulated_total_short_helionis>quantile(simulated_total_short_helionis,0.95)])
mean(simulated_total_short_helionis[simulated_total_short_helionis>quantile(simulated_total_short_helionis,0.99)])


mean(simulated_total_short_bayesia[simulated_total_short_bayesia > quantile(simulated_total_short_bayesia, 0.95)])
mean(simulated_total_short_bayesia[simulated_total_short_bayesia > quantile(simulated_total_short_bayesia, 0.99)])

mean(simulated_total_short_oryn[simulated_total_short_oryn > quantile(simulated_total_short_oryn, 0.95)])
mean(simulated_total_short_oryn[simulated_total_short_oryn > quantile(simulated_total_short_oryn, 0.99)])


mean(simulated_total_short[simulated_total_short > quantile(simulated_total_short, 0.95)])
mean(simulated_total_short[simulated_total_short > quantile(simulated_total_short, 0.99)])


#return-multiply by 0.95 since operation expense which was loaded into premium is still expense
#expected
0.95*premium_helionis-mean(simulated_total_short_helionis)
0.95*premium_bayesia-mean(simulated_total_short_bayesia)
0.95*premium_oryn-mean(simulated_total_short_oryn)
0.95*premium-mean(simulated_total_short)


#var

quantile(0.95*premium_helionis-simulated_total_short_helionis,probs=c(0.05,0.01,0.005))
quantile(0.95*premium_bayesia-simulated_total_short_bayesia,probs=c(0.05,0.01,0.005))
quantile(0.95*premium_oryn-simulated_total_short_oryn,probs=c(0.05,0.01,0.005))
quantile(0.95*premium-simulated_total_short,probs=c(0.05,0.01,0.005))



#no.siutations unproftiable
sum(0.95*premium_helionis-simulated_total_short_helionis<0)
sum(0.95*premium_bayesia-simulated_total_short_bayesia<0)
sum(0.95*premium_oryn-simulated_total_short_oryn<0)
sum(0.95*premium-simulated_total_short<0)




#long-term
#cost
simulated_total_10_years<-simulated_total_10_years_bayesia+simulated_total_10_years_helionis+simulated_total_10_years_oryn


# expected value
mean(simulated_total_10_years_helionis)
mean(simulated_total_10_years_bayesia)
mean(simulated_total_10_years_oryn)
mean(simulated_total_10_years)

# standard deviation
sd(simulated_total_10_years_helionis)
sd(simulated_total_10_years_bayesia)
sd(simulated_total_10_years_oryn)
sd(simulated_total_10_years)

# var
quantile(simulated_total_10_years_helionis,probs=c(0.95,0.99,0.995))
quantile(simulated_total_10_years_bayesia,probs=c(0.95,0.99,0.995))
quantile(simulated_total_10_years_oryn,probs=c(0.95,0.99,0.995))
quantile(simulated_total_10_years, probs=c(0.95,0.99,0.995))

# expected shortfall
mean(simulated_total_10_years_helionis[simulated_total_10_years_helionis>quantile(simulated_total_10_years_helionis,0.95)])
mean(simulated_total_10_years_helionis[simulated_total_10_years_helionis>quantile(simulated_total_10_years_helionis,0.99)])

mean(simulated_total_10_years_bayesia[simulated_total_10_years_bayesia > quantile(simulated_total_10_years_bayesia, 0.95)])
mean(simulated_total_10_years_bayesia[simulated_total_10_years_bayesia > quantile(simulated_total_10_years_bayesia, 0.99)])

mean(simulated_total_10_years_oryn[simulated_total_10_years_oryn > quantile(simulated_total_10_years_oryn, 0.95)])
mean(simulated_total_10_years_oryn[simulated_total_10_years_oryn > quantile(simulated_total_10_years_oryn, 0.99)])

mean(simulated_total_10_years[simulated_total_10_years > quantile(simulated_total_10_years, 0.95)])
mean(simulated_total_10_years[simulated_total_10_years > quantile(simulated_total_10_years, 0.99)])

      # return-multiply by 0.95 since operation expense which was loaded into premium is still expense

#return
helionis_premium_10_years<-sum(premium_helionis*inflation/interest)
bayesia_premium_10_years<-sum(premium_bayesia*inflation/interest)
oryn_premium_10_years<-sum(premium_oryn*inflation/interest)

premium_10_years<-sum(helionis_premium_10_years,bayesia_premium_10_years,oryn_premium_10_years)






#stress test- moderate 
simulated_total_short_moderate<-simulated_total_short_helionis_moderate+simulated_total_short_bayesia_moderate+simulated_total_short_oryn_moderate



mean(simulated_total_short_helionis_moderate)
mean(simulated_total_short_bayesia_moderate)
mean(simulated_total_short_oryn_moderate)
mean(simulated_total_short_moderate)



sd(simulated_total_short_helionis_moderate)
sd(simulated_total_short_bayesia_moderate)
sd(simulated_total_short_oryn_moderate)
sd(simulated_total_short_moderate)


#var
quantile(simulated_total_short_helionis_moderate,probs=c(0.95,0.99,0.995))
quantile(simulated_total_short_bayesia_moderate,probs=c(0.95,0.99,0.995))
quantile(simulated_total_short_oryn_moderate,probs=c(0.95,0.99,0.995))
quantile(simulated_total_short_moderate,probs=c(0.95,0.99,0.995))




#no.situations unprofitable
sum(premium_helionis-simulated_total_short_helionis_moderate<0)
sum(premium_bayesia-simulated_total_short_bayesia_moderate<0)
sum(premium_oryn-simulated_total_short_oryn_moderate<0)
sum(premium-simulated_total_short_moderate<0)






#stress test-extreme

simulated_total_short_shock<-simulated_total_short_helionis_shock+simulated_total_short_bayesia_shock+simulated_total_short_oryn_shock


mean(simulated_total_short_helionis_shock)
mean(simulated_total_short_bayesia_shock)
mean(simulated_total_short_oryn_shock)
mean(simulated_total_short_shock)


sd(simulated_total_short_helionis_shock)
sd(simulated_total_short_bayesia_shock)
sd(simulated_total_short_oryn_shock)
sd(simulated_total_short_shock)




#var
quantile(simulated_total_short_helionis_shock,probs=c(0.95,0.99,0.995))
quantile(simulated_total_short_bayesia_shock,probs=c(0.95,0.99,0.995))
quantile(simulated_total_short_oryn_shock,probs=c(0.95,0.99,0.995))
quantile(simulated_total_short_shock,probs=c(0.95,0.99,0.995))





#no.situations unproftiable
sum(premium_helionis-simulated_total_short_helionis_shock<0)
sum(premium_bayesia-simulated_total_short_bayesia_shock<0)
sum(premium_oryn-simulated_total_short_oryn_shock<0)
sum(premium-simulated_total_short_shock<0)



