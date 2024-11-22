##DCE test

rm(list=ls())
library(data.table)
database<-fread("data/dataVossler_wide.csv")
library("readr")
library("psych")
library("evd")          
library("apollo")
library("dplyr")   
library("tidyr")
library("tidylog")
library("readxl")

## Estimate a conditional logit model
# Having generated the dataset, we can now estimate a model. Here we use apollo to estimate a conditional logit model.


apollo_initialise()

modelOutput_settings = list(printPVal=T)

### Set core controls

apollo_control = list(
  
  modelName  ="Simulated Data",
  modelDescr ="Simple MNL model",
  indivID    ="RespondentID",
  mixing     = TRUE,
  nCores     = 4
)

apollo_beta=c(
  b_cost =0,
  meanasc_1=1,
  mean_wqhuc8 =0,
  mean_wqhuc4 =0,
  mean_wmed=0,
  mean_wlar=0,
  mean_wq4nl =0,
  mean_wmednl=0,
  sigmaasc_1=0,
  sigma_wqhuc8 =0,
  sigma_wqhuc4 =0,
  sigma_wmed=0,
  sigma_wlar=0,
  sigma_wq4nl =0,
  sigma_wmednl=0
  
)

# Ledavgift, Slätt, bitvisstökig,opreparerad, maskin smal längd höjdst platå ner
### keine Parameter fix halten

apollo_fixed = c()

apollo_draws = list(
  interDrawsType = "halton",
  interNDraws    = 1000,
  interUnifDraws = c(),
  interNormDraws = c( "drawsasc_1", "drawsasc_0","draws_wqhuc8",
                      "draws_wqhuc4",
                      "draws_wmed",
                      "draws_wlar", 
                      "draws_wq4nl","draws_wmednl")
)
### Create random parameters
apollo_randCoeff = function(apollo_beta, apollo_inputs){
  randcoeff = list()
  randcoeff[["basc_1"]] = (meanasc_1 + sigmaasc_1*drawsasc_1)
  randcoeff[["b_wqhuc8"]] = (mean_wqhuc8 + sigma_wqhuc8*draws_wqhuc8)
  randcoeff[["b_wqhuc4"]] = (mean_wqhuc4 + sigma_wqhuc4*draws_wqhuc4)
  randcoeff[["b_wmed"]] = (mean_wmed + sigma_wmed*draws_wmed)
  randcoeff[["b_wlar"]] = (mean_wlar + sigma_wlar*draws_wlar)
  randcoeff[["b_wq4nl"]] = (mean_wq4nl + sigma_wq4nl*draws_wq4nl)
  randcoeff[["b_wmednl"]] = (mean_wmednl + sigma_wmednl*draws_wmednl)
  return(randcoeff)
}

### validieren

apollo_inputs = apollo_validateInputs()

## Several observations per individual detected based on the value of RID.Setting panelData in apollo_control set to TRUE. All checks on apollo_control completed. All checks on database completed.

apollo_probabilities =function(apollo_beta, apollo_inputs, functionality="estimate"){  ### Function initialisation: do not change the following three commands ### Attach inputs and detach after function exit
  apollo_attach(apollo_beta, apollo_inputs)
  on.exit(apollo_detach(apollo_beta, apollo_inputs)) ### Create list of probabilities P
  P = list() ### List of utilities (later integrated in mnl_settings below), they must be added in the beta and 
  V = list()
  V[['alt0']] = b_cost*Cost_0+b_wqhuc8*WQ_HUC8_0+b_wqhuc4*WQ_HUC4_0+b_wmed*WQ_Medium_0+b_wlar*WQ_Large_0+b_wq4nl *WQ_HUC4_NL_0+ b_wmednl*WQ_Medium_NL_0
  V[['alt1']] = basc_1*1+b_cost*Cost_1 + b_wqhuc8*WQ_HUC8_1+b_wqhuc4*WQ_HUC4_1+b_wmed*WQ_Medium_1+b_wlar*WQ_Large_1+b_wq4nl *WQ_HUC4_NL_1+ b_wmednl*WQ_Medium_NL_1

### Define settings for MNL model component
mnl_settings = list(
    alternatives  = c(alt0=0, alt1=1) ,
    avail      = 1, # all alternatives are available in every choice
    choiceVar     =choice,
    V             = V  # tell function to use list vector defined above
  )

### Compute probabilities using MNL model
  
P[['model']] = apollo_mnl(mnl_settings, functionality) ### Take product across observation for same individual
P = apollo_panelProd(P, apollo_inputs, functionality) ### Average across inter-individual draws - nur bei Mixed Logit!
P = apollo_avgInterDraws(P, apollo_inputs, functionality) ### Prepare and return outputs of function
P = apollo_prepareProb(P, apollo_inputs, functionality)
return(P)
  
}

#HERE the function closed
model = apollo_estimate(apollo_beta, apollo_fixed,
                         apollo_probabilities, apollo_inputs,
                         estimate_settings=list(hessianRoutine="maxLik"))
apollo_modelOutput(model)

apollo_saveOutput(model)

###wtp
coefficients= as.list(model$estimate)
wtp_wq8<- -coefficients$mean_wqhuc8/coefficients$b_cost

wtp_wq8

x = rnorm(N, mean= model$estimate['mu_essential_repair'], sd = abs(model$estimate['sigma_essential_repair']))
y = -exp(rlnorm(N, mean= model$estimate['mu_price'], sd = abs(model$estimate['sigma_price'])))
wtp = x/y
mean(wtp)

