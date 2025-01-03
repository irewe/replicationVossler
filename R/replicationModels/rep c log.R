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
  
  indivID    ="RespondentID"
  
)


apollo_beta=c(
  b_1=0)


# Ledavgift, Slätt, bitvisstökig,opreparerad, maskin smal längd höjdst platå ner
### keine Parameter fix halten

apollo_fixed = c()


### validieren

apollo_inputs = apollo_validateInputs()

## Several observations per individual detected based on the value of RID.Setting panelData in apollo_control set to TRUE. All checks on apollo_control completed. All checks on database completed.

apollo_probabilities =function(apollo_beta, apollo_inputs, functionality="estimate"){
  
  ### Function initialisation: do not change the following three commands
  
  ### Attach inputs and detach after function exit
  
  apollo_attach(apollo_beta, apollo_inputs)
  
  on.exit(apollo_detach(apollo_beta, apollo_inputs))
  
  ### Create list of probabilities P
  
  P = list()
  
  ### List of utilities (later integrated in mnl_settings below), they must be added in the beta and 
  
  V = list()
  
  V[['alt1']] = b_1
  V[['alt0']] = 0
  
  ### Define settings for MNL model component
  
  mnl_settings = list(
    
    alternatives  = c( alt1=1, alt0=0) ,
    avail      = 1, # all alternatives are available in every choice
    choiceVar     =choice,
    V             = V  # tell function to use list vector defined above
  )
  
  ### Compute probabilities using MNL model
  
  P[['model']] = apollo_mnl(mnl_settings, functionality)
  
  ### Take product across observation for same individual
  
  P = apollo_panelProd(P, apollo_inputs, functionality)
  
  ### Average across inter-individual draws - nur bei Mixed Logit!
  
  ### P = apollo_avgInterDraws(P, apollo_inputs, functionality)
  
  ### Prepare and return outputs of function
  
  P = apollo_prepareProb(P, apollo_inputs, functionality)
  
  return(P)
  
}

#HERE the function closed


model1 = apollo_estimate(apollo_beta, apollo_fixed,
                         
                         apollo_probabilities, apollo_inputs,
                         
                         estimate_settings=list(hessianRoutine="maxLik"))