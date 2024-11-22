##DCE test

rm(list=ls())

library(readxl)

# Correct file path with forward slashes or escaped backslashes
file_path <- "C:/Users/louiemarie/OneDrive/Desktop/PhD/Course Materials/dce/DCEreplication/Data/dataVossler_wide.xlsx"
database <- read_excel(file_path)
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
  meanasc_1=0,
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
  interNDraws    = 500,
  interUnifDraws = c(),
  interNormDraws = c( "drawsasc_1","draws_wqhuc8",
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


# Extract coefficients from the model
coefficients = as.list(model$estimate)

### Calculate WTP for local sub-watershed (HUC8)

# Increase one BCG level across policy area
wtp_wq8 <- -((coefficients$meanasc_1 + coefficients$mean_wqhuc8 * (database$HUC8BCG - 1)) - 
               (coefficients$mean_wqhuc8 * database$HUC8BCG)) / coefficients$b_cost
mean_wtp_wq8 <- mean(wtp_wq8)

# Scenario 2: Minimum BCG Level 2 ("swimmable")
wtp__wq8_bcg2 <- -((coefficients$meanasc_1 + coefficients$mean_wqhuc8  * 2) - (coefficients$mean_wqhuc8  * database$HUC8BCG)) / coefficients$b_cost
mean_wtp_wq8_bcg2 <- mean(wtp__wq8_bcg2) 

# Scenario 3: Cap BCG Level at 3
wtp_wq8_bcg3 <- -((coefficients$meanasc_1 + coefficients$mean_wqhuc8  * pmin(database$HUC8BCG, 3)) - (coefficients$mean_wqhuc8  * database$HUC8BCG)) / coefficients$b_cost
mean_wtp_wq8_bcg3 <- mean(wtp_wq8_bcg3) 

### Calculate WTP for local watershed (HUC4)

# Increase one BCG level across policy area
wtp_wq4 <- -((coefficients$meanasc_1 + coefficients$mean_wqhuc4  * (database$HUC4BCG_base - 1) + coefficients$mean_wqhuc8  * (database$HUC8BCG - 1)) - 
               (coefficients$mean_wqhuc4 * database$HUC4BCG_base + coefficients$mean_wqhuc8 * database$HUC8BCG)) / coefficients$b_cost
mean_wtp_wq4 <- mean(wtp_wq4) 

## Minimum BCG Level 2 ("swimmable")

wtp_wq4_bcg2 <- -((coefficients$meanasc_1 + coefficients$mean_wqhuc4 *database$HUC4BCG_scenario1 + coefficients$mean_wqhuc8  * 2) - 
                    (coefficients$mean_wqhuc4 * database$HUC4BCG_base + coefficients$mean_wqhuc8*database$HUC8BCG)) / coefficients$b_cost
mean_wtp_wq4_bcg2 <- mean(wtp_wq4_bcg2)

# Scenario 3: Cap BCG Level at 3

wtp_wq4_bcg3 <- -((coefficients$meanasc_1 + coefficients$mean_wqhuc4 * database$HUC4BCG_scenario2 + coefficients$mean_wqhuc8 * pmin(database$HUC8BCG, 3)) - 
                    (coefficients$mean_wqhuc4 * database$HUC4BCG_base + coefficients$mean_wqhuc8 * database$HUC8BCG)) / coefficients$b_cost

mean_wtp_wq4_bcg3<-mean(wtp_wq4_bcg3)

## Calculate WTP for local 3 Watersheds (3 HUC4s)

# Scenario 1: One BCG level increase
wtp_wqmed<- -((coefficients$meanasc_1 + coefficients$mean_wmed * (database$MediumBCG_base - 1) + coefficients$mean_wqhuc8 * (database$HUC8BCG - 1)) -
                (coefficients$mean_wmed * database$MediumBCG_base + coefficients$mean_wqhuc8 * database$HUC8BCG)) / coefficients$b_cost

# Get the average WTP value for this scenario
mean_wtp_wqmed <- mean(wtp_wqmed)

# Scenario 2: Calculate WTP for setting Minimum BCG Level to 2 ("swimmable")
wtp_wqmed_bcg2 <- -((coefficients$meanasc_1 + coefficients$mean_wmed * database$MediumBCG_scenario1 + coefficients$mean_wqhuc8 * 2) -
                      (coefficients$mean_wmed * database$MediumBCG_base + coefficients$mean_wqhuc8 * database$HUC8BCG)) / coefficients$b_cost
mean_wtp_wqmed_bcg2 <- mean(wtp_wqmed_bcg2)

wtp_wqmed_bcg3 <- -((coefficients$meanasc_1 + coefficients$mean_wmed * database$MediumBCG_scenario2 + coefficients$mean_wqhuc8 * pmin(database$HUC8BCG, 3)) -
                      (coefficients$mean_wmed * database$MediumBCG_base + coefficients$mean_wqhuc8 * database$HUC8BCG)) / coefficients$b_cost
mean_wtp_wqmed_bcg3 <- mean(wtp_wqmed_bcg3)

## Scenario 4

# Calculate WTP for increasing one BCG level across the policy area
wtp_wqlar <- -((coefficients$meanasc_1 + coefficients$mean_wlar * 2.52 + coefficients$mean_wqhuc8 * (database$HUC8BCG - 1)) -
                 (coefficients$mean_wlar * 3.52 + coefficients$mean_wqhuc8 * database$HUC8BCG)) / coefficients$b_cost
mean_wtp_wqlar<-mean(wtp_wqlar)

# Calculate WTP for setting Minimum BCG Level to 2 ("swimmable")
wtp_wqlar_bcg2 <- -((coefficients$meanasc_1 + coefficients$mean_wlar * 2 + coefficients$mean_wqhuc8 * 2) -
                      (coefficients$mean_wlar * 3.52 + coefficients$mean_wqhuc8 * database$HUC8BCG)) / coefficients$b_cost
mean_wtp_wqlar_bcg2 <- mean(wtp_wqlar_bcg2)

# Calculate WTP for setting Minimum BCG Level to 3 ("biological")
wtp_wqlar_bcg3 <- -((coefficients$meanasc_1 + coefficients$mean_wlar * (3.52 - 0.59) + coefficients$mean_wqhuc8 * pmin(database$HUC8BCG, 3)) -
                      (coefficients$mean_wlar * 3.52 + coefficients$mean_wqhuc8 * database$HUC8BCG)) / coefficients$b_cost
mean_wtp_wqlar_bcg3 <- mean(wtp_wqlar_bcg3)


## Scenario 5: Non-local

# Calculate WTP for increasing one BCG level across policy area for non-local watershed (HUC4)
wtp_wq4nl <- -((coefficients$meanasc_1 + coefficients$mean_wq4nl * (database$HUC4BCG_base_NL - 1)) -
                 (coefficients$mean_wq4nl * database$HUC4BCG_base_NL)) / coefficients$b_cost
mean_wtp_wq4nl <- mean(wtp_wq4nl)

# Calculate WTP for Minimum BCG Level 2 ("swimmable") for non-local watershed (HUC4)
wtp_wq4nl_bcg2 <- -((coefficients$meanasc_1 + coefficients$mean_wq4nl * 2) -
                      (coefficients$mean_wq4nl * database$HUC4BCG_base_NL)) / coefficients$b_cost
mean_wtp_wq4nl_bcg2 <- mean(wtp_wq4nl_bcg2)

# Calculate WTP for Minimum BCG Level 3 ("biological") for non-local watershed (HUC4)
wtp_wq4nl_bcg3 <- -((coefficients$meanasc_1 + coefficients$mean_wq4nl * database$HUC4BCG_scenario2_NL) -
                      (coefficients$mean_wq4nl * database$HUC4BCG_base_NL)) / coefficients$b_cost
mean_wtp_wq4nl_bcg3<-mean(wtp_wq4nl_bcg3)

## Scenario 6: Nonlocal for 3 Watershed
#Calculate WTP for increasing one BCG level across policy area for non-local 3 watersheds (3 HUC4s)
wtp_wqmednl <- -((coefficients$meanasc_1 + coefficients$mean_wmednl * (database$MediumBCG_base_NL - 1)) -
                   (coefficients$mean_wmednl * database$MediumBCG_base_NL)) / coefficients$b_cost
mean_wtp_wqmednl <- mean(wtp_wqmednl)

# Calculate WTP for Minimum BCG Level 2 ("swimmable") for non-local 3 watersheds (3 HUC4s)
wtp_wqmednl_bcg2 <- -((coefficients$meanasc_1 + coefficients$mean_wmednl * 2) -
                        (coefficients$mean_wmednl * database$MediumBCG_base_NL)) / coefficients$b_cost
mean_wtp_wqmednl_bcg2 <- mean(wtp_wqmednl_bcg2)


# Calculate WTP for Minimum BCG Level 3 ("biological") for non-local 3 watersheds (3 HUC4s)
wtp_wqmednl_bcg3 <- -((coefficients$meanasc_1 + coefficients$mean_wmednl * database$MediumBCG_scenario2_NL) -
                        (coefficients$mean_wmednl * database$MediumBCG_base_NL)) / coefficients$b_cost

# Get the average WTP value for this scenario
mean_wtp_wqmednl_bcg3 <- mean(wtp_wqmednl_bcg3)

# Summarizing the result in a matrix (Table 2)
mean_wtp_matrix <- matrix(
  c(mean_wtp_wq8, mean_wtp_wq4, mean_wtp_wqmed, mean_wtp_wqlar, mean_wtp_wq4nl, mean_wtp_wqmednl, 
    mean_wtp_wq8_bcg2, mean_wtp_wq4_bcg2, mean_wtp_wqmed_bcg2, mean_wtp_wqlar_bcg2, mean_wtp_wq4nl_bcg2, mean_wtp_wqmednl_bcg2,
    mean_wtp_wq8_bcg3, mean_wtp_wq4_bcg3, mean_wtp_wqmed_bcg3, mean_wtp_wqlar_bcg3, mean_wtp_wq4nl_bcg3, mean_wtp_wqmednl_bcg3),
  nrow = 3, byrow = TRUE
)

# Assign column and row names
colnames(mean_wtp_matrix) <- c("Subwatershed", "Watershed", "3 Watersheds", "Study region", "Non-Local Watershed", "Non-Local 3 Watersheds")
rownames(mean_wtp_matrix) <- c("One-level BCG Improvement", 
                               "Minimum BCG Level 2 ('swimmable')", 
                               "Minimum BCG Level 3 ('biological')")

# Print the matrix
print(mean_wtp_matrix)