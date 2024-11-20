# Load Apollo library

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


apollo_initialise()
modelOutput_settings = list(printPVal=T)

# Define Apollo control settings
apollo_control <- list(
  modelName       = "Random Parameters Model",
  modelDescription = "Model with Specified Utility Functions",
  indivID         = "RespondentID",  # Adjust based on your dataset
  mixing          = TRUE,
  nCores          = 4
)

# Initialize the Apollo model


# Define model parameters and constraints
apollo_beta <- c(
  mu_b1      = 0,
  sigma_b1   = 1,
  b_wq8      = 0,
  b_wq4      = 0,
  b_wmed     = 0,
  b_wlar     = 0,
  b_wq4nl    = 0,
  b_wmednl   = 0,
  b_cost     = 0
)

# No fixed parameters in this case apollo_fixed = null 
apollo_fixed <- c() 

# Define the random coefficients function

apollo_randCoeff <- function(apollo_beta, apollo_inputs) {
  randcoeff <- list()
  randcoeff[["b1"]] <- apollo_beta["mu_b1"] + apollo_beta["sigma_b1"] * apollo_draws[,1]
  return(randcoeff)
}

apollo_inputs = apollo_validateInputs()

# Set the number of draws and type of draws
apollo_inputs <- apollo_inputs(
  apollo_control = apollo_control,
  database = database,
  draws = c("normal", 1000)
)

# Define the model function
apollo_probabilities <- function(apollo_beta, apollo_inputs, functionality="estimate") {
  # Attach the list of inputs (including data and fixed parameters)
  apollo_attach(apollo_inputs)
  
  # Create a list to hold the probabilities
  P <- list()
  
  # Define the utility functions
  V <- list()
  V[["alt0"]] <- b_wq8 * WQ_HUC8_0 + b_wq4 * WQ_HUC4_0 + b_wmed * WQ_Medium_0 +
    b_wlar * WQ_Large_0 + b_wq4nl * WQ_HUC4_NL_0 + 
    b_wmednl * WQ_Medium_NL_0 + b_cost * Cost_0
  V[["alt1"]] <- b1 + b_wq8 * WQ_HUC8_1 + b_wq4 * WQ_HUC4_1 + b_wmed * WQ_Medium_1 +
    b_wlar * WQ_Large_1 + b_wq4nl * WQ_HUC4_NL_1 + 
    b_wmednl * WQ_Medium_NL_1 + b_cost * Cost_1
  
  # Compute choice probabilities using the MNL model
  P <- apollo_mnl(V, apollo_inputs)
  
  # Return the list of probabilities
  return(P)
}

# Estimate the model
model1 <- apollo_estimate(apollo_beta, apollo_fixed, apollo_probabilities, apollo_inputs,
                          estimate_settings = list(hessianRoutine = "maxLik"))

# Print the model output
apollo_modelOutput(model1)
