library(data.table)

## Read data
data <- fread("data/dataVossler.tab")
## Read column descriptions
colDesc <- fread("data/colDesc.csv")

head(data[, .N , by = c("RespondentID", "ChoiceID", "Referendum", "ASC", "Vote")], 40)

## Trying to understand blocks and counters etc
View(
  data[RespondentID %in% c(1), c("ChoiceID",
  "RespondentID", "BlockType", "Counter", "Counter_Block1", "ASC",
  "Spatial_Unit", "NL", "Referendum", "Vote", #"Cost",
  "WQ_HUC4", "WQ_Medium", "WQ_Large",
  "WQ_HUC4_NL", "WQ_Medium_NL"
  )]
)

## How many respondents got to vote for each referendum
data[, .(n_respondents = length(unique(RespondentID[!is.na(Vote)]))), by = c(
  "Spatial_Unit", "Referendum")]

## Different possible combinations of attribute levels
combos <- 
data[ASC == 1, .(n_respondents = length(unique(RespondentID[!is.na(Vote)]))), by = c(
  "Cost",
  "Spatial_Unit", "Referendum")]

## Function to 
referendum_levels <- function(Referendum = 1){
  if (Referendum %in% 1:4){
    spatial_scale <- "Single_HUC4"
    location <- "local"
  } else if (Referendum %in% 5:8) {
    spatial_scale <- "Three_Medium"
    location <- "local"
  } else if (Referendum %in% 9:12) {
    spatial_scale <- "Full_Large"
    location <- "local"
  } else if (Referendum %in% 13:16) {
    spatial_scale <- "Single_HUC4"
    location <- "non-local"
  } else if (Referendum %in% 17:20) {
    spatial_scale <- "Three_Medium"
    location <- "non-local"
  }
    
    bcg_vec <- rep(c("1_up", "Sc_1", "Sc_2", "Sc_3"), 5)
    bcg <- bcg_vec[Referendum]
    
    list(spatial_scale, location, bcg)
}

## get the right level of each attribute for each referendum
ref_levels <- as.data.frame(t(sapply(combos$Referendum, referendum_levels)))
combos$spatial_scale <- ref_levels[,1]
combos$location <- ref_levels[,2]
combos$bcg_change <- ref_levels[,3]
#combos <- combos[order(Referendum, Cost)]
