library(data.table)

## Read data
data <- fread("data/dataVossler.tab")
## Read column descriptions
colDesc <- fread("data/colDesc.csv")

head(data[, .N , by = c("RespondentID", "ChoiceID", "Referendum", "ASC", "Vote")], 40)

## Trying to understand blocks and counters etc
View(
  data[RespondentID %in% c(1,2), c(
  "RespondentID", "BlockType", "Counter", "Counter_Block1", "ASC",
  "Spatial_Unit", "NL", "Referendum", "Vote" #"Cost",
  #"WQ_HUC4", "WQ_Medium", "WQ_Large",
  #"WQ_HUC4_NL", "WQ_Medium_NL"
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

combos <- combos[order(Referendum, Cost)]
