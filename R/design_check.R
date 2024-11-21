library(data.table)
library(ggplot2)
library(dplyr)

## Read data
data <- fread("data/dataVossler.tab")
## Read column descriptions
colDesc <- fread("data/colDesc.csv")
source("R/plot_heatmap.R")


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
    
    c(spatial_scale, location, bcg)
}

## get the right level of each attribute for each referendum
ref_levels <- as.data.frame(t(sapply(combos$Referendum, referendum_levels)))
combos$spatial_scale <- ref_levels[,1]
combos$location <- ref_levels[,2]
combos$bcg_change <- ref_levels[,3]
#combos <- combos[order(Referendum, Cost)]
combos_num <- combos %>%
  mutate(across(where(is.character), ~ as.integer(as.factor(.))))

## Remove cost in first analysis
combos_num_nocost <- unique(
  combos_num[, c("Referendum", "spatial_scale", "location", "bcg_change")])

cor_matrix <- cor(combos_num_nocost[, 2:4])
plot_heatmap(melt(cor_matrix), main_title = "Correlation between spatial and BCG, 20 referendums")

## Now add the cost:
cor_matrix <- cor(combos_num[, c("Cost","spatial_scale", "location", "bcg_change")])
plot_heatmap(melt(cor_matrix))

## Which combinations of referendum did each respondent have?
referendums <- data[!is.na(Vote) & ASC == 1, c("RespondentID", "Referendum")]

referendums <- dcast(
  referendums,
  RespondentID ~ Referendum, 
  fun.aggregate = length,           # Use length to count occurrences (ensures 1 where present)
  value.var = "Referendum")                 # Reshape by referendum

referendums <- referendums %>%
  group_by(across(2:21)) %>%
  summarise(N = n(), .groups = "drop")

## Select the most common combo of Referendums
main_referendum <- colnames(referendums[which.max(referendums$N),] %>% 
  select(where(~ any(. != 0))))
main_referendum <- as.integer(main_referendum)
main_referendum <- main_referendum[!is.na(main_referendum)]

## Test correlation without cost:
cor_matrix <- cor(combos_num_nocost[Referendum %in% main_referendum, 2:4])
plot_heatmap(melt(cor_matrix))



## No take a sample individual, with the most common combo of referendum and ...
ind_main_referendum <- data[!is.na(Vote) & ASC == 1, c("RespondentID", "Referendum")]
ind_main_referendum <- ind_main_referendum[Referendum %in% main_referendum, .N, by = RespondentID]
ind_main_referendum <- ind_main_referendum[N == length(main_referendum), RespondentID][1]
ind_main_referendum <- data[!is.na(Vote) & ASC == 1 & RespondentID == ind_main_referendum, c("Cost", "Referendum")]

ind_main_referendum <- merge(ind_main_referendum, combos_num_nocost, all.x=TRUE, by = "Referendum")
cor_matrix <- cor(
  ind_main_referendum[, c("Cost", "spatial_scale", "location", "bcg_change")])

plot_heatmap(melt(cor_matrix), "Individual 15")

