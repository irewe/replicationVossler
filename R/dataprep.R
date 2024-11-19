library(data.table)

data <- fread("data/dataVossler.tab")
head(data[, .N , by = c("RespondentID", "ChoiceID")], 40)
data[, .N , by = c("BlockType", "Spatial_Unit", "NL","ASC")]
data[, .N , by = c("BlockType", "Spatial_Unit", "NL", "Cost", "ASC", "Referendum")]
data[, .N , by = c("BlockType", "Spatial_Unit", "NL")]

data$WQ_HUC8_b <- ifelse(data$WQ_HUC8 == 0, 0, 1)
data$WQ_HUC4_b <- ifelse(data$WQ_HUC4 == 0, 0, 1)
data$WQ_Medium_b <- ifelse(data$WQ_Medium == 0, 0, 1)
data$WQ_Large_b <- ifelse(data$WQ_Large == 0, 0, 1)

combos <- 
data[, .N , by = c(
  "BlockType", "ASC",
  "Spatial_Unit", "NL", "Cost",
  "WQ_HUC4_b", "WQ_Medium_b", "WQ_Large_b")]


colnames(data)
unique(data$WQ_HUC4)
