library(data.table)

data <- fread("data/dataVossler.tab")
head(data[, .N , by = c("RespondentID", "ChoiceID")], 100)
data[, .N , by = c("BlockType", "Spatial_Unit", "NL", "Cost","Referendum")]

data[, .N, by = "WQ_Medium"]

colnames(data)
unique(data$HUC4)
