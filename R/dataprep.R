library(data.table)

data <- fread("data/dataVossler.tab")
head(data[, .N , by = c("RespondentID", "ChoiceID")],100)
data[, .N , by = c("BlockType", "Spatial_Unit", "Ncost")]

data[, .N, by = "Ncost"]

colnames(data)
unique(data$NL)
