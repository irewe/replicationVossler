
library(data.table)

## Read data
data <- fread("data/dataVossler.tab")

data <- data[!is.na(Vote), c(
  "RespondentID", "ChoiceID", "Referendum", "Vote", "Cost", "ASC", "WQ_HUC8", "WQ_HUC4",
  "WQ_Medium", "WQ_Large", "WQ_HUC4_NL", "WQ_Medium_NL")]

data <- data[order(ChoiceID, ASC)]
### show the dataframe again
data

data_wide <- dcast(
  data,
  ChoiceID + RespondentID + Referendum ~ ASC,                 # Reshape by Alternative
  value.var = c(
    "Vote", "Cost",
    "WQ_HUC8", "WQ_HUC4", "WQ_Medium", "WQ_Large", "WQ_HUC4_NL", "WQ_Medium_NL")  # Columns to spread
)

data_wide$choice <- data_wide$Vote_1
data_wide$Vote_0 <- NULL
data_wide$Vote_1 <- NULL

fwrite(data_wide, "data/dataVossler_wide.csv")
