
library(data.table)

## Read data
data <- fread("data/dataVossler.tab")

data <- data[!is.na(Vote)]
data <- data[order(ChoiceID, ASC)]

data_wide <- dcast(
  data,
  ChoiceID + RespondentID + Referendum +
  HUC8BCG + HUC4BCG_base + HUC4BCG_scenario1 +
  HUC4BCG_scenario2 + MediumBCG_base + MediumBCG_scenario1 +
  MediumBCG_scenario2 + HUC4BCG_base_NL + HUC4BCG_scenario2_NL +
  MediumBCG_base_NL + MediumBCG_scenario2_NL ~ ASC,                 # Reshape by Alternative
  value.var = c(
    "Vote", "Cost",
    "WQ_HUC8", "WQ_HUC4", "WQ_Medium", "WQ_Large", "WQ_HUC4_NL", "WQ_Medium_NL",
    "ASC_PercentInstate", "ASC_PercentInstate_NL")  # Columns to spread
)

data_wide$choice <- data_wide$Vote_1
data_wide$Vote_0 <- NULL
data_wide$Vote_1 <- NULL
data_wide$ASC_0 <- 0
data_wide$ASC_1 <- 1


fwrite(data_wide, "data/dataVossler_wide.csv")
