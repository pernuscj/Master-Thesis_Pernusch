# Extended Dedaptive with MRS-II data
# 1. Setup ####
#Pfad zum Projekt
setwd("/Users/jennypernusch/Library/Mobile Documents/com~apple~CloudDocs/Studium/MSc/00_Masterarbeit/Thesis_Pernusch")

# data
usedData<- "./Data/DataCimbolic_bereinigt.csv"

# Thresholds definieren (mittelgradig-stark/stark): somato-vegetativ 5/9, psych 4/7, urogential 2/4, gesamt 9/17
usedThres <- c(9, 7, 4, 17)
# Arguments for cross-validation
## Number of folds used for cross-validation
usedNrFolds <- 10 # damit nicht von einer Stichprobe abhängig -> für testen 5, sonst 10 Folds

## Seed used to build the folds
usedSeed <- 2701 # Zufallssimulation reproduzierbar, immer wieder dieselben Ergebnisse, Zahl egal

# output name
nameOut <- "03_03_26"

# load libraries
library(groupdata2)


# 1. Load and prepare the MRS-II data ####

# load MRS-II data
data <- read.csv(
  usedData,
  sep = ",",
  stringsAsFactors = FALSE
)

head(data, 3)

# Items auswählen, Gesamtskala und Subskalen
items_mrs <- paste0("mrsii_", 1:11)
items_somato <- c("mrsii_1", "mrsii_2", "mrsii_3", "mrsii_11")
items_psych  <- c("mrsii_4", "mrsii_5", "mrsii_6", "mrsii_7")
items_uro    <- c("mrsii_8", "mrsii_9", "mrsii_10")

# Score functions
funSumScores <- list(
  function(x) sum(x[c(1,2,3,11)]),   # somato-vegetativ
  function(x) sum(x[c(4,5,6,7)]),    # psych
  function(x) sum(x[c(8,9,10)]),     # urogenital
  function(x) sum(x[1:11])           # Gesamt
)

# Add scores and decisions to the data set
# Scores berechnen -> neue Spalte
data$score_somato <- apply(data[, items_mrs], 1, funSumScores[[1]])
data$score_psych  <- apply(data[, items_mrs], 1, funSumScores[[2]])
data$score_uro    <- apply(data[, items_mrs], 1, funSumScores[[3]])
data$score_total  <- apply(data[, items_mrs], 1, funSumScores[[4]])

# Entscheidungen erzeugen (0=unauffällig, 1=auffällig) -> neue Spalten
data$dec_somato <- ifelse(data$score_somato >= usedThres[1], 1, 0)
data$dec_psych  <- ifelse(data$score_psych  >= usedThres[2], 1, 0)
data$dec_uro    <- ifelse(data$score_uro    >= usedThres[3], 1, 0)
data$dec_total  <- ifelse(data$score_total  >= usedThres[4], 1, 0)

# für jede Entscheidung machen, wenn 50 zu 50 ist ACC super -> Prävalenzen
table(data$dec_uro)/sum(table(data$dec_uro))
table(data$dec_somato)/sum(table(data$dec_somato))
table(data$dec_psych)/sum(table(data$dec_psych))
table(data$dec_total)/sum(table(data$dec_total))

# Standardize age
data$ageStand <- (data$Age.at.baseline..years. - mean(data$Age.at.baseline..years.)) /
  sd(data$Age.at.baseline..years.)

# Set seed before making random splits
set.seed(usedSeed)

# Add fold assignment to the data -> für Cross Validation (Training auf 4 Folds, testen auf 5 Fold)
# (with `cat_col` we want to make folds with similar item response patterns)
data <- data.frame(
  fold(data, k = usedNrFolds,
       cat_col = items_mrs,
       method = "n_fill")
)

colnames(data)[colnames(data) == ".folds"] <- "fold"
data$fold <- as.character(data$fold)

outList<- list(data=data, items=items_mrs, funSumScores=funSumScores, thres=usedThres)

# Save prepared data with folds
saveRDS(
  outList,
  paste0("./Output/input_", nameOut, "_nFolds_", 
         usedNrFolds, ".rds")
)