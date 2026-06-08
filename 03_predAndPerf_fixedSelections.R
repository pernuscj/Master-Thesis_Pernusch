# Extended Dedaptive with MRS-II data
# Fixed selections: full, somato, psych, uro
# 0. Setup ####

# used input
usedInput <- "input_03_03_26_nFolds_10.rds"

# used model
usedModel <- "mirtModels_03_03_26_nLatent_3.rds"

# Number of cores for computations
usedCores <- 2 # MacBook only has 2

# Prediction / Selection
## Seed for simulation based predictions
usedSeed <- 2701

## Number of simulations for approximations
usedNrSimTheta <- 1000
usedNrSimItems <- 10

# cost parameters
## cost false positive (4 outcomes)
usedCostFp <- c(0.25, 0.25, 0.25, 0.25)

## cost false negative (4 outcomes)
usedCostFn <- c(0.25, 0.25, 0.25, 0.25)

## measurement cost of one item
# usedCostMeas<- 0.13
usedCostMeas<- c(seq(0, 0.01, 0.001),    # erzeugt viele kleine Werte -> 0.000, 0.001, 0.002, ..., 0.010
                 seq(0.01, 0.13, 0.01))  # erzeugt gröbere Werte -> 0.01, 0.02, 0.03, ..., 0.13

# output name
nameOut <- "03_03_26"

# load libraries and functions
library(dedaptive)
source("./Code/dedaptiveExtended.R")

# 1. Load input and models ####

# Load input and extract data, items, functions and thresholds
input<- readRDS(paste0("./Output/", usedInput))
summary(input)

data <- input$data
items_mrs <- input$items
funSumScores <- input$funSumScores
thresModSev <- input$thres

# load models
modelsIrt<- readRDS(paste0("./Output/", usedModel))

# Define fixed item selections
items_mrs <- c("mrsii_1", "mrsii_2", "mrsii_3", "mrsii_4", "mrsii_5", "mrsii_6", "mrsii_7", "mrsii_8", "mrsii_9", "mrsii_10", "mrsii_11")
items_somato  <- c("mrsii_1", "mrsii_2", "mrsii_3", "mrsii_11")
items_psych <- c("mrsii_4", "mrsii_5", "mrsii_6", "mrsii_7")
items_uro   <- c("mrsii_8", "mrsii_9", "mrsii_10")

types <- c("full", "somato", "psych", "uro")

# 2. Fixed predictions ####
# Save time stamp
t1 <- Sys.time()

# Full
outFull <- cvDecisionIrt(
  modelList = modelsIrt,
  selection = "fixed",
  idName = "Record.ID",
  nCore = usedCores,
  parallelFold = TRUE,
  seed = usedSeed + 2,
  givenVar = items_mrs,
  thres = thresModSev,
  funOfItems = funSumScores,
  nSimTheta = usedNrSimTheta,
  nSimItem = usedNrSimItems
)

# Somato
outSomato <- cvDecisionIrt(
  modelList = modelsIrt,
  selection = "fixed",
  idName = "Record.ID",
  nCore = usedCores,
  parallelFold = TRUE,
  seed = usedSeed + 2,
  givenVar = items_somato,
  thres = thresModSev,
  funOfItems = funSumScores,
  nSimTheta = usedNrSimTheta,
  nSimItem = usedNrSimItems
)

# Psycho
outPsych <- cvDecisionIrt(
  modelList = modelsIrt,
  selection = "fixed",
  idName = "Record.ID",
  nCore = usedCores,
  parallelFold = TRUE,
  seed = usedSeed + 2,
  givenVar = items_psych,
  thres = thresModSev,
  funOfItems = funSumScores,
  nSimTheta = usedNrSimTheta,
  nSimItem = usedNrSimItems
)

# Uro
outUro <- cvDecisionIrt(
  modelList = modelsIrt,
  selection = "fixed",
  idName = "Record.ID",
  nCore = usedCores,
  parallelFold = TRUE,
  seed = usedSeed + 2,
  givenVar = items_uro,
  thres = thresModSev,
  funOfItems = funSumScores,
  nSimTheta = usedNrSimTheta,
  nSimItem = usedNrSimItems
)

# Print running time
print(Sys.time() - t1)

# running time: 

# 3. Evaluate performance for all cost structures ####
for (m in 1:length(usedCostMeas)) {
  
  # Extract current measurement costs per item and print it
  cm <- usedCostMeas[m]
  cat("Current cost structure (", m, "), ",
      " measurement cost per item = ", cm, "\n")
  
  # Build the list of cost parameters
  costList <- list(usedCostFp,
                   usedCostFn,
                   cm)
  
  # Evaluate full
  evalFull <- evalMultOb(outFull$pred, costList)
  predFull <- evalFull$pred
  perfFull <- evalFull$perf
  predFull$type <- "full"
  perfFull$type <- "full"
  
  # Evaluate somato
  evalSomato <- evalMultOb(outSomato$pred, costList)
  predSomato <- evalSomato$pred
  perfSomato <- evalSomato$perf
  predSomato$type <- "somato"
  perfSomato$type <- "somato"
  
  # Evaluate psych
  evalPsych <- evalMultOb(outPsych$pred, costList)
  predPsych <- evalPsych$pred
  perfPsych <- evalPsych$perf
  predPsych$type <- "psych"
  perfPsych$type <- "psych"
  
  # Evaluate uro
  evalUro <- evalMultOb(outUro$pred, costList)
  predUro <- evalUro$pred
  perfUro <- evalUro$perf
  predUro$type <- "uro"
  perfUro$type <- "uro"
  
  # Combine all fixed selections for current cost structure
  predTemp <- rbind(predFull, predSomato, predPsych, predUro)
  perfTemp <- rbind(perfFull, perfSomato, perfPsych, perfUro)
  
  # Add the tables to an overall table containing information of all cost structures
  if (m == 1) {
    predAll <- predTemp
    perfAll <- perfTemp
  } else {
    predAll <- rbind(predAll, predTemp)
    perfAll <- rbind(perfAll, perfTemp)
  }
  
  # Print current performances
  print(perfTemp)
  
  # Save current results after each cost to be safe
  write.csv(
    predAll,
    paste0("./Output/predFixed_", nameOut,
           "_nTheta", usedNrSimTheta,
           "_nItems_", usedNrSimItems, ".csv"),
    row.names = FALSE
  )
  
  write.csv(
    perfAll,
    paste0("./Output/perfFixed_", nameOut,
           "_nTheta", usedNrSimTheta,
           "_nItems_", usedNrSimItems, ".csv"),
    row.names = FALSE
  )
}

# Prüfen ####
unique(perfAll[, c("type", "nItems")])
