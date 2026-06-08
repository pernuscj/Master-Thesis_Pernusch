# Extended Dedaptive with MRS-II data
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

# 2. Predict prior item response distribution (no observed responses) ####
# Save time stamp
t1 <- Sys.time()

# Compute prior distribution of item responses
predPriorList <- cvPredPriorJointDistRespIrt(
  modelList = modelsIrt,
  idName = "Record.ID",
  nCore = usedCores,
  parallelFold = TRUE,
  seed = usedSeed+1,
  nSimTheta = usedNrSimTheta,
  nSimItem = usedNrSimItems
)

# Print running time
print(Sys.time() - t1)

# running time: 8.510493 mins

# Save output
saveRDS(predPriorList, 
        file = paste0("./Output/predPrior_", nameOut, 
               "_nTheta_", usedNrSimTheta, "_nItems_", 
               usedNrSimItems,".rds"))

# 3. Predictions with item selections ####
for (m in 1:length(usedCostMeas)) {
  # Extract current measurement costs per item and print it
  cm <- usedCostMeas[m]
  cat("Current cost structure (", m, "), ", 
      " measurement cost per item = ", cm, "\n")
  
  # Build the list of cost parameters
  costList <- list(usedCostFp,
                   usedCostFn,
                   cm)
  
  # Save time stamp
  t1 <- Sys.time()
  
  # Make dedaptive selections and  predictions for the current cost structure
  outDedaptive <- cvDecisionIrt(
    modelList = modelsIrt,
    selection = "dedaptive",
    idName = "Record.ID",
    predJointList = predPriorList,
    nCore = usedCores,
    parallelFold = TRUE,
    seed = usedSeed+2,
    costs = costList,
    thres = thresModSev,
    funOfItems = funSumScores,
    nSimTheta = usedNrSimTheta,
    nSimItem = usedNrSimItems
  )
  
  # Print running time
  cat("Time for cost structure ", m, ": ", Sys.time() - t1, "\n")
  
  # Evaluate performance
  evalRes <- evalMultOb(outDedaptive$pred, costList)
  
  # Extract prediction and performance table
  predTemp <- evalRes$pred
  perfTemp <- evalRes$perf
  
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
    paste0("./Output/predSelect_", nameOut, 
           "_nTheta", usedNrSimTheta,
           "_nItems_", usedNrSimItems,".csv"),
    row.names = FALSE
  )
  
  write.csv(
    perfAll,
    paste0("./Output/perfSelect_", nameOut, 
           "_nTheta", usedNrSimTheta,
           "_nItems_", usedNrSimItems,".csv"),
    row.names = FALSE
  )
}