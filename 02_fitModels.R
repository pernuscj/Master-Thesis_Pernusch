# Extended Dedaptive with MRS-II data
# 0. Setup ####
# Arguments for MIRT models
## Formula latent regression
usedFormula <- "ageStand"

## number of latent variables
usedNrLatent<- 3

# Created input 
# (in folder created with file '01_prepareData')
usedInput<- "input_03_03_26_nFolds_10.rds"

# Number of cores
usedCores <- 2 # only 2 cores on my MacBook

# output name
nameOut <- "03_03_26"

# Libraries and functions
library(dedaptive)

# load the additional functions
source("./Code/dedaptiveExtended.R")

# 1. Load and prepare input ####

# Load input and extract data, items, functions and thresholds
input<- readRDS(paste0("./Output/", usedInput))
summary(input)

data <- input$data
items_mrs <- input$items
funSumScores <- input$funSumScores
thresModSev <- input$thres

# 2. Fit multidimensional IRT models on k different subsets of the data ####
# Save time stamp
time1<- Sys.time()

# Fit k MIRT models
modelsIrt <- cvFitIrt(
  items = items_mrs,
  formula = usedFormula,
  data = data,
  foldVar = "fold",
  nCore = usedCores,
  model = usedNrLatent,
  technical = list(NCYCLES = 20000)
) 

# Print running time
Sys.time()-time1
#> Time difference of 16.69156 hours

# # Print output of one model
summary(modelsIrt[[1]]$model$fit)

# Save output
saveRDS(modelsIrt, 
        file = paste0("./Output/mirtModels_", nameOut, "_nLatent_",
               usedNrLatent, ".rds"))
