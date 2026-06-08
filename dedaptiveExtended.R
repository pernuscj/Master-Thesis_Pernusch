# (0) Libraries ####
library(dedaptive)
library(pROC)
library(parallel)

# (1) Training functions ####

# Train models on different subsamples (based on k-folds)

#' Function to train models for different folds
#'
#' @param fitFct Function to train models
#' @param data data set to train models
#' @param foldVar Name of the column in `data` that contains the fold assignment for
#' cross-validation
#' @param nCore Number of cores used for parallel computation (seriel if `nCore=1`)
#' @param packages Packages that are used in `fitFct` (need to be passed for parallel processing)
#' @param export Functions that need to be exported for parallel computations
#' @param ... Other arguments passed to `fitFct`
#'
#' @returns list of length k (number of folds), every entry contains the data of one fold,
#' a model trained on all data from all the other folds and fold assignment.
#' @export
#'
#' @examples # no example
cvTrain <- function(fitFct,
                    data,
                    foldVar,
                    nCore  = 1,
                    packages   = NULL,
                    export = NULL,
                    ...) {

  # extract folds
  folds <- sort(unique(as.character(data[[foldVar]])))

  # special case: only one fold (no CV), train on full data
  if (length(folds) == 1) {
    f <- folds[1]
    model <- fitFct(data = data, ...)
    out <- list(model = model, dataTest = data, fold = f)
    modelList <- list(out)
    names(modelList) <- f
    return(modelList)
  }

  dots <- list(...)

  # decide on if parallel or seriel computations
  useParallel <- nCore > 1

  if (useParallel) {

    cl <- parallel::makeCluster(nCore, outfile = "")
    on.exit(parallel::stopCluster(cl), add = TRUE)

    # ensure workers see same library paths as master
    lp <- .libPaths()
    parallel::clusterExport(cl, "lp", envir = environment())
    parallel::clusterEvalQ(cl, { .libPaths(lp); NULL })

    # load required packages on workers (if provided)
    if (!is.null(packages)) {
      parallel::clusterExport(cl, "packages", envir = environment())
      parallel::clusterEvalQ(cl, {
        ok <- vapply(packages, require, logical(1), character.only = TRUE)
        if (!all(ok)) stop("Failed to load packages: ", paste(packages[!ok], collapse = ", "))
        NULL
      })
    }

    dots <- list(...)

    # export needed objects + any user-requested exports
    toExport <- unique(c("fitFct", "data", "foldVar", "dots", export))
    parallel::clusterExport(cl, varlist = toExport, envir = environment())

    # key change: iterate over folds directly (so 'folds' isn't needed on workers)
    modelList <- parallel::parLapply(
      cl,
      X   = folds,
      fun = function(f) {

        dTrain <- data[data[[foldVar]] != f, , drop = FALSE]
        dTest  <- data[data[[foldVar]] == f, , drop = FALSE]

        model <- do.call(fitFct, c(list(data = dTrain), dots))

        list(model = model, dataTest = dTest, fold = f)
      }
    )

    names(modelList) <- folds
  }
  else {
    # serial computations

    # Initialize model list
    modelList <- vector("list", length(folds))
    names(modelList) <- folds

    for (i in 1:length(folds)) {
      f <- folds[i]

      # Train / test split
      dTrain <- data[data[[foldVar]] != f, , drop = FALSE]
      dTest <- data[data[[foldVar]] == f, , drop = FALSE]

      # Train model (with fitFct)
      model <- fitFct(data = dTrain, ...)

      # Generate output
      modelList[[i]] <- list(
        model = model,
        dataTest = dTest,
        fold = f
      )
    }
  }

  return(modelList)
}

#' Function to train MIRT models (fitIrt) for different folds
#' @description
#' The function \code{cvFitIrt()} fits multidimensional graded IRT models within
#' a cross-validation set-up. It is based on the function \code{cvTrain()} applying
#' the function \code{fitIrt()} (as argument `fitFct`).
#'
#' @param items Character vector with the names of the item/response columns in
#' data (as for \code{fitIrt()}). These columns are treated as ordered responses
#' and used to fit the graded IRT model.
#' @param formula Either NULL (no latent regression), a character string containing
#' only the right-hand side of a regression formula (e.g., "age + sex"),
#' or a one-sided formula (e.g., ~ age + sex) specifying the predictors for the
#' latent regression (as for \code{fitIrt()}).
#' @param data 	A data frame containing the item responses specified in items and,
#' if formula is not NULL, all predictor variables referenced in formula and a variable for the
#' fold assignment (`foldVar`). Each row typically corresponds to one person.
#' @param foldVar Name of the column in `data` that contains the fold assignment for
#' cross-validation
#' @param nCore Number of cores used for parallel computation (seriel if `nCore=1`)
#' @param nCore
#' @param ... Other arguments passed to the function \code{fitIrt()}
#'
#' @returns list of length k (number of folds), every entry contains the data of one fold,
#' a MIRT model trained on all data from all the other folds and fold assignment.
#' @export
#'
#' @examples # no examples
cvFitIrt<- function(items,
                    formula,
                    data,
                    foldVar,
                    nCore=1,
                    ...) {
  # Apply function cvTrain with function fitFct = fitIrt
  modelList<- cvTrain(
    fitFct = fitIrt,
    data = data,
    foldVar = foldVar,
    nCore = nCore,
    packages = c("mirt", "dedaptive"),
    items = items,
    formula = formula,
    ...)
  return(modelList)
}
# (2) Prediction and selection ####
runSubjectsPred <- function(model,
                            data,
                            predFct,
                            idName = NULL,
                            nCore = 1,
                            foldSeed = NULL,
                            ...) {

  # (0) Prepare
  dots <- list(...)
  nSub <- nrow(data)

  if (!is.null(idName) && idName %in% names(data)) {
    idVec <- as.character(data[[idName]])
  } else {
    idVec <- NULL
  }

  fml <- formals(predFct)
  hasSeed <- "seed" %in% names(fml)
  seedInDots <- "seed" %in% names(dots)

  # Set the seed to generate seeds for each subject
  seedsSub <- NULL
  if (hasSeed && !seedInDots) {
    if (!is.null(foldSeed) && !is.na(foldSeed)) {
      set.seed(as.integer(foldSeed))
    }
    seedsSub <- sample.int(.Machine$integer.max, nSub)
  }

  # Decide if parallel or serial computations
  useParallel <- nCore > 1
  if (useParallel) {
    cl <- parallel::makeCluster(nCore)
    on.exit(parallel::stopCluster(cl), add = TRUE)

    parallel::clusterEvalQ(cl, {
      library(dedaptive)
      NULL
    })

    parallel::clusterExport(
      cl,
      varlist = c("model", "data", "predFct", "dots",
                  "hasSeed", "seedInDots", "seedsSub"),
      envir   = environment()
    )

    # (1) Make predictions in parallel
    outList <- parallel::parLapply(
      cl,
      X = 1:nSub,
      fun = function(i) {
        dataSub <- data[i, , drop = FALSE]

        # Define arguments for prediction (passed to predFct)
        if (hasSeed & seedInDots == FALSE & is.null(seedsSub) == FALSE) {
          args <- c(list(model = model,
                         dataSub = dataSub,
                         seed    = seedsSub[i]),
                    dots)
        } else {
          args <- c(list(model = model,
                         dataSub = dataSub),
                    dots)
        }

        # Make predictions
        predObj <- do.call(predFct, args)
        list(pred = predObj, fold = NULL)
      }
    )

  } else {
    # (2) Make predictions seriel
    outList <- vector("list", nSub)
    for (i in 1:nSub) {
      dataSub <- data[i, , drop = FALSE]

      # Define arguments for prediction (passed to predFct)
      if (hasSeed & seedInDots == FALSE & is.null(seedsSub) == FALSE) {
        args <- c(list(model = model,
                       dataSub = dataSub,
                       seed    = seedsSub[i]),
                  dots)
      } else {
        args <- c(list(model = model,
                       dataSub = dataSub),
                  dots)
      }

      # Make predictions
      outList[[i]] <- list(
        pred = do.call(predFct, args),
        fold = NULL
      )
    }
  }

  # Rename output list (list with one entry per subject)
  if (is.null(idVec) == F) {
    names(outList) <- idVec
  }
  if (is.null(names(outList)) || any(names(outList) == "")) {
    names(outList) <- as.character(1:length(outList))
  }

  return(outList)
}


# Per-subject decision (dedaptiveIrt or fixSelectionIrt) for one model
runSubjectsDecision <- function(model,
                                data,
                                selectionFct,
                                idName = NULL,
                                predJointLookup = NULL,
                                nCore = 1,
                                foldSeed = NULL,
                                export = NULL,
                                ...) {
  # (0) Prepare
  dots <- list(...)
  nSub <- nrow(data)

  if (is.null(idName) ==F && idName %in% names(data)) {
    idVec <- as.character(data[[idName]])
  } else {
    idVec <- NULL
  }

  if (is.null(predJointLookup) == F & is.null(idVec)) {
    stop("To use 'predJointLookup', supply 'idName' present in 'data'.")
  }

  # check if selectionFct has 'seed'
  fml <- formals(selectionFct)
  hasSeed <- "seed" %in% names(fml)
  seedInDots <- "seed" %in% names(dots)

  # Generate seeds
  seedsSub <- NULL
  if (hasSeed & seedInDots == F) {
    if (is.null(foldSeed) ==F & is.na(foldSeed) == F) {
      set.seed(as.integer(foldSeed))
    }
    seedsSub <- sample.int(.Machine$integer.max, nSub)
  }

  # Define selection function for one subject
  selectionFctApply <- function(i) {
    ## Extract data of the subject
    dataSub <- data[i, , drop = FALSE]

    ## Extract prior joint distribution of items
    if (is.null(predJointLookup)) {
      pjSub <- NULL
    } else {
      pjSub <- predJointLookup(idVec[i])
    }

    ## Define arguments of selection function
    if (hasSeed & seedInDots == F & is.null(seedsSub) == F) {
      args <- c(
        list(
          model = model,
          predJointSub = pjSub,
          dataSub = dataSub,
          seed = seedsSub[i]
        ),
        dots
      )
    } else {
      args <- c(
        list(
          model = model,
          predJointSub = pjSub,
          dataSub = dataSub
        ),
        dots
      )
    }

    ## Apply function
    do.call(selectionFct, args)
  }

  # Define if parallel or seriell computations
  useParallel <- nCore > 1

  # (1) Parallel selections
  if (useParallel) {
    cl <- parallel::makeCluster(nCore)
    on.exit(parallel::stopCluster(cl), add = TRUE)

    parallel::clusterEvalQ(cl, {
      library(dedaptive)
      NULL
    })

    # export extra functions needed on core
    varlistExport <- unique(c(
      "data", "model", "selectionFct", "selectionFctApply",
      "predJointLookup", "idVec", "dots", "hasSeed", "seedInDots", "seedsSub",
      export
    ))

    # Apply function to select and predict
    parallel::clusterExport(cl, varlist = varlistExport, envir = environment())
    outList <- parallel::parLapply(
      cl,
      X   = 1:nSub,
      fun = function(i) {
        list(
          res  = selectionFctApply(i),
          fold = NULL
        )
      }
    )

  } else {
    # (2) Seriell selection

    # Apply function
    outList <- vector("list", nSub)
    for (i in 1:nSub) {
      outList[[i]] <- list(
        res  = selectionFctApply(i),
        fold = NULL
      )
    }
  }

  # Rename output
  if (is.null(idVec) == F) {
    names(outList) <- idVec
  }
  if (is.null(names(outList)) || any(names(outList) == "")) {
    names(outList) <- as.character(1:length(outList))
  }

  return(outList)
}
# Build data frame with "$pred" from a list with results
buildPredDfFromList <- function(resList,
                                extractPredFun,
                                idCol = "id",
                                includeFold = FALSE) {

  # Extract information of predictions
  rows <- lapply(1:length(resList), function(i) {
    subId   <- names(resList)[i]
    entry   <- resList[[i]]
    foldVal <- if (is.null(entry$fold) == F) entry$fold else NA

    predRow <- extractPredFun(entry)
    if (is.data.frame(predRow) ==F | nrow(predRow) != 1) {
      stop("Each entry must return a one-row data.frame via 'extractPredFun'.")
    }

    out <- data.frame(
      id = subId,
      predRow,
      check.names = FALSE,
      row.names   = NULL
    )
    if (includeFold) {
      out$fold <- foldVal
    }

    colnames(out)[1] <- idCol
    return(out)
  })

  # Build data set
  predDf <- do.call(rbind, rows)

  if (includeFold) {
    predDf <- predDf[, c(idCol, "fold", setdiff(names(predDf), c(idCol, "fold")))]
  }

  return(predDf)
}

buildPredDfFromList <- function(resList,
                                extractPredFun,
                                idCol = "id",
                                includeFold = FALSE) {


  rows <- lapply(seq_along(resList), function(i) {
    subId   <- names(resList)[i]
    entry   <- resList[[i]]
    foldVal <- if (!is.null(entry$fold)) entry$fold else NA

    predRow <- extractPredFun(entry)

    out <- data.frame(
      id = subId,
      predRow,
      check.names = FALSE,
      row.names   = NULL
    )
    if (includeFold) {
      out$fold <- foldVal
    }

    colnames(out)[1] <- idCol
    out
  })

  predDf <- do.call(rbind, rows)

  if (includeFold) {
    predDf <- predDf[, c(idCol, "fold", setdiff(names(predDf), c(idCol, "fold")))]
  }

  return(predDf)
}


# Build merged DF from a list created by runSubjectsDecision() (uses $res$pred)
mergePredFromDecList <- function(decList,
                                 idCol = "id",
                                 includeFold = FALSE) {
  out<- buildPredDfFromList(
    resList = decList,
    extractPredFun = function(x) x$res$pred,    # dedaptiveIrt / fixSelectionIrt style
    idCol = idCol,
    includeFold = includeFold
  )
  return(out)
}

# CV: apply a predFct to every subject (out-of-sample)
cvPredict <- function(modelList,
                      predFct,
                      idName = NULL,
                      nCore = 1,
                      parallelFold = TRUE,
                      seed = NULL,
                      ...) {

  dots <- list(...)
  useParallel <- nCore > 1 & parallelFold

  # NEW: only generate foldSeeds if seed is not NULL
  if (!is.null(seed)) {
    set.seed(seed)
    foldSeeds <- sample.int(.Machine$integer.max, length(modelList))
  } else {
    foldSeeds <- NULL
  }

  if (useParallel) {
    cl <- parallel::makeCluster(nCore)
    on.exit(parallel::stopCluster(cl), add = TRUE)

    parallel::clusterEvalQ(cl, {
      library(dedaptive)
      NULL
    })

    parallel::clusterExport(
      cl,
      varlist = c("runSubjectsPred", "predFct", "idName",
                  "dots", "foldSeeds", "modelList"),
      envir   = environment()
    )

    foldLists <- parallel::parLapply(
      cl,
      X = seq_along(modelList),
      fun = function(i) {
        entry <- modelList[[i]]
        if (is.null(entry$model) || is.null(entry$dataTest) || is.null(entry$fold)) {
          stop("Each modelList entry must have 'model', 'dataTest', 'fold'.")
        }

        foldSeed_i <- if (!is.null(foldSeeds)) foldSeeds[i] else NULL

        lst <- runSubjectsPred(
          model    = entry$model,
          data     = entry$dataTest,
          predFct  = predFct,
          idName   = idName,
          nCore    = 1,
          foldSeed = foldSeed_i,
          ...
        )

        lst <- lapply(lst, function(x) {
          x$fold <- entry$fold
          x
        })

        lst
      }
    )

  } else {
    foldLists <- lapply(seq_along(modelList), function(i) {
      entry <- modelList[[i]]
      if (is.null(entry$model) || is.null(entry$dataTest) || is.null(entry$fold)) {
        stop("Each modelList entry must have 'model', 'dataTest', 'fold'.")
      }

      foldSeed_i <- if (!is.null(foldSeeds)) foldSeeds[i] else NULL

      lst <- runSubjectsPred(
        model    = entry$model,
        data     = entry$dataTest,
        predFct  = predFct,
        idName   = idName,
        nCore    = if (parallelFold) 1 else nCore,
        foldSeed = foldSeed_i,
        ...
      )

      lst <- lapply(lst, function(x) {
        x$fold <- entry$fold
        x
      })

      lst
    })
  }

  predList <- do.call(c, foldLists)

  if (is.null(names(predList)) || any(names(predList) == "")) {
    names(predList) <- as.character(seq_along(predList))
  }

  return(predList)
}


# CV apply predPriorJointDistRespIrt to every subject (out-of-sample)
cvPredPriorJointDistRespIrt <- function(
    modelList,
    idName = NULL,
    nCore = 1,
    parallelFold = TRUE,
    seed = NULL,
    ...) {
  predList <- cvPredict(
    modelList = modelList,
    predFct = predJointDistRespIrt,
    idName = idName,
    nCore = nCore,
    parallelFold = parallelFold,
    seed = seed,
    givenVal = NULL,
    priorGrid = NULL,
    ...)
  return(predList)
}

cvDecisionIrt <- function(modelList,
                          selection = c("dedaptive","fixed"),
                          idName = NULL,
                          predJointList = NULL,
                          nCore = 1,
                          parallelFold = TRUE,
                          seed = NULL,
                          ...) {
  # Extract extra arguments given to selectionFct
  dots <- list(...)
  if ("seed" %in% names(dots)) {
    stop("Please pass 'seed' via the 'seed' argument of cvDecisionIrt, not via '...'.")
  }

  # Define function for selection
  selection <- match.arg(selection)
  if (selection == "dedaptive") {
    selectionFct <- dedaptiveIrt
  } else {
    selectionFct <- fixSelectionIrt
  }

  # for priors: lookup by subject id
  if (is.null(predJointList)) {
    predLookup <- NULL
  } else {
    predLookup <- function(idChar) {
      if (is.null(names(predJointList)) || !(idChar %in% names(predJointList)))
        stop("Missing prior for subject id '", idChar, "'.")
      return(predJointList[[idChar]]$pred)
    }
  }

  # Decide on parallelisation over folds
  useParallel <- nCore > 1 & parallelFold

  # Generate one seed per fold if 'seed' is given
  if (!is.null(seed)) {
    set.seed(seed)
    foldSeeds <- sample.int(.Machine$integer.max, length(modelList))
  } else {
    foldSeeds <- NULL
  }

  if (useParallel) {
    ## --- parallel over folds with parLapply ---
    cl <- parallel::makeCluster(nCore)
    on.exit(parallel::stopCluster(cl), add = TRUE)

    # load dedaptive on workers
    parallel::clusterEvalQ(cl, {
      library(dedaptive)
      NULL
    })

    # export needed objects to workers
    parallel::clusterExport(
      cl,
      varlist = c("runSubjectsDecision", "selectionFct", "idName",
                  "predLookup", "dots", "foldSeeds", "modelList"),
      envir   = environment()
    )

    foldLists <- parallel::parLapply(
      cl,
      X   = seq_along(modelList),
      fun = function(i) {
        entry <- modelList[[i]]
        if (is.null(entry$model) || is.null(entry$dataTest) || is.null(entry$fold)) {
          stop("Each modelList entry must have 'model', 'dataTest', 'fold'.")
        }

        foldSeed_i <- if (!is.null(foldSeeds)) foldSeeds[i] else NULL

        # per-subject decisions for this fold (sequential inside)
        lst <- do.call(
          runSubjectsDecision,
          c(list(model           = entry$model,
                 data            = entry$dataTest,
                 selectionFct    = selectionFct,
                 idName          = idName,
                 predJointLookup = predLookup,
                 nCore           = 1,
                 foldSeed        = foldSeed_i),
            dots)
        )

        # annotate fold
        lst <- lapply(lst, function(x) {
          x$fold <- entry$fold
          x
        })

        lst   # list of subjects for this fold
      }
    )

  } else {
    ## sequential over folds (or subject-parallel inside runSubjectsDecision) ---
    foldLists <- lapply(seq_along(modelList), function(i) {
      entry <- modelList[[i]]
      if (is.null(entry$model) || is.null(entry$dataTest) || is.null(entry$fold)) {
        stop("Each modelList entry must have 'model', 'dataTest', 'fold'.")
      }

      foldSeed_i <- if (!is.null(foldSeeds)) foldSeeds[i] else NULL

      lst <- runSubjectsDecision(
        model           = entry$model,
        data            = entry$dataTest,
        selectionFct    = selectionFct,
        idName          = idName,
        predJointLookup = predLookup,
        nCore           = if (parallelFold) 1 else nCore,
        foldSeed        = foldSeed_i,
        ...
      )

      lst <- lapply(lst, function(x) {
        x$fold <- entry$fold
        x
      })

      lst
    })
  }

  # flatten to one list entry per subject
  decList <- do.call(c, foldLists)
  if (is.null(names(decList)) || any(names(decList) == "")) {
    names(decList) <- as.character(seq_along(decList))
  }

  if (is.null(idName)) {
    idName <- "id"
  }
  predDf <- mergePredFromDecList(decList, idCol = idName, includeFold = TRUE)

  return(list(resList = decList, pred = predDf))
}

# Evaluation

evalMultOb <- function(dataPred,
                       costs) {
  # costs: list(cFp, cFn, cM)
  # dataPred: data frame with columns prob_f, diag_f, trueMean_f, predMean_f, nItems, etc.

  # 0) Extract cost parameters and add them to the prediction table
  cFp <- costs[[1]]
  cFn <- costs[[2]]
  cM  <- costs[[3]]

  # attach cost parameters as columns (one set per decision)
  for (f in 1:length(cFn)) {
    dataPred[[paste0("cFn_", f)]] <- cFn[f]
    dataPred[[paste0("cFp_", f)]] <- cFp[f]
  }
  dataPred$cM <- cM

  # 1) Predictions and costs per subject
  for (f in 1:length(cFn)) {
    probCol <- paste0("prob_", f)
    diagCol <- paste0("diag_", f)

    # 1a) classification based on marginal distribution of the scores
    dataPred[[paste0("predDiag_", f)]] <-
      fcClassFct(p = dataPred[[probCol]],
                 missCosts  = c(cFp[f], cFn[f]),
                 class1 = 0,
                 class2 = 1)

    # 1b) correct classifications
    dataPred[[paste0("correctDiag_", f)]] <-
      as.numeric(dataPred[[paste0("predDiag_", f)]] ==
                   dataPred[[diagCol]])

    # 1c) misclassification costs per decision
    #  FP: true 0, predicted 1 results in  cost cFp[f]
    #  FN: true 1, predicted 0  results in cost cFn[f]
    dataPred[[paste0("misCost_", f)]] <-
      cFp[f] * (dataPred[[diagCol]] == 0 & dataPred[[paste0("predDiag_", f)]] == 1) +
      cFn[f] * (dataPred[[diagCol]] == 1 & dataPred[[paste0("predDiag_", f)]] == 0)
  }

  # 1d) total misclassification costs across all decisions (row-wise)
  misCols <- grep("^misCost_[0-9]+$", colnames(dataPred), value = TRUE)
  if (length(misCols) > 0) {
    dataPred$misCost <- rowSums(dataPred[, misCols, drop = FALSE])
  } else {
    dataPred$misCost <- 0
  }

  # 1e) measurement costs
  dataPred$measCost <- cM * dataPred$nItems

  # indicator: max 1 item selected
  dataPred$max1Item <- ifelse(dataPred$nItems <= 1, 1, 0)

  # 1f) total costs (misclassification + measurement)
  dataPred$totCost <- dataPred$misCost + dataPred$measCost

  # 2) Performance table across subjects

  # columns: costs, items, mis/tot costs, correctness indicators
  colCostParam <- c(grep("^cFn_", colnames(dataPred), value = TRUE),
                    grep("^cFp_", colnames(dataPred), value = TRUE), "cM")
  colCostParam <- colCostParam[colCostParam %in% colnames(dataPred)]

  colItems <- c("runTime", "runTimePerItem", "nItems", "max1Item", "measCost")
  colItems <- colItems[colItems %in% colnames(dataPred)]

  colMisTot <- c(grep("^misCost_[0-9]+$", colnames(dataPred), value = TRUE),
                 grep("^misCostJoint_[0-9]+$", colnames(dataPred), value = TRUE),
                 "misCost", "totCost")

  colMisTot <- colMisTot[colMisTot %in% colnames(dataPred)]

  colCorrect <- grep("^correctDiag_[0-9]+$", colnames(dataPred), value = TRUE)

  colObjMetrics <- c(colCostParam, colItems, colMisTot, colCorrect)
  colObjMetrics <- unique(colObjMetrics[colObjMetrics %in% colnames(dataPred)])

  # mean over subjects for all these metrics
  perfTab <- stats::aggregate(
    stats::as.formula(
      paste0("cbind(",
             paste(colObjMetrics, collapse = ","),
             ") ~ 1")
    ),
    data = dataPred,
    FUN  = mean
  )

  # rename correctness means to accuracies
  if (length(colCorrect) > 0) {
    accNames <- paste0("acc_", seq_along(colCorrect))
    idxCorr  <- match(colCorrect, colnames(perfTab))
    colnames(perfTab)[idxCorr] <- accNames
  } else {
    accNames <- character(0)
  }

  # 3) Add classification / regression metrics per decision ----
  for (f in 1:length(cFn)) {
    diagCol    <- paste0("diag_", f)
    probCol    <- paste0("prob_", f)
    trueMeanCol<- paste0("trueMean_", f)
    predMeanCol<- paste0("predMean_", f)
    corrCol    <- paste0("correctDiag_", f)

    if (!all(c(diagCol, probCol, trueMeanCol, predMeanCol, corrCol) %in% colnames(dataPred))) {
      next
    }

    # Sensitivity (TPR)
    sensVal <- mean(
      dataPred[[corrCol]][dataPred[[diagCol]] == 1],
      na.rm = TRUE
    )
    perfTab[[paste0("sens_", f)]] <- sensVal

    # Specificity (TNR)
    specVal <- mean(
      dataPred[[corrCol]][dataPred[[diagCol]] == 0],
      na.rm = TRUE
    )
    perfTab[[paste0("spec_", f)]] <- specVal

    # Balanced accuracy
    perfTab[[paste0("bacc_", f)]] <- mean(c(sensVal, specVal), na.rm = TRUE)

    # AUC
    perfTab[[paste0("auc_", f)]] <-
      pROC::auc(dataPred[[diagCol]], dataPred[[probCol]])

    # Correlations
    perfTab[[paste0("corPears_", f)]] <-
      as.numeric(stats::cor(dataPred[[trueMeanCol]],
                            dataPred[[predMeanCol]],
                            use = "complete.obs",
                            method = "pearson"))

    perfTab[[paste0("corSpear_", f)]] <-
      as.numeric(stats::cor(dataPred[[trueMeanCol]],
                            dataPred[[predMeanCol]],
                            use = "complete.obs",
                            method = "spearman"))

    # RMSE
    perfTab[[paste0("rmse_", f)]] <-
      sqrt(mean(
        (dataPred[[trueMeanCol]] - dataPred[[predMeanCol]])^2,
        na.rm = TRUE
      ))

    # standardized RMSE
    perfTab[[paste0("srmse_", f)]] <-
      perfTab[[paste0("rmse_", f)]] /
      stats::sd(dataPred[[trueMeanCol]], na.rm = TRUE)
  }

  # 4) Means of performance metrics over decisions
  # for metrics that exist for every individual decision: acc, sens, spec, bacc, auc,
  # corPears, corSpear, rmse, srmse
  metricPrefixes <- c(
    "acc", "sens", "spec", "bacc", "auc",
    "corPears", "corSpear", "rmse", "srmse"
  )

  for (pref in metricPrefixes) {
    colsPref <- grep(paste0("^", pref, "_[0-9]+$"), colnames(perfTab), value = TRUE)
    if (length(colsPref) > 0) {
      perfTab[[paste0(pref, "_mean")]] <-
        mean(as.numeric(perfTab[1, colsPref]), na.rm = TRUE)
    }
  }

  return(list(pred = dataPred, perf = perfTab))
}
