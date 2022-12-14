# This file is a collection of supporting functions that I developed over the
# course of the analysis.  They are collected here in order to not disturb the
# flow of analysis in the other scripts


# MPs that are not part of a party's group in parliament are labelled as
# 'fraktionslos'.  A small complication is that the source dataset also includes
# the date the MP left the party's grouping in this field, we remove this here

doConsolidateFraktionLabels <- function(sLong) {
  rValue <-
    ifelse(str_detect(sLong, 'fraktionslos'), 'fraktionslos', sLong)
  ix <- str_locate(rValue, ' seit')[[1]]
  return(substr(rValue, 1, ifelse(!is.na(ix), ix - 1, nchar(rValue))))
}


# Names of the parties in parliament are not consistently uppercased in the
# source data

standardisePartyNames <- function(name) {
  return(names(colParties)[which.min(stringdist(toupper(name), toupper(names(colParties))))])
}


# We shorten the names of the different states that make up the Federal Republic
# of Germany. That also makes them easier to work with (e.g. Niedersachsen /
# Sachsen / Sachsen-Anhalt)

shortenStateNames <- function(name) {
  rValue <- case_when(
    str_detect(name, regex("Nordrhein", ignore_case = TRUE)) ~ 'NW',
    str_detect(name, regex("Mecklenburg", ignore_case = TRUE)) ~ 'MV',
    str_detect(name, regex("Bremen", ignore_case = TRUE)) ~ 'HB',
    str_detect(name, regex("Bayern", ignore_case = TRUE)) ~ 'BY',
    str_detect(name, regex("Hamburg", ignore_case = TRUE)) ~ 'HH',
    str_detect(name, regex("Niedersachsen", ignore_case = TRUE)) ~ 'NI',
    str_detect(name, regex("Rheinland", ignore_case = TRUE)) ~ 'RP',
    str_detect(name, regex("Baden", ignore_case = TRUE)) ~ 'BW',
    str_detect(name, regex("Anhalt", ignore_case = TRUE)) ~ 'ST',
    str_detect(name, regex("Sachsen", ignore_case = TRUE)) ~ 'SN',
    str_detect(name, regex("Brandenburg", ignore_case = TRUE)) ~ 'BB',
    str_detect(name, regex("ringen", ignore_case = TRUE)) ~ 'TH',
    str_detect(name, regex("Berlin", ignore_case = TRUE)) ~ 'BE',
    str_detect(name, regex("Hessen", ignore_case = TRUE)) ~ 'HE',
    str_detect(name, regex("Saarland", ignore_case = TRUE)) ~ 'SL',
    str_detect(name, regex("Schleswig", ignore_case = TRUE)) ~ 'SH'
  )
  return(rValue)
}
# List taken from https://www.destatis.de/DE/Methoden/abkuerzung-bundeslaender-DE-EN.html


# Recursive function that picks off one column name at a time to expand out into
# one-hot encoded columns

makeOneHot <- function(df, listColNames) {
  if (length(listColNames) == 1) {
    df <-
      df %>% pivot_wider(
        names_from = as.symbol(listColNames[[1]]),
        values_from = as.symbol(listColNames[[1]]),
        values_fn = function(x) {
          1
        },
        values_fill = 0,
        names_prefix = listColNames,
        names_repair = 'unique'
      )
  } else {
    df <- makeOneHot(df, listColNames[1])
    df <- makeOneHot(df, listColNames[-1])
  }
  return(df)
}


# We sometimes need to get a list of all those one-hot encoded column names

getOneHotColnames <- function(df, lOHCols) {
  lResult <-
    sapply(lOHCols, function(x) {
      colnames(df)[grep(x, colnames(df))]
    })
  return(unlist(lResult))
}


# Replace NAs and turn character columns into factors.  Ultimnately we only work
# with their numerical encoding

doStandardise <- function(df) {
  df <- df %>% mutate_if(is.numeric, funs(ifelse(is.na(.), 0, .)))
  df <-
    df %>% mutate_if(is.character, funs(ifelse(is.na(.), '', .)))
  
  dfn <- df %>%
    mutate(across(where(is.character), as.factor)) %>%
    mutate(across(where(is.factor), as.numeric))
  
  # Tests for predictors that need to be ignored
  lXCols <- names(dfn)
  ignoreX <- names(dfn)[nearZeroVar(dfn)]
  lXCols <- setdiff(lXCols, ignoreX)
  dfn <- dfn[, lXCols]
  
  descrCor <-  cor(dfn)
  ignoreX <- sum(abs(descrCor[upper.tri(descrCor)]) > .999)
  lXCols <- setdiff(lXCols, ignoreX)
  dfn <- dfn[, lXCols]
  
  # Normalisation of matrix
  rStandardise <- preProcess(dfn,
                             method = c('center', 'scale'))
  dfns <- predict(rStandardise, dfn)
  
  return(dfns)
}


fRMSE <- function(yp, ya) {
  sqrt(mean((yp - ya) ^ 2))
}


# This takes a dataset, splits it into a training and testing sets with sampling
# using a given method, and then applies a training algorithm determined by its
# parameters. The trained model is stored to disk.
# The function then tests model performance on the test dataset, and reports key
# stats back to the calling script.

doTestMethod <- function(df,
                         lXCols,
                         yCol,
                         trainMethodName,
                         samplingMethodName = 'up',
                         forceCalc = FALSE,
                         returnModel = FALSE,
                         versionTag = NA,
                         ...) {
  dfName <- deparse(substitute(df))
  fName <-
    paste0(
      folderTemp,
      'doTestMethod-',
      dfName,
      '-',
      trainMethodName,
      '-',
      samplingMethodName,
      ifelse(is.na(versionTag), '', paste0('-v', versionTag)),
      '.RData'
    )
  if ((forceCalc == FALSE) & file.exists(fName)) {
    load(file = fName)
  } else {
    if (exists('fixSeed')) {
      if (fixSeed) {
        set.seed(571611)
      }
    }
    
    ix <- createDataPartition(df[[yCol]],
                              p = 0.5,
                              list = TRUE)[[1]]
    
    dfX <- doStandardise(df[, lXCols])
    dfY <- factor(ifelse(df[, yCol][[1]], 'yes', 'no'),
                  levels = c('yes', 'no'))
    
    fTrainControl <- trainControl(
      method = 'repeatedcv',
      number = 10,
      repeats = 5,
      summaryFunction = twoClassSummary,
      classProbs = TRUE,
      verboseIter = TRUE,
      sampling = samplingMethodName,
      search = 'random',
      allowParallel = TRUE
    )
    
    m <- tryCatch(
      train(
        dfX[ix, ],
        dfY[ix],
        method = trainMethodName,
        metric = 'ROC',
        trControl = fTrainControl,
        tuneLength = 30,
        ...
      ),
      error = function(e) {
        return(NA)
      }
    )
    
    if (!is.na(m)) {
      pY <- predict(m, newdata = dfX[-ix, ])
      
      rP <- precision(pY, dfY[-ix])
      rR <- recall(pY, dfY[-ix])
      rCM <- confusionMatrix(pY, dfY[-ix])
      rRMSE <- fRMSE(pY, dfY[-ix])
      rValue <- list(
        'Precision' = rP,
        'Recall' = rR,
        'Confusionmatrix' = rCM,
        'Training' = trainMethodName,
        'Sampling' = samplingMethodName,
        'Model' = m
      )
    } else {
      rValue <- list(
        'Precision' = NA,
        'Recall' = NA,
        'Confusionmatrix' = NA,
        'Training' = trainMethodName,
        'Sampling' = samplingMethodName,
        'Model' = NA
      )
    }
    save(rValue, file = fName)
  }
  if (!returnModel) {
    rValue['Model'] <- NA
  }
  return(rValue)
}
