if(!require(dplyr)) {
  install.packages("dplyr")
  library("dplyr")
}
if (!require(tidyverse)) {
  install.packages("tidyverse")
  library("tidyverse")
}
if (!require(caret)) {
  install.packages("caret")
  library("caret")
}
if (!require(doParallel)) {
  install.packages("doParallel")
  library("doParallel")
}

source('Definitions.R')
source('Settings.R')
source('SupportFunctions.R')
source('Colours.R')


forceRecalc <- FALSE


# Make sure we use parallel processing to speed up things
# Note that we MUST NOT set up a cluster etc - registerDoParallel() is all that
# is required.  Doing anything else slows down processing!

registerDoParallel()


# Load data

load(paste0(folderTemp, 'dictKeyStats.RData'))
load(paste0(folderRData, 'MPs.RData'))
load(paste0(folderRData, 'politicians.RData'))
load(paste0(folderRData, 'votes.RData'))
load(paste0(folderRData, strDate, '-motions.RData'))
load(paste0(folderRData, 'constituencies.RData'))
mRegions <- read.csv(
  file = paste0(folderTemp, '20170228_BTW17_WKr_Gemeinden_ASCII.csv'),
  skip = 7,
  sep = ';',
  header = TRUE
)


# QUESTION Can we predict which MPs will fall into the 'frequent no show'
# category?

politicians$party.label <- sapply(politicians$party.label, standardisePartyNames)
constituencies <- constituencies %>%
  left_join(mRegions, by = c('number' = 'Wahlkreis.Nr')) %>%
  select(c('id', 'Landname')) %>%
  unique()

inactiveMPs <- votes %>% 
  left_join(MPs, by=c('mandate.id' = 'id')) %>% 
  left_join(politicians, by=c('politician.id'='id')) %>% 
  filter(is.na(label)) %>% 
  select(c('mandate.id')) %>% 
  unique()
activeMPs <- MPs

nsVotes <- activeMPs %>% left_join(votes, by = c('id' = 'mandate.id')) %>%
  filter(vote == 'no_show') %>%
  group_by(id) %>%
  dplyr::summarize(count = n(), .groups = 'keep')

activeMPs <- activeMPs %>%
  left_join(nsVotes, by = c('id' = 'id'))
activeMPs[is.na(activeMPs$count), 'count'] <- 0
activeMPs <- activeMPs %>%
  mutate(share = count / nrow(motions))
activeMPs <- activeMPs %>%
  left_join(politicians, by = c('politician.id' = 'id'))
activeMPs <- activeMPs %>%
  left_join(constituencies, by = c('electoral_data.constituency.id' = 'id'))


# Tidy up a few fields

activeMPs$Landname <- sapply(activeMPs$Landname, shortenStateNames)
activeMPs$fraction_membership.label <-
  lapply(activeMPs$fraction_membership.label,
         doConsolidateFraktionLabels)


# Is this MP more frequently absent?

activeMPs$isNSMP <- activeMPs$share >= dictKeyStats['cutoffMissedVotes']


# How did the MP come into their role: via parties' candidate lists, via a
# direct mandate, or as a replacement for a departing MP?
# For background on the German federal electoral system, see e.g.
# https://en.wikipedia.org/wiki/Electoral_system_of_Germany

activeMPs$listMP <- activeMPs$electoral_data.mandate_won == 'list'
activeMPs$listMP[is.na(activeMPs$listMP)] <- FALSE
activeMPs$constituencyMP <- activeMPs$electoral_data.mandate_won == 'constituency'
activeMPs$constituencyMP[is.na(activeMPs$constituencyMP)] <- FALSE
activeMPs$otherMP <- !(activeMPs$listMP | activeMPs$constituencyMP)


# Get everything set up for predicting whether an MP is a frequent no-show

YCol <- 'isNSMP'
XColsMPsConsolidated <- c(
  'electoral_data.list_position',
  'electoral_data.constituency_result',
  'fraction_membership.label',
  'sex',
  'party.label',
  'age',
  'answeredShare',
  'Landname',
  'listMP',
  'constituencyMP',
  'otherMP'
)
XColsMPsOneHot <- c('fraction_membership.label',
                    'sex',
                    'party.label',
                    'Landname')

dfTemp <- activeMPs %>%
  select(union(YCol, XColsMPsConsolidated)) %>% unique()
activeMPOneHot <- makeOneHot(dfTemp, XColsMPsOneHot)
XCols <- union(
  setdiff(XColsMPsConsolidated, XColsMPsOneHot),
  getOneHotColnames(activeMPOneHot, XColsMPsOneHot)
)


# Try a selection of common ML algorithms

lMethodSpecs1 <- list(
  list(
    'trainMethod' = 'glm',
    'sampleMethod' = 'up',
    'extraParams' = NA
  ),
  list(
    'trainMethod' = 'rda',
    'sampleMethod' = 'up',
    'extraParams' = NA
  ),
  list(
    'trainMethod' = 'bayesglm',
    'sampleMethod' = 'up',
    'extraParams' = NA
  ),
  list(
    'trainMethod' = 'lda',
    'sampleMethod' = 'up',
    'extraParams' = NA
  ),
  list(
    'trainMethod' = 'knn',
    'sampleMethod' = 'up',
    'extraParams' = NA
  ),
  list(
    'trainMethod' = 'cforest',
    'sampleMethod' = 'up',
    'extraParams' = NA
  ),
  list(
    'trainMethod' = 'treebag',
    'sampleMethod' = 'up',
    'extraParams' = NA
  )
)


# This is the core sapply call where the learning is done, and performance of
# the learned models is tested.  We collect results in a list that is then
# written to disk.
# The doTestMethod() function includes a number of optional parameters that were
# helpful during development.  We can force a recalculation of the learning model
# by deleting the summary results file and setting the forceRecalc flag.

fName <- paste0(folderTemp, 'lConsolidatedResultsMPs.RData')
if (!file.exists(fName)) {
  lConsolidatedResultsMPs <- sapply(lMethodSpecs1, function(x) {
    if (!is.na(x$extraParams)) {
      r <- doTestMethod(
        activeMPOneHot,
        XCols,
        YCol,
        trainMethodName = x$trainMethod,
        samplingMethodName = x$sampleMethod,
        forceCalc = get0('forceRecalc', ifnotfound = FALSE),
        returnModel = FALSE,
        versionTag = 0,
        parse(text = x$extraParams)
      )
    } else {
      r <- doTestMethod(
        activeMPOneHot,
        XCols,
        YCol,
        trainMethodName = x$trainMethod,
        samplingMethodName = x$sampleMethod,
        forceCalc = get0('forceRecalc', ifnotfound = FALSE),
        returnModel = FALSE,
        versionTag = 0
      )
    }
    
    return(r[c('Training', 'Sampling', 'Precision', 'Recall')])
  })
  save(lConsolidatedResultsMPs, file = fName)
}


# FINDINGS None of the methods is particularly great at identifying those MPs
# that are most likely to be frequent 'no shows'


# QUESTION Does the choice of sampling method make a (positive) difference?

lMethodSpecsSamplingTests <- list(
  list(
    'trainMethod' = 'glm',
    'sampleMethod' = 'up',
    'extraParams' = NA
  ),
  list(
    'trainMethod' = 'glm',
    'sampleMethod' = 'down',
    'extraParams' = NA
  ),
  list(
    'trainMethod' = 'glm',
    'sampleMethod' = 'smote',
    'extraParams' = NA
  ),
  list(
    'trainMethod' = 'glm',
    'sampleMethod' = 'rose',
    'extraParams' = NA
  )
)

fName <- paste0(folderTemp, 'lMPResultsSamplingTests.RData')
if (!file.exists(fName)) {
  lMPResultsSamplingTests <-
    sapply(lMethodSpecsSamplingTests, function(x) {
      if (!is.na(x$extraParams)) {
        r <- doTestMethod(
          activeMPOneHot,
          XCols,
          YCol,
          trainMethodName = x$trainMethod,
          samplingMethodName = x$sampleMethod,
          forceCalc = get0('forceRecalc', ifnotfound = FALSE),
          returnModel = FALSE,
          versionTag = 0,
          parse(text = x$extraParams)
        )
      } else {
        r <- doTestMethod(
          activeMPOneHot,
          XCols,
          YCol,
          trainMethodName = x$trainMethod,
          samplingMethodName = x$sampleMethod,
          forceCalc = get0('forceRecalc', ifnotfound = FALSE),
          returnModel = FALSE,
          versionTag = 0
        )
      }
      
      return(r[c('Training', 'Sampling', 'Precision', 'Recall')])
    })
  save(lMPResultsSamplingTests, file = fName)
}


# FINDINGS Upsampling is actually best


# QUESTION Can we predict a no-show vote? First, let's try without reference to
# the isNSMP flag


# We need to set up a dataframe with all relevant predictors included

XColsVotes <- c('weekday',
                'field_committees.label',
                'field_topics.label')
XCols <- c(XColsMPsConsolidated, XColsVotes)

activeVotes <- activeMPs %>%
  left_join(votes, by = c('id' = 'mandate.id')) %>%
  left_join(motions, by = c('poll.id' = 'id')) %>%
  mutate(isNSVote = (vote == 'no_show')) %>%
  filter(!is.na(poll.id))

# Categorical predictors need to be one-hot encoded

XColsVotesOneHot <- c(
  'fraction_membership.label',
  'sex',
  'party.label',
  'Landname',
  'weekday',
  'field_committees.label',
  'field_topics.label'
)

YCol <- 'isNSVote'

dfTemp <- activeVotes %>%
  select(all_of(c(
    YCol, XColsMPsConsolidated, XColsVotesOneHot
  ))) %>%
  unique()
activeVotesOneHot <- makeOneHot(dfTemp, XColsVotesOneHot)
XCols <- union(
  setdiff(XCols, XColsVotesOneHot),
  getOneHotColnames(activeVotesOneHot, XColsVotesOneHot)
)


# We specify a (shortened) list of ML algorithms: my tests have shown that some
# of the other algorithms are too computing intensive, at least on my laptop

lMethodSpecs2 <- list(
  list(
    'trainMethod' = 'glm',
    'sampleMethod' = 'up',
    'extraParams' = NA
  ),
  list(
    'trainMethod' = 'lda',
    'sampleMethod' = 'up',
    'extraParams' = NA
  ),
  list(
    'trainMethod' = 'bayesglm',
    'sampleMethod' = 'up',
    'extraParams' = NA
  )
)

fName <- paste0(folderTemp, 'lResultsVotes.RData')
if (!file.exists(fName)) {
  lResultsVotes <- sapply(lMethodSpecs2, function(x) {
    if (!is.na(x$extraParams)) {
      r <- doTestMethod(
        activeVotesOneHot,
        XCols,
        YCol,
        trainMethodName = x$trainMethod,
        samplingMethodName = x$sampleMethod,
        forceCalc = get0('forceRecalc', ifnotfound = FALSE),
        returnModel = FALSE,
        versionTag = 0,
        parse(text = x$extraParams)
      )
    } else {
      r <- doTestMethod(
        activeVotesOneHot,
        XCols,
        YCol,
        trainMethodName = x$trainMethod,
        samplingMethodName = x$sampleMethod,
        forceCalc = get0('forceRecalc', ifnotfound = FALSE),
        returnModel = FALSE,
        versionTag = 0
      )
    }
    return(r[c('Training', 'Sampling', 'Precision', 'Recall')])
  })
  save(lResultsVotes, file = fName)
}


# FINDINGS Overall, these are not great results in predicting no-show behaviour
# We hypothesise that a number of factors outside the dataset used here likely
# play a significant role: this would need to be analysed further in subsequent
# work (out of scope of this capstone project)


# QUESTION Does adding a flag that highlights the most conspicuously no-show MPs
# improve predictive performance?

XCols <- append(XCols,  'isNSMP')
dfTemp <- activeVotes %>%
  select(all_of(c(
    YCol, XColsMPsConsolidated, 'isNSMP', XColsVotesOneHot
  ))) %>%
  unique()
activeVotesOneHot <- makeOneHot(dfTemp, XColsVotesOneHot)

fName <- paste0(folderTemp, 'lResultsVotesEnhanced.RData')
if (!file.exists(fName) | get0('forceRecalc', ifnotfound = FALSE)) {
  lResultsVotesEnhanced <- sapply(lMethodSpecs2[1], function(x) {
    print(paste0('Running ', x$trainMethod, '(', x$sampleMethod, ')'))
    if (!is.na(x$extraParams)) {
      r <- doTestMethod(
        activeVotesOneHot,
        XCols,
        YCol,
        trainMethodName = x$trainMethod,
        samplingMethodName = x$sampleMethod,
        forceCalc = get0('forceRecalc', ifnotfound = FALSE),
        returnModel = FALSE,
        versionTag = 1,
        parse(text = x$extraParams)
      )
    } else {
      r <- doTestMethod(
        activeVotesOneHot,
        XCols,
        YCol,
        trainMethodName = x$trainMethod,
        samplingMethodName = x$sampleMethod,
        forceCalc = get0('forceRecalc', ifnotfound = FALSE),
        returnModel = FALSE,
        versionTag = 1
      )
    }
    return(r[c('Training', 'Sampling', 'Precision', 'Recall')])
  })
  save(lResultsVotesEnhanced, file = fName)
}


# FINDINGS Sadly, the flag does not make a meaningful difference


# We now stop the (implicitly created) local multi-processor cluster
stopImplicitCluster()


# ... and we are done!
print('Done.\n\n')