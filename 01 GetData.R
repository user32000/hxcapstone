if(!require(tidyverse)) {
  install.packages("tidyverse", repos = "http://cran.us.r-project.org")
  library("tidyverse")
}
  
if (!require(dplyr)) {
  install.packages("dplyr", repos = "http://cran.us.r-project.org")
  library("dplyr")
}
  


source('Settings.R')


# First, need to fetch list of Members of Parliament (MPs), filtered for MPs at
# the federal government level ('Bundestag') and in the current legislative period.
# The source for this data is abgeordnetenwatch.de which provides an API to
# access this data.
#
# The API is documented at https://www.abgeordnetenwatch.de/api/
# We're only going to request this from abgeordnetenwatch.de once, and then store
# the results. For that purpose we'll define a helper function
#
# Note that the abgeordnetenwatch API counts pages from 0.  A page parameter is
# only permitted if the (unpaged) API call returns with incomplete data

# We load in a helper function

source("getJSON.R")


# We then also define the relevant API calls
# Get a reference to the federal parliament ('Bundestag')

print('Fetching reference to current parliamentary period\n')
parliamentsAPI <- c(name = 'parliaments.JSON',
                    url = 'https://www.abgeordnetenwatch.de/api/v2/parliaments')
parliaments <- getJSON(parliamentsAPI['name'],
                       parliamentsAPI['url'],
                       path = folderJSON)
BundestagID <- parliaments[parliaments["label"] == "Bundestag", ]$id
currentBundestagID <- parliaments[parliaments["id"] == BundestagID, ]$current_project$id


# Get a list of all parliamentary periods for the federal parliament available via this API

print('Getting list of earlier parliamentary periods\n')
PeriodsAPI <- c(
  name = 'periods.JSON',
  url = paste0(
    'https://www.abgeordnetenwatch.de/api/v2/parliament-periods?parliament=',
    BundestagID
  )
)
Periods <- getJSON(PeriodsAPI['name'],
                   PeriodsAPI['url'],
                   path = folderJSON,
                   silent=FALSE)
save(Periods, file = paste0(folderRData, 'Periods-RAW.RData'))

previousBundestagID <- Periods[Periods$type == 'legislature', ] %>% slice(2) %>% pull('id')


# Get a list of all Members of Parliament in the previous legislative period

print('Downloading list of MPs\n')
MPsAPI <- c(
  name = 'mps.JSON',
  url = paste0(
    'https://www.abgeordnetenwatch.de/api/v2/candidacies-mandates?parliament_period=',
    previousBundestagID
  )
)
MPs <- getJSON(MPsAPI['name'],
               MPsAPI['url'],
               path = folderJSON,
               pageParameter = '&page=',
               silent=FALSE)
save(MPs, file = paste0(folderRData, 'MPs-RAW.RData'))


# OK we also want to know how long the current MPs have been in parliament

print('Fetching MPs history in parliament\n')
MPhistoryAPI <- c(name = 'mphistory.JSON',
                  url = 'https://www.abgeordnetenwatch.de/api/v2/candidacies-mandates/')
MPhistory <- sapply(MPs$id, function(id) {
  unlist(getJSON(
    paste0(id, '-', MPhistoryAPI['name']),
    paste0(MPhistoryAPI['url'], id),
    path = folderJSON,
    silent=FALSE
  ))
}, simplify = FALSE) %>% bind_rows()
save(MPhistory, file = paste0(folderRData, 'mphistory-RAW.RData'))


# However, tbe MP entity does not give a complete picture, hence we also need to
# get a list of politicians' data to augment MP datasets

print('Getting politicians\n')
politiciansAPI <- c(name = 'politicians.JSON',
                    url = 'https://www.abgeordnetenwatch.de/api/v2/politicians/')
politicians <- sapply(as.list(MPs$politician[['id']]), function(id) {
  unlist(getJSON(
    paste0(id, '-', politiciansAPI['name']),
    paste0(politiciansAPI['url'], id),
    path = folderJSON,
    silent=FALSE
  ))
}, simplify = FALSE) %>% bind_rows()
save(politicians, file = paste0(folderRData, 'politicians-RAW.RData'))


# Get a list of all constituencies

print('Dowloading complete list of constituencies\n')
constituencyAPI <- c(name = 'constituencies.JSON',
                     url = 'https://www.abgeordnetenwatch.de/api/v2/constituencies')
constituencies <- getJSON(constituencyAPI['name'], constituencyAPI['url'], path = folderJSON)
save(constituencies, file = paste0(folderRData, 'constituencies.RData'))


# Get a list of all parties in the legislative period we are working on

print('Fetching list of parties in parliament\n')
partiesAPI <- c(name = 'parties.JSON',
                url = 'https://www.abgeordnetenwatch.de/api/v2/parties')
parties <- getJSON(partiesAPI['name'],
                   partiesAPI['url'],
                   path = folderJSON,
                   silent=FALSE)
save(parties, file = paste0(folderRData, 'parties-RAW.RData'))


# Get a list of all parliamentary groups in the current legislative period

print('Retrieving list of parliamentary working groups\n')
fractionsAPI <- c(
  name = 'fractions.JSON',
  url = paste0(
    'https://www.abgeordnetenwatch.de/api/v2/fractions?legislature=',
    previousBundestagID
  )
)
fractions <- getJSON(fractionsAPI['name'],
                     fractionsAPI['url'],
                     path = folderJSON,
                     silent=FALSE)
save(fractions, file = paste0(folderRData, 'fractions-RAW.RData'))


# Get a list of all committees and their memberships

print('... and committees and their memberships\n')
committeeAPI <- c(
  name = 'committees.JSON',
  url = paste0(
    'https://www.abgeordnetenwatch.de/api/v2/committees?field_legislature=',
    previousBundestagID
  )
)
committees <- getJSON(committeeAPI['name'],
                      committeeAPI['url'],
                      path = folderJSON,
                      silent=FALSE)
save(committees, file = paste0(folderRData, 'committees-RAW.RData'))

committeeMembershipsAPI <- c(name = 'committeeMemberships.JSON',
                             url = 'https://www.abgeordnetenwatch.de/api/v2/committee-memberships')
committeeMemberships <- sapply(committees$id, function(id) {
  getJSON(
    paste0(id, '-', committeeMembershipsAPI['name']),
    paste0(committeeMembershipsAPI['url'], '?committee=', id),
    path = folderJSON,
    silent=FALSE
  )
}, simplify = FALSE) %>% bind_rows()
save(committeeMemberships,
     file = paste0(folderRData, 'committeeMemberships-RAW.RData'))


# Get a list of all motions of the current legislative period

print('Dowloading complete list of motions\n')
motionsAPI <- c(
  name = paste0(Sys.Date(), '-motions.JSON'),
  url = paste0(
    'https://www.abgeordnetenwatch.de/api/v2/polls?field_legislature=',
    previousBundestagID
  )
)
motions <- getJSON(motionsAPI['name'],
                   motionsAPI['url'],
                   path = folderJSON,
                   silent=FALSE)
save(motions, file = paste0(folderRData, Sys.Date(), '-motions-RAW.RData'))


# Get a classification list for topics that parliament votes on

print('... and topics\n')
topicsAPI <- c(name = 'topics.JSON',
               url = 'https://www.abgeordnetenwatch.de/api/v2/topics')
topics <- getJSON(topicsAPI['name'],
                  topicsAPI['url'],
                  path = folderJSON,
                  silent=FALSE)
save(topics, file = paste0(folderRData, 'topics-RAW.RData'))


# Get a list of all votes cast in the current legislative period
# This call returns a list of dataframes, hence piping the output through
# bind_rows to create one big joined-up dataframe.
# We'll try to only call this once per day and store the results in a consolidated
# votes JSON

print('Retrieving record of votes cast on each motion\n')
consolidatedVotesJSONFileName <- paste0(folderJSON, Sys.Date(), '-consolidatedVotes.JSON')
votesAPI <-
  c(name = 'votes.JSON', url = 'https://www.abgeordnetenwatch.de/api/v2/votes')
votes <- sapply(motions$id, function(id) {
  getJSON(paste0(id, '-', votesAPI['name']),
          paste0(votesAPI['url'], '?poll=', id),,
          pageParameter = '&page=',
          path = folderJSON,
          silent=FALSE)
}, simplify = FALSE) %>% bind_rows()
save(votes, file = paste0(folderRData, 'votes-RAW.RData'))


# Finally, we also need to get two additional datasets:
# 1. a shape file to draw all voting districts
# 2. a mapping of voting districts

if (!file.exists(paste0(folderTemp, 'Geometrie_Wahlkreise_19DBT_VG250.shp'))) {
  temp <- tempfile()
  download.file(
    'https://www.bundeswahlleiter.de/dam/jcr/67e3e9b8-dbca-4bc9-8977-ac792665bbce/btw17_geometrie_wahlkreise_vg250_shp.zip',
    temp
  )
  unzip(temp, exdir = folderTemp)
  unlink(temp)
}

if (!file.exists(paste0(folderTemp, '20170228_BTW17_WKr_Gemeinden_ASCII.csv'))) {
  temp <- tempfile()
  download.file(
    'https://www.bundeswahlleiter.de/dam/jcr/0a36af27-fea1-48a6-9776-2a4a8b238ad9/btw17_wkr_gemeinden.zip',
    temp
  )
  unzip(temp, files = '20170228_BTW17_WKr_Gemeinden_ASCII.csv', exdir =
          folderTemp)
  unlink(temp)
}

strDate <- format(Sys.Date(), "%Y-%m-%d")
save(strDate, file = paste0(folderRData, 'MostRecentAPIDownload.RData'))

print('\nDone.\n\n')
