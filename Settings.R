fixSeed <- TRUE


folderJSON <- './JSON_files/'
folderRData <- './RData_files/'
folderImages <- './images/'
folderFilters <- './filters/'
folderTemp <- './temp/'


createFolderIfNeeded <- function(f) {
  if (!file.exists(f)) {
    dir.create(f)
  }
}

createFolderIfNeeded(folderJSON)
createFolderIfNeeded(folderRData)
createFolderIfNeeded(folderImages)
createFolderIfNeeded(folderFilters)
createFolderIfNeeded(folderTemp)


# Set this to manually override the date of the API download to use in the 
# analysis
# strDate <- '2022-08-25'

strDate <- get0('strDate', ifnotfound = NA)
if (is.na(strDate)) {
  load(file = paste0(folderRData, 'MostRecentAPIDownload.RData'))
}


# Start and end date of the parliamentary period we are analysing

sStartDate <- '24 October 2017'
sEndDate <- '26 October 2021'

options(future.globals.maxSize = 1.2 * 1024 ^ 3)