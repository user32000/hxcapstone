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


# Set this to the date of the API downlaod to use in the analysis

strDate <- '2022-08-25'

if (is.na(strDate)) {
  strDate <- Sys.Date()
}


# Start and end date of the parliamentary period we are analysing

sStartDate <- '24 October 2017'
sEndDate <- '26 October 2021'

options(future.globals.maxSize = 1.2 * 1024 ^ 3)