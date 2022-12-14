if (!require(plyr)) { install.packages("plyr") }
if (!require(jsonlite)) {
  install.packages("jsonlite", repos = "http://cran.us.r-project.org")
  }


getJSON <- function(name,
                    URL,
                    path = '',
                    forceReload = FALSE,
                    silent = TRUE,
                    pageParameter = 'page=') {
  filename = paste0(path, name)
  
  # Reload from cached file if possible
  if (!file.exists(filename) | forceReload) {
    pages = list()
    pageNumber <- 0
    collectedItems <- 0
    totalItems <- 0
    
    # How do we add the page parameter
    pageParameterConnectingChar <-
      ifelse(grepl('\\?', URL), '', '?')
    
    if (collectedItems == 0) {
      if (!silent) {
        message(sprintf("Requesting %s", URL))
      }
      tryCatch(
        dataJSON <- fromJSON(URL),
        error = function(e)
          stop(e)
      )
      if (toupper(dataJSON$meta$status) != "OK")
        stop(
          sprintf(
            "Encountered error '%s' when fetching %s.  Aborting",
            dataJSON$meta$status,
            URL
          )
        )
      if ('count' %in% names(dataJSON$meta$result)) {
        collectedItems <- as.numeric(dataJSON$meta$result['count'])
        totalItems <- as.numeric(dataJSON$meta$result['total'])
      } else {
        collectedItems <- 1
        totalItems <- 1
      }
      pages[[pageNumber + 1]] <- dataJSON$data
      pageNumber <- pageNumber + 1
    }
    
    # There are more items to fetch, we'll have to page them
    while (collectedItems < totalItems) {
      url <-
        paste0(URL,
               pageParameterConnectingChar ,
               pageParameter,
               as.character(pageNumber))
      if (!silent) {
        message(
          sprintf(
            "Requesting %s (got %5.0f of %5.0f items)",
            url,
            collectedItems,
            totalItems
          )
        )
      }
      tryCatch(
        dataJSON <- fromJSON(url),
        error = function(e)
          stop(e)
      )
      if (toupper(dataJSON$meta$status) != "OK")
        stop(
          sprintf(
            "Encountered error '%s' when fetching %s.  Aborting",
            dataJSON$meta$status,
            url
          )
        )
      pages[[pageNumber + 1]] <- dataJSON$data
      collectedItems <-
        collectedItems + as.numeric(dataJSON$meta$result['count'])
      pageNumber <- pageNumber + 1
    }
    
    if (collectedItems == 0)
      stop(sprintf("Did not collect any data when fetching %s.  Aborting", URL))
    
    if (!silent) {
      message(sprintf(
        "Got %5.0f of %5.0f items in total",
        collectedItems,
        totalItems
      ))
    }
    
    # Merge the pages we reeived into one single JSON
    if (length(pages) > 1) {
      dataJSON <- rbind_pages(pages)
    } else {
      dataJSON <- pages[[1]]
    }
    exportJSON <- toJSON(dataJSON)
    write(exportJSON, filename)
  } else {
    if (!silent) {
      message(sprintf("Loading %s from %s", URL, filename))
    }
    dataJSON <- fromJSON(filename)
  }
  return(dataJSON)
}
