if(!require(tidyverse)) {
  install.packages("tidyverse")
  library("tidyverse")
  }

source('Settings.R')


if (!file.exists(paste0(folderFilters, 'columns.lua'))) {
  download.file(url = 'https://github.com/jdutant/columns/raw/master/columns.lua',
                destfile = paste0(folderFilters, 'columns.lua'))
}
# With very special thanks to John C. Lokman and 
# https://levelup.gitconnected.com/use-columns-adjust-margins-and-do-more-in-markdown-with-these-simple-pandoc-commands-adb4c19f9f35
