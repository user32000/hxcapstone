if(!require(stringdist)) {
  install.packages('stringdist')
  library('stringdist')
  }

source('SupportFunctions.R')


colVotes <- c('#008000', '#800000', '#C0C0C0', '#000000')
names(colVotes) <- c('yes', 'no', 'abstain', 'no_show')

colFractions <- c(
  '#804087',
  '#B82E2F',
  '#59803D',
  '#333335',
  '#DCB63F',
  '#4687C3',
  '#999999',
  '#EEEEEE'
)
names(colFractions) <- c('DIE LINKE',
                         'SPD',
                         'DIE GRÜNEN',
                         'CDU/CSU',
                         'FDP',
                         'AfD',
                         'fraktionslos',
                         'NA')

colParties <- c(
  '#804087',
  '#B82E2F',
  '#59803D',
  '#333335',
  '#333335',
  '#DCB63F',
  '#A62931',
  '#4687C3',
  '#BBBBBB',
  '#BBBBBB',
  '#BBBBBB',
  '#BBBBBB'
)
names(colParties) <- c(
  'DIE LINKE',
  'SPD',
  'DIE GRÜNEN',
  'CDU',
  'CSU',
  'FDP',
  'Die PARTEI',
  'AfD',
  'ZENTRUM',
  'Parteilos',
  'parteilos',
  'NA'
)
levelsParties <- c(
  'CDU',
  'CSU',
  'SPD',
  'FDP',
  'DIE GRÜNEN',
  'DIE LINKE',
  'Die PARTEI',
  'AfD',
  'ZENTRUM',
  'Parteilos',
  'parteilos',
  'NA'
)

colJoint <- c(colVotes, colParties)
dfColours <- data.frame(label = names(colJoint), colour = colJoint)

colHighlight <- c('#CCCCCC', '#960200')
names(colHighlight) <- c(FALSE, TRUE)

colHighlight2 <- c('#EEEEEE', '#960200')
names(colHighlight2) <- c(FALSE, TRUE)
