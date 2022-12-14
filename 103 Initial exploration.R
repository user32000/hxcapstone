if(!require(jsonlite)) { install.packages("jsonlite") }
if (!require(lubridate)) { install.packages("lubridate") }
if (!require(dplyr)) { install.packages("dplyr") }
if (!require(purrr)) { install.packages("purrr") }
if (!require(assertr)) { install.packages("assertr") }
if (!require(tidyverse)) { install.packages("tidyverse") }
if (!require(sf)) { install.packages("sf") }
if (!require(ggplot2)) { install.packages("ggplot2") }
if (!require(ggthemes)) { install.packages("ggthemes") }
if (!require(scales)) { install.packages("scales") }
if (!require(gridExtra)) { install.packages("gridExtra") }

source('Definitions.R')
source('Settings.R')
source('VotePie.R')

# We store data points that will be needed in the report in this dict so that
# we don't have to replicate calculations in the report file


if (file.exists(paste0(folderTemp, 'dictKeyStats.RData'))) {
  load(paste0(folderTemp, 'dictKeyStats.RData'))
} else {
  dictKeyStats <- c()
}

# Calculate some baseline statistics for our report

load(paste0(folderRData, 'Periods.RData'))
dictKeyStats['earliestParliament'] <- min(Periods[Periods$type == 'legislature', 'start_date_period'])
dictKeyStats['currentParliamentStartDate'] <- max(Periods[Periods$type == 'legislature', 'start_date_period'])
dictKeyStats['currentParliamentEndDate'] <- max(Periods[Periods$type == 'legislature', 'end_date_period'])
dictKeyStats['numPeriodsInAPI'] <- nrow(Periods)

load(paste0(folderRData, strDate, '-sKMotions.RData'))
dictKeyStats['mostRecentVoteDate'] <- max(skMotions$field_poll_date)

load(paste0(folderRData, 'votes.RData'))
nsVoteShare <- summary(votes$vote)['no_show'] / nrow(votes)

# QUESTION What is the composition of parliament in this period?
load(paste0(folderRData, 'MPs.RData'))
load(paste0(folderRData, 'politicians.RData'))

politicians$party.label <- sapply(politicians$party.label, standardisePartyNames)

gMPbyParty <- MPs %>%
  left_join(politicians, by = c('politician.id' = 'id')) %>%
  plotPie(inner = 'party.label', coloursInner = colParties) +
  ggtitle('Composition of parliament') +
  theme(
    panel.background = element_rect(fill = "transparent", colour = NA),
    plot.background = element_rect(fill = "transparent", colour = NA)
  )
gMPbyParty
ggsave(
  plot = gMPbyParty,
  width = 7,
  height = 7,
  units = 'cm',
  dpi = 'print',
  path = folderImages,
  filename = 'Composition of parliament.jpg'
)


# QUESTION What is overall attendance at parliamentary votes?
# FINDINGS In total, 10% of potential votes are 'no shows'
load(file = paste0(folderRData, 'Votes.RData'))

dictKeyStats['ShareNoShowTotal'] = sum(votes$vote == 'no_show')/nrow(votes)
plotPie(votes, inner = 'vote', coloursInner = colVotes) +
  labs(title = "Voting behaviour, all MPs")


# QUESTION Who are the top 'no shows'?
# FINDINGS The top 'no shows' include Ingrid Remmers and Jimmy Schulz.  A quick
# search shows they both died during the legislative period:
# 1. https://en.wikipedia.org/wiki/Ingrid_Remmers
# 2. https://en.wikipedia.org/wiki/Jimmy_Schulz

load(file = paste0(folderRData, strDate, '-Motions.RData'))
load(file = paste0(folderRData, 'MPs.RData'))
load(paste0(folderRData, 'politicians.RData'))
nsMPs <- votes %>%
  filter(vote == 'no_show') %>%
  group_by(mandate.id) %>%
  dplyr::summarize(count = dplyr::n(), .groups = 'keep')
nsMPs <- nsMPs %>% mutate(missedVotes = count / nrow(motions))

nsMPs <- nsMPs %>%
  left_join(MPs, by = c('mandate.id' = 'id')) %>%
  left_join(politicians, by = c('politician.id' = 'id')) %>%
  select(
    c(
      'mandate.id',
      'missedVotes',
      'politician.label',
      'occupation',
      'party.label',
      'start_date'
      )
  )
nsMPs %>%
  ungroup() %>%
  select(-c('mandate.id')) %>%
  arrange(desc(missedVotes)) %>%
  head()

# FINDINGS So we should filter out votes from MPs who are no longer active
# As it turns out, that doesn't drop the share of no-show votes by much
inactiveMPs <- nsMPs %>%
  filter(is.na(politician.label)) %>%
  select('mandate.id')

# MPs already only shows those who are active - a recent change in the
# abgeordnetenwatch API
activeMPs <- MPs

activeVotes <- activeMPs %>% left_join(votes, by = c('id' = 'mandate.id'))
gPie <- plotPie(activeVotes, inner = 'vote', coloursInner = colVotes) +
  ggtitle("Voting behaviour") +
  theme(
    panel.background = element_rect(fill = "transparent", colour = NA),
    plot.background = element_rect(fill = "transparent", colour = NA)
  )
gPie
ggsave(
  plot = gPie,
  width = 7,
  height = 7,
  units = 'cm',
  dpi = 'print',
  path = folderImages,
  filename = 'No-show share of votes.jpg'
)

dictKeyStats['ShareNoShowActive'] = sum(activeVotes$vote == 'no_show')/nrow(activeVotes)

activeMPVoteShare <- activeMPs %>% left_join(votes, by = c('id' = 'mandate.id')) %>%
  filter(vote == 'no_show') %>%
  group_by(id) %>%
  dplyr::summarize(count = n(), .groups = 'keep')
activeMPVoteShare <- activeMPVoteShare %>%
  mutate(missedVotes = count / nrow(motions))

activeMPs <- activeMPs %>%
  left_join(activeMPVoteShare, by = c('id' = 'id')) %>%
  left_join(politicians, by = c('politician.id' = 'id')) %>%
  select(c(
    'id',
    'missedVotes',
    'politician.label',
    'occupation',
    'party.label'
  ))
topNSMPs <- activeMPs %>%
  ungroup() %>%
  select(-c('id')) %>%
  arrange(desc(missedVotes)) %>%
  head()
topNSMPs
save(topNSMPs, file = paste0(folderTemp, 'List of top NS MPs.RData'))

gNSHistogram <- activeMPs %>%
  ggplot(aes(x = round(missedVotes * 100, 0))) +
  geom_histogram(bins = 10) +
  xlab('No-show votes') +
  ylab('# of MPs') +
  ggtitle('Frequency of no-shows') +
  theme(
    panel.background = element_rect(fill = "transparent", colour = NA),
    plot.background = element_rect(fill = "transparent", colour = NA)
  )
gNSHistogram
ggsave(
  plot = gNSHistogram,
  width = 7,
  height = 7,
  units = 'cm',
  dpi = 'print',
  path = folderImages,
  filename = 'Frequency of no-show votes by MP.jpg'
)

# FINDINGS OK so now the top 'no shows' include (appropriately?) the Head of
# Government, Chancellor Angela Merkel, Dr. Helge Braun which a quick Google
# search identifies as Head of the Chancellery and Federal Minister for Special
# Affairs, and Peter Altmeier, Federal Minister for Economic Affairs and Energy.

# Let's focus on those MPs that have a share >= 5% as frequent no-shows
# QUESTION Who are these MPs?
dictKeyStats['cutoffMissedVotes'] = 0.05
activeMPs$isNS <- activeMPs$missedVotes >= dictKeyStats['cutoffMissedVotes']

nsMPs <- activeMPs %>%
  filter(isNS == TRUE)

nsMPs %>% ungroup() %>%
  select(c(
    'politician.label',
    'occupation',
    'party.label',
    'missedVotes'
  )) %>%
  arrange(desc(missedVotes))

nrow(nsMPs)
dictKeyStats['NumberTopNSMPs'] = nrow(nsMPs)


# FINDINGS We find that there is a relatively small number of frequent
# 'offenders', i.e. who have been absent without notice for 5% or more of all
# polls.
# Let's look at their characteristics in a bit more detail

nsMPs <- nsMPs %>% left_join(MPs[c(
  'id',
  'politician.id',
  'electoral_data.constituency.id',
  'electoral_data.mandate_won',
  'electoral_data.list_position',
  'electoral_data.constituency_result',
  'fraction_membership.fraction.id'
)],
by = c('id' = 'id')) %>%
  left_join(politicians[c('id',
                          'sex',
                          'age',
                          'answeredShare')],
            by = c('politician.id' = 'id'))

gNSMPsParties <- nsMPs %>%
  plotPie(inner = 'party.label', coloursInner = colParties) +
  ggtitle('No-show MPs by party affiliation') +
  theme(
    panel.background = element_rect(fill = "transparent", colour = NA),
    plot.background = element_rect(fill = "transparent", colour = NA)
  )
gNSMPsParties
ggsave(
  plot = gNSMPsParties,
  width = 7,
  height = 7,
  units = 'cm',
  dpi = 'print',
  path = folderImages,
  filename = 'Share of no-show votes by party.jpg'
)

# FINDINGS More than half of all no-show MPs are from fringe parties on either
# the left or the right of the political spectrum, or from MPs no longer
# affiliated with any party.


# QUESTION Where are they from?
load(paste0(folderRData, 'constituencies.RData'))
shpConstituencies <- st_read(paste0(folderTemp, 'Geometrie_Wahlkreise_19DBT_VG250.shp'))

nsConstituencies <- shpConstituencies %>%
  left_join(constituencies, by = c('WKR_NR' = 'number')) %>%
  left_join(nsMPs, by = c('id' = 'electoral_data.constituency.id')) %>%
  select(c('politician.label', 'geometry', 'isNS'))

nsConstituencies[is.na(nsConstituencies$isNS), 'isNS'] <- FALSE

gMap <- nsConstituencies %>% ggplot() +
  geom_sf(aes(fill = isNS), size = 0.1) +
  coord_sf(label_axes = '') +
  scale_fill_manual(
    'No-show MP constituency',
    values = colHighlight2,
    limits = c(TRUE),
    labels = c('No-show constituency')
  ) +
  ggtitle('MP constituencies') +
  theme(
    panel.background = element_rect(fill = "transparent", colour = NA),
    plot.background = element_rect(fill = "transparent", colour = NA),
    legend.position = 'bottom',
    legend.title = element_blank()
  )
gMap
ggsave(
  plot = gMap,
  width = 7,
  height = 7,
  units = 'cm',
  dpi = 'print',
  path = folderImages,
  filename = 'Constituency map.jpg'
)

# With thanks to https://data-se.netlify.app/2017/10/22/afd-map-foreigners/


# FINDINGS As it turns out, there does not seem to be much of a geographic pattern
# QUESTION What else can we learn about these MPs?

activeMPData <- activeMPs %>% left_join(MPs[c(
  'id',
  'politician.id',
  'electoral_data.constituency.id',
  'electoral_data.mandate_won',
  'electoral_data.list_position',
  'electoral_data.constituency_result',
  'fraction_membership.fraction.id'
)],
by = c('id' = 'id')) %>%
  left_join(politicians[c('id',
                          'sex',
                          'age',
                          'answeredShare')],
            by = c('politician.id' = 'id'))

activeMPData$isNSText <- ifelse(activeMPData$isNS, 'yes', 'no')

gNSMPsByListPosition <- activeMPData %>%
  filter(electoral_data.mandate_won == 'list') %>%
  ggplot(aes(y = electoral_data.list_position)) +
  geom_boxplot() +
  facet_grid(cols = vars(isNSText)) +
  ggtitle(label = element_blank(),
          subtitle = element_text('MPs elected via party lists')) +
  theme(
    legend.position = 'none',
    panel.background = element_rect(fill = "transparent", colour = NA),
    plot.background = element_rect(fill = "transparent", colour = NA)
  )

gNSMPsByConstituencyResult <- activeMPData %>%
  filter(electoral_data.mandate_won == 'constituency') %>%
  ggplot(aes(y = electoral_data.constituency_result)) +
  geom_boxplot() +
  facet_grid(cols = vars(isNSText)) +
  ggtitle(label = element_blank(),
          subtitle = element_text('MPs elected via constituencies')) +
  theme(
    legend.position = 'none',
    panel.background = element_rect(fill = "transparent", colour = NA),
    plot.background = element_rect(fill = "transparent", colour = NA)
  )

gNSMPsEngagement <- activeMPData %>%
  ggplot(aes(y = answeredShare)) +
  geom_boxplot() +
  facet_grid(cols = vars(isNSText)) +
  ggtitle(label = element_blank(),
          subtitle = element_text('Level of voter engagement')) +
  theme(
    legend.position = 'none',
    panel.background = element_rect(fill = "transparent", colour = NA),
    plot.background = element_rect(fill = "transparent", colour = NA)
  )

gPowerBase <- grid.arrange(
  gNSMPsByListPosition,
  gNSMPsByConstituencyResult,
  gNSMPsEngagement,
  ncol = 3,
  top = 'No-show MPs by strength of their power base in party & constituency and engagement with the public'
)
gPowerBase
ggsave(
  plot = gPowerBase,
  width = 21,
  height = 7,
  units = 'cm',
  dpi = 'print',
  path = folderImages,
  filename = 'Power base.jpg'
)

gNSMPsByAge <- activeMPData %>%
  filter(age > 0) %>%
  ggplot(aes(y = age)) +
  geom_boxplot() +
  facet_grid(cols = vars(isNSText)) +
  ggtitle(label = element_text('No-show MPs by age')) +
  xlab('MP is a frequent no-show') +
  theme(
    legend.position = 'none',
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    panel.background = element_rect(fill = "transparent", colour = NA),
    plot.background = element_rect(fill = "transparent", colour = NA)
  )
gNSMPsByAge
ggsave(
  plot = gNSMPsByAge,
  width = 7,
  height = 7,
  units = 'cm',
  dpi = 'print',
  path = folderImages,
  filename = 'Age.jpg'
)

tNSMPsBySex <- activeMPData %>%
  filter(sex != 'n') %>%
  group_by(sex, isNS) %>%
  dplyr::summarise(n = n()) %>%
  group_by(sex) %>%
  mutate(s = n / sum(n)) %>%
  select(-n)

tNSMPsBySex %>%
  pivot_wider(names_from = sex, values_from = s)

tNSMPsBySex$colnames <- paste0(tNSMPsBySex$sex, '-', tNSMPsBySex$isNS)
for (i in seq(1, nrow(tNSMPsBySex))) {
  dictKeyStats[tNSMPsBySex[['colnames']][i]] = tNSMPsBySex[['s']][i]
}


# FINDINGS There doesn't seem to be much of a difference in no-show MP's gender
# when compared against the more diligent MPs

# QUESTION what, if anything, can we learn about the polls that have the highest
# share of no-shows (i.e. topics, dates, # of votes on the day)?
# QUESTION Let's identify those polls that have the highest share of no-shows

skActiveVotes <- activeVotes %>%
  select(c('id', 'poll.id', 'vote'))

nsVotes <- skActiveVotes %>%
  group_by(poll.id, vote) %>%
  dplyr::summarise(n = n()) %>%
  ungroup() %>%
  group_by(poll.id) %>%
  mutate(N = sum(n),
         s = n / N) %>%
  filter(vote == 'no_show') %>%
  select(c('poll.id', 'n', 's'))

gNSVoteFreq <- nsVotes %>%
  ggplot(aes(x = s)) +
  geom_freqpoly(binwidth = 0.0002) +
  theme(
    panel.background = element_rect(fill = "transparent", colour = NA),
    plot.background = element_rect(fill = "transparent", colour = NA)
  )
gNSVoteFreq
ggsave(
  plot = gNSVoteFreq,
  width = 7,
  height = 7,
  units = 'cm',
  dpi = 'print',
  path = folderImages,
  filename = 'Frequency of no-show votes.jpg'
)

nsVotes %>%
  mutate(S = round(s, 2)) %>%
  group_by(S) %>%
  summarise(N = sum(n)) %>%
  arrange(desc(N)) %>%
  head(1) %>%
  pull(S)

# FINDINGS We find that ~8% of votes being no-show seems most common.  We will
# therefore now focus our analysis on polls where we have a higher proportion of
# no-shows

nsVoteIDs <- nsVotes %>%
  filter(s > 0.08) %>%
  select('poll.id')

load(file = paste0(folderRData, strDate, '-motions.RData'))
pollsData <- motions  %>%
  select(c(
    'id',
    'field_committees.label',
    'field_topics.label',
    'weekday'
  )) %>%
  unique()
pollsData$isNSPoll <- pollsData$id %in% nsVoteIDs$poll.id
pollsData$cShortName <- lCommitteeShortNames[pollsData$field_committees.label]
pollsData$tShortName <- lTopicsShortNames[pollsData$field_topics.label]

nCPollsData <- pollsData %>%
  group_by(cShortName) %>%
  dplyr::summarise(nC = n())
nTPollsData <- pollsData %>%
  group_by(tShortName) %>%
  dplyr::summarise(nT = n())
nDPollsData <- pollsData %>%
  select(c('id', 'weekday')) %>%
  unique() %>%
  group_by(weekday) %>%
  dplyr::summarise(nD = n())

votesData <- activeVotes %>%
  left_join(pollsData, by = c('poll.id' = 'id')) %>%
  select(c(
    'id',
    'poll.id',
    'weekday',
    'cShortName',
    'tShortName',
    'vote'
  ))

nDVotesData <- votesData %>%
  group_by(weekday, vote) %>%
  dplyr::summarise(nNSVotes = n())

nDVotesData <- nDVotesData %>%
  group_by(weekday) %>%
  mutate(nVotes = sum(nNSVotes)) %>%
  filter(vote == 'no_show') %>%
  mutate(sNSVotes = nNSVotes / nVotes)

pollsByDay <- nDPollsData %>%
  left_join(nDVotesData, by = c('weekday' = 'weekday')) %>%
  mutate(yNS = nD * sNSVotes, yOther = nD - yNS)

pollsByDay <- pollsByDay %>%
  pivot_longer(cols = c(yNS, yOther),
               names_to = 'vote2',
               values_to = 'n') %>%
  mutate(v = (vote2 == 'yNS')) %>%
  select(c('weekday', 'v', 'sNSVotes', 'n'))

gPollsWeekday <- pollsByDay %>%
  ggplot(aes(x = factor(weekday, levels = lWeekdays),
             y = n), color = 'white') +
  geom_bar(aes(fill = v), position = 'stack', stat = 'identity') +
  scale_fill_manual(
    values = colHighlight,
    limits = c(TRUE),
    labels = c('No-show votes')
  ) +
  geom_text(aes(y = n, label = ifelse(v, percent(sNSVotes), '')), vjust =
              -0.75) +
  ggtitle('Distribution of polls by weekday') +
  xlab('Weekday') +
  ylab('# of polls, share of votes that are no-show') +
  theme(
    legend.position = 'bottom',
    legend.title = element_blank(),
    panel.background = element_rect(fill = "transparent", colour = NA),
    plot.background = element_rect(fill = "transparent", colour = NA)
  )
gPollsWeekday
ggsave(
  plot = gPollsWeekday,
  width = 14,
  height = 7,
  units = 'cm',
  dpi = 'print',
  path = folderImages,
  filename = 'No-shows by weekday.jpg'
)

# FINDINGS The majority of polls is held on Thursdays and Fridays, i.e. at the
# end of a working week.  However, Fridays are also the weekday with the highest
# share of no-show votes - we hypothesise that this may in part be because
# Friday may be seen as the start of the weekend by some MPs.

nCVotesData <- votesData %>%
  group_by(cShortName, vote) %>%
  dplyr::summarise(nNSVotes = n())

nCVotesData <- nCVotesData %>%
  group_by(cShortName) %>%
  mutate(nVotes = sum(nNSVotes)) %>%
  filter(vote == 'no_show') %>%
  mutate(sNSVotes = nNSVotes / nVotes)

pollsByCommittee <- nCPollsData %>%
  left_join(nCVotesData, by = c('cShortName' = 'cShortName')) %>%
  mutate(yNS = nC * sNSVotes, yOther = nC - yNS)

pollsByCommittee <- pollsByCommittee %>%
  pivot_longer(cols = c(yNS, yOther),
               names_to = 'vote2',
               values_to = 'n') %>%
  mutate(v = (vote2 == 'yNS')) %>%
  select(c('cShortName', 'v', 'sNSVotes', 'n'))

lCLevels <- pollsByCommittee %>%
  arrange(n) %>%
  select(cShortName) %>%
  unique() %>%
  pull() %>%
  as.character()

gPollsCommittee <- pollsByCommittee %>%
  ggplot(aes(x = factor(cShortName, levels = lCLevels),
             y = n), color = 'white') +
  geom_bar(aes(fill = v), position = 'stack', stat = 'identity') +
  coord_flip() +
  scale_fill_manual(
    values = colHighlight,
    limits = c(TRUE),
    labels = c('No-show votes')
  ) +
  geom_text(
    aes(y = n, label = ifelse(v, percent(sNSVotes, accuracy = 1), '')),
    vjust = 'center',
    hjust = -0.25,
    angle = 0
  ) +
  ggtitle('Polls by committee') +
  xlab('Committee') +
  ylab('# of polls, share of no-show votes') +
  theme(
    axis.text.x = element_text(
      angle = 90,
      vjust = 0.5,
      hjust = 1
    ),
    legend.position = 'bottom',
    legend.title = element_blank(),
    panel.background = element_rect(fill = "transparent", colour = NA),
    plot.background = element_rect(fill = "transparent", colour = NA)
  )

# FINDINGS We also find that, polls concerned with rules of procedure have by
# far the hightest share of no-show votes

nTVotesData <- votesData %>%
  group_by(tShortName, vote) %>%
  dplyr::summarise(nNSVotes = n())

nTVotesData <- nTVotesData %>%
  group_by(tShortName) %>%
  mutate(nVotes = sum(nNSVotes)) %>%
  filter(vote == 'no_show') %>%
  mutate(sNSVotes = nNSVotes / nVotes)

pollsByTopic <- nTPollsData %>%
  left_join(nTVotesData, by = c('tShortName' = 'tShortName')) %>%
  mutate(yNS = nT * sNSVotes, yOther = nT - yNS)

pollsByTopic <- pollsByTopic %>%
  pivot_longer(cols = c(yNS, yOther),
               names_to = 'vote2',
               values_to = 'n') %>%
  mutate(v = (vote2 == 'yNS')) %>%
  select(c('tShortName', 'v', 'sNSVotes', 'n'))

lTLevels <- pollsByTopic %>%
  arrange(n) %>%
  select(tShortName) %>%
  unique() %>%
  pull() %>%
  as.character()

gPollsTopics <- pollsByTopic %>%
  ggplot(aes(x = factor(tShortName, levels = lTLevels),
             y = n), color = 'white') +
  geom_bar(aes(fill = v), position = 'stack', stat = 'identity') +
  coord_flip() +
  scale_fill_manual(
    values = colHighlight,
    limits = c(TRUE),
    labels = c('No-show votes')
  ) +
  geom_text(aes(y = n, label = ifelse(v, percent(sNSVotes, accuracy = 1), '')),
            vjust = 'center',
            hjust = -0.25) +
  ggtitle('Polls by topic') +
  xlab('Topics') +
  ylab('# of polls, share of no-show votes') +
  theme(
    axis.text.x = element_text(vjust = 0.5, hjust = 1),
    legend.position = 'bottom',
    legend.title = element_blank(),
    panel.background = element_rect(fill = "transparent", colour = NA),
    plot.background = element_rect(fill = "transparent", colour = NA)
  )

gCommitteesAndTopics <- grid.arrange(gPollsCommittee,
                                     gPollsTopics,
                                     ncol = 2,
                                     top = 'Polls by committee & topic')
ggsave(
  plot = gCommitteesAndTopics,
  width = 21,
  height = 18,
  units = 'cm',
  dpi = 'print',
  path = folderImages,
  filename = 'No-show committees and topics.jpg'
)


formattedPollsByCommittee <- pollsByCommittee %>%
  filter(v == TRUE) %>%
  mutate('Committee' = cShortName,
         'No-show share' = percent(sNSVotes, 0.1)) %>%
  arrange(desc(sNSVotes)) %>%
  select(c('Committee', 'No-show share'))

formattedPollsByCommittee %>%
  head(5)

percent(mean(pollsByCommittee$sNSVotes), 0.1)

formattedPollsByTopic <- pollsByTopic %>%
  filter(v == TRUE) %>%
  mutate('Topic' = tShortName, 'No-show share' = percent(sNSVotes, 0.1)) %>%
  arrange(desc(sNSVotes)) %>%
  select(c('Topic', 'No-show share'))

formattedPollsByTopic %>%
  head(5)

percent(mean(pollsByTopic$sNSVotes), 0.1)

# FINDINGS Interestingly, foreign affairs has the highest share of no-shows, 
# whereas gender equality and affairs relating to the 'new' federal states 
# (i.e. the former East Germany) have the highest rate of attendance

# ############################################################################
#
# With this, we conclude the exploratory analysis
#
# ############################################################################

save(dictKeyStats, file = paste0(folderTemp, 'dictKeyStats.RData'))
