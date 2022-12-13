if(!require(tidyverse))
  install.packages("tidyverse")
if (!require(dplyr))
  install.packages("dplyr")
if (!require(rvest))
  install.packages("rvest")
if (!require(fuzzyjoin))
  install.packages("fuzzyjoin")


source('Settings.R')


# We clean every dataset we downloaded via the API as follows:
# 1. unpack and unnest all columns that contain a data.frame or a list
# 2. make sure that columns that contain dates are converted to Date format
# 3. make sure that all columns that contain some id number are formatted as 
# numeric
#
# We then also create a 'skeleton' stripped-down version of each dataset with 
# only the core linking columns. All of these are then stored as RData files for 
# faster future access

load(file = paste0(folderRData, 'MPs-RAW.RData'))
MPs <- MPs %>% 
  unpack(c(parliament_period, politician, electoral_data), names_sep = '.')
MPs <- MPs %>% 
  unpack(c(electoral_data.electoral_list, 
           electoral_data.constituency), names_sep = '.')
MPs <- MPs %>% 
  unnest('fraction_membership', names_sep = ".", keep_empty = TRUE)
MPs <- MPs %>% 
  unpack('fraction_membership.fraction', names_sep = ".")
MPs$start_date <- as.Date(MPs$start_date)
MPs <- MPs %>% 
  mutate_if((str_detect(names(.), "id$")), as.numeric)
save(MPs, file = paste0(folderRData, 'MPs.RData'))

skMPs <- MPs %>% select(
  id,
  label,
  politician.id,
  politician.label,
  electoral_data.id,
  electoral_data.electoral_list.id,
  electoral_data.constituency.id,
  fraction_membership.id,
  fraction_membership.fraction.id,
  start_date)
save(skMPs, file = paste0(folderRData, 'skMPs.RData'))
rm(list = c('MPs', 'skMPs'))

load(file = paste0(folderRData, 'mphistory-RAW.RData'))
MPhistory$start_date <- as.Date(MPhistory$start_date)
MPhistory <- MPhistory %>% 
  mutate_if((str_detect(names(.), "id$")), as.numeric)
save(MPhistory, file = paste0(folderRData, 'mphistory.RData'))

skMPhistory <- MPhistory %>% 
  select(
  -entity_type,-label,-api_url,
  -parliament_period.entity_type,-parliament_period.label,
  -parliament_period.api_url,-parliament_period.abgeordnetenwatch_url,
  -politician.entity_type,-politician.api_url,-politician.abgeordnetenwatch_url,
  -electoral_data.entity_type,-electoral_data.label,
  -electoral_data.electoral_list.entity_type,-electoral_data.electoral_list.label,
  -electoral_data.electoral_list.api_url,-electoral_data.constituency.entity_type,
  -electoral_data.constituency.label,-electoral_data.constituency.api_url,
  -electoral_data.constituency_result,
  -fraction_membership.entity_type,-fraction_membership.fraction.label,
  -fraction_membership.entity_type,
  fraction_membership.fraction.label,
  -fraction_membership.fraction.api_url,-fraction_membership.valid_from,
  -info
)
save(skMPhistory, file = paste0(folderRData, 'skMPhistory.RData'))
rm(list = c('MPhistory', 'skMPhistory'))

load(file = paste0(folderRData, 'politicians-RAW.RData'))
politicians <- politicians %>% 
  mutate_if((str_detect(names(.), "id$")), as.numeric)
politicians$year_of_birth <- as.numeric(politicians$year_of_birth)
politicians$statistic_questions <- as.numeric(politicians$statistic_questions)
politicians$statistic_questions_answered <- 
  as.numeric(politicians$statistic_questions_answered)
save(politicians, file = paste0(folderRData, 'politicians.RData'))

skPoliticians <- politicians %>% 
  select(id, label, sex, year_of_birth, education, party.id, party.label)
save(skPoliticians, file = paste0(folderRData, 'skPoliticians.RData'))
rm(list = c('politicians', 'skPoliticians'))

load(file = paste0(folderRData, 'parties-RAW.RData'))
parties$short_name <- ifelse(parties$label == 'Bündnis 90/Die Grünen',
                             'DIE GRÜNEN',
                             parties$label)
save(parties, file = paste0(folderRData, 'parties.RData'))
rm(list = c('parties'))

load(file = paste0(folderRData, 'fractions-RAW.RData'))
fractions <- fractions %>% 
  unpack('legislature', names_sep = '.')
fractions <- fractions %>% 
  mutate_if((str_detect(names(.), "id$")), as.numeric)
save(fractions, file = paste0(folderRData, 'fractions.RData'))

skFractions <- fractions %>% 
  select(id, label, short_name)
save(skFractions, file = paste0(folderRData, 'skFractions.RData'))
rm(list = c('fractions'))

load(file = paste0(folderRData, 'committees-RAW.RData'))
committees <- committees %>% 
  unpack('field_legislature', names_sep = '.')
committees <- committees %>% 
  unnest('field_topics',  names_sep = '.', keep_empty = TRUE)
committees <- committees %>% 
  mutate_if((str_detect(names(.), "id$")), as.numeric)
save(committees, file = paste0(folderRData, 'committees.RData'))

skCommittees <- committees %>% 
  select(id, label, field_topics.id)
save(skCommittees, file = paste0(folderRData, 'skCommittees.RData'))
rm(list = c('committees', 'skCommittees'))

load(file = paste0(folderRData, 'committeeMemberships-RAW.RData'))
committeeMemberships <- committeeMemberships %>% 
  unpack(c('committee', 'candidacy_mandate'), names_sep = '.')
committeeMemberships <- committeeMemberships %>% 
  mutate_if((str_detect(names(.), "id$")), as.numeric)
save(committeeMemberships, file = paste0(folderRData, 
                                         'committeeMemberships.RData'))

skCommitteeMemberships <- committeeMemberships %>% 
  select(id, committee.id, candidacy_mandate.id, committee_role)
save(skCommitteeMemberships, file = paste0(folderRData, 
                                           'skCommitteeMemberships.RData'))


# The name of the next file can be manually set to whatever snapshot of
# motions and votes we want to work with. By default we are using the current
# date, on the assumption that this script is run immediately following
# 01 GetData.R

load(file = paste0(folderRData, Sys.Date(), '-motions-RAW.RData'))
motions <- motions %>% 
  unnest('field_committees', names_sep = '.', keep_empty = TRUE)
motions <- motions %>% 
  unpack('field_legislature', names_sep = '.')
motions <- motions %>% 
  unnest('field_related_links', names_sep = '.', keep_empty = TRUE)
motions <- motions %>% 
  unnest('field_topics', names_sep = '.', keep_empty = TRUE)
motions$field_poll_date <- as.Date(motions$field_poll_date)
motions <- motions %>% 
  mutate_if((str_detect(names(.), "id$")), as.numeric)
save(motions, file = paste0(folderRData, Sys.Date(), '-motions.RData'))

skMotions <- motions %>% 
  select(id,
         field_committees.id,
         field_legislature.id,
         field_poll_date,
         field_topics.id) %>% 
  distinct()
save(skMotions, file = paste0(folderRData, Sys.Date(), '-skMotions.RData'))
rm(list = c('motions', 'skMotions'))

load(file = paste0(folderRData, 'topics-RAW.RData'))
topics <- topics %>% 
  select(-parent)
topics <- topics %>% 
  mutate_if((str_detect(names(.), "id$")), as.numeric)
save(topics, file = paste0(folderRData, 'topics.RData'))

skTopics <- topics %>% 
  select(id, label)
save(skTopics, file = paste0(folderRData, 'skTopics.RData'))
rm(list = c('topics', 'skTopics'))

load(file = paste0(folderRData, 'votes-RAW.RData'))
votes <- votes %>% 
  unpack('poll', names_sep = '.')
votes <- votes %>% 
  unpack('mandate', names_sep = '.')
votes$vote <- factor(votes$vote, levels = c("yes", "no", "abstain", "no_show"))
votes <- votes %>% 
  mutate_if((str_detect(names(.), "id$")), as.numeric)
save(votes, file = paste0(folderRData, 'votes.RData'))

skVotes <- votes %>% 
  select(id, mandate.id, poll.id, vote)
save(skVotes, file = paste0(folderRData, 'skVotes.RData'))
rm(list = c('votes', 'skVotes'))

load(file = paste0(folderRData, 'Periods-RAW.RData'))
Periods$election_date <- as.Date(Periods$election_date)
Periods$start_date_period <- as.Date(Periods$start_date_period)
Periods$end_date_period <- as.Date(Periods$end_date_period)
Periods <- Periods %>% 
  mutate_if((str_detect(names(.), "id$")), as.numeric)
save(Periods, file = paste0(folderRData, 'Periods.RData'))
rm(list = c('Periods'))
