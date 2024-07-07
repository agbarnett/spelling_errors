# 0_search_pubmed.R
# search pubmed for spelling errors; only English language
# July 2024
library(rentrez)
library(dplyr)
library(binom)
source('0_my_pubmed_key_do_not_share.R')

# spelling errors
errors = list(
#  c("statistically significant"), # not a spelling error, but still interesting
#  c("statistically insignificant"), # not a spelling error, but still interesting
  c('randomised controlled trail','randomised controlled trails','randomized controlled trail','randomized controlled trails'),
  c('pubic health'), # could be some non errors here
  c('public heath'),
  c('screeing'),
  c('clinicans'),
  c('reserach','reserachers'), # much more common if affiliation is included; this search is not restricted to the title and abstract
  c('prevalance','prevalances'),
  c('casual inference','casual effect','casual association'), 
  c('principle component analysis','principle component analyses'),
  c('odd ratio'),
  c('odds ration','odds rations'),
  c('risk ration','risk rations'),
  c('confident interval','confident intervals',"confidence inteval","confidance interval"),
  c('statically significant'),
  c("fischer's exact","fischers exact","fischer exact"),
  c('kaplan meir','kapan meier'), # do not need hyphen as this is included
  c('guassian','gausian'))

# loop through errors and years
data = NULL
years = 1970:2023
for (enum in 1:length(errors)){
  these_errors = errors[[enum]]
  for (e in these_errors){ # loop through related errors
    place = ifelse(e %in% c('reserach','reserachers'), '[All fields]', '[tiab]')
    for (y in years){
      query = paste(e, place, ' AND ', y, '[pdat] AND English[Language]', sep ='')
      res = entrez_search(db = 'pubmed', term = query, api_key = my.api.key, retmax=20000 )
      if(res$count == 0){
        next # skip if no results
      }
      if(res$count >=20000){cat('overflow for year',y,', term', e, '\n', sep='')}
      frame = data.frame(year = y, enum = enum, error = these_errors[1], ids = res$ids) # keep all IDs with error
      data = bind_rows(data, frame)
      # spit out occasional query translation
      if(runif(1) < 0.01){cat(res$QueryTranslation, '\n')}
    }
  }
}

# remove duplicates using Pubmed ID (abstracts with multiple matches to spelling errors)
data = unique(data)
# then get frequencies, including over plurals (and other similar misspellings)
freqs = group_by(data, year, error, enum) %>%
  tally() %>%
  ungroup()

# add zero years/errors
full = expand.grid(year = years, enum = 1:length(errors))
freqs = full_join(freqs, full, by=c('year','enum')) %>%
  mutate(n = ifelse(is.na(n), 0, n)) 

# get the denominator per year
denom = NULL
for (y in years){
  query = paste(y, '[pdat] AND English[Language]', sep ='')
  res = entrez_search(db = 'pubmed', term = query, api_key = my.api.key)
  frame = data.frame(year = y, denom = res$count)
  denom = bind_rows(denom, frame)
}

# add denominator and calculate confidence interval
freqs = left_join(freqs, denom, by='year') %>%
  mutate(p = 10000*n / denom, # per 10,000 abstracts
         lower = binom.exact(n = denom, x = n)$lower,
         upper = binom.exact(n = denom, x = n)$upper,
         lower = lower*10000,
         upper = upper*10000)
#
freqs = arrange(freqs, enum, year)

# add overall total
total = group_by(freqs, year, denom) %>%
  summarise(n = sum(n)) %>%
  ungroup() %>%
  mutate(
    enum = 99,
    p = 10000*n / denom, # per 10,000 abstracts
       lower = binom.exact(n = denom, x = n)$lower,
       upper = binom.exact(n = denom, x = n)$upper,
       lower = lower*10000,
       upper = upper*10000)

# add back missing labels
labels = filter(data, !is.na(error)) %>%
  select(enum, error) %>%
  unique()
total_label = data.frame(enum = 99, error = 'Total')
labels = bind_rows(labels, total_label)
freqs = select(freqs, -error) %>%
  left_join(labels, by='enum')

# save
search_date = as.Date(Sys.Date())
save(freqs, search_date, errors, years, file = 'data/0_pubmed.RData')
