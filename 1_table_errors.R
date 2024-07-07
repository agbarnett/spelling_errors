# 1_table_errors.R
# make table of errors
# June 2024
library(dplyr)
library(flextable)

# data from 0_search_pubmed.R
load('data/0_pubmed.RData')

# get the average
table = group_by(freqs, error, enum) %>%
  summarise(mean_rate = mean(p)) %>% # as a rate per 10,000
  ungroup()  

# add longer names
error_table = NULL
for (k in 1:length(errors)){
  f = data.frame(enum = k, errors = paste(errors[[k]], collapse=', '))
  error_table = bind_rows(error_table, f)
}
for_table = left_join(error_table, table, by='enum') %>%
  arrange(desc(mean_rate)) 

ftab = select(for_table, -enum, -error) %>%
  flextable() %>%
  width(j=1, unit='cm', width = 13.5) %>%
  width(j=2, unit='cm', width = 2) %>%
  theme_box() %>%
  colformat_double(j=2, digits=2)

# export order for trend plot
save(for_table, file = 'data/1_table.RData')

# export table for Word
save_as_docx(ftab, path = 'tables/rates.docx', align = "center")

