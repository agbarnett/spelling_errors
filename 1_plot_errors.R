# 1_plot_errors.R
# simple plots of the results over time
# July 2024
library(ggplot2)
library(dplyr)
# colour blind friendly
colours = c("grey22", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00" , 'grey88')

# data from 0_search_pubmed.R
load('data/0_pubmed.RData')
# add breaks to labels
freqs = mutate(freqs,
              error = case_when(
                error == 'principle component analysis' ~ 'principle component\nanalysis',
                error == 'randomised controlled trail' ~ 'randomised controlled\ntrail',
                TRUE ~ as.character(error)
              ))

# raw numbers
nplot = ggplot(data = freqs, aes(x = year, y = n))+
  geom_line(linewidth=1.05)+
  theme_bw()+
  theme(panel.grid.minor = element_blank(),
        legend.position = 'none',
        strip.text = element_text(size=8),
        axis.text.x = element_text(size=8, angle=45, hjust=1))+
  facet_wrap(~error, scales='free_y')+
  xlab('Year')+
  ylab('Number of abstracts')
nplot
ggsave(nplot, file = 'figures/numbers.jpg', dpi = 500, units='in', width=7, height=6)

# rates (not smoothed)
rplot = ggplot(data = freqs, aes(x = year, y = p, ymin=lower, ymax=upper))+
  geom_line(linewidth=1.05, col = colours[2])+
  geom_ribbon(alpha=0.2, col = colours[2])+
  theme_bw()+
  theme(panel.grid.minor = element_blank(),
        legend.position = 'none',
        strip.text = element_text(size=8),
        axis.text.x = element_text(size=8, angle=45, hjust=1))+
  facet_wrap(~error, scales='free_y')+
  xlab('Year')+
  ylab('Spelling errors per 10,000 abstracts')
rplot
ggsave(rplot, file = 'figures/rates.jpg', dpi = 500, units='in', width=7, height=7)


