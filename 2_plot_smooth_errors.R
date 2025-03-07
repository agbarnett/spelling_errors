# 2_plot_smooth_errors.R
# plot smooth trends in errors
# July 2024
library(stringr)
library(ggplot2)
library(dplyr)
# colour blind friendly
colours = c("grey22", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00" , 'grey88')

# smooth estimates from 1_model_errors.R
load('data/1_model_results.RData')
trends = filter(model_results, str_detect(pattern='fitted', rowname)) %>%
  mutate(year = years[time])

# averages for ordering from 1_table_errors.R
load('data/1_table.RData')

# sort trends by mean and create facet label
trends = left_join(trends, for_table, by='enum') %>%
  arrange(mean_rate, year) %>%
  mutate(facet = as.numeric(as.factor(mean_rate)))
labels = select(trends, error) %>% unique() %>% pull()
# carriage returns in two labels
labels = case_when(
  labels == 'randomised controlled trail' ~ 'randomised controlled\ntrail',
  labels == 'principle component analysis' ~ 'principle component\nanalysis',
  labels == 'Total' ~ 'Total across all errors',
  TRUE ~ as.character(labels)
)
trends = mutate(trends, facet = factor(facet, levels=1:length(labels), labels=labels))

# plot trends in rates
tplot = ggplot(data = trends, aes(x = year, y = mean, ymin=x95_ci_low, ymax=x95_ci_upp ))+
  geom_line(linewidth=1.05, col=colours[2])+ 
  geom_ribbon(alpha=0.2, col=colours[2])+
  theme_bw()+
  theme(panel.grid.minor = element_blank(),
        legend.position = 'none',
        strip.text = element_text(size=9),
        axis.text.x = element_text(size=8, angle=45, hjust=1))+
  facet_wrap(~facet, scales='free_y', labeller = label_value)+
  coord_cartesian(ylim=c(0,NA))+ # remove negative on y-axis
  xlab('Year')+
  ylab('Spelling errors per 10,000 abstracts')
tplot
ggsave(tplot, file = 'figures/smooth_trend_in_rates.jpg', dpi = 500, units='in', width=8, height=7)

