# 99_functions.R
# functions for spelling errors
# July 2024

#### function to plot convergence ###
plot_convergence = function(insamples, inerror, model){
  
  # colour blind friendly
  colours = c("grey22", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00" , 'grey88')
  
  # combine two chains
  chain1 = data.frame(insamples$chain1) %>% mutate(s = 1:n())
  chain2 = data.frame(insamples$chain2) %>% mutate(s = 1:n())
  chains = bind_rows(chain1, chain2, .id = 'chain') %>%
    clean_names()
  
   # switch to long
  long = reshape2::melt(chains, id=c('s','chain')) %>%
    filter(str_detect(variable, 'alpha')) # not the fitted values
  
  # line plot
  cplot = ggplot(data=long, aes(x=s, y=value, col=factor(chain)))+
    geom_line(lty=2)+
    theme_bw()+
    theme(panel.grid.minor = element_blank())+
    xlab('Iteration')+
    ylab('Estimate')+
    scale_color_manual('Chain', values=colours[2:3])+
    facet_wrap(~variable, scales = 'free_y')
  
  # overlapping density plot
  hplot = ggplot(data=long, aes(x=value, fill=factor(chain)))+
    geom_density(alpha=0.5)+
    theme_bw()+
    theme(panel.grid.minor = element_blank())+
    xlab('Estimate')+
    scale_fill_manual('Chain', values=colours[2:3])+
    facet_wrap(~variable, scales = 'free')
  
  ## save plots
  #
  outfile = paste('figures/mcmc/MCMC_line_', inerror, '.jpg', sep='')
  jpeg(outfile, width=5, height=4.5, units='in', res=400, quality = 100)
  print(cplot)
  invisible(dev.off())
  #
  outfile = paste('figures/mcmc/MCMC_density_', inerror, '.jpg', sep='')
  jpeg(outfile, width=5, height=4.5, units='in', res=400, quality = 100)
  print(hplot)
  invisible(dev.off())
  
}