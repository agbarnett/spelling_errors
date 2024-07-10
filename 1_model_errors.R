# 1_model_errors.R
# Bayesian model of spelling errors over time
# July 2024
library(dplyr)
library(splines)
library(nimble) # use Bayes to model smooth rates
library(janitor)
library(stringr)
library(ggplot2) 
source('99_functions.R') # for plot_convergence

# data from 0_search_pubmed.R
load('data/0_pubmed.RData')

# make spline basis from years, needed to increase df for two errors
basis2 = ns(years, df = 2, intercept=TRUE)
basis3 = ns(years, df = 3, intercept=TRUE)
basis4 = ns(years, df = 4, intercept=TRUE)

# Poisson model with offset
code <- nimbleCode({
  ## Likelihood
  for (i in 1:N){ # loop through years
    counts[i] ~ dpois(mu[i])
    log(mu[i]) <- log(denom[i]/10000) + inprod(basis[i,1:df],alpha[1:df])
    log(fitted[i]) <- inprod(basis[i,1:df],alpha[1:df]) # fitted for denominator of 10,000 abstracts
  }
  for (j in 1:df){alpha[j] ~ dnorm(0, sd = 10^5)} # vague priors
})

# loop through errors
model_results = NULL
error_numbers = unique(freqs$enum)
n_errors = length(error_numbers)
for (this_error in error_numbers){
  # select degrees of freedom; some models showed relatively poor chain mixing
  basis = basis3
  if(this_error %in% c(11)){basis = basis2} # for confident interval
  if(this_error %in% c(8)){basis = basis4} # for PCA
  # set up data
  for_bayes = filter(freqs, enum == this_error) %>% # select data for this error
    arrange(year)
  N = nrow(for_bayes)
  basis = basis[1:N,] # just get matrix
  df = ncol(basis) # degrees of freedom
  constants = list(df = df, 
                   N = N,
                   denom = for_bayes$denom,
                   basis = basis)
  dependent = list(counts = for_bayes$n)

  # initial values and parameters to monitor  
  inits = list(alpha=rep(0,df))
  parms = c('alpha','fitted')
  
  # start the model
  model = nimbleModel(code, 
                      data = dependent,
                      inits = inits,
                      constants = constants)
  
  # get the MCMC parameters
  source('99_mcmc.R')
  
  # run model
  mcmc =  nimbleMCMC(model = model,
                     inits = inits,
                     monitors = parms,
                     niter = MCMC*2*thin, # times 2 for burn-in 
                     thin = thin,
                     nchains = n.chains, 
                     nburnin = MCMC,
                     summary = TRUE, 
                     setSeed = seed,
                     WAIC = FALSE)
  
  # check convergence and export plots
  plot_convergence(insamples = mcmc$samples, inerror = this_error)
  
  # extract the trend
  to_plot = data.frame(mcmc$summary$all.chains) %>%
    tibble::rownames_to_column() %>%
    clean_names() %>%
    mutate(enum = this_error,
           time = as.numeric(str_remove_all(rowname, '[^0-9]')))
  # concatenate results
  model_results = bind_rows(model_results, to_plot)
} # end of loop

# save
save(model_results, years, file='data/1_model_results.RData')


