# 1_model_errors.R
# use a Bayesian model of spelling errors
# July 2024
library(splines)
library(nimble) # use Bayes to model smooth estimates
library(janitor)
library(stringr)

# data from 0_search_pubmed.R
load('data/0_pubmed.RData')



# make spline basis
df = 3
basis = ns(years, df = df, intercept=TRUE)

# Poisson model with offset
code <- nimbleCode({
  ## Likelihood
  for (i in 1:N){ # loop through years
    counts[i] ~ dpois(mu[i])
    log(mu[i]) <- log(denom[i]/10000) + inprod(basis[i,1:df],alpha[1:df])
    log(fitted[i]) <- inprod(basis[i,1:df],alpha[1:df]) # fitted for denominator of 10,000
  }
  for (j in 1:df){alpha[j] ~ dnorm(0, sd = 10^5)} # vague priors
})

# loop through errors
model_results = NULL
for (this_error in 1:16){
  # set up data
  for_bayes = filter(freqs, enum == this_error) %>%
    arrange(year)
  N = nrow(for_bayes)
  constants = list(df = df, 
                   N = N,
                   denom = for_bayes$denom,
                   basis = basis[1:N,])
  dependent = list(counts = for_bayes$n)

  # initial values and parameters to monitor  
  inits = list(alpha=rep(0,3))
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


