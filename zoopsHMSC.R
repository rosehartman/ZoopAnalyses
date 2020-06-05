#Joint species distribution models with HMSC

library(Hmsc)
library(tidyverse)
library(MASS)

#set up the model
m = Hmsc(Y = as.matrix(zoopmonCom1), XData = as.data.frame(zoopmonenv), 
         XFormula = ~region+season)

#Now do MCMC sampling on it to estimate model parameters
  
  mm = sampleMcmc(m, thin = 10, samples = 1000, transient = 5000,
                 nChains = 2)

  
#Get rid of NAs in the environmental matrix
  summary(envmat)
  envmat2 = dplyr::select(envmat, SampleID, month, Volume, 
                          Year, Date, SalSurf, Latitude, Longitude, 
                          Tide, Station, Secchi, Temperature, season, region)
  allzoopCom = allzoopCom[complete.cases(envmat2),]
  envmat2 = envmat2[complete.cases(envmat2),]
  
  #set up the model
  m2 = Hmsc(Y = as.matrix(allzoopCom), XData = as.data.frame(envmat2), 
           XFormula = ~region+season)
  
  #Now do MCMC sampling on it to estimate model parameters
  
  mm = sampleMcmc(m, thin = 10, samples = 1000, transient = 5000,
                  nChains = 2)
  mm2 = sampleMcmc(m2, thin = 10, samples = 1000, transient = 5000,
                  nChains = 2)
  
  #Check MCMC convergence diagnostics
  mpost = convertToCodaObject(mm2)
  effectiveSize(mpost$Beta)
  
  gelman.diag(mpost$Beta, multivariate=FALSE)$psrf
  
  #Look at effective sample size graphically
  par(mfrow=c(1,2))
  hist(effectiveSize(mpost$Beta), main="ess(beta)")
  hist(gelman.diag(mpost$Beta, multivariate=FALSE)$psrf, main="psrf(beta)")
  
  #To assess the model’s explanatory power, we apply the evaluateModelFit 
  #function to the posterior predictive
  #distribution simulated by the function computePredictedValues.
  preds = computePredictedValues(mm)
  evaluateModelFit(hM = mm, predY = preds)
  #
  
  
# We next evaluate the model’s predictive power through two-fold cross validation.
  partition = createPartition(mm, nfolds = 2)
  preds = computePredictedValues(mm, partition = partition)

  evaluateModelFit(hM = mm, predY = preds)
  
  #Let us now look at the estimates of the β parameters. We may do so visually by applying the plotBeta
  #function.
  postBeta = getPostEstimate(mm2, parName = "Beta")
  plotBeta(mm2, post = postBeta, param = "Support", supportLevel = 0.95)

  #Estimating species-to-species associations

  studyDesign = data.frame(sample = as.factor(1:n))
  rL = HmscRandomLevel(units = studyDesign$sample)
  m = Hmsc(Y = Y, XData = XData, XFormula = ~x1+x2,
           studyDesign = studyDesign, ranLevels = list(sample = rL))
  m = sampleMcmc(m, thin = thin, samples = samples, transient = transient,
                 nChains = nChains, verbose = verbose)  