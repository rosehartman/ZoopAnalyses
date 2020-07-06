#Joint species distribution models with HMSC

library(Hmsc)
library(tidyverse)
library(MASS)
library(corrplot)

allzoopCom = read.csv("allzoopCom.csv")
envmat2 = read.csv("envmat2.csv")

#try a lognormal distribution
#get trait matrix
#get phylogenies
#get rid of sampleID random effect

#Get rid of NAs in the environmental matrix
#  summary(envmat)
#  envmat2 = dplyr::select(envmat, SampleID, month, Volume, 
#                          Year, Date, SalSurf, Latitude, Longitude, 
#                          Tide, Station, Secchi, Temperature, season, region)
#  allzoopCom = allzoopCom[complete.cases(envmat2),]
#  envmat2 = envmat2[complete.cases(envmat2),]
  
  #set up the model
  m2 = Hmsc(Y = as.matrix(allzoopCom), XData = as.data.frame(envmat2), 
           XFormula = ~region+season + SalSurf + Temperature)
  
  #Now do MCMC sampling on it to estimate model parameters
  
  mm2 = sampleMcmc(m2, thin = 10, samples = 1000, transient = 5000,
                  nChains = 2)
  
  #Check MCMC convergence diagnostics
  mpost = convertToCodaObject(mm2)
  diags = data.frame(effectiveSize(mpost$Beta), 
                     gelman.diag(mpost$Beta, multivariate=TRUE)$psrf)
  
  
  #We are looking for high effective sample size and
  #scale=reduction factors (gelman.diag) close to 1
  
  #Look at effective sample size graphically
  par(mfrow=c(1,2))
  hist(effectiveSize(mpost$Beta), main="ess(beta)")
  hist(gelman.diag(mpost$Beta, multivariate=TRUE)$psrf, main="psrf(beta)")
  
  #there are a few high scale-reduction factors, but otherwise pretty OK.
  #Tortanus and decapoda are the only real problem children.
  
  #To assess the model’s explanatory power, we apply the evaluateModelFit 
  #function to the posterior predictive
  #distribution simulated by the function computePredictedValues.
  preds = computePredictedValues(mm2, nParallel = 2)
  fit = evaluateModelFit(hM = mm2, predY = preds)
  #
  
  
# We next evaluate the model’s predictive power through two-fold cross validation.
  partition = createPartition(mm2, nfolds = 2)
  predscross = computePredictedValues(mm2, partition = partition)

  evaluateModelFit(hM = mm2, predY = predscross)
  
  #Let us now look at the estimates of the β parameters. We may do so visually by applying the plotBeta
  #function.
  postBeta = getPostEstimate(mm2, parName = "Beta")
  plotBeta(mm2, post = postBeta, param = "Support", supportLevel = 0.50)
  plotBeta(mm2, post = postBeta, param = "Mean", supportLevel = 0.9)
  

  #Estimating species-to-species associations
  envmat2 = mutate(envmat2, SampleID = as.factor(SampleID), Station = as.factor(Station))
studydesign = data.frame(sample = as.factor(envmat2$SampleID), 
                         plot = as.factor(envmat2$Station))
  rL = HmscRandomLevel(units = envmat2$SampleID)
  rL2 = HmscRandomLevel(units = envmat2$Station)
  m = Hmsc(Y = as.matrix(allzoopCom), XData = as.data.frame(envmat2),
           XFormula =  ~SalSurf + Temperature + region + season,
           studyDesign = studydesign, ranLevels = list("plot" = rL2), distr = "lognormal poisson")
  
  m = sampleMcmc(m, thin = 10, samples = 100, transient = 500,
                 nChains = 2)  
  
  #In a model with random effects, it is important to look at the convergence diagnostics not only for the
  #parameters, but also for the 
  #parameters. The matrix 
  #is the matrix of species-to-species residual
  #covariances.
  
  mpost2 = convertToCodaObject(m)
  
  par(mfrow=c(2,2))
  
  hist(effectiveSize(mpost2$Beta), main="ess(beta)")
  hist(gelman.diag(mpost2$Beta, multivariate=TRUE)$psrf, main="psrf(beta)")
  hist(effectiveSize(mpost2$Omega[[1]]), main="ess(omega)")
  hist(gelman.diag(mpost2$Omega[[1]], multivariate=TRUE)$psrf, main="psrf(omega)")
  
  postBeta2 = getPostEstimate(m, parName="Beta")
  plotBeta(m, post=postBeta2, param="Support", supportLevel = 0.5)
#mostly pretty similar to without the random effet, but a few diffferences
  
  
#In addition to the  parameters related to the fixed effects, we can now look at the estimated species-tospecies
#  associations. We extract them from the model object with the computeAssociations function, which
#  also converts the covariances to the more convenient scale of correlation (ranging from -1 to +1). In the
#  script below, we choose to plot only those associations for which the posterior probability for being negative
# or positive is at least 0.95. There is no specific function for plotting species-to-species associations in HMSC,
#  but such plots can be generated straightforwardly with the corrplot function.
  par(mfrow=c(1,1))
  OmegaCor = computeAssociations(m)
  supportLevel = 0.5
  toPlot = ((OmegaCor[[1]]$support>supportLevel)
            + (OmegaCor[[1]]$support<(1-supportLevel))>0)*OmegaCor[[1]]$mean
  corrplot(toPlot, method = "color",
           col = colorRampPalette(c("blue","white","red"))(200),
           title = paste("random effect level:", m$rLNames[1]), mar=c(0,0,1,0))

  