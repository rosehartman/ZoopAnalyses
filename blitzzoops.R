#now maybe look at the blitz zooplankton



library(zooper)
library(lubridate)
library(tidyverse)
library(readxl)
library(visreg)
library(vegan)
library(RColorBrewer)

#add FRP data
zoopsfrp <- read_excel("zoopsexportqry.xlsx", 
                       col_types = c("text", "date", "text", 
                                     "text", "numeric", "text", "numeric", 
                                     "numeric", "numeric", "numeric", 
                                     "numeric", "numeric", "numeric", 
                                     "numeric", "numeric", "numeric", 
                                     "text", "text", "numeric", "numeric", 
                                     "text", "numeric"))

#combine the slides
totalcells = group_by(zoopsfrp, SampleID) %>% 
  summarise(totcells = max(CellNumber), totCountall = sum(Count))

zoo = group_by(zoopsfrp, Station, Date, SampleID, Dilution, CommonName,
               Volume) %>% 
  summarise(totCount = sum(Count))

zoo = merge(zoo, totalcells)

#adjust for subsampling
zoo$atotal = (zoo$totCount/zoo$totcells)*zoo$Dilution
#Calculate CPUE (total critters divided by volume sampled)
zoo$CPUE = zoo$atotal/zoo$Volume

#import analysis categories to make it easier to deal with
taxa <- read.csv("zoocodes.csv")
taxa = mutate(taxa, Lifestage = LifeStage) %>%
  dplyr::select(CommonName, Lifestage, Analy, Analy2, CN)
#attach the other categories to the main data set
zoop = merge(zoo, taxa)

#the 2018 and 2019 data
library(readr)
zoop_FRP2 <- read_csv("zoop_line2270.csv", 
                      col_types = cols(Date = col_date(format = "%Y-%m-%d")))

zoop_FRP2x <-  dplyr::select(zoop_FRP2, CommonName, SampleID, Station, Date, 
                             Dilution, Volume, totCount, totcells, totCountall, atotal,
                             CPUE, LifeStage, Analy, Analy2, CN) %>%
  rename(Lifestage = LifeStage)

#Make sure we don't have duplicates with the other FRP data set

zoop2y = filter(zoop_FRP2x, !SampleID %in% unique(zoop$SampleID))



FRPzoop = rbind(zoop2y, zoop)

save(FRPzoop, file = "FRPzoop.RData")
  
#add the the stations

stas = read.csv("stations.csv")

stations = dplyr::select(stas, Site, Site2, FRPStation, 
                         Ggdist, SiteType, Region) %>%
  rename(Station = FRPStation) %>%
  mutate(Region = factor(Region, levels = c("Suisun Marsh", "Suisun Bay", "Confluence",
                                            "Sac SanJ", "Cache")))



zoop2 = left_join(stations, FRPzoop) %>%
  dplyr::select(Site2, SiteType, Region, Station, Ggdist, SampleID, Date, CPUE, 
         Lifestage, CommonName, Analy, Analy2)



################ALL THE ZOOPS!!!###################
blitzoops = zoop2 %>%
  mutate(Month = month(Date), Year = year(Date)) %>%
  filter(Month %in% c(3,4,5), Year %in% c(2017, 2018, 2019))

#Decker and Flyway became tidal in 2019
#but we only have flyway data from 2019 anyway.
deck = dplyr::filter(blitzoops, Site2 == "Decker")
blitzoops$SiteType[which(blitzoops$Site2 == "Decker" & blitzoops$Year >2018)] = "Tidal" 


#############################################################
#my favorite stacked bar plot


#First calculate the average CPUE of each critter (analysis group/life stage) by location and year
blitzzoops = mutate(blitzoops, Year = year(Date), 
                  Month = month(Date), analyLS = paste(Analy, Lifestage))
#Put the rare ones into an "other" category

sumz = group_by(blitzzoops, analyLS) %>%
  summarize(tot = sum(CPUE), percent = tot/sum(blitzzoops$CPUE, na.rm = T)) %>%
  mutate(AnalyLS = analyLS)

sumz$AnalyLS[which(sumz$percent < 0.00015)] = "other"

blitzzoops = merge(blitzzoops, dplyr::select(sumz, AnalyLS, analyLS)) %>%
  filter(AnalyLS != " ")

blitzzoops$AnalyLS[which(blitzzoops$AnalyLS == "other larvae")] = "other"
blitzzoops$AnalyLS[which(blitzzoops$AnalyLS == "Diptera larvae")] = "Insecta"
blitzzoops$AnalyLS[which(blitzzoops$AnalyLS == "Hemiptera adult")] = "Insecta"
blitzzoops$AnalyLS[which(blitzzoops$AnalyLS == "Gammariae adult")] = "Amphipoda adult"


save(blitzzoops, file = "blitzzoops.RData")

zoop20x= group_by(blitzzoops, SampleID, AnalyLS, Analy, Lifestage, Site2, SiteType, Region, Year, Ggdist) %>% 
  summarize(CPUE = sum(CPUE, na.rm = T))
zooLitave = group_by(zoop20x, AnalyLS, Analy, Lifestage, Site2, SiteType, Region, Year) %>% 
  summarize(CPUE = mean(CPUE, na.rm = T), ggdist = mean(Ggdist))
zooLitave =droplevels(zooLitave)

#set up labels
zoolabs = c("Amphipoda", "Annelida", "Barnacle Nauplii", "Calanoida",
            "Cal juv", "Cal nauplii", 
            "Cladocera","Cnidaria","Collembola", "Cyclopoida",
             "Cyclopoid juv", "Gammarid", "Harpacticoida", "Insecta", "Nematoda",
            "Ostracoda","Rotifera")

mypal = c(brewer.pal(12, "Set3"), brewer.pal(8, "Dark2"))

#Now a bar plot
z1 = ggplot(zooLitave, aes(x=SiteType, y= CPUE))
z1 + geom_bar(stat = "identity", aes(fill = AnalyLS), position = "fill") + 
  scale_fill_manual(values = c(mypal, "white", "green"), name = NULL) + 
  facet_grid(.~Region, scales = "free_x", space = "free") +
  #  coord_cartesian(ylim = c(0, 16000)) +
  xlab("Site") + ylab("Percent composition")+
  theme(legend.position = "right", axis.text.x = element_text(angle = 90))

z1 + geom_bar(stat = "identity", aes(fill = AnalyLS)) + 
  scale_fill_manual(values = c(mypal, "white", "green"), name = NULL) + 
  facet_grid(.~SiteType+ Region, scales = "free_x", space = "free") +
  #  coord_cartesian(ylim = c(0, 16000)) +
  xlab("Site") + ylab("Percent composition")+
  theme(legend.position = "right", axis.text.x = element_text(angle = 90))

z1 + geom_bar(stat = "identity", aes(fill = AnalyLS)) + 
  scale_fill_manual(values = c(mypal, "white", "green"), name = NULL) + 
  facet_wrap(~Year + SiteType, scales = "free_x") +
  #  coord_cartesian(ylim = c(0, 16000)) +
  xlab("Site") + ylab("mean CPUE")+
  theme(legend.position = "right", axis.text.x = element_text(angle = 90))

z2 = ggplot(zooLitave, aes(x=Year, y= CPUE))
z2 + geom_bar(stat = "identity", aes(fill = AnalyLS), position = "fill") + 
  scale_fill_manual(values = c(mypal, "white", "green"), name = NULL) + 
  facet_wrap(~Site2 + SiteType, scales = "free_x") +
  #  coord_cartesian(ylim = c(0, 16000)) +
  xlab("Site") + ylab("Percent composition")+
  theme(legend.position = "right")

#########################################################################
###########################################################################################
#stats
#Overall CPUE GLM


#First calculate total zoop CPUE per sample
zoosum = group_by(blitzzoops, SampleID, Site2, Region, SiteType, Year) %>% 
  summarize(CPUE = sum(CPUE, na.rm = T), ggdist = mean(Ggdist)) %>%
  mutate(Year2 = as.factor(Year))

#Model log-transformed catch against time (month) and station
g1s = lm(log(CPUE)~SiteType + Region + Year2, data = zoosum)
summary(g1s)
#Catch increases over time, FRP has lower catch, slightly higher catch upstream

#check out the diagnostic plots
plot(g1s)
#looks good!

#You can look at the partial residuals plots
visreg(g1s)

##############################################################################
#multivariate stats

#Create a community matrix
blitzmat = pivot_wider(blitzzoops, names_from = AnalyLS, values_from = CPUE, 
                     id_cols = c(SampleID, Site2, Station, Region, SiteType, Date, Year, Ggdist),
                     values_fill = 0, values_fn = sum)
Envmat = blitzmat[,1:8]
test = blitzmat[,9:27]
test2 = test[,order(names(test))]

Blitzmat2 = as.matrix(test2)

#relative abundance amtrix
Blitzmatp = Blitzmat2/rowSums(Blitzmat2)

#make better names for adding to the NMDS plots
names(Blitzmat2) =  c("Amphipoda", "Annelida", "Arachnida", 
                    "Barnacle naup", "Calanoida","Cal juv", 
                    "Cal naup", "Cladocera", "Cnidaria",
                    "Collembola", "Cyclopoida", "Cyclo juv",
                    "Gammaridae", "Harpacticoida", "Insecta",
                    "Nematoda", "Ostracoda", "Other", "Rotifera")

names(Blitzmatp) = names(Blitzmat2)


#PerMANOVA of abundance matrix survey and month
adonis(Blitzmat2 ~ SiteType + Region + Site2 + Year, data = Envmat)


#Do it again with relative abundance
a1 = adonis(Blitzmatp ~ SiteType + Region + Site2 + Year, data = Envmat)
a1
#Site is the biggest predicotr, year hardly at all

#now some non=metric multidimentional scaling
n1 = metaMDS(Blitzmat2, trymax = 300)
n2 = metaMDS(Blitzmatp, trymax = 300)
#Nope, not working.
Envmat$Region = as.factor(Envmat$Region)
Envmat$Site2 = as.factor(Envmat$Site2)
Envmat$SiteType = as.factor(Envmat$SiteType)
Envmat$Year = as.factor(Envmat$Year)

################################################################3
#Try just a subset
cachemat = filter(blitzzoops, Region == "Cache") %>%
  pivot_wider(names_from = AnalyLS, values_from = CPUE, 
                       id_cols = c(SampleID, Site2, Station, Region, SiteType, Date, Year, Ggdist),
                       values_fill = 0, values_fn = sum)
Envmat = cachemat[,1:8]
test = cachemat[,9:26]
test2 = test[,order(names(test))]

cachemat2 = as.matrix(test2)

#make better names for adding to the NMDS plots
names(cachemat2) =  c("Amphipoda", "Annelida", "Arachnida", 
                      "Calanoida","Cal juv", 
                      "Cal naup", "Cladocera", "Cnidaria",
                      "Collembola", "Cyclopoida", "Cyclo juv",
                      "Gammaridae", "Harpacticoida", "Insecta",
                      "Nematoda", "Ostracoda", "Other", "Rotifera")


#relative abundance amtrix
cachematp = cachemat2/rowSums(cachemat2)


#now some non=metric multidimentional scaling
n1 = metaMDS(cachemat2, trymax = 300)
n2 = metaMDS(cachematp, trymax = 300)
#Yes!
Envmat$Region = as.factor(Envmat$Region)
Envmat$Site2 = as.factor(Envmat$Site2)
Envmat$SiteType = as.factor(Envmat$SiteType)
Envmat$Year = as.factor(Envmat$Year)



#Show the NMDS plots
source("plotNMDS.R")

#do a quick plot with hulls by location
PlotNMDS(n2, data = Envmat, group = "SiteType")
#Huh. Maybe this is because we don't have a ton of samples from inside Lookout?
PlotNMDS(n2, data = Envmat, group = "Year")
#Huh again.
PlotNMDS(n2, data = Envmat, group = "Site2")
#Huh again.

################################################################3
#Now let's try Suisun
susmat = filter(blitzzoops, Region == "Suisun Marsh") %>%
  pivot_wider(names_from = AnalyLS, values_from = CPUE, 
              id_cols = c(SampleID, Site2, Station, Region, SiteType, Date, Year, Ggdist),
              values_fill = 0, values_fn = sum)
Envmat = susmat[,1:8]
test = susmat[,9:27]
test2 = test[,order(names(test))]

susmat2 = as.matrix(test2)

#make better names for adding to the NMDS plots
names(susmat2) =  c("Amphipoda", "Annelida", "Arachnida", "Barniclenaup",
                      "Calanoida","Cal juv", 
                      "Cal naup", "Cladocera", "Cnidaria",
                      "Collembola", "Cyclopoida", "Cyclo juv",
                      "Gammaridae", "Harpacticoida", "Insecta",
                      "Nematoda", "Ostracoda", "Other", "Rotifera")


#relative abundance amtrix
susmatp = susmat2/rowSums(susmat2)


#now some non=metric multidimentional scaling
ns1 = metaMDS(susmat2, trymax = 300)
ns2 = metaMDS(susmatp, trymax = 300)
#Yes!
Envmat$Region = as.factor(Envmat$Region)
Envmat$Site2 = as.factor(Envmat$Site2)
Envmat$SiteType = as.factor(Envmat$SiteType)
Envmat$Year = as.factor(Envmat$Year)


#Show the NMDS plots
source("plotNMDS.R")

#do a quick plot with hulls by location
PlotNMDS(ns2, data = Envmat, group = "SiteType")
#Huh. Maybe this is because we don't have a ton of samples from inside Lookout?
PlotNMDS(ns2, data = Envmat, group = "Year")
#Huh again.
PlotNMDS(ns2, data = Envmat, group = "Site2")
#Huh again.


library(Hmsc)
library(tidyverse)
library(MASS)
library(corrplot)

allzoopCom = Blitzmatp
envmat2 = Envmat

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
envmat2$Year2 = as.factor(envmat2$Year)
m2 = Hmsc(Y = as.matrix(allzoopCom), XData = as.data.frame(envmat2), 
          XFormula = ~Region + SiteType + Year2)

#Now do MCMC sampling on it to estimate model parameters

mm2 = sampleMcmc(m2, thin = 10, samples = 1000, transient = 5000,
                 nChains = 2)

#Check MCMC convergence diagnostics
mpost = convertToCodaObject(mm2)
diags = data.frame(effectiveSize(mpost$Beta), 
                   gelman.diag(mpost$Beta, multivariate=TRUE)$psrf)

save(mm2, mpost, diag, file = "test.RData")
load("test.RData")
#We are looking for high effective sample size and
#scale=reduction factors (gelman.diag) close to 1

#Look at effective sample size graphically
par(mfrow=c(1,2))
hist(effectiveSize(mpost$Beta), main="ess(beta)")
hist(gelman.diag(mpost$Beta, multivariate=TRUE)$psrf, main="psrf(beta)")

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
plotBeta(mm2, post = postBeta, param = "Support", 
         supportLevel = 0.95, 
         covNamesNumbers = c(T, F),
         mar = c(.1,.1,0,0))
plotBeta(mm2, post = postBeta, param = "Mean")

