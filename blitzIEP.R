#Blitz data plus EMP stuff

library(zooper)
library(lubridate)
library(tidyverse)
library(readxl)
library(visreg)
library(lme4)
library(vegan)
library(RColorBrewer)

library(effects)
library(visreg)

#IEPzoop = Zoopsynther(
#  Data_type = "Community",
#  Sources = c("EMP", "20mm"),
#  Size_class = c("Meso"),
#  Months = c(3,4),
#  Years = c(2017, 2018, 2019),
#  Redownload_data = T,
#  All_env = T,
#  Shiny = F
#)

#save(IEPzoop, file = "IEPblitz.RData")
load("IEPblitz.RData")

# Get the FRP zoop data ready to mesh
load("FRPzoop.RData")


#import analysis categories to make it easier to deal with
frptaxa <- read_excel("FRP_EMPcrosswalk.xlsx", 
                      sheet = "zooper")
frptaxa = dplyr::select(frptaxa,  FRP_Meso, Lifestage, Taxname, Analy, Analy2) %>%
  rename(CommonName = FRP_Meso) %>%
  filter(CommonName != "NA")

FRPzoop2 = dplyr::select(FRPzoop, CommonName, SampleID, Station, Date, CPUE) %>%
  left_join(frptaxa)

#Load stations that I want to use
stations = read.csv("stations2.csv")

#filter the data so I just have the stations I want
FRPzoop2.1 = left_join(filter(stations, survey == "FRP"), FRPzoop2, by = "Station") %>%
  dplyr::select(Site, Region, SiteType, Site2, survey, Station, Ggdist, SampleID, Date, CPUE,
         Lifestage, Taxname, Analy, Analy2) %>%
mutate(Region = factor(Region, levels = c("Suisun Marsh", "Suisun Bay", "Confluence",
                                          "Sac SanJ", "Cache")))



IEPzoop.1 = left_join(filter(stations, survey != "FRP"), IEPzoop, by = "Station")

#now upload the crosswalk I want to use
crosswalk <- read_excel("FRP_EMPcrosswalk.xlsx", 
                        sheet = "zooper")

#if tehre were no species names on the data, zooper added "_UNID"
#so now I can't add it to the crosswalk. Let's get rid of that.

IEPzoop.1 = mutate(IEPzoop.1, Taxname = str_remove_all(Taxname, "_UnID"),
                 Taxlifestage = str_remove_all(Taxlifestage, "_UnID"))

#add analysis classes
IEPzoop.2 = mutate(crosswalk, Taxlifestage = paste(Taxname, Lifestage)) %>%
  dplyr::select(Taxlifestage, Phylum, Class, Order, Analy, Analy2) %>%
  distinct()%>%
  right_join(IEPzoop.1)

IEPzoop.3 = dplyr::select(IEPzoop.2, Site, Region, SiteType, Site2, survey, Station, Ggdist, SampleID, 
                        Date, CPUE, Lifestage, Taxname, Analy, Analy2)
#OK, now I think we are ready to merge

allzoopsb = rbind(FRPzoop2.1, IEPzoop.3) %>%
  mutate(Year = year(Date), Month = month(Date)) %>%
  filter(Year %in% c(2017, 2018, 2019), Month %in% c(3,4))

###################################################


#Decker and Flyway became tidal in 2019
#but we only have flyway data from 2019 anyway.
allzoopsb$SiteType[which(allzoopsb$Site2 == "Decker" & allzoopsb$Year >2018)] = "Tidal" 



#First calculate the average CPUE of each critter (analysis group/life stage) by location and year
allzoopsb = mutate(allzoopsb,  AnalyLS = paste(Analy, Lifestage)) %>%
  filter(Analy != "NA") %>%
  mutate(AnalyLS = factor(AnalyLS, levels = c("Cyclopoida Adult","Cyclopoida Juvenile", "Calanoida Adult",
                                              "Calanoida Juvenile","Calanoida Larva","Copepoda Adult", "Copepoda Larva",   
                                              "Harpacticoida Undifferentiated", "Barnacle Nauplii Larva", "Cladocera Adult", 
                                              "Rotifera Adult",   "Decapoda Larva"), 
                          labels = c("Cyclopoid", "Cyclo copepedite", "Calanoid", "Cala copepedite", "Cala nauplii", 
                                     "Other copepods", "Copepod nauplii", "Harpacticoids", "Barnacle nauplii",
                                     "Cladocera", "Rotifers", "Crab zoea")))

zoop20x= group_by(allzoopsb, SampleID, survey, AnalyLS, Analy, Lifestage, Site2, 
                  SiteType, Region, Year, Ggdist) %>% 
  summarize(CPUE = sum(CPUE, na.rm = T))
zooLitave = group_by(zoop20x, AnalyLS, Analy, survey, Lifestage, Site2, SiteType, 
                     Region, Year) %>% 
  summarize(CPUE = mean(CPUE, na.rm = T), ggdist = mean(Ggdist))
zooLitave =droplevels(zooLitave)

zooLitave2 = group_by(zooLitave, AnalyLS, Analy, survey, Lifestage, SiteType, 
                     Region) %>% 
  summarize(CPUE = mean(CPUE, na.rm = T), ggdist = mean(ggdist))

#how many samples per site type/region combo?
samplesize = group_by(allzoopsb, Region, SiteType) %>%
  summarize(n = length(unique(SampleID)))


#how many samples per site type/region/year/survey combo?
samplesize2 = group_by(allzoopsb, Region, SiteType, survey, Year) %>%
  summarize(n = length(unique(SampleID)))

zooLitave2 = merge(zooLitave2, samplesize)
#set up labels
#zoolabs = c("Amphipoda", "Annelida", "Barnacle Nauplii", "Calanoida",
 #           "Cal juv", "Cal nauplii", 
  #          "Cladocera","Cnidaria","Collembola", "Cyclopoida",
   #         "Cyclopoid juv", "Gammarid", "Harpacticoida", "Insecta", "Nematoda",
    #        "Ostracoda","Rotifera")

mypal = c(brewer.pal(12, "Set3"), brewer.pal(8, "Dark2"))

#Now a bar plot
z1 = ggplot(zooLitave, aes(x=Site2, y= CPUE))
z1 + geom_bar(stat = "identity", aes(fill = AnalyLS), position = "fill") + 
  scale_fill_manual(values = c(mypal, "white", "green"), name = NULL) + 
  facet_grid(.~Region + SiteType, scales = "free_x", space = "free") +
  #  coord_cartesian(ylim = c(0, 16000)) +
  xlab("Site") + ylab("Percent composition")+
  theme(legend.position = "right", axis.text.x = element_text(angle = 90))

z1 = ggplot(zooLitave, aes(x=SiteType, y= CPUE))
z1 + geom_bar(stat = "identity", aes(fill = AnalyLS), position = "fill") + 
  scale_fill_manual(values = c(mypal, "white", "green"), name = NULL) + 
  facet_grid(.~Region, scales = "free_x", space = "free") +
  #  coord_cartesian(ylim = c(0, 16000)) +
  xlab("Site") + ylab("Percent composition")+
  theme(legend.position = "right", axis.text.x = element_text(angle = 90))

z2 = ggplot(zooLitave2, aes(x=SiteType, y= CPUE))
z2 + geom_bar(stat = "identity", aes(fill = AnalyLS), position = "fill") + 
  scale_fill_manual(values = c(mypal, "white", "green"), name = NULL) + 
  geom_text(aes(x = SiteType, y = 1, label = n))+
  facet_grid(.~Region, scales = "free_x", space = "free") +
  #  coord_cartesian(ylim = c(0, 16000)) +
  xlab(NULL) + ylab("Percent composition")+
  theme(legend.position = "right", text = element_text(size = 16),
        axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5, size = 16))

z2 + geom_bar(stat = "identity", aes(fill = AnalyLS, position = 'fill')) + 
  scale_fill_manual(values = c(mypal, "white", "green"), name = NULL) + 
  geom_text(aes(x = Region, y = 1, label = n), size = 5)+
  facet_grid(.~SiteType, scales = "free_x", space = "free") +
  #  coord_cartesian(ylim = c(0, 16000)) +
  xlab(NULL) + ylab("Percent Composition")+
  
  theme(legend.position = "right", text = element_text(size = 16),
        axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5, size = 16))

##########################################################################
#Statistics
#What if "site" was my random effect?

#First calculate total zoop CPUE per sample
zoosum = group_by(allzoopsb, SampleID, Site2, Region, SiteType, Year) %>% 
  summarize(CPUE = sum(CPUE, na.rm = T), ggdist = mean(Ggdist)) %>%
  mutate(Year2 = as.factor(Year))

#Model log-transformed catch against time (month) and station
g1s = lmer(log(CPUE)~SiteType + Region + Year2 + (1|Site2), data = zoosum)
summary(g1s)
#gross
visreg(g1s)

##############################################################
#plot the partial residuals

partplot = function(data, model, term){
p1.1 = Effect(term, model, residuals = T)
term2 = noquote(term)

dat = data.frame(term2 = p1.1$variables[[1]]$levels, fit = p1.1$fit, 
                 lower = p1.1$lower, upper = p1.1$upper)
datres = data.frame(term2 = p1.1$data[[term]], resid = p1.1$residuals) %>%
  left_join(dat) %>%
  mutate(fitres = fit+resid) %>%
  rename(fit1 = fit, fit = fitres)


test = ggplot() +
  geom_point(data = datres, aes(x = term2, y = fit, color = term2), 
              alpha = 0.3, position = "jitter")+
  xlab(term) + ylab("log(CPUE)")+
  geom_point(data = dat, aes(x = term2, y = fit)) +
  geom_errorbar(data = dat, aes(x = term2, ymin = lower, ymax = upper))+
  scale_color_discrete(guide = NULL)+
  theme_bw() + theme(text = element_text(size = 16))

test
}


partplot(zoosum, g1s, "SiteType")
partplot(zoosum, g1s, "Region")
partplot(zoosum, g1s, "Year2")


g2 = lm(log(CPUE)~SiteType + Region + Year2 + Site2, data = zoosum)
summary(g2)

visreg(g2)

#plot of mean total CPUE
zoosumx = group_by(zoosum, Site2, Region, SiteType, Year) %>% 
  summarize(mCPUE = mean(CPUE, na.rm = T)) %>%
  group_by(Region, SiteType) %>% 
  summarize(mmCPUE = mean(mCPUE, na.rm = T), sd = sd(mCPUE), se = sd(mCPUE)/length(mCPUE))

bg = ggplot(zoosumx, aes(x = SiteType, y = mmCPUE, fill = SiteType)) +
  geom_bar(stat = "identity") + 
  geom_errorbar(aes(ymin = mmCPUE-sd, ymax = mmCPUE+sd), group = zoosumx$Region) +
  facet_grid(.~Region, scales = "free_x", space = "free")

bg + 
 # coord_cartesian(ylim = c(0, 6000)) + 
  scale_fill_discrete(guide = NULL)

######################################################
#multivariate stats
#Create a community matrix
blitzmat = pivot_wider(allzoopsb, names_from = AnalyLS, values_from = CPUE, 
                       id_cols = c(SampleID, Site2, Station, Region, SiteType, Date, Year, Ggdist),
                       values_fill = 0, values_fn = sum)
Envmat = blitzmat[,1:8]
test = blitzmat[,9:20]
test2 = test[,order(names(test))]

Blitzmat2 = as.matrix(test2)

#relative abundance amtrix
Blitzmatp = Blitzmat2/rowSums(Blitzmat2)

#make better names for adding to the NMDS plots
names(Blitzmat2) =  c("Barnacle Nauplii", "Calanoida","Cal juv", "Cal nauplii", 
                                "Cladocera","Copepoda", "Copepoda nauplii", "Cumacea",
                                "Cyclopoda", "Cyclopoid juv", "crab zoea", "Harpacticoida",
                                "Rotifera")
names(Blitzmatp) = names(Blitzmat2)


#PerMANOVA of abundance matrix survey and month
adonis(Blitzmat2 ~ SiteType + Region + Year + Site2, data = Envmat)


#Do it again with relative abundance
a1 = adonis(Blitzmatp ~ SiteType + Region + Year + Site2, data = Envmat)
a1
#Site is the biggest predicotr, year hardly at all

#visualize the permanova results
perman = a1$aov.tab[-6,]
perman = mutate(perman, predictors = row.names(perman))

bp<- ggplot(perman, aes(x="", y=R2, fill= predictors))+
  geom_bar(width = 1, stat = "identity")
pie = bp + coord_polar("y", start=0)

library(scales)
blank_theme <- theme_minimal()+
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.border = element_blank(),
    panel.grid=element_blank(),
    axis.ticks = element_blank(),
    plot.title=element_text(size=14, face="bold")
  )

pie + blank_theme +
  theme(axis.text.x=element_blank()) +
  scale_fill_brewer(palette = "Set2")

#now some non=metric multidimentional scaling
n1 = metaMDS(Blitzmat2, trymax = 300)
n2 = metaMDS(Blitzmatp, trymax = 300)
#Nope, not working.

################################################################3


library(Hmsc)
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
#m2 = Hmsc(Y = as.matrix(allzoopCom), XData = as.data.frame(envmat2), 
#          XFormula = ~Region + SiteType + Year2)

#studydesign = data.frame(plot = as.factor(envmat2$Site2))
#rL2 = HmscRandomLevel(units = envmat2$Site2)
#m = Hmsc(Y = as.matrix(allzoopCom), XData = as.data.frame(envmat2),
#         XFormula =  ~SiteType + Region + Year,
#         studyDesign = studydesign, ranLevels = list("plot" = rL2), distr = "lognormal poisson")

m = Hmsc(Y = as.matrix(allzoopCom), XData = as.data.frame(envmat2),
                  XFormula =  ~SiteType + Region + Year,
                 distr = "lognormal poisson")
         
#Now do MCMC sampling on it to estimate model parameters

m2 = sampleMcmc(m, thin = 10, samples = 1000, transient = 5000,
               nChains = 2)  



#Check MCMC convergence diagnostics
mpost = convertToCodaObject(m2)
diags = data.frame(effectiveSize(mpost$Beta), 
                   gelman.diag(mpost$Beta, multivariate=TRUE)$psrf)

#We are looking for high effective sample size and
#scale=reduction factors (gelman.diag) close to 1

#Look at effective sample size graphically
par(mfrow=c(1,2))
hist(effectiveSize(mpost$Beta), main="ess(beta)")
hist(gelman.diag(mpost$Beta, multivariate=TRUE)$psrf, main="psrf(beta)")

#To assess the model’s explanatory power, we apply the evaluateModelFit 
#function to the posterior predictive
#distribution simulated by the function computePredictedValues.
preds = computePredictedValues(m2, nParallel = 2)
fit = evaluateModelFit(hM = m2, predY = preds)
#

# We next evaluate the model’s predictive power through two-fold cross validation.
partition = createPartition(m, nfolds = 2)
predscross = computePredictedValues(m2, partition = partition)

evaluateModelFit(hM = m2, predY = predscross)

#Let us now look at the estimates of the β parameters. We may do so visually by applying the plotBeta
#function.
postBeta = getPostEstimate(m2, parName = "Beta")
plotBeta(m, post = postBeta, param = "Support", 
         supportLevel = 0.90, 
         covNamesNumbers = c(T, F),
         mar = c(.1,.1,0,0))
plotBeta(m, post = postBeta, param = "Mean", supportLevel = 0.5)

betamean = postBeta$mean
for(j in 1:ncol(postBeta$mean)) {
for(i in 1:nrow(postBeta$mean)) {
  if ((postBeta$support[i,j] >0.95)) betamean[i,j] = betamean[i,j] else {
    if(postBeta$supportNeg[i,j] > 0.95) betamean[i,j] = betamean[i,j] else betamean[i,j] = 0
  }
}}

betamean = as.data.frame(betamean)
betamean$variables = m$covNames
betamean2 = pivot_longer(betamean, cols = `Barnacle nauplii`:Rotifers, 
                         names_to = "Species", values_to = "support")

ggplot(betamean2) + geom_tile(aes(x=variables, y = Species, fill = support))+
  scale_fill_gradient2(low = "blue",
                       mid = "white",
                       high = "red", name = NULL) +
  #scale_x_discrete(labels = c("Cache, \n Channel, 2017", "Confluence", 
  ##                            "Sac SanJ", "Suisun Bay", "Suisun Marsh",
  #                           "Diked", "Muted", "Tidal", "2018", "2019") 
  #                 )+
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))


#I don't tknow why it looks like diked wetlands have less cladocera

clads = filter(zoop20x, AnalyLS == "Cladocera")
c1 = lmer(log(CPUE+1)~ SiteType + Year + (1|Site2), data = clads)
summary(c1)

c2 = glm(log(CPUE+1)~ SiteType + Region + Year + Site2, data = clads)
summary(c2)

#Hm. It looks like having "site" in there reverses the effect of "site type" HOw obnoxious
