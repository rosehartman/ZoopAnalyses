#LEt's start over with EMP and FRP integrated from the zooplankton synthesis app

library(zooper)
library(lubridate)
library(tidyverse)
library(readxl)
library(visreg)
library(vegan)
library(RColorBrewer)

#load data that I dowloded from the singy app
EMP20 = read.csv("EMP20.csv")

#EMP20 = Zoopsynther(Data_type = "Community",
#                               Sources = c("EMP", "20mm"),
#                               Size_class = "Meso",
#                    Months = c(3,4,5,6),
#                               Date_range = c("2017-01-01", "2019-12-30"),
#                    Redownload_data = T)

#write.csv(EMP20, "EMP20.csv", row.names = F)

#Load stations that I want to use
stas = read.csv("stations.csv")

stations = rename(stas, FRP = FRPStation, `20mm` = Twentymil, EMP = "EMPzoop") %>%
  mutate(`20mm` = as.character(`20mm`), FMWT = NULL, EMPreg = NULL,
         Site = factor(Site, levels = c("Wings","Little Honker Bay",
                                        "Grizzly Bay", "Browns", "Winter",
                                        "Sherman Lake", "Stacys", "Horseshoe Bend",
                                        "Prospect", "Lindsey"))) %>%
  pivot_longer(cols = c(FRP, `20mm`, EMP), names_to = "survey", values_to = "Station") %>%
  distinct() %>%
  filter(Shallowdeep =="Y", !is.na(Station), Station != "")

#filter the data so I just have the stations I want

EMP20.1 = left_join(filter(stations, survey != "FRP"), EMP20, by = "Station")

#now upload the crosswalk I want to use
crosswalk <- read_excel("FRP_EMPcrosswalk.xlsx", 
                        sheet = "zooper")

#if tehre were no species names on the data, zooper added "_UNID"
#so now I can't add it to the crosswalk. Let's get rid of that.

EMP20.1 = mutate(EMP20.1, Taxname = str_remove_all(Taxname, "_UnID"),
                 Taxlifestage = str_remove_all(Taxlifestage, "_UnID"))

#add analysis classes
EMP20.2 = mutate(crosswalk, Taxlifestage = paste(Taxname, Lifestage)) %>%
  dplyr::select(Taxlifestage, Phylum, Class, Order, Analy, Analy2) %>%
  distinct()%>%
  right_join(EMP20.1)

EMP20.3 = dplyr::select(EMP20.2, Site, survey, Station, Ggdist, SampleID, 
                 Date, CPUE, Lifestage, Taxname, Analy, Analy2)

################################################################################################################################

#add FRP data
zoopsfrp <- read_excel("zoopsexportqry.xlsx", 
                       col_types = c("text", "date", "text", 
                                     "text", "numeric", "text", "numeric", 
                                     "numeric", "numeric", "numeric", 
                                     "numeric", "numeric", "numeric", 
                                     "numeric", "numeric", "numeric", 
                                     "text", "text", "numeric", "numeric", 
                                     "text", "numeric"))

#First, lots of data manipulation

#Calculate CPUE

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
frptaxa <- read_excel("FRP_EMPcrosswalk.xlsx", 
                      sheet = "zooper")
frptaxa = dplyr::select(frptaxa,  FRP_Meso, Lifestage, Taxname, Analy, Analy2) %>%
  rename(CommonName = FRP_Meso) %>%
  filter(CommonName != "NA")

#attach the other categories to the main data set
zoop = merge(zoo, frptaxa)
zoop2 = left_join(stations, zoop) %>%
  filter(survey == "FRP") %>%
  dplyr::select(Site, survey, Station, Ggdist, SampleID, Date, CPUE, 
         Lifestage, Taxname, Analy, Analy2)

#the 2018 and 2019 data
library(readr)
zoop_FRP2 <- read_csv("zoop_line2270.csv", 
                      col_types = cols(Date = col_date(format = "%Y-%m-%d"))) %>%
  dplyr::select(CommonName, SampleID, Station, Date, CPUE, LifeStage)

zoop_FRP2.1 = merge(zoop_FRP2, frptaxa)
zoop3 = left_join(stations, zoop_FRP2.1 ) %>%
  filter(survey == "FRP") %>%
  dplyr::select(Site, survey, Station, Ggdist, SampleID, Date, CPUE, 
         Lifestage, Taxname, Analy, Analy2)

#Make sure we don't have duplicates with the other FRP data set
test = filter(zoop3, SampleID %in% unique(zoop2$SampleID))

zoop3.1 = filter(zoop3, !SampleID %in% unique(zoop2$SampleID))

#looks like the new data set just has the latter half of 2018 and 2019
#so we weren't missing anything from the 2018 20mm analysis.

#But we still seem to be missing some.
sumz = zoop_FRP2 %>%
  mutate(Month = month(Date), Year = year(Date)) %>%
  group_by(Station, Month, Year) %>%
  summarize(n = length(unique(SampleID)))

################ALL THE ZOOPS!!!###################
allzoops = rbind(zoop2, EMP20.3, zoop3.1) %>%
  mutate(Month = month(Date), Year = year(Date), analyLS = paste(Analy, Lifestage)) %>%
  filter(Month %in% c(3,4,5,6), Year %in% c(2017, 2018, 2019)) 

allzoops = mutate(allzoops, analyLS = factor(analyLS, levels = c(
  "Cladocera Adult", "Cyclopoida Adult","Cyclopoida Juvenile", 
  "Calanoida Adult", "Calanoida Juvenile" , "Calanoida Larva",
  "Copepoda Adult","Copepoda Larva","Harpacticoida Undifferentiated" ,               
   "Rotifera Adult", "Cumacea Adult","Barnacle Nauplii Larva",        
  "Decapoda Larva" ), 
  labels = c( "Cladocera", "Cyclopoids","Cyclo Juv", 
    "Calanoids", "Cala Juv" , "Cala Naup",
    "Other copepods","Copepod Naup","Harpacticoids" ,               
    "Rotifers", "Cumaceans","Barnacle Naup",        
    "Crab zoea" )))



############################################################################################################
#now some exploritory plots

#quick plot of CPUE by location

#First calculate the average CPUE of each critter (analysis group/life stage) by location and month and year
allzoops = mutate(allzoops, Year = year(Date), 
                  Month = month(Date), 
                  Month2 = factor(Month, levels = c(3,4,5,6), labels= c("Mar", "Apr","May","Jun")))



zoop20x= group_by(allzoops, SampleID, analyLS, Analy, Lifestage, Site, Month, Month2, survey, Year, Ggdist) %>% 
  summarize(CPUE = sum(CPUE, na.rm = T))
zooLitave = group_by(zoop20x, analyLS, Analy, Lifestage, Site, Month, Month2,survey, Year) %>% 
  summarize(CPUE = mean(CPUE, na.rm = T), ggdist = mean(Ggdist))
zooLitave =droplevels(zooLitave) %>%
  filter(analyLS != "NA NA")

#set up labels
zoolabs = c("Barnacle Nauplii", "Calanoida","Cal juv", "Cal nauplii", 
            "Cladocera","Copepoda", "Copepoda nauplii", "Cumacea",
            "Cyclopoda", "Cyclopoid juv", "crab zoea", "Harpacticoida",
            "Rotifera")

mypal = c(brewer.pal(12, "Set3"), brewer.pal(8, "Dark2"))

#Now a bar plot
z1 = ggplot(zooLitave, aes(x=survey, y= CPUE))
z1 + geom_bar(stat = "identity", aes(fill = analyLS), position = "fill") + 
  scale_fill_manual(values = c(mypal, "white", "green"),  name = NULL) + 
  facet_grid(Site~Month2, scales = "free_y") +
  #  coord_cartesian(ylim = c(0, 16000)) +
  xlab("Survey") + ylab("CPUE (count per cubic meter)")+
  theme(legend.position = "right")

z1 + geom_bar(stat = "identity", aes(fill = analyLS)) + 
  scale_fill_manual(values = c(mypal, "white", "green"), name = NULL) + 
  facet_grid(Site~Year+Month, scales = "free_y") +
  #  coord_cartesian(ylim = c(0, 16000)) +
  xlab("Sampling Month") + ylab("CPUE (count per cubic meter)")+
  theme(legend.position = "right")

###########################################################################################
#stats
#Overall CPUE GLM

#try taking out rotifers
#allzoops = filter(allzoops, Analy != "rotifera")

#First calculate total zoop CPUE per sample
zoosum = group_by(allzoops, SampleID, Site, Month, survey) %>% 
  summarize(CPUE = sum(CPUE, na.rm = T), ggdist = mean(Ggdist))

#Model log-transformed catch against time (month) and station
g1s = lm(log(CPUE)~Month+survey + ggdist, data = zoosum)
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
Commat = pivot_wider(allzoops, names_from = analyLS, values_from = CPUE, 
                     id_cols = c(SampleID, Site, Station, Date, Month, survey, Year, Ggdist),
                     values_fill = 0, values_fn = sum)
Envmat = Commat[,1:8]
test = Commat[,9:21]
test2 = test[,order(names(test))]

Commat2 = as.matrix(test2)

#relative abundance amtrix
Commatp = Commat2/rowSums(Commat2)

#make better names for adding to the NMDS plots
names(Commat2) =  c("Barnacle naup", "Calanoida","Cal juv", "Cal naup", 
                    "Cladocera", "Other", "other naup",  "Cumacea",
                    "Cyclopoda", "Cyclo juv", "Crab Zoea", "Harpacticoida",
                     "Rotifera")
names(Commatp) = names(Commat2)


#PerMANOVA of abundance matrix survey and month
adonis(Commat2~Month + survey + Site, data = Envmat)


#Do it again with relative abundance
a1 = adonis(Commatp~ survey+ Site+Month, data = Envmat)
a1
#site and month make a much bigger difference  than survey for community compositino

#now some non=metric multidimentional scaling
#n1 = metaMDS(Commat2, trymax = 300)
#n2 = metaMDS(Commatp, trymax = 1000)
#Nope, not working.
Envmat$survey = as.factor(Envmat$survey)
Envmat$Site = as.factor(Envmat$Site)
Envmat$Month = as.factor(Envmat$Month)
Envmat$Year = as.factor(Envmat$Year)

######################################################################
#Maybe need to try a subset?
EnvmatGriz = filter(Envmat, Site == "Grizzly Bay")
CommatGriz = Commatp[which(Envmat$Site== "Grizzly Bay"),]

ng1 = metaMDS(CommatGriz, trymax = 100)

#Show the NMDS plots
source("plotNMDS.R")

#do a quick plot with hulls by location
PlotNMDS(ng1, data = EnvmatGriz, group = "survey")
#there really aren't significatn differences
PlotNMDS(ng1, data = EnvmatGriz, group = "Month")
PlotNMDS(ng1, data = EnvmatGriz, group = "Year")
#those are much more interesting 

#################################################################
#Take a closer look at the copepods

#put them togetehr with finer=scale taxonomic resoltion

cops = filter(allzoops, Analy %in% c("Cyclopoida", "Calanoida", "Copepoda"), Site != "Ryer Island")



#Now a bar plot
c1 = ggplot(cops, aes(x=survey, y= CPUE))
c1 + geom_bar(stat = "identity", aes(fill = Analy2), position = "fill") + 
  scale_fill_manual(values = c(mypal, "white", "green"), name = NULL) + 
  facet_grid(Site~Year+Month, scales = "free_y") +
  #  coord_cartesian(ylim = c(0, 16000)) +
  xlab("Sampling Month") + ylab("CPUE (count per cubic meter)")+
  theme(legend.position = "right")


#by life stage

c1 + geom_bar(stat = "identity", aes(fill = Lifestage), position = "fill") + 
  # scale_fill_manual(values = c(mypal, "white", "green"), name = NULL) + 
  facet_grid(Site~Year+Month, scales = "free_y") +
  #  coord_cartesian(ylim = c(0, 16000)) +
  xlab("Sampling Month") + ylab("CPUE (count per cubic meter)")+
  theme(legend.position = "right")
#it kinda looks like FRP gets more juveniles

#Try just looking at adults
copad = filter(cops, Lifestage == "Adult")
c2 = ggplot(copad, aes(x=survey, y= CPUE))
c2 + geom_bar(stat = "identity", aes(fill = Analy2), position = "fill") + 
  scale_fill_manual(values = c(mypal, "white", "green", "black"), name = NULL) + 
  facet_grid(Site~Year+Month, scales = "free_y") +
  #  coord_cartesian(ylim = c(0, 16000)) +
  xlab("Sampling Month") + ylab("CPUE (count per cubic meter)")+
  theme(legend.position = "right")

##############################################################################
#multivariate stats on adult copepods

#Create a community matrix
copmat = pivot_wider(copad, names_from = Analy2, values_from = CPUE, 
                     id_cols = c(SampleID, Site, Station, Date, Month, survey, Year, Ggdist),
                     values_fill = 0, values_fn = sum)
Envmat = copmat[,1:8]
test = copmat[,9:21]
test2 = test[,order(names(test))]

copmat2 = as.matrix(test2)

#relative abundance amtrix
copmatp = copmat2/rowSums(copmat2)


#PerMANOVA of abundance matrix survey and month
adonis(copmat2~Month + survey + Site, data = Envmat)

#Do it again with relative abundance
a1 = adonis(copmatp~Month + survey+ Ggdist + Year, data = Envmat)
a1
#Month is definitely the biggest thing

#now some non=metric multidimentional scaling
n1 = metaMDS(copmat2, trymax = 300)
#Nope
n2 = metaMDS(copmatp, trymax = 2000)
#Nope

Envmat$survey = as.factor(Envmat$survey)
Envmat$Site = as.factor(Envmat$Site)
Envmat$Month = as.factor(Envmat$Month)
Envmat$Year = as.factor(Envmat$Year)

#Show the NMDS plots
source("plotNMDS.R")

#do a quick plot with hulls by location
PlotNMDS(n1, data = Envmat, group = "survey")
#there are definitely differences, but they are a bit of a mess
PlotNMDS(n1, data = Envmat, group = "Month")
PlotNMDS(n1, data = Envmat, group = "Year")


#do a quick plot with hulls by location
PlotNMDS(n2, data = Envmat, group = "survey")
#there are definitely differences, but they are a bit of a mess
PlotNMDS(n2, data = Envmat, group = "Month")
PlotNMDS(n2, data = Envmat, group = "Year")


sim1 = with(Envmat, simper(copmatp, survey))
summary(sim1)

sim2 = with(Envmat, simper(copmatp, Month))
summary(sim2)

#############################################################

#For sites where both surveys sampled in teh same months, what was the total differences in catch?

Copave = copmat %>%
  pivot_longer(cols = c("Acanthocyclops":"Paracyclops"),
               names_to = "Taxname", values_to = "CPUE") %>%
  group_by(Site, Month, survey, Year, Ggdist, Taxname) %>%
  summarize(CPUEm = mean(CPUE))


test = ungroup(Copave) %>% expand(survey, Site, Month, Year)

Copave2 = Copave %>%
  group_by(Site, Month, survey, Year, Ggdist) %>%
  summarize(CPUEm = mean(CPUEm))

missing = anti_join(test, Copave2)
missing$foo = "foo"

Copave3 = left_join(Copave, missing[,2:5])
Copave3 = filter(Copave3, is.na(foo))

px = ggplot(Copave3, aes(x = survey, y = CPUEm, fill = Taxname)) +
  geom_bar(stat = "identity") + facet_grid(Year~Site)
px

py = ggplot(Copave3, aes(x = survey, y = CPUEm, fill = Taxname)) +
  geom_bar(stat = "identity") + facet_grid(.~Site)+ 
  scale_fill_manual(values = c(mypal, "white", "green", "black"), name = NULL) 
py

py = ggplot(Copave3, aes(x = survey, y = CPUEm, fill = Taxname)) +
  geom_bar(stat = "identity", position = "fill") + facet_grid(.~Site)+ 
  scale_fill_manual(values = c(mypal, "white", "green", "black"), name = NULL) 
py

py2 = ggplot(Copave3, aes(x = survey, y = CPUEm, fill = Taxname)) +
  geom_bar(stat = "identity", position = "fill") + 
  scale_fill_manual(values = c(mypal, "white", "green", "black"), name = NULL) 
py2

###########################J

#what if I do the average percent composition instead of average everything?


Copave4 = cbind(Envmat, copmatp) %>%
  pivot_longer(cols = c("Acanthocyclops":"Tortanus"),
               names_to = "Taxname", values_to = "CPUE") %>%
  group_by(Site, Month, survey, Year, Ggdist, Taxname) %>%
  summarize(CPUEm = mean(CPUE))

Copave5 = Copave4 %>%
  group_by(Month, survey, Year, Taxname) %>%
  summarize(CPUEm = mean(CPUEm))

Copave6 = Copave5 %>%
  group_by(survey, Taxname) %>%
  summarize(CPUEm = mean(CPUEm))

ptot = ggplot(Copave6, aes(x= survey, y = CPUEm, fill = Taxname))
ptot + geom_bar(stat = "identity", position = "fill") + 
  scale_fill_manual(values = c(mypal, "white", "green", "black"), name = NULL) 


#Joint species distribution models with HMSC

library(Hmsc)
library(tidyverse)
library(MASS)
library(corrplot)

allzoopCom = Commatp
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
m2 = Hmsc(Y = as.matrix(allzoopCom), XData = as.data.frame(envmat2), 
          XFormula = ~survey + Month + Ggdist)

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
         covNamesNumbers = c(F, F),
         mar = c(.1,.1,.1,.1))
plotBeta(mm2, post = postBeta, param = "Mean")

