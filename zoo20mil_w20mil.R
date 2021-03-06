#Clean version of our zooplankton data that was taken concurrently with 20 mm in 2017

#Look at the all the 20mm comparison samples
library(lubridate)
library(dplyr)
library(ggplot2)
library(reshape2)
library(vegan)
library(RColorBrewer)
library(visreg)

#import the data
library(readxl)
zoopsfrp <- read_excel("zoopsexportqry.xlsx", 
                                         col_types = c("text", "date", "text", 
                                                       "text", "numeric", "text", "numeric", 
                                                       "numeric", "numeric", "numeric", 
                                                       "numeric", "numeric", "numeric", 
                                                       "numeric", "numeric", "numeric", 
                                                       "text", "text", "numeric", "numeric", 
                                                       "text", "numeric"))
View(zoopsfrp)

################################################################################################################################
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
                      sheet = "allfrptaxa")

#attach the other categories to the main data set
zoop = merge(zoo, frptaxa)

#There were hardly any cumaceans, so I'm going to lump them in with "other"
#zoop$Analy[which(zoop$CommonName=="Cumaceans")] = "other"

#Fix a few of the common names
zoop$CommonName[which(zoop$CommonName== "Cladocera")] = "Cladocera Other"
zoop$CommonName[which(zoop$CommonName== "Simocephalus")] = "Cladocera Other"
zoop$CommonName[which(zoop$CommonName== "Sida")] = "Cladocera Other"
zoop$CommonName[which(zoop$CommonName== "Ilyocryptus")] = "Cladocera Other"
zoop$CommonName[which(zoop$CommonName== "Chydorus")] = "Cladocera Other"
zoop$CommonName[which(zoop$CommonName== "Moina")] = "Cladocera Other"
zoop$CommonName[which(zoop$CommonName== "Branchionus")] = "Rotifer Other"
zoop$CommonName[which(zoop$CommonName== "Kellicottia")] =  "Rotifer Other"
zoop$CommonName[which(zoop$CommonName== "Keratella")] =  "Rotifer Other"
zoop$CommonName[which(zoop$CommonName== "Platyias")] =  "Rotifer Other"
zoop$CommonName[which(zoop$CommonName== "Diacyclops")] = "Cyclopoid Other"
zoop$CommonName[which(zoop$CommonName== "Eucyclops")] = "Cyclopoid Other"
zoop$CommonName[which(zoop$CommonName== "Halicyclops")] = "Cyclopoid Other"
zoop$Analy[which(zoop$Analy %in% c("Amphipod", "gammarid", "corophium"))] = "amphipod"

zoop$Analy[which(zoop$Analy %in% c("Corixidae", "Ceratopogonidae larvae"))] = "insect"

#remove categories not counted by 20mm
#zoop = filter(zoop, Analy!= "mollusca"  & Analy != "insect", 
#                Analy != "terrestrial", Analy != "collembola" &Analy != "isopod" & 
# CommonName != "Nematode" &
#                 CommonName != "Mite"& CommonName != "Hydra")
#
 zoop = filter(zoop, !Analy %in% c("annelid","amphipod", "mollusca", "insect", "terrestrial","collembola" ,"isopod", 
                                    "Nematode", "Mite","Hydra", "mysid")) 

#create a new "month" variable
zoop$Month = month(zoop$Date)


#filter out just the samples from the spring of 2017 and 2018
zooLit = filter(zoop, Month %in% c(3,4,5,6), Date > "2017-1-1" & Date < "2018-6-20")


test = group_by(zooLit, SampleID, Station, Date, Month) %>%
  summarize(tot = sum(CPUE))

#Import information on the samples that were taken in conjunction with the 20mm survey
X20mil <- read.csv("20milfrp.csv")

#subset just the samples taken in conjunction with 20mm
zooLit2 = merge(zooLit, X20mil, by = "SampleID")

#create a new variable that is a combination of analysis group and life stage
zooLit2$anlyLS = paste(zooLit2$Analy, zooLit2$lifestage)


# THere was one sample with a rediculously huge number of rotifers
#I'm going to remove the wierd rotifer sample from Lindsey, 'cause it's wierd.
#zooLit2 = filter(zooLit2, SampleID != "ZOOP1-7JUN2017")

#add station info
stations = read.csv("stations.csv")
zooLit2.1 = select(stations, Site, FRPStation) %>%
  rename(Station = FRPStation) %>%
  left_join(zooLit2)

#################################################################################################
#Now let's import the 20mm data and do fun stuff with it
library(readxl)
X20mm <- read_excel("20mm Zooplankton Catch Matrix_1995-2018.xlsx", 
                    sheet = "20-mm CB CPUE Data", col_types = c("date", 
                                                                "numeric", "numeric", "numeric", 
                                                                "date", "numeric", "numeric", "numeric", 
                                                                "numeric", "numeric", "numeric", 
                                                                "numeric", "numeric", "numeric", 
                                                                "numeric", "numeric", "numeric", 
                                                                "numeric", "numeric", "numeric", 
                                                                "numeric", "numeric", "numeric", 
                                                                "numeric", "numeric", "numeric", 
                                                                "numeric", "numeric", "numeric", 
                                                                "numeric", "numeric", "numeric", 
                                                                "numeric", "numeric", "numeric", 
                                                                "numeric", "numeric", "numeric", 
                                                                "numeric", "numeric", "numeric", 
                                                                "numeric", "numeric", "numeric", 
                                                                "numeric", "numeric", "numeric", 
                                                                "numeric", "numeric", "numeric", 
                                                                "numeric", "numeric", "numeric", 
                                                                "numeric", "numeric", "numeric", 
                                                                "numeric", "numeric", "numeric", 
                                                                "numeric", "numeric", "numeric", 
                                                                "numeric", "numeric", "numeric", 
                                                                "numeric", "numeric", "numeric", 
                                                                "numeric", "numeric", "numeric", 
                                                                "numeric", "numeric", "numeric", 
                                                                "numeric", "numeric", "numeric", 
                                                                "numeric", "numeric", "numeric", 
                                                                "numeric", "numeric", "numeric", 
                                                                "numeric", "numeric"))
X20mm2 = pivot_longer(X20mm[, -c(79:85)], 
                      cols = Acanthocyclops:`Unid rotifer`, 
                      names_to = "Taxname", values_to = "CPUE") %>%
  mutate(Month = month(SampleDate), Year = year(SampleDate),
         SampleID = paste(Station, SampleDate)) %>%
  filter(Year %in% c(2017, 2018))

#check and see which critters got dropped in later years
test = group_by(X20mm2, Taxname) %>%
  summarise(count = sum(CPUE))

test2 = filter(test, count != 0)
x20mm2 = left_join(test2, X20mm2) %>%
  select(-count)

check = ggplot(X20mm2, aes(x = Month, y = CPUE, fill = Taxname)) + 
  geom_bar(stat = "identity", position = "fill") + facet_grid(.~Year)

check

#import analysis categories to make it easier to deal with
taxa20 <- read_excel("FRP_EMPcrosswalk.xlsx", 
                      sheet = "attatch20mm")
taxa20$Taxname = taxa20$`20mm`

#attach the other categories to the main data set
zoop20 = left_join(x20mm2, taxa20)

check2 = ggplot(zoop20, aes(x = Month, y = CPUE, fill = Analy)) + 
  geom_bar(stat = "identity", position = "fill") + facet_grid(.~Year)

check2

#Subset just the dates and samples we are interested in
zoop20 = filter(zoop20, Station %in% c(703, 609, 602, 
                                           705, 706, 720, 723, 724, 
                                           726, 801, 501, 520))%>%
  mutate(Station = as.character(Station))

sta20 = distinct(select(stations, Site, Twentymil)) %>%
  mutate(Station = as.character(Twentymil)) %>%
  filter(Site != "winter", Site != "Dow Wetlands", 
         Site != "Browns", Site != "Wings", Site != "Tule Red")

zoop20.1 =  
  left_join(zoop20, sta20[,c(1,3)])


#Make the column names match so we can merge the data sets
names(zooLit2.1)
zooLit3 = select(zooLit2.1, Site, SampleID, Station, Date, CPUE, Analy, lifestage, anlyLS, Month)
zooLit3$survey = "FRP"


names(zoop20.1)
zoop20.2 = rename(zoop20.1, Date = SampleDate) %>%
  mutate(anlyLS = paste(Analy, lifestage),
         survey = "20mm") %>%
  select(SampleID, Site, Station, Date, CPUE, Analy, lifestage, anlyLS, Month, survey)

allzoops = rbind(zoop20.2, zooLit3) %>%
  mutate(Year = year(Date))


#re-organize the "other" category

allzoops$anlyLS[which(allzoops$anlyLS == "other adult")] = "other"
allzoops$anlyLS[which(allzoops$anlyLS == "other juv")] = "other"
allzoops$anlyLS[which(allzoops$anlyLS == "other nauplii")] = "other"
allzoops$anlyLS[which(allzoops$anlyLS == "insect adult")] = "other"
allzoops$anlyLS[which(allzoops$anlyLS == "amphipod adult")] = "amphipoda"
allzoops$anlyLS[which(allzoops$anlyLS == "amphipod juv")] = "amphipoda"

#import distances from the Golden Gate (approximate)
stations20mm <- read_excel("stations20mm.xlsx")
names(stations20mm) = c("Ggdist", "Station")
allzoops2 = merge(allzoops, stations20mm)

#drop out some mistakes
allzoops = filter(allzoops2, !is.na(Month), !is.na(Year), Site != "Webb Tract", Site != "Tule Red", Site != "Wings",
                  Site != "winter", Site != "Dow Wetlands", Site != "Browns", Site != "Ryer Island", Month != 7)
 
############################################################################################################
#now some exploritory plots

#quick plot of CPUE by location

#First calculate the average CPUE of each critter (analysis group/life stage) by location and month and year
allzoops = mutate(allzoops, Year = year(Date))
zoop20x= group_by(allzoops, SampleID, anlyLS, Analy, lifestage, Site, Month, survey, Year, Ggdist) %>% 
  summarize(CPUE = sum(CPUE, na.rm = T))
zooLitave = group_by(zoop20x, anlyLS, Analy, lifestage, Site, Month, survey, Year) %>% 
  summarize(CPUE = mean(CPUE, na.rm = T), ggdist = mean(Ggdist))
zooLitave =droplevels(zooLitave)

#set up labels
zoolabs = c("Calanoida","Cal juv", "Cal nauplii", 
            "Cladocera", "Copepoda nauplii", "cumaceans",
            "Cyclopoda", "Cyclopoid juv", "decapod juv", "fish",  "Harpacticoida",
           "Ostracoda", "other",
            "Rotifera")

mypal = c(brewer.pal(12, "Set3"), brewer.pal(8, "Dark2"))

#Now a bar plot
z1 = ggplot(zooLitave, aes(x=survey, y= CPUE))
z1 + geom_bar(stat = "identity", aes(fill = anlyLS), position = "fill") + 
 scale_fill_manual(values = c(mypal, "white", "green"), name = NULL) + 
  facet_grid(Site~Year+Month, scales = "free_y") +
  #  coord_cartesian(ylim = c(0, 16000)) +
  xlab("Sampling Month") + ylab("CPUE (count per cubic meter)")+
  theme(legend.position = "right")

z1 + geom_bar(stat = "identity", aes(fill = anlyLS)) + 
  scale_fill_manual(values = c(mypal, "white", "green"), name = NULL) + 
  facet_grid(Site~Year+Month, scales = "free_y") +
  #  coord_cartesian(ylim = c(0, 16000)) +
  xlab("Sampling Month") + ylab("CPUE (count per cubic meter)")+
  theme(legend.position = "right")


#look at it one month at a time
#march
zmarch = ggplot(filter(zooLitave, Month == 3), aes(x=survey, y= CPUE))
zmarch + geom_bar(stat = "identity", aes(fill = anlyLS)) + 
  scale_fill_manual(values = c(mypal, "white", "green"), name = NULL) + 
  facet_grid(Site~Year, scales = "free_y") +
  #coord_cartesian(ylim = c(0, 16000)) +
  xlab("Sampling Month") + ylab("CPUE (count per cubic meter)")+
  theme(legend.position = "right")

zmarch + geom_bar(stat = "identity", aes(fill = anlyLS), position = "fill") + 
  scale_fill_manual(values = c(mypal, "white", "green"), name = NULL) + 
  facet_grid(Site~Year, scales = "free_y") +
  #coord_cartesian(ylim = c(0, 16000)) +
  xlab("Sampling Month") + ylab("CPUE (count per cubic meter)")+
  theme(legend.position = "right")


#april
zapril = ggplot(filter(zooLitave, Month == 4), aes(x=survey, y= CPUE))
zapril + geom_bar(stat = "identity", aes(fill = anlyLS)) + 
  scale_fill_manual(values = c(mypal, "white", "green"), name = NULL) + 
  facet_grid(Site~Year, scales = "free_y") +
  #  coord_cartesian(ylim = c(0, 16000)) +
  xlab("Sampling Month") + ylab("CPUE (count per cubic meter)")+
  theme(legend.position = "right")

#may
zmay = ggplot(filter(zooLitave, Month == 5), aes(x=survey, y= CPUE))
zmay + geom_bar(stat = "identity", aes(fill = anlyLS)) + 
  scale_fill_manual(values = c(mypal, "white", "green"), name = NULL) + 
  facet_grid(Site~Year, scales = "free_y") +
  #  coord_cartesian(ylim = c(0, 16000)) +
  xlab("Sampling Month") + ylab("CPUE (count per cubic meter)")+
  theme(legend.position = "right")

#june
zjune = ggplot(filter(zooLitave, Month == 6), aes(x=survey, y= CPUE))
zjune + geom_bar(stat = "identity", aes(fill = anlyLS)) + 
  scale_fill_manual(values = c(mypal, "white", "green"), name = NULL) + 
  facet_grid(Site~Year, scales = "free_y") +
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
Commat = pivot_wider(allzoops, names_from = anlyLS, values_from = CPUE, 
                     id_cols = c(SampleID, Site, Station, Date, Month, survey, Year, Ggdist),
                    values_fill = 0, values_fn = sum)
Envmat = Commat[,1:8]
test = Commat[,9:21]
test2 = test[,order(names(test))]

Commat2 = as.matrix(test2)

#relative abundance amtrix
Commatp = Commat2/rowSums(Commat2)

#make better names for adding to the NMDS plots
names(Commat2) =  c("Calanoida","Cal juv", "Cal naup", 
                   "Cladocera", "cop naup",  "Cumacea",
                   "Cyclopoda", "Cyclo juv", "Decapoda", "Harpacticoida",
                    "Ostracoda", "other", "Rotifera")
names(Commatp) = names(Commat2)


#PerMANOVA of abundance matrix survey and month
adonis(Commat2~Month + survey + Site, data = Envmat)


#Do it again with relative abundance
a1 = adonis(Commatp~Month + survey+ Site, data = Envmat)
a1
#looks pretty similar

#now some non=metric multidimentional scaling
n1 = metaMDS(Commat2, trymax = 300)
n2 = metaMDS(Commatp, trymax = 1000)
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
#there are definitely differences, but they are a bit of a mess
PlotNMDS(ng1, data = EnvmatGriz, group = "Month")
PlotNMDS(ng1, data = EnvmatGriz, group = "Year")


#################################################################
#Take a closer look at the copepods

#put them togetehr with finer=scale taxonomic resoltion
zoop20.3 = rename(zoop20.1, CommonName = FRP, Date = SampleDate) %>%
  mutate(survey = "20mm") %>%
  select(Date, CommonName, Site, survey, CPUE, Station, Month, Year, SampleID, Analy, lifestage)

zooLit2.2 = mutate(zooLit2.1, survey = "FRP", Year = year(Date)) %>%
  select(CommonName, CPUE, Date, Site, survey, Station, Month, Year, SampleID, Analy, lifestage)

finezoops = rbind(zoop20.3, zooLit2.2)
finezoops2 = left_join(stations20mm, finezoops)

cops = filter(finezoops2, Analy %in% c("cyclopoid", "calanoid", "copepod"))%>%
  filter(!is.na(Month), !is.na(Year), Site != "Webb Tract", Site != "Tule Red", Site != "Wings",
                  Site != "winter", Site != "Dow Wetlands", Site != "Browns", Site != "Ryer Island", Month != 7)



#Let's do it on the genus level
cops$CommonName[which(cops$CommonName == "Limnoithona tetraspina")] = "Limnoithona spp."
cops$CommonName[which(cops$CommonName == "Tortanus dextrilobatus")] = "Tortanus"
cops$CommonName[which(cops$CommonName == "Tortanus discaudatus")] = "Tortanus"
cops$CommonName[which(cops$CommonName == "Pseudodiaptomus forbesii")] = "Pseudodiaptomus spp."
cops$CommonName[which(cops$CommonName == "Pseudodiaptomus marinus")] = "Pseudodiaptomus spp."
cops$CommonName[which(cops$CommonName == "Oithona davisae")] = "Oithona"
cops$CommonName[which(cops$CommonName == "Oithona similis")] = "Oithona"


#Now a bar plot
c1 = ggplot(cops, aes(x=survey, y= CPUE))
c1 + geom_bar(stat = "identity", aes(fill = CommonName), position = "fill") + 
 # scale_fill_manual(values = c(mypal, "white", "green"), name = NULL) + 
  facet_grid(Site~Year+Month, scales = "free_y") +
  #  coord_cartesian(ylim = c(0, 16000)) +
  xlab("Sampling Month") + ylab("CPUE (count per cubic meter)")+
  theme(legend.position = "right")


#by life stage

c1 + geom_bar(stat = "identity", aes(fill = lifestage), position = "fill") + 
  # scale_fill_manual(values = c(mypal, "white", "green"), name = NULL) + 
  facet_grid(Site~Year+Month, scales = "free_y") +
  #  coord_cartesian(ylim = c(0, 16000)) +
  xlab("Sampling Month") + ylab("CPUE (count per cubic meter)")+
  theme(legend.position = "right")

#Try just looking at adults
copad = filter(cops, lifestage == "adult")
c2 = ggplot(copad, aes(x=survey, y= CPUE))
c2 + geom_bar(stat = "identity", aes(fill = CommonName), position = "fill") + 
   scale_fill_manual(values = c(mypal, "white", "green", "black"), name = NULL) + 
  facet_grid(Site~Year+Month, scales = "free_y") +
  #  coord_cartesian(ylim = c(0, 16000)) +
  xlab("Sampling Month") + ylab("CPUE (count per cubic meter)")+
  theme(legend.position = "right")

##############################################################################
#multivariate stats on adult copepods

#Create a community matrix
copmat = pivot_wider(copad, names_from = CommonName, values_from = CPUE, 
                     id_cols = c(SampleID, Site, Station, Date, Month, survey, Year, Ggdist),
                     values_fill = 0, values_fn = sum)
Envmat = copmat[,1:8]
test = copmat[,9:24]
test2 = test[,order(names(test))]

copmat2 = as.matrix(test2)

#relative abundance amtrix
copmatp = copmat2/rowSums(copmat2)


#PerMANOVA of abundance matrix survey and month
adonis(copmat2~Month + survey + Site, data = Envmat)


#Do it again with relative abundance
a1 = adonis(copmatp~Month + survey+ Ggdist, data = Envmat)
a1
#site is a much bigger difference, survey much smaller

#now some non=metric multidimentional scaling
n1 = metaMDS(copmat2, trymax = 300)
#yay!
n2 = metaMDS(copmatp, trymax = 2000)
#yay!

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

#I need to check and see if diaptomidae is actually in 20mm

#need to get the EMP data.
#Maybe I grab 20mm at EMP from Sam and integrate?