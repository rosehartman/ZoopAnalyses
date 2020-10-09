#LEt's start over with EMP and FRP integrated from the zooplankton synthesis app

library(zooper)
library(lubridate)
library(tidyverse)

#load data that I dowloded from the singy app
EMP20 = read.csv("EMP20mm.csv")

#Load stations that I want to use
stas = read.csv("stations.csv")

stations = rename(stas, FRP = FRPStation, `20mm` = Twentymil, EMP = "EMPzoop") %>%
  mutate(`20mm` = as.character(`20mm`), FMWT = NULL, EMPreg = NULL) %>%
  pivot_longer(cols = c(FRP, `20mm`, EMP), names_to = "survey", values_to = "Station") %>%
  distinct() %>%
  filter(Shallowdeep =="Y", !is.na(Station), Station != "")

#filter the data so I just have the stations I want

EMP20.1 = left_join(filter(stations, survey != "FRP"), EMP20, by = "Station")

#now upload the crosswalk I want to use
crosswalk <- read_excel("FRP_EMPcrosswalk.xlsx", 
                        sheet = "zooper")

#add analysis classes
EMP20.2 = mutate(crosswalk, Taxlifestage = paste(Taxname, Lifestage)) %>%
  select(Taxlifestage, Phylum, Class, Order, Analy, Analy2) %>%
  distinct()%>%
  right_join(EMP20.1)

EMP20.3 = select(EMP20.2, Site, survey, Station, SampleID, Date, CPUE, Lifestage, Taxname, Analy, Analy2)

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
View(zoopsfrp)

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
frptaxa = select(frptaxa,  FRP_Meso, Lifestage, Taxname, Analy, Analy2) %>%
  rename(CommonName = FRP_Meso) %>%
  filter(CommonName != "NA")

#attach the other categories to the main data set
zoop = merge(zoo, frptaxa)
zoop2 = left_join(stations, zoop) %>%
  filter(survey == "FRP") %>%
  select(Site, survey, Station, SampleID, Date, CPUE, Lifestage, Taxname, Analy, Analy2)


################ALL THE ZOOPS!!!###################
allzoops = rbind(zoop2, EMP20.3)

############################################################################################################
#now some exploritory plots

#quick plot of CPUE by location

#First calculate the average CPUE of each critter (analysis group/life stage) by location and month and year
allzoops = mutate(allzoops, Year = year(Date), 
                  Month = month(Date), analyLS = paste(Analy, Lifestage))



zoop20x= group_by(allzoops, SampleID, analyLS, Analy, Lifestage, Site, Month, survey, Year, Ggdist) %>% 
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

