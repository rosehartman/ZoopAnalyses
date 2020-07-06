#Let's look at some zooplankton data!!! YAYA!!!!

#I'll start with EMP, 20mm, townet, and FMWT from 2000-2017
#There really isn't enough FRP data to make it worth including

library(tidyverse)
library(vegan)
library(lubridate)
library(readr)
library(MASS)
library(visreg)
library(RColorBrewer)

#I suppose I should have used the zooper package to download the data by hand,
#but I just used the shiny app 'cause it's easier.

allzoop <- read_csv("data-_V0.3.0_2020-06-03.csv", 
                                   col_types = cols(BottomDepth = col_number(), 
                                                    DO = col_number(), Microcystis = col_character(), 
                                                    Turbidity = col_number(), pH = col_number(),
                                                    Datetime = col_datetime()))
#some quick exploritory plots
allzoop$month = month(allzoop$Date)

p1 = ggplot(allzoop, aes(x = month, y = CPUE))
p1+  geom_bar(aes(fill = Order), stat = "identity")+
  facet_grid(Source~Year)


#averages by month and species
monthzoop = group_by(allzoop, month, Year, Source, Taxname, Class, Order, Family) %>%
  summarize(CPUE = mean(CPUE))

#total CPUE by month
zoopTots = group_by(allzoop, month, Year, Source, SampleID, Station, Volume) %>%
  summarize(CPUE = sum(CPUE))
monthtotave = group_by(zoopTots, month, Year, Source) %>%
  summarize(CPUE = mean(CPUE))

p2 = ggplot(monthtotave, aes(x= month, y = CPUE))
p2 + geom_bar(stat = "identity")+
  facet_grid(Source~Year)

#Why does 20mm seem to have consistantly higher CPUE?
#Also, what was happening in summer of 2017 for EMP?

p3 = ggplot(filter(allzoop, Year == 2017, Source == "EMP"), aes(x = month, y = CPUE))
p3 + geom_bar(stat = "identity", aes(fill = Taxname)) + facet_wrap(~Station)

#Is there something wrong with EMP in July of 2017? Some aweful high CPUE values.

test = filter(allzoop, Year == 2017, Source == "EMP", month == 7)
summary(test)
test2 = group_by(test, SampleID, Station, Volume) %>% 
  summarize(CPUE = sum(CPUE))

hist(test2$Volume)
hist(test2$CPUE)
hist(zoopTots$Volume)
hist(zoopTots$CPUE)
ggplot(test2, aes(x=CPUE, y = Volume)) + geom_point()
ggplot(zoopTots, aes(x=log(CPUE), y = Volume)) + geom_point()

hist(log(zoopTots$CPUE))
#well, that's a beautiful bell curve

#Let's try something:
m1 = glm.nb(round(CPUE) ~ Source + month, data = zoopTots)
summary(m1)
visreg(m1)

m2 = glm(round(CPUE) ~ Source + month, data = zoopTots, family = "poisson")
summary(m2)

zoopTots$month2 = as.factor(zoopTots$month)
m3 = glm(log(CPUE) ~ Source*month2, data = zoopTots)
summary(m3)
visreg(m3)
visreg(m3, xvar = "Source", by = "month2")

###################################################################################
#Because EMP is the only data source in the winter, and they are much
#more spatially restricted, I might have to cut out the winter, or at least Jan and FEb.

#I'll also want to come up with some regional definitions.
#maybe seasonal definitions too.
taxa = read.csv("Taxa.csv", stringsAsFactors = F)

seasons = data.frame(month = 1:12, 
                     season = c("winter", "winter", 
                                "spring", "spring", "spring",
                                "summer", "summer", "summer",
                                "fall", "fall", "fall", "fall"))

allzoop2 = merge(allzoop, seasons) %>%
filter(month >2) %>%
  mutate(region = NA)

#add my regions

allzoop2$region[which(allzoop2$Latitude < 38.084  & allzoop2$Longitude < -122.194)] = "SBP"
allzoop2$region[which(allzoop2$Latitude >= 38.084  & allzoop2$Longitude < -122.194)] = "Napa"
allzoop2$region[which(allzoop2$Longitude >= -122.194 & allzoop2$Longitude <= -121.875)] = "Suisun"
allzoop2$region[which(allzoop2$Longitude < -121.699 & allzoop2$Longitude > -121.875 &
                        allzoop2$Latitude < 38.127 & allzoop2$Latitude > 37.996)] = "Confluence"
allzoop2$region[which(allzoop2$Longitude >= -121.699 &
                        allzoop2$Latitude < 38.127 )] = "SCDelta"
allzoop2$region[which(allzoop2$Longitude > -121.807 &
                        allzoop2$Latitude > 38.127 )] = "Cache"

#I'm just going to get rid of all the data with "NA" for the lats and longs
allzoop2 = filter(allzoop2, !is.na(Latitude)) %>%
  left_join(taxa, by = "Taxname")


#averages by season and species
szoop2 = group_by(allzoop2, season, Year, Tax2, Tax3, region) %>%
  summarize(CPUE = mean(CPUE), sal = mean(SalSurf, na.rm = T))

ps1 = ggplot(szoop2, aes(x= Year, y = CPUE))
ps1 + geom_bar(stat = "identity",  aes(fill = Tax2))+
  facet_grid(region~season)

mypal = c(brewer.pal(9,"Set1"), brewer.pal(8,"Set2"), brewer.pal(8, "Dark2"))
ps1 + geom_bar(stat = "identity",  aes(fill = Tax2), position = "fill")+
  facet_grid(region~season) +
  scale_fill_manual(values = mypal)

###########################################################################
#Order by salinity - (within a season)

szoop2 = arrange(szoop2, season, sal)

psal = ggplot(szoop2, aes(x = as.factor(sal), y = CPUE, fill = Tax2)) + 
  geom_bar(stat = "identity", position = "fill")
psal + facet_wrap(~season, scales = "free_x")

szoop2 = mutate(szoop2, salbin = cut(sal, c(0,1,2,3,4,5,6,7,10,12,15, 17, 18, 20, 25, 30, 35)))

psal = ggplot(szoop2, aes(x = salbin, y = CPUE, fill = Tax2)) + 
  geom_bar(stat = "identity", position = "fill")
psal + facet_wrap(~season, scales = "free_x")

szoopsum = group_by(szoop2, season, Tax2, Tax3, salbin, region) %>%
  summarize(CPUE = mean(CPUE))

psal2 = ggplot(szoopsum, aes(x = salbin, y = CPUE, fill = Tax2)) + 
  geom_bar(stat = "identity", position = "fill")
psal2 + facet_grid(region~season, scales = "free_x")

#########################################################################

#set up community matrix

allzooptax = group_by(allzoop2, SampleID, Tax2) %>%
  summarize(CPUE = sum(CPUE))
allzoopCom = pivot_wider(allzooptax, id_cols = SampleID, names_from = Tax2, 
                         values_from = CPUE) %>%
  ungroup()
allzoopCom = dplyr::select(as_tibble(allzoopCom), -starts_with("Sample"))

envmat = group_by(allzoop2, SampleID, month, Volume, Year, Date, SalSurf, Latitude, 
                  Longitude, Tide, Station, Chl, 
                  Secchi, Temperature, BottomDepth, Turbidity, 
                  Microcystis, pH, DO, season, region) %>%
  summarize(tot = sum(CPUE)) %>%
  droplevels() %>%
  ungroup()

#let's see what a PERMANOVA will get us
#a1 = adonis(allzoopCom~ region + season + Year, data = envmat)
#"Error: cannot allocate vector of size 1.4 Gb"

allzooptax2 = group_by(allzoop2, SampleID, month, Volume, Year, Date, SalSurf, Latitude, 
                      Longitude, Tide, Station, Chl, 
                      Secchi, Temperature, BottomDepth, Turbidity, 
                      Microcystis, pH, DO, season, region, Tax2) %>%
  summarize(CPUE = sum(CPUE))
  
allzoopmon = group_by(allzooptax2, month, Year, region, season, Tax2) %>%
  summarize(CPUE = mean(CPUE), sal = mean(SalSurf, na.rm = T)) %>%
  droplevels()

zoopmonCom = pivot_wider(allzoopmon, 
                         names_from = Tax2, 
                         values_from = CPUE) %>%
  ungroup()

zoopmonCom1 = dplyr::select(zoopmonCom, -month, -Year, -region, -season, -sal)
zoopmonenv = dplyr::select(zoopmonCom, month, Year, region, season, sal) %>%
  mutate(region = as.factor(region))

a2 = adonis(zoopmonCom1~region + season, data = zoopmonenv)
a2

NMDS1 = metaMDS(zoopmonCom1)
source("plotNMDS.R")
PlotNMDS(NMDS1, zoopmonenv, group = "region")
PlotNMDS(NMDS1, zoopmonenv, group = "season")

plot(NMDS1, type="n", shrink = T, cex=1)
points(NMDS1, display = "sites", col = "blue")

with(zoopmonenv, ordisurf(NMDS1~sal, labcex = 1, lwd.cl = 2,
                          main = "Community NDMS with isohalines", add = T))
text(NMDS1, dis="species", cex=.8) 
