#Let's take another look at the FRP data that was taken concurrently with 20mm

library(tidyverse)
library(vegan)
library(readxl)
library(lubridate)
library(zooper)

#vrsion of the combined FRP, EMP, 20mil data from teh shiny app
FRP20mil = read.csv("FRP20mm.csv")
FRP20mil = mutate(FRP20mil, Date = as.Date(Date))

FRP20 = filter(FRP20mil, Source == "FRP")

#But does FRP have some updates?
FRP = read_xlsx("zoopsexportqry.xlsx")
max(FRP20$Date)
#"2018-06-06"
 max(FRP$Date)
 
 FRP2 = rename(FRP, FRP_Meso = "CommonName") %>%
   left_join(crosswalk)
#[1] "2018-12-13 UTC"
 
 #no, we should be good for the 20mm comparison
 
FRP20milsp = mutate(FRP20mil, Month = month(Date)) %>%
                      filter(Year %in% c(2016, 2017, 2018), 
                             Month %in% c(3,4,5,6))

#line up which stations we want to analyze
stations = read.csv("stations.csv")

#attach the stations to the master data set and filter by ones that
#we want to analyze for this thingy

x20mil = mutate(stations, Station = as.character(Twentymil)) %>%
  select(Site, Station) %>%
  inner_join(FRP20milsp)

xFRP = mutate(stations, Station = as.character(FRPStation)) %>%
  select(Site, Station) %>%
  inner_join(FRP20milsp)

xEMP = mutate(stations, Station = as.character(EMPzoop)) %>%
  select(Site, Station) %>%
  inner_join(FRP20milsp)

#Put them all together with the new "Site" designation
FRP20EMP = rbind(x20mil, xFRP, xEMP)

#plot it
#Plot it
ggplot(FRP20EMP, aes(x=Month, y = CPUE, fill = Taxname)) + 
  geom_bar(stat = "identity") +
  facet_grid(Site~Year+Source)


#Total catch
F2Etot = group_by(FRP20EMP, Site, Month, Year, SampleID, Source) %>%
  summarize(totCPUE = sum(CPUE)) %>%
  group_by(Month, Year, Site, Source) %>%
  summarize(aveCPUE = mean(totCPUE), n = length(unique(SampleID)))

#Plot it
ggplot(F2Etot, aes(x=Month, y = n, fill = Source)) + 
         geom_bar(stat = "identity", position = "dodge") +
         facet_grid(Site~Year)
