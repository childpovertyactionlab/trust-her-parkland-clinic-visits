library(tidyverse)
library(tidycensus)
library(sf)
library(readxl)
library(rio)
library(tidyr)
library(highcharter)
library(leaflet)
library(reshape2)
library(lubridate)
library(purrr)
library(readr)

libDB <- "/Users/anushachowdhury/CPAL Dropbox"
parklandDir <- "/Maternal Health/02_Data/Parkland Family Planning/reexternalreparklanddatareport"
parklandFiles <- list.files(path = paste0(libDB, parklandDir), pattern = "*.xls", full.names = TRUE)

##### age #####
ageList <- list()

for (file in parklandFiles) {
  ageList[[file]] <- import(file, which = 1) %>%
    mutate(date = file) %>%
    janitor::clean_names(.)
}

age<-do.call(rbind, ageList)%>%
  mutate(date = substr(date, nchar(date) - 24, nchar(date)),
         quarter = substr(date, 6, 6),
         quarter = paste("Quarter", quarter, sep = " "),
         date = as.Date(substr(date, 8, 13), format = "%m%d%y"),
         year = lubridate::year(date))
write_csv(age, "/Users/anushachowdhury/CPAL Dropbox/Maternal Health/04_Projects/Parkland Family Planning Report/data/age.csv")

##### race #####

raceList <- list()

for (file in parklandFiles) {
  raceList[[file]] <- import(file, which = 2) %>%
    mutate(date = file) %>%
    janitor::clean_names(.)
}

race<-do.call(rbind, raceList)%>%
  mutate(date = substr(date, nchar(date) - 24, nchar(date)),
         quarter = substr(date, 1, 6),
         date = as.Date(substr(date, 8, 13), format = "%m%d%y"),
         year = lubridate::year(date))
write_csv(race, "/Users/anushachowdhury/CPAL Dropbox/Maternal Health/04_Projects/Parkland Family Planning Report/data/race.csv")

##### zip #####

zipList <- list()

for (file in parklandFiles) {
  zipList[[file]] <- import(file, which = 3) %>%
    mutate(date = file) %>%
    janitor::clean_names(.)
}

zip <-do.call(rbind, zipList) %>%
  mutate(date = substr(date, nchar(date) - 24, nchar(date)),
         quarter = substr(date, 1, 6),
         date = as.Date(substr(date, 8, 13), format = "%m%d%y"),
         year = lubridate::year(date))
write_csv(zip, "/Users/anushachowdhury/CPAL Dropbox/Maternal Health/04_Projects/Parkland Family Planning Report/data/zip.csv")

##### payor #####
payorList <- list()

for (file in parklandFiles) {
  payorList[[file]] <- import(file, which = 4) %>%
    mutate(date = file) %>%
    janitor::clean_names(.)
}

payor <-map(payorList, ~ .x %>%
              pivot_longer(cols = starts_with(c('jan','feb', 'march','apr', 'may','june','jul', 'aug', 'sept','oct', 'nov', 'dec')),
                           names_to = 'Month',
                           values_drop_na = TRUE))%>%
  bind_rows()%>%
  mutate(date = substr(date, nchar(date) - 24, nchar(date)),
         quarter = substr(date, 1, 6),
         date = as.Date(substr(date, 8, 13), format = "%m%d%y"),
         year = lubridate::year(date),
         Month=ifelse(Month=='sept', 'sep', Month),
         newDate = as.Date(paste0(year(date), "-", Month, "-01"), format = "%Y-%b-%d"))
write_csv(payor, "/Users/anushachowdhury/CPAL Dropbox/Maternal Health/04_Projects/Parkland Family Planning Report/data/payor.csv")

##### method #####
methodList <- list()
for (file in parklandFiles) {
  methodList[[file]] <- import(file, which = 5) %>%
    mutate(date = file) %>%
    janitor::clean_names(.)
}

method <-map(methodList, ~ .x %>%
               pivot_longer(c("x15", "x15_17", "x18_19", "x20_24", "x25_29", "x30_34", "x35_39", "x40_44", "x45"),
                            names_to = 'ageCategory',
                            values_drop_na = TRUE))%>%
  bind_rows()%>%
  mutate(date = substr(date, nchar(date) - 24, nchar(date)),
         quarter = substr(date, 1, 6),
         date = as.Date(substr(date, 8, 13), format = "%m%d%y"),
         year = lubridate::year(date),
         ageCategory= ifelse(ageCategory == "x15", "Under 15", 
                             ifelse(ageCategory =='x15_17', '15-17', 
                                    ifelse(ageCategory =='x18_19', '18-19', 
                                           ifelse(ageCategory =='x20_24','20-24', 
                                                  ifelse(ageCategory =='x25_29', '25-29',
                                                         ifelse(ageCategory =='x30_34', '30-34', 
                                                                ifelse(ageCategory =='x35_39', '35-39',
                                                                       ifelse(ageCategory =='x40_44', '40-44',
                                                                              ifelse(ageCategory =='x45', '45+', ageCategory))))))))))
write_csv(method, "/Users/anushachowdhury/CPAL Dropbox/Maternal Health/04_Projects/Parkland Family Planning Report/data/method.csv")

##### Title X #####
XList<- list()

for (file in parklandFiles) {
  XList[[file]] <- import(file, which = 6) %>%
    mutate(date = file) %>%
    janitor::clean_names(.)
}

titleX <-map(XList, ~ .x %>%
               pivot_longer(cols = starts_with(c('jan','feb', 'march','apr', 'may','june','jul', 'aug', 'sept','oct', 'nov', 'dec')),
                            names_to = 'Month',
                            values_drop_na = TRUE))%>%
  bind_rows()%>%
  mutate(date = substr(date, nchar(date) - 24, nchar(date)),
         quarter = substr(date, 1, 6),
         date = as.Date(substr(date, 8, 13), format = "%m%d%y"),
         year = lubridate::year(date),
         Month=ifelse(Month=='sept', 'sep', Month),
         newDate = as.Date(paste0(year(date), "-", Month, "-01"), format = "%Y-%b-%d"))
write_csv(titleX, "/Users/anushachowdhury/CPAL Dropbox/Maternal Health/04_Projects/Parkland Family Planning Report/data/titleX.csv")

##### LARC #####
LARCList <- list()

for (file in parklandFiles) {
  LARCList[[file]] <- import(file, which = 7) %>%
    mutate(date = file) %>%
    janitor::clean_names(.)
}

larc<-do.call(rbind, LARCList)%>%
  mutate(date = substr(date, nchar(date) - 24, nchar(date)),
         quarter = substr(date, 6, 6),
         quarter = paste("Quarter", quarter, sep = " "),
         date = as.Date(substr(date, 8, 13), format = "%m%d%y"),
         year = lubridate::year(date))
write_csv(larc, '/Users/anushachowdhury/CPAL Dropbox/Maternal Health/04_Projects/Parkland Family Planning Report/data/larc.csv')





