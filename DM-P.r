library(tm) 
library(stringi) 
library(proxy) 
library(FactoMineR) 
library(rvest)
library(RCurl)
library(data.table)

library('dplyr')
library('assertthat')
#postavljanje na direktorij u kojem se nalaze dataset-ovi
setwd('C:/Users/Armin/Desktop/DMgithub')

#ucitavanje datasetova
flights <- read.csv("flights.csv")
airlines <- read.csv("airlines.csv")
airports <- read.csv("airports.csv")

#upoznavanje sa nazivima kolona
names(flights)
names(airlines)
names(airports)

#povezivanje datasetova
##prvo æemo povezati airlines i flights preko kolone koja se odnosi na airline kod
## ali prvo moramo namjestiti da se kolone isto zovu kako bi ih povezali
colnames(flights)[5] <- "AIRLINE_CODE"
colnames(airlines)[1] <- "AIRLINE_CODE"

##povezivanje datasetova
airline_flights <- merge(flights,airlines,by = c("AIRLINE_CODE"))

##povezivanje novokreiranog dataseta airline_flights sa datasetom airports
##pri cemu povezujemo prvo preko odredista, a zatim preko polazista
## i ovdje prvo moramo namjestiti da kolone imaju isti naziv 
##tako sto cemo prvo postaviti da se prva kolona iz airporta naziva "DEST_CODE"

colnames(airports) <- c("DEST_CODE","DEST_AIRPORT", "DEST_CITY", "DEST_STATE", "DEST_COUNTRY", "DEST_LATITUDE", "DEST_LONGITUDE" )
colnames(airline_flights)[9] <- "DEST_CODE" 
dest_merge <- full_join(airline_flights,airports,by = "DEST_CODE")

## zatim izvrsiti povezivanje po toj koloni
## a zatim preimenovati u "ORIGIN_CODE"
## pa izvrsiti povezivanje po toj koloni

colnames(airports) <- c("ORIGIN_CODE","ORIGIN_AIRPORT", "ORIGIN_CITY", "ORIGIN_STATE", "ORIGIN_COUNTRY", "ORIGIN_LATITUDE", "ORIGIN_LONGITUDE" )
colnames(dest_merge)[8] <- "ORIGIN_CODE"
origin_merge <- full_join(dest_merge,airports,by = "ORIGIN_CODE")

#konacni dataset 
data <- origin_merge

#############
#analiza
dim(data)
names(data)

#metode vizualizacije
summary(data$ARRIVAL_DELAY) 
hist(data$ARRIVAL_DELAY,breaks = 300, xlim = c(-80,100))
hist(data$ARRIVAL_DELAY,breaks = 300, xlim = c(-10,100))
#plot(x = data$ARRIVAL_DELAY,y = data$DEPARTURE_DELAY, xlab = "ARRIVAL_DELAY",ylab = "DEPARTURE_DELAY",xlim = c(-10,10),ylim = c(-10,10),		 main = "ARRIVAL_DELAY vs DEPARTURE_DELAY")
summary(data$DEPARTURE_DELAY)
hist(data$DEPARTURE_DELAY,breaks = 300, xlim = c(-80,100))
hist(data$DEPARTURE_DELAY,breaks = 300, xlim = c(-10,100))

#deskriptivna statistika
summary(data)
glimpse(data) 

data$DEPARTURE_DELAY[data$DEPARTURE_DELAY==""]<-NA

#broj missing value-a unuatr dataseta
print(sum(is.na(data$DEPARTURE_DELAY)))
#brisanje svih missing value-a u datasetu
data = data[complete.cases(data), ]
#broj missing value-a nakon brisanja u datasetu radi provjere
print(sum(is.na(data$DEPARTURE_DELAY)))

#tehnike za èisæenje i transformaciju
##zbog velièine data dataseta ogranit æemo se samo na one letove koji imaju kašnjenje veæe od 60 minuta
data <- data [which(data$ARRIVAL_DELAY > 60),]
dim(data)
##uklanjanje nepotrebnih kolona: "TAIL_NUMBER", "TAXI_OUT", "WHEELS_ON", "WHEELS_OFF","TAXI_IN", "FLIGHT_NUMBER"
data$TAIL_NUMBER <- NULL
data$WHEELS_ON <- NULL
data$WHEELS_OFF <- NULL
data$TAXI_IN <- NULL
data$FLIGHT_NUMBER <- NULL
data$TAXI_OUT <- NULL
names(data)
##dodavanje kolone LOSS
data <- mutate(data, LOSS = ARRIVAL_DELAY - DEPARTURE_DELAY)
names(data)
