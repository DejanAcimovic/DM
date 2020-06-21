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
setwd('C:/Users/Kenan/Desktop/DM Projekat')

#ucitavanje datasetova
flights <- read.csv("flights.csv")
airlines <- read.csv("airlines.csv")
airports <- read.csv("airports.csv")

#upoznavanje sa nazivima kolona
names(flights)
names(airlines)
names(airports)

#povezivanje datasetova
##prvo cemo povezati airlines i flights preko kolone koja se odnosi na airline kod
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
set <- data
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

#MISSING VALUES
data$DEPARTURE_DELAY[data$DEPARTURE_DELAY==""]<-NA
#broj missing value-a unuatr dataseta
print(sum(is.na(data$DEPARTURE_DELAY)))
print(sum(is.na(data$ARRIVAL_DELAY)))
#brisanje svih missing value-a u datasetu
data = data[complete.cases(data$DEPARTURE_DELAY), ]
data = data[complete.cases(data$ARRIVAL_DELAY), ]
#broj missing value-a nakon brisanja u datasetu radi provjere
print(sum(is.na(data$DEPARTURE_DELAY)))
print(sum(is.na(data$ARRIVAL_DELAY)))

#tehnike za ciscenje i transformaciju
##zbog velicine data dataseta ogranit cemo se samo na one letove koji imaju kasnjenje pri polasku manje od 60 minuta
##i koji imaju kasnjenje pri dolasku manje od 60 minuta
data <- data [which(data$DEPARTURE_DELAY < 60 & data$DEPARTURE_DELAY >0 & data$ARRIVAL_DELAY >0 & data$ARRIVAL_DELAY <60),]
dim(data)

#AMERA 
#pomoc (poslije se moze brisati)
data_pom <- data
set1_pom <- data
#AMERA

dim(data)
###########################################
##uklanjanje nepotrebnih kolona: "TAIL_NUMBER", "TAXI_OUT", "WHEELS_ON", "WHEELS_OFF","TAXI_IN", "FLIGHT_NUMBER"
data$TAIL_NUMBER <- NULL
data$WHEELS_ON <- NULL
data$WHEELS_OFF <- NULL
data$TAXI_IN <- NULL
data$FLIGHT_NUMBER <- NULL
data$TAXI_OUT <- NULL
names(data)
##dodavanje kolone LOSS
data <- mutate(data, LOSS = ARRIVAL_DELAY + DEPARTURE_DELAY)
names(data)
dim(data)

# ######################################
# #ZADATAK 2 
# ##########
# #FUNKCIJE ZA PROCJENU KLASIFIKATORA
# get_accuracy <- function(df, predicted, actual){
#   confusion_table = table(predicted, df[,actual])
#   TP = confusion_table[2,2]
#   TN = confusion_table[1,1]
#   FN = confusion_table[1,2]
#   FP = confusion_table[2,1]
#   accuracy = round((TP + TN) / sum(TP,FP,TN,FN), 2)
#   return(accuracy)
# }
# 
# get_classification_error_rate <- function(df, predicted, actual){
#   confusion_table = table(predicted, df[,actual])
#   TP = confusion_table[2,2]
#   TN = confusion_table[1,1]
#   FN = confusion_table[1,2]
#   FP = confusion_table[2,1]
#   classification_error_rate = round((FP + FN) / sum(TP,FP,TN,FN),2)
#   return(classification_error_rate)
# }
# 
# get_precision <- function(df, predicted, actual){
#   confusion_table = table(predicted, df[,actual])
#   TP = confusion_table[2,2]
#   TN = confusion_table[1,1]
#   FN = confusion_table[1,2]
#   FP = confusion_table[2,1]
#   precision = round(TP / (TP + FP), 2)
#   return(precision)
# }
# 
# get_sensitivity <- function(df, predicted, actual){
#   confusion_table = table(predicted, df[,actual])
#   TP = confusion_table[2,2]
#   TN = confusion_table[1,1]
#   FN = confusion_table[1,2]
#   FP = confusion_table[2,1]
#   sensitivity = round(TP / (TP + FN), 2)
#   return(sensitivity)
# }
# 
# get_specificity <- function(df, predicted, actual){
#   confusion_table = table(predicted, df[,actual])
#   TP = confusion_table[2,2]
#   TN = confusion_table[1,1]
#   FN = confusion_table[1,2]
#   FP = confusion_table[2,1]
#   specificity = round(TN / (TN + FP), 2)
#   return(specificity)
# }
# ###########
#KLASIFIKACIJA
library(tidyverse)
library(lubridate)

#Kreiramo varijablu koju cemo predvidjati, i koja ima vrijednost 2, ako je kasnjenje pri dolasku vece od 15 min
#u suprotnom vrijednost je 1
delay_A15 <- ''
data <- mutate(data , delay_A15)
data$delay_A15<- "1"
data$delay_A15[data$ARRIVAL_DELAY>15] <- "2"
data$delay_A15<- as.factor(data$delay_A15)

#AMERA
##pomoc (poslije se moze brisati)
#zato sto samo predvidjamo kasnjenje pri dolasku, a ne i pri polasku
delay_D15 <- ''
data <- mutate(data , delay_D15)
data$delay_D15<- "1"
data$delay_D15[data$DEPARTURE_DELAY>15] <- "2"
data$delay_D15<- as.factor(data$delay_D15)
#AMERA

glimpse(data)
str(data)
summary(data)



#AMERA
#pomoc (poslije se moze brisati)
pom1 <- data
#data <- pom1
#AMERA

# 
# ##izbaciti sve NA vrijednosti kako bi smanjili dataset, jer smo sa missing values izbacivali samo vezano za delay
# dim(data)
# data <- na.omit(data)
# dim(data)
# 
# #Zbog predugog izvrsavanja procesa treniranja uzeli smo prvih 3.000 podataka
# data <- data[1:3000,]
# dim(data)  
# 
# library(caret)
# 
# #Provjera da li neka od kolona jos uvijek ima NA vrijednosti
# colSums(is.na(data))
# dim(data)
# preprocesing_pom <- data
# #data <- preprocesing_pom
# 
# #START Preprocessing
# ##prilagodjavanje varijabli za proces treniranja
# set.seed(13)
# data$ORIGIN_CODE  <- as.factor(data$ORIGIN_CODE )
# data$DEST_CODE  <- as.factor(data$DEST_CODE )
# data$DEST_AIRPORT  <- NULL
# data$DEST_CITY <- NULL
# data$ORIGIN_AIRPORT <- NULL
# data$ORIGIN_CITY <- NULL
# data$ORIGIN_COUNTRY  <- NULL
# data$DEST_COUNTRY  <- NULL
# 
# data$ORIGIN_LONGITUDE <- as.integer(data$ORIGIN_LONGITUDE ) 
# data$ ORIGIN_LATITUDE <- as.integer(data$ ORIGIN_LATITUDE ) 
# data$DEST_LONGITUDE<- as.integer(data$DEST_LONGITUDE ) 
# data$DEST_LATITUDE  <- as.integer(data$DEST_LATITUDE  ) 
# #END Preprocessing
# 
# #START podjela na train i test
# spl <- sample(nrow(data), 0.7*nrow(data))
# train <- data[spl, ]
# test <- data[-spl, ]
# dim(train)
# str(train)
# #END podjela na train i test
# 
# #CORSS_START
# #GLM
# log_reg_mod <- train(delay_A15 ~ ., data = train, method = "glm", family = "binomial", trControl=trainControl(method = "cv", number = 5, repeats = 5), tuneGrid=expand.grid(parameter=c(0.001, 0.01, 0.1, 1,10,100, 1000)))
# pred.step <- predict(log_reg_mod, newdata = test)
# glm_confusionMatrix<-confusionMatrix(pred.step, test[, "delay_A15"])
# table(pred.step, test[, "delay_A15"])
# 
# get_accuracy(test, pred.step, "delay_A15")
# get_classification_error_rate(test, pred.step, "delay_A15")
# get_precision(test, pred.step, "delay_A15")
# get_sensitivity(test, pred.step, "delay_A15")
# get_specificity(test, pred.step, "delay_A15")
# 
# #NAIVE BAYES
# library(e1071)
# naivebayes <- naiveBayes(delay_A15~ . , data = train, trControl=trainControl(method = "cv", number = 5, repeats = 5))
# test<- na.omit(test)
# val <- predict(naivebayes , newdata=test)
# conf <- confusionMatrix(val, test[,"delay_A15"])
# conf
# get_accuracy(test, val, "delay_A15")
# get_classification_error_rate(test,  val, "delay_A15")
# get_precision(test, val, "delay_A15")
# get_sensitivity(test,  val, "delay_A15")
# get_specificity(test,  val, "delay_A15")
# 
# #RANDOM FOREST
# #install.packages('ROSE')
# library(ROSE)
# data.rose <- ROSE(delay_A15 ~ ., data = train, seed = 1)$data
# table(data.rose$delay_A15)
# data.rose_test <- ROSE(delay_A15 ~ ., data = test, seed = 1)$data
# table(data.rose_test$delay_A15)
# 
# random_forest <- train(delay_A15~ . , data = data.rose, method = "ranger", trControl=trainControl(method = "cv", number = 5, repeats = 5))
# random_forest_validation <- predict(random_forest, newdata=data.rose_test)
# confusion_matrix_rf <- confusionMatrix(random_forest_validation, test[,"delay_A15"])
# confusion_matrix_rf
# confusion_matrix_rf2 <- confusionMatrix(random_forest_validation, data.rose_test[,"delay_A15"])
# confusion_matrix_rf2
# 
# get_accuracy(test, random_forest_validation, "delay_A15")
# get_classification_error_rate(test,  random_forest_validation, "delay_A15")
# get_precision(test, random_forest_validation, "delay_A15")
# get_sensitivity(test,  random_forest_validation, "delay_A15")
# get_specificity(test,  random_forest_validation, "delay_A15")
# 
# #CROSS_END

#ZADATAK 3
##
#install.packages('factoextra')
library(factoextra)
library(cluster)
library(NbClust)

#install.packages("magrittr")
library("magrittr")

data <- pom1
data <- na.omit(data)
data <- data[1:50000,]

#funkcija za tendenciju zahtjeva samo numericke vrijednosti, s brisemo kategoricke varijable

new <- data
new$AIRLINE_CODE <- NULL
new$ORIGIN_CODE  <- NULL
new$DEST_CODE  <- NULL
new$CANCELLATION_REASON <- NULL
new$AIRLINE  <- NULL
new$DEST_AIRPORT <- NULL
new$DEST_CITY <- NULL
new$DEST_STATE <- NULL
new$DEST_COUNTRY <- NULL
new$ORIGIN_CITY <- NULL
new$ORIGIN_AIRPORT <- NULL
new$ORIGIN_STATE<- NULL
new$ORIGIN_COUNTRY<- NULL
new$delay_D15<- NULL
new$delay_A15<- NULL
str(new)

#Od numerickih vrijednosti biramo one na osnovu kojih cemo vrsiti klastering
#neke od varijable imaju vrijednost 0, recimo da li je let otkazan, otkazan zbog vremena i sl. Tako da biramo samo najrelevantnije
new1 <- new [, -c(1,16,15,18,21)]
str(new1)

new5 <- scale(new1)
str(new5)
dim(new5)
new5 <- new5[1:1000,]
class(new5)
get_clust_tendency(new5, n = 40, gradient = gradient_col)

#koristimo PAM I GAP statistiku
pam1 <- function(x,k) list(cluster = pam(x,k, cluster.only=TRUE))
gap_stat5 <- clusGap(new5, FUN = pam1, K.max = 5, B = 100) 
fviz_gap_stat(gap_stat5)
#dobili smo da je optimalno k=4
set.seed(123)
km.res <- kmeans(new5, 4, nstart = 25)
head(km.res$cluster, 20)

fviz_cluster(km.res, data = new5,
             ellipse.type = "convex",
             palette = "jco",
             ggtheme = theme_minimal())

# Compute hierarchical clustering
res.hc <- new5 %>%
  scale() %>%                    # Scale the data
  dist(method = "euclidean") %>% # Compute dissimilarity matrix
  hclust(method = "ward.D2")     # Compute hierachical clustering

# Visualize using factoextra
# Cut in 4 groups and color by groups
fviz_dend(res.hc, k = 4, # Cut in four groups
          cex = 0.5, # label size
          k_colors = c("#2E9FDF", "#00AFBB", "#E7B800", "#FC4E07"),
          color_labels_by_k = TRUE, # color labels by groups
          rect = TRUE # Add rectangle around groups
)

