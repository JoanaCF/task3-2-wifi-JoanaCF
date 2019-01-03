#### General description ####
# Evaluate Techniques for Wifi Locationing
# Joana
# 03/11 - 21/11 & 02/11

#### Goal #### 
# Our client would like us to investigate the feasibility of using "wifi fingerprinting" to determine a person's location in indoor spaces.

#### A. Clear environment ####
rm(list=ls())

#### B. Dataset ####
######## B.1 Upload datset - training and dataset #### 
library(readr)
trainingData <- read_csv("Desktop/UBIQUM/2. Tasks/Course 3/Task 3/Data/UJIndoorLoc/trainingData.csv", na = "NA")
validationData <- read_csv("Desktop/UBIQUM/2. Tasks/Course 3/Task 3/Data/UJIndoorLoc/validationData.csv", na = "NA")

######## B.2 Description of the dataset ####
# UJIIndoorLoc database
# http://archive.ics.uci.edu/ml/datasets/UJIIndoorLoc
# The UJIIndoorLoc database covers three buildings of Universitat Jaume I with 4 or more floors and almost 110.000m2. 
# It can be used for classification, e.g. actual building and floor identification, or regression, e.g. actual longitude and latitude estimation. 
# It was created in 2013 by means of more than 20 different users and 25 Android devices. 
# The database consists of 19937 training/reference records (trainingData.csv file) and 1111 validation/test records (validationData.csv file). 

# Each WiFi fingerprint can be characterized by the detected Wireless Access Points (WAPs) and the corresponding Received Signal Strength Intensity (RSSI). 
# The intensity values are represented as negative integer values ranging -104dBm (extremely poor signal) to 0dbM. 
# The positive value 100 is used to denote when a WAP was not detected. 
# During the database creation, 520 different WAPs were detected. Thus, the WiFi fingerprint is composed by 520 intensity values. 

## attributes
# Attribute 520 (WAP520): Intensity value for WAP520. Negative integer values from -104 to 0 and +100. Positive Vvalue 100 used if WAP520 was not detected. 
# Attribute 521 (Longitude): Longitude. Negative real values from -7695.9387549299299000 to -7299.786516730871000 
# Attribute 522 (Latitude): Latitude. Positive real values from 4864745.7450159714 to 4865017.3646842018. 
# Attribute 523 (Floor): Altitude in floors inside the building. Integer values from 0 to 4. 
# Attribute 524 (BuildingID): ID to identify the building. Measures were taken in three different buildings. Categorical integer values from 0 to 2. 
# Attribute 525 (SpaceID): Internal ID number to identify the Space (office, corridor, classroom) where the capture was taken. Categorical integer values. 
# Attribute 526 (RelativePosition): Relative position with respect to the Space (1 - Inside, 2 - Outside in Front of the door). Categorical integer values. 
# Attribute 527 (UserID): User identifier (see below). Categorical integer values. 
# Attribute 528 (PhoneID): Android device identifier (see below). Categorical integer values. 
# Attribute 529 (Timestamp): UNIX Time when the capture was taken. Integer value. 

#### C. Explore dataset #### 
#head(trainingData)
#head(validationData)
#sum(is.na(trainingData))
#sum(is.na(validationData))
#str(trainingData)
#str(trainingData, list.len= nrow(trainingData))
#sum(names(trainingData)==names(validationData))
### C: Both datasets have 529 variables and all of them are the same 
### C: no NAs in neither 

######## $ TIMESTAMP ####
### training 
library(ggplot2)
#qplot(trainingData$TIMESTAMP)
### C: 7 levels / dates
#sum(is.na(trainingData$TIMESTAMP))

#trainingData$TIMESTAMP_dt<-as.POSIXlt(as.numeric(trainingData$TIMESTAMP),origin="1970-01-01",tz="GMT")
#head(trainingData$TIMESTAMP_dt)
#str(trainingData$TIMESTAMP_dt)
#summary(trainingData$TIMESTAMP_dt)
### C: assuming an origin of 01/01/2013 - from 30/05 to 20/06

### validation 
#qplot(validationData$TIMESTAMP)
#validationData$TIMESTAMP_dt <- as.POSIXlt(as.numeric(validationData$TIMESTAMP),origin="1970-01-01",tz="GMT")
### C: quite different distribution - 8 levels
#qplot(validationData$TIMESTAMP_dt)
#summary(validationData$TIMESTAMP_dt)
### C: assuming an origin of 01/01/2013 - from 19/09 to 10/08


######## $ PHONEID ####
### training
library(highcharter)
#hchart(trainingData$PHONEID)
#hchart(as.factor(trainingData$PHONEID))
#str(as.factor(trainingData$PHONEID))
### C: most common: 13 HTC Wildfire S 2.3.5 0,11 14 & LT22i 4.0.4 0,1,9,16 - more tha 4.500 observations. Less common (<200) is 16 LT26i 4.0.4 3 (phone id - 13 & 14)
### C: some code ids missing: 0, 2, 4, 5, 9, 12, 15, 20, 21 / only 16 levels
trainingData$PHONEID<-as.factor(trainingData$PHONEID)


### validation 
validationData$PHONEID<-as.factor(validationData$PHONEID)
#hchart(validationData$PHONEID)
#str(validationData$PHONEID)
### C: highest frequency - HTC Wildfire S 2.3.5 0,11 (phone id - 13)
### C: some code ids missing: 1, 3, 6, 7, 8, 10, 11, 16, 17, 18, 19, 22, 23, 24 /only 11 levels - different ones than the testing set 


######## $ USERID ####
### training 
#hchart(trainingData$USERID)
trainingData$USERID<-as.factor(trainingData$USERID)
#hchart(trainingData$USERID)
### C: higheest frequency : userid 1 - USER0001 170 and userid 11 - USER0011 176 
#str(trainingData$USERID)
### C: 18 levels - no userid 0

### validation
#hchart(validationData$USERID)
#summary(validationData$USERID)
### C: pilot was run with userid 0 - USER0000 (Validation User) 
validationData$USERID<-as.factor(validationData$USERID)
#str(validationData$USERID)
### C: CAREFUL using this variable for classification / prediction 


######## $ RELATIVEPOSITION ####
### training 
#str(trainingData$RELATIVEPOSITION)
### C: 3329 inside and 16608 outside - why is the outside useful?
#trainingData$RELATIVEPOSITION<-as.factor(trainingData$RELATIVEPOSITION)
trainingData$RELATIVEPOSITION <- factor(trainingData$RELATIVEPOSITION,
                                        levels = c(1,2),
                                        labels = c("inside", "outside"))
#hchart(trainingData$RELATIVEPOSITION)

### validation 
#summary(validationData$RELATIVEPOSITION)
#head(validationData$RELATIVEPOSITION)
#hchart(validationData$RELATIVEPOSITION)
### C: No information on this in validation set  


######## $ SPACEID ####
### training
#summary(trainingData$SPACEID)
#str(trainingData$SPACEID)
#hchart(trainingData$SPACEID)
trainingData$SPACEID<-as.factor(trainingData$SPACEID)
hchart(trainingData$SPACEID)
summary(trainingData$SPACEID)
### C: 123 levels - highest frequency: 101- 104; 106 - 108; 201- 202

### validation
#hchart(validationData$SPACEID)
validationData$SPACEID <- as.factor(validationData$SPACEID)
#summary(validationData$SPACEID)
### C: No information on this in validation set  


######## $ BUILDINGID ####
## training 
#hchart(trainingData$BUILDINGID)
trainingData$BUILDINGID<-as.factor(trainingData$BUILDINGID)
trainingData$BUILDINGID <- factor(trainingData$BUILDINGID, levels= c(0,1,2), labels= c("TI", "TD", "TC"))
### C: highest frequency on building 3 (almost the doble of the other two) 

#hchart(trainingData$BUILDINGID)
#hchart(validationData$BUILDINGID)

## validation
#hchart(validationData$BUILDINGID)
validationData$BUILDINGID<-as.factor(validationData$BUILDINGID)
validationData$BUILDINGID <- factor(validationData$BUILDINGID, levels= c(0,1,2), labels= c("TI", "TD", "TC"))
### C: highest frequency on building 1 (> 500) 

######## $ FLOOR ####  
### training 
#hchart(trainingData$FLOOR)
trainingData$FLOOR <- as.factor(trainingData$FLOOR)
### C: similar frequency from 0 to 3 floor, and more than 4 times less on 4th floor

### validation 
#hchart(validationData$FLOOR)
validationData$FLOOR <- as.factor(validationData$FLOOR)
### C: least frequent - 0 and 4th floor. most frequent 1st floor

######## $ LATITUDE ####
# training 
#summary(trainingData$LATITUDE)
### C: highest frequency 4864840,4864850 (~1850 obs)
# max(trainingData$LATITUDE)-min(trainingData$LATITUDE) ## 270.9428
### C: min - 4864746; max - 4865017

# validation
#summary(validationData$LATITUDE)
# max(validationData$LATITUDE)-min(validationData$LATITUDE) ## 269.3492
### C: min - 4864748; max - 4865017 - similar range 


######## $ LONGITUDE ####
# training
# summary(trainingData$LONGITUDE)
# max(trainingData$LONGITUDE) - min(trainingData$LONGITUDE)
### C: min: -7691; max: -7301; range: 390.5194

# validation
#summary(validationData$LONGITUDE)
# max(validationData$LONGITUDE) - min(validationData$LONGITUDE)
### C: min: -7696; max: -7300 // range: 390.5194

######## C.2 Explore relationship between non wap variables ####
#### floor and relative position
#qplot(trainingData$FLOOR, trainingData$RELATIVEPOSITION)
#qplot(trainingData$SPACEID, trainingData$RELATIVEPOSITION)

#### longitude and latitude
#qplot(trainingData$LONGITUDE, trainingData$LATITUDE)
#qplot(validationData$LONGITUDE, validationData$LATITUDE)

####longitude and latitude, with floor, building and relativeposition 
## training 
# ggplot(data=trainingData, aes(x=LONGITUDE, y=LATITUDE)) + geom_point()+facet_wrap(~BUILDINGID)
### C: buildings are isolated 

#ggplot(data=trainingData, aes(x=LONGITUDE, y=LATITUDE)) + geom_point()+facet_grid(~FLOOR)
#ggplot(data=trainingData, aes(x=LONGITUDE, y=LATITUDE)) + geom_point()+facet_wrap(~FLOOR)
### C: 4th floor only on building 3

#ggplot(data=trainingData, aes(x=LONGITUDE, y=LATITUDE)) + geom_point()+facet_wrap(~RELATIVEPOSITION)

# ggplot_coordinates <- ggplot(data=trainingData, aes(x=LONGITUDE, y=LATITUDE)) + geom_point()
# ggplot_coordinates + geom_point(size=4, aes(color = ifelse(LONGITUDE < -7580, "TI",
#                                    ifelse( LONGITUDE < - 7400 & LATITUDE > 4864830, "TD", "TC")))) +
# scale_color_manual(values = c("TI" = "red", "TD" = "purple", "TC" = "blue"),
#          name = "Buildings")

# ggplot_coordinates+ geom_point(aes(color = ifelse(FLOOR == 0, "0",
#                                                    ifelse(FLOOR ==1, "1",
#                                                           ifelse(FLOOR == 2, "2", 
#                                                                 ifelse(FLOOR == 3, "3","4"))))))+
#   scale_color_manual(values = c("1"= "red", "2"="purple", "3"="pink", "0"="orange", "4"="yellow"), name="Floors")+facet_wrap(~BUILDINGID + FLOOR)


## validation
#ggplot_coordinates_valid <- ggplot(data=validationData, aes(x=LONGITUDE, y=LATITUDE)) + geom_point()

#ggplot_coordinates_valid + geom_point(aes(color = ifelse(FLOOR == 0, "0",
#                                                 ifelse(FLOOR ==1, "1",
#                                                        ifelse(FLOOR == 2, "2", 
#                                                             ifelse(FLOOR == 3, "3","4"))))))+
#scale_color_manual(values = c("1"= "red", "2"="purple", "3"="pink", "0"="orange", "4"="yellow"), name="Floors")+facet_wrap(~BUILDINGID + FLOOR)

#### SPACE ID and FLOOR and BUILDINGID and RELATIVE POSITION 
# qplot(trainingData$SPACEID, trainingData$FLOOR)
# qplot(trainingData$SPACEID, trainingData$RELATIVEPOSITION)
# qplot(trainingData$SPACEID, trainingData$BUILDINGID)

######## C.3 Explore wap-variables ####
library(dplyr)
#### D. Pre.processing ####
######## D.1 New dataset - no waps with 100 only ####
### how many attributes whose min value is 100 
# sum(apply(trainingData,2,function(x)(min(x==100))))
### C: 55 waps which min is 100
# sum(apply(validationData,2,function(x)(min(x==100))))
### C: 153 waps which min is 100

min100_train <- names(which(apply(trainingData[,1:520],2,function(x) min(x)) == 100))
### C: vector list of waps with 100 // 55 
min100_valid <- names(which(apply(validationData[,1:520],2,function(x)min(x))== 100))
### C: vector list of waps with 100 // 153
min100_total<-c(min100_valid,min100_train)

### training_clean // new dataset without waps that only yield 100 
training_clean<-trainingData[,-which(names(trainingData) %in% min100_total)]
str(training_clean)
# apply(training_clean,2,function(x)names(min(x==100))) ### NULL  - checked
# sum(apply(training_clean,2,function(x)names(min(x==100)))) ### 0 - checked
### C: new dataset - only with 321 variables

### validation_clean // new data set without waps that only yield 100 
validation_clean<-validationData[,-which(names(validationData) %in% min100_total)]
str(validation_clean)
# apply(validation_clean,2,function(x)names(min(x==100))) ### NULL  - checked
### C: new dataset - only with 321 variables

######## D.2 New version dataset -105 instead of 100 ####
training_clean[training_clean==100] <- -105

######## D.3 New version dataset - no obs with 100 only ####
training_clean <- training_clean[apply(training_clean[,1:312],1,mean)!= -105,]
str(training_clean)
# sum(apply(training_clean,2,function(x)(min(x==100)))) ### still zero 

######## D.4 Separate dataset by building and by floor ####
# sum(apply(training_clean[,c(1:312)],2,function(x)min(x=="100"))) ### Checking 
# sum(apply(training_clean[,c(1:312)],2,function(x)(x=="-105"))) ### Checking 

# train_TC<-training_clean %>% filter(BUILDINGID=="TC")
# train_TI<-training_clean %>% filter(BUILDINGID=="TI")
# train_TD<-training_clean %>% filter(BUILDINGID=="TD")
### C: TD building - 5,196 x 321 / TI building - 5,249 x 321 / TC building - 9,492 x 321

### $ TI Building - Floor 0, 1, 2 and 3 
# train_TI_F0<-train_TI %>% filter(FLOOR==0)
### C: TI building and floor 0 - 1,059 x 271
# train_TI_F1<-train_TI %>% filter(FLOOR==1)
### C: TI building and floor 1 - 1,356 x 271 
# train_TI_F2<-train_TI %>% filter(FLOOR==2)
### C: TI building and floor 2 - 1,443 x 271 
# train_TI_F3<-train_TI %>% filter(FLOOR==3)
### C: TI building and floor 2 - 1,391 x 271 

### $ TD Building - Floor 0, 1, 2 and 3 
# train_TD_F0<-train_TD %>% filter(FLOOR==0)
### C: TD building and floor 0 - 1,368 x 271
# train_TD_F1<-train_TD %>% filter(FLOOR==1)
### C: TD building and floor 1 - 1,484 x 271
# train_TD_F2<-train_TD %>% filter(FLOOR==2)
### C: TD building and floor 2 - 1,396  x 271
# train_TD_F3<-train_TD %>% filter(FLOOR==3)
### C: TD building and floor 3 - 948  x 271

### $ TC Building - Floor 0, 1, 2, 3 and 4 
# train_TC_F0<-train_TC %>% filter(FLOOR==0)
### C: TC building and floor 0 - 1,942 x 271
# train_TC_F1<-train_TC %>% filter(FLOOR==1)
### C: TC building and floor 1 - 2,162 x 271
# train_TC_F2<-train_TC %>% filter(FLOOR==2)
### C: TC building and floor 2 - 1,577 x 271
# train_TC_F3<-train_TC %>% filter(FLOOR==3)
### C: TC building and floor 3 - 2,709 x 271
# train_TC_F4<-train_TC %>% filter(FLOOR==4)
### C: TC building and floor 4 - 1,102 x 271

###### VALIDATION
# valid_TC<-validation_clean %>% filter(BUILDINGID=="TC")
# valid_TI<-validation_clean %>% filter(BUILDINGID=="TI")
# valid_TD<-validation_clean %>% filter(BUILDINGID=="TD")

### $ TC Building - Floor 0, 1, 2, 3 and 4 
# valid_TC_F0<-valid_TC %>% filter(FLOOR==0)
# valid_TC_F1<-valid_TC %>% filter(FLOOR==1)
# valid_TC_F2<-valid_TC %>% filter(FLOOR==2)
# valid_TC_F3<-valid_TC %>% filter(FLOOR==3)
# valid_TC_F4<-valid_TC %>% filter(FLOOR==4)

### $ TD Building - Floor 0, 1, 2 and 3 
# valid_TD_F0<-valid_TD %>% filter(FLOOR==0)
# valid_TD_F1<-valid_TD %>% filter(FLOOR==1)
# valid_TD_F2<-valid_TD %>% filter(FLOOR==2)
# valid_TD_F3<-valid_TD %>% filter(FLOOR==3)

### $ TI Building - Floor 0, 1, 2 and 3 
# valid_TI_F0<-valid_TI %>% filter(FLOOR==0)
# valid_TI_F1<-valid_TI %>% filter(FLOOR==1)
# valid_TI_F2<-valid_TI %>% filter(FLOOR==2)
# valid_TI_F3<-valid_TI %>% filter(FLOOR==3)
### C: obsevations are not equally distributed by floors

#### E. Explore wap-variables in different datasets #### 
## TRAINING 
# ggplot(data=train_TI, aes(x=LONGITUDE, y=LATITUDE)) + geom_point()+facet_wrap(~FLOOR)
### No big difference
# ggplot(data=train_TD, aes(x=LONGITUDE, y=LATITUDE)) + geom_point()+facet_wrap(~FLOOR)
### A lot of differences - 2nd floor the most complete - 3rd floor the least 
# ggplot(data=train_TC, aes(x=LONGITUDE, y=LATITUDE)) + geom_point()+facet_wrap(~FLOOR)
### Some differences - 0, 1 and 2 floor - few observation son south side. 3rd floor is the more complete and 4th floor/ south-east is really bad

## VALIDATION
# ggplot(data=valid_TI, aes(x=LONGITUDE, y=LATITUDE)) + geom_point()+facet_wrap(~FLOOR)
### No big difference
# ggplot(data=valid_TD, aes(x=LONGITUDE, y=LATITUDE)) + geom_point()+facet_wrap(~FLOOR)
### A lot of differences - 0 and 3rd floor are very poorly covered
# ggplot(data=valid_TC, aes(x=LONGITUDE, y=LATITUDE)) + geom_point()+facet_wrap(~FLOOR)
### Floor zero very poorly covered

### $ signal between -30 to 0 // Building and floor 
# sum(apply(training_clean[,c(1:312)],1,function(x) length(which(x > -30 & x <= 0)))) ### 682
# sum(apply(train_TI[,c(1:312)],1,function(x) length(which(x > -30 & x <= 0)))) ### 2
# sum(apply(train_TD[,c(1:312)],1,function(x) length(which(x > -30 & x <= 0)))) ### 8 
# sum(apply(train_TC[,c(1:312)],1,function(x) length(which(x > -30 & x <= 0)))) ### 672
### C: something happening in TC

# sum(apply(train_TC_F0[,c(1:312)],1,function(x) length(which(x > -30 & x <= 0)))) ### 3
# sum(apply(train_TC_F1[,c(1:312)],1,function(x) length(which(x > -30 & x <= 0)))) ### 2
# sum(apply(train_TC_F2[,c(1:312)],1,function(x) length(which(x > -30 & x <= 0)))) ### 0
# sum(apply(train_TC_F3[,c(1:312)],1,function(x) length(which(x > -30 & x <= 0)))) ### 372
# sum(apply(train_TC_F4[,c(1:312)],1,function(x) length(which(x > -30 & x <= 0)))) ### 295 
### C: something happening in TC - 3rd and 4th floors

# which(apply(train_TC_F3[,c(1:312)],2,function(x) length(which(x > -30 & x <= 0)))>0)
# which(apply(train_TC_F4[,c(1:312)],2,function(x) length(which(x > -30 & x <= 0)))>0)
### C: treat this dataset separately 

### $ signal between -90 to -30 // Building and floor 
# names(which(apply(train_TI_F0[,c(1:312)],2,function(x) length(which(x >= -90 & x <=-30)))>0)) ### 89
# names(which(apply(train_TI_F1[,c(1:312)],2,function(x) length(which(x >= -90 & x <=-30)))>0)) ### 101
# names(which(apply(train_TI_F2[,c(1:312)],2,function(x) length(which(x >= -90 & x <=-30)))>0)) ### 112
# names(which(apply(train_TI_F3[,c(1:312)],2,function(x) length(which(x >= -90 & x <=-30)))>0)) ### 103

# names(which(apply(train_TD_F0[,c(1:312)],2,function(x) length(which(x >= -90 & x <=-30)))>0)) ### 101
# names(which(apply(train_TD_F1[,c(1:312)],2,function(x) length(which(x >= -90 & x <=-30)))>0)) ### 110
# names(which(apply(train_TD_F2[,c(1:312)],2,function(x) length(which(x >= -90 & x <=-30)))>0)) ### 134
# names(which(apply(train_TD_F3[,c(1:312)],2,function(x) length(which(x >= -90 & x <=-30)))>0)) ### 98

# names(which(apply(train_TC_F0[,c(1:312)],2,function(x) length(which(x >= -90 & x <=-30)))>0)) ### 70
# names(which(apply(train_TC_F1[,c(1:312)],2,function(x) length(which(x >= -90 & x <=-30)))>0)) ### 107
# names(which(apply(train_TC_F2[,c(1:312)],2,function(x) length(which(x >= -90 & x <=-30)))>0)) ### 93
# names(which(apply(train_TC_F3[,c(1:312)],2,function(x) length(which(x >= -90 & x <=-30)))>0)) ### 103
# names(which(apply(train_TC_F4[,c(1:312)],2,function(x) length(which(x >= -90 & x <=-30)))>0)) ### 68
### all floors have similiar frequency of strong signals

#### F. Check for variance #### 
## general dataset
apply(training_clean[,c(1:312)], 2, var)
names(which(apply(training_clean[,c(1:312)], 2, var)==0)) ## 0

## by building
names(which(apply(train_TI[,c(1:312)], 2, var)==0)) ## 166
names(which(apply(train_TD[,c(1:312)], 2, var)==0)) ## 144
names(which(apply(train_TC[,c(1:312)], 2, var)==0)) ## 190 

#### G. Create a dataset with erroneous values ####
## training_erroneous <-apply(training_clean,1,function(x)(x>=-30))
training_error<-training_clean[apply(training_clean[,c(1:312)],1,function(x)max(x)>=-30),]
# summary(training_error)
# str(training_error)

## check what's going on
hchart(training_error$BUILDINGID)
### 454 only from TC building 
hchart(training_error$FLOOR)
### 252 only on 3 floor and 200 on 4th floor
hchart(training_error$USERID)
### 397 userid =6 // 52 userid=14 // 5 userid= 3 // 9 userid=1 
hchart(training_error$PHONEID)
### 397 phoneid = 19 // 52 phoneid=7 // 5 phoneid =16 // 13 phoneid 14
#hchart(training_error$RELATIVEPOSITION)
### 464 outside 
#hchart(training_error$SPACEID)
### no relevant conclusion

ggplot(data=training_error %>% filter (BUILDINGID=="TC"), aes(x=LONGITUDE, y=LATITUDE)) + geom_point(aes(color = ifelse(FLOOR == 0, "0",
                                                                                                                        ifelse(FLOOR ==1, "1",
                                                                                                                               ifelse(FLOOR == 2, "2", 
                                                                                                                                      ifelse(FLOOR == 3, "3","4"))))))+
  scale_color_manual(values = c("1"= "red", "2"="purple", "3"="pink", "0"="orange", "4"="yellow"), name="Floors")+facet_wrap(~BUILDINGID + FLOOR)

#ggplot(data=training_error, aes(x=USERID, y=PHONEID)) + geom_point() + facet_wrap(training_error$BUILDINGID)
### C: Phone 19 belongs to user 6, phone 7 belongs to user 14, phone 14 belongs to user 1, user 9 and user 16 

#ggplot(data=training_error, aes(x=FLOOR, y=BUILDINGID)) + geom_point()
### C: Erros on thrid floor from TI and TC

#plot(training_error$FLOOR, training_error$BUILDINGID)
### C: zero floor errors are equally distributed, errors on 3d floor are mostly from TC and errors on 4th floor are only on TC

#### G. Explore erroneous values (user 6 and user 14)
# Phoneid 19 belongs to userid 6, 
# phone 7 belongs to user 14

### what's up with these users?

# user 6 
PHONEID_19 <-training_clean %>% filter(PHONEID==19)
### C: 980 x 321
# hchart(PHONEID_19$USERID)
### only user 6 
USERID_6 <-training_clean %>% filter(USERID==6)
dim(USERID_6)
### C: 980 322
dim(USERID_6_error)
### C:  397 322
# hchart(USERID_6$PHONEID)
### only phone id 19 
USERID_6_error <-training_error %>% filter(USERID==6)
### C:  397 x 321

# user 14 
USERID_14 <-training_clean %>% filter(USERID==14)
### C: 1,596 x 321
#hchart(USERID_14$PHONEID)
## only phone 7 
USERID_14_error <-training_error %>% filter(USERID==14)
### C:  52 x 321

### all path - user 6 
ggplot(data=USERID_6, aes(x=LONGITUDE, y=LATITUDE)) + geom_point() + facet_wrap(~ USERID_6$FLOOR + USERID_6$BUILDINGID)
### it has only passed through a corridor of the 3rd floor and a corridor of 4th floor of one building (TC)

### path with errors- user 6 
ggplot(data=USERID_6_error, aes(x=LONGITUDE, y=LATITUDE)) + geom_point(colour="red") + facet_wrap(~ USERID_6_error$FLOOR + USERID_6_error$BUILDINGID)

### all path - user 14 
# ggplot(data=USERID_14, aes(x=LONGITUDE, y=LATITUDE)) + geom_point() + facet_wrap(~ USERID_14$FLOOR + USERID_14$BUILDINGID)
### C: it has only been in floors 0 and 1 of TD and floors 2 and 3 of TC 

### path with errors- user 14 
# ggplot(data=USERID_14_error, aes(x=LONGITUDE, y=LATITUDE)) + geom_point() + facet_wrap(~ USERID_14_error$FLOOR + USERID_14_error$BUILDINGID)
#qplot(USERID_14_error$TIMESTAMP)
# class(timestamp)

### changing timestamp type 
USERID_14_error$TIMESTAMP_dt<-as.POSIXct(as.numeric(USERID_14_error$TIMESTAMP),origin="1970-01-01",tz="GMT")
#head(USERID_14_error$TIMESTAMP_dt)
#hchart(USERID_14_error$TIMESTAMP_dt)

#ggplot(data = USERID_14_error, aes(x = USERID_14_error$TIMESTAMP_dt, y = USERID_14_error$BUILDINGID)) + geom_point()+facet_wrap(USERID_14_error$FLOOR)
### C: errors happened in the beggining and in the end of its routh / path

### changing timestamp type 
USERID_6_error$TIMESTAMP_dt<-as.POSIXct(as.numeric(USERID_6_error$TIMESTAMP),origin="1970-01-01",tz="GMT")
USERID_6$TIMESTAMP_dt<-as.POSIXct(as.numeric(USERID_6$TIMESTAMP),origin="1970-01-01",tz="GMT")
#head(USERID_6_error$TIMESTAMP_dt)

ggplot(data = USERID_6_error, aes(x = USERID_6_error$TIMESTAMP_dt, y = USERID_6_error$BUILDINGID)) + geom_point()+facet_wrap(USERID_6_error$FLOOR)
ggplot(data = USERID_6, aes(x = USERID_6$TIMESTAMP_dt, y = USERID_6$BUILDINGID)) + geom_point()+facet_wrap(USERID_6$FLOOR)
### C: errors happened along the way 

### all path - random user 

#ggplot(data=(training_clean %>% filter(USERID==11)), aes(x=LONGITUDE, y=LATITUDE)) + geom_point() + facet_wrap(~BUILDINGID + FLOOR)

## USER 1 - has been on all floors (everywhere) in building TI
## USER 17 has been in TD - first floor and TC 0 floor 
## USER 11 - has been on all buildings - all floors of TI, 0 and first floor of TD, second and third floor of TC 

#### H. Cleaning erroneous values ####
## cleaning observations that a signal between -30 and 0 
## these are wrong/ erroneous values, since the maximum signal is -30. 
# str(USERID_14_error) ## 52 obs. of  321 variables:
# str(USERID_6_error) ## 397 obs. of  321 variables:
training_clean_v2 <- training_clean[apply(training_clean[,c(1:312)],1,function(x)max(x)<=-30),]
sum(apply(validation_clean[,c(1:312)],1,function(x)max(x)<=-30))
# summary(training_clean_v2)
# str(training_clean)## 19861 obs = 19402+472-13
# str(training_error) ## 472 obs
# str(training_clean_v2) ## 19402 obs. of  321 variables:

# dim(trainingData)

#### ------------------------- Different from script 3.3_v2 ------------------------- ####
#### I. Feature selection ####
## Deleting columns: "SPACEID" "RELATIVEPOSITION" "USERID" "PHONEID" "TIMESTAMP"       
training_vf <- training_clean_v2[,-c(320:321)] 
training_vf <- training_vf[,-c(317:318)] 
# colnames(training_vf)
# dim(training_vf) ## 19402 obs. of  317 variables

#### J. Pre-processing - User6 removal ####
training_vf <- training_vf %>% filter(USERID!=6)
dim(training_vf) # 18819   317
hchart(training_vf$USERID)

#### K. Duplicated rows removal ####
sum(duplicated(training_vf)) ## 711
training_vf<-training_vf[!duplicated(training_vf),]
# dim(training_vf) # 18108   317
colnames(training_vf)

#### L. Feature selection v2 ####
colnames(training_vf)
training_vf <- training_vf[,-c(317)] 

#### M. Prepare validation ####
# remove columns 
colnames(validation_clean)
validation_vf <- validation_clean[,-c(317:318)]
validation_vf <- validation_vf[,-c(318:319)]
validation_vf <- validation_vf[,-c(317)]
colnames(validation_vf)

# replace 150 by -105
validation_vf[validation_vf==100] <- -105

# duplicated rows 
sum(duplicated(validation_vf))

#### N. Creating training dataset - data partition ####
training_BUILD_sample<-createDataPartition(y=training_vf$BUILDINGID,  p=0.10)
class(training_BUILD_sample) ### list 

## Training (sample)
training_BUILD_sample_1 <- training_vf[training_BUILD_sample$Resample1,]
dim(training_BUILD_sample_1) ## 1812  316
colnames(training_BUILD_sample_1) ## 312 waps + long + lat + floor + buildingID

#### O. Cross validation ####
Cross_validation <- trainControl(
  method = "repeatedcv",
  number = 3,
  repeats = 3,
  allowParallel = TRUE)

#### P. Modeling BUILDINGID ####
# set.seed (123)
# svm_building_vf <- train(BUILDINGID ~ . - LATITUDE - LONGITUDE - FLOOR, 
# data = training_BUILD_sample_1, 
  # method = "svmLinear3", 
    # trControl = Cross_validation)

# svm_building_vf
# cost  Loss  Accuracy   Kappa 
# 0.25  L1    0.9996321  0.9994331

# save(svm_building_vf,file = "svm_building_vf.Rdata")
load("svm_building_vf.Rdata")
svm_building_vf_prediction <- predict(svm_building_vf,validation_vf)

library(Metrics)
accuracy(svm_building_vf_prediction, validation_vf$BUILDINGID) #0.9936994
confusionMatrix(data=svm_building_vf_prediction,validation_vf$BUILDINGID)

# worse accuracy than withouth taking user6 observations 

#### 2nd APPROACH - MODEL FLOORINDEX ####

##### Create Floorindex ####
training_vf$FLOORINDEX <- paste0(training_vf$BUILDINGID, training_vf$FLOOR)
validation_vf$FLOORINDEX <- paste0(validation_vf$BUILDINGID, validation_vf$FLOOR)

training_vf$FLOORINDEX <- as.factor(training_vf$FLOORINDEX)
validation_vf$FLOORINDEX <- as.factor(validation_vf$FLOORINDEX)

##### Create separate datasets based on building ####
train_TC_vf <-training_vf %>% filter(BUILDINGID=="TC")
train_TI_vf <-training_vf %>% filter(BUILDINGID=="TI")
train_TD_vf <-training_vf %>% filter(BUILDINGID=="TD")

valid_TC_vf <-validation_vf %>% filter(BUILDINGID=="TC")
valid_TI_vf <-validation_vf %>% filter(BUILDINGID=="TI")
valid_TD_vf <-validation_vf %>% filter(BUILDINGID=="TD")

##### Model FLOORINDEX ####
#### TC ####
# svmLinear_floor_TC_vf <- train(FLOOR ~. -LATITUDE - LONGITUDE - BUILDINGID - FLOORINDEX, 
  #                                                 data = train_TC_vf , 
   #                                               method = "svmLinear", 
    #                                             trControl = Cross_validation)

# svmLinear_floor_TC_vf 
# save(svmLinear_floor_TC_vf,file = "svmLinear_floor_TC_vf.Rdata")
load("svmLinear_floor_TC_vf.Rdata")
svmLinear_floor_TC_vf_prediction <- predict(svmLinear_floor_TC_vf,valid_TC_vf) 

## Accuracy 
accuracy(svmLinear_floor_TC_vf_prediction, valid_TC_vf$FLOOR) ### accuracy 0.8938  / kappa 0.8543      
confusionMatrix(data=svmLinear_floor_TC_vf_prediction, valid_TC_vf$FLOOR)

#### TD ####
train_TD_vf$FLOORINDEX <-as.character(train_TD_vf$FLOORINDEX)
train_TD_vf$FLOORINDEX <-as.factor(train_TD_vf$FLOORINDEX)
levels(train_TD_vf$FLOORINDEX)

train_TD_vf$FLOOR <-as.character(train_TD_vf$FLOOR)
train_TD_vf$FLOOR <-as.factor(train_TD_vf$FLOOR)
levels(train_TD_vf$FLOOR)

valid_TD_vf$FLOORINDEX <-as.character(valid_TD_vf$FLOORINDEX)
valid_TD_vf$FLOORINDEX <-as.factor(valid_TD_vf$FLOORINDEX)
levels(train_TD_vf$FLOORINDEX)

valid_TD_vf$FLOOR <-as.character(valid_TD_vf$FLOOR)
valid_TD_vf$FLOOR <-as.factor(valid_TD_vf$FLOOR)
levels(train_TD_vf$FLOOR)


# svmLinear_floor_TD_vf <- train(FLOOR ~. -LATITUDE - LONGITUDE - BUILDINGID - FLOORINDEX, 
    #                           data = train_TD_vf , 
   #                            method = "svmLinear", 
  #                             trControl = Cross_validation)

# svmLinear_floor_TD_vf 
# save(svmLinear_floor_TD_vf,file = "svmLinear_floor_TD_vf.Rdata")
load("svmLinear_floor_TD_vf.Rdata")
svmLinear_floor_TD_vf_prediction <- predict(svmLinear_floor_TD_vf,valid_TD_vf) 

## Accuracy 
accuracy(svmLinear_floor_TD_vf_prediction, valid_TD_vf$FLOOR) ### accuracy 0.7993    / kappa 0.7121
confusionMatrix(data=svmLinear_floor_TD_vf_prediction, valid_TD_vf$FLOOR)

#### TI ####
train_TI_vf$FLOORINDEX <-as.character(train_TI_vf$FLOORINDEX)
train_TI_vf$FLOORINDEX <-as.factor(train_TI_vf$FLOORINDEX)
levels(train_TI_vf$FLOORINDEX)

train_TI_vf$FLOOR <-as.character(train_TI_vf$FLOOR)
train_TI_vf$FLOOR <-as.factor(train_TI_vf$FLOOR)
levels(train_TI_vf$FLOOR)

valid_TI_vf$FLOORINDEX <-as.character(valid_TI_vf$FLOORINDEX)
valid_TI_vf$FLOORINDEX <-as.factor(valid_TI_vf$FLOORINDEX)
levels(train_TI_vf$FLOORINDEX)

valid_TI_vf$FLOOR <-as.character(valid_TI_vf$FLOOR)
valid_TI_vf$FLOOR <-as.factor(valid_TI_vf$FLOOR)
levels(train_TI_vf$FLOOR)


# svmLinear_floor_TI_vf <- train(FLOOR ~. -LATITUDE - LONGITUDE - BUILDINGID - FLOORINDEX, 
  #                             data = train_TI_vf , 
   #                            method = "svmLinear", 
    #                           trControl = Cross_validation)

# svmLinear_floor_TI_vf 
# save(svmLinear_floor_TI_vf,file = "svmLinear_floor_TI_vf.Rdata")
load("svmLinear_floor_TI_vf.Rdata")
svmLinear_floor_TI_vf_prediction <- predict(svmLinear_floor_TI_vf,valid_TI_vf) 

## Accuracy 
accuracy(svmLinear_floor_TI_vf_prediction, valid_TI_vf$FLOOR) ### accuracy 0.9494 / kappa 0.9288 
confusionMatrix(data=svmLinear_floor_TI_vf_prediction, valid_TI_vf$FLOOR)


######## Include FLOOR predictions in respective datasets ####
valid_TC_vf$FLOOR <- svmLinear_floor_TC_vf_prediction
valid_TD_vf$FLOOR <- svmLinear_floor_TD_vf_prediction
valid_TI_vf$FLOOR <- svmLinear_floor_TI_vf_prediction
######## Q. Model LONGITUDE based on predicted floor and waps in the different datasets ####
#### TC ####
# colnames(train_TC_vf)
# set.seed(123)
# rf_longitude_TC_vf <-  randomForest::randomForest(LONGITUDE ~ . - LATITUDE - BUILDINGID - FLOORINDEX,
  #                                   data = train_TC_vf, 
   #                                ntree=100,
    #                            tuneLength =20, 
     #                        trControl = Cross_validation )

# rf_longitude_TC_vf 
# save(rf_longitude_TC_vf, file="rf_longitude_TC_vf.Rdata")
load("rf_longitude_TC_vf.Rdata")
rf_longitude_TC_vf_prediction <- predict(rf_longitude_TC_vf,valid_TC_vf)
rf_longitude_TC_vf_prediction
postResample(rf_longitude_TC_vf_prediction,valid_TC_vf$LONGITUDE)
#       RMSE   Rsquared        MAE 
#  10.7037323  0.8870557  7.0089658 (70 trees and tune length 30) - with user6
#  10.5724280  0.8896429  6.9497967 (100 trees and tune length 20) - with user6 
#  20.2320889  0.7182912  9.7414582 (100 trees and tune length 20) - without 

#### TI ####
colnames(train_TI_vf)
set.seed(123)
 rf_longitude_TI_vf <-  randomForest::randomForest(LONGITUDE ~ . - LATITUDE - BUILDINGID - FLOORINDEX,
                                   data = train_TI_vf, 
                               ntree=100,
                            tuneLength =20, 
                        trControl = Cross_validation )

rf_longitude_TI_vf 
save(rf_longitude_TI_vf, file="rf_longitude_TI_vf.Rdata")
load("rf_longitude_TI_vf.Rdata")
rf_longitude_TI_vf_prediction <- predict(rf_longitude_TI_vf,valid_TI_vf)
rf_longitude_TI_vf_prediction
postResample(rf_longitude_TI_vf_prediction,valid_TI_vf$LONGITUDE)
#       RMSE   Rsquared        MAE 
#    7.0087619 0.9318441 4.6775261  (100 trees and tune length 20) - with user 6 
#    6.4643004 0.9421383 4.5272628 (100 trees and tune length 20) - without user 6 

#### TD #### 
# colnames(train_TD_vf)
# set.seed(123)
# rf_longitude_TD_vf <-  randomForest::randomForest(LONGITUDE ~ . - LATITUDE - BUILDINGID - FLOORINDEX,
#                                data = train_TD_vf, 
#                           ntree=100,
#                       tuneLength =20, 
#                  trControl = Cross_validation )

# rf_longitude_TD_vf 
# save(rf_longitude_TD_vf, file="rf_longitude_TD_vf.Rdata")
load("rf_longitude_TD_vf.Rdata")
rf_longitude_TD_vf_prediction <- predict(rf_longitude_TD_vf,valid_TD_vf)
rf_longitude_TD_vf_prediction
postResample(rf_longitude_TD_vf_prediction,valid_TD_vf$LONGITUDE)
##       RMSE   Rsquared        MAE 
#    10.2011955  0.9527722  7.1335756 (100 trees and tune length 20) - with user6
#   12.2885364  0.9337247  7.5588625 (100 trees and tune length 20) - without user6



######## Q. Model LATITUDE based on predicted floor and waps in the different datasets ####
#### TC ####
# colnames(train_TC_vf)
# set.seed(123)
# rf_latitude_TC_vf <-  randomForest::randomForest(LATITUDE ~ . - LONGITUDE - BUILDINGID - FLOORINDEX,
 #                                  data = train_TC_vf, 
  #                              ntree=100,
   #                         tuneLength =20, 
    #                    trControl = Cross_validation )

# rf_latitude_TC_vf
# save(rf_latitude_TC_vf, file="rf_latitude_TC_vf.Rdata")
load("rf_latitude_TC_vf.Rdata")
rf_latitude_TC_vf_prediction <- predict(rf_latitude_TC_vf,valid_TC_vf)
rf_latitude_TC_vf_prediction
postResample(rf_latitude_TC_vf_prediction,valid_TC_vf$LATITUDE)
#       RMSE   Rsquared        MAE 
#    9.0957511 0.9019608 6.3635272 (100 trees and tune length 20) - WITH user6 
#   12.526265  0.812248  7.659067 (100 trees and tune length 20) - WITHout user6 
#### TI ####
# colnames(train_TI_vf)
# set.seed(123)
# rf_latitude_TI_vf <-  randomForest::randomForest(LATITUDE ~ . - LONGITUDE - BUILDINGID - FLOORINDEX,
  #                                               data = train_TI_vf, 
   #                                              ntree=100,
    #                                             tuneLength =20, 
     #                                            trControl = Cross_validation )

# rf_latitude_TI_vf
# save(rf_latitude_TI_vf, file="rf_latitude_TI_vf.Rdata")
load("rf_latitude_TC_vf.Rdata")
rf_latitude_TI_vf_prediction <- predict(rf_latitude_TI_vf,valid_TI_vf)
rf_latitude_TI_vf_prediction
postResample(rf_latitude_TI_vf_prediction,valid_TI_vf$LATITUDE)
#       RMSE   Rsquared        MAE 
#    5.4395317 0.9716765 3.8059218  (100 trees and tune length 20) - with user6
#    5.4202764 0.9719225 3.7862496  (100 trees and tune length 20) - without user6
#### TD ####
# colnames(train_TD_vf)
# set.seed(123)
# rf_latitude_TD_vf <-  randomForest::randomForest(LATITUDE ~ . - LONGITUDE - BUILDINGID - FLOORINDEX,
  #                                               data = train_TD_vf, 
   #                                              ntree=100,
    #                                             tuneLength =20, 
     #                                            trControl = Cross_validation )

# rf_latitude_TD_vf
# save(rf_latitude_TD_vf, file="rf_latitude_TD_vf.Rdata")
load("rf_latitude_TD_vf.Rdata")
rf_latitude_TD_vf_prediction <- predict(rf_latitude_TD_vf,valid_TD_vf)
rf_latitude_TD_vf_prediction
postResample(rf_latitude_TD_vf_prediction,valid_TD_vf$LATITUDE)
#       RMSE   Rsquared        MAE 
#       RMSE   Rsquared        MAE 
# 10.6867520  0.9091881  7.4728915  (100 trees and tune length 20) - with user6 
# 13.356380  0.864081  8.014365     (100 trees and tune length 20) - without user6
