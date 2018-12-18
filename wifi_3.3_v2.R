#### General description ####
# Evaluate Techniques for Wifi Locationing
# Joana
# 03/11 - 21/11 & 02/11

#### Goal #### 
# Our client would like us to investigate the feasibility of using "wifi fingerprinting" to determine a person's location in indoor spaces.

#### A. Clear environment ####
rm(list=ls())

#### B.1 Upload datset - training and dataset #### 
library(readr)
trainingData <- read_csv("Desktop/UBIQUM/2. Tasks/Course 3/Task 3/Data/UJIndoorLoc/trainingData.csv", na = "NA")
validationData <- read_csv("Desktop/UBIQUM/2. Tasks/Course 3/Task 3/Data/UJIndoorLoc/validationData.csv", na = "NA")

#### B.2 Description of the dataset ####
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

#### C.1 Explore dataset #### 
#head(trainingData)
#head(validationData)
#sum(is.na(trainingData))
#sum(is.na(validationData))
#str(trainingData)
#str(trainingData, list.len= nrow(trainingData))
#sum(names(trainingData)==names(validationData))
### C: Both datasets have 529 variables and all of them are the same 
### C: no NAs in neither 

### $ TIMESTAMP ####
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


### $ PHONEID ####
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


### $ USERID ####
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


### $ RELATIVEPOSITION ####
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


### $ SPACEID ####
### training
#summary(trainingData$SPACEID)
#str(trainingData$SPACEID)
#hchart(trainingData$SPACEID)
trainingData$SPACEID<-as.factor(trainingData$SPACEID)
#hchart(trainingData$SPACEID)
#str(trainingData$SPACEID)
### C: 123 levels - highest frequency: 101- 104; 106 - 108; 201- 202

### validation
#hchart(validationData$SPACEID)
validationData$SPACEID <- as.factor(validationData$SPACEID)
#summary(validationData$SPACEID)
### C: No information on this in validation set  


### $ BUILDINGID ####
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

### $ FLOOR ####  
### training 
#hchart(trainingData$FLOOR)
trainingData$FLOOR <- as.factor(trainingData$FLOOR)
### C: similar frequency from 0 to 3 floor, and more than 4 times less on 4th floor

### validation 
#hchart(validationData$FLOOR)
validationData$FLOOR <- as.factor(validationData$FLOOR)
### C: least frequent - 0 and 4th floor. most frequent 1st floor

### $ LATITUDE ####
# training 
#summary(trainingData$LATITUDE)
### C: highest frequency 4864840,4864850 (~1850 obs)
# max(trainingData$LATITUDE)-min(trainingData$LATITUDE) ## 270.9428
### C: min - 4864746; max - 4865017

# validation
#summary(validationData$LATITUDE)
# max(validationData$LATITUDE)-min(validationData$LATITUDE) ## 269.3492
### C: min - 4864748; max - 4865017 - similar range 


### $ LONGITUDE ####
# training
# summary(trainingData$LONGITUDE)
# max(trainingData$LONGITUDE) - min(trainingData$LONGITUDE)
### C: min: -7691; max: -7301; range: 390.5194

# validation
#summary(validationData$LONGITUDE)
# max(validationData$LONGITUDE) - min(validationData$LONGITUDE)
### C: min: -7696; max: -7300 // range: 390.5194

#### C.2 Explore relationship between non wap variables ####
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

#### C.3 Explore wap-variables ####
library(dplyr)
#### D.1 New dataset - no waps with 100 only ####
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

#### D.2 New version dataset -105 instead of 100 ####
training_clean[training_clean==100] <- -105

#### D.3 New version dataset - no obs with 100 only ####
training_clean <- training_clean[apply(training_clean[,1:312],1,mean)!= -105,]
str(training_clean)
# sum(apply(training_clean,2,function(x)(min(x==100)))) ### still zero 

#### D.4 Separate dataset by building and by floor ####
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
# hchart(training_error$BUILDINGID)
### 454 only from TC building 
# hchart(training_error$FLOOR)
### 252 only on 3 floor and 200 on 4th floor
# hchart(training_error$USERID)
### 397 userid =6 // 52 userid=14 // 5 userid= 3 // 9 userid=1 
# hchart(training_error$PHONEID)
### 397 phoneid = 19 // 52 phoneid=7 // 5 phoneid =16 // 13 phoneid 14
#hchart(training_error$RELATIVEPOSITION)
### 464 outside 
#hchart(training_error$SPACEID)
### no relevant conclusion

# ggplot(data=training_error, aes(x=LONGITUDE, y=LATITUDE)) + geom_point(aes(color = ifelse(FLOOR == 0, "0",
#                                                                            ifelse(FLOOR ==1, "1",
#                                                                                  ifelse(FLOOR == 2, "2", 
#                                                                                         ifelse(FLOOR == 3, "3","4"))))))+
# scale_color_manual(values = c("1"= "red", "2"="purple", "3"="pink", "0"="orange", "4"="yellow"), name="Floors")+facet_wrap(~BUILDINGID + FLOOR)

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
### C: 980 x 321
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
# ggplot(data=USERID_6, aes(x=LONGITUDE, y=LATITUDE)) + geom_point() + facet_wrap(~ USERID_6$FLOOR + USERID_6$BUILDINGID)
### it has only passed through a corridor of the 3rd floor and a corridor of 4th floor of one building (TC)

### path with errors- user 6 
# ggplot(data=USERID_6_error, aes(x=LONGITUDE, y=LATITUDE)) + geom_point() + facet_wrap(~ USERID_6_error$FLOOR + USERID_6_error$BUILDINGID)
### same path without errors - device problem?

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

# ggplot(data = USERID_6_error, aes(x = USERID_6_error$TIMESTAMP_dt, y = USERID_6_error$BUILDINGID)) + geom_point()+facet_wrap(USERID_6_error$FLOOR)
# ggplot(data = USERID_6, aes(x = USERID_6$TIMESTAMP_dt, y = USERID_6$BUILDINGID)) + geom_point()+facet_wrap(USERID_6$FLOOR)
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

#### I. Feature selection ####
## Deleting columns: "SPACEID" "RELATIVEPOSITION" "USERID" "PHONEID" "TIMESTAMP"       
training_clean_v3 <- training_clean_v2[,-c(317:321)] 

# colnames(training_clean_v3)
# str(training_clean_v3) ## 19402 obs. of  316 variables

#### J. Parallel Processing ####
library(foreach)
library(iterators)
library(parallel)
library(doMC)
registerDoMC(cores = 4)

# library(doSNOW) ### Error: object 'doSNOW' not found

#### K. (missing Creating a sample / data partition)
#### L. Prepare validation dataset ####
## Make the same amendments, such as delete the same variables and changing 100 to -105 

## delete columns
# str(training_c_part_train) ## 1942 obs. of  316 variables:
# str(validation_clean) ## 1111 obs. of  321 variables:
# colnames(validation_clean)

validation_v3<-validation_clean[,-c(317:321)] 
# colnames(validation_v3)
# str(validation_v3) ## 1111 obs. of  316 variables:

## change 100 by -105 
validation_v3[validation_v3==100] <- -105

 ########## Distinct from script 3.1 ############
#### M. Position points - carfeull v3####
# str(training_clean_v3) ## 19402 obs. of  317 variables:
# training_clean_v3$Unique_position <- paste(training_clean_v3$LONGITUDE, training_clean_v3$LATITUDE, training_clean_v3$FLOOR)
# training_clean_v3$Unique_position <- factor(training_clean_v3$Unique_position)
# str(training_clean_v3$Unique_position) ### only 933 levels

# validation_v3$Unique_position <- paste(validation_v3$LONGITUDE, validation_v3$LATITUDE, validation_v3$FLOOR)
# validation_v3$Unique_position <- factor(validation_v3$Unique_position)
# str(validation_v3$Unique_position) # 1061 levels


#### N. Duplicated rows ####
# sum(duplicated(training_clean_v3)) ## 715 duplicated
# sum(duplicated(validation_v3)) ## 0 duplicated
duplicated_rows<-training_clean_v3[duplicated(training_clean_v3),]

dim(training_clean_v3) ## 19402   318 / 19402   317 / 19402   316
dim(training_clean_v3[duplicated(training_clean_v3),]) ## 715 318 /  715 317 /  715 316
dim(training_clean_v3[!duplicated(training_clean_v3),]) ## 18687   318 / 18687   317 / 18687   316
training_clean_v7 <- training_clean_v3[!duplicated(training_clean_v3),] 
dim(training_clean_v7)  # /  18687   317 / 18687   316
dim(validation_v3) # / 1111  317 / 1111  316
validation_v7 <- validation_v3


#### O. Creating training dataset - data partition ####
training_sample_buildingid_v7<-createDataPartition(y=training_clean_v7$BUILDINGID,  p=0.10)
class(training_sample_buildingid_v7) ### list 

## Training model ( sample )
training_c7_part_buildingID <- training_clean_v7[training_sample_buildingid_v7$Resample1,]
dim(training_c7_part_buildingID) ## 1870  316
colnames(training_c7_part_buildingID) ## 312 waps + long + lat + floor + buildingID 

## Cross validation 
Cross_validation <- trainControl(
  method = "repeatedcv",
  number = 3,
  repeats = 3,
  allowParallel = TRUE)

#### P. Modelling BUILDINGID ####
# with duplicates: best model: svm linear / accuracy 1 
# set.seed (123)
# svm_building_nodupli <- train(BUILDINGID ~ . - LATITUDE - LONGITUDE - FLOOR, 
  #   data = training_c7_part_buildingID, 
    #                 method = "svmLinear3", 
      #             trControl = Cross_validation)

# svm_building_nodupli
# cost  Loss  Accuracy   Kappa 
# 0.25  L1    1          1

# save(svm_building_nodupli,file = "svm_building_nodupli.Rdata")
load("svm_building_nodupli.Rdata")
svm_building_nodupli_prediction <- predict(svm_building_nodupli,validation_v7)

library(Metrics)
accuracy(svm_building_nodupli_prediction , validation_v7$BUILDINGID) # 1
#### Q. Modelling FLOORINDEX ####
#### Q.1 Creating FLOORINDEX ####
training_clean_v8 <- training_clean_v7
validation_v8 <- validation_v7
training_clean_v8$FLOORINDEX <- paste0(training_clean_v8$BUILDINGID, training_clean_v8$FLOOR)
validation_v8$FLOORINDEX <- paste0(validation_v8$BUILDINGID, validation_v8$FLOOR)

training_clean_v8$FLOORINDEX <- as.factor(training_clean_v8$FLOORINDEX)
validation_v8$FLOORINDEX <- as.factor(validation_v8$FLOORINDEX)

colnames(training_clean_v8)
colnames(validation_v8)

#### Q.2 Creating training dataset for floorindex ####
training_sample_floorindex_v8<-createDataPartition(y=training_clean_v8$FLOORINDEX,  p=0.10)
class(training_sample_floorindex_v8) ### list 

## Training model ( sample )
training_c8_part_floorindex <- training_clean_v8[training_sample_floorindex_v8$Resample1,]
dim(training_c8_part_floorindex) ## 1875  317
dim(training_clean_v8) ## 18687   317
colnames(training_c8_part_floorindex) ## 312 waps + long + lat + floor + buildingID + floorindex

# prop.table(table(training_c8_part_floorindex$FLOORINDEX))
# prop.table(table(training_clean_v8$FLOORINDEX))
### good representativity 

#### Q.3 Model // building + waps ####
# with duplicates: best model: svm linear / accuracy 1 
# set.seed(123)
# svmLinear_floorindex_waps_build_nodupli <- train(FLOORINDEX ~. - LATITUDE - LONGITUDE - FLOOR, 
  #  data = training_c8_part_floorindex, method = "svmLinear", trControl = Cross_validation, preProcess= c("center","scale"))

# svmLinear_floorindex_waps_build_nodupli 
### acc=   0.9711943  kappa = 0.9684839

# save(svmLinear_floorindex_waps_build_nodupli ,file = "svmLinear_floorindex_waps_build_nodupli .Rdata")
load("svmLinear_floorindex_waps_build_nodupli .Rdata")
svmLinear_floorindex_waps_build_nodupli_prediction <- predict(svmLinear_floorindex_waps_build_nodupli,validation_v8) 
svmLinear_floorindex_waps_build_nodupli_prediction

## Accuracy 
accuracy(svmLinear_floorindex_waps_build_nodupli_prediction, validation_v8$FLOORINDEX) ### 0.9090909


#### Q.4 Include FLOORINDEX predictions - v9 ####
training_clean_v9 <- training_clean_v8
validation_v9 <- validation_v8
validation_v9$FLOORINDEX <- svmLinear_floorindex_waps_build_nodupli_prediction
dim(validation_v9)
dim(training_clean_v9)
#### R. Modelling LONGITUDE ####
#### R.1 Creating training dataset for longitude ####
training_sample_long_v9<-createDataPartition(y=training_clean_v9$LONGITUDE,  p=0.10)
class(training_sample_long_v9) ### list 

## Training model (sample)
training_c9_part_longitude <- training_clean_v9[training_sample_long_v9$Resample1,]
dim(training_c9_part_longitude) ## 1871  317
dim(training_clean_v9) ## 18687   317
colnames(training_c9_part_longitude) ## 312 waps + long + lat + floor + buildingID + floorindex


#### R.2 Model // BUILDING + waps ####
#### $ Knn - best ####
# knn_longitude_nodupli <- train(LONGITUDE ~ . - FLOOR - LATITUDE - FLOORINDEX,
  #                    data = training_c9_part_longitude,
   #                method = "knn",     
    #               preProcess=c("center", "scale"),        
     #              trControl = Cross_validation)

# knn_longitude_nodupli #  k 5   RMSE   9.397362  Rsquared    0.9943213     MAE  5.934774
                           
# save(knn_longitude_nodupli, file="knn_longitude_nodupli.Rdata")
load("knn_longitude_nodupli.Rdata")
knn_longitude_nodupli_prediction <- predict(knn_longitude_nodupli,validation_v9)
knn_longitude_nodupli_prediction
postResample(knn_longitude_nodupli_prediction,validation_v9$LONGITUDE)
#       RMSE   Rsquared        MAE 
#  17.170668   0.979887   8.520379 

#### R.3 Model // FLOORINDEX + waps ####
#### $ Knn        ####
# knn_longitude_nodupli_floorindex <- train(LONGITUDE ~ . - FLOOR - LATITUDE - BUILDINGID,
     #            data = training_c9_part_longitude,
    #            method = "knn",     
   #            preProcess=c("center", "scale"),        
  #            trControl = Cross_validation)

# knn_longitude_nodupli_floorindex 
# save(knn_longitude_nodupli_floorindex, file="knn_longitude_nodupli_floorindex.Rdata")
load("knn_longitude_nodupli_floorindex.Rdata")
knn_longitude_nodupli_floorindex_prediction <- predict(knn_longitude_nodupli_floorindex,validation_v9)
knn_longitude_nodupli_floorindex_prediction
postResample(knn_longitude_nodupli_floorindex_prediction,validation_v9$LONGITUDE)
#       RMSE   Rsquared        MAE 
#  17.4331731  0.9793754  8.6967551 

#### S. Modelling LATITUDE ####
#### S.1 Creating training dataset for latutude ####
training_sample_lat_v9<-createDataPartition(y=training_clean_v9$LATITUDE,  p=0.10)
class(training_sample_lat_v9) ### list 

## Training model (sample)
training_c9_part_latitude <- training_clean_v9[training_sample_lat_v9$Resample1,]
dim(training_c9_part_latitude) ## 1871  317
dim(training_clean_v9) ## 18687   317
colnames(training_c9_part_latitude) ## 312 waps + long + lat + floor + buildingID + floorindex

#### S.2 Model // BUILDINGID + waps ####
#### $ KNN - best #### 
# knn_latitude_nodupli<- train(LATITUDE ~ . - FLOOR - LONGITUDE - FLOORINDEX,
  # data = training_c9_part_latitude,
 # method = "knn", 
# preProcess=c("center", "scale"),  
# trControl = Cross_validation )

# knn_latitude_nodupli #  k  5 RMSE 8.014935      Rsquared  0.9860376    MAE  5.135546
# save(knn_latitude_nodupli, file="knn_latitude_4th.Rdata")
load("knn_latitude_nodupli.Rdata")
knn_latitude_nodupli_prediction <- predict(knn_latitude_nodupli,validation_v9)
knn_latitude_nodupli_prediction 
postResample(knn_latitude_nodupli_prediction,validation_v9$LATITUDE)
#       RMSE   Rsquared        MAE 
# 15.9535166  0.9486272   8.4783345 

#### $ SVM Linear ####
# svmLinear_latitude_nodupli<- train(LATITUDE ~ . - FLOOR - LONGITUDE - FLOORINDEX,
  #                           data = training_c9_part_latitude,
   #                          method = "svmLinear", 
    #                         preProcess=c("center", "scale"),  
     #                        trControl = Cross_validation )

# svmLinear_latitude_nodupli
# save(svmLinear_latitude_nodupli, file="knn_latitude_4th.Rdata")
load("svmLinear_latitude_nodupli.Rdata")
svmLinear_latitude_nodupli_prediction <- predict(svmLinear_latitude_nodupli,validation_v9)
svmLinear_latitude_nodupli_prediction 
postResample(svmLinear_latitude_nodupli_prediction,validation_v9$LATITUDE)
#       RMSE   Rsquared        MAE 
# 18.4417116  v0.9319652 13.6428134 

#### S.2 Model // FLOORINDEX + waps ####
#### $ Knn        ####
knn_latitude_nodupli_floorindex<- train(LATITUDE ~ . - FLOOR - LONGITUDE - BUILDINGID,
       data = training_c9_part_latitude,
       method = "knn", 
       preProcess=c("center", "scale"),  
       trControl = Cross_validation )

knn_latitude_nodupli_floorindex 
save(knn_latitude_nodupli_floorindex , file="knn_latitude_nodupli_floorindex .Rdata")
load("knn_latitude_nodupli_floorindex .Rdata")
knn_latitude_nodupli_floorindex_prediction <- predict(knn_latitude_nodupli_floorindex,validation_v9)
knn_latitude_nodupli_floorindex_prediction 
postResample(knn_latitude_nodupli_floorindex_prediction,validation_v9$LATITUDE)
#       RMSE   Rsquared        MAE 
# 15.2888766  0.9530216  8.4852797 
