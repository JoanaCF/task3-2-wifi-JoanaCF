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

dim(trainingData)
1-19402/19937

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

#### --------------------- Distinct from script 3.1 --------------------- ####
#### M. Unique Position identifiers ####
# TRAINING 
training_clean_v3.1 <- training_clean_v3

training_clean_v3.1$Unique_position <- paste(training_clean_v3$LONGITUDE, training_clean_v3$LATITUDE, training_clean_v3$FLOOR)
training_clean_v3.1$Unique_position <- as.factor(training_clean_v3.1$Unique_position)
levels(training_clean_v3.1$Unique_position) ### only 933 levels

training_unique_points_v2 <- training_clean_v3.1[!duplicated(training_clean_v3.1$Unique_position),]
dim(training_clean_v3.1)
dim(training_unique_points_v2) ## 933 317
dim(training_unique_points) ## 933 317

# VALIDATION 
validation_v3.1 <- validation_v3
validation_v3.1$Unique_position <- paste(validation_v3$LONGITUDE, validation_v3$LATITUDE, validation_v3$FLOOR)
validation_v3.1$Unique_position <- factor(validation_v3.1$Unique_position)
str(validation_v3.1$Unique_position) # 1061 levels

validation_unique_points <- validation_v3.1[!duplicated(validation_v3.1$Unique_position),] ### 1061  317

ggplot(data= training_unique_points_v2, aes(x=LONGITUDE, y=LATITUDE)) + geom_point() + facet_wrap(training_unique_points_v2$FLOOR)
ggplot(data=validation_unique_points, aes(x=LONGITUDE, y=LATITUDE)) + geom_point()+ facet_wrap(validation_unique_points$FLOOR)

#### N. Duplicated rows ####
sum(duplicated(training_clean_v3)) ## 715 duplicated
# sum(duplicated(validation_v3)) ## 0 duplicated
duplicated_rows<-training_clean_v3[duplicated(training_clean_v3),]

dim(training_clean_v3) ## 19402   318 / 19402   317 / 19402   316
dim(training_clean_v3[duplicated(training_clean_v3),]) ## 715 318 /  715 317 /  715 316
dim(training_clean_v3[!duplicated(training_clean_v3),]) ## 18687   318 / 18687   317 / 18687   316
training_clean_v7 <- training_clean_v3[!duplicated(training_clean_v3),] 
dim(training_clean_v7)  # /  18687   317 / 18687   316
dim(validation_v3) # / 1111  317 / 1111  316
validation_v7 <- validation_v3

# (continues on the wifi_3.3 script)

#### O. Creating training dataset - data partition ####
training_sample_buildingid_v7<-createDataPartition(y=training_clean_v7$BUILDINGID,  p=0.10)
class(training_sample_buildingid_v7) ### list 

## Training model ( sample )
training_c7_part_buildingID <- training_clean_v7[training_sample_buildingid_v7$Resample1,]
dim(training_c7_part_buildingID) ## 1870  316
colnames(training_c7_part_buildingID) ## 312 waps + long + lat + floor + buildingID + !(BuildingID_Pred)

## Cross validation 
Cross_validation <- trainControl(
  method = "repeatedcv",
  number = 3,
  repeats = 3,
  allowParallel = TRUE)

#### P. 1ST APPROACH - Modelling BUILDINGID ####
######## $ Svm Linear 3 ####
# with duplicates: best model: svm linear / accuracy 1 
# set.seed (123)
# svm_building_nodupli <- train(BUILDINGID ~ . - LATITUDE - LONGITUDE - FLOOR - BuildingID_Pred, 
  # data = training_c7_part_buildingID, 
   #              method = "svmLinear3", 
    #         trControl = Cross_validation)

# svm_building_nodupli
# cost  Loss  Accuracy   Kappa 
# 0.25  L1    1          1

# save(svm_building_nodupli,file = "svm_building_nodupli.Rdata")
load("svm_building_nodupli.Rdata")
svm_building_nodupli_prediction <- predict(svm_building_nodupli,validation_v7)

library(Metrics)
accuracy(svm_building_nodupli_prediction , validation_v7$BUILDINGID) #1 !(0.9990999)!
confusionMatrix(data=svm_building_nodupli_prediction , validation_v7$BUILDINGID)
#### Q. Modelling FLOORINDEX ####
######## Q.1 Creating FLOORINDEX ####
training_clean_v8 <- training_clean_v7
validation_v8 <- validation_v7
training_clean_v8$FLOORINDEX <- paste0(training_clean_v8$BUILDINGID, training_clean_v8$FLOOR)
validation_v8$FLOORINDEX <- paste0(validation_v8$BUILDINGID, validation_v8$FLOOR)

training_clean_v8$FLOORINDEX <- as.factor(training_clean_v8$FLOORINDEX)
validation_v8$FLOORINDEX <- as.factor(validation_v8$FLOORINDEX)

colnames(training_clean_v8)
colnames(validation_v8)

######## Q.2 Creating training dataset for floorindex ####
dim(training_clean_v8)
colnames(training_clean_v8)
training_clean_v8$BuildingID_Pred <- NULL
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

######## Q.3 Model // building + waps ####
######## $ Svm Linear ####
set.seed(123)
svmLinear_floorindex_waps_build_nodupli <- train(FLOORINDEX ~. - LATITUDE - LONGITUDE - FLOOR, 
data = training_c8_part_floorindex, 
 method = "svmLinear", 
trControl = Cross_validation, 
 preProcess= c("center","scale"))

svmLinear_floorindex_waps_build_nodupli
### acc= 0.9763515  kappa = 0.9741264

save(svmLinear_floorindex_waps_build_nodupli ,file = "svmLinear_floorindex_waps_build_nodupli .Rdata")
load("svmLinear_floorindex_waps_build_nodupli .Rdata")
svmLinear_floorindex_waps_build_nodupli_prediction <- predict(svmLinear_floorindex_waps_build_nodupli,validation_v8) 

## Accuracy 
accuracy(svmLinear_floorindex_waps_build_nodupli_prediction, validation_v8$FLOORINDEX) ### 0.8749     
confusionMatrix(data=svmLinear_floorindex_waps_build_nodupli_prediction, validation_v8$FLOORINDEX)

### C: SVM linear 3 (accuracy of 84%)

######## $ RF ####
# set.seed(123)
# rf_floorindex_waps_build_nodupli <- randomForest::randomForest(FLOORINDEX ~. - LATITUDE - LONGITUDE - FLOOR,
  #                                                           data = training_c8_part_floorindex, 
   #                                                         ntree=100,
    #                                                       tuneLength = 20, 
     #                                                     trControl = Cross_validation )


# rf_floorindex_waps_build_nodupli
### acc= 0.9763515  kappa = 0.9741264

# save(rf_floorindex_waps_build_nodupli,file = "rf_floorindex_waps_build_nodupli.Rdata")
load("rf_floorindex_waps_build_nodupli.Rdata")
rf_floorindex_waps_build_nodupli_prediction <- predict(rf_floorindex_waps_build_nodupli,validation_v8) 

## Accuracy 
accuracy(rf_floorindex_waps_build_nodupli_prediction, validation_v8$FLOORINDEX) ###  0.8704    
confusionMatrix(data=rf_floorindex_waps_build_nodupli_prediction, validation_v8$FLOORINDEX)
### C: SVM linear 3 (accuracy of 84%)

######## $ KNN ####
# set.seed(123)
# knn_floorindex_waps_build_nodupli <- train(FLOORINDEX ~. - LATITUDE - LONGITUDE - FLOOR,
  #                                        data = training_c8_part_floorindex, 
   #                                        method = "knn", 
    #                                       trControl = Cross_validation, 
     #                                      preProcess= c("center","scale"))


# knn_floorindex_waps_build_nodupli
### acc= 0.9228519  kappa =0.9155929

# save(knn_floorindex_waps_build_nodupli,file = "knn_floorindex_waps_build_nodupli.Rdata")
load("knn_floorindex_waps_build_nodupli.Rdata")
knn_floorindex_waps_build_nodupli_prediction <- predict(knn_floorindex_waps_build_nodupli,validation_v8) 

## Accuracy 
accuracy(knn_floorindex_waps_build_nodupli_prediction, validation_v8$FLOORINDEX) ### 0.7975    
confusionMatrix(data=knn_floorindex_waps_build_nodupli_prediction, validation_v8$FLOORINDEX)
### C: SVM linear (accuracy of 84%)

######## Q.4 Try to replicate best model to predict floor , instead of floorindex ####
# set.seed(123)
# svmLinear_floor_waps_build_nodupli <- train(FLOOR ~. - LATITUDE - LONGITUDE - FLOORINDEX, 
  #                                               data = training_c8_part_floorindex, 
   #                                              method = "svmLinear", 
    #                                             trControl = Cross_validation, 
     #                                            preProcess= c("center","scale"))

# svmLinear_floor_waps_build_nodupli 
### acc= 0.9763515  kappa = 0.9741264

# save(svmLinear_floor_waps_build_nodupli  ,file = "svmLinear_floor_waps_build_nodupli.Rdata")
load("svmLinear_floor_waps_build_nodupli.Rdata")
svmLinear_floor_waps_build_nodupli_prediction <- predict(svmLinear_floor_waps_build_nodupli,validation_v8) 

## Accuracy 
accuracy(svmLinear_floor_waps_build_nodupli_prediction, validation_v8$FLOOR) ### 0.8757876    
confusionMatrix(data=svmLinear_floor_waps_build_nodupli_prediction, validation_v8$FLOOR)

######## Q.5 Try to replicate best model to predict floorindex only with waps ####
######## $ SVM Linear - BEST ####
# set.seed(123)
# svmLinear_floorindex_onlywaps_build_nodupli <- train(FLOORINDEX ~. - LATITUDE - LONGITUDE - FLOOR - BUILDINGID, 
  #                                               data = training_c8_part_floorindex, 
   #                                              method = "svmLinear", 
    #                                             trControl = Cross_validation, 
     #                                            preProcess= c("center","scale"))

# svmLinear_floorindex_onlywaps_build_nodupli
### acc= 0.9756407 kappa= 0.973347

# save(svmLinear_floorindex_onlywaps_build_nodupli ,file = "svmLinear_floorindex_onlywaps_build_nodupli.Rdata")
load("svmLinear_floorindex_onlywaps_build_nodupli.Rdata")
svmLinear_floorindex_onlywaps_build_nodupli_prediction <- predict(svmLinear_floorindex_onlywaps_build_nodupli,validation_v8) 

## Accuracy 
accuracy(svmLinear_floorindex_onlywaps_build_nodupli_prediction, validation_v8$FLOORINDEX) ### 0.8803     
confusionMatrix(data=svmLinear_floorindex_onlywaps_build_nodupli_prediction, validation_v8$FLOORINDEX)

######## $ KNN ####
set.seed(123)
knn_floorindex_onlywaps_build_nodupli <- train(FLOORINDEX ~. - LATITUDE - LONGITUDE - FLOOR - BUILDINGID,
                                     data = training_c8_part_floorindex, 
                                        method = "knn", 
                                       trControl = Cross_validation, 
                                      preProcess= c("center","scale"))


knn_floorindex_onlywaps_build_nodupli
### acc= 0.9196429  kappa 0.9120693

# save(knn_floorindex_onlywaps_build_nodupli,file = "knn_floorindex_onlywaps_build_nodupli.Rdata")
load("knn_floorindex_onlywaps_build_nodupli.Rdata")
knn_floorindex_onlywaps_build_nodupli_prediction <- predict(knn_floorindex_onlywaps_build_nodupli,validation_v8) 

## Accuracy 
accuracy(knn_floorindex_onlywaps_build_nodupli_prediction, validation_v8$FLOORINDEX) ### 0.7911791
confusionMatrix(data=knn_floorindex_onlywaps_build_nodupli_prediction, validation_v8$FLOORINDEX)

######## $ RF ####
# set.seed(123)
# rf_floorindex_onlywaps_build_nodupli <- randomForest::randomForest(FLOORINDEX ~. - LATITUDE - LONGITUDE - FLOOR - BUILDINGID,
  #                                           data = training_c8_part_floorindex, 
   #                                                         ntree=100,
    #                                                       tuneLength = 20, 
     #                                                     trControl = Cross_validation )

# rf_floorindex_onlywaps_build_nodupli 

# save(rf_floorindex_onlywaps_build_nodupli,file = "rf_floorindex_onlywaps_build_nodupli.Rdata")
load("rf_floorindex_onlywaps_build_nodupli.Rdata")
rf_floorindex_onlywaps_build_nodupli_prediction <- predict(rf_floorindex_onlywaps_build_nodupli,validation_v8) 

## Accuracy 
accuracy(rf_floorindex_onlywaps_build_nodupli_prediction, validation_v8$FLOORINDEX) ### 0.8749
confusionMatrix(data=rf_floorindex_onlywaps_build_nodupli_prediction, validation_v8$FLOORINDEX)
### C: SVM linear 3 (accuracy of 84%)

######## Q.7 Including FLOORINDEX predictions - v9 ####
training_clean_v9 <- training_clean_v8
validation_v9 <- validation_v8
validation_v9$FLOORINDEX <-svmLinear_floorindex_onlywaps_build_nodupli_prediction
dim(validation_v9)
dim(training_clean_v9)
colnames(training_clean_v9)
#### R. Modelling LONGITUDE ####
######## R.1 Creating training dataset for longitude ####
training_sample_long_v9<-createDataPartition(y=training_clean_v9$LONGITUDE,  p=0.10)
class(training_sample_long_v9) ### list 

## Training model (sample)
training_c9_part_longitude <- training_clean_v9[training_sample_long_v9$Resample1,]
dim(training_c9_part_longitude) ## 1871  317
dim(training_clean_v9) ## 18687   317
colnames(training_c9_part_longitude) ## 312 waps + long + lat + floor + buildingID + floorindex

######## R.2 Model // BUILDING + waps + FLOORINDEX ####
######## $ Knn  ####
# knn_longitude_nodupli <- train(LONGITUDE ~ . - FLOOR - LATITUDE,
  #                data = training_c9_part_longitude,
   #          method = "knn",     
    #       preProcess=c("center", "scale"),        
     #    trControl = Cross_validation)

# knn_longitude_nodupli
                           
# save(knn_longitude_nodupli, file="knn_longitude_nodupli.Rdata")
load("knn_longitude_nodupli.Rdata")
knn_longitude_nodupli_prediction <- predict(knn_longitude_nodupli,validation_v9)
knn_longitude_nodupli_prediction
postResample(knn_longitude_nodupli_prediction,validation_v9$LONGITUDE)
#       RMSE   Rsquared        MAE 
#  17.0273735  0.9803324  8.7831506 

######## $ RF - BEST ####
colnames(training_c9_part_longitude)
# set.seed(123)
# rf_longitude_nodupli <-  randomForest::randomForest(LONGITUDE ~ . - FLOOR - LATITUDE,
  #                                        data = training_c9_part_longitude, 
   #                                      ntree=70,
    #                                    tuneLength =30, 
     #                                  trControl = Cross_validation )
                
# rf_longitude_nodupli
# save( rf_longitude_nodupli, file=" rf_longitude_nodupli.Rdata")
load("rf_longitude_nodupli.Rdata")
rf_longitude_nodupli_prediction <- predict( rf_longitude_nodupli,validation_v9)
rf_longitude_nodupli_prediction
postResample(rf_longitude_nodupli_prediction,validation_v9$LONGITUDE)
#       RMSE   Rsquared        MAE 
#  11.2913706  0.9912916  7.4688997 

######## R.3 Model // BUILDING + waps ####
######## $ Knn ####
# knn_longitude_nodupli_nofloorindex <- train(LONGITUDE ~ . - FLOOR - LATITUDE - FLOORINDEX,
  #              data = training_c9_part_longitude,
   #       method = "knn",     
    #   preProcess=c("center", "scale"),        
     # trControl = Cross_validation)

# knn_longitude_nodupli_nofloorindex

# save(knn_longitude_nodupli_nofloorindex, file="knn_longitude_nodupli_nofloorindex.Rdata")
load("knn_longitude_nodupli_nofloorindex.Rdata")
knn_longitude_nodupli_nofloorindex_prediction <- predict(knn_longitude_nodupli_nofloorindex,validation_v9)
knn_longitude_nodupli_nofloorindex_prediction
postResample(knn_longitude_nodupli_nofloorindex_prediction,validation_v9$LONGITUDE)
#       RMSE   Rsquared        MAE 
#  17.0409814  0.9804115  9.0524824 

######## $ RF ####
# rf_longitude_nodupli_nofloorindex <-  randomForest::randomForest(LONGITUDE ~ . - FLOOR - LATITUDE,
  #                                       data = training_c9_part_longitude, 
   #                                      ntree=70,
    #                                    tuneLength = 30, 
     #                                  trControl = Cross_validation )

# rf_longitude_nodupli_nofloorindex
# save( rf_longitude_nodupli_nofloorindex, file="rf_longitude_nodupli_nofloorindex.Rdata")
load("rf_longitude_nodupli_nofloorindex.Rdata")
rf_longitude_nodupli_nofloorindex_prediction <- predict(rf_longitude_nodupli_nofloorindex,validation_v9)
rf_longitude_nodupli_nofloorindex_prediction
postResample(rf_longitude_nodupli_nofloorindex_prediction,validation_v9$LONGITUDE)
#       RMSE   Rsquared        MAE 
#  12.0172380  0.9901385  7.6700758 

######## R.3 Model // FLOORINDEX + waps ####
######## $ Knn        ####
# knn_longitude_nodupli_floorindex <- train(LONGITUDE ~ . - FLOOR - LATITUDE - BUILDINGID - BuildingID_Pred,
  #          data = training_c9_part_longitude,
   #         method = "knn",     
    #        preProcess=c("center", "scale"),        
     #       trControl = Cross_validation)

# knn_longitude_nodupli_floorindex 
# save(knn_longitude_nodupli_floorindex, file="knn_longitude_nodupli_floorindex.Rdata")
load("knn_longitude_nodupli_floorindex.Rdata")
knn_longitude_nodupli_floorindex_prediction <- predict(knn_longitude_nodupli_floorindex,validation_v9)
knn_longitude_nodupli_floorindex_prediction
postResample(knn_longitude_nodupli_floorindex_prediction,validation_v9$LONGITUDE)
#       RMSE   Rsquared        MAE 
# 15.3323960  0.9841318  8.1420201 

######## $ RF         ####
colnames(training_c9_part_longitude)
# rf_longitude_nodupli_nobuild <-  randomForest::randomForest(LONGITUDE ~ . - FLOOR - LATITUDE - BUILDINGID,
  #                                     data = training_c9_part_longitude, 
   #                                   ntree=100,
    #                                tuneLength = 30, 
     #                               trControl = Cross_validation )

# rf_longitude_nodupli_nobuild
# save(rf_longitude_nodupli_nobuild, file="rf_longitude_nodupli_nobuild.Rdata")
load("rf_longitude_nodupli_nobuild.Rdata")
rf_longitude_nodupli_nobuild_prediction <- predict(rf_longitude_nodupli_nobuild,validation_v9)
rf_longitude_nodupli_nobuild_prediction
postResample(rf_longitude_nodupli_nobuild_prediction,validation_v9$LONGITUDE)
#       RMSE   Rsquared        MAE 
#  14.2590746  0.9861286  8.0959430 


#### S. Modelling LATITUDE ####
######## S.1 Creating training dataset for latitude ####
training_sample_lat_v9<-createDataPartition(y=training_clean_v9$LATITUDE,  p=0.10)
class(training_sample_lat_v9) ### list 

## Training model (sample)
training_c9_part_latitude <- training_clean_v9[training_sample_lat_v9$Resample1,]
dim(training_c9_part_latitude) ## 1871  317
dim(training_clean_v9) ## 18687   317
colnames(training_c9_part_latitude) ## 312 waps + long + lat + floor + buildingID + floorindex

######## S.2 Model // BUILDINGID + waps ####
######## $ KNN #### 
# set.seed(123)
# knn_latitude_nodupli<- train(LATITUDE ~ . - FLOOR - LONGITUDE - FLOORINDEX,
  # data = training_c9_part_latitude,
  # method = "knn", 
  # preProcess=c("center", "scale"),  
  # trControl = Cross_validation )

# knn_latitude_nodupli 
#  k  5 RMSE 7.732120     Rsquared 0.9870028    MAE 5.081710

# save(knn_latitude_nodupli, file="knn_latitude_nodupli.Rdata")
load("knn_latitude_nodupli.Rdata")
knn_latitude_nodupli_prediction <- predict(knn_latitude_nodupli,validation_v9)
knn_latitude_nodupli_prediction 
postResample(knn_latitude_nodupli_prediction,validation_v9$LATITUDE)
#       RMSE   Rsquared        MAE 
# 14.6317833  0.9569639  8.5144453 

######## $ SVM Linear ####
# set.seed(123)
# svmLinear_latitude_nodupli<- train(LATITUDE ~ . - FLOOR - LONGITUDE - FLOORINDEX,
  #                         data = training_c9_part_latitude,
    #                      method = "svmLinear", 
     #                    preProcess=c("center", "scale"),  
      #                  trControl = Cross_validation )

# svmLinear_latitude_nodupli
# save(svmLinear_latitude_nodupli, file="knn_latitude_4th.Rdata")
load("svmLinear_latitude_nodupli.Rdata")
svmLinear_latitude_nodupli_prediction <- predict(svmLinear_latitude_nodupli,validation_v9)
svmLinear_latitude_nodupli_prediction 
postResample(svmLinear_latitude_nodupli_prediction,validation_v9$LATITUDE)
#       RMSE   Rsquared        MAE 
# 19.8727510  0.9233298 14.1724920 

######## $ RF ####
# set.seed(123)
# rf_latitude_nodupli<- randomForest::randomForest(LATITUDE ~ . - FLOOR - LONGITUDE - FLOORINDEX,
  #                 data = training_c9_part_latitude,
   #                ntrees = 100,
    #               tuneLength =40, 
     #             trControl = Cross_validation)

# rf_latitude_nodupli
# save(rf_latitude_nodupli, file="rf_latitude_nodupli.Rdata")
load("rf_latitude_nodupli.Rdata")
rf_latitude_nodupli_prediction <- predict(rf_latitude_nodupli,validation_v9)
rf_latitude_nodupli_prediction 
postResample(rf_latitude_nodupli_prediction,validation_v9$LATITUDE)
# RMSE   Rsquared        MAE 
# 10.2222961  0.9798898  7.1050344 


######## S.2 Model // FLOORINDEX + waps ####
######## $ Knn        ####
# knn_latitude_nodupli_floorindex <- train(LATITUDE ~ . - FLOOR - LONGITUDE - BUILDINGID,
  #     data = training_c9_part_latitude,
   #    method = "knn", 
    #   preProcess=c("center", "scale"),  
     #  trControl = Cross_validation )

# knn_latitude_nodupli_floorindex 
# save(knn_latitude_nodupli_floorindex , file="knn_latitude_nodupli_floorindex .Rdata")
load("knn_latitude_nodupli_floorindex .Rdata")
knn_latitude_nodupli_floorindex_prediction <- predict(knn_latitude_nodupli_floorindex,validation_v9)
knn_latitude_nodupli_floorindex_prediction 
postResample(knn_latitude_nodupli_floorindex_prediction,validation_v9$LATITUDE)
#       RMSE   Rsquared        MAE 
# 14.7222790  0.9561323  7.8914757  

######## $ SVM Linear ####
# svm_latitude_nodupli_floorindex <- train(LATITUDE ~ . - FLOOR - LONGITUDE - BUILDINGID,
  # data = training_c9_part_latitude,
   # method = "svmLinear", 
   # preProcess=c("center", "scale"),  
  # trControl = Cross_validation )

# svm_latitude_nodupli_floorindex
# save(svm_latitude_nodupli_floorindex, file="svm_latitude_nodupli_floorindex.Rdata")
load("svm_latitude_nodupli_floorindex.Rdata")
svm_latitude_nodupli_floorindex_prediction <- predict(svm_latitude_nodupli_floorindex,validation_v9)
svm_latitude_nodupli_floorindex_prediction 
postResample(svm_latitude_nodupli_floorindex_prediction,validation_v9$LATITUDE)
#       RMSE   Rsquared        MAE 
# 19.0039959  0.9294503 13.6651296 


######## $ RF         ####
# set.seed(123)
# rf_latitude_nodupli_nobuild<- randomForest::randomForest(LATITUDE ~ . - FLOOR - LONGITUDE - BUILDINGID,
  #                                               data = training_c9_part_latitude,
   #                                              ntrees = 100,
    #                                             tuneLength =40, 
     #                                            trControl = Cross_validation)


# rf_latitude_nodupli_nobuild
# save(rf_latitude_nodupli_nobuild, file="rf_latitude_nodupli_nobuild.Rdata")
load("rf_latitude_nodupli_nobuild.Rdata")
rf_latitude_nodupli_nobuild_prediction <- predict(rf_latitude_nodupli_nobuild,validation_v9)
rf_latitude_nodupli_nobuild_prediction 
postResample(rf_latitude_nodupli_nobuild_prediction,validation_v9$LATITUDE)
# RMSE        Rsquared   MAE 
# 12.4014429  0.9694561  7.5562525 


######## S.2 Model // BUILDING + FLOORINDEX + waps ####
######## $ KNN #### 
# set.seed(123)
# knn_latitude_nodupli_build_floor<- train(LATITUDE ~ . - FLOOR - LONGITUDE,
      # data = training_c9_part_latitude,
      # method = "knn", 
      # preProcess=c("center", "scale"),  
      # trControl = Cross_validation )

# knn_latitude_nodupli_build_floor

# save(knn_latitude_nodupli_build_floor, file="knn_latitude_nodupli_build_floor.Rdata")
load("knn_latitude_nodupli_build_floor.Rdata")
knn_latitude_nodupli_build_floor_prediction <- predict(knn_latitude_nodupli_build_floor,validation_v9)
knn_latitude_nodupli_build_floor_prediction 
postResample(knn_latitude_nodupli_build_floor_prediction,validation_v9$LATITUDE)
#       RMSE   Rsquared        MAE 
# 14.0701450  0.9602915  8.4811411 

######## $ SVM #### 
# set.seed(123)
# svm_latitude_nodupli_build_floor<- train(LATITUDE ~ . - FLOOR - LONGITUDE,
  # data = training_c9_part_latitude,
   # method = "knn", 
    #  preProcess=c("center", "scale"),  
     #   trControl = Cross_validation )

# svm_latitude_nodupli_build_floor

# save(svm_latitude_nodupli_build_floor, file="svm_latitude_nodupli_build_floor.Rdata")
load("svm_latitude_nodupli_build_floor.Rdata")
svm_latitude_nodupli_build_floor_prediction <- predict(svm_latitude_nodupli_build_floor,validation_v9)
svm_latitude_nodupli_build_floor_prediction 
postResample(svm_latitude_nodupli_build_floor_prediction,validation_v9$LATITUDE)
#       RMSE   Rsquared        MAE 
# 14.0701450  0.9602915  8.4811411  

######## $ RF - BEST ####  
# set.seed(123)
# rf_latitude_nodupli_build_floor<- randomForest::randomForest(LATITUDE ~ . - FLOOR - LONGITUDE,
      # data = training_c9_part_latitude,
      #  ntree=80, 
      #    tuneLength =30, 
      # trControl = Cross_validation)

# rf_latitude_nodupli_build_floor
# save(rf_latitude_nodupli_build_floor, file="rf_latitude_nodupli_build_floor.Rdata")
load("rf_latitude_nodupli_build_floor.Rdata")
rf_latitude_nodupli_build_floor_prediction <- predict(rf_latitude_nodupli_build_floor,validation_v9)
rf_latitude_nodupli_build_floor_prediction 
postResample(rf_latitude_nodupli_build_floor_prediction,validation_v9$LATITUDE)
# RMSE   Rsquared        MAE 
# 10.7356416  0.9773162  7.0936005 


#### T. 2ND APPROACH - Modelling FLOOR PER BUILDING DATASET ####
######## T.1 Create separate datasets ####
train_TC<-training_clean_v8 %>% filter(BUILDINGID=="TC")
train_TI<-training_clean_v8 %>% filter(BUILDINGID=="TI")
train_TD<-training_clean_v8 %>% filter(BUILDINGID=="TD")
### C: TD building - 4848  317  (0.26) / TI building - 5241  317 (0.28) / TC building - 8598  317 (0.46)

train_TC$BuildingID_Pred <- NULL
train_TI$BuildingID_Pred <- NULL
train_TD$BuildingID_Pred <- NULL


###### VALIDATION
valid_TC<-validation_v8 %>% filter(BUILDINGID=="TC")
valid_TI<-validation_v8 %>% filter(BUILDINGID=="TI")
valid_TD<-validation_v8 %>% filter(BUILDINGID=="TD")
### C: TC:  268 317  (0.24) / TI: 536 317 (0.48) / TD: 307 317 (0.28)
### C: different distribution 

valid_TC$BuildingID_Pred <- NULL
valid_TI$BuildingID_Pred <- NULL
valid_TD$BuildingID_Pred <- NULL

######## T.2 Model FLOOR based on waps of different datasets #### 
########### TC ####
######## $ SVM 
# svmLinear_floor_TC <- train(FLOOR ~. -LATITUDE - LONGITUDE - BUILDINGID - FLOORINDEX, 
  #                                                   data = train_TC, 
   #                                                  method = "svmLinear", 
    #                                                 trControl = Cross_validation)

# svmLinear_floor_TC
### acc= 0.9947273  kappa = 0.9931456

# save(svmLinear_floor_TC,file = "svmLinear_floor_TC.Rdata")
load("svmLinear_floor_TC.Rdata")
svmLinear_floor_TC_prediction <- predict(svmLinear_floor_TC,valid_TC) 

## Accuracy 
accuracy(svmLinear_floor_TC_prediction, valid_TC$FLOOR) ### 0.9328       
confusionMatrix(data=svmLinear_floor_TC_prediction, valid_TC$FLOOR)

########### TI ####
######## $ SVM 
hchart(train_TI$FLOOR)
train_TI$FLOOR<-as.character(train_TI$FLOOR)
train_TI$FLOOR <- as.factor(train_TI$FLOOR)
levels(train_TI$FLOOR)

valid_TI$FLOOR<-as.character(valid_TI$FLOOR)
valid_TI$FLOOR<-as.factor(valid_TI$FLOOR)
levels(valid_TI$FLOOR)


# svmLinear_floor_TI <- train(FLOOR ~. - LATITUDE - LONGITUDE - BUILDINGID - FLOORINDEX, 
  #                          data = train_TI, 
   #                         method = "svmLinear", 
    #                        trControl = Cross_validation)

# svmLinear_floor_TI
### acc= 0.9877885 kappa= 0.9836515

# save(svmLinear_floor_TI,file = "svmLinear_floor_TI.Rdata")
load("svmLinear_floor_TI.Rdata")
svmLinear_floor_TI_prediction <- predict(svmLinear_floor_TI,valid_TI) 

## Accuracy 
accuracy(svmLinear_floor_TI_prediction, valid_TI$FLOOR) ### 0.9478       
confusionMatrix(data=svmLinear_floor_TI_prediction, valid_TI$FLOOR)


########### TD ####
######## $ SVM 
hchart(train_TD$FLOOR)
train_TD$FLOOR<-as.character(train_TD$FLOOR)
train_TD$FLOOR <- as.factor(train_TD$FLOOR)
levels(train_TD$FLOOR)

valid_TD$FLOOR<-as.character(valid_TD$FLOOR)
valid_TD$FLOOR<-as.factor(valid_TD$FLOOR)
levels(valid_TD$FLOOR)


# svmLinear_floor_TD <- train(FLOOR ~. - LATITUDE - LONGITUDE - BUILDINGID - FLOORINDEX, 
  #                          data = train_TD, 
   #                         method = "svmLinear", 
    #                        trControl = Cross_validation)

# svmLinear_floor_TD
### acc= 0.9947745  kappa = 0.9929764

# save(svmLinear_floor_TD,file = "svmLinear_floor_TD.Rdata")
load("svmLinear_floor_TD.Rdata")
svmLinear_floor_TD_prediction <- predict(svmLinear_floor_TD,valid_TD) 

## Accuracy 
accuracy(svmLinear_floor_TD_prediction, valid_TD$FLOOR) ### 0.7915309       
confusionMatrix(data=svmLinear_floor_TD_prediction, valid_TD$FLOOR)


ggplot(data=train_TD, aes(x=LONGITUDE, y=LATITUDE)) + geom_point()+ facet_wrap(~ train_TD$FLOOR)
ggplot(data=valid_TD, aes(x=LONGITUDE, y=LATITUDE)) + geom_point()+ facet_wrap(~ valid_TD$FLOOR)


######## $ KNN 
# knn_floor_TD <- train(FLOOR ~. - LATITUDE - LONGITUDE - BUILDINGID - FLOORINDEX, 
  #                        data = train_TD, 
   #                      method = "knn", 
    #                    trControl = Cross_validation)

knn_floor_TD
### acc= 0.9947745  kappa = 0.9929764

save(knn_floor_TD,file = "knn_floor_TD.Rdata")
load("knn_floor_TD.Rdata")
knn_floor_TD_prediction <- predict(knn_floor_TD,valid_TD) 

## Accuracy 
accuracy(knn_floor_TD_prediction, valid_TD$FLOOR) ### 0.7752         
confusionMatrix(data=knn_floor_TD_prediction, valid_TD$FLOOR)

ggplot(data=train_TD, aes(x=LONGITUDE, y=LATITUDE)) + geom_point()+ facet_wrap(~ train_TD$FLOOR)
ggplot(data=valid_TD, aes(x=LONGITUDE, y=LATITUDE)) + geom_point()+ facet_wrap(~ valid_TD$FLOOR)


######## T.3 Include FLOOR predictions in respective datasets ####
valid_TC$FLOOR <- svmLinear_floor_TC_prediction
valid_TD$FLOOR <- svmLinear_floor_TD_prediction
valid_TI$FLOOR <- svmLinear_floor_TI_prediction
######## T.4 Model LONGITUDE based on predicted floor and waps in the different datasets ####
########### TC ####
######## $ RF
# colnames(train_TC)
# set.seed(123)
# rf_longitude_TC <-  randomForest::randomForest(LONGITUDE ~ . - LATITUDE - BUILDINGID - FLOORINDEX,
  #                                      data = train_TC, 
   #                                   ntree=100,
    #                                tuneLength =20, 
     #                             trControl = Cross_validation )

# rf_longitude_TC
# save(rf_longitude_TC, file="rf_longitude_TC.Rdata")
load("rf_longitude_TC.Rdata")
rf_longitude_TC_prediction <- predict(rf_longitude_TC,valid_TC)
rf_longitude_TC_prediction
postResample(rf_longitude_TC_prediction,valid_TC$LONGITUDE)
#       RMSE   Rsquared        MAE 
#  10.7037323  0.8870557  7.0089658 (70 trees and tune length 30)
#  10.5724280  0.8896429  6.9497967 (100 trees and tune length 20)

########### TI ####
######## $ RF 
# colnames(train_TI)
# set.seed(123)
# rf_longitude_TI <-  randomForest::randomForest(LONGITUDE ~ . - LATITUDE - BUILDINGID - FLOORINDEX,
 #                                              data = train_TI, 
  #                                             ntree=100,
   #                                            tuneLength =20, 
    #                                           trControl = Cross_validation )

# rf_longitude_TI
# save(rf_longitude_TI, file="rf_longitude_TI.Rdata")
load("rf_longitude_TI.Rdata")
rf_longitude_TI_prediction <- predict(rf_longitude_TI,valid_TI)
rf_longitude_TI_prediction
postResample(rf_longitude_TI_prediction,valid_TI$LONGITUDE)
#       RMSE   Rsquared        MAE 
#    7.0087619 0.9318441 4.6775261  (100 trees and tune length 20)



# common datasets:
# 11.2913706  0.9912916  7.4688997 

########### TD ####
######## $ RF 
colnames(train_TD)
set.seed(123)
rf_longitude_TD <-  randomForest::randomForest(LONGITUDE ~ . - LATITUDE - BUILDINGID - FLOORINDEX,
                                              data = train_TD, 
                                             ntree=100,
                                            tuneLength =20, 
                                           trControl = Cross_validation )

rf_longitude_TD
save(rf_longitude_TD,file="rf_longitude_TD.Rdata")
load("rf_longitude_TD.Rdata")
rf_longitude_TD_prediction <- predict(rf_longitude_TD,valid_TD)
rf_longitude_TD_prediction
postResample(rf_longitude_TD_prediction,valid_TD$LONGITUDE)
#       RMSE   Rsquared        MAE 
#    10.2011955  0.9527722  7.1335756 (100 trees and tune length 20)

# common datasets:
# 11.2913706  0.9912916  7.4688997 

######## T.5 Model LATITUDE based on predicted floor and waps in the different datasets ####
########### TC ####
######## $ RF 
# colnames(train_TC)
# set.seed(123)
# rf_latitude_TC <-  randomForest::randomForest(LATITUDE ~ . - LONGITUDE - BUILDINGID - FLOORINDEX,
  #                                    data = train_TC, 
   #                                ntree=100,
    #                            tuneLength =20, 
      #                       trControl = Cross_validation )

# rf_latitude_TC 
# save(rf_latitude_TC, file="rf_latitude_TC.Rdata")
load("rf_latitude_TC.Rdata")
rf_latitude_TC_prediction <- predict(rf_latitude_TC,valid_TC)
rf_latitude_TC_prediction
postResample(rf_latitude_TC_prediction,valid_TC$LATITUDE)
#       RMSE   Rsquared        MAE 
#    9.0957511 0.9019608 6.3635272 (100 trees and tune length 20)

########### TI ####
######## $ RF 
# colnames(train_TI)
# set.seed(123)
# rf_latitude_TI <-  randomForest::randomForest(LATITUDE ~ . - LONGITUDE - BUILDINGID - FLOORINDEX,
  #                                            data = train_TI, 
   #                                           ntree=100,
    #                                          tuneLength =20, 
     #                                         trControl = Cross_validation )

# rf_latitude_TI 
# save(rf_latitude_TI, file="rf_latitude_TI.Rdata")
load("rf_latitude_TI.Rdata")
rf_latitude_TI_prediction <- predict(rf_latitude_TI,valid_TI)
rf_latitude_TI_prediction
postResample(rf_latitude_TI_prediction,valid_TI$LATITUDE)
#       RMSE   Rsquared        MAE 
#    5.4395317 0.9716765 3.8059218  (100 trees and tune length 20)

########### TD ####
######## $ RF 
# colnames(train_TD)
# set.seed(123)
# rf_latitude_TD <-  randomForest::randomForest(LATITUDE ~ . - LONGITUDE - BUILDINGID - FLOORINDEX,
  #                                            data = train_TD, 
   #                                           ntree=100,
    #                                          tuneLength =20, 
     #                                         trControl = Cross_validation )

# rf_latitude_TD 
# save(rf_latitude_TD, file="rf_latitude_TD.Rdata")
load("rf_latitude_TI.Rdata")
rf_latitude_TD_prediction <- predict(rf_latitude_TD,valid_TD)
rf_latitude_TD_prediction
postResample(rf_latitude_TD_prediction,valid_TD$LATITUDE)
#       RMSE   Rsquared        MAE 
# 10.6867520  0.9091881  7.4728915  (100 trees and tune length 20)

## COMMON DATASET:
# RMSE   Rsquared        MAE 
# 10.7356416  0.9773162  7.0936005 

#### U. 3RD APPROACH - Including Best_wap ####
########### Creating Best_wap and deleting waps 323 and 268 ####
# TRAINING 
# colnames(training_clean_v8)
# dim(training_clean_v8) ## 18687   317
training_clean_v8.1<-training_clean_v8
training_clean_v8.1$Best_wap <- apply(training_clean_v8.1[,1:312],1,function(x)names(which.max(x)))
training_clean_v8.1$Best_wap <-as.factor(training_clean_v8.1$Best_wap)
# summary(training_clean_v8.1$Best_wap)
# levels(training_clean_v8.1$Best_wap) ## 222

# VALIDATION 
# dim(validation_v8) ## 1111  318
validation_v8$BuildingID_Pred <-NULL
validation_v8.1 <- validation_v8 ## 1111  317

validation_v8.1$Best_wap <- apply(validation_v8.1[,1:312],1,function(x)names(which.max(x)))
validation_v8.1$Best_wap <-as.factor(validation_v8.1$Best_wap)
# summary(validation_v8.1$Best_wap)
# levels(validation_v8.1$Best_wap) ## 175 

## REPEAT IT TAKING OUT wap 323 and WAP 268
training_clean_v8.1$WAP323 <- NULL
training_clean_v8.1$WAP268 <- NULL

validation_v8.1$WAP323 <- NULL
validation_v8.1$WAP268 <- NULL

training_clean_v8.1$Best_wap <- apply(training_clean_v8.1[,1:310],1,function(x)names(which.max(x)))
training_clean_v8.1$Best_wap <-as.factor(training_clean_v8.1$Best_wap)

validation_v8.1$Best_wap <- apply(validation_v8.1[,1:310],1,function(x)names(which.max(x)))
validation_v8.1$Best_wap <-as.factor(validation_v8.1$Best_wap)

########### U.2 Model BuildingID ####
########### U.2.1 Create dataset sample ####
training_sample_build<-createDataPartition(y=training_clean_v8.1$BUILDINGID,  p=0.10)
class(training_sample_build) ### list 

## Training model (sample)
training_sample_build_bestwap <- training_clean_v8.1[training_sample_build$Resample1,]
dim(training_sample_build_bestwap) ## 1870  316
dim(training_clean_v8.1) ## 18687   316
colnames(training_sample_build_bestwap) # 310 waps + long + lat + floor + build + floprindex + best_wap


########### $SVM ####
# set.seed(123)
# svm_build_best_wap <- train(BUILDINGID ~ Best_wap,
  #                        data = training_sample_build_bestwap,
   #                        method = "svmLinear", 
      #                      preProcess=c("center", "scale"),  
       #                     trControl = Cross_validation )

# svm_build_best_wap
# save(svm_build_best_wap, file="svm_build_best_wap.Rdata")
load("svm_build_best_wap.Rdata")
svm_build_best_wap_prediction <- predict(svm_build_best_wap,validation_v8.1)
svm_build_best_wap_prediction 
postResample(svm_build_best_wap_prediction,validation_v8.1$BUILDINGID) ## accuracy 0.9829 kappa  0.973   
## ac - 0.9981998 kappa - 0.9971559  (with waps and best_wap and predicted building)
confusionMatrix(data=svm_build_best_wap_prediction,validation_v8.1$BUILDINGID)

########### U.2.1 Including prediction of building ####
validation_v8.1.1 <- validation_v8.1
validation_v8.1.1$BUILDINGID <- svm_build_best_wap_prediction
summary(validation_v8.1.1$BUILDINGID)

########### U.3 Model FLOORINDEX ####
########### U.3.1 Create data sample ####
training_sample_floorindex_best_wap<-createDataPartition(y=training_clean_v8.1$FLOORINDEX,  p=0.10)
class(training_sample_floorindex_best_wap) ### list 

## Training model (sample)
training_sample_floorindex_bestwap <- training_clean_v8.1[training_sample_floorindex_best_wap$Resample1,]
dim(training_sample_floorindex_bestwap) ## 1875  316
dim(training_clean_v8.1) ## 18687   316
colnames(training_sample_floorindex_bestwap) # 310 waps + long + lat + floor + build + floprindex + best_wap

########### $SVM ####
# set.seed(123)
svm_floorindex_best_wap <- train(FLOORINDEX ~ Best_wap + BUILDINGID,
                         data = training_sample_floorindex_bestwap,
                         method = "svmLinear", 
                        preProcess=c("center", "scale"),  
                       trControl = Cross_validation )

svm_floorindex_best_wap
save(svm_floorindex_best_wap, file="svm_floorindex_best_wap.Rdata")
load("svm_floorindex_best_wap.Rdata")
svm_floorindex_best_wap_prediction <- predict(svm_floorindex_best_wap,validation_v8.1.1)
svm_floorindex_best_wap_prediction
postResample(svm_floorindex_best_wap_prediction,validation_v8.1.1$FLOORINDEX) ## accuracy 0.8713   / kappa 0.8555  
## ac - 0.9001   kappa - 0.8883    (with waps and best_wap and predicted building)
confusionMatrix(data=svm_floorindex_best_wap_prediction,validation_v8.1.1$FLOORINDEX)

########### U.3.2 Including prediction of floorindex ####
validation_v8.1.2 <- validation_v8.1.1
validation_v8.1.2$FLOORINDEX <- svm_floorindex_best_wap_prediction
summary(validation_v8.1.2$FLOORINDEX)



########### U.4 Model LONGITUDE ####
########### U.4.1 Create Data sample ####
training_sample_long_best_wap<-createDataPartition(y=training_clean_v8.1$LONGITUDE,  p=0.10)
class(training_sample_long_best_wap) ### list 

## Training model (sample)
training_sample_LONG_bestwap <- training_clean_v8.1[training_sample_long_best_wap$Resample1,]
dim(training_sample_LONG_bestwap) ## 1871  316
dim(training_clean_v8.1) ## 18687   316
colnames(training_sample_LONG_bestwap) # 310 waps + long + lat + floor + build + floprindex + best_wap

########### $RF ####
# set.seed(123)
# rf_longitude_best_wap <-  train(LONGITUDE ~ Best_wap + BUILDINGID + FLOORINDEX,
  #                                     data = training_sample_LONG_bestwap, 
   #                                    method="rf",
    #                                   ntree=50,
     #                                  tuneLength =10, 
      #                                 trControl = Cross_validation )

# rf_longitude_best_wap
# save(rf_longitude_best_wap, file="rf_longitude_best_wap.Rdata")
load("rf_longitude_best_wap.Rdata")
rf_longitude_best_wap_prediction <- predict(rf_longitude_best_wap,validation_v8.1.2)
rf_longitude_best_wap_prediction
postResample(rf_longitude_best_wap_prediction,validation_v8.1.2$LONGITUDE)
#       RMSE   Rsquared        MAE 
#    26.72027   0.95114     12.38980 (ntrees 10 / tuneLength 10)
#    26.1781929  0.9530235 12.2461255 (ntrees 50 / tuneLength 10)

########### U.4 Model LATITUDE ####
########### U.4.1 Create Data sample ####
training_sample_LATIT_best_wap<-createDataPartition(y=training_clean_v8.1$LATITUDE,  p=0.10)
class(training_sample_LATIT_best_wap) ### list 

## Training model (sample)
training_sample_LATIT_bestwap <- training_clean_v8.1[training_sample_LATIT_best_wap$Resample1,]
dim(training_sample_LATIT_bestwap) ## 1871  316
dim(training_clean_v8.1) ## 18687   316
colnames(training_sample_LATIT_bestwap) # 310 waps + long + lat + floor + build + floprindex + best_wap

########### RF ####
set.seed(123)
  rf_latit_best_wap <-  train(LATITUDE ~ Best_wap + BUILDINGID + FLOORINDEX,
                                     data = training_sample_LATIT_bestwap, 
                                    method="rf",
                                   ntree=50,
                                  tuneLength =10, 
                                 trControl = Cross_validation )

rf_latit_best_wap
save(rf_latit_best_wap, file="rf_latit_best_wap.Rdata")
load("rf_latit_best_wap.Rdata")
rf_latit_best_wap_prediction <- predict(rf_latit_best_wap,validation_v8.1.2)
rf_latit_best_wap_prediction
postResample(rf_latit_best_wap_prediction,validation_v8.1.2$LATITUDE)
#       RMSE      Rsquared        MAE 
#    18.8833196  0.9299748 10.2736053  (ntrees 10 / tuneLength 10)
