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
### training 
#hchart(trainingData$BUILDINGID)
trainingData$BUILDINGID<-as.factor(trainingData$BUILDINGID)
trainingData$BUILDINGID <- factor(trainingData$BUILDINGID, levels= c(0,1,2), labels= c("TI", "TD", "TC"))
### C: highest frequency on building 3 (almost the doble of the other two) 

#hchart(trainingData$BUILDINGID)
#hchart(validationData$BUILDINGID)

### validation
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
#hchart(trainingData$LATITUDE)
#summary(trainingData$LATITUDE)
### C: quite normally distributed - a little skewed to the right 
### C: highest frequency 4864840,4864850 (~1850 obs)
### C: 2 outliers: 4864930,4864940 (1620 obs) and 4864980,4864990 (~980 obs)
### C: min - 4864746; max - 4865017

# validation
#hchart(validationData$LATITUDE)
#summary(validationData$LATITUDE)
### C: quite normally distributed - a little skewed to the left 
### C: min - 4864748; max - 4865017 - similar range 


### $ LONGITUDE ####
# training
#hchart(trainingData$LONGITUDE)
#summary(trainingData$LONGITUDE)
### C. weird distribution 
### C: 2 outliers: -7640, -7620 (1680 obs) and -7380, -7360 (2160 obs)
### C: min: -7691; max: -7301 

# validation
#hchart(validationData$LONGITUDE)
#summary(validationData$LONGITUDE)
### C: weird distribution - more uniform with 1 outlier
### C: 1 outlier : -7650, -7600 (~350 obs)
### C: min: -7696; max: -7300 - similar to training's range

#### C.2 Explore relationship between non wap variables ####

#### floor abd relative position
#qplot(trainingData$FLOOR, trainingData$RELATIVEPOSITION)
#qplot(trainingData$SPACEID, trainingData$RELATIVEPOSITION)
### C: no comment

#### longitude and latitude
#qplot(trainingData$LONGITUDE, trainingData$LATITUDE)
#qplot(validationData$LONGITUDE, validationData$LATITUDE)
### C: seem to have mapped all building 

####longitude and latitude, with floor, building and relativeposition 

## training 
ggplot(data=trainingData, aes(x=LONGITUDE, y=LATITUDE)) + geom_point()+facet_wrap(~BUILDINGID)
### C: buildings are isolated 

#ggplot(data=trainingData, aes(x=LONGITUDE, y=LATITUDE)) + geom_point()+facet_grid(~FLOOR)
#ggplot(data=trainingData, aes(x=LONGITUDE, y=LATITUDE)) + geom_point()+facet_wrap(~FLOOR)
### C: 4th floor only on building 3

#ggplot(data=trainingData, aes(x=LONGITUDE, y=LATITUDE)) + geom_point()+facet_wrap(~RELATIVEPOSITION)


#ggplot_coordinates <- ggplot(data=trainingData, aes(x=LONGITUDE, y=LATITUDE)) + geom_point()


# ggplot_coordinates + geom_point(size=4, aes(color = ifelse(LONGITUDE < -7580, "TI",
  #                                    ifelse( LONGITUDE < - 7400 & LATITUDE > 4864830, "TD", "TC")))) +
   # scale_color_manual(values = c("TI" = "red", "TD" = "purple", "TC" = "blue"),
    #          name = "Buildings")

 ggplot_coordinates+ geom_point(aes(color = ifelse(FLOOR == 0, "0",
                                                        ifelse(FLOOR ==1, "1",
                                                                ifelse(FLOOR == 2, "2", 
                                                                       ifelse(FLOOR == 3, "3","4"))))))+
          scale_color_manual(values = c("1"= "red", "2"="purple", "3"="pink", "0"="orange", "4"="yellow"), name="Floors")+facet_wrap(~BUILDINGID + FLOOR)


## validation
# ggplot_coordinates_valid <- ggplot(data=validationData, aes(x=LONGITUDE, y=LATITUDE)) + geom_point()

ggplot_coordinates_valid + geom_point(aes(color = ifelse(FLOOR == 0, "0",
                                                         ifelse(FLOOR ==1, "1",
                                                                 ifelse(FLOOR == 2, "2", 
                                                                       ifelse(FLOOR == 3, "3","4"))))))+
 scale_color_manual(values = c("1"= "red", "2"="purple", "3"="pink", "0"="orange", "4"="yellow"), name="Floors")+facet_wrap(~BUILDINGID + FLOOR)


#### SPACE ID and FLOOR and BUILDINGID and RELATIVE POSITION 

# qplot(trainingData$SPACEID, trainingData$FLOOR)
# qplot(trainingData$SPACEID, trainingData$RELATIVEPOSITION)
# qplot(trainingData$SPACEID, trainingData$BUILDINGID)


#### C.3 Explore wap-variables ####
library(dplyr)
#### D.1 New dataset - no waps with 100 only ####

### how many attributes whose min value is 100 
sum(apply(trainingData,2,function(x)(min(x==100))))
### C: 55 waps which min is 100

sum(apply(validationData,2,function(x)(min(x==100))))
### C: 153 waps which min is 100

#### which are they?
apply(trainingData,2,function(x)names(min(x==100)))
### C: 55 waps which min is 100

min100_train <- names(which(apply(trainingData[,1:520],2,function(x) min(x)) == 100))
### C: vector list of waps with 100 // 55 
min100_valid <- names(which(apply(validationData[,1:520],2,function(x)min(x))== 100))
### C: vector list of waps with 100 // 153
min100_total<-c(min100_valid,min100_train)

### training_clean // new data set without waps that only yield 100 - ERRO 
training_clean<-trainingData[,-which(names(trainingData) %in% min100_total)]
str(training_clean)
apply(training_clean,2,function(x)names(min(x==100))) ### NULL  - checked
sum(apply(training_clean,2,function(x)names(min(x==100)))) ### 0 - checked
### C: new dataset - only with 321 variables

### validation_clean // new data set without waps that only yield 100 
validation_clean<-validationData[,-which(names(validationData) %in% min100_total)]
str(validation_clean)
apply(validation_clean,2,function(x)names(min(x==100))) ### NULL  - checked
### C: new dataset - only with 321 variables

#### D.2 New version dataset -105 instead of 100 ####
training_clean[training_clean==100] <- -105

#### D.3 New version of dataset - no obs with 100 only ####
apply(training_clean[,1:312],1,mean) != -105
training_clean <- training_clean[apply(training_clean[,1:312],1,mean)!= -105,]
str(training_clean)
str(trainingData)


#### D.4 Separate dataset by building ####
str(training_clean)
str(training_clean$BUILDINGID)
sum(apply(training_clean[,c(1:312)],2,function(x)min(x=="100"))) ### Checking 
sum(apply(training_clean[,c(1:312)],2,function(x)(x=="-105"))) ### Checking 

train_TI<-training_clean %>% filter(BUILDINGID=="TI")
### C: TI building - 5,249 x 321
train_TD<-training_clean %>% filter(BUILDINGID=="TD")
### C: TD building - 5,196 x 321
train_TC<-training_clean %>% filter(BUILDINGID=="TC")
### C: TC building - 9,492 x 321
### C: I am missing the same split on the validaton dataset 

#### D.5 Sub sample by building and floor ####
### $ TI Building - Floor 0, 1, 2 and 3 
train_TI_F0<-train_TI %>% filter(FLOOR==0)
### C: TI building and floor 0 - 1,059 x 271

train_TI_F1<-train_TI %>% filter(FLOOR==1)
### C: TI building and floor 1 - 1,356 x 271 

train_TI_F2<-train_TI %>% filter(FLOOR==2)
### C: TI building and floor 2 - 1,443 x 271 

train_TI_F3<-train_TI %>% filter(FLOOR==3)
### C: TI building and floor 2 - 1,391 x 271 

### $ TD Building - Floor 0, 1, 2 and 3 
train_TD %>% filter(FLOOR==0)
train_TD_F0<-train_TD %>% filter(FLOOR==0)
### C: TD building and floor 0 - 1,368 x 271

train_TD_F1<-train_TD %>% filter(FLOOR==1)
### C: TD building and floor 1 - 1,484 x 271

train_TD_F2<-train_TD %>% filter(FLOOR==2)
### C: TD building and floor 2 - 1,396  x 271

train_TD_F3<-train_TD %>% filter(FLOOR==3)
### C: TD building and floor 3 - 948  x 271

### $ TC Building - Floor 0, 1, 2, 3 and 4 
train_TC_F0<-train_TC %>% filter(FLOOR==0)
### C: TC building and floor 0 - 1,942 x 271

train_TC_F1<-train_TC %>% filter(FLOOR==1)
### C: TC building and floor 1 - 2,162 x 271

train_TC_F2<-train_TC %>% filter(FLOOR==2)
### C: TC building and floor 2 - 1,577 x 271

train_TC_F3<-train_TC %>% filter(FLOOR==3)
### C: TC building and floor 3 - 2,709 x 271

train_TC_F4<-train_TC %>% filter(FLOOR==4)
### C: TC building and floor 4 - 1,102 x 271

#### E. Explore wap-variables in different datasets #### 

### Latitude and longitude - building and floor 
ggplot(data=train_TI, aes(x=LONGITUDE, y=LATITUDE)) + geom_point()+facet_wrap(~FLOOR)
### No big difference
ggplot(data=train_TD, aes(x=LONGITUDE, y=LATITUDE)) + geom_point()+facet_wrap(~FLOOR)
### A lot of differences - 2nd floor the most complete - 3rd floor the least 
ggplot(data=train_TC, aes(x=LONGITUDE, y=LATITUDE)) + geom_point()+facet_wrap(~FLOOR)
### Some differences - 0, 1 and 2 floor - few observation son south side. 3rd floor is the more complete and 4th floor/ south-east is really bad

### $ signal between -30 to 0 // Building and floor 
sum(apply(training_clean[,c(1:312)],1,function(x) length(which(x > -30 & x <= 0)))) ### 682
sum(apply(train_TI[,c(1:312)],1,function(x) length(which(x > -30 & x <= 0)))) ### 2
sum(apply(train_TD[,c(1:312)],1,function(x) length(which(x > -30 & x <= 0)))) ### 8 
sum(apply(train_TC[,c(1:312)],1,function(x) length(which(x > -30 & x <= 0)))) ### 672
### C: something happening in TC

sum(apply(train_TC_F0[,c(1:312)],1,function(x) length(which(x > -30 & x <= 0)))) ### 3
sum(apply(train_TC_F1[,c(1:312)],1,function(x) length(which(x > -30 & x <= 0)))) ### 2
sum(apply(train_TC_F2[,c(1:312)],1,function(x) length(which(x > -30 & x <= 0)))) ### 0
sum(apply(train_TC_F3[,c(1:312)],1,function(x) length(which(x > -30 & x <= 0)))) ### 372
sum(apply(train_TC_F4[,c(1:312)],1,function(x) length(which(x > -30 & x <= 0)))) ### 295 
### C: something happening in TC - 3rd and 4th floors

which(apply(train_TC_F3[,c(1:312)],2,function(x) length(which(x > -30 & x <= 0)))>0)
which(apply(train_TC_F4[,c(1:312)],2,function(x) length(which(x > -30 & x <= 0)))>0)
### C: most of them are the same // might be linked to the same waps - waps on the same floor?

which(apply(train_TC_F3[,c(1:312)],1,function(x) length(which(x > -30 & x <= 0)))>0)
which(apply(train_TC_F4[,c(1:312)],1,function(x) length(which(x > -30 & x <= 0)))>0)
### C: treat this dataset separately 

### $ signal lower than -90 // Building and floor 
names(which(apply(train_TC[,c(1:312)],2,function(x) length(which(x <= -90)))>0)) ### 121
names(which(apply(train_TD[,c(1:312)],2,function(x) length(which(x <= -90)))>0)) ### 163
names(which(apply(train_TI[,c(1:312)],2,function(x) length(which(x <= -90)))>0)) ### 141

names(which(apply(train_TC_F0[,c(1:312)],2,function(x) length(which(x <= -90)))>0)) ### 67
names(which(apply(train_TC_F1[,c(1:312)],2,function(x) length(which(x <= -90)))>0)) ### 100
names(which(apply(train_TC_F2[,c(1:312)],2,function(x) length(which(x <= -90)))>0)) ### 91
names(which(apply(train_TC_F3[,c(1:312)],2,function(x) length(which(x <= -90)))>0)) ### 115
names(which(apply(train_TC_F4[,c(1:312)],2,function(x) length(which(x <= -90)))>0)) ### 87

names(which(apply(train_TD_F0[,c(1:312)],2,function(x) length(which(x <= -90)))>0)) ### 72
names(which(apply(train_TD_F1[,c(1:312)],2,function(x) length(which(x <= -90)))>0)) ### 133
names(which(apply(train_TD_F2[,c(1:312)],2,function(x) length(which(x <= -90)))>0)) ### 141
names(which(apply(train_TD_F3[,c(1:312)],2,function(x) length(which(x <= -90)))>0)) ### 116

names(which(apply(train_TI_F0[,c(1:312)],2,function(x) length(which(x <= -90)))>0)) ### 93
names(which(apply(train_TI_F1[,c(1:312)],2,function(x) length(which(x <= -90)))>0)) ### 104
names(which(apply(train_TI_F2[,c(1:312)],2,function(x) length(which(x <= -90)))>0)) ### 129
names(which(apply(train_TI_F3[,c(1:312)],2,function(x) length(which(x <= -90)))>0)) ### 114
### no strong conclusion from now 

### $ signal between -90 to -30 // Building and floor 
names(which(apply(train_TI_F0[,c(1:312)],2,function(x) length(which(x >= -90 & x <=-30)))>0)) ### 89
names(which(apply(train_TI_F1[,c(1:312)],2,function(x) length(which(x >= -90 & x <=-30)))>0)) ### 101
names(which(apply(train_TI_F2[,c(1:312)],2,function(x) length(which(x >= -90 & x <=-30)))>0)) ### 112
names(which(apply(train_TI_F3[,c(1:312)],2,function(x) length(which(x >= -90 & x <=-30)))>0)) ### 103

names(which(apply(train_TD_F0[,c(1:312)],2,function(x) length(which(x >= -90 & x <=-30)))>0)) ### 101
names(which(apply(train_TD_F1[,c(1:312)],2,function(x) length(which(x >= -90 & x <=-30)))>0)) ### 110
names(which(apply(train_TD_F2[,c(1:312)],2,function(x) length(which(x >= -90 & x <=-30)))>0)) ### 134
names(which(apply(train_TD_F3[,c(1:312)],2,function(x) length(which(x >= -90 & x <=-30)))>0)) ### 98

names(which(apply(train_TC_F0[,c(1:312)],2,function(x) length(which(x >= -90 & x <=-30)))>0)) ### 70
names(which(apply(train_TC_F1[,c(1:312)],2,function(x) length(which(x >= -90 & x <=-30)))>0)) ### 107
names(which(apply(train_TC_F2[,c(1:312)],2,function(x) length(which(x >= -90 & x <=-30)))>0)) ### 93
names(which(apply(train_TC_F3[,c(1:312)],2,function(x) length(which(x >= -90 & x <=-30)))>0)) ### 103
names(which(apply(train_TC_F4[,c(1:312)],2,function(x) length(which(x >= -90 & x <=-30)))>0)) ### 68
### no conclusion for now

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
summary(training_error)
str(training_error)

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

ggplot(data=training_error, aes(x=LONGITUDE, y=LATITUDE)) + geom_point(aes(color = ifelse(FLOOR == 0, "0",
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
hchart(PHONEID_19$USERID)
### only user 6 
USERID_6 <-training_clean %>% filter(USERID==6)
### C: 980 x 321
#hchart(USERID_6$PHONEID)
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
ggplot(data=USERID_6_error, aes(x=LONGITUDE, y=LATITUDE)) + geom_point() + facet_wrap(~ USERID_6_error$FLOOR + USERID_6_error$BUILDINGID)
### same path without errors - device problem?

### all path - user 14 
ggplot(data=USERID_14, aes(x=LONGITUDE, y=LATITUDE)) + geom_point() + facet_wrap(~ USERID_14$FLOOR + USERID_14$BUILDINGID)
### C: it has only been in floors 0 and 1 of TD and floors 2 and 3 of TC 

### path with errors- user 14 
ggplot(data=USERID_14_error, aes(x=LONGITUDE, y=LATITUDE)) + geom_point() + facet_wrap(~ USERID_14_error$FLOOR + USERID_14_error$BUILDINGID)
#qplot(USERID_14_error$TIMESTAMP)
class(timestamp)

### changing timestamp type 
USERID_14_error$TIMESTAMP_dt<-as.POSIXct(as.numeric(USERID_14_error$TIMESTAMP),origin="1970-01-01",tz="GMT")
#head(USERID_14_error$TIMESTAMP_dt)
#hchart(USERID_14_error$TIMESTAMP_dt)

#ggplot(data = USERID_14_error, aes(x = USERID_14_error$TIMESTAMP_dt, y = USERID_14_error$BUILDINGID)) + geom_point()+facet_wrap(USERID_14_error$FLOOR)
### C: errors happened in the beggining and in the end of its routh / path

### changing timestamp type 
USERID_6_error$TIMESTAMP_dt<-as.POSIXct(as.numeric(USERID_6_error$TIMESTAMP),origin="1970-01-01",tz="GMT")
#head(USERID_6_error$TIMESTAMP_dt)

ggplot(data = USERID_6_error, aes(x = USERID_6_error$TIMESTAMP_dt, y = USERID_6_error$BUILDINGID)) + geom_point()+facet_wrap(USERID_6_error$FLOOR)
### C: errors happened in the beggining and in the end of its routh / path

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

# summary(training_clean_v2)
# str(training_clean)## 19861 obs = 19402+472-13
# str(training_error) ## 472 obs
# str(training_clean_v2) ## 19402 obs. of  321 variables:

## training_clean_2 is the new dataset 

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


#### I. Creating a sample / data partition ####
library(caret)

set.seed(123)
training_sample<-createDataPartition(y=training_clean_v3$BUILDINGID, times = 1,  p=0.10)
class(training_sample) ### list 

## Training model ( sample )
training_c_part_train <- training_clean_v3[training_sample$Resample1,]
str(training_c_part_train) ## 1942 obs. of  316 variables

## Cross validation 
Cross_validation <- trainControl(
  method = "repeatedcv",
  number = 3,
  repeats = 3,
  allowParallel = TRUE)

## Check the distribution of training and testing sets 
hchart(training_c_part_train$FLOOR)
hchart(training_clean_v3$FLOOR)
prop.table(table(training_c_part_train$FLOOR))
prop.table(table(training_clean_v3$FLOOR))
### C: quite balanced distribution of floor

ggplot(data=training_clean_v3, aes(x=LONGITUDE, y=LATITUDE))+geom_point()
ggplot(data=training_c_part_train, aes(x=LONGITUDE, y=LATITUDE))+geom_point()
### C: quite balanced distributions of location (latitude and longitude)

prop.table(table(training_c_part_train$BUILDINGID))
prop.table(table(training_clean_v3$BUILDINGID))
### C: quite balanced distribution of building ID 

#### J. Prepare validation dataset ####
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

#### L. Modelling BUILDING ID - with all waps ####
library("Metrics")
#### $ Models - KNN ####
colnames(training_c_part_train)
is.factor(training_c_part_train$BUILDINGID)

set.seed(123)
Knn_building <- train(BUILDINGID ~ . - LATITUDE - LONGITUDE - FLOOR, 
                      data = training_c_part_train, 
                      method = "knn", 
                      trControl = Cross_validation)
Knn_building ##  K=5 //  accuracy - 0.9953652 kappa - 0.9927903 // 1 min 
saveRDS(Knn_building,file = "KNN_BUILDING.RDS")
KNN_building_prediction <- predict(Knn_building,validation_v3) 
KNN_building_prediction

## Accuracy 
accuracy(KNN_building_prediction, validation_v3$BUILDINGID)
### C: 0.9909991
table(KNN_building_prediction) 
### C:  TI   TD   TC : 529 317 265 
table(validation_v3$BUILDINGID)
### C:  TI   TD   TC : 536 307 268 
### C: TC is quite well predicted only 3 people were not predicted accurately

#### $ Models - SVM Linear 3 ####

set.seed (123)
svm_building <- train(BUILDINGID ~ . - LATITUDE - LONGITUDE - FLOOR, 
                      data = training_c_part_train, 
                      method = "svmLinear3", 
                      trControl = Cross_validation)

svm_building
# cost  Loss  Accuracy   Kappa 
# 0.25  L1    0.9993127  0.9989282
# ~3 min 
saveRDS(svm_building,file = "svm_linear_building.RDS")

svm_building_prediction <- predict(svm_building,validation_v3)
svm_building_prediction 
svm_building_prediction_train <- predict(svm_building,training_clean_v3)

library(Metrics)
accuracy(svm_building_prediction, validation_v3$BUILDINGID)
### C: 1

#### $ Models - SVM Radial ####
svm_building_radial <- train(BUILDINGID ~ . - LATITUDE - LONGITUDE - FLOOR, 
                      data = training_c_part_train, 
                      method = "svmRadial",
                      trControl = Cross_validation)

svm_building_radial
saveRDS(svm_building_radial,file = "svm_radial_building.RDS")
# cost  Loss  Accuracy   Kappa // with all variables
#  0.25  0.9989708  0.9983979

svm_building_radial_prediction <- predict(svm_building_radial,validation_v3)
svm_building_radial_prediction 
library(Metrics)
accuracy(svm_building_radial_prediction, validation_v3$BUILDINGID)
### C:0.9981998
table(svm_building_radial_prediction)
### C:  TI   TD   TC : 534 309 268 
table(validation_v3$BUILDINGID)
### C:  TI   TD   TC : 536 307 268 
### Again, TC has been perfectly predicted, and only two observations' were wrongly predicted. 

#### $ Models - RF ####
set.seed(123)
rf_building <- train(BUILDINGID ~ . - LATITUDE - LONGITUDE - FLOOR, data = training_c_part_train, 
                     method = "rf", ntree=5 ,
                     tuneLength = 10, 
                     trControl = Cross_validation)

saveRDS(rf_building,file = "rf_building.RDS")
### C:
# mtry  Accuracy   Kappa
#   2   0.9697573  0.9525263
#  36   0.9948523  0.9919686

rf_building_prediction <- predict(rf_building,validation_v3)
accuracy(rf_building_prediction, validation_v3$BUILDINGID)
### C: 0.9765977
table(rf_building_prediction) 
### C:  TI   TD   TC : 525 325 261 
table(validation_v3$BUILDINGID) 
### C:  TI   TD   TC : 536 307 268 
### C: worse prediction - not one single class predicted accurately. still TC is the best one and TD the worst. 

#### M. Check variance again ####
nzv<-nearZeroVar(training_clean_v2[,1:312], saveMetrics= TRUE)

nzv[nzv$nzv,][1:10,]
str(nzv)
nzv %>% filter(nzv=="FALSE") ## none that has a variance close to zero 


#### N. Create a new column with max waps ####
summary(training_clean_v3[,1:312])
str(training_clean_v3)
apply(training_clean_v3[,1:312],1,function(x)names(which.max(x)))

training_clean_v4<-training_clean_v3
summary(training_clean_v4)
training_clean_v4$Best_wap <- apply(training_clean_v3[,1:312],1,function(x)names(which.max(x)))
training_clean_v4$Best_wap <-as.factor(training_clean_v4$Best_wap)
summmary(training_clean_v4$Best_wap)
hchart(training_clean_v4$Best_wap)
str(training_clean_v4$Best_wap) ## 222 levels
str(training_clean_v4) ## 19402 obs. of  317 variables:
View(training_clean_v4$Best_wap)

#### O. Modelling BUILDING ID -  with only best_wap variable ####
set.seed(123)
training_sample_2nd<-createDataPartition(y=training_clean_v4$BUILDINGID, times = 1,  p=0.10, list = FALSE)
class(training_sample_2nd) ### list 

## Training model (sample)
training_c_part_train_2nd <- training_clean_v4[training_sample_2nd,]
str(training_c_part_train_2nd) ## 1942 obs. of  317 variables:

## Check the distribution of training and general dataset
# hchart(training_c_part_train_2nd$FLOOR)
# hchart(training_clean_v4$FLOOR)
# prop.table(table(training_c_part_train_2nd$FLOOR))
# prop.table(table(training_clean_v4$FLOOR))
### C: quite balanced distribution of floor

# ggplot(data=training_clean_v4, aes(x=LONGITUDE, y=LATITUDE))+geom_point()
# ggplot(data=training_c_part_train_2nd, aes(x=LONGITUDE, y=LATITUDE))+geom_point()
### C: quite balanced distributions of location (latitude and longitude)

# prop.table(table(training_c_part_train_2nd$BUILDINGID))
# prop.table(table(training_clean_v4$BUILDINGID))
### C: quite balanced distribution of building ID 

# hchart(training_c_part_train_2nd$Best_wap)
# hchart(training_clean_v4$Best_wap)
### C:difficult to check 
# prop.table(table(training_c_part_train_2nd$Best_wap))
# prop.table(table(training_clean_v4$Best_wap))
### C:difficult to compare

## Prepare validation dataset 
validation_v4 <- validation_v3
# str(validation_v4) ## 1111 obs. of  316 variables:
validation_v4$Best_wap <- apply(validation_v4[,1:312],1,function(x)names(which.max(x)))
# str(validation_v4) ##1111 obs. of  317 variables:
validation_v4$Best_wap <- as.factor(validation_v4$Best_wap)
# str(validation_v4$Best_wap) ###  Factor w/ 175 levels

#### Error in model.frame.default(Terms, newdata, na.action = na.action, xlev = object$xlevels) : factor Best_wap has new levels WAP268, WAP323
## In the validation dataset, best_wap has 2 levels that dont exist in the training dataset. 

validation_v4 ## 1,111 x 317
validation_v4.1<-validation_v4 %>% filter(Best_wap!="WAP268")
validation_v4.1<-validation_v4.1 %>% filter(Best_wap!="WAP323")
validation_v4.1 ## 1,109 x 317


#### $ Models - Knn - It didn't work ####
#### $ Models - svmLinear ####
set.seed(123)

svm_linear_building_2nd <- train(BUILDINGID ~ Best_wap, 
                      data = training_clean_v4, 
                      method = "svmLinear", 
                      trControl = Cross_validation)
svm_linear_building_2nd
### Accuracy - 0.9970449   // Kappa - 0.9953929
saveRDS(svm_linear_building_2nd,file = "svm_linear_building_2nd_best_wap.RDS")
svm_building_prediction_2nd <- predict(svm_linear_building_2nd,validation_v4.1)
svm_building_prediction_2nd
accuracy(svm_building_prediction_2nd, validation_v4.1$BUILDINGID)
### C: accuracy = 1

#### $ Models - svmRadial - I had to stop it ####
set.seed(123)
svm_radial_building_2nd <- train(BUILDINGID ~ Best_wap, 
                          data = training_clean_v4, 
                          method = "svmRadial", 
                          trControl = Cross_validation)

#### $ Models - rf - I had to stop it  ####
set.seed(123)
rf_building_2nd <- train(BUILDINGID ~ Best_wap, 
                                 data = training_clean_v4, 
                                 method = "rf", ntree=5 ,
                         tuneLength = 10, 
                                 trControl = Cross_validation)

#### P. Building prediction added to datasets
validation_v3$BuildingID_Pred <- svm_building_prediction
summary(validation_v3)

training_clean_v3$BuildingID_Pred <- svm_building_prediction_train
summary(training_clean_v3)

#### P. Split the dataset by building ####
# TI 
training_TI_v3<-training_clean_v3 %>% filter(BUILDINGID=="TI")
# summary(training_TI_v3)
validation_TI_v3<-validation_v3 %>% filter(BUILDINGID=="TI")
# summary(validation_TI_v3)

# TD
training_TD_v3<-training_clean_v3 %>% filter(BUILDINGID=="TD")
# summary(training_TD_v3)
validation_TD_v3<-validation_v3 %>% filter(BUILDINGID=="TD")
# summary(validation_TD_v3)

# TC
training_TC_v3<-training_clean_v3 %>% filter(BUILDINGID=="TC")
summary(training_TC_v3)
validation_TC_v3<-validation_v3 %>% filter(BUILDINGID=="TC")
# summary(validation_TC_v3)

#### Q. Create an index for floor ####
training_clean_v4$FLOORINDEX <- paste0(training_clean_v4$BUILDINGID, training_clean_v4$FLOOR)
validation_v4$FLOORINDEX <- paste0(validation_v4$BUILDINGID, validation_v4$FLOOR)
validation_v4.1$FLOORINDEX <- paste0(validation_v4.1$BUILDINGID, validation_v4.1$FLOOR)


training_clean_v4$FLOORINDEX <- as.factor(training_clean_v4$FLOORINDEX)
validation_v4$FLOORINDEX <- as.factor(validation_v4$FLOORINDEX)
validation_v4.1$FLOORINDEX <- as.factor(validation_v4.1$FLOORINDEX)

# summary(training_clean_v4$FLOORINDEX)
# summary(validation_v4.1$FLOORINDEX)
# summary(validation_v4$FLOORINDEX)

#### R. Modelling - FLOOR  ####
#### S. Create sample ####

set.seed(123)
training_sample_floor<-createDataPartition(y=training_clean_v4$FLOORINDEX, times = 2,  p=0.10)
class(training_sample_floor) ### list 

## Training model ( sample )
training_floor_train <- training_clean_v4[training_sample_floor$Resample1,]
testing_floor_train <- training_clean_v4[training_sample_floor$Resample2,]
str(training_floor_train) ## 1942 obs. of  316 variables
summary(training_floor_train)


############ IGNORE THIS PART - validation with minus 2 observations ############
#### T. Modelling FLOOR with Building_pred X best_wap #### 
#### $ KNN ####
summary(training_clean_v4)

set.seed(123)
Knn_floor <- train(FLOORINDEX ~ Best_wap + BUILDINGID, 
                   data = training_floor_train, 
                   method = "knn", 
                   trControl = Cross_validation, 
                   preProcess= c("center","scale"))

Knn_floor ## k = 5 / accuracy -  0.7635485 kappa - 0.7635485
saveRDS(Knn_floor,file = "knn_floor_real.RDS")
Knn_floor_prediction <- predict(Knn_floor,validation_v4.1) 
Knn_floor_prediction

## Accuracy 
accuracy(Knn_floor_prediction, validation_v4.1$FLOORINDEX) 
### C: 0.7321912

#### $ SVM Radial ####
set.seed(123)
svm_Radial_floor <- train(FLOORINDEX ~ Best_wap + BUILDINGID, 
                          data = training_floor_train, 
                          method = "svmRadial", 
                          trControl = Cross_validation, 
                          preProcess= c("center","scale"))

svm_Radial_floor ## C - 1.00  Accuracy - 0.8722963  Kappa - 0.8722963
saveRDS(svm_Radial_floor,file = "svm_Radial_floor.RDS")
svm_Radial_floor_prediction <- predict(svm_Radial_floor,validation_v4.1) 
svm_Radial_floor_prediction

## Accuracy 
accuracy(svm_Radial_floor_prediction, validation_v4.1$FLOORINDEX) 
### C:  0.8503156

#### $ SVM Linear ####
set.seed(123)
svm_Linear_floor <- train(FLOORINDEX ~ Best_wap + BUILDINGID, 
                          data = training_floor_train, 
                          method = "svmLinear", 
                          trControl = Cross_validation, 
                          preProcess= c("center","scale"))

svm_Linear_floor ##  0.880513  0.8696097
saveRDS(svm_Linear_floor,file = "svm_Linear_floor.RDS")
svm_Linear_floor_prediction <- predict(svm_Linear_floor,validation_v4.1) 
svm_Linear_floor_prediction

## Accuracy 
accuracy(svm_Linear_floor_prediction, validation_v4.1$FLOORINDEX) 
### C:  0.8593327

#### $ SVM Linear3 ####
set.seed(123)
svm_Linear3_floor <- train(FLOORINDEX ~ Best_wap + BUILDINGID, 
                          data = training_floor_train, 
                          method = "svmLinear3", 
                          trControl = Cross_validation, 
                          preProcess= c("center","scale"))

svm_Linear3_floor ##  0.25  L2 0.8805122  0.8696053
saveRDS(svm_Linear3_floor,file = "svm_Linear3_floor.RDS")
svm_Linear3_floor_prediction <- predict(svm_Linear3_floor,validation_v4.1) 
svm_Linear3_floor_prediction

## Accuracy 
accuracy(svm_Linear3_floor_prediction, validation_v4.1$FLOORINDEX) 
### C: 0.8728584

#### $ RF - BEST ####
set.seed(123)
rf_floor <- train(FLOORINDEX ~ Best_wap + BUILDINGID, 
                           data = training_floor_train, 
                           method = "rf", ntree= ,
                           tuneLength = 10, 
                           trControl = Cross_validation)

rf_floor 
##  mtry  Accuracy   Kappa 
##   2   0.1585595  0.04111637
## 124   0.8763983  0.86515352

saveRDS(rf_floor,file = "rf_floor.RDS")
rf_floor_prediction <- predict(rf_floor,validation_v4.1) 
rf_floor_prediction

## Accuracy 
accuracy(rf_floor_prediction, validation_v4.1$FLOORINDEX) 
### C:  0.8746619



#### U. Modelling FLOOR with Building_pred X best_wap X waps ####
#### $ SVM Linear - with best_wap #### 
set.seed(123)
svm_Linear_floor_2ND <- train(FLOORINDEX ~ . - LATITUDE - LONGITUDE,
                          data = training_floor_train, 
                          method = "svmLinear", 
                          trControl = Cross_validation, 
                          preProcess= c("center","scale"))

svm_Linear_floor_2ND ## 0.9919518  0.991217
saveRDS(svm_Linear_floor_2ND,file = "svm_Linear_floor_2ND_WITH_WAPPS.RDS")
svm_Linear_floor_2ND_prediction <- predict(svm_Linear_floor_2ND,validation_v4.1) 
svm_Linear_floor_2ND_prediction

## Accuracy 
accuracy(svm_Linear_floor_2ND_prediction, validation_v4.1$FLOORINDEX) 
### C: 0.962128

#### $ SVM Linear - without best_wap #### 
set.seed(123)
svm_Linear_floor_2ND_nobest <- train(FLOORINDEX ~ . - LATITUDE - LONGITUDE - Best_wap,
                              data = training_floor_train, 
                              method = "svmLinear", 
                              trControl = Cross_validation, 
                              preProcess= c("center","scale"))

svm_Linear_floor_2ND_nobest ## 0.9988003  0.9986909
saveRDS(svm_Linear_floor_2ND_nobest,file = "svm_Linear_floor_2ND_WITH_WAPPS_NO_BEST_WAP.RDS")
svm_Linear_floor_2ND_NOBEST_prediction <- predict(svm_Linear_floor_2ND_nobest,validation_v4.1) 
svm_Linear_floor_2ND_NOBEST_prediction

## Accuracy 
accuracy(svm_Linear_floor_2ND_NOBEST_prediction, validation_v4.1$FLOORINDEX) 
### C: 0.9783589

#### $ Knn - with best_wap ####
set.seed(123)
Knn_floor_2ND <- train(FLOORINDEX ~ . - LATITUDE - LONGITUDE,
                              data = training_floor_train, 
                              method = "knn", 
                              trControl = Cross_validation, 
                              preProcess= c("center","scale"))
Knn_floor_2ND ## k - 5 ; accuracy - 0.9095899  ; kappa - 0.9013091
saveRDS(Knn_floor_2ND,file = "knn_2ND_WITH_WAPPS.RDS")
knn_floor_2ND_prediction <- predict(Knn_floor_2ND,validation_v4.1) 
knn_floor_2ND_prediction

## Accuracy 
accuracy(knn_floor_2ND_prediction, validation_v4.1$FLOORINDEX) 
### C: 0.8295762

#### $ Knn - without best_wap ####
set.seed(123)
Knn_floor_2ND_nobest <- train(FLOORINDEX ~ . - LATITUDE - LONGITUDE - Best_wap,
                       data = training_floor_train, 
                       method = "knn", 
                       trControl = Cross_validation, 
                       preProcess= c("center","scale"))
Knn_floor_2ND_nobest ## k - 5 ; accuracy - 0.9479473  ; kappa - 0.9431952
saveRDS(Knn_floor_2ND_nobest,file = "knn_2ND_WITH_WAPPS_NO_BEST_WAP.RDS")
knn_floor_2ND_nobest_prediction <- predict(Knn_floor_2ND_nobest,validation_v4.1) 
knn_floor_2ND_nobest_prediction

## Accuracy 
accuracy(knn_floor_2ND_nobest_prediction, validation_v4.1$FLOORINDEX) 
### C: 0.8376916

#### $ SVM Radial - with best_wap ####
set.seed(123)
svm_Radial_floor_2ND <- train(FLOORINDEX ~ . - LATITUDE - LONGITUDE,
                                           data = training_floor_train, 
                                           method = "svmRadial", 
                                           trControl = Cross_validation, 
                                           preProcess= c("center","scale"))

svm_Radial_floor_2ND ## C - 1; accuracy - 0.9508661    ; kappa - 0.9463793
saveRDS(svm_Radial_floor_2ND,file = "svm_Radial_floor_2ND_WAPPS.RDS")
svm_Radial_floor_2ND_prediction <- predict(svm_Radial_floor_2ND,validation_v4.1) 
svm_Radial_floor_2ND_prediction

## Accuracy 
accuracy(svm_Radial_floor_2ND_prediction, validation_v4.1$FLOORINDEX) 
### C: 0.8503156

#### $ SVM Radial - without best_wap ####
set.seed(123)
svm_Radial_floor_2ND_nobest <- train(FLOORINDEX ~ . - LATITUDE - LONGITUDE - Best_wap,
                              data = training_floor_train, 
                              method = "svmRadial", 
                              trControl = Cross_validation, 
                              preProcess= c("center","scale"))

svm_Radial_floor_2ND_nobest ## C - 1; accuracy -  0.9763748   ; kappa -  0.9742237 
saveRDS(svm_Radial_floor_2ND_nobest,file = "svm_Radial_floor_2ND_WAPPS_NO_BEST_WAP.RDS")
svm_Radial_floor_2ND__nobest_prediction <- predict(svm_Radial_floor_2ND_nobest,validation_v4.1) 
svm_Radial_floor_2ND__nobest_prediction 

## Accuracy 
accuracy(svm_Radial_floor_2ND__nobest_prediction, validation_v4.1$FLOORINDEX) 
### C: 0.8683499

#### $ RF - with best_wap - BEST ####
set.seed(123)
rf_floor_2ND <- train(FLOORINDEX ~ . - LATITUDE - LONGITUDE,
                              data = training_floor_train, 
                              method = "rf",ntree= 5,
                              tuneLength = 10, 
                              trControl = Cross_validation)

rf_floor_2ND 
# mtry  Accuracy   Kappa  
##  2   0.4175704  0.3531609
## 12   0.8960583  0.8864647
## 83   0.9967455  0.9964488
saveRDS(rf_floor_2ND,file = "rf_floor_2ND_WAPS.RDS")
rf_floor_2ND_prediction <- predict(rf_floor_2ND,validation_v4.1) 
rf_floor_2ND_prediction

## Accuracy 
accuracy(rf_floor_2ND_prediction, validation_v4.1$FLOORINDEX) 
### C:  1 

#### $ RF - without best_wap - BEST ####
set.seed(123)
rf_floor_2ND_nobest <- train(FLOORINDEX ~ . - LATITUDE - LONGITUDE - Best_wap,
                      data = training_floor_train, 
                      method = "rf",ntree= 5,
                      tuneLength = 10, 
                      trControl = Cross_validation)

rf_floor_2ND_nobest 
# mtry  Accuracy   Kappa  
##  2   0.5992941  0.5595545
## 37   0.9952055  0.9947686
##212   1.0000000  1.0000000
saveRDS(rf_floor_2ND_nobest ,file = "rf_floor_2ND_WAPS_NO_BEST_WAP.RDS")
rf_floor_2ND__nobest_prediction <- predict(rf_floor_2ND_nobest,validation_v4.1) 
rf_floor_2ND__nobest_prediction

## Accuracy 
accuracy(rf_floor_2ND__nobest_prediction, validation_v4.1$FLOORINDEX) 
### C:  1 

#### V. Modelling FLOOR with waps only ####
#### $ RF ####
set.seed(123)
rf_floor_3rD <- train(FLOORINDEX ~ . - LATITUDE - LONGITUDE - BUILDINGID - Best_wap,
                              data = training_floor_train, 
                              method = "rf", ntree= 5,
                              tuneLength = 10, 
                              trControl = Cross_validation)

rf_floor_3rD
##   mtry  Accuracy   Kappa    
##    2   0.6827964  0.5847937
##   37   0.9986293  0.9982189
##  180   1.0000000  1.0000000

saveRDS(rf_floor_3rD,file="rf_floor_3rD_ONLY_WAPS.RDS")

rf_floor_3rD_prediction <- predict(rf_floor_3rD, validation_v4.1)
rf_floor_3rD_prediction
accuracy(rf_floor_3rD_prediction,validation_v4.1$FLOORINDEX) ## 0.9693417

##### TESTS #### 

### took the Best_wap column 
training_clean_v5 <- training_clean_v4
training_clean_v5$Best_wap <- NULL
summary(training_clean_v5)

validation_v5 <- validation_v4
validation_v5$Best_wap <- NULL
summary(validation_v5)


set.seed(123)
training_sample_floor_test<-createDataPartition(y=training_clean_v5$FLOORINDEX, times = 1,  p=0.10)
class(training_sample_floor_test) ### list 

## Training model ( sample )
training_floor_train_test <- training_clean_v5[training_sample_floor_test$Resample1,]
str(training_floor_train_test) ## 1942 obs. of  316 variables
summary(training_floor_train_test)

#### RF - Only with building #### 
set.seed(123)
rf_floor_test <- train(FLOORINDEX ~ BUILDINGID, 
                  data = training_floor_train_test, 
                  method = "rf", ntree=5,
                  tuneLength = 10, 
                  trControl = Cross_validation)

rf_floor_test
## Accuracy   Kappa 
## 0.2742691  0.1951056

saveRDS(rf_floor_test,file = "rf_floor_test.RDS")
rf_floor_prediction_test <- predict(rf_floor_test,validation_v5) 
rf_floor_prediction_test

## Accuracy 
accuracy(rf_floor_prediction_test, validation_v5$FLOORINDEX) 
### C:   0.3132313

#### RF - Only with building and waps #### 
set.seed(123)
colnames(training_floor_train_test)

rf_floor_test_2 <- train(FLOORINDEX ~. - LATITUDE - LONGITUDE - FLOOR, 
                       data = training_floor_train_test, 
                       method = "rf", ntree=5,
                       tuneLength = 10, 
                       trControl = Cross_validation)

rf_floor_test_2

saveRDS(rf_floor_test_2,file = "rf_floor_test_2.RDS")
rf_floor_prediction_test_2 <- predict(rf_floor_test_2,validation_v5) 
rf_floor_prediction_test_2

## Accuracy 
accuracy(rf_floor_prediction_test_2, validation_v5$FLOORINDEX) ### 1

str(validationData) ### 1111 obs. of  529 variables:
str(validation_v5) ### 1111 obs. of  317 variables:
############ redoing again ############
#### Modelling FLOORINDEX // BUILDING and WAPS ####
#### $ RF ####
colnames(training_floor_train_test)
training_sample_floor_test

set.seed(123)
rf_floor_waps_build <- train(FLOORINDEX ~. - LATITUDE - LONGITUDE - FLOOR, 
                         data = training_floor_train_test, 
                         method = "rf", ntree=5,
                         tuneLength = 10, 
                         trControl = Cross_validation)

rf_floor_waps_build
###  mtry  Accuracy   Kappa    
###     2   0.5718354  0.5292448
###    36   0.9138932  0.9059795
###   106   0.9210782  0.9138311


saveRDS(rf_floor_waps_build,file = "rf_floor_waps_build.RDS")
rf_floor_waps_build_prediction<- predict(rf_floor_waps_build,validation_v5) 
rf_floor_waps_build_prediction

## Accuracy 
accuracy(rf_floor_waps_build_prediction, validation_v5$FLOORINDEX) ### 0.8289829

#### $ KNN ####

set.seed(123)
knn_floor_waps_build <- train(FLOORINDEX ~. - LATITUDE - LONGITUDE - FLOOR, 
                         data = training_floor_train_test, 
                         method = "knn", 
                         trControl = Cross_validation,
                         preProcess= c("center","scale"))

knn_floor_waps_build 
### K= 5  acc= 0.9297925  kappa= 0.9233677

saveRDS(knn_floor_waps_build,file = "knn_floor_waps_build .RDS")
knn_floor_waps_build_prediction <- predict(knn_floor_waps_build,validation_v5) 
knn_floor_waps_build_prediction

## Accuracy 
accuracy(knn_floor_waps_build_prediction, validation_v5$FLOORINDEX) ### 0.8118812
