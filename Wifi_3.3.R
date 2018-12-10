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

### $ TIMESTAMP - new variable set as.POSIXlt ####
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


### $ PHONEID - changed data type to factor ####
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


### $ USERID - changed data type to factor ####
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


### $ RELATIVEPOSITION - changed data type to factor ONLY IN THE TRAINING DATASET ####
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


### $ SPACEID - changed data type to factor ####
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


### $ BUILDINGID - changed data type and labels into 1,2,3 ####
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


### $ FLOOR - changed data type to factor ####  
### training 
#hchart(trainingData$FLOOR)
trainingData$FLOOR <- as.factor(trainingData$FLOOR)
### C: similar frequency from 0 to 3 floor, and more than 4 times less on 4th floor

### validation 
#hchart(validationData$FLOOR)
validationData$FLOOR <- as.factor(validationData$FLOOR)
### C: least frequent - 0 and 4th floor. most frequent 1st floor

### $ LATITUDE - no change, for now ####
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


### $ LONGITUDE - no change ####
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
#ggplot(data=trainingData, aes(x=LONGITUDE, y=LATITUDE)) + geom_point()+facet_wrap(~BUILDINGID)
### C: buildings are isolated 

#ggplot(data=trainingData, aes(x=LONGITUDE, y=LATITUDE)) + geom_point()+facet_grid(~FLOOR)
#ggplot(data=trainingData, aes(x=LONGITUDE, y=LATITUDE)) + geom_point()+facet_wrap(~FLOOR)
### C: 4th floor only on building 3

#ggplot(data=trainingData, aes(x=LONGITUDE, y=LATITUDE)) + geom_point()+facet_wrap(~RELATIVEPOSITION)


ggplot_coordinates <- ggplot(data=trainingData, aes(x=LONGITUDE, y=LATITUDE)) + geom_point()


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
ggplot_coordinates_valid <- ggplot(data=validationData, aes(x=LONGITUDE, y=LATITUDE)) + geom_point()

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

#hchart(trainingData$WAP001)
#summary(trainingData$WAP001)
### -97 and 100
#hchart(trainingData$WAP002)
#summary(trainingData$WAP002)
### -90 and 100
#hchart(trainingData$WAP003)
#summary(trainingData$WAP003)
###only 100
#hchart(trainingData$WAP004)
#summary(trainingData$WAP004)
###only 100
#summary(trainingData$WAP005)
#hchart(trainingData$WAP005)
### -97 and 100

# (..)
summary(trainingData$WAP120)
hchart(trainingData$WAP120)
# 100 or from -42 to -98
### mean: 90.98 - several other values 

#summary(trainingData$WAP220)
#hchart(trainingData$WAP220)
### only 100

#summary(trainingData$WAP320)
#hchart(trainingData$WAP320)
### -98 and 100

#summary(trainingData$WAP420)
#hchart(trainingData$WAP420)
### -94 and 100

#summary(trainingData$WAP520)
#hchart(trainingData$WAP520)
###only 100

#### D. Pre-processing ####
#### D.1 waps that haven't detected anything / NEW DATASET ####

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


#### D.2 waps with poor and erroneous values / NEW DATASET ####
### all are 0 
sum(apply(training_clean[,c(1:312)],2,function(x)(min(x==0))))
### C: 0 waps with all zeros 

### all are 100
sum(which(apply(training_clean[,c(1:312)],2,function(x) min(x)) == 100))
### C: 0 records with all 100

### with unusable values - below -90 (acceptable threshold) 
sum(apply(training_clean[,c(1:312)],2,function(x)length(which(x<=-90))))
### C: 57599 records lower 100
which(apply(training_clean[,c(1:312)],2,function(x)length(which(x<=-90)))>=1000)
### C: WAP061 WAP062 WAP063 WAP064 WAP065 WAP066 WAP087                               

### with unusable values - below -100 
sum(apply(training_clean[,c(1:312)],2,function(x)length(which(x<=-100))))
### C: 303 records lower 100
hchart(apply(training_clean[,c(1:312)],2,function(x)length(which(x<=-100))))
### C: length varies from 0 to 9 // 34 with length 1 - 4 with length 9

### with unusable values - equal to -104 
sum(apply(training_clean[,c(1:312)],2,function(x)length(which(x==-104))))
### C: 4 records lower 100 - WAP282
### C: not taking out the unaceptable values yet - still to be explored 

### with erroneous values - from -30 to 0 

## training 
sum(apply(training_clean[,c(1:312)],2,function(x)length(which(x>-30 & x <=0))))
### !C: 682 records higher  -30 and 0 
which(apply(training_clean[,c(1:312)],2,function(x)length(which(x>-30 & x <=0)))>=40)
### C: WAP087 has a length of more than 80 - meaning, more than 80 times, it has recorded a wrong value 
which(apply(training_clean[,c(1:312)],2,function(x)length(which(x>-30 & x <=0)))>=1)
### C: ~50 waps have recorded erroneous values
### C: WAP011 WAP012 WAP061 WAP062 WAP063 WAP064 WAP065 WAP066 WAP069 WAP070 WAP073 WAP074 WAP077 WAP078 WAP080 WAP081 WAP082
### C: WAP083 WAP084 WAP085 WAP087 WAP105 WAP117 WAP118 WAP121 WAP122 WAP127 WAP128 WAP131 WAP132 WAP138 WAP139 WAP144 WAP145 
### C: WAP180 WAP189 WAP249 WAP262 WAP279 WAP286 WAP335 WAP342 WAP481 WAP483 WAP484 WAP486 WAP495 WAP496 WAP501 WAP502

erron_train <- names(which(apply(training_clean[,c(1:312)],2,function(x)length(which(x>-30 & x <=0)))>=1))
is.vector(erron_train)
erron_train
### C: "WAP011" "WAP012" "WAP061" "WAP062" "WAP063" "WAP064" "WAP065" "WAP066" "WAP069" "WAP070" "WAP073"
# WAP074" "WAP077" "WAP078" "WAP080" "WAP081" "WAP082" "WAP083" "WAP084" "WAP085" "WAP087" "WAP105"
# "WAP117" "WAP118" "WAP121" "WAP122" "WAP127" "WAP128" "WAP131" "WAP132" "WAP138" "WAP139" "WAP144"
# "WAP145" "WAP180" "WAP189" "WAP249" "WAP262" "WAP279" "WAP286" "WAP335" "WAP342" "WAP481" "WAP483"
# "WAP484" "WAP486" "WAP495" "WAP496" "WAP501" "WAP502"

## validation  
sum(apply(validation_clean[,c(1:312)],2,function(x)length(which(x>-30 & x <=0))))
### C: its curious that there is no erroneous values in the dataset 


### training_clean_v2 // new data set without waps that only yield 100 + waps that have yielded erroneous values (-30:0) 
training_clean_v2<-training_clean[,-which(names(training_clean) %in% erron_train)]
str(training_clean_v2)
### C: dataset with 271 variables

### validation_clean_v2 // new data set without waps that only yield 100 + waps that have yielded erroneous values (-30:0) 
validation_clean_v2<-validation_clean[,-which(names(validation_clean) %in% erron_train)]
str(validation_clean_v2)
### C: dataset with 271 variables

#### D.3 observations with only poor / erroneous values ####
### all are 0 
sum(apply(training_clean_v2[,c(1:271)],1,function(x)(min(x==0))))
### 0 records with all 0 values

### all are 100 (just checking) 
sum(apply(training_clean_v2[,c(1:271)],1,function(x)(min(x==100))))

### erroneous values - between -30 and 0 (just checking) 
sum(apply(training_clean_v2[,c(1:271)],1,function(x)(min(x>-30 & x <=0))))

### unusable values - all -104
sum(apply(training_clean_v2[,c(1:271)],1,function(x)(min(x==-104))))
### C: no observation with only -104 values

### unusable values - all values lower than 90 
sum(apply(training_clean_v2[,c(1:271)],1,function(x)(min(x<=-90))))
### C: no observation with only unusable values

#### D.4.1 sub sample by building - trainin_clean ####
str(training_clean)
str(training_clean$BUILDINGID)
colnames(training_clean)
sum(apply(training_clean[,c(1:312)],2,function(x)min(x=="100")) ) ### Checking 

train_TI<-training_clean %>% filter(BUILDINGID=="TI")
### C: TI building - 5,249 x 321
train_TD<-training_clean %>% filter(BUILDINGID=="TD")
### C: TD building - 5,196 x 321
train_TC<-training_clean %>% filter(BUILDINGID=="TC")
### C: TC building - 9,492 x 321

### C: I am missing the same split on the validaton dataset 


#### D.4.2 sub sample by building and floor ####

### $ TI Building - Floor 0, 1, 2 and 3 ####
train_TI_F0<-train_TI %>% filter(FLOOR==0)
### C: TI building and floor 0 - 1,059 x 271

train_TI_F1<-train_TI %>% filter(FLOOR==1)
### C: TI building and floor 1 - 1,356 x 271 

train_TI_F2<-train_TI %>% filter(FLOOR==2)
### C: TI building and floor 2 - 1,443 x 271 

train_TI_F3<-train_TI %>% filter(FLOOR==3)
### C: TI building and floor 2 - 1,391 x 271 

### $ TD Building - Floor 0, 1, 2 and 3 ####
train_TD %>% filter(FLOOR==0)
train_TD_F0<-train_TD %>% filter(FLOOR==0)
### C: TD building and floor 0 - 1,368 x 271

train_TD_F1<-train_TD %>% filter(FLOOR==1)
### C: TD building and floor 1 - 1,484 x 271

train_TD_F2<-train_TD %>% filter(FLOOR==2)
### C: TD building and floor 2 - 1,396  x 271

train_TD_F3<-train_TD %>% filter(FLOOR==3)
### C: TD building and floor 3 - 948  x 271

### $ TC Building - Floor 0, 1, 2, 3 and 4 ####
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



#### D.5 Explore different samples #### 

### Latitude and longitude - building and floor 
ggplot(data=train_TI, aes(x=LONGITUDE, y=LATITUDE)) + geom_point()+facet_wrap(~FLOOR)
### No big difference
ggplot(data=train_TD, aes(x=LONGITUDE, y=LATITUDE)) + geom_point()+facet_wrap(~FLOOR)
### A lot of differences - 2nd floor the most complete - 3rd floor the least 
ggplot(data=train_TC, aes(x=LONGITUDE, y=LATITUDE)) + geom_point()+facet_wrap(~FLOOR)
### Some differences - 0, 1 and 2 floor - few observation son south side. 3rd floor is the more complete and 4th floor/ south-east is really bad

### $ signal lower than 90 and higher than 90 // ERROR ####
sum(apply(training_clean[,c(1:312)],2,function(x)min(x>=-90))) ### 5 
which(apply(training_clean[,c(1:312)],2,function(x)min(x>=-90))>0) 
### WAP275 WAP354 WAP434 WAP492 WAP498 - all of them more than 210 times
summary(training_clean$WAP275) ### E: min: -88

sum(apply(training_clean[,c(1:312)],2,function(x)min(x<=-90))) ### 0 
which(apply(training_clean[,c(1:312)],2,function(x)min(x<=-90))>0) ### none 
apply(training_clean[,c(1:312)],2,function(x)length(x<=-90))
sum(apply(training_clean[,c(1:312)],2,function(x)length(x<=-90))) ### 6220344
sum(apply(training_clean[,c(1:312)],2,function(x)length(x>=-90))) ### 6220344


sum(apply(training_clean[,c(1:312)],2,function(x)length(which(x<=-90)))) ### 57599
sum(apply(training_clean[,c(1:312)],2,function(x)length(which(x>=-90)))) ### 6176092

which(apply(training_clean[,c(1:312)],2,function(x)length(which(x<=-90)))>0) ### same 312
which(apply(training_clean[,c(1:312)],2,function(x)length(which(x>=-90)))>0) ### same 312


### $ signal equal to 100 // Building ####
sum(apply(training_clean[,c(1:312)],2,function(x)min(x=="100"))) ### 0
sum(apply(train_TI[,c(1:312)],2,function(x)min(x=="100"))) ### 166
sum(apply(train_TD[,c(1:312)],2,function(x)min(x=="100"))) ### 144
sum(apply(train_TC[,c(1:312)],2,function(x)min(x=="100"))) ### 190

### $ signal between -30 to 0 // Building and floor ####
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

### $ signal lower than -90 // Building and floor ####
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
### no conclusion 

### $ signal between -90 to -30 // Building and floor ####
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
### no conclusion 

#### D.6 replace 100 by -105 //  training_clean witouth 100#### 

### first, in the general dataset 
# for(i in training_clean) if(i == 100) training_clean <- "-105"
# apply(training_clean[,c(1:312)],1,function(x) ifelse(x=="100","-105",x))
training_clean[training_clean==100] <- -105

### $ by building #### 
train_TI<-training_clean %>% filter(BUILDINGID=="TI")
### C: TI building - 5,249 x 321
train_TD<-training_clean %>% filter(BUILDINGID=="TD")
### C: TD building - 5,196 x 321
train_TC<-training_clean %>% filter(BUILDINGID=="TC")
### C: TC building - 9,492 x 321

max(train_TI[,c(1:312)]) ### -29
max(train_TD[,c(1:312)]) ### -1
max(train_TC[,c(1:312)]) ### 0 


### $ by floors: #### 

### $$ TI Building - Floor 0, 1, 2 and 3 ####
train_TI_F0<-train_TI %>% filter(FLOOR==0)
train_TI_F1<-train_TI %>% filter(FLOOR==1)
train_TI_F2<-train_TI %>% filter(FLOOR==2)
train_TI_F3<-train_TI %>% filter(FLOOR==3)

### $$ TD Building - Floor 0, 1, 2 and 3 ####
train_TD_F0<-train_TD %>% filter(FLOOR==0)
train_TD_F1<-train_TD %>% filter(FLOOR==1)
train_TD_F2<-train_TD %>% filter(FLOOR==2)
train_TD_F3<-train_TD %>% filter(FLOOR==3)

### $$ TC Building - Floor 0, 1, 2, 3 and 4 ####
train_TC_F0<-train_TC %>% filter(FLOOR==0)
train_TC_F1<-train_TC %>% filter(FLOOR==1)
train_TC_F2<-train_TC %>% filter(FLOOR==2)
train_TC_F3<-train_TC %>% filter(FLOOR==3)
train_TC_F4<-train_TC %>% filter(FLOOR==4)

max(train_TI_F0[,c(1:312)]) ### -29
max(train_TI_F1[,c(1:312)]) ### -34
max(train_TI_F2[,c(1:312)]) ### -31
max(train_TI_F3[,c(1:312)]) ### -30

max(train_TD_F0[,c(1:312)]) ### -1
max(train_TD_F1[,c(1:312)]) ### -35
max(train_TD_F2[,c(1:312)]) ### -39
max(train_TD_F3[,c(1:312)]) ### -40

max(train_TC_F0[,c(1:312)]) ### -25
max(train_TC_F1[,c(1:312)]) ### -29
max(train_TC_F2[,c(1:312)]) ### -33
max(train_TC_F3[,c(1:312)]) ### 0
max(train_TC_F4[,c(1:312)]) ### 0 

### C: weird values in TC - 3rd and 4th floor // TD - in 0th floor 




