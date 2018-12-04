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
head(trainingData)
head(validationData)
sum(is.na(trainingData))
sum(is.na(validationData))
str(trainingData)
str(trainingData, list.len= nrow(trainingData))
sum(names(trainingData)==names(validationData))

### C: Both datasets have 529 variables and all of them are the same 
### C: no NAs in neither 

### $ TIMESTAMP - new variable set as.POSIXlt ####
# training 
library(ggplot2)
qplot(trainingData$TIMESTAMP)
### C: 7 levels / dates
sum(is.na(trainingData$TIMESTAMP))

trainingData$TIMESTAMP_dt<-as.POSIXlt(as.numeric(trainingData$TIMESTAMP),origin="1970-01-01",tz="GMT")
head(trainingData$TIMESTAMP_dt)
str(trainingData$TIMESTAMP_dt)
summary(trainingData$TIMESTAMP_dt)
### C: assuming an origin of 01/01/2013 - from 30/05 to 20/06

# validation 
qplot(validationData$TIMESTAMP)
validationData$TIMESTAMP_dt <- as.POSIXlt(as.numeric(validationData$TIMESTAMP),origin="1970-01-01",tz="GMT")
### C: quite different distribution - 8 levels
qplot(validationData$TIMESTAMP_dt)
summary(validationData$TIMESTAMP_dt)
### C: assuming an origin of 01/01/2013 - from 19/09 to 10/08


### $ PHONEID - changed data type to factor ####
# training
library(highcharter)
hchart(trainingData$PHONEID)
hchart(as.factor(trainingData$PHONEID))
str(as.factor(trainingData$PHONEID))
### C: most common: 13 HTC Wildfire S 2.3.5 0,11 14 & LT22i 4.0.4 0,1,9,16 - more tha 4.500 observations. Less common (<200) is 16 LT26i 4.0.4 3 (phone id - 13 & 14)
### C: some code ids missing: 0, 2, 4, 5, 9, 12, 15, 20, 21 / only 16 levels
trainingData$PHONEID<-as.factor(trainingData$PHONEID)

# validation 
validationData$PHONEID<-as.factor(validationData$PHONEID)
hchart(validationData$PHONEID)
str(validationData$PHONEID)
### C: highest frequency - HTC Wildfire S 2.3.5 0,11 (phone id - 13)
### C: some code ids missing: 1, 3, 6, 7, 8, 10, 11, 16, 17, 18, 19, 22, 23, 24 /only 11 levels - different ones than the testing set 


### $ USERID - changed data type to factor ####
# training 
hchart(trainingData$USERID)
trainingData$USERID<-as.factor(trainingData$USERID)
hchart(trainingData$USERID)
### C: higheest frequency : userid 1 - USER0001 170 and userid 11 - USER0011 176 
str(trainingData$USERID)
### C: 18 levels - no userid 0

# validation
hchart(validationData$USERID)
summary(validationData$USERID)
### C: pilot was run with userid 0 - USER0000 (Validation User) 
validationData$USERID<-as.factor(validationData$USERID)
str(validationData$USERID)
### C: CAREFUL using this variable for classification / prediction 


### $ RELATIVEPOSITION - changed data type to factor ONLY IN THE TRAINING DATASET ####
# training 
str(trainingData$RELATIVEPOSITION)
### C: 3329 inside and 16608 outside - why is the outside useful?
trainingData$RELATIVEPOSITION<-as.factor(trainingData$RELATIVEPOSITION)
trainingData$RELATIVEPOSITION <- factor(trainingData$RELATIVEPOSITION,
       levels = c(1,2),
       labels = c("inside", "outside"))
hchart(trainingData$RELATIVEPOSITION)

# validation 
summary(validationData$RELATIVEPOSITION)
head(validationData$RELATIVEPOSITION)
### C: whats the meaning of 0? is it inside? 


### $ SPACEID - changed data type to factor ONLY IN THE TRAINING DATASET ####
# training
summary(trainingData$SPACEID)
hchart(trainingData$SPACEID)
trainingData$SPACEID<-as.factor(trainingData$SPACEID)
hchart(trainingData$SPACEID)
str(trainingData$SPACEID)
### C: 123 levels - highest frequency: 101- 104; 106 - 108; 201- 202

# validation
hchart(validationData$SPACEID)
validationData$SPACEID <- as.factor(validationData$SPACEID)
summary(validationData$SPACEID)
### C: only 1 level - 0 // not the same as the training


### $ BUILDINGID - changed data type and labels into 1,2,3 ####
# training 
hchart(trainingData$BUILDINGID)
trainingData$BUILDINGID<-as.factor(trainingData$BUILDINGID)
trainingData$BUILDINGID <- factor(trainingData$BUILDINGID, levels= c(0,1,2), labels= c("1", "2", "3"))
### C: highest frequency on building 3 (almost the doble of the other two) 

# validation
hchart(validationData$BUILDINGID)
validationData$BUILDINGID<-as.factor(validationData$BUILDINGID)
validationData$BUILDINGID <- factor(validationData$BUILDINGID, levels= c(0,1,2), labels= c("1", "2", "3"))
### C: highest frequency on building 1 (> 500) 


### $ FLOOR - no change ####  
# training 
hchart(trainingData$FLOOR)
### C: similar frequency from 0 to 3 floor, and more than 4 times less on 4th floor

# validation 
hchart(validationData$FLOOR)
### C: least frequent - 0 and 4th floor. most frequent 1st floor

### $ LATITUDE - no change ####
# training 
hchart(trainingData$LATITUDE)
summary(trainingData$LATITUDE)
### C: quite normally distributed - a little skewed to the right 
### C: highest frequency 4864840,4864850 (~1850 obs)
### C: 2 outliers: 4864930,4864940 (1620 obs) and 4864980,4864990 (~980 obs)
### C: min - 4864746; max - 4865017

# validation
hchart(validationData$LATITUDE)
summary(validationData$LATITUDE)
### C: quite normally distributed - a little skewed to the left 
### C: min - 4864748; max - 4865017 - similar range 


### $ LONGITUDE - no change ####
# training
hchart(trainingData$LONGITUDE)
summary(trainingData$LONGITUDE)
### C. weird distribution 
### C: 2 outliers: -7640, -7620 (1680 obs) and -7380, -7360 (2160 obs)
### C: min: -7691; max: -7301 

# validation
hchart(validationData$LONGITUDE)
summary(validationData$LONGITUDE)
### C: weird distribution - more uniform with 1 outlier
### C: 1 outlier : -7650, -7600 (~350 obs)
### C: min: -7696; max: -7300 - similar to training's range

### C.2 Explore relationship between variables ####

# floor abd relative position
qplot(trainingData$FLOOR, trainingData$RELATIVEPOSITION)
### C: werid result - different floors from outside?
qplot(trainingData$SPACEID, trainingData$RELATIVEPOSITION)
### C: werid result - different spaceids from outside?

# longitude and latitude
qplot(trainingData$LONGITUDE, trainingData$LATITUDE)
qplot(validationData$LONGITUDE, validationData$LATITUDE)
### C: seem to have mapped all building 

# longitude and latitude, with floor, building and relativeposition 
ggplot(data=trainingData, aes(x=LONGITUDE, y=LATITUDE)) + geom_point()+facet_wrap(~BUILDINGID)
### C: buildings are isolated 

ggplot(data=trainingData, aes(x=LONGITUDE, y=LATITUDE)) + geom_point()+facet_grid(~FLOOR)
ggplot(data=trainingData, aes(x=LONGITUDE, y=LATITUDE)) + geom_point()+facet_wrap(~FLOOR)
### C: 4th flor only on building 3

ggplot(data=trainingData, aes(x=LONGITUDE, y=LATITUDE)) + geom_point()+facet_wrap(~RELATIVEPOSITION)
### C: all building have outside WAP access points ??? 

ggplot_coordinates <- ggplot(data=trainingData, aes(x=LONGITUDE, y=LATITUDE)) + geom_point()


ggplot_coordinates + geom_point(size=4, aes(color = ifelse(LONGITUDE < -7580, "Building 1",
                                      ifelse( LONGITUDE < - 7400 & LATITUDE > 4864830, "Building 2", "Building 3")))) +
  scale_color_manual(values = c("Building 1" = "red", "Building 2" = "purple", "Building 3" = "blue"),
                     name = "Buildings")

ggplot_coordinates+ geom_point(size=4, aes(color = ifelse(FLOOR == 0, "0",
                                                         ifelse(FLOOR ==1, "1",
                                                                ifelse(FLOOR == 2, "2", 
                                                                       ifelse(FLOOR == 3, "3","4"))))))+
  scale_color_manual(values = c("1"= "red", "2"="purple", "3"="pink", "0"="orange", "4"="yellow"), name="Floors")



scatterplot3d(trainingData$LONGITUDE, trainingData$LATITUDE, trainingData$BUILDINGID)


library(RcolorBrewer)
display.brewer.all()
