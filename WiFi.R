###################
# Project Summary
###################

## Indor positioning by WiFi fingerprinting
## Predicting floor, longitude and latitude

#######################
# Set working directory
#######################


setwd("C:\\Users\\Maja\\Documents\\Ubiqum\\Module 3\\R wd\\UJIndoorLoc")


###############
# Load packages
###############


library(dplyr)
library(tidyr)
library(caret)
library(plotly)
library(lubridate)
library(rgdal)
library(parallel)
library(doParallel)
library(ggplot2)
library(ggmap)
#library(dummies)




############
## Define
############


## remove zero variance columns 

rm_zv_row <- function(df) {
    
    df <- df %>% filter(apply(df[, 1:(ncol(df) - 9)], 1, function (x) length(unique(x))) >1)
    return(df)
    
}


## normalize by rows 

norm_rows <- function(df) {
   
     df <- as_tibble(t(apply(df, 1, function (x) (x - min(x))/(max(x) - min(x)))))
    return(df)
     
}

## range

range <- function(df) { 
    
    df <- df[, 1:ncol(df)]
    return(df)
    
}


## calculate user movement

movement <- function(df) {
    
    for(i in 1:nrow(df)){
        if(((df[i,466] == df[i+1,466]))){
           row[i,] <- df[i,]
        }
        else return(row)
           
    }
}

multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
    library(grid)
    
    # Make a list from the ... arguments and plotlist
    plots <- c(list(...), plotlist)
    
    numPlots = length(plots)
    
    # If layout is NULL, then use 'cols' to determine layout
    if (is.null(layout)) {
        # Make the panel
        # ncol: Number of columns of plots
        # nrow: Number of rows needed, calculated from # of cols
        layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
            ncol = cols, nrow = ceiling(numPlots/cols))
    }
    
    if (numPlots==1) {
        print(plots[[1]])
        
    } else {
        # Set up the page
        grid.newpage()
        pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
        
        # Make each plot, in the correct location
        for (i in 1:numPlots) {
            # Get the i,j matrix positions of the regions that contain this subplot
            matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
            
            print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                layout.pos.col = matchidx$col))
        }
    }
}


#################
## Import data ##
#################


## read csv file for training and testing dataset

WiFi_train <- read.csv("trainingData.csv",
                       header = TRUE, 
                       stringsAsFactors = FALSE)


WiFi_val <- read.csv("validationData.csv",
                      header = TRUE,
                      stringsAsFactors = FALSE)

######################
## Investigate data ##
######################

# head(WiFi_train, n = 10)
# tail(WiFi_train, n = 10)
# summary(WiFi_train)
# str(WiFi_train)
# attributes(WiFi_train)
# sapply(WiFi_train, class)
# summary(WiFi_train[,521:529])
# 
# 
# head(WiFi_val, n = 10)
# tail(WiFi_val, n = 10)
# summary(WiFi_val)
# str(WiFi_val)
# attributes(WiFi_val)
# sapply(WiFi_val, class)
# summary(WiFi_val[,521:529])


#########################
## Preprocess the data ##
#########################


## convert from unix time

WiFi_train$TIMESTAMP <- as_datetime(WiFi_train$TIMESTAMP, origin = "1970-01-01", tz = "UTC")
WiFi_val$TIMESTAMP <- as_datetime(WiFi_val$TIMESTAMP, origin = "1970-01-01", tz = "UTC")


## convert from utm to longitude and latitude for training
# 
# utmcoor_train<-SpatialPoints(cbind(WiFi_train$LATITUDE, WiFi_train$LONGITUDE), 
#                              proj4string=CRS("+proj=utm +ellps=WGS84"))
# 
# longlatcoor_train<-spTransform(utmcoor_train,CRS("+proj=longlat"))
# 
# longlatcoor_train@coords ## to see the long and lat pairs
# 
# 
# ## convert from utm to longitude and latitude for testing
# utmcoor_val<-SpatialPoints(cbind(WiFi_val$LATITUDE, WiFi_val$LONGITUDE), 
#                             proj4string=CRS("+proj=utm +zone=31 +datum=WGS84"))
# 
# longlatcoor_val<-spTransform(utmcoor_val,CRS("+proj=longlat"))
# 
# longlatcoor_val@coords ## to see the long and lat pairs


## check for missing values

# length(which(is.na(WiFi_train))) ## no NAs in test set
# length(which(is.na(WiFi_val))) ## no NAs in train test


## check for duplicates

WiFi_train <- distinct(WiFi_train) # 637 duplicates
WiFi_val <- distinct(WiFi_val) # no duplicates



## for checking how models work with 100 dBm values

WiFi_train_100 <- WiFi_train
WiFi_val_100 <- WiFi_val

## changing from 100 to -105 undetected WAP signal values

WiFi_train[WiFi_train == 100] <- -105
WiFi_val[WiFi_val == 100] <- -105

WAPs <- grep("WAP", names(WiFi_train), value=T)
WiFi_train[,WAPs] <- sapply(WiFi_train[,WAPs], function(x) ifelse(x < - 90, -105, x))
WiFi_val[,WAPs] <- sapply(WiFi_val[,WAPs], function(x) ifelse(x < - 95, -105, x))

#WiFi_train_100[WiFi_train_100 == 100] <- -105

## plot

# plot(x = WiFi_train$LONGITUDE, y = WiFi_train$LATITUDE)
# plot(x = WiFi_val$LONGITUDE, y = WiFi_val$LATITUDE)
# 
# ggplot(WiFi_val, aes(x = LONGITUDE, y = LATITUDE, color = BUILDINGID)) + geom_point() 
# ggplot(WiFi_train, aes(x = LONGITUDE, y = LATITUDE, color = BUILDINGID)) + geom_point()

## check for near zero variance 
 
# nearZeroVar(WiFi_train[, range(WiFi_train) - 9], uniqueCut = 0.005181)
# nearZeroVar(WiFi_val[, range(WiFi_val) - 9], uniqueCut = 0.09)


## check for nzv, remove WAPs with zero variance

zeroVar_train_col <- nearZeroVar(WiFi_train[, 1:(ncol(WiFi_train) - 9)], saveMetrics = TRUE)
 
WiFi_train <- WiFi_train[,-(which(zeroVar_train_col$zeroVar == TRUE))]
WiFi_val <-  WiFi_val[,-(which(zeroVar_train_col$zeroVar == TRUE))]

zeroVar_val_col <- nearZeroVar(WiFi_val[, 1:(ncol(WiFi_val) - 9)], saveMetrics = TRUE)

WiFi_train <- WiFi_train[,-(which(zeroVar_val_col$zeroVar == TRUE))]
WiFi_val <-  WiFi_val[,-(which(zeroVar_val_col$zeroVar == TRUE))]
  

## remove rows with zero variance

#WiFi_train <- rm_zv_row(WiFi_train)
 WiFi_train <- WiFi_train %>% 
               filter(apply(WiFi_train[, 1:(ncol(WiFi_train) - 9)], 
               1, function (x) length(unique(x))) >1)

 WiFi_val <- WiFi_val %>% 
             filter(apply(WiFi_val[, 1:(ncol(WiFi_val) - 9)], 
             1, function (x) length(unique(x))) >1)
 
# WiFi_val <- rm_zv_row(WiFi_val)


# WiFi_train <- WiFi_train[,((apply(WiFi_train, 1, var)) != 0)]
# WiFi_val <- WiFi_val[((apply(WiFi_val, 1, var)) != 0), ]
# 
# WiFi_train_100 <- WiFi_train_100[(apply(WiFi_train_100, 1, var)) != 0, ]
# WiFi_val_100 <- WiFi_val_100[(apply(WiFi_val_100, 1, var)) != 0, ] 


## sort by time 
## ~10 observation on the same location by every user

WiFi_train <- WiFi_train[order(WiFi_train$USERID, WiFi_train$TIMESTAMP),]


## create means of WAP columns by same position 

WiFi_train <- WiFi_train %>% 
              group_by(USERID, LONGITUDE, LATITUDE, FLOOR, BUILDINGID) %>%
              summarise_at(vars(-LONGITUDE, -LATITUDE, -FLOOR, -SPACEID, -RELATIVEPOSITION, 
              -PHONEID, -TIMESTAMP, -USERID, -BUILDINGID), funs(mean(.)))

## remove columns from WIFI validation dataset

WiFi_val$SPACEID <- NULL
WiFi_val$RELATIVEPOSITION <- NULL
WiFi_val$PHONEID <- NULL
WiFi_val$TIMESTAMP <- NULL

## create BID and floor column + Long, lat and floor column

# WiFi_train <- unite(WiFi_train, LLF, c(LONGITUDE, LATITUDE, FLOOR), remove = FALSE)
WiFi_train <- unite(WiFi_train, BID_F, c(BUILDINGID, FLOOR), remove = FALSE)
WiFi_val <- unite(WiFi_val, BID_F, c(BUILDINGID, FLOOR), remove = FALSE)


## convert to factor

factor <- c("FLOOR", "BUILDINGID", "BID_F")
WiFi_train[,factor] <- lapply(WiFi_train[,factor], as.factor)
WiFi_val[,factor] <- lapply(WiFi_val[,factor], as.factor)
rm(factor)

## reorder columns

WiFi_train <- WiFi_train[,c(7:306, 2:3, 6, 5, 4, 1)]
WiFi_val <- WiFi_val[,c(1:300, 301:302, 305, 304, 303, 306)]

## create only WAPs dataframe

WAPs_train <- WiFi_train[, 1:(ncol(WiFi_train) - 6)]
WAPs_val <- WiFi_val[, 1:(ncol(WiFi_val) - 6)]
#WAPs_All_test <- WiFi_train_100[, 1:(ncol(WiFi_train_100) - 9)]


## normalize dataframe by rows
WAPs_train_norm <- norm_rows(WAPs_train)
WAPs_val_norm <- norm_rows(WAPs_val)

#############################################################
### TESTING FEATURES - ROW AND COLUMN MEANS FOR THRESHOLD ### 
#############################################################


## create column with row means
# col_mean <- WAPs_train %>% summarise_all(~mean(.[. > -105]))
# mean_rows <- WAPs_train
# 
# WAPs_val[colMeans(WAPs_train) >= -95] <- NULL
# WAPs_train[colMeans(WAPs_train) >= -95] <- NULL
# 
# 
# 
# row_mean <- apply(WAPs_train, 1, function(x) { mean(x[x > -105], na.rm=TRUE) })
# 
# WiFi_train <- cbind(WiFi_train, row_mean = WAPs_train$row_mean)
# 
# WAPs_train <- rbind(WAPs_train, apply(WAPs_train, 2, function(x) { mean(x[x > -105], na.rm=TRUE) }))
# 
# 
# WAPs_train <- WAPs_train[c(1:1801),]
# 
# WAPs_train <- WAPs_train[WAPs_train$row_mean >= -87,]
# WiFi_train <- WiFi_train[WiFi_train$row_mean >= -87,]
# 
# WAPs_train$row_mean <- NULL
# WiFi_train$row_mean <- NULL
# 
# # WAPs_All_test$row_mean <- apply(WAPs_All_test, 1, function(x) { mean(x[x > -105], na.rm=TRUE) })
# WAPs_All_test <- WAPs_All_test[WAPs_All_test$row_mean >= -75,]

## remove zv columns

# WAPs_train[vapply(WAPs_train, function(x) length(unique(x)) > 1, logical(1L))]


######### FOR TESTING PURPOSES - HIGHEST WAP AND SIGNAL #######################

## save only highest waps value
# High_RSSI <- apply(WAPs_train, 1, max)
# High_WAP <- colnames(WAPs_train)[apply(WAPs_train, 1, which.max)]

###############################################################################

## create dataframe with waps and buildning ID

names <- c("LAT", "LONG", "BID", "FLOOR", "BID_F", "USERID")
range <- c(301:306)

WAP_BF_LL <- cbind(WAPs_train, WiFi_train$LONGITUDE, WiFi_train$LATITUDE,
                   WiFi_train$BUILDINGID, WiFi_train$FLOOR, 
                   WiFi_train$BID_F,  WiFi_train$USERID)

colnames(WAP_BF_LL)[range] <- names

WAP_BF_LL_val <- cbind(WAPs_val, WiFi_val$LONGITUDE, WiFi_val$LATITUDE, 
                        WiFi_val$BUILDINGID, WiFi_val$FLOOR, 
                        WiFi_val$BID_F, WiFi_val$USERID)

colnames(WAP_BF_LL_val)[range] <- names

### TRY ALL THE SAME BUT WITH NORMALIZATION

WAP_BF_LL_norm <- cbind(WAPs_train_norm, WiFi_train$LONGITUDE, WiFi_train$LATITUDE,
                        WiFi_train$BUILDINGID, WiFi_train$FLOOR, 
                        WiFi_train$BID_F,  WiFi_train$USERID)

colnames(WAP_BF_LL_norm)[range] <- names


WAP_BF_LL_val_norm <- cbind(WAPs_val_norm, WiFi_val$LONGITUDE, WiFi_val$LATITUDE, 
                            WiFi_val$BUILDINGID, WiFi_val$FLOOR, 
                            WiFi_val$BID_F, WiFi_val$USERID)

colnames(WAP_BF_LL_val_norm)[range] <- names

###########
## PLOTTING
###########


## repeated plots with cleaned dataset

plot(x = WiFi_test$LONGITUDE, y = WiFi_test$LATITUDE)
plot(x = WiFi_train$LONGITUDE, y = WiFi_train$LATITUDE)

ggplot(WiFi_test, aes(x = LONGITUDE, y = LATITUDE, color = ~ source)) + geom_point() 


## distribution of signal

ggplot(WAPs_train, aes(x = (grep = "WAP"))) + 
       geom_histogram() +
       labs(x = "Readings", title = "Count of Readings")  + 
       theme_minimal()


ggplot() +
    geom_histogram(data = x, aes(values), fill = "red", alpha = 1, binwidth = 5) +
    geom_histogram(data = y, aes(values), fill = "blue", alpha = 1, binwidth = 5) +
    ggtitle("Distribution of WAPs signal strength (Training and Test sets)") +
    xlab("WAP strength")


x <- WAPs_train
x <- stack(x)
x <- x[-grep(-105, x$values),]

hist(x$values, xlab = "RSSI", main = "RSSI training set", col = "light blue")

# test data
y <- WAPs_val
y <- stack(y)

y <- y[-grep(-105, y$values),]
hist(y$values, xlab = "RSSI", main = "RSSI validation set", col = "light blue")


################
### MODELING ###
################

#######################################
### TRAIN CONTROL AND PREPROCESSING ###
#######################################

# for parallel processing

cl <- makeCluster(detectCores() - 1)
registerDoParallel(cl)

preP_train <- preProcess(WAP_BF_LL[,1:(ncol(WAP_BF_LL) - 6)], method=c("center", "scale"))

fitControl <- trainControl(method = "repeatedcv",
              number = 10,
              repeats = 3,  
              preP_train)

# set seed for reproducibility

set.seed(1234)


#############################
### 1. BUIDLDING ID MODEL ###
#############################

## without normalization
## define an 80%/20% train/test split of the dataset

inTraining_BID <- createDataPartition(WAP_BF_LL$BID, p = .8, list = FALSE)
trainSet_BID <- WAP_BF_LL[inTraining_BID,]
testSet_BID <- WAP_BF_LL[-inTraining_BID,]


## with normalization
## define an 80%/20% train/test split of the dataset

inTraining_BID_norm <- createDataPartition(WAP_BF_LL_norm$BID, p = .8, list = FALSE)
trainSet_BID_norm <- WAP_BF_LL_norm[inTraining_BID_norm,]
testSet_BID_norm <- WAP_BF_LL_norm[-inTraining_BID_norm,]


#####################
### RANDOM FOREST ###
#####################

## ACCURACY FOR BUILDING RF ####################################
## # higher computational cost than knn 
## WITHOUT NORM:
## 
## WAPS
##  Accuracy     Kappa 
## 0.9981998 0.9971564 
##
## WITH NORM:
##
## WAPS
## Accuracy    Kappa 
##      1        1 
##
###################################################################


RFFit_BID <- train(BID~.  -BID_F -FLOOR -LONG -LAT -USERID, 
                    data = trainSet_BID, 
                    method = "rf",
                    trControl=fitControl)

predictionRF_BID <- predict(RFFit_BID, testSet_BID)

#performace measurment
confusionMatrix(predictionRF_BID, testSet_BID$BID)
postResample(predictionRF_BID, testSet_BID$BID)

#plot predicted vs actual
plot(predictionRF_BID, testSet_BID$BID)

## predict validation dataset
predictionRF_BID_val <- predict(RFFit_BID, WAP_BF_LL_val)
WAP_BF_LL_val$BID_predicted_RF <- predictionRF_BID_val
confusionMatrix(predictionRF_BID_val, WAP_BF_LL_val$BID)
postResample(predictionRF_BID_val, WAP_BF_LL_val$BID)



## PREDICTING WITH NORMALIZATION
RFFit_BID_norm <- train(BID~. -BID_F -FLOOR -LONG -LAT -USERID, 
                        data = trainSet_BID_norm, 
                        method = "rf",
                        trControl=fitControl)

predictionRF_BID_norm<- predict(RFFit_BID_norm, testSet_BID_norm)
#performace measurment
confusionMatrix(predictionRF_BID_norm, testSet_BID_norm$BID)
postResample(predictionRF_BID_norm, testSet_BID_norm$BID)
#plot predicted vs actual
plot(predictionRF_BID_norm, testSet_BID_norm$BID)



## predict validation dataset
predictionRF_BID_val_norm <- predict(RFFit_BID_norm, WAP_BF_LL_val_norm)
WAP_BF_LL_val_norm$BID_predicted_norm_RF <- predictionRF_BID_val_norm
confusionMatrix(predictionRF_BID_val_norm, WAP_BF_LL_val_norm$BID)
postResample(predictionRF_BID_val_norm, WAP_BF_LL_val_norm$BID)




##########
## SVM ##
##########

## ACCURACY FOR BUILDING SVM ####################################
##
## WITHOUT NORM:
## 
## WAPS
## Accuracy     Kappa 
## 0.9972997 0.9957344 
##
## WITH NORM:
##
## WAPS
## Accuracy    Kappa 
##      1        1 
##
###################################################################

## WITHOUT NORMALIZATION
SVMFit_BID <- train(BID~. -BID_F -FLOOR -LONG -LAT -USERID, 
                    data = trainSet_BID, 
                    method = "svmLinear",
                    trControl=fitControl)

predictionSVM_BID <- predict(SVMFit_BID, testSet_BID)

#performace measurment
confusionMatrix(predictionSVM_BID, testSet_BID$BID)
postResample(predictionSVM_BID, testSet_BID$BID)

#plot predicted vs actual
plot(predictionSVM_BID, testSet_BID$BID)

## predict validation dataset
predictionSVM_BID_val <- predict(SVMFit_BID, WAP_BF_LL_val)
WAP_BF_LL_val$BID_predicted_SVM <- predictionSVM_BID_val
confusionMatrix(predictionSVM_BID_val, WAP_BF_LL_val$BID)
postResample(predictionSVM_BID_val, WAP_BF_LL_val$BID)


##WITH NORMALIZATION
SVMFit_BID_norm <- train(BID~. -BID_F -FLOOR -LONG -LAT -USERID, 
                        data = trainSet_BID_norm, 
                        method = "svmLinear",
                        trControl=fitControl)

predictionSVM_BID_norm<- predict(SVMFit_BID_norm, testSet_BID_norm)

#performace measurment
confusionMatrix(predictionSVM_BID_norm, testSet_BID_norm$BID)
postResample(predictionSVM_BID_norm, testSet_BID_norm$BID)

#plot predicted vs actual
plot(predictionRF_BID_norm, testSet_BID_norm$BID)

## predict validation dataset
predictionSVM_BID_val_norm <- predict(SVMFit_BID_norm, WAP_BF_LL_val_norm)
WAP_BF_LL_val_norm$BID_predicted_norm_SVM <- predictionSVM_BID_val_norm
confusionMatrix(predictionSVM_BID_val_norm, WAP_BF_LL_val_norm$BID)
postResample(predictionSVM_BID_val_norm, WAP_BF_LL_val_norm$BID)



##########
## KNN ##
##########

## ACCURACY FOR BUILDING KNN ####################################
##
## WITHOUT NORM:
## 
## WAPS
## Accuracy     Kappa 
## 0.9909991 0.9857862 
##
## WITH NORM:
##
## WAPS
## Accuracy    Kappa 
##      1        1 
##
###################################################################


## WITHOUT NORMALIZATION
KNNFit_BID <- train(BID~. -BID_F -FLOOR -LONG -LAT -USERID, 
                    data = trainSet_BID, 
                    method = "knn",
                    trControl=fitControl)

predictionKNN_BID <- predict(KNNFit_BID, testSet_BID)

#performace measurment
confusionMatrix(predictionKNN_BID, testSet_BID$BID)
postResample(predictionKNN_BID, testSet_BID$BID)

#plot predicted vs actual
plot(predictionKNN_BID, testSet_BID$BID)

## predict validation dataset
predictionKNN_BID_val <- predict(KNNFit_BID, WAP_BF_LL_val)
WAP_BF_LL_val$BID_predicted_KNN <- predictionKNN_BID_val
confusionMatrix(predictionKNN_BID_val, WAP_BF_LL_val$BID)
postResample(predictionKNN_BID_val, WAP_BF_LL_val$BID)


##WITH NORMALIZATION
KNNFit_BID_norm <- train(BID~. -BID_F -FLOOR -LONG -LAT -USERID, 
                        data = trainSet_BID_norm, 
                        method = "knn",
                        trControl=fitControl,
                        tuneGrid = expand.grid(k = c(1:9)))

predictionKNN_BID_norm<- predict(KNNFit_BID_norm, testSet_BID_norm)

#performace measurment
confusionMatrix(predictionKNN_BID_norm, testSet_BID_norm$BID)
postResample(predictionKNN_BID_norm, testSet_BID_norm$BID)

#plot predicted vs actual
plot(predictionKNN_BID_norm, testSet_BID_norm$BID)

## predict validation dataset
predictionKNN_BID_val_norm <- predict(KNNFit_BID_norm, WAP_BF_LL_val_norm)
WAP_BF_LL_val_norm$BID_predicted_norm_KNN <- predictionKNN_BID_val_norm
confusionMatrix(predictionKNN_BID_val_norm, WAP_BF_LL_val_norm$BID)
postResample(predictionKNN_BID_val_norm, WAP_BF_LL_val_norm$BID)







######################
### 2. FLOOR MODEL ###
######################

## train test for floor
inTraining_F <- createDataPartition(WAP_BF_LL$FLOOR, p = .8, list = FALSE)
trainSet_F <- WAP_BF_LL[inTraining_F,]
testSet_F <- WAP_BF_LL[-inTraining_F,]

### normalization part
# define an 80%/20% train/test split of the dataset
inTraining_F_norm <- createDataPartition(WAP_BF_LL_norm$FLOOR, p = .8, list = FALSE)
trainSet_F_norm <- WAP_BF_LL_norm[inTraining_F_norm,]
testSet_F_norm <- WAP_BF_LL_norm[-inTraining_F_norm,]



##################
## RANDOM FOREST
##################

## ACCURACY FOR FLOOR RF ####################################
##
## WITHOUT NORM:
## 
## WAPS
## Accuracy     Kappa 
## 0.8649865 0.8118546 
## 
## WAPS + BID
##  Accuracy     Kappa 
## 0.8604860 0.8056101 
##
## WITH NORM:
##
## WAPS
## Accuracy     Kappa 
## 0.9216922 0.8901694
##      
## WAPS + BID
## Accuracy     Kappa 
## 0.9207921 0.8889126      
##
###################################################################

RFFit_F <- train(FLOOR~. -BID -BID_F -LONG -LAT -USERID, 
                data = trainSet_F, 
                method = "rf",
                trControl=fitControl,
                allowParalel = TRUE)

predictionRF_F <- predict(RFFit_F, testSet_F)

#performace measurment
confusionMatrix(predictionRF_F, testSet_F$FLOOR)
postResample(predictionRF_F, testSet_F$FLOOR)

#plot predicted vs actual
plot(predictionRF_F, testSet_F$FLOOR)

## predict validation dataset
predictionRF_F_val <- predict(RFFit_F, WAP_BF_LL_val)
WAP_BF_LL_val$F_predicted_RF <- predictionRF_F_val
confusionMatrix(predictionRF_F_val, WAP_BF_LL_val$FLOOR)
postResample(predictionRF_F_val, WAP_BF_LL_val$FLOOR)


## PREDICTING WITH NORMALIZATION
RFFit_F_norm <- train(FLOOR~. -BID_F -LONG -LAT -USERID, 
                        data = trainSet_F_norm, 
                        method = "rf",
                        trControl=fitControl)

predictionRF_F_norm<- predict(RFFit_F_norm, testSet_F_norm)

#performace measurment
confusionMatrix(predictionRF_F_norm, testSet_F_norm$FLOOR)
postResample(predictionRF_F_norm, testSet_F_norm$FLOOR)

#plot predicted vs actual
plot(predictionRF_F_norm, testSet_F_norm$FLOOR)

## predict validation dataset
predictionRF_F_val_norm <- predict(RFFit_F_norm, WAP_BF_LL_val_norm)
WAP_BF_LL_val_norm$F_predicted_norm_RF <- predictionRF_F_val_norm
confusionMatrix(predictionRF_F_val_norm, WAP_BF_LL_val_norm$FLOOR)
postResample(predictionRF_F_val_norm, WAP_BF_LL_val_norm$FLOOR)



###########
### SVM ###
###########

## ACCURACY FOR FLOOR SVM ####################################
##
## WITHOUT NORM:
## 
## WAPS
## Accuracy     Kappa 
## 0.9054905 0.8684170 
##
## WAPS + BID
## Accuracy     Kappa 
## 0.9054905 0.8683912 
##
##
## WITH NORM:
##
## WAPS
## Accuracy     Kappa 
## 0.8991899 0.8606929 
##
## WAPS + BID
## Accuracy     Kappa 
## 0.8982898 0.8595664 
##
###################################################################

SVMFit_F <- train(FLOOR~. -BID_F -LONG -LAT -USERID, 
                  data = trainSet_F, 
                  method = "svmLinear",
                  trControl=fitControl)

predictionSVM_F <- predict(SVMFit_F, testSet_F)

#performace measurment
confusionMatrix(predictionSVM_F, testSet_F$FLOOR)
postResample(predictionSVM_F, testSet_F$FLOOR)

#plot predicted vs actual
plot(predictionRF_F, testSet_F$FLOOR)

## predict validation dataset
predictionSVM_F_val <- predict(SVMFit_F, WAP_BF_LL_val)
WAP_BF_LL_val$F_predicted_SVM <- predictionSVM_F_val
confusionMatrix(predictionSVM_F_val, WAP_BF_LL_val$FLOOR)
postResample(predictionSVM_F_val, WAP_BF_LL_val$FLOOR)


## PREDICTING WITH NORMALIZATION
SVMFit_F_norm <- train(FLOOR~. -BID_F -LONG -LAT -USERID, 
                      data = trainSet_F_norm, 
                      method = "svmLinear",
                      trControl=fitControl)

predictionSVM_F_norm<- predict(SVMFit_F_norm, testSet_F_norm)

#performace measurment
confusionMatrix(predictionSVM_F_norm, testSet_F_norm$FLOOR)
postResample(predictionSVM_F_norm, testSet_F_norm$FLOOR)

#plot predicted vs actual
plot(predictionRF_F_norm, testSet_F_norm$FLOOR)

## predict validation dataset
predictionSVM_F_val_norm <- predict(SVMFit_F_norm, WAP_BF_LL_val_norm)
WAP_BF_LL_val_norm$F_predicted_norm_SVM <- predictionSVM_F_val_norm
confusionMatrix(predictionSVM_F_val_norm, WAP_BF_LL_val_norm$FLOOR)
postResample(predictionSVM_F_val_norm, WAP_BF_LL_val_norm$FLOOR)



###########
### KNN ###
###########

## ACCURACY FOR FLOOR KNN ####################################
##
## WITHOUT NORM:
## 
## WAPS
## Accuracy     Kappa 
## 0.9081908 0.8718793 
##
## WAPS + BID
## Accuracy     Kappa 
## 0.9108911 0.8755803 
##
##
## WITH NORM:
##
## WAPS
## Accuracy     Kappa 
## 0.9207921 0.8892644  
##
## WAPS + BID
## Accuracy     Kappa 
## 0.9207921 0.8892644  
##
###################################################################

KNNFit_F <- train(FLOOR ~ . -BID_F -LONG -LAT -USERID, 
                  data = trainSet_F,
                  method = "knn",
                  trControl=fitControl,
                  tuneGrid = expand.grid(k = c(1:9)))


predictionKNN_F <- predict(KNNFit_F, testSet_F)


#performace measurment
confusionMatrix(predictionKNN_F, testSet_F$FLOOR)
postResample(predictionKNN_F, testSet_F$FLOOR)

#plot predicted vs actual
plot(predictionKNN_F, testSet_F$FLOOR)

## predict validation dataset
predictionKNN_F_val <- predict(KNNFit_F, WAP_BF_LL_val)
WAP_BF_LL_val$F_predicted_KNN <- predictionKNN_F_val
confusionMatrix(predictionKNN_F_val, WAP_BF_LL_val$FLOOR)
postResample(predictionKNN_F_val, WAP_BF_LL_val$FLOOR)


## PREDICTING WITH NORMALIZATION
KNNFit_F_norm <- train(FLOOR~. -BID_F -LONG -LAT -USERID, 
                        data = trainSet_F_norm, 
                        method = "knn",
                        trControl=fitControl,
                        tuneGrid = expand.grid(k = c(1:9)))

predictionKNN_F_norm<- predict(KNNFit_F_norm, testSet_F_norm)

#performace measurment
confusionMatrix(predictionKNN_F_norm, testSet_F_norm$FLOOR)
postResample(predictionKNN_F_norm, testSet_F_norm$FLOOR)

#plot predicted vs actual
plot(predictionKNN_F_norm, testSet_F_norm$FLOOR)

## predict validation dataset
predictionKNN_F_val_norm <- predict(KNNFit_F_norm, WAP_BF_LL_val_norm)
WAP_BF_LL_val_norm$F_predicted_norm_KNN <- predictionKNN_F_val_norm
confusionMatrix(predictionKNN_F_val_norm, WAP_BF_LL_val_norm$FLOOR)
postResample(predictionKNN_F_val_norm, WAP_BF_LL_val_norm$FLOOR)







#############################
### 3. BUILDING AND FLOOR ###
#############################

## train test for floor
inTraining_BF <- createDataPartition(WAP_BF_LL$BID_F, p = .8, list = FALSE)
trainSet_BF <- WAP_BF_LL[inTraining_BF,]
testSet_BF <- WAP_BF_LL[-inTraining_BF,]

### normalization part
# define an 80%/20% train/test split of the dataset
inTraining_BF_norm <- createDataPartition(WAP_BF_LL_norm$BID_F, p = .8, list = FALSE)
trainSet_BF_norm <- WAP_BF_LL_norm[inTraining_BF_norm,]
testSet_BF_norm <- WAP_BF_LL_norm[-inTraining_BF_norm,]


#####################
### RANDOM FOREST ###
#####################

## ACCURACY FOR BUILDING AND FLOOR RF ####################################
##
## WITHOUT NORM:
## 
## WAPS
## Accuracy     Kappa 
## 0.9009901 0.8614242 
##
## WITH NORM:
##
## WAPS
## Accuracy     Kappa 
## 0.9117912 0.8768040
##
###########################################################################

RFFit_BF <- train(BID_F~. -BID -FLOOR - LONG - LAT -USERID, 
                    data = trainSet_BF, 
                    method = "rf",
                    trControl=fitControl)

predictionRF_BF <- predict(RFFit_BF, testSet_BF)
#performace measurment
confusionMatrix(predictionRF_BF, testSet_BF$BID_F)
postResample(predictionRF_BF, testSet_BF$BID_F)
#plot predicted vs actual
plot(predictionRF_BF, testSet_BF$BID_F)


## predict validation dataset
predictionRF_BF_val <- predict(RFFit_BF, WAP_BF_LL_val)
WAP_BF_LL_val$BF_predicted <- predictionRF_BF_val
confusionMatrix(predictionRF_BF_val, WAP_BF_LL_val$BID_F)
postResample(predictionRF_BF_val, WAP_BF_LL_val$BID_F)


## PREDICTING WITH NORMALIZATION
RFFit_BF_norm <- train(BID_F~. -FLOOR -BID -LONG -LAT -USERID, 
                      data = trainSet_BF_norm, 
                      method = "rf",
                      trControl = fitControl)

predictionRF_BF_norm<- predict(RFFit_BF_norm, testSet_BF_norm)
#performace measurment
confusionMatrix(predictionRF_BF_norm, testSet_BF_norm$BID_F)
postResample(predictionRF_BF_norm, testSet_BF_norm$BID_F)
#plot predicted vs actual
plot(predictionRF_BF_norm, testSet_BF_norm$BID_F)



## predict validation dataset
predictionRF_BF_val_norm <- predict(RFFit_BF_norm, WAP_BF_LL_val_norm)
WAP_BF_LL_val_norm$BF_predicted_norm <- predictionRF_BF_val_norm
confusionMatrix(predictionRF_BF_val_norm, WAP_BF_LL_val_norm$BID_F)
postResample(predictionRF_BF_val_norm, WAP_BF_LL_val_norm$BID_F)


###########
### SVM ###
###########

## ACCURACY FOR BUILDING ND FLOOR SVM ####################################
##
## WITHOUT NORM:
## 
## WAPS
## Accuracy     Kappa 
## 0.9009901 0.8614242 
##
## WITH NORM:
##
## WAPS
## Accuracy     Kappa 
## 0.9117912 0.8768040
##
#########################################################################


SVMFit_BF <- train(BID_F~. -BID -FLOOR - LONG - LAT -USERID, 
                    data = trainSet_BF, 
                    method = "svmLinear",
                    trControl=fitControl)

predictionSVM_BF <- predict(SVMFit_BF, testSet_BF)
#performace measurment
confusionMatrix(predictionSVM_BF, testSet_BF$BID_F)
postResample(predictionSVM_BF, testSet_BF$BID_F)
#plot predicted vs actual
plot(predictionSVM_BF, testSet_BF$BID_F)


## predict validation dataset
predictionSVM_BF_val <- predict(SVMFit_BF, WAP_BF_LL_val)
WAP_BF_LL_val$BF_predicted_SVM <- predictionSVM_BF_val
confusionMatrix(predictionSVM_BF_val, WAP_BF_LL_val$BID_F)
postResample(predictionSVM_BF_val, WAP_BF_LL_val$BID_F)


## PREDICTING WITH NORMALIZATION
SVMFit_BF_norm <- train(BID_F~. -FLOOR -BID -LONG -LAT -USERID, 
                        data = trainSet_BF_norm, 
                        method = "svmLinear",
                        trControl = fitControl)

predictionSVM_BF_norm<- predict(SVMFit_BF_norm, testSet_BF_norm)
#performace measurment
confusionMatrix(predictionSVM_BF_norm, testSet_BF_norm$BID_F)
postResample(predictionSVM_BF_norm, testSet_BF_norm$BID_F)
#plot predicted vs actual
plot(predictionSVM_BF_norm, testSet_BF_norm$BID_F)



## predict validation dataset
predictionSVM_BF_val_norm <- predict(SVMFit_BF_norm, WAP_BF_LL_val_norm)
WAP_BF_LL_val_norm$BF_predicted_norm_SVM <- predictionSVM_BF_val_norm
confusionMatrix(predictionSVM_BF_val_norm, WAP_BF_LL_val_norm$BID_F)
postResample(predictionSVM_BF_val_norm, WAP_BF_LL_val_norm$BID_F)



###########
### KNN ###
###########

## ACCURACY FOR BUILDING AND FLOOR KNN ####################################
##
## WITHOUT NORM:
## 
## WAPS
## Accuracy     Kappa 
## 0.9009901 0.8614242 
##
## WITH NORM:
##
## WAPS
## Accuracy     Kappa 
## 0.9162916 0.9063041
##
##########################################################################


KNNFit_BF <- train(BID_F~. -BID -FLOOR - LONG - LAT -USERID, 
                    data = trainSet_BF, 
                    method = "knn",
                    trControl=fitControl)

predictionKNN_BF <- predict(KNNFit_BF, testSet_BF)
#performace measurment
confusionMatrix(predictionKNN_BF, testSet_BF$BID_F)
postResample(predictionKNN_BF, testSet_BF$BID_F)
#plot predicted vs actual
plot(predictionKNN_BF, testSet_BF$BID_F)


## predict validation dataset
predictionKNN_BF_val <- predict(KNNFit_BF, WAP_BF_LL_val)
WAP_BF_LL_val$BF_predicted_KNN <- predictionKNN_BF_val
confusionMatrix(predictionKNN_BF_val, WAP_BF_LL_val$BID_F)
postResample(predictionKNN_BF_val, WAP_BF_LL_val$BID_F)


## PREDICTING WITH NORMALIZATION
KNNFit_BF_norm <- train(BID_F~. -FLOOR -BID -LONG -LAT -USERID, 
                        data = trainSet_BF_norm, 
                        method = "knn",
                        trControl = fitControl)

predictionKNN_BF_norm<- predict(KNNFit_BF_norm, testSet_BF_norm)
#performace measurment
confusionMatrix(predictionKNN_BF_norm, testSet_BF_norm$BID_F)
postResample(predictionKNN_BF_norm, testSet_BF_norm$BID_F)
#plot predicted vs actual
plot(predictionKNN_BF_norm, testSet_BF_norm$BID_F)



## predict validation dataset
predictionKNN_BF_val_norm <- predict(KNNFit_BF_norm, WAP_BF_LL_val_norm)
WAP_BF_LL_val_norm$BF_predicted_norm_KNN <- predictionKNN_BF_val_norm
confusionMatrix(predictionKNN_BF_val_norm, WAP_BF_LL_val_norm$BID_F)
postResample(predictionKNN_BF_val_norm, WAP_BF_LL_val_norm$BID_F)





##########################
### 4. LONGITUDE MODEL ###
##########################

## train test for floor
inTraining_LONG <- createDataPartition(WAP_BF_LL$LONG, p = .8, list = FALSE)
trainSet_LONG <- WAP_BF_LL[inTraining_LONG,]
testSet_LONG <- WAP_BF_LL[-inTraining_LONG,]

### normalization part
# define an 80%/20% train/test split of the dataset
inTraining_LONG_norm <- createDataPartition(WAP_BF_LL_norm$LONG, p = .8, list = FALSE)
trainSet_LONG_norm <- WAP_BF_LL_norm[inTraining_LONG_norm,]
testSet_LONG_norm <- WAP_BF_LL_norm[-inTraining_LONG_norm,]


#####################
### RANDOM FOREST ###
#####################

## ERROR METRICS FOR LONGITUDE RF ####################################
##
## WITHOUT NORM:
## 
## WAPS
## 
## WAPS + BID
## 
## WAPS + BID + FLOOR
##
## WITH NORM:
##
## WAPS
## 
## WAPS + BID
##  RMSE      Rsquared       MAE 
## 11.238008  0.976711  7.627660 
## 
## WAPS + BID + FLOOR
##
##
###################################################################


RFFit_LONG <- train(LONG ~. -BID -FLOOR -BID_F -LAT -USERID, 
                    data = trainSet_LONG, 
                    method = "rf",
                    trControl=fitControl)

predictionRF_LONG <- predict(RFFit_LONG, testSet_LONG)
#performace measurment
#confusionMatrix(predictionRF_LONG, testSet_LONG$LONG)
postResample(predictionRF_LONG, testSet_LONG$LONG)
#plot predicted vs actual
plot(predictionRF_LONG, testSet_LONG$LONG)


## predict validation dataset
predictionRF_LONG_val <- predict(RFFit_LONG, WAP_BF_LL_val)
WAP_BF_LL_val$LONG_predicted <- predictionRF_LONG_val
#confusionMatrix(predictionRF_LONG_val, WAP_BF_LL_val$LONG)
postResample(predictionRF_LONG_val, WAP_BF_LL_val$LONG)


## PREDICTING WITH NORMALIZATION
RFFit_LONG_norm <- train(LONG~. -FLOOR -BID_F -LAT -USERID, 
                        data = trainSet_LONG_norm, 
                        method = "rf",
                        trControl = fitControl)

predictionRF_LONG_norm<- predict(RFFit_LONG_norm, testSet_LONG_norm)
#performace measurment
#confusionMatrix(predictionRF_BF_norm, testSet_BF_norm$BID_F)
postResample(predictionRF_LONG_norm, testSet_LONG_norm$LONG)
#plot predicted vs actual
plot(predictionRF_LONG_norm, testSet_LONG_norm$LONG)



## predict validation dataset
predictionRF_LONG_val_norm <- predict(RFFit_LONG_norm, WAP_BF_LL_val_norm)
WAP_BF_LL_val_norm$LONG_predicted_norm <- predictionRF_LONG_val_norm
#confusionMatrix(predictionRF_BF_val_norm, WAP_BF_LL_val_norm$BID_F)
postResample(predictionRF_LONG_val_norm, WAP_BF_LL_val_norm$LONG)
plot(predictionRF_LONG_val_norm, WAP_BF_LL_val_norm$LONG)



###########
### SVM ###
###########

## ERROR METRICS FOR LONGITUDE SVM ####################################
##
## WITHOUT NORM:
## 
## WAPS
##  RMSE   Rsquared        MAE 
## 42.9083999  0.7028582 31.0780332 
## 
## WAPS + BID
## RMSE   Rsquared        MAE 
## 31.9571323  0.8113285 23.2354168
## 
## WAPS + BID + FLOOR
## RMSE   Rsquared        MAE 
## 31.3189529  0.8175153 22.6880459
##
## WITH NORM:
##
## WAPS
## RMSE       Rsquared       MAE 
## 26.003939  0.865622 19.520817
## 
## WAPS + BID
## RMSE         Rsquared        MAE 
## 16.4447116  0.9486262 12.2472728
## 
## 
## WAPS + BID + FLOOR
##   RMSE       Rsquared        MAE 
## 16.5584100  0.9479758 12.3166355 
##
###################################################################

SVMFit_LONG <- train(LONG ~. -BID_F -LAT -USERID, 
                    data = trainSet_LONG, 
                    method = "svmLinear",
                    trControl=fitControl)

predictionSVM_LONG <- predict(SVMFit_LONG, testSet_LONG)
#performace measurment
#confusionMatrix(predictionRF_LONG, testSet_LONG$LONG)
postResample(predictionSVM_LONG, testSet_LONG$LONG)
#plot predicted vs actual
plot(predictionSVM_LONG, testSet_LONG$LONG)


## predict validation dataset
predictionSVM_LONG_val <- predict(SVMFit_LONG, WAP_BF_LL_val)
WAP_BF_LL_val$LONG_predicted_SVM <- predictionSVM_LONG_val
#confusionMatrix(predictionRF_LONG_val, WAP_BF_LL_val$LONG)
postResample(predictionSVM_LONG_val, WAP_BF_LL_val$LONG)


## PREDICTING WITH NORMALIZATION
SVMFit_LONG_norm <- train(LONG~. -BID_F -LAT -USERID, 
                          data = trainSet_LONG_norm, 
                          method = "svmLinear",
                          trControl = fitControl)

predictionSVM_LONG_norm<- predict(SVMFit_LONG_norm, testSet_LONG_norm)
#performace measurment
#confusionMatrix(predictionRF_BF_norm, testSet_BF_norm$BID_F)
postResample(predictionSVM_LONG_norm, testSet_LONG_norm$LONG)
#plot predicted vs actual
plot(predictionSVM_LONG_norm, testSet_LONG_norm$LONG)



## predict validation dataset
predictionSVM_LONG_val_norm <- predict(SVMFit_LONG_norm, WAP_BF_LL_val_norm)
WAP_BF_LL_val_norm$LONG_predicted_norm_SVM <- predictionSVM_LONG_val_norm
#confusionMatrix(predictionRF_BF_val_norm, WAP_BF_LL_val_norm$BID_F)
postResample(predictionSVM_LONG_val_norm, WAP_BF_LL_val_norm$LONG)
plot(predictionSVM_LONG_val_norm, WAP_BF_LL_val_norm$LONG)



###########
### KNN ###
###########

## ERROR METRICS FOR LONGITUDE KNN ####################################
##
## WITHOUT NORM:
## 
## WAPS
## RMSE         Rsquared        MAE 
## 10.4146423  0.9781637  5.8707571 
## 
## WAPS + BID
## RMSE        Rsquared        MAE 
## 10.4146423  0.9781637  5.8707571
## 
## WAPS + BID + FLOOR
## RMSE         Rsquared        MAE 
## 10.4138647  0.9781669  5.8714815 
##
##
## WITH NORM:
##
## WAPS
## RMSE      Rsquared       MAE 
## 7.2645693 0.9893796 5.0692584  
## 
## WAPS + BID
## RMSE     Rsquared      MAE 
## 7.215484 0.989522 5.038213
## 
## WAPS + BID + FLOOR
##  RMSE      Rsquared       MAE 
## 7.3201809 0.9892134 5.0240284
##
###################################################################

KNNFit_LONG <- train(LONG ~. -BID_F -LAT -USERID, 
                    data = trainSet_LONG, 
                    method = "knn",
                    trControl=fitControl)

predictionKNN_LONG <- predict(KNNFit_LONG, testSet_LONG)
#performace measurment
#confusionMatrix(predictionRF_LONG, testSet_LONG$LONG)
postResample(predictionKNN_LONG, testSet_LONG$LONG)
#plot predicted vs actual
plot(predictionKNN_LONG, testSet_LONG$LONG)


## predict validation dataset
predictionKNN_LONG_val <- predict(KNNFit_LONG, WAP_BF_LL_val)
WAP_BF_LL_val$LONG_predicted_KNN <- predictionKNN_LONG_val
#confusionMatrix(predictionRF_LONG_val, WAP_BF_LL_val$LONG)
postResample(predictionKNN_LONG_val, WAP_BF_LL_val$LONG)


## PREDICTING WITH NORMALIZATION
KNNFit_LONG_norm <- train(LONG~. -BID -FLOOR -BID_F -LAT -USERID, 
                         data = trainSet_LONG_norm, 
                         method = "knn",
                         trControl = fitControl)

predictionKNN_LONG_norm<- predict(KNNFit_LONG_norm, testSet_LONG_norm)
#performace measurment
#confusionMatrix(predictionRF_BF_norm, testSet_BF_norm$BID_F)
postResample(predictionKNN_LONG_norm, testSet_LONG_norm$LONG)
#plot predicted vs actual
plot(predictionKNN_LONG_norm, testSet_LONG_norm$LONG)



## predict validation dataset
predictionKNN_LONG_val_norm <- predict(KNNFit_LONG_norm, WAP_BF_LL_val_norm)
WAP_BF_LL_val_norm$LONG_predicted_norm_KNN <- predictionKNN_LONG_val_norm
#confusionMatrix(predictionRF_BF_val_norm, WAP_BF_LL_val_norm$BID_F)
postResample(predictionKNN_LONG_val_norm, WAP_BF_LL_val_norm$LONG)
plot(predictionKNN_LONG_val_norm, WAP_BF_LL_val_norm$LONG)






#########################
### 5. LATITUDE MODEL ###
#########################

inTraining_LAT <- createDataPartition(WAP_BF_LL$LAT, p = .8, list = FALSE)
trainSet_LAT <- WAP_BF_LL[inTraining_LAT,]
testSet_LAT <- WAP_BF_LL[-inTraining_LAT,]

### normalization part
# define an 80%/20% train/test split of the dataset
inTraining_LAT_norm <- createDataPartition(WAP_BF_LL_norm$LAT, p = .8, list = FALSE)
trainSet_LAT_norm <- WAP_BF_LL_norm[inTraining_LAT_norm,]
testSet_LAT_norm <- WAP_BF_LL_norm[-inTraining_LAT_norm,]



#####################
### RANDOM FOREST ###
#####################

## ERROR METRICS FOR LATITUDE RF ####################################
##
## WITHOUT NORM:
## 
## WAPS
##  RMSE  Rsquared       MAE 
## 27.005438  0.953028 15.186299
## 
## WAPS + BID
## 
## WAPS + BID + FLOOR
##
## WITH NORM:
##
## WAPS
## RMSE   Rsquared        MAE 
## 26.2176783  0.9557745 14.8449372 
## 
## WAPS + BID
## 
## WAPS + BID + FLOOR
##
##
###################################################################

RFFit_LAT <- train(LAT ~. -BID -FLOOR -BID_F -LONG -USERID, 
                    data = trainSet_LAT,
                    method = "rf",
                    trControl=fitControl)

predictionRF_LAT <- predict(RFFit_LAT, testSet_LAT)
#performace measurment
#confusionMatrix(predictionRF_LAT, testSet_LAT$LAT)
postResample(predictionRF_LAT, testSet_LAT$LAT)
#plot predicted vs actual
plot(predictionRF_LAT, testSet_LAT$LAT)


## predict validation dataset
predictionRF_LAT_val <- predict(RFFit_LAT, WAP_BF_LL_val)
WAP_BF_LL_val$LAT_predicted <- predictionRF_LAT_val
#confusionMatrix(predictionRF_LAT_val, WAP_BF_LL_val$LAT)
postResample(predictionRF_LAT_val, WAP_BF_LL_val$LAT)



## PREDICTING WITH NORMALIZATION
RFFit_LAT_norm <- train(LAT~. -FLOOR -BID -BID_F -LONG -USERID, 
                        data = trainSet_LAT_norm, 
                        method = "rf",
                        trControl = fitControl)

predictionRF_LAT_norm<- predict(RFFit_LAT_norm, testSet_LAT_norm)
#performace measurment
#confusionMatrix(predictionRF_BF_norm, testSet_BF_norm$BID_F)
postResample(predictionRF_LAT_norm, testSet_LAT_norm$LAT)
#plot predicted vs actual
plot(predictionRF_LAT_norm, testSet_LAT_norm$LAT)

## predict validation dataset
predictionRF_LAT_val_norm <- predict(RFFit_LAT_norm, WAP_BF_LL_val_norm)
WAP_BF_LL_val_norm$LAT_predicted_norm <- predictionRF_LAT_val_norm
#confusionMatrix(predictionRF_BF_val_norm, WAP_BF_LL_val_norm$BID_F)
postResample(predictionRF_LAT_val_norm, WAP_BF_LL_val_norm$LAT)



###########
### SVM ###
###########

## ERROR METRICS FOR LATITUDE SVM ####################################
##
## WITHOUT NORM:
## TAKES TOO LONG TO TRAIN
## WAPS
##  RMSE        Rsquared        MAE 
## 86.1869197  0.6712394 59.3363407
## 
## WAPS + BID
## RMSE         Rsquared        MAE 
## 65.2159864  0.7807547 44.0094645 
## 
## 
## WAPS + BID + FLOOR
##
## WITH NORM:
##
## WAPS
## RMSE   Rsquared        MAE 
## 43.4942428  0.8749682 33.5225970
## 
## WAPS + BID
## RMSE         Rsquared        MAE 
## 25.4733174  0.9555554 19.1291735
## 
## WAPS + BID + FLOOR
##  RMSE        Rsquared        MAE 
## 25.3874709  0.9559907 19.0773772
##
###################################################################

SVMFit_LAT <- train(LAT ~. -FLOOR -BID_F -LONG -USERID, 
                    data = trainSet_LAT,
                    method = "svmLinear",
                    trControl=fitControl)

predictionSVM_LAT <- predict(SVMFit_LAT, testSet_LAT)
#performace measurment
#confusionMatrix(predictionRF_LAT, testSet_LAT$LAT)
postResample(predictionSVM_LAT, testSet_LAT$LAT)
#plot predicted vs actual
plot(predictionSVM_LAT, testSet_LAT$LAT)


## predict validation dataset
predictionSVM_LAT_val <- predict(SVMFit_LAT, WAP_BF_LL_val)
WAP_BF_LL_val$LAT_predicted_SVM <- predictionSVM_LAT_val
#confusionMatrix(predictionRF_LAT_val, WAP_BF_LL_val$LAT)
postResample(predictionSVM_LAT_val, WAP_BF_LL_val$LAT)



## PREDICTING WITH NORMALIZATION
SVMFit_LAT_norm <- train(LAT~. -BID -FLOOR -BID_F -LONG -USERID, 
                        data = trainSet_LAT_norm, 
                        method = "svmLinear",
                        trControl = fitControl)

predictionSVM_LAT_norm<- predict(SVMFit_LAT_norm, testSet_LAT_norm)
#performace measurment
#confusionMatrix(predictionRF_BF_norm, testSet_BF_norm$BID_F)
postResample(predictionSVM_LAT_norm, testSet_LAT_norm$LAT)
#plot predicted vs actual
plot(predictionSVM_LAT_norm, testSet_LAT_norm$LAT)

## predict validation dataset
predictionSVM_LAT_val_norm <- predict(SVMFit_LAT_norm, WAP_BF_LL_val_norm)
WAP_BF_LL_val_norm$LAT_predicted_norm_SVM <- predictionSVM_LAT_val_norm
#confusionMatrix(predictionRF_BF_val_norm, WAP_BF_LL_val_norm$BID_F)
postResample(predictionSVM_LAT_val_norm, WAP_BF_LL_val_norm$LAT)



###########
### KNN ###
###########

## ERROR METRICS FOR LATITUDE KNN ####################################
## 
## BEST RESULTS WITH NORMALIZATION AND WAPS + BID
## 
## WITHOUT NORM:
## 
## WAPS  
## RMSE         Rsquared        MAE 
## 13.4711305  0.9874978  6.4188946
## 
## WAPS + BID
## RMSE         Rsquared        MAE 
## 13.4711305  0.9874978  6.4188946
## 
## WAPS + BID + FLOOR          
## RMSE        Rsquared        MAE 
## 13.4711305  0.9874978  6.4188946
##     
## WITH NORM:
## 
## WAPS
##  RMSE     Rsquared       MAE 
## 8.1480624 0.9954245 5.3689703 
## 
## WAPS + BID
##   RMSE    Rsquared       MAE 
## 8.0533099 0.9955182 5.3584980  
## 
## WAPS + BID + FLOOR
##  RMSE     Rsquared       MAE 
## 8.1324580 0.9954603 5.3943641 
## 
#################################################


KNNFit_LAT <- train(LAT ~. -BID_F -LONG -USERID, 
                    data = trainSet_LAT,
                    method = "knn",
                    trControl=fitControl)

predictionKNN_LAT <- predict(KNNFit_LAT, testSet_LAT)
#performace measurment
#confusionMatrix(predictionRF_LAT, testSet_LAT$LAT)
postResample(predictionKNN_LAT, testSet_LAT$LAT)
#plot predicted vs actual
plot(predictionKNN_LAT, testSet_LAT$LAT)

## predict validation dataset
predictionKNN_LAT_val <- predict(KNNFit_LAT, WAP_BF_LL_val)
WAP_BF_LL_val$LAT_predicted_KNN <- predictionKNN_LAT_val
#confusionMatrix(predictionRF_LAT_val, WAP_BF_LL_val$LAT)
postResample(predictionKNN_LAT_val, WAP_BF_LL_val$LAT)



## PREDICTING WITH NORMALIZATION
KNNFit_LAT_norm <- train(LAT~. -FLOOR -BID_F -LONG -USERID, 
                        data = trainSet_LAT_norm, 
                        method = "knn",
                        trControl = fitControl)

predictionKNN_LAT_norm<- predict(KNNFit_LAT_norm, testSet_LAT_norm)
#performace measurment
#confusionMatrix(predictionRF_BF_norm, testSet_BF_norm$BID_F)
postResample(predictionKNN_LAT_norm, testSet_LAT_norm$LAT)
#plot predicted vs actual
plot(predictionKNN_LAT_norm, testSet_LAT_norm$LAT)

## predict validation dataset
predictionKNN_LAT_val_norm <- predict(KNNFit_LAT_norm, WAP_BF_LL_val_norm)
WAP_BF_LL_val_norm$LAT_predicted_norm_KNN <- predictionKNN_LAT_val_norm
#confusionMatrix(predictionRF_BF_val_norm, WAP_BF_LL_val_norm$BID_F)
postResample(predictionKNN_LAT_val_norm, WAP_BF_LL_val_norm$LAT)



#########################
### 6. VISUALIZATIONS ###
#########################

#ggplot(WAP_BF_LL_val, aes(x = LAT, y = LAT_predicted, color = LAT)) + geom_point() 

## training vs validation dataset locations
ggplot()+ geom_point(data = WAP_BF_LL_norm , aes(x = LONG, y = LAT, colour = "Training set"))
        + geom_point(data = WAP_BF_LL_val_norm , aes(x = LONG, y = LAT, colour = "Validation set"))
        + ggtitle("Training vs validation dataset locations") 
        + theme_minimal()



plot_ly(WAP_BF_LL_val_norm, x = ~LONG_predicted_norm_KNN, y = ~LAT_predicted_norm_KNN, z = ~F_predicted_norm_KNN) %>%
        add_markers() %>%
        layout(scene = list(xaxis = list(title = 'Longitude'),
               yaxis = list(title = 'Latitude'),
               zaxis = list(title = 'Floor')))

plot_ly(type = "scatter3d", mode = "markers", WAP_BF_LL_val_norm, x = ~LONG, y = ~LAT, z = ~FLOOR, colours = "mediumturquoise") %>%
        add_markers() %>%
        layout(scene = list(xaxis = list(title = 'Longitude'),
               yaxis = list(title = 'Latitude'),
               zaxis = list(title = 'Floor')))


ggplot() + 
       geom_point(data = WAP_BF_LL_val_norm, aes(x = LONG, y = LAT, colour = "Original")) + 
       labs(x = "Longitude", y = "Latitude", title = "Validation location original vs predicted")  +
       facet_wrap(~FLOOR, nrow = 1) + 
       geom_point(data = WAP_BF_LL_val_norm, aes(x = LONG_predicted_norm_KNN, y = LAT_predicted_norm_KNN, colour = "Predicted")) +
       theme_minimal()

ggplot() +
    geom_point(data = WAP_BF_LL_val_norm , aes(x = LONG, y = LAT, colour = "Original")) +
    geom_point(data = WAP_BF_LL_val_norm , aes(x = LONG_predicted_norm_KNN, y = LAT_predicted_norm_KNN, colour = "Predicted")) +
    ggtitle("Validation set values vs predicted") +
    theme_minimal()



plot(LAT ~ LONG, data = WAP_BF_LL_val_norm, pch = 20, col = "lightskyblue1")
points(LAT_predicted_norm_KNN ~ LONG_predicted_norm_KNN, data = WAP_BF_LL_val_norm, pch = 20, col = "red")

## calculate position error ## 
## LL_Error 8.219731 ###
WAP_BF_LL_val_norm$LL_Error <- sqrt((WAP_BF_LL_val_norm$LONG - WAP_BF_LL_val_norm$LONG_predicted_norm_KNN)^2 +
                                    (WAP_BF_LL_val_norm$LAT - WAP_BF_LL_val_norm$LAT_predicted_norm_KNN)^2)
hist(WAP_BF_LL_val_norm$LL_Error, 
     freq = T,
     xlab = " Absolute error (m)", 
     col = "light blue",
     main = "Distance error for position")


## calculate mean of lat ##
## LAT_Error  5.394364 ##
WAP_BF_LL_val_norm$LAT_Error <- (WAP_BF_LL_val_norm$LAT - WAP_BF_LL_val_norm$LAT_predicted_norm_KNN)
hist(WAP_BF_LL_val_norm$LAT_Error, 
     freq = T, 
     xlab = " Absolute error (m)", 
     col = "light blue", 
     main = "Distance error in meters LATITUDE")

## calculate mean of long ##
## LONG_Error 5.038213 ###
WAP_BF_LL_val_norm$LONG_Error <- (WAP_BF_LL_val_norm$LONG - WAP_BF_LL_val_norm$LONG_predicted_norm_KNN)
hist(WAP_BF_LL_val_norm$LONG_Error, 
     freq = T, 
     xlab = " Absolute error (m)", 
     col = "light blue", 
     main = "Distance error in meters LONGITUDE")


## BUILDING
plot(RFFit_BID_norm, main = "RF model for building")
plot(KNNFit_BID_norm, main = "KNN model for building")

## FLOOR
plot(RFFit_F_norm)
plot(KNNFit_F_norm)

## LONG
plot(RFFit_LONG_norm)
plot(KNNFit_LONG_norm)

## LAT
plot(RFFit_LAT_norm)
plot(KNNFit_LAT_norm)

# Stop Cluster. After performing your tasks, stop your cluster. 
stopCluster(cl)

