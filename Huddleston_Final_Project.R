# MSDS 411 Final Project. Assignment 4
#  Huddleston, James
# https://hackr.io/blog/k-means-clustering
# Call Detail Record (CDR) Analysis K-means clustering
# https://github.com/treselle-systems/CDR_analysis_using_K_means_in_Tableau
## 
# Installing
install.packages("readr")
# Loading
library(data.table)
library(dplyr)
library("ggplot2")
library(tidyr)
library('caret')
library(Rtsne)
library(ggpcorrplot)
library("reshape")#install.packages("reshape")
#######
#set working directory 
setwd('C:/Users/huddl/OneDrive/A_Northwestern University/MSDS_411_Unsupervised/Final_Project/Huddleston_James_FinalProject/')
# read Italia cellular network over the city of Milano
cdr_input <- read.csv('sms-call-internet-mi-2013-11-17.txt',sep="\t",header=F)
# create a csv of the dataframe
write.csv(cdr_input, 'sms-call-internet-mi-2013-11-17.csv')
# Read tab separated values data set
#data_frame <- fread('sms-call-internet-mi-2013-11-17.txt')
#head(data_frame)  
#summary(data_frame)
#########################################
## EDA
#check the head and tail of the dataset 
head(cdr_input)
tail(cdr_input)
# check the number of observations of the data set
dim(cdr_input)
# the structure of the dataset 
str(cdr_input)
# statistical summary of dataset 
summary(cdr_input)
# rename the column numbers to the actual labed names from the decription. 
colnames(cdr_input) <- c("square_id","time_interval","country_code","sms_in_activity","sms_out_activity","call_in_activity","call_out_activity","internet_traffic_activity")
# subset the input dataframe for the 500 square_id: The id of the square that is part of the city GRID
# As it has five million records, a subset of the file containing activity information for 500 square IDs is used as a use case.
cdr_input_subset_df <- subset(cdr_input,cdr_input$square_id<=500)
head(cdr_input_subset_df)
dim(cdr_input_subset_df)
str(cdr_input_subset_df)
summary(cdr_input_subset_df)
#creating backup of subset dataframe fo reating missing values
cdr_input_subset_df_bkp <- cdr_input_subset_df
 
####################################################
# function used to additional fields to the dataframe  for treating missing values
#cdr_input_subset_df_1 <- cdr_input_subset_df%>%group_by(country_code)%>%mutate(funs(replace(.,which(is.na(.)),mean(., na.rm=TRUE))))
#summary(cdr_input_subset_df_1)

cdr_input_subset_df_cln <- cdr_input_subset_df%>% group_by(country_code) %>%mutate(sms_in_activity = ifelse(is.na(sms_in_activity),as.integer(mean(sms_in_activity, na.rm = TRUE)), sms_in_activity))

cdr_input_subset_df_cln  <- cdr_input_subset_df_cln %>% mutate(sms_in_activity = ifelse(is.na(sms_in_activity),0, sms_in_activity))

cdr_input_subset_df_cln <- cdr_input_subset_df_cln%>% group_by(country_code) %>%mutate(sms_out_activity = ifelse(is.na(sms_out_activity),as.integer(mean(sms_out_activity, na.rm = TRUE)), sms_out_activity))

cdr_input_subset_df_cln  <- cdr_input_subset_df_cln %>% mutate(sms_out_activity = ifelse(is.na(sms_out_activity),0, sms_out_activity))


cdr_input_subset_df_cln <- cdr_input_subset_df_cln%>% group_by(country_code) %>%mutate(call_in_activity = ifelse(is.na(call_in_activity),as.integer(mean(call_in_activity, na.rm = TRUE)), call_in_activity))

cdr_input_subset_df_cln  <- cdr_input_subset_df_cln %>% mutate(call_in_activity = ifelse(is.na(call_in_activity),0, call_in_activity))


cdr_input_subset_df_cln <- cdr_input_subset_df_cln%>% group_by(country_code) %>%mutate(call_out_activity = ifelse(is.na(call_out_activity),as.integer(mean(call_out_activity, na.rm = TRUE)), call_out_activity))

cdr_input_subset_df_cln  <- cdr_input_subset_df_cln %>% mutate(call_out_activity = ifelse(is.na(call_out_activity),0, call_out_activity))

summary(cdr_input_subset_df_cln)

#########################################
# convert numeric column into factor column
###
factorColumns <- c("square_id","country_code")
cdr_input_subset_df_cln[factorColumns] <- lapply(cdr_input_subset_df_cln[factorColumns],as.factor)
head(cdr_input_subset_df_cln)
###########
# calculate the datetime from epoch unix time
#####
val <- cdr_input_subset_df_cln$time_interval/1000
cdr_input_subset_df_cln$outputTime <- as.POSIXct(val, origin="1970-01-01",tz="UTC")
head(cdr_input_subset_df_cln)
####
# derive activity start date and hour from time interval
###########
# derive activity start date and hour from time interval
cdr_input_subset_df_cln$activity_start_time <- cdr_input_subset_df_cln$outputTime 
#cdr_input_subset_df$activity_date <- as.Date(as.POSIXct(cdr_input_subset_df$activity_start_time,origin="1970-01-01"))
cdr_input_subset_df_cln$activity_date <- as.Date(cdr_input_subset_df_cln$activity_start_time)
cdr_input_subset_df_cln$activity_time <- format(cdr_input_subset_df_cln$activity_start_time,"%H")

# derive total activity from sms in and out, call in and out and internet traffic activity 
cdr_input_subset_df_cln$total_activity <- rowSums(cdr_input_subset_df_cln[, c(4,5,6,7,8)],na.rm=T)

#############
#    plotting the activity_time Vs total_activity
##################################################
totalActivityDF <- aggregate(total_activity ~ activity_time,cdr_input_subset_df_cln,FUN=sum)

totalActivityPlot <- ggplot(data=totalActivityDF, aes(x=activity_time, y=total_activity)) + geom_bar(stat="identity")+theme_bw()

print(totalActivityPlot)

#####
#  plotting square_id Vs total_activity
####
totalGridActivityDF <- aggregate(total_activity ~ square_id,cdr_input_subset_df_cln,FUN=sum)

# sort the data frame by total acitivity descending
totalGridActivityDF <- totalGridActivityDF[order(-totalGridActivityDF$total_activity),]

totalGridActivitySubset <- head(totalGridActivityDF,25)

# top 100 grids by total acitivity
totalGridActivityPlot <- ggplot(data=totalGridActivitySubset, aes(x=reorder(square_id,-total_activity),   y=total_activity)) + geom_bar(stat="identity")+theme_bw()

print(totalGridActivityPlot)

#####
#   plotting country_code Vs total_activity  
#######
# dummy count variable
cdr_input_subset_df_cln$count <- 1

# Sum the total number of acitivity by country code
totalActivityCountryDF <- aggregate(count ~ country_code,cdr_input_subset_df_cln,FUN=sum)

# sort the data frame by 
totalActivityCountryDF <- totalActivityCountryDF[order(-totalActivityCountryDF$count),]

totalActivityCountryPlot <- ggplot(data=head(totalActivityCountryDF,20), aes(x=factor(1), fill=country_code)) + geom_bar(width=1)+coord_polar("y")

head(totalActivityCountryDF)
print(totalActivityCountryPlot)

################################################
#   Using elbow method to find the optimum number of clusters
####################
# subset the required columns
cdrActivityDF <- subset(cdr_input_subset_df_cln,select=c("activity_time","total_activity"))

# find the vector of sum of squared error(SSE) for each cluster selection
wcss <- vector()
for(i in 1:20) wcss[i] = sum(kmeans(cdrActivityDF,i)$withinss)

# form the dataframe with cluster and its SSE
clusterFrame <- data.frame(withinss=wcss,Cluster=seq(1:20))

# plot the result and selects the optimal cluster number by looking at the graph. When number of cluster increases then the SSE will be reduced.
ggplot(data=clusterFrame, aes(x=Cluster, y=withinss, group=1)) + geom_line(colour="red", size=1.5) + geom_point(colour="red", size=4, shape=21, fill="white")+theme_bw()


################################################
# Applying K-Means Clustering for activity time and total activity
################################################################
# subset the required columns
cdrActivityDF <- subset(cdrActivityDF,select=c("activity_time","total_activity"))

# Applying k-means on acitivity hours and total_activity
cdrClusterModel <- kmeans(cdrActivityDF,10,nstart=10)

# print the summary of the model
print(summary(cdrClusterModel))
###
print(cdrClusterModel$centers)
# Append the identified cluster to the input dataframe
cdrActivityDF$cluster <- as.factor(cdrClusterModel$cluster)
cdrActivityDF$activity_time <- as.factor(cdrActivityDF$activity_time)

# melt the dataframe
cdrActivityDF.melt <- melt(cdrActivityDF)

#####
# heat map plot for the clustering results  the most revenue generating clusters
cdrActivityClusterPlot <- ggplot(cdrActivityDF.melt, aes(activity_time,cluster)) + geom_tile(aes(fill = value))+ scale_fill_gradient(low = "#FBCAC5",high = "#D66889")+theme_bw()
print(cdrActivityClusterPlot)

##
# Applying K-Means Clustering for other variables
#####
## Using elbow method to find the optimum number of clusters 
# subset the required columns
cdr_cluster <- subset(cdr_input_subset_df_cln,select=c("country_code",
                                                       "sms_in_activity",
                                                       "sms_out_activity",
                                                       "call_in_activity",
                                                       "call_out_activity",
                                                       "internet_traffic_activity"
))
## cleaning na from the dataset 
cdr_cluster  <- cdr_cluster%>% drop_na(internet_traffic_activity)%>% drop_na(call_out_activity)%>% drop_na(sms_out_activity)

# Using the elbow method to find the optimal number of clusters

install.packages('caTools')
library(caTools)
set.seed(6)
wcss = vector()
for (i in 1:10) wcss[i] = sum(kmeans(cdr_cluster, i)$withinss)
 plot(1:10,
      wcss,
      type = 'b',
      main = paste('The Elbow Method'),
      xlab = 'Number of clusters',
      ylab = 'WCSS')

# Fitting K-Means to the dataset
set.seed(29)
kmeans = kmeans(x = cdr_cluster, centers = 10)
y_kmeans = kmeans$cluster

#####
#  Visualising K-Means clusters
#######
# Visualising the clusters
library(cluster)
clusplot(cdr_cluster[c("country_code", "sms_in_activity")],
         y_kmeans,
         lines = 0,
         shade = TRUE,
         color = TRUE,
         labels = 6,
         plotchar = FALSE,
         span = TRUE,
         main = paste('Clusters of CDR Data'),
         xlab = 'country_code',
         ylab = 'sms_in_activity')


#####
clusplot(cdr_cluster[c("call_in_activity", "internet_traffic_activity")],
         y_kmeans,
         lines = 0,
         shade = TRUE,
         color = TRUE,
         labels = 6,
         plotchar = FALSE,
         span = TRUE,
         main = paste('Clusters of CDR Data'),
         xlab = 'call_in_activity',
         ylab = 'internet_traffic_activity')

##    END R Code for Assignment 4  #####################################################################
###################################################################################################
# Alternative method create three new variable from the 8 original variables to determine peak hours 
##################################################################################################
library("ggplot2")
library("reshape")
library("gridExtra")

# set the working directory
setwd('C:/Users/huddl/OneDrive/A_Northwestern University/MSDS_411_Unsupervised/Final_Project/Huddleston_James_FinalProject/')


# function used to read the data input file and returns the dataframe
fn.readCallDetailRecord <- function(inputFile){
  # read Italia cellular network over the city of Milano
  inputDF <- read.csv(file=inputFile,sep="\t",header=F)
  
  # check the number of observations
  nrow(inputDF)
  
  # check the structure of the dataset
  str(inputDF)
  
  # rename the dataframe columns
  colnames(inputDF) <- c("square_id","time_interval","country_code","sms_in_activity","sms_out_activity","call_in_activity","call_out_activity","internet_traffic_activity")
  
  # subset the input dataframe for the 500 square_ids
  inputSubsetDF <- subset(inputDF,inputDF$square_id<=500)
  return(inputSubsetDF)
}

# function used to additional fields to the dataframe
fn.deriveAdditionalFields <- function(inputSubsetDF){
  # convert numeric column into factor column
  factorColumns <- c("square_id","country_code")
  inputSubsetDF[factorColumns] <- lapply(inputSubsetDF[factorColumns],as.factor)
  
  # derive activity start date and hour from time interval
  inputSubsetDF$activity_start_time <- fn.findStartTimeInterval(inputSubsetDF$time_interval)
  inputSubsetDF$activity_date <- as.Date(as.POSIXct(inputSubsetDF$activity_start_time,origin="1970-01-01"))
  inputSubsetDF$activity_time <- format(inputSubsetDF$activity_start_time,"%H")
  
  # derive total activity from sms in and out, call in and out and internet traffic activity 
  inputSubsetDF$total_activity <- rowSums(inputSubsetDF[, c(4,5,6,7,8)],na.rm=T)
  return(inputSubsetDF)
}

# function used to calculate the datetime from epoch unix time
fn.findStartTimeInterval <- function(inputTime){
  val <- inputTime/1000
  outputTime <- as.POSIXct(val, origin="1970-01-01",tz="UTC")
  return(outputTime)
}

# function used to plot the activity_time Vs total_activity
fn.findPeekActivityByHours <- function(inputSubsetDF){
  # Sum the total acitivity by hours
  totalActivityDF <- aggregate(total_activity ~ activity_time,inputSubsetDF,FUN=sum)
  
  totalActivityPlot <- ggplot(data=totalActivityDF, aes(x=activity_time, y=total_activity)) + geom_bar(stat="identity")+theme_bw()
  
  print(totalActivityPlot)
}

# function used to plot square_id Vs total_activity 
fn.findPeekActivityByGrid <- function(inputSubsetDF){
  # Sum the total acitivity by square grid id
  totalGridActivityDF <- aggregate(total_activity ~ square_id,inputSubsetDF,FUN=sum)
  
  # sort the data frame by total acitivity descending
  totalGridActivityDF <- totalGridActivityDF[order(-totalGridActivityDF$total_activity),]
  
  totalGridActivitySubset <- head(totalGridActivityDF,25)
  
  # top 100 grids by total acitivity
  totalGridActivityPlot <- ggplot(data=totalGridActivitySubset, aes(x=reorder(square_id,-total_activity), y=total_activity)) + geom_bar(stat="identity")+theme_bw()
  
  print(totalGridActivityPlot)
}

# function used to plot country_code Vs total_activity 
fn.findPeekActivityByCountryCode <- function(inputSubsetDF){
  # dummy count variable
  inputSubsetDF$count <- 1
  
  # Sum the total number of acitivity by country code
  totalActivityCountryDF <- aggregate(count ~ country_code,inputSubsetDF,FUN=sum)
  
  # sort the data frame by 
  totalActivityCountryDF <- totalActivityCountryDF[order(-totalActivityCountryDF$count),]
  
  totalActivityCountryPlot <- ggplot(data=head(totalActivityCountryDF,10), aes(x=factor(1), fill=country_code)) + geom_bar(width=1)+coord_polar("y")
  
  print(totalActivityCountryPlot)
}

fn.findOptimalCluster <- function(inputSubsetDF){
  # subset the required columns
  cdrActivityDF <- subset(inputSubsetDF,select=c("activity_time","total_activity"))
  
  # find the vector of sum of squared error(SSE) for each cluster selection
  wcss <- vector()
  for(i in 1:20) wcss[i] = sum(kmeans(cdrActivityDF,i)$withinss)
  
  # form the dataframe with cluster and its SSE
  clusterFrame <- data.frame(withinss=wcss,Cluster=seq(1:20))
  
  # plot the result and selects the optimal cluster number by looking at the graph. When number of cluster increases then the SSE will be reduced.
  ggplot(data=clusterFrame, aes(x=Cluster, y=withinss, group=1)) + geom_line(colour="red", size=1.5) + geom_point(colour="red", size=4, shape=21, fill="white")+theme_bw()
}

# function used to apply the K-means algorithm
fn.findCDRKMeans <- function(cdrActivityDF){
  # subset the required columns
  cdrActivityDF <- subset(cdrActivityDF,select=c("activity_time","total_activity"))
  
  # Applying k-means on acitivity hours and total_activity
  cdrClusterModel <- kmeans(cdrActivityDF,10,nstart=10)
  
  # print the summary of the model
  print(summary(cdrClusterModel))
  
  # Append the identified cluster to the input dataframe
  cdrActivityDF$cluster <- as.factor(cdrClusterModel$cluster)
  cdrActivityDF$activity_time <- as.factor(cdrActivityDF$activity_time)
  
  # melt the dataframe
  cdrActivityDF.melt <- melt(cdrActivityDF)
  
  # heat map plot for the clustering results
  cdrActivityClusterPlot <- ggplot(cdrActivityDF.melt, aes(activity_time,cluster)) + geom_tile(aes(fill = value))+ scale_fill_gradient(low = "#FBCAC5",high = "#D66889")+theme_bw()
  print(cdrActivityClusterPlot)
}

# function used to perform CDR cluster analysis
fn.cdrClusterAnalysis <- function(){
  #Data preprocessing
  cdrAcitivityInputDF <- fn.readCallDetailRecord("sms-call-internet-mi-2013-11-17.txt")
  cdrAcitivityInputDF <- fn.deriveAdditionalFields(cdrAcitivityInputDF)
  
  #EDA
  fn.findPeekActivityByHours(cdrAcitivityInputDF)
  fn.findPeekActivityByGrid(cdrAcitivityInputDF)
  fn.findPeekActivityByCountryCode(cdrAcitivityInputDF)
  
  #K-means
  fn.findCDRKMeans(cdrAcitivityInputDF)
}

fn.cdrClusterAnalysis()
#########################################################################################################
## 
########################################################################################################

