#Plotting site T times series for paper 1


#-----------------------------------------------------------------------------------------
rm(list=ls())
library(data.table)
library(rpca)
library(rsvd)
library(gridExtra)
library(reshape2)
library(ggplot2)
library(dplyr)
library(plyr)
library(lubridate)
library(R.matlab)

#-----------------------------------------------------------------------------------------

##
#upload data
#too clunky
setwd("E:\\SDT\\Figures for Paper\\Timeseries")
zID=readMat("T_01_zID.mat")

#also too clunky
## upload csv files
#T_01
T_01_labels=as.data.frame(fread('T_01_labels.csv'))
T_01_times=as.data.frame(fread('T_01_times.csv'))
#T_02
T_02_labels=as.data.frame(fread('T_02_labels.csv'))
T_02_times=as.data.frame(fread('T_02_times.csv'))
#T_03
T_03_labels=as.data.frame(fread('T_03_labels.csv'))
T_03_times=as.data.frame(fread('T_03_times.csv'))

#going with this
##upload together
T_all=as.data.frame(fread('T_01_02_03.csv'))

##separate chorus values
T_chorus=T_all[T_all[,2]==1,]
T_chorus_times=T_all[,1]



