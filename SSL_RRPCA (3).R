#SSL: RRPCA
# Created by: Ella Bea Kim for the JASA public
# Last updated: January 2023
#The following code shows an example of how the RRPCA step (denoising) is implemented. 
#Please access "READ ME: SSL method slideshow" for an overview of steps and example parameters
#Note that this script utilizes R language while all other toolboxes exist in MATLAB under https://github.com/MarineBioAcousticsRC/Triton
#Acoustic data for MBNMS available via the Sanctsound data portal (https://sanctsound.ioos.us/).
#Example dataset in dryad titled: "SDT_BF_01_PSD_20min_20Hz.csv"

# Overview: RRPCA separates original matrix into Low Rank (LR) and Sparse (SP) matrices. Chronic sounds like fish chorus should be separated into the LR matrix, and transient noises like small vessel noise should be separated into the SP matrix. Note this is all dependent on how you change frequency binning.

#-----------------------------------------------------------------------------------------
#first add packages to library. make sure you have these installed prior to running this line.
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

#upload data
setwd("INSERT WORKING DIRECTORY") #set working directory
#using example csv file upload but can insert own soundscape metrics csv file
SDTdata=as.data.frame(fread('SDT_BF_01_PSD_20min_20Hz.csv')) #upload csv-- which is output from soundscape metrics toolbox (MATLAB)
nv=SDTdata #simplify language

#save as data frame to get values numeric, change column names to just frequency values
as.data.frame(colnames(nv))
hix  = as.numeric( gsub("PSD_","",names(nv)[4:42]) ) #gives freq range for plotting--can change values based on signal of interest
Nv   = as.data.frame(as.matrix( ( nv[,4:42]) )) 

#Plot original data
colnames(Nv) = hix #this makes the column names numeric 
orgdata <- reshape :: melt(t(Nv)  ) #melt gets the data into a columns that ggplot recognizes 
pO = ggplot(orgdata, aes(X1, value, group = as.factor(X2)))+ 
  geom_line(alpha=.05)+ scale_x_continuous()+ 
  ggtitle("Original Data")+ xlab("")+ theme_minimal() #changing alpha value makes plot more legible
#x axis frequency, y axis SL in dB
#plots entire deployment
#-----------------------------------------------------------------------------------------

#setup input and run rrpca
input = Nv 
#RUN RRPCA
nvpcaTOL = rrpca(input)  

#LOW RANK RESULTS
Lr = as.data.frame(nvpcaTOL$L) #separate LR
colnames(Lr) = hix #assign hix to column names so that it registers as numeric
NvMlr <- reshape :: melt(t(Lr)  )
pL = ggplot(NvMlr, aes(X1, value, group = as.factor(X2)))+ 
  geom_line(alpha=.05)+ scale_x_continuous()+ 
  ggtitle("Low rank")+ xlab("")+ theme_minimal() #can change alpha
#this should result in a plot that gets rid of some of the wiggle. 

#SPARSE RESULTS
Sp = as.data.frame(nvpcaTOL$S) 
colnames(Sp) = hix 
NvMSp <- reshape :: melt(t(Sp)  )
pS = ggplot(NvMSp, aes(X1, value, group = as.factor(X2)))+ 
  geom_line(alpha=.3 )+ scale_x_continuous()+ 
  ggtitle("Sparse")+ xlab("")+ theme_minimal() #changed alpha from .05 to .5 to be able to see these outliers more
#this should result in a plot of the transient signals 

#plot original, low rank, and sparse all together
#this plots entire deployment but can section off particular weeks and plot-- 
grid.arrange(pO,pL,pS)

#save matrices
#low rank
Lrmatrix=as.matrix(Lr) #get the LR data (in decibels) into matrix form
#sparse
Spmatrix=as.matrix(Sp)
#original
Omatrix=as.matrix(Nv)

#if curious to visualize as LTSA like plots-- section of weeks from LR, Sp, OM, then use image(Lrmatrix),image(SpMatrix), etc.

### save matrices(for entire deployment)
write.csv(Lrmatrix, "INSERT OUTPUT DIRECTORY.csv",row.names=FALSE) #insert output directories here
write.csv(Spmatrix, "INSERT OUTPUT DIRECTORY.csv",row.names=FALSE)
write.csv(Omatrix, "INSERT OUTPUT DIRECTORY.csv",row.names=FALSE)

#saving time vectors
OMTT=nv[1]
Tmatrix=as.matrix(OMTT) #get the LR data (in decibels) into matrix form
write.csv(Tmatrix,"INSERT OUTPUT DIRECTORY\\OMTT.csv",row.names=FALSE)

#from here you can save csv file into a tpws file with OMTT and upload it into matlab-- and run the clustering toolkit on it, followed by the neural network toolkit:
#https://github.com/MarineBioAcousticsRC/Triton

