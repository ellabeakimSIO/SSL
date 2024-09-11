# SOCAL RRPCA

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

#-----------------------------------------------------------------------------------------

#Deployment SOCAL_35_P

#RRPCA for entire matrix

setwd("E:\\SOCAL\\SOCAL_RRPCA\\Soundscape_Metrics_SOCAL\\SOCAL_35_P")
SOCALdata=as.data.frame(fread('SOCAL_35_P_LTSA_1s_1Hz_PSD_20min.csv'))
nv=SOCALdata #change to megan's language- and delete row of infinity values

#save as data frame to get values numeric, change column names to just frequency values, convert from dv to pressure
as.data.frame(colnames(nv))
hix  = as.numeric( gsub("PSD_","",names(nv)[4:41]) ) #gives freq range for plotting--ELLA changed from 25 to 17 b/c it was introducing NAs before. and didn't run lines 160-164... do i need those or is that only for truncating step????
Nv   = as.data.frame(as.matrix( ( nv[,4:41]) )) 
NvP  = as.data.frame(10^(Nv/20))     #converting from db to pressure-- "un-dbing"

#Plot original data
colnames(Nv) = hix #thist makes the column names numeric rather than TOL_123
orgdata <- reshape :: melt(t(Nv)  ) #melt gets the data into a columns that ggplot recognizes 
pO = ggplot(orgdata, aes(X1, value, group = as.factor(X2)))+ 
  geom_line(alpha=.05)+ scale_x_continuous()+ 
  ggtitle("Original Data")+ xlab("")+ theme_minimal() 
#skipping ploting step but keeping this here 

#RRPCA method for source separation
#-----------------------------------------------------------------------------------------

#setup input and run rrpca
input = NvP # or Nv (dB units)
nvpcaTOL = rrpca(input) #PREDICTED RANK IS OFTEN ZERO... is that BAD? lamda was screwing this up. 
#NOTE: pressure units avoid a negative change in ambient when transients removed

#LOW RANK RESULTS
Lr = as.data.frame(nvpcaTOL$L) # all negative values. WAIT L is ALL THE SAME VALUE, but S is diff values.... now megan and i fixed this.
LrDB = 10*log10( Lr^2 ) #these values are all very close
colnames(LrDB) = hix #assign hix to column names so that it registers as numeric
NvMlr <- reshape :: melt(t(LrDB)  )
pL = ggplot(NvMlr, aes(X1, value, group = as.factor(X2)))+ 
  geom_line(alpha=.05)+ scale_x_continuous()+ 
  ggtitle("Low rank")+ xlab("")+ theme_minimal() #changed alpha from 0.05 to 0.2
#this should result in a plot that gets rid of some of the wiggle. 

#SPARSE RESULTS
Sp = as.data.frame(nvpcaTOL$S) # all positive values... THERE ARE NEG VALUES HERE WHICH IS ODD
SpDB = 10*log10( Sp^2 ) #something is off here.... inf values???
colnames(Sp) = hix 
NvMSp <- reshape :: melt(t(Sp)  )
pS = ggplot(NvMSp, aes(X1, value, group = as.factor(X2)))+ 
  geom_line(alpha=.3 )+ scale_x_continuous()+ 
  ggtitle("Sparse")+ xlab("")+ theme_minimal() #changed alpha from .05 to .5 to be able to see these outliers more

#save matrices
Lrmatrix=as.matrix(LrDB) #get the LR data (in decibels) into matrix form
#now lets just see what image of sparse looks like
Spmatrix=as.matrix(SpDB)
Spmatrix[Spmatrix == -Inf] <- 0 #get rid of all the -inf values bc sparse matrix is screwed up
#now original
Omatrix=as.matrix(Nv)

### write matrices(for entire deployment)
write.csv(Lrmatrix, "E:\\SOCAL\\SOCAL_RRPCA\\RRPCA_SOCAL\\matrices\\SOCAL_35_P\\Lrmatrix_SOCAL_P_35.csv",row.names=FALSE)
write.csv(Spmatrix, "E:\\SOCAL\\SOCAL_RRPCA\\RRPCA_SOCAL\\matrices\\SOCAL_35_P\\Spmatrix_SOCAL_P_35.csv",row.names=FALSE)
write.csv(Omatrix, "E:\\SOCAL\\SOCAL_RRPCA\\RRPCA_SOCAL\\matrices\\SOCAL_35_P\\Omatrix_SOCAL_P_35.csv",row.names=FALSE)

#saving time vector as csv
OMTT=nv[1]
Tmatrix=as.matrix(OMTT) #get the LR data (in decibels) into matrix form
write.csv(Tmatrix,"E:\\SOCAL\\SOCAL_RRPCA\\RRPCA_SOCAL\\matrices\\SOCAL_35_P\\OMTT.csv",row.names=FALSE)


#-----------------------------------------------------------------------------------------

#Deployment SOCAL_4_A

#RRPCA for entire matrix

setwd("E:\\SOCAL\\SOCAL_RRPCA\\Soundscape_Metrics_SOCAL\\SOCAL_04_A")
SOCALdata=as.data.frame(fread('SOCAL_04_A_LTSA_1s_1Hz_PSD_20min.csv'))
nv=SOCALdata #change to megan's language- and delete row of infinity values

#save as data frame to get values numeric, change column names to just frequency values, convert from dv to pressure
as.data.frame(colnames(nv))
hix  = as.numeric( gsub("PSD_","",names(nv)[4:41]) ) #gives freq range for plotting--ELLA changed from 25 to 17 b/c it was introducing NAs before. and didn't run lines 160-164... do i need those or is that only for truncating step????
Nv   = as.data.frame(as.matrix( ( nv[,4:41]) )) 
NvP  = as.data.frame(10^(Nv/20))     #converting from db to pressure-- "un-dbing"

#Plot original data
colnames(Nv) = hix #thist makes the column names numeric rather than TOL_123
orgdata <- reshape :: melt(t(Nv)  ) #melt gets the data into a columns that ggplot recognizes 
pO = ggplot(orgdata, aes(X1, value, group = as.factor(X2)))+ 
  geom_line(alpha=.05)+ scale_x_continuous()+ 
  ggtitle("Original Data")+ xlab("")+ theme_minimal() 
#skipping ploting step but keeping this here 

#RRPCA method for source separation
#-----------------------------------------------------------------------------------------

#setup input and run rrpca
input = NvP # or Nv (dB units)
nvpcaTOL = rrpca(input) #PREDICTED RANK IS OFTEN ZERO... is that BAD? lamda was screwing this up. 
#NOTE: pressure units avoid a negative change in ambient when transients removed

#LOW RANK RESULTS
Lr = as.data.frame(nvpcaTOL$L) # all negative values. WAIT L is ALL THE SAME VALUE, but S is diff values.... now megan and i fixed this.
LrDB = 10*log10( Lr^2 ) #these values are all very close
colnames(LrDB) = hix #assign hix to column names so that it registers as numeric
NvMlr <- reshape :: melt(t(LrDB)  )
pL = ggplot(NvMlr, aes(X1, value, group = as.factor(X2)))+ 
  geom_line(alpha=.05)+ scale_x_continuous()+ 
  ggtitle("Low rank")+ xlab("")+ theme_minimal() #changed alpha from 0.05 to 0.2
#this should result in a plot that gets rid of some of the wiggle. 

#SPARSE RESULTS
Sp = as.data.frame(nvpcaTOL$S) # all positive values... THERE ARE NEG VALUES HERE WHICH IS ODD
SpDB = 10*log10( Sp^2 ) #something is off here.... inf values???
colnames(Sp) = hix 
NvMSp <- reshape :: melt(t(Sp)  )
pS = ggplot(NvMSp, aes(X1, value, group = as.factor(X2)))+ 
  geom_line(alpha=.3 )+ scale_x_continuous()+ 
  ggtitle("Sparse")+ xlab("")+ theme_minimal() #changed alpha from .05 to .5 to be able to see these outliers more

#save matrices
Lrmatrix=as.matrix(LrDB) #get the LR data (in decibels) into matrix form
#now lets just see what image of sparse looks like
Spmatrix=as.matrix(SpDB)
Spmatrix[Spmatrix == -Inf] <- 0 #get rid of all the -inf values bc sparse matrix is screwed up
#now original
Omatrix=as.matrix(Nv)

### write matrices(for entire deployment)
write.csv(Lrmatrix, "E:\\SOCAL\\SOCAL_RRPCA\\RRPCA_SOCAL\\matrices\\SOCAL_4_A\\Lrmatrix_SOCAL_A_4.csv",row.names=FALSE)
write.csv(Spmatrix, "E:\\SOCAL\\SOCAL_RRPCA\\RRPCA_SOCAL\\matrices\\SOCAL_4_A\\Spmatrix_SOCAL_A_4.csv",row.names=FALSE)
write.csv(Omatrix, "E:\\SOCAL\\SOCAL_RRPCA\\RRPCA_SOCAL\\matrices\\SOCAL_4_A\\Omatrix_SOCAL_A_4.csv",row.names=FALSE)

#saving time vector as csv
OMTT=nv[1]
Tmatrix=as.matrix(OMTT) #get the LR data (in decibels) into matrix form
write.csv(Tmatrix,"E:\\SOCAL\\SOCAL_RRPCA\\RRPCA_SOCAL\\matrices\\SOCAL_4_A\\OMTT.csv",row.names=FALSE)

##___________________________________________________________________________________________

#Deployment SOCAL_T_03

#RRPCA for entire matrix

setwd("E:\\SOCAL\\SOCAL_RRPCA\\Soundscape_Metrics_SOCAL\\SOCAL_T_03")
SOCALdata=as.data.frame(fread('SOCAL_T_03_LTSA_1s_1Hz_PSD_20min.csv'))
nv=SOCALdata #change to megan's language- and delete row of infinity values

#save as data frame to get values numeric, change column names to just frequency values, convert from dv to pressure
as.data.frame(colnames(nv))
hix  = as.numeric( gsub("PSD_","",names(nv)[4:41]) ) #gives freq range for plotting--ELLA changed from 25 to 17 b/c it was introducing NAs before. and didn't run lines 160-164... do i need those or is that only for truncating step????
Nv   = as.data.frame(as.matrix( ( nv[,4:41]) )) 
NvP  = as.data.frame(10^(Nv/20))     #converting from db to pressure-- "un-dbing"

#Plot original data
colnames(Nv) = hix #thist makes the column names numeric rather than TOL_123
orgdata <- reshape :: melt(t(Nv)  ) #melt gets the data into a columns that ggplot recognizes 
pO = ggplot(orgdata, aes(X1, value, group = as.factor(X2)))+ 
  geom_line(alpha=.05)+ scale_x_continuous()+ 
  ggtitle("Original Data")+ xlab("")+ theme_minimal() 
#skipping ploting step but keeping this here 

#RRPCA method for source separation
#-----------------------------------------------------------------------------------------

#setup input and run rrpca
input = NvP # or Nv (dB units)
nvpcaTOL = rrpca(input) #PREDICTED RANK IS OFTEN ZERO... is that BAD? lamda was screwing this up. 
#NOTE: pressure units avoid a negative change in ambient when transients removed

#LOW RANK RESULTS
Lr = as.data.frame(nvpcaTOL$L) # all negative values. WAIT L is ALL THE SAME VALUE, but S is diff values.... now megan and i fixed this.
LrDB = 10*log10( Lr^2 ) #these values are all very close
colnames(LrDB) = hix #assign hix to column names so that it registers as numeric
NvMlr <- reshape :: melt(t(LrDB)  )
pL = ggplot(NvMlr, aes(X1, value, group = as.factor(X2)))+ 
  geom_line(alpha=.05)+ scale_x_continuous()+ 
  ggtitle("Low rank")+ xlab("")+ theme_minimal() #changed alpha from 0.05 to 0.2
#this should result in a plot that gets rid of some of the wiggle. 

#SPARSE RESULTS
Sp = as.data.frame(nvpcaTOL$S) # all positive values... THERE ARE NEG VALUES HERE WHICH IS ODD
SpDB = 10*log10( Sp^2 ) #something is off here.... inf values???
colnames(Sp) = hix 
NvMSp <- reshape :: melt(t(Sp)  )
pS = ggplot(NvMSp, aes(X1, value, group = as.factor(X2)))+ 
  geom_line(alpha=.3 )+ scale_x_continuous()+ 
  ggtitle("Sparse")+ xlab("")+ theme_minimal() #changed alpha from .05 to .5 to be able to see these outliers more

#save matrices
Lrmatrix=as.matrix(LrDB) #get the LR data (in decibels) into matrix form
#now lets just see what image of sparse looks like
Spmatrix=as.matrix(SpDB)
Spmatrix[Spmatrix == -Inf] <- 0 #get rid of all the -inf values bc sparse matrix is screwed up
#now original
Omatrix=as.matrix(Nv)

### write matrices(for entire deployment)
write.csv(Lrmatrix, "E:\\SOCAL\\SOCAL_RRPCA\\RRPCA_SOCAL\\matrices\\SOCAL_T_03\\Lrmatrix_SOCAL_T_03.csv",row.names=FALSE)
write.csv(Spmatrix, "E:\\SOCAL\\SOCAL_RRPCA\\RRPCA_SOCAL\\matrices\\SOCAL_T_03\\Spmatrix_SOCAL_T_03.csv",row.names=FALSE)
write.csv(Omatrix, "E:\\SOCAL\\SOCAL_RRPCA\\RRPCA_SOCAL\\matrices\\SOCAL_T_03\\Omatrix_SOCAL_T_03.csv",row.names=FALSE)

#saving time vector as csv
OMTT=nv[1]
Tmatrix=as.matrix(OMTT) #get the LR data (in decibels) into matrix form
write.csv(Tmatrix,"E:\\SOCAL\\SOCAL_RRPCA\\RRPCA_SOCAL\\matrices\\SOCAL_T_03\\OMTT.csv",row.names=FALSE)

##___________________________________________________________________________________________

#Deployment SOCAL_15_A

#RRPCA for entire matrix

setwd("E:\\SOCAL\\SOCAL_RRPCA\\Soundscape_Metrics_SOCAL\\SOCAL_15_A")
SOCALdata=as.data.frame(fread('SOCAL_15_A_LTSA_1s_1Hz_PSD_20min.csv'))
nv=SOCALdata #change to megan's language- and delete row of infinity values

#save as data frame to get values numeric, change column names to just frequency values, convert from dv to pressure
as.data.frame(colnames(nv))
hix  = as.numeric( gsub("PSD_","",names(nv)[4:41]) ) #gives freq range for plotting--ELLA changed from 25 to 17 b/c it was introducing NAs before. and didn't run lines 160-164... do i need those or is that only for truncating step????
Nv   = as.data.frame(as.matrix( ( nv[,4:41]) )) 
NvP  = as.data.frame(10^(Nv/20))     #converting from db to pressure-- "un-dbing"

#Plot original data
colnames(Nv) = hix #thist makes the column names numeric rather than TOL_123
orgdata <- reshape :: melt(t(Nv)  ) #melt gets the data into a columns that ggplot recognizes 
pO = ggplot(orgdata, aes(X1, value, group = as.factor(X2)))+ 
  geom_line(alpha=.05)+ scale_x_continuous()+ 
  ggtitle("Original Data")+ xlab("")+ theme_minimal() 
#skipping ploting step but keeping this here 

#RRPCA method for source separation
#-----------------------------------------------------------------------------------------

#setup input and run rrpca
input = NvP # or Nv (dB units)
nvpcaTOL = rrpca(input) #PREDICTED RANK IS OFTEN ZERO... is that BAD? lamda was screwing this up. 
#NOTE: pressure units avoid a negative change in ambient when transients removed

#LOW RANK RESULTS
Lr = as.data.frame(nvpcaTOL$L) # all negative values. WAIT L is ALL THE SAME VALUE, but S is diff values.... now megan and i fixed this.
LrDB = 10*log10( Lr^2 ) #these values are all very close
colnames(LrDB) = hix #assign hix to column names so that it registers as numeric
NvMlr <- reshape :: melt(t(LrDB)  )
pL = ggplot(NvMlr, aes(X1, value, group = as.factor(X2)))+ 
  geom_line(alpha=.05)+ scale_x_continuous()+ 
  ggtitle("Low rank")+ xlab("")+ theme_minimal() #changed alpha from 0.05 to 0.2
#this should result in a plot that gets rid of some of the wiggle. 

#SPARSE RESULTS
Sp = as.data.frame(nvpcaTOL$S) # all positive values... THERE ARE NEG VALUES HERE WHICH IS ODD
SpDB = 10*log10( Sp^2 ) #something is off here.... inf values???
colnames(Sp) = hix 
NvMSp <- reshape :: melt(t(Sp)  )
pS = ggplot(NvMSp, aes(X1, value, group = as.factor(X2)))+ 
  geom_line(alpha=.3 )+ scale_x_continuous()+ 
  ggtitle("Sparse")+ xlab("")+ theme_minimal() #changed alpha from .05 to .5 to be able to see these outliers more

#save matrices
Lrmatrix=as.matrix(LrDB) #get the LR data (in decibels) into matrix form
#now lets just see what image of sparse looks like
Spmatrix=as.matrix(SpDB)
Spmatrix[Spmatrix == -Inf] <- 0 #get rid of all the -inf values bc sparse matrix is screwed up
#now original
Omatrix=as.matrix(Nv)

### write matrices(for entire deployment)
write.csv(Lrmatrix, "E:\\SOCAL\\SOCAL_RRPCA\\RRPCA_SOCAL\\matrices\\SOCAL_15_A\\Lrmatrix_SOCAL_15_A.csv",row.names=FALSE)
write.csv(Spmatrix, "E:\\SOCAL\\SOCAL_RRPCA\\RRPCA_SOCAL\\matrices\\SOCAL_15_A\\Spmatrix_SOCAL_15_A.csv",row.names=FALSE)
write.csv(Omatrix, "E:\\SOCAL\\SOCAL_RRPCA\\RRPCA_SOCAL\\matrices\\SOCAL_15_A\\Omatrix_SOCAL_15_A.csv",row.names=FALSE)

#saving time vector as csv
OMTT=nv[1]
Tmatrix=as.matrix(OMTT) #get the LR data (in decibels) into matrix form
write.csv(Tmatrix,"E:\\SOCAL\\SOCAL_RRPCA\\RRPCA_SOCAL\\matrices\\SOCAL_15_A\\OMTT.csv",row.names=FALSE)


#######__________________#####

#Deployment MB02_02

#RRPCA for entire matrix

setwd("E:\\SDT\\SDT RRPCA\\RRPCA SDT\\z_applying to MB02_02\\take3")
MBdata=as.data.frame(fread('MB02_02_1s_1Hz_LTSA_PSD_20min.csv'))
nv=MBdata #change to megan's language- and delete row of infinity values

#save as data frame to get values numeric, change column names to just frequency values, convert from dv to pressure
as.data.frame(colnames(nv))
hix  = as.numeric( gsub("PSD_","",names(nv)[4:41]) ) #gives freq range for plotting--ELLA changed from 25 to 17 b/c it was introducing NAs before. and didn't run lines 160-164... do i need those or is that only for truncating step????
Nv   = as.data.frame(as.matrix( ( nv[,4:41]) )) 
NvP  = as.data.frame(10^(Nv/20))     #converting from db to pressure-- "un-dbing"

#Plot original data
colnames(Nv) = hix #thist makes the column names numeric rather than TOL_123
orgdata <- reshape :: melt(t(Nv)  ) #melt gets the data into a columns that ggplot recognizes 
pO = ggplot(orgdata, aes(X1, value, group = as.factor(X2)))+ 
  geom_line(alpha=.05)+ scale_x_continuous()+ 
  ggtitle("Original Data")+ xlab("")+ theme_minimal() 
#skipping ploting step but keeping this here 

#RRPCA method for source separation
#-----------------------------------------------------------------------------------------

#setup input and run rrpca
input = NvP # or Nv (dB units)
nvpcaTOL = rrpca(input) #PREDICTED RANK IS OFTEN ZERO... is that BAD? lamda was screwing this up. 
#NOTE: pressure units avoid a negative change in ambient when transients removed

#LOW RANK RESULTS
Lr = as.data.frame(nvpcaTOL$L) # all negative values. WAIT L is ALL THE SAME VALUE, but S is diff values.... now megan and i fixed this.
LrDB = 10*log10( Lr^2 ) #these values are all very close
colnames(LrDB) = hix #assign hix to column names so that it registers as numeric
NvMlr <- reshape :: melt(t(LrDB)  )
pL = ggplot(NvMlr, aes(X1, value, group = as.factor(X2)))+ 
  geom_line(alpha=.05)+ scale_x_continuous()+ 
  ggtitle("Low rank")+ xlab("")+ theme_minimal() #changed alpha from 0.05 to 0.2
#this should result in a plot that gets rid of some of the wiggle. 

#SPARSE RESULTS
Sp = as.data.frame(nvpcaTOL$S) # all positive values... THERE ARE NEG VALUES HERE WHICH IS ODD
SpDB = 10*log10( Sp^2 ) #something is off here.... inf values???
colnames(Sp) = hix 
NvMSp <- reshape :: melt(t(Sp)  )
pS = ggplot(NvMSp, aes(X1, value, group = as.factor(X2)))+ 
  geom_line(alpha=.3 )+ scale_x_continuous()+ 
  ggtitle("Sparse")+ xlab("")+ theme_minimal() #changed alpha from .05 to .5 to be able to see these outliers more

#save matrices
Lrmatrix=as.matrix(LrDB) #get the LR data (in decibels) into matrix form
#now lets just see what image of sparse looks like
Spmatrix=as.matrix(SpDB)
Spmatrix[Spmatrix == -Inf] <- 0 #get rid of all the -inf values bc sparse matrix is screwed up
#now original
Omatrix=as.matrix(Nv)

### write matrices(for entire deployment)
write.csv(Lrmatrix, "E:\\SDT\\SDT RRPCA\\RRPCA SDT\\z_applying to MB02_02\\take3\\Lrmatrix_MB02_02.csv",row.names=FALSE)
write.csv(Spmatrix,  "E:\\SDT\\SDT RRPCA\\RRPCA SDT\\z_applying to MB02_02\\take3\\Spmatrix_MB02_02.csv",row.names=FALSE)
write.csv(Omatrix,  "E:\\SDT\\SDT RRPCA\\RRPCA SDT\\z_applying to MB02_02\\take3\\Omatrix_MB02_02.csv",row.names=FALSE)

#saving time vector as csv
OMTT=nv[1]
Tmatrix=as.matrix(OMTT) #get the LR data (in decibels) into matrix form
write.csv(Tmatrix,"E:\\SDT\\SDT RRPCA\\RRPCA SDT\\z_applying to MB02_02\\take3\\OMTT.csv",row.names=FALSE)



#######__________________#####

#SOCAL P_40

#RRPCA for entire matrix

setwd("E:\\SOCAL\\SOCAL_RRPCA\\Soundscape_Metrics_SOCAL\\SOCAL_P_40")
MBdata=as.data.frame(fread('SOCAL_P_40_1s_1Hz_LTSA_PSD_20min.csv'))
nv=MBdata #change to megan's language

#save as data frame to get values numeric, change column names to just frequency values, convert from db to pressure
as.data.frame(colnames(nv))
hix  = as.numeric( gsub("PSD_","",names(nv)[4:41]) ) #4-41 = 60-800 Hz
Nv   = as.data.frame(as.matrix( ( nv[,4:41]) )) 
NvP  = as.data.frame(10^(Nv/20))     #converting from db to pressure-- "un-dbing"

#Plot original data
colnames(Nv) = hix #thist makes the column names numeric rather than TOL_123
orgdata <- reshape :: melt(t(Nv)  ) #melt gets the data into a columns that ggplot recognizes 
pO = ggplot(orgdata, aes(X1, value, group = as.factor(X2)))+ 
  geom_line(alpha=.05)+ scale_x_continuous()+ 
  ggtitle("Original Data")+ xlab("")+ theme_minimal() 
#skipping ploting step but keeping this here 

#RRPCA method for source separation
#-----------------------------------------------------------------------------------------

#setup input and run rrpca
input = NvP # or Nv (dB units)
nvpcaTOL = rrpca(input) #PREDICTED RANK IS OFTEN ZERO... is that BAD? lamda was screwing this up. 
#NOTE: pressure units avoid a negative change in ambient when transients removed

#LOW RANK RESULTS
Lr = as.data.frame(nvpcaTOL$L) # all negative values. WAIT L is ALL THE SAME VALUE, but S is diff values.... now megan and i fixed this.
LrDB = 10*log10( Lr^2 ) #these values are all very close
colnames(LrDB) = hix #assign hix to column names so that it registers as numeric
NvMlr <- reshape :: melt(t(LrDB)  )
pL = ggplot(NvMlr, aes(X1, value, group = as.factor(X2)))+ 
  geom_line(alpha=.05)+ scale_x_continuous()+ 
  ggtitle("Low rank")+ xlab("")+ theme_minimal() #changed alpha from 0.05 to 0.2
#this should result in a plot that gets rid of some of the wiggle. 

#SPARSE RESULTS
Sp = as.data.frame(nvpcaTOL$S) # all positive values... THERE ARE NEG VALUES HERE WHICH IS ODD
SpDB = 10*log10( Sp^2 ) #something is off here.... inf values???
colnames(Sp) = hix 
NvMSp <- reshape :: melt(t(Sp)  )
pS = ggplot(NvMSp, aes(X1, value, group = as.factor(X2)))+ 
  geom_line(alpha=.3 )+ scale_x_continuous()+ 
  ggtitle("Sparse")+ xlab("")+ theme_minimal() #changed alpha from .05 to .5 to be able to see these outliers more

#save matrices
Lrmatrix=as.matrix(LrDB) #get the LR data (in decibels) into matrix form
#now lets just see what image of sparse looks like
Spmatrix=as.matrix(SpDB)
Spmatrix[Spmatrix == -Inf] <- 0 #get rid of all the -inf values bc sparse matrix is screwed up
#now original
Omatrix=as.matrix(Nv)

### write matrices(for entire deployment)
write.csv(Lrmatrix, "E:\\SOCAL\\SOCAL_RRPCA\\RRPCA_SOCAL\\matrices\\SOCAL_P_40\\Lrmatrix_SOCAL_P_40.csv",row.names=FALSE)
write.csv(Spmatrix,  "E:\\SOCAL\\SOCAL_RRPCA\\RRPCA_SOCAL\\matrices\\SOCAL_P_40\\SPmatrix_SOCAL_P_40.csv",row.names=FALSE)
write.csv(Omatrix,  "E:\\SOCAL\\SOCAL_RRPCA\\RRPCA_SOCAL\\matrices\\SOCAL_P_40\\Omatrix_SOCAL_P_40.csv",row.names=FALSE)

#saving time vector as csv
OMTT=nv[1]
Tmatrix=as.matrix(OMTT) #get the LR data (in decibels) into matrix form
write.csv(Tmatrix,"E:\\SOCAL\\SOCAL_RRPCA\\RRPCA_SOCAL\\matrices\\SOCAL_P_40\\OMTT.csv",row.names=FALSE)



#######__________________#####

#SOCAL 18_G

#RRPCA for entire matrix

setwd("E:\\SOCAL\\SOCAL_RRPCA\\Soundscape_Metrics_SOCAL\\SOCAL_18_G")
SOCALdata=as.data.frame(fread('SOCAL_18G_1s_1Hz_LTSA_PSD_20min.csv'))
nv=SOCALdata #change to megan's language

#save as data frame to get values numeric, change column names to just frequency values, convert from db to pressure
as.data.frame(colnames(nv))
hix  = as.numeric( gsub("PSD_","",names(nv)[4:41]) ) #4-41 = 60-800 Hz
Nv   = as.data.frame(as.matrix( ( nv[,4:41]) )) 
NvP  = as.data.frame(10^(Nv/20))     #converting from db to pressure-- "un-dbing"

#Plot original data
colnames(Nv) = hix #thist makes the column names numeric rather than TOL_123
orgdata <- reshape :: melt(t(Nv)  ) #melt gets the data into a columns that ggplot recognizes 
pO = ggplot(orgdata, aes(X1, value, group = as.factor(X2)))+ 
  geom_line(alpha=.05)+ scale_x_continuous()+ 
  ggtitle("Original Data")+ xlab("")+ theme_minimal() 
#skipping ploting step but keeping this here 

#RRPCA method for source separation
#-----------------------------------------------------------------------------------------

#setup input and run rrpca
input = NvP # or Nv (dB units)
nvpcaTOL = rrpca(input) #PREDICTED RANK IS OFTEN ZERO... is that BAD? lamda was screwing this up. 
#NOTE: pressure units avoid a negative change in ambient when transients removed

#LOW RANK RESULTS
Lr = as.data.frame(nvpcaTOL$L) # all negative values. WAIT L is ALL THE SAME VALUE, but S is diff values.... now megan and i fixed this.
LrDB = 10*log10( Lr^2 ) #these values are all very close
colnames(LrDB) = hix #assign hix to column names so that it registers as numeric
NvMlr <- reshape :: melt(t(LrDB)  )
pL = ggplot(NvMlr, aes(X1, value, group = as.factor(X2)))+ 
  geom_line(alpha=.05)+ scale_x_continuous()+ 
  ggtitle("Low rank")+ xlab("")+ theme_minimal() #changed alpha from 0.05 to 0.2
#this should result in a plot that gets rid of some of the wiggle. 

#SPARSE RESULTS
Sp = as.data.frame(nvpcaTOL$S) # all positive values... THERE ARE NEG VALUES HERE WHICH IS ODD
SpDB = 10*log10( Sp^2 ) #something is off here.... inf values???
colnames(Sp) = hix 
NvMSp <- reshape :: melt(t(Sp)  )
pS = ggplot(NvMSp, aes(X1, value, group = as.factor(X2)))+ 
  geom_line(alpha=.3 )+ scale_x_continuous()+ 
  ggtitle("Sparse")+ xlab("")+ theme_minimal() #changed alpha from .05 to .5 to be able to see these outliers more

#save matrices
Lrmatrix=as.matrix(LrDB) #get the LR data (in decibels) into matrix form
#now lets just see what image of sparse looks like
Spmatrix=as.matrix(SpDB)
Spmatrix[Spmatrix == -Inf] <- 0 #get rid of all the -inf values bc sparse matrix is screwed up
#now original
Omatrix=as.matrix(Nv)

### write matrices(for entire deployment)
write.csv(Lrmatrix, "E:\\SOCAL\\SOCAL_RRPCA\\Soundscape_Metrics_SOCAL\\SOCAL_18_G\\Lrmatrix_SOCAL18_G.csv",row.names=FALSE)
write.csv(Spmatrix,  "E:\\SOCAL\\SOCAL_RRPCA\\Soundscape_Metrics_SOCAL\\SOCAL_18_G\\Spmatrix_SOCAL18_G.csv",row.names=FALSE)
write.csv(Omatrix,  "E:\\SOCAL\\SOCAL_RRPCA\\Soundscape_Metrics_SOCAL\\SOCAL_18_G\\Omatrix_SOCAL18_G.csv",row.names=FALSE)

#saving time vector as csv
OMTT=nv[1]
Tmatrix=as.matrix(OMTT) #get the LR data (in decibels) into matrix form
write.csv(Tmatrix,"E:\\SOCAL\\SOCAL_RRPCA\\RRPCA_SOCAL\\matrices\\SOCAL_18_G\\OMTT.csv",row.names=FALSE)



#######__________________#####

#SOCAL T01_B
#RRPCA for entire matrix

setwd("E:\\SOCAL\\SOCAL_RRPCA\\Soundscape_Metrics_SOCAL\\SOCAL_T_01_B")
SOCALdata=as.data.frame(fread('SOCAL_T_01_B_PSD_20min.csv'))
nv=SOCALdata #change to megan's language

#save as data frame to get values numeric, change column names to just frequency values, convert from db to pressure
as.data.frame(colnames(nv))
hix  = as.numeric( gsub("PSD_","",names(nv)[4:41]) ) #4-41 = 60-800 Hz
Nv   = as.data.frame(as.matrix( ( nv[,4:41]) )) 
NvP  = as.data.frame(10^(Nv/20))     #converting from db to pressure-- "un-dbing"

#Plot original data
colnames(Nv) = hix #thist makes the column names numeric rather than TOL_123
orgdata <- reshape :: melt(t(Nv)  ) #melt gets the data into a columns that ggplot recognizes 
pO = ggplot(orgdata, aes(X1, value, group = as.factor(X2)))+ 
  geom_line(alpha=.05)+ scale_x_continuous()+ 
  ggtitle("Original Data")+ xlab("")+ theme_minimal() 
#skipping ploting step but keeping this here 

#RRPCA method for source separation
#-----------------------------------------------------------------------------------------

#setup input and run rrpca
input = NvP # or Nv (dB units)
nvpcaTOL = rrpca(input) #PREDICTED RANK IS OFTEN ZERO... is that BAD? lamda was screwing this up. 
#NOTE: pressure units avoid a negative change in ambient when transients removed

#LOW RANK RESULTS
Lr = as.data.frame(nvpcaTOL$L) # all negative values. WAIT L is ALL THE SAME VALUE, but S is diff values.... now megan and i fixed this.
LrDB = 10*log10( Lr^2 ) #these values are all very close
colnames(LrDB) = hix #assign hix to column names so that it registers as numeric
NvMlr <- reshape :: melt(t(LrDB)  )
pL = ggplot(NvMlr, aes(X1, value, group = as.factor(X2)))+ 
  geom_line(alpha=.05)+ scale_x_continuous()+ 
  ggtitle("Low rank")+ xlab("")+ theme_minimal() #changed alpha from 0.05 to 0.2
#this should result in a plot that gets rid of some of the wiggle. 

#SPARSE RESULTS
Sp = as.data.frame(nvpcaTOL$S) # all positive values... THERE ARE NEG VALUES HERE WHICH IS ODD
SpDB = 10*log10( Sp^2 ) #something is off here.... inf values???
colnames(Sp) = hix 
NvMSp <- reshape :: melt(t(Sp)  )
pS = ggplot(NvMSp, aes(X1, value, group = as.factor(X2)))+ 
  geom_line(alpha=.3 )+ scale_x_continuous()+ 
  ggtitle("Sparse")+ xlab("")+ theme_minimal() #changed alpha from .05 to .5 to be able to see these outliers more

#save matrices
Lrmatrix=as.matrix(LrDB) #get the LR data (in decibels) into matrix form
#now lets just see what image of sparse looks like
Spmatrix=as.matrix(SpDB)
Spmatrix[Spmatrix == -Inf] <- 0 #get rid of all the -inf values bc sparse matrix is screwed up
#now original
Omatrix=as.matrix(Nv)

### write matrices(for entire deployment)
write.csv(Lrmatrix, "E:\\SOCAL\\SOCAL_RRPCA\\RRPCA_SOCAL\\matrices\\SOCAL_T_01_B\\Lrmatrix_SOCALT_01_B.csv",row.names=FALSE)
write.csv(Spmatrix,  "E:\\SOCAL\\SOCAL_RRPCA\\RRPCA_SOCAL\\matrices\\SOCAL_T_01_B\\Spmatrix_SOCALT_01_B.csv",row.names=FALSE)
write.csv(Omatrix,  "E:\\SOCAL\\SOCAL_RRPCA\\RRPCA_SOCAL\\matrices\\SOCAL_T_01_B\\Omatrix_SOCALT_01_B.csv",row.names=FALSE)

#saving time vector as csv
OMTT=nv[1]
Tmatrix=as.matrix(OMTT) #get the LR data (in decibels) into matrix form
write.csv(Tmatrix,"E:\\SOCAL\\SOCAL_RRPCA\\RRPCA_SOCAL\\matrices\\SOCAL_T_01_B\\OMTT.csv",row.names=FALSE)




#######__________________#####

#SOCAL T0_02
#RRPCA for entire matrix

setwd("E:\\SOCAL\\SOCAL_RRPCA\\Soundscape_Metrics_SOCAL\\SOCAL_T_02")
SOCALdata=as.data.frame(fread('SOCAL_T_02_1s_1Hz_LTSA_PSD_20min.csv'))
nv=SOCALdata #change to megan's language

#save as data frame to get values numeric, change column names to just frequency values, convert from db to pressure
as.data.frame(colnames(nv))
hix  = as.numeric( gsub("PSD_","",names(nv)[4:41]) ) #4-41 = 60-800 Hz
Nv   = as.data.frame(as.matrix( ( nv[,4:41]) )) 
NvP  = as.data.frame(10^(Nv/20))     #converting from db to pressure-- "un-dbing"

#Plot original data
colnames(Nv) = hix #thist makes the column names numeric rather than TOL_123
orgdata <- reshape :: melt(t(Nv)  ) #melt gets the data into a columns that ggplot recognizes 
pO = ggplot(orgdata, aes(X1, value, group = as.factor(X2)))+ 
  geom_line(alpha=.05)+ scale_x_continuous()+ 
  ggtitle("Original Data")+ xlab("")+ theme_minimal() 
#skipping ploting step but keeping this here 

#RRPCA method for source separation
#-----------------------------------------------------------------------------------------

#setup input and run rrpca
input = NvP # or Nv (dB units)
nvpcaTOL = rrpca(input) #PREDICTED RANK IS OFTEN ZERO... is that BAD? lamda was screwing this up. 
#NOTE: pressure units avoid a negative change in ambient when transients removed

#LOW RANK RESULTS
Lr = as.data.frame(nvpcaTOL$L) # all negative values. WAIT L is ALL THE SAME VALUE, but S is diff values.... now megan and i fixed this.
LrDB = 10*log10( Lr^2 ) #these values are all very close
colnames(LrDB) = hix #assign hix to column names so that it registers as numeric
NvMlr <- reshape :: melt(t(LrDB)  )
pL = ggplot(NvMlr, aes(X1, value, group = as.factor(X2)))+ 
  geom_line(alpha=.05)+ scale_x_continuous()+ 
  ggtitle("Low rank")+ xlab("")+ theme_minimal() #changed alpha from 0.05 to 0.2
#this should result in a plot that gets rid of some of the wiggle. 

#SPARSE RESULTS
Sp = as.data.frame(nvpcaTOL$S) # all positive values... THERE ARE NEG VALUES HERE WHICH IS ODD
SpDB = 10*log10( Sp^2 ) #something is off here.... inf values???
colnames(Sp) = hix 
NvMSp <- reshape :: melt(t(Sp)  )
pS = ggplot(NvMSp, aes(X1, value, group = as.factor(X2)))+ 
  geom_line(alpha=.3 )+ scale_x_continuous()+ 
  ggtitle("Sparse")+ xlab("")+ theme_minimal() #changed alpha from .05 to .5 to be able to see these outliers more

#save matrices
Lrmatrix=as.matrix(LrDB) #get the LR data (in decibels) into matrix form
#now lets just see what image of sparse looks like
Spmatrix=as.matrix(SpDB)
Spmatrix[Spmatrix == -Inf] <- 0 #get rid of all the -inf values bc sparse matrix is screwed up
#now original
Omatrix=as.matrix(Nv)

### write matrices(for entire deployment)
write.csv(Lrmatrix, "E:\\SOCAL\\SOCAL_RRPCA\\Soundscape_Metrics_SOCAL\\SOCAL_T_02\\Lrmatrix_SOCALT_02.csv",row.names=FALSE)
write.csv(Spmatrix,  "E:\\SOCAL\\SOCAL_RRPCA\\Soundscape_Metrics_SOCAL\\SOCAL_T_02\\Spmatrix_SOCALT_02.csv",row.names=FALSE)
write.csv(Omatrix,  "E:\\SOCAL\\SOCAL_RRPCA\\Soundscape_Metrics_SOCAL\\SOCAL_T_02\\Omatrix_SOCALT_02.csv",row.names=FALSE)

#saving time vector as csv
OMTT=nv[1]
Tmatrix=as.matrix(OMTT) #get the LR data (in decibels) into matrix form
write.csv(Tmatrix,"E:\\SOCAL\\SOCAL_RRPCA\\Soundscape_Metrics_SOCAL\\SOCAL_T_02\\OMTT.csv",row.names=FALSE)


#######__________________#####

#SOCAL 05_A
#RRPCA for entire matrix

setwd("E:\\SOCAL\\SOCAL_RRPCA\\Soundscape_Metrics_SOCAL\\SOCAL_05_A")
SOCALdata=as.data.frame(fread('SOCAL_5A_1s_1Hz_LTSA_PSD_20min.csv'))
nv=SOCALdata #change to megan's language

#save as data frame to get values numeric, change column names to just frequency values, convert from db to pressure
as.data.frame(colnames(nv))
hix  = as.numeric( gsub("PSD_","",names(nv)[4:41]) ) #4-41 = 60-800 Hz
Nv   = as.data.frame(as.matrix( ( nv[,4:41]) )) 
NvP  = as.data.frame(10^(Nv/20))     #converting from db to pressure-- "un-dbing"

#Plot original data
colnames(Nv) = hix #thist makes the column names numeric rather than TOL_123
orgdata <- reshape :: melt(t(Nv)  ) #melt gets the data into a columns that ggplot recognizes 
pO = ggplot(orgdata, aes(X1, value, group = as.factor(X2)))+ 
  geom_line(alpha=.05)+ scale_x_continuous()+ 
  ggtitle("Original Data")+ xlab("")+ theme_minimal() 
#skipping ploting step but keeping this here 

#RRPCA method for source separation
#-----------------------------------------------------------------------------------------

#setup input and run rrpca
input = NvP # or Nv (dB units)
nvpcaTOL = rrpca(input) #PREDICTED RANK IS OFTEN ZERO... is that BAD? lamda was screwing this up. 
#NOTE: pressure units avoid a negative change in ambient when transients removed

#LOW RANK RESULTS
Lr = as.data.frame(nvpcaTOL$L) # all negative values. WAIT L is ALL THE SAME VALUE, but S is diff values.... now megan and i fixed this.
LrDB = 10*log10( Lr^2 ) #these values are all very close
colnames(LrDB) = hix #assign hix to column names so that it registers as numeric
NvMlr <- reshape :: melt(t(LrDB)  )
pL = ggplot(NvMlr, aes(X1, value, group = as.factor(X2)))+ 
  geom_line(alpha=.05)+ scale_x_continuous()+ 
  ggtitle("Low rank")+ xlab("")+ theme_minimal() #changed alpha from 0.05 to 0.2
#this should result in a plot that gets rid of some of the wiggle. 

#SPARSE RESULTS
Sp = as.data.frame(nvpcaTOL$S) # all positive values... THERE ARE NEG VALUES HERE WHICH IS ODD
SpDB = 10*log10( Sp^2 ) #something is off here.... inf values???
colnames(Sp) = hix 
NvMSp <- reshape :: melt(t(Sp)  )
pS = ggplot(NvMSp, aes(X1, value, group = as.factor(X2)))+ 
  geom_line(alpha=.3 )+ scale_x_continuous()+ 
  ggtitle("Sparse")+ xlab("")+ theme_minimal() #changed alpha from .05 to .5 to be able to see these outliers more

#save matrices
Lrmatrix=as.matrix(LrDB) #get the LR data (in decibels) into matrix form
#now lets just see what image of sparse looks like
Spmatrix=as.matrix(SpDB)
Spmatrix[Spmatrix == -Inf] <- 0 #get rid of all the -inf values bc sparse matrix is screwed up
#now original
Omatrix=as.matrix(Nv)

### write matrices(for entire deployment)
write.csv(Lrmatrix, "E:\\SOCAL\\SOCAL_RRPCA\\RRPCA_SOCAL\\matrices\\SOCAL_5_A\\Lrmatrix_SOCAL5_A.csv",row.names=FALSE)
write.csv(Spmatrix,  "E:\\SOCAL\\SOCAL_RRPCA\\RRPCA_SOCAL\\matrices\\SOCAL_5_A\\Spmatrix_SOCAL5_A.csv",row.names=FALSE)
write.csv(Omatrix,  "E:\\SOCAL\\SOCAL_RRPCA\\RRPCA_SOCAL\\matrices\\SOCAL_5_A\\Omatrix_SOCAL5_A.csv",row.names=FALSE)

#saving time vector as csv
OMTT=nv[1]
Tmatrix=as.matrix(OMTT) #get the LR data (in decibels) into matrix form
write.csv(Tmatrix,"E:\\SOCAL\\SOCAL_RRPCA\\RRPCA_SOCAL\\matrices\\SOCAL_5_A\\OMTT.csv",row.names=FALSE)




#######__________________#####

#SOCAL 21_P
#RRPCA for entire matrix

setwd("E:\\SOCAL\\SOCAL_RRPCA\\Soundscape_Metrics_SOCAL\\LJ_21_P")
SOCALdata=as.data.frame(fread('SOCAL_LJ_21_P_1s_1Hz_LTSA_PSD_20min.csv'))
nv=SOCALdata #change to megan's language

#save as data frame to get values numeric, change column names to just frequency values, convert from db to pressure
as.data.frame(colnames(nv))
hix  = as.numeric( gsub("PSD_","",names(nv)[4:41]) ) #4-41 = 60-800 Hz
Nv   = as.data.frame(as.matrix( ( nv[,4:41]) )) 
NvP  = as.data.frame(10^(Nv/20))     #converting from db to pressure-- "un-dbing"

#Plot original data
colnames(Nv) = hix #thist makes the column names numeric rather than TOL_123
orgdata <- reshape :: melt(t(Nv)  ) #melt gets the data into a columns that ggplot recognizes 
pO = ggplot(orgdata, aes(X1, value, group = as.factor(X2)))+ 
  geom_line(alpha=.05)+ scale_x_continuous()+ 
  ggtitle("Original Data")+ xlab("")+ theme_minimal() 
#skipping ploting step but keeping this here 

#RRPCA method for source separation
#-----------------------------------------------------------------------------------------

#setup input and run rrpca
input = NvP # or Nv (dB units)
nvpcaTOL = rrpca(input) #PREDICTED RANK IS OFTEN ZERO... is that BAD? lamda was screwing this up. 
#NOTE: pressure units avoid a negative change in ambient when transients removed

#LOW RANK RESULTS
Lr = as.data.frame(nvpcaTOL$L) # all negative values. WAIT L is ALL THE SAME VALUE, but S is diff values.... now megan and i fixed this.
LrDB = 10*log10( Lr^2 ) #these values are all very close
colnames(LrDB) = hix #assign hix to column names so that it registers as numeric
NvMlr <- reshape :: melt(t(LrDB)  )
pL = ggplot(NvMlr, aes(X1, value, group = as.factor(X2)))+ 
  geom_line(alpha=.05)+ scale_x_continuous()+ 
  ggtitle("Low rank")+ xlab("")+ theme_minimal() #changed alpha from 0.05 to 0.2
#this should result in a plot that gets rid of some of the wiggle. 

#SPARSE RESULTS
Sp = as.data.frame(nvpcaTOL$S) # all positive values... THERE ARE NEG VALUES HERE WHICH IS ODD
SpDB = 10*log10( Sp^2 ) #something is off here.... inf values???
colnames(Sp) = hix 
NvMSp <- reshape :: melt(t(Sp)  )
pS = ggplot(NvMSp, aes(X1, value, group = as.factor(X2)))+ 
  geom_line(alpha=.3 )+ scale_x_continuous()+ 
  ggtitle("Sparse")+ xlab("")+ theme_minimal() #changed alpha from .05 to .5 to be able to see these outliers more

#save matrices
Lrmatrix=as.matrix(LrDB) #get the LR data (in decibels) into matrix form
#now lets just see what image of sparse looks like
Spmatrix=as.matrix(SpDB)
Spmatrix[Spmatrix == -Inf] <- 0 #get rid of all the -inf values bc sparse matrix is screwed up
#now original
Omatrix=as.matrix(Nv)

### write matrices(for entire deployment)
write.csv(Lrmatrix, "E:\\SOCAL\\SOCAL_RRPCA\\RRPCA_SOCAL\\matrices\\LJ_21_P\\Lrmatrix_LJ_21_P.csv",row.names=FALSE)
write.csv(Spmatrix,  "E:\\SOCAL\\SOCAL_RRPCA\\RRPCA_SOCAL\\matrices\\LJ_21_P\\Spmatrix_LJ_21_P.csv",row.names=FALSE)
write.csv(Omatrix,  "E:\\SOCAL\\SOCAL_RRPCA\\RRPCA_SOCAL\\matrices\\LJ_21_P\\Omatrix_LJ_21_p.csv",row.names=FALSE)

#saving time vector as csv
OMTT=nv[1]
Tmatrix=as.matrix(OMTT) #get the LR data (in decibels) into matrix form
write.csv(Tmatrix,"E:\\SOCAL\\SOCAL_RRPCA\\RRPCA_SOCAL\\matrices\\LJ_21_P\\OMTT.csv",row.names=FALSE)



