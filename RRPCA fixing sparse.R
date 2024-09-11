#RRPCA_fixing_sparse

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

#updated version fixed up on 7/28/2022 while making final figures
##############################################################
#############################################################
#SDT BF_01 PSD Median Values, 0:1000 Hz (from df 10 data), 20 Hz, 20 min binning
#20 Hz now because 100 Hz was wacky
#THIS IS THE GOOD ONE

#upload data
setwd("E:\\SDT\\SDT RRPCA\\Soundscape Metrics SDT\\SDT_BF_01_Soundscape Metrics\\SDT_BF_01_\\20Hz_20min_0-to-800Hz")
SDTdata=as.data.frame(fread('SDT_BF_01_LTSA_1s_1Hz_PSD_20min.csv'))

#separate out one week for visualizations
weekhr=SDTdata[1658:2161,1:42] #Row 1658-2161 (8/20-8/26)
nv=weekhr #change to megan's language

#save as data frame to get values numeric, change column names to just frequency values, convert from dv to pressure
as.data.frame(colnames(nv))
hix  = as.numeric( gsub("PSD_","",names(nv)[4:42]) ) #gives freq range for plotting--ELLA changed from 25 to 17 b/c it was introducing NAs before. and didn't run lines 160-164... do i need those or is that only for truncating step????
Nv   = as.data.frame(as.matrix( ( nv[,4:42]) )) 

#Plot original data
colnames(Nv) = hix #this makes the column names numeric rather than PSD_123
orgdata <- reshape :: melt(t(Nv)  ) #melt gets the data into a columns that ggplot recognizes 
pO = ggplot(orgdata, aes(X1, value, group = as.factor(X2)))+ 
  geom_line(alpha=.1)+ scale_x_continuous()+ 
  ggtitle("Original Data")+ xlab("")+ylab("Amplitude (dB)")+ylim(55,100)+theme_minimal() #changed from alpha=0.05 to .2 to make lines darker
#this plot is for the week

#RRPCA method for source separation
#-----------------------------------------------------------------------------------------

#setup input and run rrpca
input = Nv # or Nv (dB units)
nvpcaTOL = rrpca(input) #PREDICTED RANK IS OFTEN ZERO... is that BAD? lamda was screwing this up. 
#NOTE: pressure units avoid a negative change in ambient when transients removed

#LOW RANK RESULTS
Lr = as.data.frame(nvpcaTOL$L)
colnames(Lr) = hix #assign hix to column names so that it registers as numeric
NvMlr <- reshape :: melt(t(Lr)  )
pL = ggplot(NvMlr, aes(X1, value, group = as.factor(X2)))+ 
  geom_line(alpha=.1)+ scale_x_continuous()+ 
  ggtitle("Low rank")+ xlab("")+ylab("Amplitude (dB)")+ylim(55,100)+theme_minimal() #changed alpha from 0.05 to 0.2
#this should result in a plot that gets rid of some of the wiggle. 

#SPARSE RESULTS in dB
Sp = as.data.frame(nvpcaTOL$S) # 
#Sp[Sp < 1e-15] <- 0 #set neg values=0 to avoid rounding errors
colnames(Sp) = hix 
NvMSp <- reshape :: melt(t(Sp)  )
pS = ggplot(NvMSp, aes(X1, value, group = as.factor(X2)))+ 
  geom_line(alpha=.1)+
  ggtitle("Sparse")+ xlab("")+ylab("Amplitude (dB)")+scale_x_continuous()+theme_minimal() #changed alpha from .05 to .5 to be able to see these outliers more

#plot original, low rank, and sparse all together (as spectra)
grid.arrange(pO,pL,pS)


#plots as points instead of lines
pSpoint = ggplot(NvMSp, aes(X1, value, na.rm=TRUE, group = as.factor(X2)))+ 
  geom_point(alpha=.1)+ scale_x_continuous()+ 
  ggtitle("Sparse")+ xlab("")+ylim(55,100)+theme_minimal() #changed alpha from .05 to .5 to be able to see these outliers more
pOpoint=ggplot(orgdata, aes(X1, value, group = as.factor(X2)))+ 
  geom_point(alpha=.1)+ scale_x_continuous()+ 
  ggtitle("Original Data")+ xlab("")+ylim(55,100)+theme_minimal() 
pLrpoint=ggplot(NvMlr, aes(X1, value, group = as.factor(X2)))+ 
  geom_point(alpha=.1)+ scale_x_continuous()+ 
  ggtitle("Low rank")+ xlab("")+ylim(55,100)+theme_minimal() 
#plot original, low rank, and sparse all together
grid.arrange(pOpoint,pLrpoint,pSpoint)


#figuring out wiggliness of OM vs LR vs sparse-- compute first deriv and then stdev
#standard deviation
OM_num<-as.numeric(unlist(Nv))
sdOM=sd(OM_num,na.rm=FALSE)

LR_num<-as.numeric(unlist(Lr))
sdLR=sd(LR_num,na.rm=FALSE)

SP_num<-as.numeric(unlist(Sp))
sdSp=sd(SP_num,na.rm=FALSE)

#first derivative and then standard deviation
#OM_num<-as.numeric(unlist(Nv))
#sdOM=sd(OM_num,na.rm=FALSE)
#derivOM=deriv(Nv)
#in order to make this work i would have to get the equation/slope of each of the lines (each row) and then take the deriv of each of those lines
#i don't think i should remove the slope though


################################

#create a spectrogram of low rank

#colors
library("RColorBrewer")
display.brewer.all()
display.brewer.pal(n = 9, name = 'YlOrRd') #use this as legend
brewer.pal(n = 8, name = "RdBu")
brewer.pal(n=6,name = "YlOr")

#improving colors
Lrmatrix=as.matrix(Lr) #get the LR data (in decibels) into matrix form
#create color map
#colMap <- colorRampPalette(c("FFCC00","FF9900","FF3300" ))(Lrmatrix)
#plot
Lrspectro=image(Lrmatrix, col=brewer.pal(n=9,name="YlOrRd")) #this should create a spectrogram-- not really sure what axes are here...but it looks like there are 7 choruses!!
display.brewer.pal(n = 9, name = 'YlOrRd') #use this as legend for all 3


#now lets just see what image of sparse looks like
Spmatrix=as.matrix(Sp)
#SpDBabs=abs(Spmatrix) #if you want to exclude negative values then include this line
Spspectro=image(Spmatrix, col=brewer.pal(n=9,name="YlOrRd")) 
#Spspectro=image(SpDBabs, col=brewer.pal(n=9,name="YlOrRd")) #not including neg

#now original
Omatrix=as.matrix(Nv)
Ospectro=image(Omatrix, col=brewer.pal(n=9,name="YlOrRd"))

#figuring out maxDB for plots
max(LrDB)
min(LrDB)
max(Nv)
min(Nv)
max(SpDB)
min(SpDB)
max(SpDB, na.rm = TRUE)
min(SpDB, na.rm = TRUE)

#figuring out LR spectrum colors
diff=100-55
colorblock=diff/9 #each color block is 5 (so need to eliminate top 3 color blocks)
Lrspectro=image(Lrmatrix, col=brewer.pal("#B2182B","#D6604D","#F4A582","#FDDBC7","#D1E5F0","#92C5DE")) #this should create a spectrogram-- not really sure what axes are here...but it looks like there are 7 choruses!!
display.brewer.pal(n = 6, name = 'YlOrRd') #use this as legend for all 3


##re do spectrograms with normalizing amplitude
#Normalize all DB
LrNormalized=LrDB/(max(LrDB))
LNorm=as.matrix(LrNormalized)

SpNoramlized=SpDBabs/(max(SpDBabs))
SpNorm=as.matrix(SpNoramlized)

Omatrix=as.matrix(Nv)
ONormalized=Omatrix/(max(Omatrix))
ONorm=as.matrix(ONormalized)

#spectrograms
Ospectro=image(Omatrix, col=brewer.pal(n=9,name="YlOrRd"))
Lrspectro=image(Lrmatrix, col=brewer.pal(n=9,name="YlOrRd")) 
Spspectro=image(Spmatrix, col=brewer.pal(n=9,name="YlOrRd")) 


###############################################

#if you want to just look at night times-- didn't end up using this part

#make sure recognized as date, then add date, hr, month
weekhr$`yyyy-mm-ddTHH:MM:SSZ`
weekhr$DateF     = as.POSIXct( gsub(".000Z", "", gsub("T", " ", weekhr$`yyyy-mm-ddTHH:MM:SSZ`)), tz = "GMT" )
weekhr$DateFday  = as.Date(weekhr$DateF)
weekhr$Hr  = hour(weekhr$DateF)
weekhr$Mth = month(weekhr$DateFday) 

#separate out nighttime hours--
nv=weekhr
as.data.frame(colnames(nv))
nv=nv[1:504,4:55]
#adding to hix so that i can have the dates as well
hix  = as.numeric( gsub("PSD_","",names(nv)[1:48]) ) 

#adding categorical column names
hix=append(hix,"date",after=length(hix))
hix=append(hix,"day",after=length(hix))
hix=append(hix,"hr",after=length(hix))
hix=append(hix,"month",after=length(hix))
colnames(nv) = hix #this makes the column names numeric rather than TOL_123


#day vs. night  --- day=6-6PST = 12pm-1am: 12-1am UTC. night=1-12. 
nv$daynight = with(nv, ifelse(hr > 0 & hr < 13, "night", "day")) #create day and night labels
night <- nv[ which(nv$daynight=='night'),] #separated out day and night


#now lets run night through RRPCA! 
Nv   = as.data.frame(as.matrix( ( night[,1:48]) )) #change last value so it ends before day/night
NvP  = as.data.frame(10^(Nv/20))     #converting from db to pressure-- "un-dbing"
#(10^(Nv[1,1]/20)) check and make sure same value as first in Nvp. kk its good. 

#Plot original data
hix=hix[1:48] #eliminate the categorical names 
colnames(Nv) = hix #thist makes the column names numeric rather than TOL_123
orgdata <- reshape :: melt(t(Nv)  ) #melt gets the data into a columns that ggplot recognizes 
pO = ggplot(orgdata, aes(X1, value, group = as.factor(X2)))+ 
  geom_line(alpha=.1)+ scale_x_continuous()+ 
  ggtitle("Original Data")+ xlab("")+ theme_minimal() #changed from alpha=0.05 to .1 to make lines darker
#this plot groups by time step. x axis is frequency but its logarithmic. 

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
  geom_line(alpha=.1)+ scale_x_continuous()+ 
  ggtitle("Low rank")+ xlab("")+ theme_minimal() #changed alpha from 0.05 to 0.1
#this should result in a plot that gets rid of some of the wiggle. 

#SPARSE RESULTS
Sp = as.data.frame(nvpcaTOL$S) # all positive values... THERE ARE NEG VALUES HERE WHICH IS ODD
SpDB = 10*log10( Sp^2 ) #something is off here.... inf values???
colnames(Sp) = hix 
NvMSp <- reshape :: melt(t(Sp)  )
pS = ggplot(NvMSp, aes(X1, value, group = as.factor(X2)))+ 
  geom_line(alpha=.3)+ scale_x_continuous()+ 
  ggtitle("Sparse")+ xlab("")+ theme_minimal() #changed alpha from .05 to .5 to be able to see these outliers more

#plot original, low rank, and sparse all together
grid.arrange(pO,pL,pS)

#create a spectrogram of low rank
Lrnightmatrix=as.matrix(LrDB) #get the LR data (in decibels) into matrix form
Lrspectro=image(Lrnightmatrix) #this should create a spectrogram-- not really sure what axes are here...but it looks like there are 7 choruses!!
Lrspectro+title(main="LR night")

#now lets just see what image of sparse looks like
Spnightmatrix=as.matrix(SpDB)
Spnightmatrix[Spnightmatrix == -Inf] <- 0 #get rid of all the -inf values bc sparse matrix is screwed up
Spspectro=image(Spnightmatrix) #looks weird...
Spspectro+title(main="Sparse night")

#now original night
Onightmatrix=as.matrix(Nv)
Ospectro=image(Onightmatrix)+title(main="Original")

#######################################################
###############################################
###############################################
###############################################
#RRPCA for entire matrix

setwd("E:\\SDT\\SDT RRPCA\\Soundscape Metrics SDT\\SDT_BF_01_Soundscape Metrics\\SDT_BF_01_\\20Hz_20min_0-to-1000Hz")
SDTdata=as.data.frame(fread('SDT_BF_01_LTSA_1s_1Hz_PSD_20min.csv'))

nv=SDTdata #change to megan's language

#save as data frame to get values numeric, change column names to just frequency values, convert from dv to pressure
as.data.frame(colnames(nv))
hix  = as.numeric( gsub("PSD_","",names(nv)[4:51]) ) #gives freq range for plotting--ELLA changed from 25 to 17 b/c it was introducing NAs before. and didn't run lines 160-164... do i need those or is that only for truncating step????
Nv   = as.data.frame(as.matrix( ( nv[,4:51]) )) 
#NvP  = as.data.frame(10^(Nv/20))     #converting from db to pressure-- "un-dbing"

#Plot original data
colnames(Nv) = hix #thist makes the column names numeric rather than TOL_123
orgdata <- reshape :: melt(t(Nv)  ) #melt gets the data into a columns that ggplot recognizes 
pO = ggplot(orgdata, aes(X1, value, group = as.factor(X2)))+ 
  geom_line(alpha=.05)+ scale_x_continuous()+ 
  ggtitle("Original Data")+ xlab("")+ theme_minimal() #changed from alpha=0.05 to .2 to make lines darker
#this plot groups by time step. x axis is frequency but its logarithmic. 
#wait but this is for everything not just the week....

#RRPCA method for source separation
#-----------------------------------------------------------------------------------------

#setup input and run rrpca
input = Nv # or Nv (dB units)
nvpcaTOL = rrpca(input) #PREDICTED RANK IS OFTEN ZERO... is that BAD? lamda was screwing this up. 
#NOTE: pressure units avoid a negative change in ambient when transients removed

#LOW RANK RESULTS
Lr = as.data.frame(nvpcaTOL$L) # all negative values. WAIT L is ALL THE SAME VALUE, but S is diff values.... now megan and i fixed this.
#LrDB = 10*log10( Lr^2 ) #these values are all very close
colnames(Lr) = hix #assign hix to column names so that it registers as numeric
NvMlr <- reshape :: melt(t(Lr)  )
pL = ggplot(NvMlr, aes(X1, value, group = as.factor(X2)))+ 
  geom_line(alpha=.05)+ scale_x_continuous()+ 
  ggtitle("Low rank")+ xlab("")+ theme_minimal() #changed alpha from 0.05 to 0.2
#this should result in a plot that gets rid of some of the wiggle. 

#SPARSE RESULTS
Sp = as.data.frame(nvpcaTOL$S) # all positive values... THERE ARE NEG VALUES HERE WHICH IS ODD
#SpDB = 10*log10( Sp^2 ) #something is off here.... inf values???
colnames(Sp) = hix 
NvMSp <- reshape :: melt(t(Sp)  )
pS = ggplot(NvMSp, aes(X1, value, group = as.factor(X2)))+ 
  geom_line(alpha=.3 )+ scale_x_continuous()+ 
  ggtitle("Sparse")+ xlab("")+ theme_minimal() #changed alpha from .05 to .5 to be able to see these outliers more

#save matrices
Lrmatrix=as.matrix(Lr) #get the LR data (in decibels) into matrix form
#now lets just see what image of sparse looks like
Spmatrix=as.matrix(Sp)
#Spmatrix[Spmatrix == -Inf] <- 0 #get rid of all the -inf values bc sparse matrix is screwed up
#now original
Omatrix=as.matrix(Nv)

### write matrices(for entire deployment)
write.csv(Lrmatrix, "E:\\SDT\\SDT RRPCA\\RRPCA SDT\\matrices\\Lrmatrix_SDT_BF_01.csv",row.names=FALSE)
write.csv(Spmatrix, "E:\\SDT\\SDT RRPCA\\RRPCA SDT\\matrices\\Spmatrix_SDT_BF_01.csv",row.names=FALSE)
write.csv(Omatrix, "E:\\SDT\\SDT RRPCA\\RRPCA SDT\\matrices\\Omatrix_SDT_BF_01.csv",row.names=FALSE)


#############################


#saving time vectors

OMTT=nv[1]
Tmatrix=as.matrix(OMTT) #get the LR data (in decibels) into matrix form
write.csv(Tmatrix,"E:\\SDT\\SDT RRPCA\\RRPCA SDT\\matrices\\OMTT.csv",row.names=FALSE)


##############################################################

#Deployment 2: SDT_DP_01

#RRPCA for entire matrix

setwd("E:\\SDT\\SDT RRPCA\\Soundscape Metrics SDT\\SDT_DP_01_Soundscape Metrics")
SDTdata=as.data.frame(fread('SDT_DP_01_LTSA_1s_1Hz_PSD_20min.csv'))
nv=SDTdata

#save as data frame to get values numeric, change column names to just frequency values, convert from dv to pressure
as.data.frame(colnames(nv))
hix  = as.numeric( gsub("PSD_","",names(nv)[4:51]) ) #gives freq range for plotting--ELLA changed from 25 to 17 b/c it was introducing NAs before. and didn't run lines 160-164... do i need those or is that only for truncating step????
Nv   = as.data.frame(as.matrix( ( nv[,4:51]) )) 
#NvP  = as.data.frame(10^(Nv/20))     #converting from db to pressure-- "un-dbing"

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
input = Nv # or Nvp (pressure)
nvpcaTOL = rrpca(input) #PREDICTED RANK IS OFTEN ZERO... is that BAD? lamda was screwing this up. 
#NOTE: pressure units avoid a negative change in ambient when transients removed

#LOW RANK RESULTS
Lr = as.data.frame(nvpcaTOL$L) # all negative values. WAIT L is ALL THE SAME VALUE, but S is diff values.... now megan and i fixed this.
#LrDB = 10*log10( Lr^2 ) #these values are all very close
colnames(Lr) = hix #assign hix to column names so that it registers as numeric
NvMlr <- reshape :: melt(t(Lr)  )
pL = ggplot(NvMlr, aes(X1, value, group = as.factor(X2)))+ 
  geom_line(alpha=.05)+ scale_x_continuous()+ 
  ggtitle("Low rank")+ xlab("")+ theme_minimal() #changed alpha from 0.05 to 0.2
#this should result in a plot that gets rid of some of the wiggle. 

#SPARSE RESULTS
Sp = as.data.frame(nvpcaTOL$S) # all positive values... THERE ARE NEG VALUES HERE WHICH IS ODD
#SpDB = 10*log10( Sp^2 ) #something is off here.... inf values???
colnames(Sp) = hix 
NvMSp <- reshape :: melt(t(Sp)  )
pS = ggplot(NvMSp, aes(X1, value, group = as.factor(X2)))+ 
  geom_line(alpha=.3 )+ scale_x_continuous()+ 
  ggtitle("Sparse")+ xlab("")+ theme_minimal() #changed alpha from .05 to .5 to be able to see these outliers more

#save matrices
Lrmatrix=as.matrix(Lr) #get the LR data (in decibels) into matrix form
#now lets just see what image of sparse looks like
Spmatrix=as.matrix(Sp)
#Spmatrix[Spmatrix == -Inf] <- 0 #get rid of all the -inf values bc sparse matrix is screwed up
#now original
Omatrix=as.matrix(Nv)

### write matrices(for entire deployment)
write.csv(Lrmatrix, "E:\\SDT\\SDT RRPCA\\RRPCA SDT\\matrices\\DP_01\\Lrmatrix_SDT_DP_01.csv",row.names=FALSE)
write.csv(Spmatrix, "E:\\SDT\\SDT RRPCA\\RRPCA SDT\\matrices\\DP_01\\Spmatrix_SDT_DP_01.csv",row.names=FALSE)
write.csv(Omatrix, "E:\\SDT\\SDT RRPCA\\RRPCA SDT\\matrices\\DP_01\\Omatrix_SDT_DP_01.csv",row.names=FALSE)

#saving time vector as csv
OMTT=nv[1]
Tmatrix=as.matrix(OMTT) #get the LR data (in decibels) into matrix form
write.csv(Tmatrix,"E:\\SDT\\SDT RRPCA\\RRPCA SDT\\matrices\\DP_01\\OMTT.csv",row.names=FALSE)


##############################################################

#Deployment 3: SDT_GR_01

#RRPCA for entire matrix

setwd("E:\\SDT\\SDT RRPCA\\Soundscape Metrics SDT\\SDT_GR_01_Soundscape Metrics")
SDTdata=as.data.frame(fread('SDT_GR_01_LTSA_1s_1Hz_PSD_20min.csv'))
nv=SDTdata #change to megan's language- and delete row of infinity values

#save as data frame to get values numeric, change column names to just frequency values, convert from dv to pressure
as.data.frame(colnames(nv))
hix  = as.numeric( gsub("PSD_","",names(nv)[4:51]) ) #gives freq range for plotting--ELLA changed from 25 to 17 b/c it was introducing NAs before. and didn't run lines 160-164... do i need those or is that only for truncating step????
Nv   = as.data.frame(as.matrix( ( nv[,4:51]) )) 
#  NvP  = as.data.frame(10^(Nv/20))     #converting from db to pressure-- "un-dbing"

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
write.csv(Lrmatrix, "E:\\SDT\\SDT RRPCA\\RRPCA SDT\\matrices\\GR_01\\Lrmatrix_SDT_GR_01.csv",row.names=FALSE)
write.csv(Spmatrix, "E:\\SDT\\SDT RRPCA\\RRPCA SDT\\matrices\\GR_01\\Spmatrix_SDT_GR_01.csv",row.names=FALSE)
write.csv(Omatrix, "E:\\SDT\\SDT RRPCA\\RRPCA SDT\\matrices\\GR_01\\Omatrix_SDT_GR_01.csv",row.names=FALSE)

#saving time vector as csv
OMTT=nv[1]
Tmatrix=as.matrix(OMTT) #get the LR data (in decibels) into matrix form
write.csv(Tmatrix,"E:\\SDT\\SDT RRPCA\\RRPCA SDT\\matrices\\GR_01\\OMTT.csv",row.names=FALSE)

##############################################################

#Deployment 4: SDT_HP_01

#RRPCA for entire matrix

setwd("E:\\SDT\\SDT RRPCA\\Soundscape Metrics SDT\\SDT_HP_01_Soundscape Metrics")
SDTdata=as.data.frame(fread('SDT_HP_01_LTSA_1s_1Hz_PSD_20min.csv'))
nv=SDTdata #change to megan's language- and delete row of infinity values

#save as data frame to get values numeric, change column names to just frequency values, convert from dv to pressure
as.data.frame(colnames(nv))
hix  = as.numeric( gsub("PSD_","",names(nv)[4:51]) ) #gives freq range for plotting--ELLA changed from 25 to 17 b/c it was introducing NAs before. and didn't run lines 160-164... do i need those or is that only for truncating step????
Nv   = as.data.frame(as.matrix( ( nv[,4:51]) )) 
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
write.csv(Lrmatrix, "E:\\SDT\\SDT RRPCA\\RRPCA SDT\\matrices\\HP_01\\Lrmatrix_SDT_HP_01.csv",row.names=FALSE)
write.csv(Spmatrix, "E:\\SDT\\SDT RRPCA\\RRPCA SDT\\matrices\\HP_01\\Spmatrix_SDT_HP_01.csv",row.names=FALSE)
write.csv(Omatrix, "E:\\SDT\\SDT RRPCA\\RRPCA SDT\\matrices\\HP_01\\Omatrix_SDT_HP_01.csv",row.names=FALSE)

#saving time vector as csv
OMTT=nv[1]
Tmatrix=as.matrix(OMTT) #get the LR data (in decibels) into matrix form
write.csv(Tmatrix,"E:\\SDT\\SDT RRPCA\\RRPCA SDT\\matrices\\HP_01\\OMTT.csv",row.names=FALSE)

###############################################################################

#Deployment 5: SDT_PR_01

#RRPCA for entire matrix

setwd("E:\\SDT\\SDT RRPCA\\Soundscape Metrics SDT\\SDT_PR_01_Soundscape Metrics")
SDTdata=as.data.frame(fread('SDT_PR_01_LTSA_1s_1Hz_PSD_20min.csv'))
nv=SDTdata #change to megan's language- and delete row of infinity values

#save as data frame to get values numeric, change column names to just frequency values, convert from dv to pressure
as.data.frame(colnames(nv))
hix  = as.numeric( gsub("PSD_","",names(nv)[4:51]) ) #gives freq range for plotting--ELLA changed from 25 to 17 b/c it was introducing NAs before. and didn't run lines 160-164... do i need those or is that only for truncating step????
Nv   = as.data.frame(as.matrix( ( nv[,4:51]) )) 
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
write.csv(Lrmatrix, "E:\\SDT\\SDT RRPCA\\RRPCA SDT\\matrices\\PR_01\\Lrmatrix_SDT_PR_01.csv",row.names=FALSE)
write.csv(Spmatrix, "E:\\SDT\\SDT RRPC\A\RRPCA SDT\\matrices\\PR_01\\Spmatrix_SDT_PR_01.csv",row.names=FALSE)
write.csv(Omatrix, "E:\\SDT\\SDT RRPCA\\RRPCA SDT\\matrices\\PR_01\\Omatrix_SDT_PR_01.csv",row.names=FALSE)

#saving time vector as csv
OMTT=nv[1]
Tmatrix=as.matrix(OMTT) #get the LR data (in decibels) into matrix form
write.csv(Tmatrix,"E:\\SDT\\SDT RRPCA\\RRPCA SDT\\matrices\\PR_01\\OMTT.csv",row.names=FALSE)

###############################################################################

#Deployment 6: SDT_SL_01

#RRPCA for entire matrix

setwd("E:\\SDT\\SDT RRPCA\\Soundscape Metrics SDT\\SDT_SL_01_Soundscape Metrics")
SDTdata=as.data.frame(fread('SDT_SL_01_LTSA_1s_1Hz_PSD_20min.csv'))
nv=SDTdata #change to megan's language- and delete row of infinity values

#save as data frame to get values numeric, change column names to just frequency values, convert from dv to pressure
as.data.frame(colnames(nv))
hix  = as.numeric( gsub("PSD_","",names(nv)[4:51]) ) #gives freq range for plotting--ELLA changed from 25 to 17 b/c it was introducing NAs before. and didn't run lines 160-164... do i need those or is that only for truncating step????
Nv   = as.data.frame(as.matrix( ( nv[,4:51]) )) 
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
  ggtitle("Sparse")+ xlab("")+ ylab("Amplitude (dB)")+ theme_minimal() #changed alpha from .05 to .5 to be able to see these outliers more

#save matrices
Lrmatrix=as.matrix(LrDB) #get the LR data (in decibels) into matrix form
#now lets just see what image of sparse looks like
Spmatrix=as.matrix(SpDB)
Spmatrix[Spmatrix == -Inf] <- 0 #get rid of all the -inf values bc sparse matrix is screwed up
#now original
Omatrix=as.matrix(Nv)

### write matrices(for entire deployment)
write.csv(Lrmatrix, "E:\\SDT\\SDT RRPCA\\RRPCA SDT\\matrices\\SL_01\\Lrmatrix_SDT_SL_01.csv",row.names=FALSE)
write.csv(Spmatrix, "E:\\SDT\\SDT RRPCA\\RRPCA SDT\\matrices\\SL_01\\Spmatrix_SDT_SL_01.csv",row.names=FALSE)
write.csv(Omatrix, "E:\\SDT\\SDT RRPCA\\RRPCA SDT\\matrices\\SL_01\\Omatrix_SDT_SL_01.csv",row.names=FALSE)

#saving time vector as csv
OMTT=nv[1]
Tmatrix=as.matrix(OMTT) #get the LR data (in decibels) into matrix form
write.csv(Tmatrix,"E:\\SDT\\SDT RRPCA\\RRPCA SDT\\matrices\\SL_01\\OMTT.csv",row.names=FALSE)


##############################################################
#############################################################
#DEPLOYMENT 1:
#SDT BF_01 PSD Median Values, 0:800 Hz (from df 10 data), 20 Hz, 20 min binning

#upload data
setwd("E:\\SDT\\SDT RRPCA\\Soundscape Metrics SDT\\SDT_BF_01_Soundscape Metrics\\SDT_BF_01_\\20Hz_20min_0-to-800Hz")
SDTdata=as.data.frame(fread('SDT_BF_01_LTSA_1s_1Hz_PSD_20min.csv'))

#separate out one week for visualizations
weekhr=SDTdata[1658:2161,1:42] #Row 1658-2161 (8/20-8/26)
nv=weekhr #change to megan's language

#save as data frame to get values numeric, change column names to just frequency values, convert from dv to pressure
as.data.frame(colnames(nv))
hix  = as.numeric( gsub("PSD_","",names(nv)[4:42]) ) #gives freq range for plotting--ELLA changed from 25 to 17 b/c it was introducing NAs before. and didn't run lines 160-164... do i need those or is that only for truncating step????
Nv   = as.data.frame(as.matrix( ( nv[,4:42]) )) 
NvP  = as.data.frame(10^(Nv/20))     #converting from db to pressure-- "un-dbing"

#Plot original data
colnames(Nv) = hix #this makes the column names numeric rather than PSD_123
orgdata <- reshape :: melt(t(Nv)  ) #melt gets the data into a columns that ggplot recognizes 
pO = ggplot(orgdata, aes(X1, value, group = as.factor(X2)))+ 
  geom_line(alpha=.05)+ scale_x_continuous()+ 
  ggtitle("Original Data")+ xlab("")+ ylab("Amplitude (dB re 1 ?Pa^2/Hz)")+ theme_minimal() #changed from alpha=0.05 to .2 to make lines darker
#this plot groups by time step. x axis is frequency but its logarithmic. 
#this is for everything not just the week....


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
  ggtitle("Low Rank")+ xlab("")+ ylab("Amplitude (dB re 1 ?Pa^2/Hz)")+ theme_minimal() #changed alpha from 0.05 to 0.2
#this should result in a plot that gets rid of some of the wiggle. 

#SPARSE RESULTS
Sp = as.data.frame(nvpcaTOL$S) # all positive values... THERE ARE NEG VALUES HERE WHICH IS ODD
SpDB = 10*log10( Sp^2 ) #something is off here.... inf values???
colnames(Sp) = hix 
NvMSp <- reshape :: melt(t(Sp)  )
pS = ggplot(NvMSp, aes(X1, value, group = as.factor(X2)))+ 
  geom_line(alpha=.3 )+ scale_x_continuous()+ 
  ggtitle("Sparse")+ xlab("")+ ylab("Amplitude (dB re 1 ?Pa^2/Hz)")+ theme_minimal() #changed alpha from .05 to .5 to be able to see these outliers more

#plot original, low rank, and sparse all together
grid.arrange(pO,pL,pS)

#create a spectrogram of low rank
Lrmatrix=as.matrix(LrDB) #get the LR data (in decibels) into matrix form
Lrspectro=image(Lrmatrix,axes=F,legend.only=T) #this should create a spectrogram-- not really sure what axes are here...but it looks like there are 7 choruses!!
Lrspectro+title(main="LR")
Lrspectro=image.plot(Lrmatrix)



#now lets just see what image of sparse looks like
Spmatrix=as.matrix(SpDB)
Spmatrix[Spmatrix == -Inf] <- 0 #get rid of all the -inf values bc sparse matrix is screwed up
Spspectro=image(Spmatrix) #looks weird...
Spspectro+title(main="Sparse")

#now original
Omatrix=as.matrix(Nv)
Ospectro=image(Omatrix)+title(main="Original")

#########################################
## trying to fix issue with sparse matrix-- running it on pascal values

#setup input and run rrpca
input = Nv # or Nv (dB units)
nvpcaTOL = rrpca(input) #PREDICTED RANK IS OFTEN ZERO... is that BAD? lamda was screwing this up. 
#NOTE: pressure units avoid a negative change in ambient when transients removed

#LOW RANK RESULTS
Lr = as.data.frame(nvpcaTOL$L) # all negative values. WAIT L is ALL THE SAME VALUE, but S is diff values.... now megan and i fixed this.
#LrDB = 10*log10( Lr^2 ) #these values are all very close
colnames(Lr) = hix #assign hix to column names so that it registers as numeric
NvMlr <- reshape :: melt(t(Lr)  )
pL = ggplot(NvMlr, aes(X1, value, group = as.factor(X2)))+ 
  geom_line(alpha=.05)+ scale_x_continuous()+ 
  ggtitle("Low Rank")+ xlab("")+ ylab("Amplitude (dB)")+ theme_minimal() #changed alpha from 0.05 to 0.2
#this should result in a plot that gets rid of some of the wiggle. 

#SPARSE RESULTS
Sp = as.data.frame(nvpcaTOL$S) # all positive values... THERE ARE NEG VALUES HERE WHICH IS ODD
Sp[Sp == -Inf] <- NA #get rid of all the -inf values bc sparse matrix is screwed up
Sp[Sp < 0] <- NA   
colnames(Sp) = hix 
NvMSp <- reshape :: melt(t(Sp)  )
pS = ggplot(NvMSp, aes(X1, value, group = as.factor(X2)))+ 
  geom_line(alpha=.1 )+ scale_x_continuous()+ 
  ggtitle("Sparse")+ xlab("Frequency (Hz)")+ ylab("Amplitude (dB)")+ theme_minimal() #changed alpha from .05 to .5 to be able to see these outliers more

#plot original, low rank, and sparse all together
grid.arrange(pO,pL,pS)

#create a spectrogram of low rank
Lrmatrix=as.matrix(Lr) #get the LR data (in decibels) into matrix form
Lrspectro=image(Lrmatrix) #this should create a spectrogram-- not really sure what axes are here...but it looks like there are 7 choruses!!
Lrspectro+title(main="LR")
box(col="black")

#now lets just see what image of sparse looks like
Spmatrix=as.matrix(Sp)
#Spmatrix[Spmatrix == -Inf] <- 0 #get rid of all the -inf values bc sparse matrix is screwed up
Spspectro=image(Spmatrix) #looks weird...
Spspectro+title(main="Sparse")
box(col="black")

#now original
Omatrix=as.matrix(Nv)
Ospectro=image(Omatrix)+title(main="Original")
box(col="black")




################################################################################
#RRPCA for entire matrix-- so that we can save matrices and cluster

#upload data

setwd("E:\\SDT\\SDT RRPCA\\Soundscape Metrics SDT\\SDT_BF_01_Soundscape Metrics\\SDT_BF_01_\\20Hz_20min_0-to-800Hz")
SDTdata=as.data.frame(fread('SDT_BF_01_LTSA_1s_1Hz_PSD_20min.csv'))

nv=SDTdata #change to megan's language

#save as data frame to get values numeric, change column names to just frequency values, convert from dv to pressure
as.data.frame(colnames(nv))
hix  = as.numeric( gsub("PSD_","",names(nv)[4:42]) ) #gives freq range for plotting--ELLA changed from 25 to 17 b/c it was introducing NAs before. and didn't run lines 160-164... do i need those or is that only for truncating step????
Nv   = as.data.frame(as.matrix( ( nv[,4:42]) )) 
NvP  = as.data.frame(10^(Nv/20))     #converting from db to pressure-- "un-dbing"

#Plot original data
colnames(Nv) = hix #thist makes the column names numeric rather than TOL_123
orgdata <- reshape :: melt(t(Nv)  ) #melt gets the data into a columns that ggplot recognizes 
pO = ggplot(orgdata, aes(X1, value, group = as.factor(X2)))+ 
  geom_line(alpha=.05)+ scale_x_continuous()+ 
  ggtitle("Original Data")+ xlab("")+ theme_minimal() #changed from alpha=0.05 to .2 to make lines darker

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
write.csv(Lrmatrix, "E:\\SDT\\SDT RRPCA\\RRPCA SDT\\matrices\\BF_01\\Lrmatrix_SDT_BF_01.csv",row.names=FALSE)
write.csv(Spmatrix, "E:\\SDT\\SDT RRPCA\\RRPCA SDT\\matrices\\BF_01\\Spmatrix_SDT_BF_01.csv",row.names=FALSE)
write.csv(Omatrix, "E:\\SDT\\SDT RRPCA\\RRPCA SDT\\matrices\\BF_01\\Omatrix_SDT_BF_01.csv",row.names=FALSE)

#saving time vector as csv
OMTT=nv[1]
Tmatrix=as.matrix(OMTT) #get the LR data (in decibels) into matrix form
write.csv(Tmatrix,"E:\\SDT\\SDT RRPCA\\RRPCA SDT\\matrices\\BF_01\\OMTT.csv",row.names=FALSE)

#___________________________________________________________________________________________________

#Deployment 2: DP_01 
#skipped week long visualizations
################################################################################
#RRPCA for entire matrix-- so that we can save matrices and cluster

#upload data

setwd("E:\\SDT\\SDT RRPCA\\Soundscape Metrics SDT\\SDT_DP_01_Soundscape Metrics")
SDTdata=as.data.frame(fread('SDT_DP_01_LTSA_1s_1Hz_PSD_20min.csv'))

nv=SDTdata #change to megan's language

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
  ggtitle("Original Data")+ xlab("")+ theme_minimal() #changed from alpha=0.05 to .2 to make lines darker

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
write.csv(Lrmatrix, "E:\\SDT\\SDT RRPCA\\RRPCA SDT\\matrices\\DP_01\\Lrmatrix_SDT_DP_01.csv",row.names=FALSE)
write.csv(Spmatrix, "E:\\SDT\\SDT RRPCA\\RRPCA SDT\\matrices\\DP_01\\Spmatrix_SDT_DP_01.csv",row.names=FALSE)
write.csv(Omatrix, "E:\\SDT\\SDT RRPCA\\RRPCA SDT\\matrices\\DP_01\\Omatrix_SDT_DP_01.csv",row.names=FALSE)

#saving time vector as csv
OMTT=nv[1]
Tmatrix=as.matrix(OMTT) #get the LR data (in decibels) into matrix form
write.csv(Tmatrix,"E:\\SDT\\SDT RRPCA\\RRPCA SDT\\matrices\\DP_01\\OMTT.csv",row.names=FALSE)


#___________________________________________________________________________________________________

#Deployment 3: GR_01 
#skipped week long visualizations
################################################################################
#RRPCA for entire matrix-- so that we can save matrices and cluster

#upload data

setwd("E:\\SDT\\SDT RRPCA\\Soundscape Metrics SDT\\SDT_GR_01_Soundscape Metrics")
SDTdata=as.data.frame(fread('SDT_GR_01_LTSA_1s_1Hz_PSD_20min.csv'))

nv=SDTdata #change to megan's language

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
  ggtitle("Original Data")+ xlab("")+ theme_minimal() #changed from alpha=0.05 to .2 to make lines darker

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
write.csv(Lrmatrix, "E:\\SDT\\SDT RRPCA\\RRPCA SDT\\matrices\\GR_01\\Lrmatrix_SDT_GR_01.csv",row.names=FALSE)
write.csv(Spmatrix, "E:\\SDT\\SDT RRPCA\\RRPCA SDT\\matrices\\GR_01\\Spmatrix_SDT_GR_01.csv",row.names=FALSE)
write.csv(Omatrix, "E:\\SDT\\SDT RRPCA\\RRPCA SDT\\matrices\\GR_01\\Omatrix_SDT_GR_01.csv",row.names=FALSE)

#saving time vector as csv
OMTT=nv[1]
Tmatrix=as.matrix(OMTT) #get the LR data (in decibels) into matrix form
write.csv(Tmatrix,"E:\\SDT\\SDT RRPCA\\RRPCA SDT\\matrices\\GR_01\\OMTT.csv",row.names=FALSE)


#___________________________________________________________________________________________________

#Deployment 4: HP_01 
#skipped week long visualizations
################################################################################
#RRPCA for entire matrix-- so that we can save matrices and cluster

#upload data

setwd("E:\\SDT\\SDT RRPCA\\Soundscape Metrics SDT\\SDT_HP_01_Soundscape Metrics")
SDTdata=as.data.frame(fread('SDT_HP_01_LTSA_1s_1Hz_PSD_20min.csv'))

nv=SDTdata #change to megan's language

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
  ggtitle("Original Data")+ xlab("")+ theme_minimal() #changed from alpha=0.05 to .2 to make lines darker

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
write.csv(Lrmatrix, "E:\\SDT\\SDT RRPCA\\RRPCA SDT\\matrices\\HP_01\\Lrmatrix_SDT_HP_01.csv",row.names=FALSE)
write.csv(Spmatrix, "E:\\SDT\\SDT RRPCA\\RRPCA SDT\\matrices\\HP_01\\Spmatrix_SDT_HP_01.csv",row.names=FALSE)
write.csv(Omatrix, "E:\\SDT\\SDT RRPCA\\RRPCA SDT\\matrices\\HP_01\\Omatrix_SDT_HP_01.csv",row.names=FALSE)

#saving time vector as csv
OMTT=nv[1]
Tmatrix=as.matrix(OMTT) #get the LR data (in decibels) into matrix form
write.csv(Tmatrix,"E:\\SDT\\SDT RRPCA\\RRPCA SDT\\matrices\\HP_01\\OMTT.csv",row.names=FALSE)


#___________________________________________________________________________________________________

#Deployment 5: PR_01 
#skipped week long visualizations
################################################################################
#RRPCA for entire matrix-- so that we can save matrices and cluster

#upload data

setwd("E:\\SDT\\SDT RRPCA\\Soundscape Metrics SDT\\SDT_PR_01_Soundscape Metrics")
SDTdata=as.data.frame(fread('SDT_PR_01_LTSA_1s_1Hz_PSD_20min.csv'))

nv=SDTdata #change to megan's language

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
  ggtitle("Original Data")+ xlab("")+ theme_minimal() #changed from alpha=0.05 to .2 to make lines darker

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
write.csv(Lrmatrix, "E:\\SDT\\SDT RRPCA\\RRPCA SDT\\matrices\\PR_01\\Lrmatrix_SDT_PR_01.csv",row.names=FALSE)
write.csv(Spmatrix, "E:\\SDT\\SDT RRPCA\\RRPCA SDT\\matrices\\PR_01\\Spmatrix_SDT_PR_01.csv",row.names=FALSE)
write.csv(Omatrix, "E:\\SDT\\SDT RRPCA\\RRPCA SDT\\matrices\\PR_01\\Omatrix_SDT_PR_01.csv",row.names=FALSE)

#saving time vector as csv
OMTT=nv[1]
Tmatrix=as.matrix(OMTT) #get the LR data (in decibels) into matrix form
write.csv(Tmatrix,"E:\\SDT\\SDT RRPCA\\RRPCA SDT\\matrices\\PR_01\\OMTT.csv",row.names=FALSE)



#___________________________________________________________________________________________________

#Deployment 6: SL_01 
#skipped week long visualizations
################################################################################
#RRPCA for entire matrix-- so that we can save matrices and cluster

#upload data

setwd("E:\\SDT\\SDT RRPCA\\Soundscape Metrics SDT\\SDT_SL_01_Soundscape Metrics")
SDTdata=as.data.frame(fread('SDT_SL_01_LTSA_1s_1Hz_PSD_20min.csv'))

nv=SDTdata #change to megan's language

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
  ggtitle("Original Data")+ xlab("")+ theme_minimal() #changed from alpha=0.05 to .2 to make lines darker

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
write.csv(Lrmatrix, "E:\\SDT\\SDT RRPCA\\RRPCA SDT\\matrices\\SL_01\\Lrmatrix_SDT_SL_01.csv",row.names=FALSE)
write.csv(Spmatrix, "E:\\SDT\\SDT RRPCA\\RRPCA SDT\\matrices\\SL_01\\Spmatrix_SDT_SL_01.csv",row.names=FALSE)
write.csv(Omatrix, "E:\\SDT\\SDT RRPCA\\RRPCA SDT\\matrices\\SL_01\\Omatrix_SDT_SL_01.csv",row.names=FALSE)

#saving time vector as csv
OMTT=nv[1]
Tmatrix=as.matrix(OMTT) #get the LR data (in decibels) into matrix form
write.csv(Tmatrix,"E:\\SDT\\SDT RRPCA\\RRPCA SDT\\matrices\\SL_01\\OMTT.csv",row.names=FALSE)



#Deployment 7: SW_01 
#skipped week long visualizations
################################################################################
#RRPCA for entire matrix-- so that we can save matrices and cluster

#upload data

setwd("E:\\SDT\\SDT RRPCA\\Soundscape Metrics SDT\\SDT_SW_01_Soundscape Metrics")
SDTdata=as.data.frame(fread('SDT_SW_01_PSD_20min.csv'))

nv=SDTdata #change to megan's language

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
  ggtitle("Original Data")+ xlab("")+ theme_minimal() #changed from alpha=0.05 to .2 to make lines darker

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
write.csv(Lrmatrix, "E:\\SDT\\SDT RRPCA\\RRPCA SDT\\matrices\\SW_01\\Lrmatrix_SDT_SW_01.csv",row.names=FALSE)
write.csv(Spmatrix, "E:\\SDT\\SDT RRPCA\\RRPCA SDT\\matrices\\SW_01\\Spmatrix_SDT_SW_01.csv",row.names=FALSE)
write.csv(Omatrix, "E:\\SDT\\SDT RRPCA\\RRPCA SDT\\matrices\\SW_01\\Omatrix_SDT_SW_01.csv",row.names=FALSE)

#saving time vector as csv
OMTT=nv[1]
Tmatrix=as.matrix(OMTT) #get the LR data (in decibels) into matrix form
write.csv(Tmatrix,"E:\\SDT\\SDT RRPCA\\RRPCA SDT\\matrices\\SW_01\\OMTT.csv",row.names=FALSE)


#Deployment 8: SZ_01 
#skipped week long visualizations
################################################################################
#RRPCA for entire matrix-- so that we can save matrices and cluster

#upload data

setwd("E:\\SDT\\SDT RRPCA\\Soundscape Metrics SDT\\SDT_SZ_01_Soundscape Metrics")
SDTdata=as.data.frame(fread('SDT_SZ_01_PSD_20min.csv'))

nv=SDTdata #change to megan's language

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
  ggtitle("Original Data")+ xlab("")+ theme_minimal() #changed from alpha=0.05 to .2 to make lines darker

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
write.csv(Lrmatrix, "E:\\SDT\\SDT RRPCA\\RRPCA SDT\\matrices\\SZ_01\\Lrmatrix_SDT_SW_01.csv",row.names=FALSE)
write.csv(Spmatrix, "E:\\SDT\\SDT RRPCA\\RRPCA SDT\\matrices\\SZ_01\\Spmatrix_SDT_SW_01.csv",row.names=FALSE)
write.csv(Omatrix, "E:\\SDT\\SDT RRPCA\\RRPCA SDT\\matrices\\SZ_01\\Omatrix_SDT_SW_01.csv",row.names=FALSE)

#saving time vector as csv
OMTT=nv[1]
Tmatrix=as.matrix(OMTT) #get the LR data (in decibels) into matrix form
write.csv(Tmatrix,"E:\\SDT\\SDT RRPCA\\RRPCA SDT\\matrices\\SZ_01\\OMTT.csv",row.names=FALSE)



#Deployment 9: WQ_01 
#skipped week long visualizations
################################################################################
#RRPCA for entire matrix-- so that we can save matrices and cluster

#upload data

setwd("E:\\SDT\\SDT RRPCA\\Soundscape Metrics SDT\\SDT_WQ_01_Soundscape Metrics")
SDTdata=as.data.frame(fread('SDT_WQ_PSD_20min.csv'))

nv=SDTdata #change to megan's language

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
  ggtitle("Original Data")+ xlab("")+ theme_minimal() #changed from alpha=0.05 to .2 to make lines darker

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
write.csv(Lrmatrix, "E:\\SDT\\SDT RRPCA\\RRPCA SDT\\matrices\\WQ_01\\Lrmatrix_SDT_WQ_01.csv",row.names=FALSE)
write.csv(Spmatrix, "E:\\SDT\\SDT RRPCA\\RRPCA SDT\\matrices\\WQ_01\\Spmatrix_SDT_WQ_01.csv",row.names=FALSE)
write.csv(Omatrix, "E:\\SDT\\SDT RRPCA\\RRPCA SDT\\matrices\\WQ_01\\Omatrix_SDT_WQ_01.csv",row.names=FALSE)

#saving time vector as csv
OMTT=nv[1]
Tmatrix=as.matrix(OMTT) #get the LR data (in decibels) into matrix form
write.csv(Tmatrix,"E:\\SDT\\SDT RRPCA\\RRPCA SDT\\matrices\\WQ_01\\OMTT.csv",row.names=FALSE)



#THESE ITERATIONS WERE NOT USED
##############################################################
#############################################################
#SDT BF_01 PSD Median Values, 0:800 Hz (from df 10 data), 300 Hz, 20 min binning

#upload data
setwd("E:\\SDT\\SDT RRPCA\\Soundscape Metrics SDT\\SDT_BF_01_Soundscape Metrics\\SDT_BF_01_\\20Hz_20min_0-to-800Hz")
SDTdata=as.data.frame(fread('SDT_BF_01_LTSA_1s_1Hz_PSD_20min.csv'))

#separate out one week for visualizations
weekhr=SDTdata[1658:2161,1:42] #Row 1658-2161 (8/20-8/26)
nv=weekhr #change to megan's language

#save as data frame to get values numeric, change column names to just frequency values, convert from dv to pressure
as.data.frame(colnames(nv))
hix  = as.numeric( gsub("PSD_","",names(nv)[4:42]) ) #gives freq range for plotting--ELLA changed from 25 to 17 b/c it was introducing NAs before. and didn't run lines 160-164... do i need those or is that only for truncating step????
Nv   = as.data.frame(as.matrix( ( nv[,4:42]) )) 
NvP  = as.data.frame(10^(Nv/20))     #converting from db to pressure-- "un-dbing"

#Plot original data
colnames(Nv) = hix #this makes the column names numeric rather than PSD_123
orgdata <- reshape :: melt(t(Nv)  ) #melt gets the data into a columns that ggplot recognizes 
pO = ggplot(orgdata, aes(X1, value, group = as.factor(X2)))+ 
  geom_line(alpha=.05)+ scale_x_continuous()+ 
  ggtitle("Original Data")+ xlab("")+ theme_minimal() #changed from alpha=0.05 to .2 to make lines darker
#this plot groups by time step. x axis is frequency but its logarithmic. 
#this is for everything not just the week....


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
  ggtitle("Low Rank")+ xlab("")+ theme_minimal() #changed alpha from 0.05 to 0.2
#this should result in a plot that gets rid of some of the wiggle. 

#SPARSE RESULTS
Sp = as.data.frame(nvpcaTOL$S) # all positive values... THERE ARE NEG VALUES HERE WHICH IS ODD
SpDB = 10*log10( Sp^2 ) #something is off here.... inf values???
colnames(Sp) = hix 
NvMSp <- reshape :: melt(t(Sp)  )
pS = ggplot(NvMSp, aes(X1, value, group = as.factor(X2)))+ 
  geom_line(alpha=.3 )+ scale_x_continuous()+ 
  ggtitle("Sparse")+ xlab("Frequency (Hz)")+ theme_minimal() #changed alpha from .05 to .5 to be able to see these outliers more

#plot original, low rank, and sparse all together
grid.arrange(pO,pL,pS)


#create a spectrogram of low rank
Lrmatrix=as.matrix(LrDB) #get the LR data (in decibels) into matrix form
Lrspectro=image(Lrmatrix) #this should create a spectrogram-- not really sure what axes are here...but it looks like there are 7 choruses!!
Lrspectro+title(main="LR")

#now lets just see what image of sparse looks like
Spmatrix=as.matrix(SpDB)
Spmatrix[Spmatrix == -Inf] <- 0 #get rid of all the -inf values bc sparse matrix is screwed up
Spspectro=image(Spmatrix) #looks weird...
Spspectro+title(main="Sparse")

#now original
Omatrix=as.matrix(Nv)
Ospectro=image(Omatrix)+title(main="Original")



################################################################################
#RRPCA for entire matrix-- so that we can save matrices and cluster

#upload data

setwd("E:\\SDT\\SDT RRPCA\\Soundscape Metrics SDT\\SDT_BF_01_Soundscape Metrics\\SDT_BF_01_\\20Hz_20min_0-to-800Hz")
SDTdata=as.data.frame(fread('SDT_BF_01_LTSA_1s_1Hz_PSD_20min.csv'))

nv=SDTdata #change to megan's language

#save as data frame to get values numeric, change column names to just frequency values, convert from dv to pressure
as.data.frame(colnames(nv))
hix  = as.numeric( gsub("PSD_","",names(nv)[2:42]) ) #gives freq range for plotting--ELLA changed from 25 to 17 b/c it was introducing NAs before. and didn't run lines 160-164... do i need those or is that only for truncating step????
Nv   = as.data.frame(as.matrix( ( nv[,2:42]) )) 
NvP  = as.data.frame(10^(Nv/20))     #converting from db to pressure-- "un-dbing"

#Plot original data
colnames(Nv) = hix #thist makes the column names numeric rather than TOL_123
orgdata <- reshape :: melt(t(Nv)  ) #melt gets the data into a columns that ggplot recognizes 
pO = ggplot(orgdata, aes(X1, value, group = as.factor(X2)))+ 
  geom_line(alpha=.05)+ scale_x_continuous()+ 
  ggtitle("Original Data")+ xlab("")+ theme_minimal() #changed from alpha=0.05 to .2 to make lines darker

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
write.csv(Lrmatrix, "E:\SDT\SDT RRPCA\RRPCA SDT\matrices\BF_01\\Lrmatrix_SDT_BF_01_300hz.csv",row.names=FALSE)
write.csv(Spmatrix, "E:\\SDT\\SDT RRPCA\\RRPCA SDT\\matrices\\300 hz matrices\\Spmatrix_SDT_BF_01_300hz.csv",row.names=FALSE)
write.csv(Omatrix, "E:\\SDT\\SDT RRPCA\\RRPCA SDT\\matrices\\300 hz matrices\\Omatrix_SDT_BF_01_300hz.csv",row.names=FALSE)


#############################

#saving time vectors

OMTT=nv[1]
Tmatrix=as.matrix(OMTT) #get the LR data (in decibels) into matrix form
write.csv(Tmatrix,"E:\\SDT\\SDT RRPCA\\RRPCA SDT\\matrices\\300 hz matrices\\OMTT.csv",row.names=FALSE)



##############################################################
#############################################################
#SDT BF_01 PSD Median Values, 0:1000 Hz (from df 10 data), 100 Hz, 20 min binning
#100 Hz now because 300 Hz was wacky

#upload data
setwd("E:\\SDT\\SDT RRPCA\\Soundscape Metrics SDT\\SDT_BF_01_Soundscape Metrics\\SDT_BF_01_\\100 Hz+20min_0-to-1000Hz")
SDTdata=as.data.frame(fread('SDT_BF_01_LTSA_1s_1Hz_PSD_20min.csv'))

#separate out one week for visualizations
weekhr=SDTdata[1658:2161,1:11] #Row 1658-2161 (8/20-8/26)
nv=weekhr #change to megan's language

#save as data frame to get values numeric, change column names to just frequency values, convert from dv to pressure
as.data.frame(colnames(nv))
hix  = as.numeric( gsub("PSD_","",names(nv)[3:11]) ) #changed to erase 100 Hz bin and start from 200 hz
Nv   = as.data.frame(as.matrix( ( nv[,3:11]) )) 
NvP  = as.data.frame(10^(Nv/20))     #converting from db to pressure-- "un-dbing"

#Plot original data
colnames(Nv) = hix #this makes the column names numeric rather than PSD_123
orgdata <- reshape :: melt(t(Nv)  ) #melt gets the data into a columns that ggplot recognizes 
pO = ggplot(orgdata, aes(X1, value, group = as.factor(X2)))+ 
  geom_line(alpha=.05)+ scale_x_continuous()+ 
  ggtitle("Original Data")+ xlab("")+ theme_minimal() #changed from alpha=0.05 to .2 to make lines darker
#this plot groups by time step. x axis is frequency but its logarithmic. 
#this is for everything not just the week....

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

#plot original, low rank, and sparse all together
grid.arrange(pO,pL,pS)


#create a spectrogram of low rank
Lrmatrix=as.matrix(LrDB) #get the LR data (in decibels) into matrix form
Lrspectro=image(Lrmatrix) #this should create a spectrogram-- not really sure what axes are here...but it looks like there are 7 choruses!!
Lrspectro+title(main="LR")

#now lets just see what image of sparse looks like
Spmatrix=as.matrix(SpDB)
Spmatrix[Spmatrix == -Inf] <- 0 #get rid of all the -inf values bc sparse matrix is screwed up
Spspectro=image(Spmatrix) #looks weird...
Spspectro+title(main="Sparse")

#now original
Omatrix=as.matrix(Nv)
Ospectro=image(Omatrix)+title(main="Original")