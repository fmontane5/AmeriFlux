
# This script reads AmeriFlux L2 files (*.csv), gets the time step in the L2 data (either half-hourly or hourly) and it creates an input file 
# for REddyProc package (*.txt) with the appropriate time step (either half-hourly or hourly) and format.  
# For additional information about the format of a REddyProc input file see example available in: 
# https://www.bgc-jena.mpg.de/bgi/uploads/Services/REddyProcWebDataFormat/Example_DETha98.txt
# Author: Francesc Montane
# Contact: fmontane@email.arizona.edu

######################################################################################################################################
#####   IMPORTANT NOTES BEFORE RUNNING THE SCRIPT                                                                                 ####
#####                                                                                                                             ####
#####   1- Make sure that only the required *.csv L2 files are placed in the working directory (wd)                               ####
#####   2- Make sure that the L2 files in the wd are only for one site                                                            ####
#####   3- Make sure that the L2 files in the wd are only of one type (either gap filled or with gaps, but not both)              ####
#####   4- Make sure that each L2 file in the wd contains data for the whole year (starting at DOY=1)                             ####
#####   5- Delete L2 files in the wd that do not start at DOY=1, if any (for instance, L2 files for NR1 and year 1998)            ####
#####   6- To create a REddyProc input file with one year only, place only one L2 *csv file for that particular year in the wd    ####
#####   7 -To create a REddyProc input file with several years, place the L2 *csv files for all the years in the wd               ####          
#####                                                                                                                             ####
######################################################################################################################################

# modify working directory and "filepath"


setwd("D:/Sites_DOE/AmeriFlux/Niwot Ridge/L2_gap_filled/V008")
filepath="D:/Sites_DOE/AmeriFlux/Niwot Ridge/L2_gap_filled/V008/"


# read all the *.csv files in the working directory


tempFilelist = list.files(filepath,pattern="*.csv")

# get the AmeriFlux files information (site and years)
files<-substr(tempFilelist,7,14)

# get the AmeriFlux site code
site<-substr(tempFilelist[1],7,9)

#get the data and header from the L2 files

myfiles = do.call("rbind", lapply(paste0(filepath,tempFilelist), function(x) read.csv(x, skip=20,header=FALSE,stringsAsFactors = FALSE)))

AmFluxheader=read.csv(tempFilelist[1],skip=17, strip.white=TRUE, nrows=1 ,header=FALSE,stringsAsFactors=FALSE)

colnames(myfiles)<-AmFluxheader

# replace -6999 with -9999 (REddyProc only replaces -9999 with NAs)

temp <- (myfiles[,] == -6999)
myfiles[temp]=-9999

# get if time step in L2 files is hourly or half-hourly

time_step<-(myfiles$HRMIN[2]- myfiles$HRMIN[1])/100

# time step in hours (either 1 hour or 0.5 hours)

h_time_step<-ifelse(time_step==1,1,0.5)

# get the number of time steps per day

n_steps_day<-ifelse(time_step==1,24,48)

#generate sequences of time in REddyProc format (variable "Hour") for one day and for both hourly and half hourly L2 data
# the last element of the sequence of time should be equal to 0
seqday_half<-seq(0.5,24,by=0.5)
seqday_half[48]<-c(0)
seqday_hour<-seq(1,24,by=1)
seqday_hour[24]<-c(0)

# use the correct sequence of time for one day depending on the time step in the L2 files
if (h_time_step==0.5){
seqday_use<-seqday_half
} else{
seqday_use<-seqday_hour
}

# generate annual sequences of times, repeating the day sequences for non leap years and leap years
seqday_use_365<-rep(seqday_use,365)
seqday_use_366<-rep(seqday_use,366)

# get the years from the L2 files

year<-unique(myfiles$YEAR)
year_ini<-min(year)
year_end<-max(year)

# extract the variable DOY for each year
for (i in 1:length(year)){
assign(paste0("DOY_",year[i]),subset(myfiles,YEAR==year[i],select=c(DOY)))
} 

# get the number of days for each year

testget<-mget(ls(pattern = "DOY_*"), .GlobalEnv)
day_year<-seq(1:length(year))
for (i in 1:length(year)){

day_year[i]<-c(max(testget[[i]]))
} 

for (i in 1:length(day_year)){
assign(paste0("daystot_",year[i]),day_year[i])

} 

#generate sequences for the variables Hour and DoY in REddyProc format
# the variable Hour in REddyProc format takes as reference time the final time of the 30 min or 1 hour L2 time step
# (L2 files take as reference time the initial time of the 30 min or 1 hour time step)
# the variable DoY in REddyProc format should be modified accordingly to Hour

for (i in 1:length(year)){
assign(paste0("data_",year[i]),subset(myfiles,YEAR==year[i]))
if (day_year[i]==365) {
seqday_year<-seqday_use_365
seqday_length<-length(seqday_use_365)
} else {
seqday_year<-seqday_use_366
seqday_length<-length(seqday_use_366)
}
assign(paste0("SeqDay_",year[i]),seqday_year)

day_tot<-day_year[i]
add_day<-day_year[i]+1
DoYrest_year<-rep(seq(1,day_tot),each=n_steps_day)
DoYrest_year<-DoYrest_year[-1]
DoYrest_year[seqday_length]<-c(add_day)
assign(paste0("SeqDoY_",year[i]),DoYrest_year)
} 

# get the sequences of Hour and DoY in REddyProc format in a list

testget_seq<-mget(ls(pattern = "SeqDay_*"), .GlobalEnv)
testget_doy<-mget(ls(pattern = "SeqDoY_*"), .GlobalEnv)


# get the sequences of Hour and DoY in REddyProc format from the list to a data frame

testdf_seq1<-data.frame(
         lNames = rep(names(testget_seq), lapply(testget_seq, length)),
         lVal = unlist(testget_seq))

		 
testdf_seq2<-data.frame(
         lNames = rep(names(testget_doy), lapply(testget_doy, length)),
         lVal = unlist(testget_doy))

#add the sequences of Hour and DoY in REddyProc format to the data frame with the L2 data

myfiles$hour_eddy<-testdf_seq1$lVal
myfiles$doy_eddy<-testdf_seq2$lVal

# IMPORTANT: change VPD units from kPa (VPD units in L2 files) to hPa (VPD units in REddyProc), 
# without taking possible missing values (-9999) in the data

myfiles<-transform(myfiles,VPD=ifelse(VPD>=0,VPD*10,-9999))
		 
# Select needed variables for REddyProc

need_vars<-c("YEAR", "doy_eddy", "hour_eddy", "NEE", "LE", "H", "Rg", "TA", "TS1", "RH","VPD", "UST")
myfiles2<-myfiles[need_vars]
colnames(myfiles2)<-c("Year", "DoY", "Hour", "NEE", "LE", "H", "Rg", "Tair", "Tsoil", "rH", "VPD", "Ustar")


# insert a second row (header) with units (format required for REddyProc)

units<-c("-", "-", "-",	"umolm-2s-1", "Wm-2", "Wm-2",	"Wm-2", "degC", "degC",	"%", "hPa",	"ms-1")
myfiles3<-rbind(units,myfiles2)

write.table(myfiles3, paste(site,year_ini,year_end,"eddy.txt",sep="_"),sep=" ",col.names=TRUE,row.names=FALSE,quote=FALSE)




