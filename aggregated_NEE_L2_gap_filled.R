
# This script reads AmeriFlux L2 gap filled files and calculates aggregated NEE for daily (g C/m2/day), monthly (g C/m2/day) and annual (g C/m2/year). 
# The L2 files should contain data for the whole year.
# Author: Francesc Montané

# modify working directory and "filepath"
# set working directory

setwd("D:/Sites_DOE/AmeriFlux/Morgan Monroe State Forest/L2_gap_filled")
filepath= "D:/Sites_DOE/AmeriFlux/Morgan Monroe State Forest/L2_gap_filled/"



# read all the .csv files in the working directory


tempFilelist = list.files(filepath,pattern="*.csv")


# get the AmeriFlux files information (site and years)
files<-substr(tempFilelist,5,14)

# get the AmeriFlux site code
site<-substr(files[1],3,5)

# combine all the files in one data frame

myfiles = do.call("rbind", lapply(paste0(filepath,tempFilelist), function(x) read.csv(x, skip=20,header=FALSE,stringsAsFactors = FALSE)))


AmFluxheader=read.csv(file = paste0(filepath,tempFilelist[1]),skip=17, strip.white=TRUE, nrows=1 ,header=FALSE, na.strings=c('-9999','-6999'),stringsAsFactors=FALSE)
colnames(myfiles)<-AmFluxheader

# replace missing values with NAs

temp <- (myfiles[,] == -9999 | myfiles[,] == -6999)
myfiles[temp]=NA




# aggregated NEE daily

year<-unique(myfiles$YEAR)

for (i in 1:length(year)){
assign(paste0("DOY_cum_",year[i]),subset(myfiles,YEAR==year[i],select=c(DOY)))
} 

testget<-mget(ls(pattern = "DOY_cum_*"), .GlobalEnv)
day_year<-seq(1:length(year))
for (i in 1:length(year)){

day_year[i]<-c(max(testget[[i]]))
} 

DOY_cumdays<-cumsum(day_year)




DOY_cumdays_prev<- c(0,DOY_cumdays[1:length(year)-1])

for (i in 1:length(testget)){

testget[[i]]<-testget[[i]] + DOY_cumdays_prev[i]
} 

data_frame<-do.call(rbind, lapply(testget, data.frame, stringsAsFactors=FALSE))

myfiles$DOY_cum<-data_frame[[1]]


# get if time step is hourly or half-hourly

time_step<-(myfiles$HRMIN[2]- myfiles$HRMIN[1])/100

# time step in hours (either 1 hour or 0.5 hours)

h_time_step<-ifelse(time_step==1,1,0.5)


# change units from umol/m2/s to gC/m2 in each time step of the tower data (1 hour time step=3600 seconds) 
# conversion factors: 1000000 umol CO2 = 1mol CO2; 1mol C = 1 mol CO2; 1 mol C = 12 g C)
 
NEE_gCm2<-data.frame(myfiles$NEE * h_time_step* 3600 * 12 / 1000000)
names(NEE_gCm2)<- c("NEE_gCm2")

myfiles<-cbind(myfiles,NEE_gCm2)

# calculate aggregated NEE in each day with units gC/m2/day

agg_NEE_day<-aggregate(myfiles$NEE_gCm2,by=list(myfiles$DOY_cum),sum)
names(agg_NEE_day)<-c("DOY_cum","agg_NEE_day")

# use subset to extract just one DOY value per day (condition HRMIN=0)
 
test_extract_day<-subset(myfiles$DOY,myfiles$HRMIN=="0")
test_extract_year<-subset(myfiles$YEAR,myfiles$HRMIN=="0")

agg_NEE_day<-cbind(test_extract_year,test_extract_day,agg_NEE_day)
names(agg_NEE_day)<-c("YEAR", "DOY", "DOY_cum","agg_NEE_day")

# create txt with agregated daily values, but first select the needed variables 

need_vars_d<-c("YEAR", "DOY", "agg_NEE_day")
data_daily<-agg_NEE_day[need_vars_d]

write.table(data_daily, paste(site,"L2gapfilled_NEE_daily.txt",sep="_"),col.names=TRUE,row.names=FALSE)



# aggregated NEE monthly

# get minimum and maximum for years in the data

year_min<-min(myfiles$YEAR)
year_max<-max(myfiles$YEAR)

# generate sequences of dates and extract months and years

seqdates<-seq(as.Date(paste(year_min,"-01-01",sep="")),as.Date(paste(year_max,"-12-31",sep="")),by="day")
seqdates_m<-format(seqdates, "%m")
seqdates_Y<-format(seqdates, "%Y")
seqdates_Y_m<-paste(seqdates_Y,seqdates_m,sep="_")


agg_NEE_day<-cbind(agg_NEE_day,seqdates_Y_m)
agg_NEE_month<-aggregate(agg_NEE_day$agg_NEE_day,by=list(agg_NEE_day$seqdates_Y_m),mean)
names(agg_NEE_month)<-c("month_class","agg_NEE_month")

year_monthly_month<-data.frame(rep(year_min:year_max,each=12))
names(year_monthly_month)<-c("year")

month_monthly_month<-data.frame(rep(1:12,length(day_year)))
names(month_monthly_month)<-c("month")

agg_NEE_month<-cbind(year_monthly_month,month_monthly_month,agg_NEE_month)


#  select only the needed variables to create a txt file with agregated NEE monthly values with units g C/m2/day

need_vars_m<-c("year", "month", "agg_NEE_month")
data_monthly<-agg_NEE_month[need_vars_m]

write.table(data_monthly, paste(site,"L2gapfilled_NEE_monthly.txt",sep="_"),col.names=TRUE,row.names=FALSE)


# aggregated NEE annual

agg_NEE_year<-aggregate(agg_NEE_month$agg_NEE_month,by=list(agg_NEE_month$year),mean)
names(agg_NEE_year)<-c("month_class","agg_NEE_year")


agg_NEE_year<-cbind(agg_NEE_year,day_year)


agg_NEE_year<-transform(agg_NEE_year, agg_NEE_year_units = agg_NEE_year * day_year)


years_years<-data.frame(c(year_min:year_max))
names(years_years)<-c("year")

agg_NEE_year<-cbind(years_years,agg_NEE_year)

#  select only the needed variables to create a txt file with agregated NEE annual values with units g C/m2/year

need_vars_y<-c("year", "agg_NEE_year_units")
data_annual<-agg_NEE_year[need_vars_y]
names(data_annual)<-c("year","agg_NEE_year")


write.table(data_annual, paste(site,"L2gapfilled_NEE_annual.txt",sep="_"),col.names=TRUE,row.names=FALSE)



