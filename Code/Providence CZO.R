library(dplyr)
library(lubridate)
library(ggplot2)
library(plotly)
library(zoo)
library(plyr)
library(reshape)
#UpMetF
#temp names: 1: open..deg.C.   open10_VWC   '1/1/2008 0:00
#           7: air_temp_open.Deg.C.  soil_VWC_open10     2014-10-01 00:00:00

#UpMetS
#         1:  Air_temp_Open..deg.C    VWC.Open30 VWC.Qkde10   1/1/2010 0:00 
#         2:   Air_temp_Open..deg.C   1/1/2011     VWC.Open10
#         5:  same as 6
#         6:      2014-10-01 00:00:00 soil_VWC_open10     air_temp_open.Deg.C.
#         7:  air_temp_open.Deg.C.    VWC_open10                        2015-10-01 06:00:00

ReadTable <- function(location, aspect) {
  files<-list.files(path = paste('/Users/katya/Google Drive/PhD/Soil Moisture/Southern Sierra CZO/Providence/',location,'/',aspect,'/', sep=""), full.names = TRUE, recursive = TRUE, pattern=glob2rx(paste("*",'soil_snow_hourly',"*.csv*", sep='')))
  for (i in c(1:length(files))){
  print(i)  
  tmp<-read.csv(files[i], header=T)
  Date<-as.character(levels(tmp$date_time))[tmp$date_time]
    if (!is.na(strptime(Date[1], "%Y-%m-%d %H:%M:%S", tz="GMT"))){
    Date<-as.character(levels(tmp$date_time))[tmp$date_time]
    Date<-strptime(Date, "%Y-%m-%d %H:%M:%S", tz="GMT")
    print('True')
  }
  if (!is.na(strptime(Date[1], "%m/%d/%Y %H:%M", tz="GMT"))){
  Date<-as.character(levels(tmp$date_time))[tmp$date_time]
  Date<-strptime(Date, "%m/%d/%Y %H:%M", tz="GMT")
  print('true')
  }
  SMtmp<-as.data.frame(as.POSIXct(Date))
  names(SMtmp)<-'Date'
  ind<-which(names(tmp)=="Temp.Open10.deg.C"  | names(tmp)=="open10_Temp.avg." | names(tmp)=="soil_temp_open10.Deg.C."  )
  SMtmp$SoilT<-unlist(tmp[ind])
  ind<-which(names(tmp)=="depth_Open.cm"  | names(tmp)=="open..cm." | names(tmp)=="snow_depth_Open.cm" | names(tmp)=="snow_open.cm." |names(tmp)=="snow_depth_open.cm.")
  SMtmp$SnowD<-unlist(tmp[ind])
  
  if(length(tmp$VWC_open10)!=0){SMtmp$VWC10<-tmp$VWC_open10
  print('yes0')}
  else if(length(tmp$soil_VWC_open10)!=0){SMtmp$VWC10<-tmp$soil_VWC_open10
  print('yes1')}
  else if(length(tmp$VWC.Open10)!=0){SMtmp$VWC10<-tmp$VWC.Open10
  print('yes2')}
  else if(length(tmp$open10_VWC)!=0){SMtmp$VWC10<-tmp$open10_VWC
  print('yes2')}
  else {SMtmp$VWC10<-tmp$Day*0-1
  print('no VWC10')}
  
  if(length(tmp$VWC_open30)!=0){SMtmp$VWC30<-tmp$VWC_open10
  print('yes0')}
  else if(length(tmp$soil_VWC_open30)!=0){SMtmp$VWC30<-tmp$soil_VWC_open30
  print('yes1')}
  else if(length(tmp$VWC.Open30)!=0){SMtmp$VWC30<-tmp$VWC.Open30
  print('yes2')}
  else if(length(tmp$open30_VWC)!=0){SMtmp$VWC30<-tmp$open30_VWC
  print('yes2')}
  else {SMtmp$VWC30<-tmp$Day*0-1
  print('no VWC30')}
  
  if(length(tmp$VWC_open60)!=0){SMtmp$VWC60<-tmp$VWC_open10
  print('yes0')}
  else if(length(tmp$soil_VWC_open60)!=0){SMtmp$VWC60<-tmp$soil_VWC_open60
  print('yes1')}
  else if(length(tmp$VWC.Open60)!=0){SMtmp$VWC60<-tmp$VWC.Open60
  print('yes2')}
  else if(length(tmp$open60_VWC)!=0){SMtmp$VWC60<-tmp$open60_VWC
  print('yes3')}
  else {SMtmp$VWC60<-tmp$Day*0-1
  print('no VWC60')}
  
  if(length(tmp$air_temp_open.Deg.C)!=0){SMtmp$AirT<-tmp$air_temp_open.Deg.C
  print('yes0')}
  else if(length(tmp$open..deg.C.)!=0){SMtmp$AirT<-tmp$open..deg.C.
  print('yes1')}
  else if(length(tmp$Air_temp_Open..deg.C)!=0){SMtmp$AirT<-tmp$Air_temp_Open..deg.C
  print('yes2')}
  else {SMtmp$AirT<-tmp$Day*0-999
  print('no Tair')}
  
  print(length(SMtmp$VWC10))

  #SMtmp$AirT<-tmp[,58]
  if (i==1){SM<-SMtmp}
  if (i!=1){SM<-bind_rows(SM, SMtmp)}
  }
  SM$VWC10[SM$VWC10==-999]<-NA
  SM$VWC30[SM$VWC30==-999]<-NA
  SM$VWC60[SM$VWC60==-999]<-NA
  SM$AirT[SM$AirT==-999]<-NA
  return(SM)
  
}
LNorth2008<-read.csv('/Users/katya/Google Drive/PhD/Soil Moisture/Southern Sierra CZO/Providence/Lower Met/WY_2008_LowMetN_Soil_Snow_hourly.csv',header=T)
UFlat2008<-read.csv('/Users/katya/Google Drive/PhD/Soil Moisture/Southern Sierra CZO/Providence/Upper Met/Flat/WY_2008_UpMetF_soil_snow_hourly.csv', header=T)
UFlat2008$Date<-as.character(levels(UFlat2008$date_time))[UFlat2008$date_time]

UFlat2008$open..deg.C.[UFlat2008$open..deg.C.==-999]<-NaN
UFlat2008$open10_Temp.avg.[UFlat2008$open10_Temp.avg.==-999]<-NaN
UFlat2008$acde10_Temp.avg.[UFlat2008$acde10_Temp.avg.==-999]<-NaN
LNorth2008$open..deg.C.[LNorth2008$open..deg.C.==-999]<-NaN
LNorth2008$open10_Temp..avg.C.[LNorth2008$open10_Temp..avg.C.==-999]<-NaN
plot(UFlat2008$open..deg.C., type='l')
lines(UFlat2008$open10_Temp.avg., type='l', col='red')
lines(LNorth2008$open10_Temp..avg.C., col='blue')

UFlat2009<-read.csv('/Users/katya/Google Drive/PhD/Soil Moisture/Southern Sierra CZO/Providence/Upper Met/Flat/WY_2009_UpMetF_soil_snow_hourly.csv', header=T)
UFlat2009$Date<-as.character(levels(UFlat2009$date_time))[UFlat2009$date_time]

UFlat2009$open..deg.C.[UFlat2009$open..deg.C.==-999]<-NaN
UFlat2009$open10_Temp.avg.[UFlat2009$open10_Temp.avg.==-999]<-NaN
plot(UFlat2009$open..deg.C., type='l')
lines(UFlat2009$open10_Temp.avg., type='l', col='red')
lines(SMFlat$AirT, col='blue')

UFlat2010<-read.csv('/Users/katya/Google Drive/PhD/Soil Moisture/Southern Sierra CZO/Providence/Upper Met/Flat/WY_2010_UpMetF_soil_snow_hourly.csv', header=T)
UFlat2010$Date<-as.character(levels(UFlat2010$date_time))[UFlat2010$date_time]
UFlat2010$Date<-strptime(UFlat2010$Date, "%m/%d/%Y %H:%M", tz="GMT")
LNorth2010<-read.csv('/Users/katya/Google Drive/PhD/Soil Moisture/Southern Sierra CZO/Providence/Lower Met/WY_2010_LowMetN_Soil_Snow_hourly.csv',header=T)
LNorth2010$Air_temp_Open..deg.C[LNorth2010$Air_temp_Open..deg.C==-999]<-NaN
LNorth2010$Temp.Open10.deg.C[LNorth2010$Temp.Open10.deg.C==-999]<-NaN

UFlat2010$Temp.Open10.deg.C[UFlat2010$Temp.Open10.deg.C==-999]<-NaN
UFlat2010$Air_temp_Open..deg.C[UFlat2010$Temp.Open10.deg.C==-999]<-NaN
plot(UFlat2010$Date,UFlat2010$Air_temp_Open..deg.C, type='l', xlab='2010',)
lines(UFlat2010$Date,UFlat2010$Temp.Open10.deg.C, type='l', col='red')
lines(SMFlat$Date, SMFlat$VWC10*100, col='blue')
points(UFlat2010$Date, UFlat2010$depth_Open.cm, col='gray')

p<-ggplot()+geom_line(aes(x=as.POSIXct(UFlat2010$Date), y=UFlat2010$depth_Open.cm))+
  geom_line(aes(x=as.POSIXct(UFlat2010$Date), y=UFlat2010$Temp.Open10.deg.C, col='red'))
ggplotly(p)

plot(LNorth2010$Air_temp_Open..deg.C, type='l')
lines(LNorth2010$Temp.Open10.deg.C, type='l', col='red')

UFlat2011<-read.csv('/Users/katya/Google Drive/PhD/Soil Moisture/Southern Sierra CZO/Providence/Upper Met/Flat/WY_2011_UpMetF_soil_snow_hourly.csv', header=T)
UFlat2011$Date<-as.character(levels(UFlat2011$date_time))[UFlat2011$date_time]

UFlat2011$Temp.Open10.deg.C[UFlat2011$Temp.Open10.deg.C==-999]<-NaN
UFlat2011$Air_temp_Open..deg.C[UFlat2011$Temp.Open10.deg.C==-999]<-NaN
plot(UFlat2011$Air_temp_Open..deg.C, type='l')
lines(UFlat2011$Temp.Open10.deg.C, type='l', col='red')

UFlat2012<-read.csv('/Users/katya/Google Drive/PhD/Soil Moisture/Southern Sierra CZO/Providence/Upper Met/Flat/WY_2012_UpMetF_soil_snow_hourly.csv', header=T)
UFlat2012$Date<-as.character(levels(UFlat2012$date_time))[UFlat2012$date_time]
UFlat2012$Date<-strptime(UFlat2012$Date, "%d/%m/%Y %H:%M", tz="GMT")
UFlat2012$soil_temp_open10.Deg.C.[UFlat2012$soil_temp_open10.Deg.C.==-999]<-NaN
UFlat2012$air_temp_open.Deg.C.[UFlat2012$air_temp_open.Deg.C.==-999]<-NaN
plot(UFlat2012$air_temp_open.Deg.C., type='l')
lines(UFlat2012$soil_temp_open10.Deg.C., type='l', col='red')
p<-ggplot()+geom_line(aes(x=as.POSIXct(UFlat2013$Date), y=UFlat2013$snow_open.cm.))+
  geom_line(aes(x=as.POSIXct(UFlat2013$Date), y=UFlat2013$soil_temp_open10.Deg.C., col='red'))
ggplotly(p)

UFlat2013<-read.csv('/Users/katya/Google Drive/PhD/Soil Moisture/Southern Sierra CZO/Providence/Upper Met/Flat/WY_2013_UpMetF_soil_snow_hourly.csv', header=T)
UFlat2013$Date<-as.character(levels(UFlat2013$date_time))[UFlat2013$date_time]
UFlat2013$Date<-strptime(UFlat2013$Date, "%d/%m/%Y %H:%M", tz="GMT")
UFlat2013$soil_temp_open10.Deg.C.[UFlat2013$soil_temp_open10.Deg.C.==-999]<-NaN
UFlat2013$air_temp_open.Deg.C.[UFlat2013$air_temp_open.Deg.C.==-999]<-NaN
plot(UFlat2013$air_temp_open.Deg.C., type='l')
lines(UFlat2013$soil_temp_open10.Deg.C., type='l', col='red')

p<-ggplot()+geom_line(aes(x=as.POSIXct(UFlat2013$Date), y=UFlat2013$snow_open.cm.))+
  geom_line(aes(x=as.POSIXct(UFlat2013$Date), y=UFlat2013$soil_temp_open10.Deg.C., col='red'))
ggplotly(p)

UFlat2014<-read.csv('/Users/katya/Google Drive/PhD/Soil Moisture/Southern Sierra CZO/Providence/Upper Met/Flat/WY_2014_UpMetF_soil_snow_hourly.csv', header=T)
UFlat2014$Date<-as.character(levels(UFlat2014$date_time))[UFlat2014$date_time]
UFlat2014$Date<-strptime(UFlat2014$Date, "%Y-%m-%d %H:%M", tz="GMT")
UFlat2014$soil_temp_open10.Deg.C.[UFlat2014$soil_temp_open10.Deg.C.==-999]<-NaN
UFlat2014$air_temp_open.Deg.C.[UFlat2014$air_temp_open.Deg.C.==-999]<-NaN
plot(UFlat2014$air_temp_open.Deg.C., type='l')
lines(UFlat2014$soil_temp_open10.Deg.C., type='l', col='red')

p<-ggplot()+geom_line(aes(x=as.POSIXct(UFlat2014$Date), y=UFlat2014$snow_depth_open.cm.))+
  geom_line(aes(x=as.POSIXct(UFlat2014$Date), y=UFlat2014$soil_temp_open10.Deg.C., col='red'))
ggplotly(p)

UFlat2015<-read.csv('/Users/katya/Google Drive/PhD/Soil Moisture/Southern Sierra CZO/Providence/Upper Met/Flat/WY_2015_UpMetF_soil_snow_hourly.csv', header=T)
UFlat2015$Date<-as.character(levels(UFlat2015$date_time))[UFlat2015$date_time]
UFlat2015$Date<-strptime(UFlat2015$Date, "%Y-%m-%d %H:%M", tz="GMT")
UFlat2015$soil_temp_open10.Deg.C.[UFlat2015$soil_temp_open10.Deg.C.==-999]<-NaN
UFlat2015$air_temp_open.Deg.C.[UFlat2015$air_temp_open.Deg.C.==-999]<-NaN
plot(UFlat2015$air_temp_open.Deg.C., type='l')
lines(UFlat2015$soil_temp_open10.Deg.C., type='l', col='red')

p<-ggplot()+geom_line(aes(x=as.POSIXct(UFlat2015$Date), y=UFlat2015$snow_depth_open.cm.))+
  geom_line(aes(x=as.POSIXct(UFlat2015$Date), y=UFlat2015$soil_temp_open10.Deg.C., col='red'))
ggplotly(p)

UFlat2016<-read.csv('/Users/katya/Google Drive/PhD/Soil Moisture/Southern Sierra CZO/Providence/Upper Met/North/WY_2016_UpMetN_soil_snow_hourly.csv', header=T)
UFlat2016$Date<-as.character(levels(UFlat2016$date_time))[UFlat2016$date_time]

UFlat2016$soil_temp_open10.Deg.C.[UFlat2016$soil_temp_open10.Deg.C.==-999]<-NaN
UFlat2016$air_temp_open.Deg.C.[UFlat2016$air_temp_open.Deg.C.==-999]<-NaN
plot(UFlat2016$air_temp_open.Deg.C., type='l')
lines(UFlat2016$soil_temp_open10.Deg.C., type='l', col='red')

UFlat2017<-read.csv('/Users/katya/Google Drive/PhD/Soil Moisture/Southern Sierra CZO/Providence/Upper Met/Flat/WY_2017_UpMetF_soil_snow_hourly.csv', header=T)
UFlat2017$Date<-as.character(levels(UFlat2017$date_time))[UFlat2017$date_time]

UFlat2017$soil_temp_open10.Deg.C.[UFlat2017$soil_temp_open10.Deg.C.==-999]<-NaN
UFlat2017$air_temp_open.Deg.C.[UFlat2017$air_temp_open.Deg.C.==-999]<-NaN
plot(UFlat2017$air_temp_open.Deg.C., type='l')
lines(UFlat2017$soil_temp_open10.Deg.C., type='l', col='red')

Clim2009<-read.csv('/Users/katya/Downloads/gap_filled_upper_prov_hourly_wy2008.csv', header=T)
Date<-as.character(levels(Clim2009$date_time))[Clim2009$date_time]
Date<-strptime(Date, "%m/%d/%Y %H:%M", tz="GMT")
Clim2009$Date<-Date
Clim2009$hour<-hour(Clim2009$Date)
Clim2009$day<-day(Clim2009$Date)
Clim2009$year<-year(Clim2009$Date)
Clim2009$month<-month(Clim2009$Date)
Clim2009$wy<-ifelse((Clim2009$month >= 10), Clim2009$year+1, Clim2009$year)
Clim2009$RH<-Clim2009$HMP_RH....

ClimLower<-read.csv('/Users/katya/Downloads/RDS-2018-0028/Data/LOWER_PROVIDENCE_15MIN_CLIMATOLOGY.csv', header=T)
ClimLower$RELATIVE_HUMIDITY[ClimLower$RELATIVE_HUMIDITY==-9999]<-NA
Date<-as.character(levels(ClimLower$DATE_TIME))[ClimLower$DATE_TIME]
Date<-strptime(Date, "%m/%d/%y %H:%M", tz="GMT")
ClimLower$Date<-Date
ClimLower$hour<-hour(ClimLower$Date)
ClimLower$day<-day(ClimLower$Date)
ClimLower$year<-year(ClimLower$Date)
ClimLower$month<-month(ClimLower$Date)
ClimLower$wy<-ifelse((ClimLower$month >= 10), ClimLower$year+1, ClimLower$year)

Clim<-read.csv('/Users/katya/Google Drive/PhD/Soil Moisture/Southern Sierra CZO/Providence/Upper Met/WeatherStationRecord/Data/UPPER_PROVIDENCE_15MIN_CLIMATOLOGY.csv', header=T)
Clim$AIR_TEMPERATURE[Clim$AIR_TEMPERATURE==-9999]<-NA
Clim$RELATIVE_HUMIDITY[Clim$RELATIVE_HUMIDITY==-9999]<-NA
Clim$WIND_SPEED[Clim$WIND_SPEED==-9999]<-NA
Date<-as.character(levels(Clim$DATE_TIME))[Clim$DATE_TIME]
Date<-strptime(Date, "%m/%d/%y %H:%M", tz="GMT")
Clim$Date<-Date
Clim$hour<-hour(Clim$Date)
Clim$day<-day(Clim$Date)
Clim$year<-year(Clim$Date)
Clim$month<-month(Clim$Date)
Clim$wy<-ifelse((Clim$month >= 10), Clim$year+1, Clim$year)
plot(Clim$Date,Clim$AIR_TEMPERATURE, type='l')
tmp<-aggregate(Clim$AIR_TEMPERATURE, by=list(Clim$hour,Clim$day, Clim$month, Clim$year), FUN=mean)
tmp2<-aggregate(Clim$RELATIVE_HUMIDITY, by=list(Clim$hour,Clim$day, Clim$month, Clim$year), FUN=mean)
tmp4<-aggregate(ClimLower$AIR_TEMPERATURE, by=list(ClimLower$hour,ClimLower$day, ClimLower$month, ClimLower$year), FUN=mean)
tmp3<-aggregate(ClimLower$RELATIVE_HUMIDITY, by=list(ClimLower$hour,ClimLower$day, ClimLower$month, ClimLower$year), FUN=mean)
tmp5<-aggregate(Clim$WIND_SPEED, by=list(Clim$hour,Clim$day, Clim$month, Clim$year), FUN=mean)

tmp3$Upper<-tmp2$x
fit <- lm(tmp3$Upper~tmp3$x)

summary(fit)

pred<-predict(fit, tmp3)
tmp3$Upper[is.na(tmp3$Upper)]<-pred[is.na(tmp3$Upper)]


LM<-lm(tmp3$x~tmp4)
out<-seq(as.POSIXct("2002-10-01", tz = "UTC"),
    as.POSIXct("2017-10-01", tz = "UTC"),
    by = "60 min")

out<-seq(as.POSIXct("2002-10-01", tz = "UTC"),
         as.POSIXct("2017-10-01", tz = "UTC"),
         by = "60 min")


ClimHr<-as.data.frame(tmp$x)
names(ClimHr)<-'MeanTemp'
ClimHr$RH<-tmp3$Upper
ClimHr$Wind<-tmp5$x
ClimHr$RH1<-c(tmp3$Upper[1],tmp3$Upper[1:(length(tmp3$Upper)-1)])
ClimHr$RH10<-c(tmp3$Upper[1:10],tmp3$Upper[1:(length(tmp3$Upper)-10)])
ClimHr$Date<-out
plot(ClimHr$Date,ClimHr$MeanTemp, type='l')
points(ClimHr$Date[ClimHr$MeanTemp<0],ClimHr$MeanTemp[ClimHr$MeanTemp<0], col='red')
out<-approx(ClimHr$MeanTemp, xout=ClimHr$MeanTemp)

ClimRain<-read.csv('/Users/katya/Google Drive/PhD/Soil Moisture/Southern Sierra CZO/Providence/Upper Met/WeatherStationRecord/Data/UPPER_PROVIDENCE_DAILY_CLIMATOLOGY.csv', header=T)
ClimRain$PRECIPITATION[ClimRain$PRECIPITATION==-9999]<-NA
ClimRain$Date<-as.character(levels(ClimRain$Date))[ClimRain$Date]
ClimRain$Date<-strptime(ClimRain$Date, "%m/%d/%y", tz="GMT")

location='Upper Met'
aspect='South'

SMFlat<-ReadTable(location,'Flat')
SMNorth<-ReadTable(location,'North')
SMSouth<-ReadTable(location,'South')


p<-ggplot()+geom_line(aes(x=as.POSIXct(SMFlat$Date), y=SMFlat$SnowD))+
  geom_line(aes(x=as.POSIXct(SMFlat$Date), y=SMFlat$SoilT, col='red'))
ggplotly(p)

df<-merge(x=SMFlat,y=SMNorth,by="Date", all=T)  #.x=flat, .y=North, =south
df<-merge(x=df, y=SMSouth, by='Date', all=T)
SMmerged<-merge(x=df, y=ClimHr, by='Date', all.x=TRUE)

for (i in c(1:length(SMmerged$Date))){
  if (is.na(SMmerged$AirT.x[i]) & !is.na(SMmerged$AirT.y[i])){
    SMmerged$AirT.x[i]<-SMmerged$AirT.y[i]
  }
  else if (is.na(SMmerged$AirT.x[i]) & !is.na(SMmerged$AirT[i])){
    SMmerged$AirT.x[i]<-SMmerged$AirT[i]
  }
  else if (is.na(SMmerged$AirT.x[i]) & !is.na(SMmerged$MeanTemp[i])){
    SMmerged$AirT.x[i]<-SMmerged$MeanTemp[i]
  }
}

SMmerged$TAirInterp<-na.approx(SMmerged$AirT.x)
SMmerged$Temp1<-c(SMmerged$TAirInterp[1],SMmerged$TAirInterp[1:(length(SMmerged$TAirInterp)-1)])

plot(SMFlat$Date, SMFlat$VWC10, type='l', col='brown')
lines(SMSouth$Date, SMSouth$VWC10, col='red')
lines(SMNorth$Date, SMNorth$VWC10, col='blue')

library(rts)
ends <- endpoints(SMmerged$Date,'hours',6) 
out<-period.apply(SMmerged$TAirInterp,ends ,mean)

out<-as.data.frame(out)
names(out)<-'Tair6hr'
out$Date<-SMmerged$Date[ends]

Freeze<-out$Tair6hr*-999
Thaw<-out$Tair6hr*-999
for (i in c(2:(length(out$Tair6hr)-1))){

  if ((out$Tair6hr[i]>=0 & out$Tair6hr[i+1]>0 & out$Tair6hr[i-1]<0 )){
    Thaw[i]<-0
}
if ((out$Tair6hr[i]<=0 &out$Tair6hr[i+1]<0 & out$Tair6hr[i-1]>0)){
  Freeze[i]<-0}
  if ((out$Tair6hr[i]>=0 &out$Tair6hr[i+1]<0 & out$Tair6hr[i-1]<0)){
    Thaw[i]<-0}
  if ((out$Tair6hr[i]<=0 &out$Tair6hr[i+1]>0 & out$Tair6hr[i-1]>0)){
    Freeze[i]<-0}
}

length(out$Tair6hr[Freeze==0])


  
FreezeTime<-difftime(out$Date[Thaw==0],out$Date[Freeze==0], units="hours")
ThawTime<-c(difftime(out$Date[Freeze==0][2:716],out$Date[Thaw==0][1:715], units="hours"),0)


ThawInd<-Thaw
ThawInd[Thaw==0]<-ThawTime
ThawInd[Thaw!=0]<-NA
FreezeInd<-Freeze
FreezeInd[Freeze==0]<-FreezeTime
FreezeInd[Freeze!=0]<-NA
Thaw[Thaw!=0]<-NA
Freeze[Freeze!=0]<-NA

out$Freeze<-Freeze
out$Freeze[Freeze==0]<-1
out$Thaw<-Thaw
out$Thaw[Thaw==0]<-1



#Thaw[ThawInd<=6]<-3
#Freeze[FreezeInd<=6]<-3
#Thaw[Thaw==0 & FreezeTime<=6]<-3
#Freeze[Freeze==0 & ThawTime<=6]<-3

#SMmerged$F<-SMmerged$TAirInterp*0
#SMmerged$T<-SMmerged$TAirInterp*0
#SMmerged$F[Freeze==0]<-1
#SMmerged$T[Thaw==0]<-1

r<-ggplot()+geom_line(aes(out$Date, out$Tair6hr))+
  geom_point(aes(out$Date, Freeze), color='blue')+
  geom_point(aes(out$Date, Thaw), color='green')+
  geom_text(aes(x=out$Date, y=Thaw,label=as.numeric(ThawInd)),hjust=0, vjust=0)+
  geom_text(aes(x=out$Date, y=Freeze,label=as.numeric(FreezeInd)),hjust=0, vjust=0)
ggplotly(r)



VanGenuchten<-function(ThetaR, ThetaS, alpha, n, depth, SM){
  theta<-SM*0
  SoilRetention<-((ThetaS-ThetaR)/(1+(alpha*h)^n)^m)+ThetaR
  h<-seq(0,800,0.01)
  m=1-1/n
  for (i in c(1:length(SM))){
  
  

  #h_ind<-h[which(round(SoilRetention,4)==SM[i])][1]
  h_ind<-((((ThetaS-ThetaR)/(SM[i]-ThetaR))^(1/m)-1)^(1/n))/alpha
  h_ind-depth
  theta[i]<-((ThetaS-ThetaR)/(1+(alpha*(h_ind-depth))^n)^m)+ThetaR
  }
  return(theta)
}

ThetaR<-0.0391094
ThetaS<-0.4215058
alpha<-10^(-1.459)
n<-10^(0.242)
depth<-10

# ThetaR<-0.039
# ThetaS<-0.387
# alpha<-10^(-1.574)
# n<-10^(0.161)
# depth<-10

surfaceSM<-VanGenuchten(ThetaR, ThetaS, alpha, n, depth, SMmerged$VWC10.x)




SM=0.06
Dry[is.na(Dry)]
Dry<-SMmerged$VWC10.x*0-999
Wet<-SMmerged$VWC10.x*0-999

library(pracma)
tmp<-smooth.spline(SMmerged$VWC10.x, spar=0.0005, nknots=14250) #3562 for every day , 1100 every 3.2 days, 7124.917 every 12 hrs
SMmerged$smooth<-c(tmp$y)
tmp<-movavg(SMmerged$VWC10.x, 24)
SMmerged$smooth<-c(tmp)
for (i in c(2:(length(SMmerged$smooth)-1))){
  
  if ((SMmerged$smooth[i]>=SM & SMmerged$smooth[i+1]>SM & SMmerged$smooth[i-1]<SM )){
    Wet[i]<-0.06
  }
  if ((SMmerged$smooth[i]<=SM &SMmerged$smooth[i+1]<SM & SMmerged$smooth[i-1]>SM)){
    Dry[i]<-0.06}
  if ((SMmerged$smooth[i]>=SM &SMmerged$smooth[i+1]<SM & SMmerged$smooth[i-1]<SM)){
    Wet[i]<-0.06}
  if ((SMmerged$smooth[i]<=SM &SMmerged$smooth[i+1]>SM & SMmerged$smooth[i-1]>SM)){
    Dry[i]<-0.06}
}

length(SMmerged$Date[Dry==0.06])
length(SMmerged$Date[Wet==0.06])


DryTime<-c(difftime(SMmerged$Date[Wet==0.06][1:13],SMmerged$Date[Dry==0.06][1:13], units="hours"),0)
WetTime<-c(difftime(SMmerged$Date[Dry==0.06][2:14],SMmerged$Date[Wet==0.06][1:13], units="hours"))


WetInd<-Wet
WetInd[Wet==0.06]<-WetTime
WetInd[Wet!=0.06]<-NA
DryInd<-Dry
DryInd[Dry==0.06]<-DryTime
DryInd[Dry!=0.06]<-NA
Wet[Wet!=0.06]<-NA
Dry[Dry!=0.06]<-NA

SMmerged$W<-SMmerged$VWC10.x*0
SMmerged$D<-SMmerged$VWC10.x*0
SMmerged$W2<-SMmerged$VWC10.x*0

SMmerged$W[Wet==0.06]<-1
SMmerged$D[Dry==0.06]<-1

ClimRain$DateShort<-ClimRain$Date
ClimRain$Date<-format(ClimRain$Date,"%Y-%m-%d %H:%M:%S")
ClimRain$Date<-as.POSIXct(ClimRain$DateShort, "%Y-%m-%d %H:%M:%S",tz= "GMT")
tmp<-merge(x=SMmerged, y=ClimRain, by='Date', all=TRUE)

tmp<-merge(tmp, out,  by='Date', all=TRUE)

tmp$PRECIPITATION[is.na(tmp$PRECIPITATION)]<--999
tmp$VWC10.x[is.na(tmp$VWC10.x)]<--999
tmp$smooth[is.na(tmp$smooth)]<--999

tmp$WD<-tmp$VWC10.x*0
tmp$WD[tmp$PRECIPITATION>1 & tmp$smooth<0.06]<-1

ind<-which(tmp$smooth[which(tmp$WD==1)+50]>0.06)

tmp$W2<-tmp$VWC10.x*0
tmp$W2[which(tmp$WD==1)][ind]<-1

tmp$WDFT<-tmp$VWC10.x*0
tmp$WDFT[tmp$WD==1]<-'WD'
tmp$WDFT[tmp$D==1]<-'D'
tmp$WDFT[tmp$W==1]<-'W'
tmp$WDFT[tmp$Freeze==1]<-'F'
tmp$WDFT[tmp$Thaw==1]<-'T'

r<-ggplot()+geom_line(aes(SMmerged$Date, SMmerged$smooth))+
  geom_point(aes(SMmerged$Date, Dry), color='blue')+
  geom_point(aes(SMmerged$Date, Wet), color='green')+
  geom_text(aes(x=SMmerged$Date, y=Wet,label=as.numeric(WetInd)),hjust=0, vjust=0)+
  geom_text(aes(x=SMmerged$Date, y=Dry,label=as.numeric(DryInd)),hjust=0, vjust=0)
ggplotly(r)

ClimRain<-ClimRain[ClimRain$Date>"2008-01-01 GMT" & ClimRain$Date<"2017-10-01 GMT",]
SMmerged<-SMmerged[SMmerged$Date>"2008-01-01 GMT" & SMmerged$Date <"2017-10-01 GMT",]
RainInd<-ClimRain$Date[ClimRain$PRECIPITATION>1]
tmp<-tmp[tmp$Date>"2008-01-01 GMT" & tmp$Date<"2017-10-01 GMT",]

#Precip is in cm
r<-ggplot()+geom_line(aes(SMmerged$Date, SMmerged$smooth))+
  #geom_line(aes(SMmerged$Date, surfaceSM), color='blue')+
  geom_point(aes(as.POSIXct(RainInd), (ClimRain$PRECIPITATION[ClimRain$PRECIPITATION>1]*0+0.01)), color='red')+
  geom_point(aes(as.POSIXct(tmp$Date[tmp$WD==1]), (tmp$WD[tmp$WD==1]*0+0.02)), color='orange')+
  geom_point(aes(as.POSIXct(tmp$Date[tmp$D==1]), (tmp$D[tmp$D==1]*0+0.02)), color='dark red')+
  geom_point(aes(as.POSIXct(tmp$Date[tmp$W==1]), (tmp$W[tmp$W==1]*0+0.02)), color='blue')+
  geom_point(aes(as.POSIXct(tmp$Date[tmp$Freeze==1]), (tmp$Freeze[tmp$Freeze==1]*0+0.025)), color='light blue')+
  geom_point(aes(as.POSIXct(tmp$Date[tmp$Thaw==1]), (tmp$Thaw[tmp$Thaw==1]*0+0.025)), color='brown')+
  #geom_point(aes(as.POSIXct(tmp$Date[tmp$W2==1]), (tmp$W2[tmp$W2==1]*0+0.021)), color='pink')+
  geom_text(aes(x=as.POSIXct(RainInd), y=(ClimRain$PRECIPITATION[ClimRain$PRECIPITATION>1]*0+0.01),label=(ClimRain$PRECIPITATION[ClimRain$PRECIPITATION>1])),hjust=0, vjust=0)
ggplotly(r)

#-------------Graph soil moisture and temp-------
tmp3<-tmp[tmp$Date>"2013-10-01 GMT" & tmp$Date<"2014-01-01 GMT",]

maxPR   <- max(tmp3$PRECIPITATION[tmp3$PRECIPITATION>1 & tmp3$smooth<0.06], na.rm = T)
maxSF   <- max(tmp3$smooth*100, na.rm = T)
maxT<- max(tmp3$TAirInterp, na.rm = T)
par(mar = c(4, 4, 3, 4) + 0.1)
plot(tmp3$Date, tmp3$smooth*100,
     type = 'l', col = "#fe9929",
     ylim = c(0, 1.1 * maxSF),
     xaxs = "i", yaxs = "i",
     xlab = "Date", ylab = "Soil Moisture [%]",
     main = "Soil Moisture at 100 cm", cex.main = 0.9)
abline(h=6, lwd=1, lty=2, col='gray' )
points(tmp3$Date[tmp3$D==1], (tmp3$D[tmp3$D==1]*0+6), pch=16, cex=0.8, col='brown')
points(tmp3$Date[tmp3$W==1], (tmp3$W[tmp3$W==1]*0+6), pch=16, cex=0.8, col='light blue')
points(tmp3$Date[tmp3$WD==1], (tmp3$WD[tmp3$WD==1]*0+6), pch=4, cex=0.8, col='black')
#legend(tmp3$Date[2], 40, legend=c("Soil Moisture"),  col=c("#fe9929"), lty=1)


par(new = TRUE)
plot(x = tmp3$Date, y = rep(0, length(tmp3$smooth)),
     type = "n", ylim = c(6 * maxPR, 0),
     xaxs = "i", yaxs = "i",
     axes = F, xlab = "", ylab = "")
segments(x0 = tmp3$Date[tmp3$PRECIPITATION>1& tmp3$smooth<0.06], y0 = rep(0, length(tmp3$PRECIPITATION[tmp3$PRECIPITATION>1& tmp3$smooth<0.06])),
         x1 = tmp3$Date[tmp3$PRECIPITATION>1& tmp3$smooth<0.06], y1 = tmp3$PRECIPITATION[tmp3$PRECIPITATION>1& tmp3$smooth<0.06],
         lend = 2, lwd =2)
yrAxis  <- seq(0, round_any(maxPR,10), length.out = 5)
axis(4, at = yrAxis, labels = paste0(yrAxis))
mtext(y = yrAxis, par(usr)[1], labels = yrAxis)
mtext("Daily Precip. [cm]", side = 4, line = 2, adj = 1)


maxT<- max(tmp3$TAirInterp, na.rm = T)
minT<- min(tmp3$TAirInterp, na.rm = T)
plot(x = tmp3$Date[!is.na(tmp3$Tair6hr)], y = tmp3$Tair6hr[!is.na(tmp3$Tair6hr)],
     type = 'l', col = "#fe9929",
     ylim = c(minT,  maxT),
     xaxs = "i", yaxs = "i",
     xlab = "Date", ylab = "Air Temperature [C^0]",
     main = "Air Temperature", cex.main = 0.9)
points(tmp3$Date[tmp3$Freeze==1], (tmp3$Freeze[tmp3$Freeze==1]*0), pch=16, cex=0.8, col='blue')
points(tmp3$Date[tmp3$Thaw==1], (tmp3$Thaw[tmp3$Thaw==1]*0), pch=16, cex=0.8, col='dark green')



par(new = TRUE)
plot(x = tmp$Date[!is.na(tmp$Tair6hr)], y = tmp$Tair6hr[!is.na(tmp$Tair6hr)],
     type = "l", ylim = c( 0, 3 * maxT),
     xaxs = "i", yaxs = "i",
     axes = FALSE, xlab = "", ylab = "")

yrAxis  <- seq(0, round_any(maxT,10), length.out = 5)
axis(4, at = yrAxis, labels = paste0(yrAxis))
mtext(y = yrAxis, par(usr)[1], labels = yrAxis)
mtext("Daily Precip. [cm]", side = 4, line = 2, adj = 1)






##-------------------Cycle to Day Calcs----------

Cycle<-c()
j=0
i=1;
FireDate<-"2016-10-01 GMT"
Time<-as.POSIXct(FireDate)
for (i in c(1:50)){
  tmp2<-tmp[tmp$Date>FireDate,]
  ind<-tmp2$WDFT[tmp2$WDFT!='0']
  if (tmp2$WDFT[tmp2$WDFT!='0'][i]=='T' | tmp2$WDFT[tmp2$WDFT!='0'][i]=='D' | tmp2$WDFT[tmp2$WDFT!='0'][i]=='WD' ){
    Cycle<-c(Cycle, j)
    Time<-c(Time, tmp2$Date[tmp2$WDFT!='0'][i])
    j=j+1}
  i=i+1
  }
  
tmp3<-as.numeric(difftime(Time[2:length(Time)],Time[1:(length(Time)-1)], units="days")[1:11])

Sum<-tmp3[1]
for (i in c(1:12)){
  Sum<-c(Sum, Sum[i]+tmp3[i+1])
}

Sum2008<-Sum
Sum2009<-Sum
Sum2010<-Sum
Sum2011<-Sum
Sum2012<-Sum
Sum2013<-Sum
Sum2014<-Sum
Sum2015<-Sum
Sum2016<-Sum
Sum2017<-Sum

df<-as.data.frame(Sum2008)
df$Sum2009<-Sum2009
df$Sum2010<-Sum2010
df$Sum2011<-Sum2011
df$Sum2012<-Sum2012
df$Sum2013<-Sum2013
df$Sum2014<-Sum2014
df$Sum2015<-Sum2015
df$Sum2016<-Sum2016
df$Sum2017<-Sum2017
df$Cycle<-c(1:13)

df2<-melt(data = SumDT, id = "Cycle")

df$means<-rowMeans(df)
for (i in 1:13){
df$q025[i]<-quantile(df[i,1:10],.025)
df$q975[i]<-quantile(df[i,1:10],.975)}


df2<-melt(data = df[1:11,], id = "Cycle")

 ggplot()+geom_boxplot( aes(df2$Cycle, df2$value, group=df2$Cycle))+xlab('Cycle')+ylab('days')+theme_classic(base_size = 20)+
   theme(axis.text=element_text(size=16))+scale_x_continuous(breaks = scales::pretty_breaks(n = 10))
 
 +scale_x_discrete(breaks=c(1:11),labels=c(1:11))


 
 
 
 
 
 ind<-tmp2$WDFT[tmp2$WDFT!='0']
 DryON=0
 Cycle<-c(1:50)*0
 j=0
 i=1;
 FireDate<-"2017-10-01 GMT"
 Time<-as.POSIXct(FireDate)
 for (i in c(2:48)){
   ind<-which(tmp$Date==FireDate)-1
   tmp2<-tmp[tmp$Date>FireDate,]
   
   if (tmp2$WDFT[tmp2$WDFT!='0'][1]=='WD'){
     Cycle[1]<-1
   Time<-c(Time, tmp2$Date[tmp2$WDFT!='0'][1])
   j=j+1}
   
   if (tmp2$WDFT[tmp2$WDFT!='0'][i]=='D'|tmp2$WDFT[tmp2$WDFT!='0'][i]=='WD'){
     DryON<-1
   }
   if (tmp2$WDFT[tmp2$WDFT!='0'][i]=='W'){
     DryON<-0
   }
   if ((tmp2$WDFT[tmp2$WDFT!='0'][i]=='T'& DryON==0) | (tmp2$WDFT[tmp2$WDFT!='0'][i]=='D' & tmp2$WDFT[tmp2$WDFT!='0'][i-1]!='T') | tmp2$WDFT[tmp2$WDFT!='0'][i]=='WD' ){
     Cycle[i]<-1
     Time<-c(Time, tmp2$Date[tmp2$WDFT!='0'][i])
     j=j+1}
   i=i+1
 }
 
 
 #below need to make sure consecutive WD is not double counted and that WD is W at the end of the season
 
 #DO NOT RUN TWICE
 tmp$WDFT[tmp$WDFT!="0"][383]<-"0"
 tmp$WDFT[tmp$WDFT!='0'][372]<-"W"
 tmp$WDFT[tmp$WDFT!='0'][214]<-"W"
 tmp$WDFT[tmp$WDFT!='0'][240]<-"0"
 tmp$WDFT[tmp$WDFT!='0'][238]<-"0"
 tmp$WDFT[tmp$WDFT!='0'][237]<-"0"
 tmp$WDFT[tmp$WDFT!='0'][236]<-"0"
 tmp$WDFT[tmp$WDFT!="0"][152]<-"0"
 tmp$WDFT[tmp$WDFT!="0"][151]<-"0"
 tmp$WDFT[tmp$WDFT!="0"][150]<-"W"
 
 
 TMP<-tmp
 
 
 DryON=0
 Cycle<-c(1:length(TMP$Date[TMP$WDFT!='0']))*0
DryIND<-c(1:length(TMP$Date[TMP$WDFT!='0']))*0
 j=0
 i=1;
 Time<-as.POSIXct(FireDate)
 OriginalIND<-which(TMP$WDFT!='0')
 for (i in c(3:(length(TMP$Date[TMP$WDFT!='0'])-2))){
   if (TMP$WDFT[TMP$WDFT!='0'][1]=='WD'){
     Cycle[1]<-1
     Time<-c(Time, TMP$Date[TMP$WDFT!='0'][1])
     j=j+1}
   
   if (TMP$WDFT[TMP$WDFT!='0'][i]=='D'|TMP$WDFT[TMP$WDFT!='0'][i]=='WD'){
     DryON<-1
     DryInd[i]<-1
     if ( (TMP$WDFT[TMP$WDFT!='0'][i]=='WD') & TMP$VWC10.x[TMP$WDFT!='0'][i]>0.06 ){
       DryON<-0}
     
   }
   #if ((TMP$WDFT[TMP$WDFT!='0'][i]=='T'& DryON==1)){
   #  TMP$WDFT[TMP$WDFT!='0'][i]<-'0'
   #  TMP$WDFT[TMP$WDFT!='0'][i-1]<-'0'
  # }
   
   if (TMP$WDFT[TMP$WDFT!='0'][i]=='W'){
     DryON<-0
   }
   if( TMP$WDFT[TMP$WDFT!='0'][i]=='WD' & TMP$WDFT[TMP$WDFT!='0'][i+1]=='WD' & (TMP$Date[TMP$WDFT!='0'][i+1]-TMP$Date[TMP$WDFT!='0'][i]==1 )){
     TMP$WDFT[TMP$WDFT!='0'][i+1]<-'D'
     TMP$WDFT[TMP$WDFT!='0'][i]<-'W'
     Cycle[i]<-0
     Time<-Time[1:(length(Time)-1)]
     
     
   }
   if ((TMP$WDFT[TMP$WDFT!='0'][i]=='T'& DryON==0) | (TMP$WDFT[TMP$WDFT!='0'][i]=='D' & TMP$WDFT[TMP$WDFT!='0'][i-1]!='T') | TMP$WDFT[TMP$WDFT!='0'][i]=='WD' ){
     Cycle[i]<-1
     Time<-c(Time, TMP$Date[TMP$WDFT!='0'][i])
     if(TMP$WDFT[TMP$WDFT!='0'][i]=='WD' & (TMP$WDFT[TMP$WDFT!='0'][i+1]=='W') & difftime(TMP$Date[TMP$WDFT!='0'][i+1],TMP$Date[TMP$WDFT!='0'][i],units='days')<=2 ){  # TMP$smooth[OriginalIND[i]+1]>0.06 
       TMP$WDFT[TMP$WDFT!='0'][i]<-'0'
       #TMP$WDFT[TMP$WDFT!='0'][i+1]<-'W'
       Cycle[i]<-0
       print('T')
       Time<-Time[1:(length(Time)-1)]
       DryON<-0
     }
     
     
     if(TMP$WDFT[TMP$WDFT!='0'][i]=='D' & TMP$WDFT[TMP$WDFT!='0'][i-1]=='W' & TMP$WDFT[TMP$WDFT!='0'][i+1]=='W'){  # TMP$smooth[OriginalIND[i]+1]>0.06 
       TMP$WDFT[TMP$WDFT!='0'][i]<-'0'
       #TMP$WDFT[TMP$WDFT!='0'][i+1]<-'W'
       Cycle[i]<-0
       print('T')
       Time<-Time[1:(length(Time)-1)]
       DryON<-0
     }

     if ( (TMP$WDFT[TMP$WDFT!='0'][i]=='T'| TMP$WDFT[TMP$WDFT!='0'][i]=='F') & TMP$SnowD.x[TMP$WDFT!='0'][i]>10 ){
       Cycle[i]<-0
       Time<-Time[1:(length(Time)-1)]
       j=j+1}
     
     if ( (TMP$WDFT[TMP$WDFT!='0'][i]=='WD') & TMP$SnowD.x[TMP$WDFT!='0'][i]>10  & (TMP$SnowD.x[TMP$WDFT!='0'][i]>10 | TMP$SnowD.x[which(TMP$Date==TMP$Date[TMP$WDFT!='0'][i]+(24*60*60))]>10) ){
       Cycle[i]<-0
       Time<-Time[1:(length(Time)-1)]
       print('snow')
       j=j+1}
     

     # if( TMP$WDFT[TMP$WDFT!='0'][i]=='W' & TMP$WDFT[TMP$WDFT!='0'][i+2]=='W' & (TMP$Date[TMP$WDFT!='0'][i+2]-TMP$Date[TMP$WDFT!='0'][i+1]==1 )){
     #   TMP$WDFT[TMP$WDFT!='0'][i+2]<-'0'
     #   TMP$WDFT[TMP$WDFT!='0'][i+1]<-'0'
     #   #TMP$WDFT[TMP$WDFT!='0'][i+1]<-'W'
     #   Cycle[i]<-0
     #   print('T')
     #   Time<-Time[1:(length(Time)-1)]
     #   DryON<-0
     # 
     # }
     
     j=j+1}
   if ( TMP$WDFT[TMP$WDFT!='0'][i-2]=='WD' & TMP$WDFT[TMP$WDFT!='0'][i]=='T' & (difftime(TMP$Date[TMP$WDFT!='0'][i-2],TMP$Date[TMP$WDFT!='0'][i-1], units='hours' )<24) &TMP$VWC10.x[TMP$WDFT!='0'][i]<0.06 & TMP$SnowD.x[TMP$WDFT!='0'][i]<10 & TMP$SnowD.x[TMP$WDFT!='0'][i-1]<10 & TMP$SnowD.x[TMP$WDFT!='0'][i-2]<10){
     Cycle[i]<-1
     print(i)
     print('snow4')
     j=j+1}
   if ( (TMP$WDFT[TMP$WDFT!='0'][i]=='WD' | TMP$WDFT[TMP$WDFT!='0'][i]=='D' ) & (TMP$SnowD.x[TMP$WDFT!='0'][i]>10 | TMP$SnowD.x[which(TMP$Date==TMP$Date[TMP$WDFT!='0'][i]+(24*60*60))]>10) ){
     Cycle[i]<-0
     Time<-Time[1:(length(Time)-1)]
     print('snow2')
     j=j+1}
   i=i+1
 }
 # 
 # for (i in c(2:(length(TMP$Date[TMP$WDFT!='0'])-2))){
 #   if (TMP$WDFT[TMP$WDFT!='0'][i]=='WD' & TMP$WDFT[TMP$WDFT!='0'][i+1]=='W' & difftime(TMP$Date[TMP$WDFT!='0'][i+1],TMP$Date[TMP$WDFT!='0'][i],units='days')<=3 ){
 #     TMP$WDFT[TMP$WDFT!='0'][i]<-'0'
 #     Cycle[i]<-0
 #     Time<-Time[1:(length(Time)-1)]
 #     print('T')
 #     }
 # }


 library(matrixStats)
 j=1;
 SumDT<-as.data.frame(c(1:11))
 names(SumDT)<-'2008'
 for(n in c(2008:2016)){
 FireDate<-paste(n,'-10-01 GMT', sep='')
 ind2<-which(TMP$Date[TMP$WDFT!='0']>FireDate)
 TMP2<-TMP[TMP$Date>FireDate &TMP$WDFT!='0',]
 Cycle2<-Cycle[ind2]
 DryInd2<-DryInd[ind2]
Time2<-TMP2$Date[which(Cycle2==1)]
Time2<-as.POSIXct(c(round_date(TMP$Date[TMP$Date==FireDate], unit='day'),Time2))
 tmp3<-as.numeric(difftime(Time2[2:length(Time2)],Time2[1:(length(Time2)-1)], units="days")[1:11])
 
 Sum<-tmp3[1]
 for (i in c(1:10)){
   Sum<-c(Sum, Sum[i]+tmp3[i+1])
 }
 SumDT[j]<-as.data.frame(Sum)
 j=j+1
 }
 
 names(SumDT)<-c('2008','2009','2010','2011','2012','2013','2014','2015','2016')
 SumDT$Cycle<-c(1:11)
 SumDT<-round(SumDT)
 
 df2<-melt(data = SumDT, id = "Cycle")
 
 SumDT$means<-rowMeans(SumDT[,1:8])
 SumDT$median<- apply(SumDT[,1:8], 1, median)
 for (i in 1:11){
   SumDT$q025[i]<-quantile(SumDT[i,1:8],.25)
   SumDT$q975[i]<-quantile(SumDT[i,1:8],.75)}
 
 
 TMP3<-TMP[TMP$Date>"2008-10-01 GMT" & TMP$Date<"2017-10-01 GMT",]
 ind2<-which(TMP$Date[TMP$WDFT!='0']>"2008-10-01 GMT" & TMP$Date[TMP$WDFT!='0']<"2017-10-01 GMT")
 Cycle2<-Cycle[ind2]
 
 

 ggplot()+geom_boxplot( aes(df2$Cycle, df2$value, group=df2$Cycle), middle = SumDT$means, lower=unlist(SumDT$q025), upper=unlist(SumDT$q975))+xlab('Cycle')+ylab('days')+theme_classic(base_size = 16)+
   theme(axis.text=element_text(size=16))+scale_x_continuous(breaks = scales::pretty_breaks(n = 10))+coord_flip()+ylim(-3,450)+
   theme(panel.border = element_rect(colour = "black", fill=NA, size=1))+
   stat_summary(fun.y = mean, geom = "errorbar")+
   theme(legend.text=element_text(size=16))
 
 
 
 pdf(file = "/Users/katya/Google Drive/PhD/Hydrophobicity/ForGraphs/DayDistributionV2.pdf",   # The directory you want to save the file in
     width = 6.5, # The width of the plot in inches
     height = 3) # The height of the plot in inches        
 
 ggplot()+geom_boxplot(middle = SumDT$median, lower = unlist(SumDT$q025), upper=unlist(SumDT$q975),ymax=apply(SumDT[,1:8], 1, FUN=max),ymin=apply(SumDT[,1:8], 1, FUN=min), aes(df2$Cycle, df2$value, group=df2$Cycle), outlier.shape = NA)+xlab('Cycle')+ylab('days')+theme_classic(base_size = 12)+
   theme(axis.text=element_text(size=12))+scale_x_continuous(breaks = scales::pretty_breaks(n = 10))+coord_flip()+ylim(-3,400)+
   theme(panel.border = element_rect(colour = "black", fill=NA, size=1))+
   stat_summary(fun.y = mean, geom = "errorbar")
 
 dev.off()
 


 
 #2008 WFTx11 correct
 #2009 WD WFTx10 correct
 #2010 WFTx11  correct
 #2011 WFTx9 WFTD WFT correct
 #2012 WD WFTx10 correct
 #2013 WD WD WD WD WD WFTx6  correct
 #2014  WFTx11 correct
 #2015  WFTx11 correct
 #2016  WD WFTx10 correct

 #2008 wftx6, wdx1 ->2009 WD wftx3
 #2009 wd wftx8 wd -> 2010 wftx2
 #2010 wftx4 wdx2, rest is wft from 2011
 #2011: wft x10 Wftdx1
 #2012 wd wftx3 wftx2 wdx6
 #2013 WDx5 WFTx6
 #2014 FTx7 -> 2015 ftdx1 wd wftx2
 #2015 wdx1 wft x4  wdx2 wftd wd -> 2016 wd wft

 #Tally: wft=6+8+4+11+3+2+6+7+6+1 =54  =70%
 #wd=1+1+1+2+1+7+5+1+2+1+1 = 23
 
 #6 hr freeze and 6 hr thaw period
 # 10 cm snow depth
 
 #2008 wftx7 WDx1 ->2009 WDx1 wftx2
 #2009 wd wftx8 wd ->2010 wd
 #2010 wd wftx5 wdx3 ->2011 wftd wft
 #2011 wftd wftx2 wftd wdx5 ->2012 wd wft
 #2012 wd wftx3 wftdx1 wdx6
 #2013 wdx5 wftx3     wdx3
 #2014 wftx5 wftdx1 -> 2015 wd wftx4
 #2015 wd wftx7  wftd  wd ->2016 wd
 #2016 wd wftx2     wftdx1  wdx2 ->2017 
 
 #all  
 #wft=7+8+5+4+4+3+6+8=45  #60%
 #wd=1+2+4+5+7+8+0+3=30

 #first 6 cycles
 #wft 6+5+5+4+4+1+6+5=36
 #wd=0+1+1+1+1+5+0+1=10
 #78%
 
 #good to plot 2012
 #2008   wft  wft  wft  wft  wft wft   wft  wd | wd   wftd  wft
 #2009   wd   wftd wft  wft  wft wft   wft  wft  wftd wd |  wd
 #2010   wd   wft  wft  wft  wft wft   wftd wd   wd   wft   wft 
 #2011   wft  wft  wft  wft  wft wft   wft  wft  wft  wftd  wftd  
 #2012   wd   wft  wft  wft  wftd wd   wd   wd   wd   wd    wd 
 #2013   wd   wd   wd   wd   wd   wftd wft  wftd wd   wd    wd  
 #2014   wft  wft  wft  wft  wft  wftd|wd   wft  wft  wft   wft
 #2015   wd   wft  wft  wft  wft  wft  wft  wft  wftd wd|   wd 
 #2016
 
 #all   71%
 #ft    3+7+7+7+7+7+6+4+3+2+2=55   
 # wd   5+1+1+1+1+1+1+3+3+4+2=23
 
 #first 6 cycles  79%
 #ft    3+7+7+7+7+7  =38
 # wd   5+1+1+1+1+1  =10
 
 
 #-------------Graph soil moisture and temp-------
 #2009,10 same issue
 #2010 fix begining 
 TMP3<-TMP[TMP$Date>"2016-10-01 GMT" & TMP$Date<"2017-10-1 GMT",]
 ind2<-which(TMP$Date[TMP$WDFT!='0']>"2016-10-01 GMT" & TMP$Date[TMP$WDFT!='0']<"2017-10-1 GMT")
 Cycle2<-Cycle[ind2]
 TMP3ind<-TMP3[TMP3$WDFT!='0',]
 DryIND<-DryIND[ind2]

  # pdf(file = "/Users/katya/Google Drive/PhD/Hydrophobicity/ForGraphs/Climate2013.pdf",   # The directory you want to save the file in
  #     width = 6.5, # The width of the plot in inches
  #     height = 4) # The height of the plot in inches

 op<-par( oma = c(0,0,0,0) + 0.1,
      mar = c(0,0,0,0) + 0.1,
      mfrow=c(2,1))
 maxPR   <- max(TMP3$PRECIPITATION[TMP3$PRECIPITATION>1], na.rm = T)
 maxSF   <- max(TMP3$smooth*100, na.rm = T)
 maxT<- max(TMP3$TAirInterp, na.rm = T)
 par(mar = c(3, 4, 1, 4) + 0.1)
 plot(TMP3$Date, TMP3$smooth*100,
      type = 'l', col = "#fe9929", lwd=2,
      ylim = c(0, 1.4 * maxSF),
      xaxs = "i", yaxs = "i",
      xlab = NA, ylab = "Soil Moisture [%]",
      main = "Wet-Dry Cycles", cex.main = 0.9)
 ind<-TMP3$Date[TMP3$SnowD.x>10]
  lines(TMP3$Date, TMP3$smooth2*100)
# points(TMP3$Date[TMP3$D==1], (TMP3$D[TMP3$D==1]*0+6), pch=16, cex=0.8, col='brown')
# points(TMP3$Date[TMP3$W==1], (TMP3$W[TMP3$W==1]*0+6), pch=16, cex=0.8, col='light blue')
# points(TMP3$Date[TMP3$WD==1], (TMP3$WD[TMP3$WD==1]*0+6), pch=4, cex=0.8, col='black')
 #legend(TMP3$Date[2], 40, legend=c("Soil Moisture"),  col=c("#fe9929"), lty=1)
 xleft<-c(TMP3$Date[TMP3$WDFT=='W' | TMP3$WDFT=='WD'])[1:12]
 xright<-c(TMP3$Date[TMP3$WDFT=='WD'|TMP3$WDFT=='D'],TMP3$Date[length(TMP3$Date)])[1:12]+(60*60*24)
 if (TMP3$WDFT[TMP3$WDFT!='0'][1]=='D'){
   xright<-xright[2:length(xright)]
 }
ybottom<-rep(0,length(TMP$WDFT[TMP$WDFT=='W' | TMP$WDFT=='WD']))[1]
 ytop<-rep(60,length(TMP$WDFT[TMP$WDFT=='W' | TMP$WDFT=='WD']))[1]
 rect(xleft, ybottom, xright, ytop, col=rgb(0,0,1,alpha=0.2) ,lwd=0.5)
 #rect(ind[1:(length(ind)-1)], -5, ind[2:(length(ind))], 50, border='#bdbdbd', lwd=0.5 )
 rect(xleft, ybottom, xright, ytop, fill=NA ,lwd=0.5)
 abline(h=6, lwd=1, lty=2, col='gray' )
 abline(v=as.POSIXct(TMP3$Date[1]), lwd=3, lty=1, col='red' )
lines(TMP3$Date, TMP3$smooth*100,
      type = 'l', col = "#fe9929",lwd=2)
 par(new = TRUE)
 plot(x = TMP3$Date, y = rep(0, length(TMP3$smooth)),
      type = "n", ylim = c(4 * maxPR, 0),
      xaxs = "i", yaxs = "i",
      axes = F, xlab = "", ylab = "")
 segments(x0 = TMP3$Date[TMP3$PRECIPITATION>1], y0 = rep(0, length(TMP3$PRECIPITATION[TMP3$PRECIPITATION>1])),
          x1 = TMP3$Date[TMP3$PRECIPITATION>1], y1 = TMP3$PRECIPITATION[TMP3$PRECIPITATION>1],
          lend = 2, lwd =3)
 yrAxis  <- seq(0, round_any(maxPR,10), length.out = 5)
 axis(4, at = yrAxis, labels = paste0(yrAxis))
 mtext(y = yrAxis, par(usr)[1], labels = yrAxis)
 mtext("Daily Precip. [cm]", side = 4, line = 2, adj = 1)
 
 
 maxT<- max(TMP3$TAirInterp, na.rm = T)
 minT<- min(TMP3$TAirInterp, na.rm = T)
 plot(x = TMP3$Date[!is.na(TMP3$Tair6hr)], y = TMP3$Tair6hr[!is.na(TMP3$Tair6hr)],
      type = 'l', col = "#fe9929",
      ylim = c(minT,  maxT),
      xaxs = "i", yaxs = "i",
      xlab = "Date", ylab = "Air Temperature [C]",
      main = "Freeze-Thaw Cycles", cex.main = 0.9, lwd=0.7)
 ind<-TMP3$Date[TMP3$SnowD.x>10]
 rect(ind[1:(length(ind)-1)], -15, ind[2:(length(ind))], 35, border='#d9d9d9', lwd=0.7)
 #rect(ind[1:(length(ind)-1)], minT, ind[2:(length(ind))], maxT, border='#d9d9d9' )
 lines(x = TMP3$Date[!is.na(TMP3$Tair6hr)], y = TMP3$Tair6hr[!is.na(TMP3$Tair6hr)],col = "#fe9929", lwd=0.7)
 
 abline(h=0, lwd=1, lty=2, col='gray' )
 abline(v=as.POSIXct(TMP3$Date[1]), lwd=6, lty=1, col='red' )
 # #points(TMP3$Date[TMP3$Freeze==1], (TMP3$Freeze[TMP3$Freeze==1]*0), pch=16, cex=0.8, col='blue')
 # #points(TMP3$Date[TMP3$Thaw==1], (TMP3$Thaw[TMP3$Thaw==1]*0), pch=16, cex=0.8, col='dark green')
 # xleft<-c(TMP3$Date[TMP3$WDFT=='F' & TMP3$smooth*100>6] )
 # xright<-c(TMP3$Date[TMP3$WDFT=='T' & TMP3$smooth*100>6] )
 # ybottom<-rep(-40,length(TMP$WDFT[TMP$WDFT=='F']))[1]
 # ytop<-rep(40,length(TMP$WDFT[TMP$WDFT=='T']))[1]
 # rect(xleft, ybottom, xright, ytop, col=rgb(0,0,1,alpha=0.2),lwd=0.5 )
 # 
 # xleft<-c(TMP3$Date[TMP3$WDFT=='F' & TMP3$smooth*100<6] )
 # xright<-c(TMP3$Date[TMP3$WDFT=='T' & TMP3$smooth*100<6] )
 # ybottom<-rep(-40,length(TMP$WDFT[TMP$WDFT=='F']))[1]
 # ytop<-rep(40,length(TMP$WDFT[TMP$WDFT=='T']))[1]
 # rect(xleft, ybottom, xright, ytop, col=rgb(0,0,1,alpha=0.2),lwd=0.5 )
 # 
 xleft<-TMP3ind$Date[(TMP3ind$WDFT=='T'| TMP3ind$WDFT=='F') & (c(diff(Cycle2),0)==1 )][2:5]-6*60*60
 xright<-TMP3ind$Date[(TMP3ind$WDFT=='T'| TMP3ind$WDFT=='F') & Cycle2==1 ]+6*60*60
 ybottom<-rep(-40,length(TMP$WDFT[TMP$WDFT=='F' &TMP$WDFT!='0']))[1]
 ytop<-rep(40,length(TMP$WDFT[TMP$WDFT=='T'&TMP$WDFT!='0' & Cycle2==1]))[1]
 rect(xleft, ybottom, xright, ytop, col=rgb(0,0,1,alpha=0.2),lwd=0.5 )
 
 TMP$Date[TMP$WDFT!='0']
 par(op)
#dev.off()
 
 ##
 
 #-------------Graph soil moisture and temp-------
 TMP3<-TMP[TMP$Date>"2008-10-01 GMT" & TMP$Date<"2017-10-01 GMT",]
 ind2<-which(TMP$Date[TMP$WDFT!='0']>"2008-10-01 GMT" & TMP$Date[TMP$WDFT!='0']<"2017-10-01 GMT")
 Cycle2<-Cycle[ind2]
 TMP3ind<-TMP3[TMP3$WDFT!='0',]
 DryIND<-DryIND[ind2]
 pdf(file = "/Users/katya/Google Drive/PhD/Hydrophobicity/ForGraphs/ClimateFull3.pdf",   # The directory you want to save the file in
     width = 6.5, # The width of the plot in inches
     height = 4.6) # The height of the plot in inches
 
 op<-par( oma = c(0,0,0,0) + 0.1,
          mar = c(0,0,0,0) + 0.1,
          mfrow=c(2,1))
 maxPR   <- max(TMP3$PRECIPITATION[TMP3$PRECIPITATION>1], na.rm = T)
 maxSF   <- max(TMP3$smooth*100, na.rm = T)
 maxT<- max(TMP3$TAirInterp, na.rm = T)
 par(mar = c(3, 4, 2, 4) + 0.1)
 plot(TMP3$Date, as.numeric(TMP3$smooth*100),
      type = 'l', col = "#fe9929", lwd=1.5,
      ylim = c(0, 1.2 * maxSF),
      xaxs = "i", yaxs = "i",
      xlab = NA, ylab = "Soil Moisture [%]",
       main="Wet-Dry Cycles",cex.main = 0.9)
 abline(h=6, lwd=1, lty=2, col='gray' )
# abline(v=as.POSIXct(TMP3$Date[1]), lwd=4, lty=1, col='red' )
 #points(TMP3$Date[TMP3$D==1], (TMP3$D[TMP3$D==1]*0+6), pch=16, cex=0.8, col='brown')
 #points(TMP3$Date[TMP3$W==1], (TMP3$W[TMP3$W==1]*0+6), pch=16, cex=0.8, col='light blue')
 #points(TMP3$Date[TMP3$WD==1], (TMP3$WD[TMP3$WD==1]*0+6), pch=4, cex=0.8, col='black')
 #legend(TMP3$Date[2], 40, legend=c("Soil Moisture"),  col=c("#fe9929"), lty=1)
 xleft<-c(TMP3$Date[TMP3$WDFT=='W' | TMP3$WDFT=='WD'])
 xright<-c(TMP3$Date[TMP3$WDFT=='WD'|TMP3$WDFT=='D'],TMP3$Date[length(TMP3$Date)])+(60*60*24)
 if (TMP3$WDFT[TMP3$WDFT!='0'][1]=='D'){
   xright<-xright[2:length(xright)]
 }
 ybottom<-rep(0,length(TMP$WDFT[TMP$WDFT=='W' | TMP$WDFT=='WD']))[1]
 ytop<-rep(40,length(TMP$WDFT[TMP$WDFT=='W' | TMP$WDFT=='WD']))[1]
 #rect(xleft, ybottom, xright, ytop, col=rgb(0,0,1,alpha=0.2) )
 par(new = TRUE)
 plot(x = TMP3$Date, y = rep(0, length(TMP3$smooth)),
      type = "n", ylim = c(6 * maxPR, 0),
      xaxs = "i", yaxs = "i",
      axes = F, xlab = "", ylab = "")
 segments(x0 = TMP3$Date[TMP3$PRECIPITATION>1], y0 = rep(0, length(TMP3$PRECIPITATION[TMP3$PRECIPITATION>1])),
          x1 = TMP3$Date[TMP3$PRECIPITATION>1], y1 = TMP3$PRECIPITATION[TMP3$PRECIPITATION>1],
          lend = 2, lwd =3)
 yrAxis  <- seq(0, round_any(maxPR,10), length.out = 5)
 axis(4, at = yrAxis, labels = paste0(yrAxis))
 mtext(y = yrAxis, par(usr)[1], labels = yrAxis)
 mtext("Daily Precip. [cm]", side = 4, line = 2, adj = 1)
 abline(v=as.POSIXct("2013-10-01 GMT"), lwd=3, lty=1, col='gray' )
 abline(v=as.POSIXct("2014-10-01 GMT"), lwd=3, lty=1, col='gray' )
 
 maxT<- max(TMP3$TAirInterp, na.rm = T)
 minT<- min(TMP3$TAirInterp, na.rm = T)
 plot(x = TMP3$Date[!is.na(TMP3$Tair6hr)], y = TMP3$Tair6hr[!is.na(TMP3$Tair6hr)], lwd=0.5,
      type = 'l', col = "#fe9929",
      ylim = c(minT,  maxT),
      xaxs = "i", yaxs = "i",
      xlab = "Date", ylab = "Air Temperature [C]",
       main="Freeze-Thaw Cycles",cex.main = 0.9)
#rect(TMP3$Date[TMP3$SnowD.x>10],-60, TMP3$Date[TMP3$SnowD.x>10], 60, border='#f0f0f0')
 abline(h=0, lwd=1, lty=2, col='gray' )
 abline(v=as.POSIXct("2013-10-01 GMT"), lwd=3, lty=1, col='gray' )
 abline(v=as.POSIXct("2014-10-01 GMT"), lwd=3, lty=1, col='gray' )
 #abline(v=as.POSIXct(TMP3$Date[1]), lwd=4, lty=1, col='red' )
 #points(TMP3$Date[TMP3$Freeze==1], (TMP3$Freeze[TMP3$Freeze==1]*0), pch=16, cex=0.8, col='blue')
 #points(TMP3$Date[TMP3$Thaw==1], (TMP3$Thaw[TMP3$Thaw==1]*0), pch=16, cex=0.8, col='dark green')
 xleft<-c(TMP3$Date[TMP3$WDFT=='F' & TMP3$smooth*100>6] )
 xright<-c(TMP3$Date[TMP3$WDFT=='T' & TMP3$smooth*100>6] )
 ybottom<-rep(-40,length(TMP$WDFT[TMP$WDFT=='F']))[1]
 ytop<-rep(40,length(TMP$WDFT[TMP$WDFT=='T']))[1]
 #rect(xleft, ybottom, xright, ytop, col=rgb(0,0,1,alpha=0.2) )
 
 xleft<-c(TMP3$Date[TMP3$WDFT=='F' & TMP3$smooth*100<6] )
 xright<-c(TMP3$Date[TMP3$WDFT=='T' & TMP3$smooth*100<6] )
 ybottom<-rep(-40,length(TMP$WDFT[TMP$WDFT=='F']))[1]
 ytop<-rep(40,length(TMP$WDFT[TMP$WDFT=='T']))[1]
# rect(xleft, ybottom, xright, ytop, col=rgb(0,1,0,alpha=0.2) )
 
 
 par(op)
 dev.off()
 
 ##------open MED results-----
 library(reshape)
 MED<-read.csv('/Users/katya/Google Drive/PhD/Hydrophobicity/Hydrophobicity Final/ResultsR.csv', header=T)
 sd(c(MED$WFT[MED$Cycle==0],MED$WFTD[MED$Cycle==0]))
 median(c(MED$WFT[MED$Cycle==0],MED$WFTD[MED$Cycle==0]))
 mean(c(MED$WFT[MED$Cycle==0],MED$WFTD[MED$Cycle==0],MED$WD[MED$Cycle==0],MED$WDFT[MED$Cycle==0]))
sd(c(MED$WFT[MED$Cycle==0],MED$WFTD[MED$Cycle==0],MED$WD[MED$Cycle==0],MED$WDFT[MED$Cycle==0]))
 
 
 sd(c(MED$WFT[MED$Cycle==1],MED$WFTD[MED$Cycle==1]))
 median(c(MED$WFT[MED$Cycle==1],MED$WFTD[MED$Cycle==1]))
 mean(c(MED$WFT[MED$Cycle==1],MED$WFTD[MED$Cycle==1],MED$WD[MED$Cycle==1],MED$WDFT[MED$Cycle==1]))
sd(c(MED$WFT[MED$Cycle==1],MED$WFTD[MED$Cycle==1],MED$WD[MED$Cycle==1],MED$WDFT[MED$Cycle==1]))
 
 
 MEDmelt<-melt(MED, id='Cycle')
 MEDmelt<-MEDmelt[!is.na(MEDmelt$value),]
 
 
pdf(file = "/Users/katya/Google Drive/PhD/Hydrophobicity/ForGraphs/Hydrophobicity.pdf",   # The directory you want to save the file in
    width = 6.3, # The width of the plot in inches
    height = 4) # The height of the plot in inches        
  
DFTmean<-c(mean(MED$DFT[MED$Cycle==0]),mean(MED$DFT[MED$Cycle==1]),mean(MED$DFT[MED$Cycle==2]),mean(MED$DFT[MED$Cycle==3]),mean(MED$DFT[MED$Cycle==4]),mean(MED$DFT[MED$Cycle==5]),mean(MED$DFT[MED$Cycle==6]),mean(MED$DFT[MED$Cycle==7]),mean(MED$DFT[MED$Cycle==8]),mean(MED$DFT[MED$Cycle==9]),mean(MED$DFT[MED$Cycle==10]),mean(MED$DFT[MED$Cycle==11]))
WFTmean<-c(mean(MED$WFT[MED$Cycle==0]),mean(MED$WFT[MED$Cycle==1]),mean(MED$WFT[MED$Cycle==2]),mean(MED$WFT[MED$Cycle==3]),mean(MED$WFT[MED$Cycle==4]),mean(MED$WFT[MED$Cycle==5]),mean(MED$WFT[MED$Cycle==6]),mean(MED$WFT[MED$Cycle==7]),mean(MED$WFT[MED$Cycle==8]),mean(MED$WFT[MED$Cycle==9]),mean(MED$WFT[MED$Cycle==10]),mean(MED$WFT[MED$Cycle==11]))
WDFTmean<-c(mean(MED$WDFT[MED$Cycle==0]),mean(MED$WDFT[MED$Cycle==1]),mean(MED$WDFT[MED$Cycle==2]),mean(MED$WDFT[MED$Cycle==3]),mean(MED$WDFT[MED$Cycle==4]),mean(MED$WDFT[MED$Cycle==5]),mean(MED$WDFT[MED$Cycle==6]),mean(MED$WDFT[MED$Cycle==7]),mean(MED$WDFT[MED$Cycle==8]),mean(MED$WDFT[MED$Cycle==9]),mean(MED$WDFT[MED$Cycle==10]),mean(MED$WDFT[MED$Cycle==11]))
WFTDmean<-c(mean(MED$WFTD[MED$Cycle==0]),mean(MED$WFTD[MED$Cycle==1]),mean(MED$WFTD[MED$Cycle==2]),mean(MED$WFTD[MED$Cycle==3]),mean(MED$WFTD[MED$Cycle==4]),mean(MED$WFTD[MED$Cycle==5]),mean(MED$WFTD[MED$Cycle==6]),mean(MED$WFTD[MED$Cycle==7]),mean(MED$WFTD[MED$Cycle==8]),mean(MED$WFTD[MED$Cycle==9]),mean(MED$WFTD[MED$Cycle==10]),mean(MED$WFTD[MED$Cycle==11]))
WDmean<-c(mean(MED$WD[MED$Cycle==0]),mean(MED$WD[MED$Cycle==1]),mean(MED$WD[MED$Cycle==2]),mean(MED$WD[MED$Cycle==3]),mean(MED$WD[MED$Cycle==4]),mean(MED$WD[MED$Cycle==5]),mean(MED$WD[MED$Cycle==6]),mean(MED$WD[MED$Cycle==7]),mean(MED$WD[MED$Cycle==8]),mean(MED$WD[MED$Cycle==9]),mean(MED$WD[MED$Cycle==10]),mean(MED$WD[MED$Cycle==11]))

C0<-c(median(MED$DFT[MED$Cycle==0],na.rm=T),median(MED$WFT[MED$Cycle==0],na.rm=T),median(MED$WFTD[MED$Cycle==0],na.rm=T),median(MED$WD[MED$Cycle==0],na.rm=T),median(MED$WDFT[MED$Cycle==0]))
C1<-c(median(MED$DFT[MED$Cycle==1],na.rm=T),median(MED$WFT[MED$Cycle==1],na.rm=T),median(MED$WFTD[MED$Cycle==1],na.rm=T),median(MED$WD[MED$Cycle==1],na.rm=T),median(MED$WDFT[MED$Cycle==1]))
C2<-c(median(MED$DFT[MED$Cycle==2],na.rm=T),median(MED$WFT[MED$Cycle==2],na.rm=T),median(MED$WFTD[MED$Cycle==2],na.rm=T),median(MED$WD[MED$Cycle==2],na.rm=T),median(MED$WDFT[MED$Cycle==2]))
C3<-c(median(MED$DFT[MED$Cycle==3],na.rm=T),median(MED$WFT[MED$Cycle==3],na.rm=T),median(MED$WFTD[MED$Cycle==3],na.rm=T),median(MED$WD[MED$Cycle==3],na.rm=T),median(MED$WDFT[MED$Cycle==3]))
C4<-c(median(MED$DFT[MED$Cycle==4],na.rm=T),median(MED$WFT[MED$Cycle==4],na.rm=T),median(MED$WFTD[MED$Cycle==4],na.rm=T),median(MED$WD[MED$Cycle==4],na.rm=T),median(MED$WDFT[MED$Cycle==4]))
C5<-c(median(MED$DFT[MED$Cycle==5],na.rm=T),median(MED$WFT[MED$Cycle==5],na.rm=T),median(MED$WFTD[MED$Cycle==5],na.rm=T),median(MED$WD[MED$Cycle==5],na.rm=T),median(MED$WDFT[MED$Cycle==5]))
C6<-c(median(MED$DFT[MED$Cycle==6],na.rm=T),median(MED$WFT[MED$Cycle==6],na.rm=T),median(MED$WFTD[MED$Cycle==6],na.rm=T),median(MED$WD[MED$Cycle==6],na.rm=T),median(MED$WDFT[MED$Cycle==6]))
C7<-c(median(MED$DFT[MED$Cycle==7],na.rm=T),median(MED$WFT[MED$Cycle==7],na.rm=T),median(MED$WFTD[MED$Cycle==7],na.rm=T),median(MED$WD[MED$Cycle==7],na.rm=T),median(MED$WDFT[MED$Cycle==7]))
C8<-c(median(MED$DFT[MED$Cycle==8],na.rm=T),median(MED$WFT[MED$Cycle==8],na.rm=T),median(MED$WFTD[MED$Cycle==8],na.rm=T),median(MED$WD[MED$Cycle==8],na.rm=T),median(MED$WDFT[MED$Cycle==8]))
C9<-c(median(MED$DFT[MED$Cycle==9],na.rm=T),median(MED$WFT[MED$Cycle==9],na.rm=T),median(MED$WFTD[MED$Cycle==9],na.rm=T),median(MED$WD[MED$Cycle==9],na.rm=T),median(MED$WDFT[MED$Cycle==9]))
C10<-c(median(MED$DFT[MED$Cycle==10],na.rm=T),median(MED$WFT[MED$Cycle==10],na.rm=T),median(MED$WFTD[MED$Cycle==10],na.rm=T),median(MED$WD[MED$Cycle==10],na.rm=T),median(MED$WDFT[MED$Cycle==10]))
C11<-c(median(MED$DFT[MED$Cycle==11],na.rm=T),median(MED$WFT[MED$Cycle==11],na.rm=T),median(MED$WFTD[MED$Cycle==11],na.rm=T),median(MED$WD[MED$Cycle==11],na.rm=T),median(MED$WDFT[MED$Cycle==11]))

M0<-c(min(MED$DFT[MED$Cycle==0],na.rm=T),min(MED$WFT[MED$Cycle==0],na.rm=T),min(MED$WFTD[MED$Cycle==0],na.rm=T),min(MED$WD[MED$Cycle==0],na.rm=T),min(MED$WDFT[MED$Cycle==0]))
M1<-c(min(MED$DFT[MED$Cycle==1],na.rm=T),min(MED$WFT[MED$Cycle==1],na.rm=T),min(MED$WFTD[MED$Cycle==1],na.rm=T),min(MED$WD[MED$Cycle==1],na.rm=T),min(MED$WDFT[MED$Cycle==1]))
M2<-c(min(MED$DFT[MED$Cycle==2],na.rm=T),min(MED$WFT[MED$Cycle==2],na.rm=T),min(MED$WFTD[MED$Cycle==2],na.rm=T),min(MED$WD[MED$Cycle==2],na.rm=T),min(MED$WDFT[MED$Cycle==2]))
M3<-c(min(MED$DFT[MED$Cycle==3],na.rm=T),min(MED$WFT[MED$Cycle==3],na.rm=T),min(MED$WFTD[MED$Cycle==3],na.rm=T),min(MED$WD[MED$Cycle==3],na.rm=T),min(MED$WDFT[MED$Cycle==3]))
M4<-c(min(MED$DFT[MED$Cycle==4],na.rm=T),min(MED$WFT[MED$Cycle==4],na.rm=T),min(MED$WFTD[MED$Cycle==4],na.rm=T),min(MED$WD[MED$Cycle==4],na.rm=T),min(MED$WDFT[MED$Cycle==4]))
M5<-c(min(MED$DFT[MED$Cycle==5],na.rm=T),min(MED$WFT[MED$Cycle==5],na.rm=T),min(MED$WFTD[MED$Cycle==5],na.rm=T),min(MED$WD[MED$Cycle==5],na.rm=T),min(MED$WDFT[MED$Cycle==5]))
M6<-c(min(MED$DFT[MED$Cycle==6],na.rm=T),min(MED$WFT[MED$Cycle==6],na.rm=T),min(MED$WFTD[MED$Cycle==6],na.rm=T),min(MED$WD[MED$Cycle==6],na.rm=T),min(MED$WDFT[MED$Cycle==6]))
M7<-c(min(MED$DFT[MED$Cycle==7],na.rm=T),min(MED$WFT[MED$Cycle==7],na.rm=T),min(MED$WFTD[MED$Cycle==7],na.rm=T),min(MED$WD[MED$Cycle==7],na.rm=T),min(MED$WDFT[MED$Cycle==7]))
M8<-c(min(MED$DFT[MED$Cycle==8],na.rm=T),min(MED$WFT[MED$Cycle==8],na.rm=T),min(MED$WFTD[MED$Cycle==8],na.rm=T),min(MED$WD[MED$Cycle==8],na.rm=T),min(MED$WDFT[MED$Cycle==8]))
M9<-c(min(MED$DFT[MED$Cycle==9],na.rm=T),min(MED$WFT[MED$Cycle==9],na.rm=T),min(MED$WFTD[MED$Cycle==9],na.rm=T),min(MED$WD[MED$Cycle==9],na.rm=T),min(MED$WDFT[MED$Cycle==9]))
M10<-c(min(MED$DFT[MED$Cycle==10],na.rm=T),min(MED$WFT[MED$Cycle==10],na.rm=T),min(MED$WFTD[MED$Cycle==10],na.rm=T),min(MED$WD[MED$Cycle==10],na.rm=T),min(MED$WDFT[MED$Cycle==10]))
M11<-c(min(MED$DFT[MED$Cycle==11],na.rm=T),min(MED$WFT[MED$Cycle==11],na.rm=T),min(MED$WFTD[MED$Cycle==11],na.rm=T),min(MED$WD[MED$Cycle==11],na.rm=T),min(MED$WDFT[MED$Cycle==11]))

Max0<-c(max(MED$DFT[MED$Cycle==0],na.rm=T),max(MED$WFT[MED$Cycle==0],na.rm=T),max(MED$WFTD[MED$Cycle==0],na.rm=T),max(MED$WD[MED$Cycle==0],na.rm=T),max(MED$WDFT[MED$Cycle==0]))
Max1<-c(max(MED$DFT[MED$Cycle==1],na.rm=T),max(MED$WFT[MED$Cycle==1],na.rm=T),max(MED$WFTD[MED$Cycle==1],na.rm=T),max(MED$WD[MED$Cycle==1],na.rm=T),max(MED$WDFT[MED$Cycle==1]))
Max2<-c(max(MED$DFT[MED$Cycle==2],na.rm=T),max(MED$WFT[MED$Cycle==2],na.rm=T),max(MED$WFTD[MED$Cycle==2],na.rm=T),max(MED$WD[MED$Cycle==2],na.rm=T),max(MED$WDFT[MED$Cycle==2]))
Max3<-c(max(MED$DFT[MED$Cycle==3],na.rm=T),max(MED$WFT[MED$Cycle==3],na.rm=T),max(MED$WFTD[MED$Cycle==3],na.rm=T),max(MED$WD[MED$Cycle==3],na.rm=T),max(MED$WDFT[MED$Cycle==3]))
Max4<-c(max(MED$DFT[MED$Cycle==4],na.rm=T),max(MED$WFT[MED$Cycle==4],na.rm=T),max(MED$WFTD[MED$Cycle==4],na.rm=T),max(MED$WD[MED$Cycle==4],na.rm=T),max(MED$WDFT[MED$Cycle==4]))
Max5<-c(max(MED$DFT[MED$Cycle==5],na.rm=T),max(MED$WFT[MED$Cycle==5],na.rm=T),max(MED$WFTD[MED$Cycle==5],na.rm=T),max(MED$WD[MED$Cycle==5],na.rm=T),max(MED$WDFT[MED$Cycle==5]))
Max6<-c(max(MED$DFT[MED$Cycle==6],na.rm=T),max(MED$WFT[MED$Cycle==6],na.rm=T),max(MED$WFTD[MED$Cycle==6],na.rm=T),max(MED$WD[MED$Cycle==6],na.rm=T),max(MED$WDFT[MED$Cycle==6]))
Max7<-c(max(MED$DFT[MED$Cycle==7],na.rm=T),max(MED$WFT[MED$Cycle==7],na.rm=T),max(MED$WFTD[MED$Cycle==7],na.rm=T),max(MED$WD[MED$Cycle==7],na.rm=T),max(MED$WDFT[MED$Cycle==7]))
Max8<-c(max(MED$DFT[MED$Cycle==8],na.rm=T),max(MED$WFT[MED$Cycle==8],na.rm=T),max(MED$WFTD[MED$Cycle==8],na.rm=T),max(MED$WD[MED$Cycle==8],na.rm=T),max(MED$WDFT[MED$Cycle==8]))
Max9<-c(max(MED$DFT[MED$Cycle==9],na.rm=T),max(MED$WFT[MED$Cycle==9],na.rm=T),max(MED$WFTD[MED$Cycle==9],na.rm=T),max(MED$WD[MED$Cycle==9],na.rm=T),max(MED$WDFT[MED$Cycle==9]))
Max10<-c(max(MED$DFT[MED$Cycle==10],na.rm=T),max(MED$WFT[MED$Cycle==10],na.rm=T),max(MED$WFTD[MED$Cycle==10],na.rm=T),max(MED$WD[MED$Cycle==10],na.rm=T),max(MED$WDFT[MED$Cycle==10]))
Max11<-c(max(MED$DFT[MED$Cycle==11],na.rm=T),max(MED$WFT[MED$Cycle==11],na.rm=T),max(MED$WFTD[MED$Cycle==11],na.rm=T),max(MED$WD[MED$Cycle==11],na.rm=T),max(MED$WDFT[MED$Cycle==11]))


Q1_0<-c(unlist(quantile(MED$DFT[MED$Cycle==0],0.25,na.rm=T)),unlist(quantile(MED$WFT[MED$Cycle==0],0.25,na.rm=T)),unlist(quantile(MED$WFTD[MED$Cycle==0],0.25,na.rm=T)),unlist(quantile(MED$WD[MED$Cycle==0],0.25,na.rm=T)),unlist(quantile(MED$WDFT[MED$Cycle==0],0.25)))
Q1_1<-c(unlist(quantile(MED$DFT[MED$Cycle==1],0.25,na.rm=T)),unlist(quantile(MED$WFT[MED$Cycle==1],0.25,na.rm=T)),unlist(quantile(MED$WFTD[MED$Cycle==1],0.25,na.rm=T)),unlist(quantile(MED$WD[MED$Cycle==1],0.25,na.rm=T)),unlist(quantile(MED$WDFT[MED$Cycle==1],0.25)))
Q1_2<-c(unlist(quantile(MED$DFT[MED$Cycle==2],0.25,na.rm=T)),unlist(quantile(MED$WFT[MED$Cycle==2],0.25,na.rm=T)),unlist(quantile(MED$WFTD[MED$Cycle==2],0.25,na.rm=T)),unlist(quantile(MED$WD[MED$Cycle==2],0.25,na.rm=T)),unlist(quantile(MED$WDFT[MED$Cycle==2],0.25)))
Q1_3<-c(unlist(quantile(MED$DFT[MED$Cycle==3],0.25,na.rm=T)),unlist(quantile(MED$WFT[MED$Cycle==3],0.25,na.rm=T)),unlist(quantile(MED$WFTD[MED$Cycle==3],0.25,na.rm=T)),unlist(quantile(MED$WD[MED$Cycle==3],0.25,na.rm=T)),unlist(quantile(MED$WDFT[MED$Cycle==3],0.25)))
Q1_4<-c(unlist(quantile(MED$DFT[MED$Cycle==4],0.25,na.rm=T)),unlist(quantile(MED$WFT[MED$Cycle==4],0.25,na.rm=T)),unlist(quantile(MED$WFTD[MED$Cycle==4],0.25,na.rm=T)),unlist(quantile(MED$WD[MED$Cycle==4],0.25,na.rm=T)),unlist(quantile(MED$WDFT[MED$Cycle==4],0.25)))
Q1_5<-c(unlist(quantile(MED$DFT[MED$Cycle==5],0.25,na.rm=T)),unlist(quantile(MED$WFT[MED$Cycle==5],0.25,na.rm=T)),unlist(quantile(MED$WFTD[MED$Cycle==5],0.25,na.rm=T)),unlist(quantile(MED$WD[MED$Cycle==5],0.25,na.rm=T)),unlist(quantile(MED$WDFT[MED$Cycle==5],0.25)))
Q1_6<-c(unlist(quantile(MED$DFT[MED$Cycle==6],0.25,na.rm=T)),unlist(quantile(MED$WFT[MED$Cycle==6],0.25,na.rm=T)),unlist(quantile(MED$WFTD[MED$Cycle==6],0.25,na.rm=T)),unlist(quantile(MED$WD[MED$Cycle==6],0.25,na.rm=T)),unlist(quantile(MED$WDFT[MED$Cycle==6],0.25)))
Q1_7<-c(unlist(quantile(MED$DFT[MED$Cycle==7],0.25,na.rm=T)),unlist(quantile(MED$WFT[MED$Cycle==7],0.25,na.rm=T)),unlist(quantile(MED$WFTD[MED$Cycle==7],0.25,na.rm=T)),unlist(quantile(MED$WD[MED$Cycle==7],0.25,na.rm=T)),unlist(quantile(MED$WDFT[MED$Cycle==7],0.25)))
Q1_8<-c(unlist(quantile(MED$DFT[MED$Cycle==8],0.25,na.rm=T)),unlist(quantile(MED$WFT[MED$Cycle==8],0.25,na.rm=T)),unlist(quantile(MED$WFTD[MED$Cycle==8],0.25,na.rm=T)),unlist(quantile(MED$WD[MED$Cycle==8],0.25,na.rm=T)),unlist(quantile(MED$WDFT[MED$Cycle==8],0.25)))
Q1_9<-c(unlist(quantile(MED$DFT[MED$Cycle==9],0.25,na.rm=T)),unlist(quantile(MED$WFT[MED$Cycle==9],0.25,na.rm=T)),unlist(quantile(MED$WFTD[MED$Cycle==9],0.25,na.rm=T)),unlist(quantile(MED$WD[MED$Cycle==9],0.25,na.rm=T)),unlist(quantile(MED$WDFT[MED$Cycle==9],0.25)))
Q1_10<-c(unlist(quantile(MED$DFT[MED$Cycle==10],0.25,na.rm=T)),unlist(quantile(MED$WFT[MED$Cycle==10],0.25,na.rm=T)),unlist(quantile(MED$WFTD[MED$Cycle==10],0.25,na.rm=T)),unlist(quantile(MED$WD[MED$Cycle==10],0.25,na.rm=T)),unlist(quantile(MED$WDFT[MED$Cycle==10],0.25)))
Q1_11<-c(unlist(quantile(MED$DFT[MED$Cycle==11],0.25,na.rm=T)),unlist(quantile(MED$WFT[MED$Cycle==11],0.25,na.rm=T)),unlist(quantile(MED$WFTD[MED$Cycle==11],0.25,na.rm=T)),unlist(quantile(MED$WD[MED$Cycle==11],0.25,na.rm=T)),unlist(quantile(MED$WDFT[MED$Cycle==11],0.25)))

Q2_0<-c(unlist(quantile(MED$DFT[MED$Cycle==0],0.75,na.rm=T)),unlist(quantile(MED$WFT[MED$Cycle==0],0.75,na.rm=T)),unlist(quantile(MED$WFTD[MED$Cycle==0],0.75,na.rm=T)),unlist(quantile(MED$WD[MED$Cycle==0],0.75,na.rm=T)),unlist(quantile(MED$WDFT[MED$Cycle==0],0.75)))
Q2_1<-c(unlist(quantile(MED$DFT[MED$Cycle==1],0.75,na.rm=T)),unlist(quantile(MED$WFT[MED$Cycle==1],0.75,na.rm=T)),unlist(quantile(MED$WFTD[MED$Cycle==1],0.75,na.rm=T)),unlist(quantile(MED$WD[MED$Cycle==1],0.75,na.rm=T)),unlist(quantile(MED$WDFT[MED$Cycle==1],0.75)))
Q2_2<-c(unlist(quantile(MED$DFT[MED$Cycle==2],0.75,na.rm=T)),unlist(quantile(MED$WFT[MED$Cycle==2],0.75,na.rm=T)),unlist(quantile(MED$WFTD[MED$Cycle==2],0.75,na.rm=T)),unlist(quantile(MED$WD[MED$Cycle==2],0.75,na.rm=T)),unlist(quantile(MED$WDFT[MED$Cycle==2],0.75)))
Q2_3<-c(unlist(quantile(MED$DFT[MED$Cycle==3],0.75,na.rm=T)),unlist(quantile(MED$WFT[MED$Cycle==3],0.75,na.rm=T)),unlist(quantile(MED$WFTD[MED$Cycle==3],0.75,na.rm=T)),unlist(quantile(MED$WD[MED$Cycle==3],0.75,na.rm=T)),unlist(quantile(MED$WDFT[MED$Cycle==3],0.75)))
Q2_4<-c(unlist(quantile(MED$DFT[MED$Cycle==4],0.75,na.rm=T)),unlist(quantile(MED$WFT[MED$Cycle==4],0.75,na.rm=T)),unlist(quantile(MED$WFTD[MED$Cycle==4],0.75,na.rm=T)),unlist(quantile(MED$WD[MED$Cycle==4],0.75,na.rm=T)),unlist(quantile(MED$WDFT[MED$Cycle==4],0.75)))
Q2_5<-c(unlist(quantile(MED$DFT[MED$Cycle==5],0.75,na.rm=T)),unlist(quantile(MED$WFT[MED$Cycle==5],0.75,na.rm=T)),unlist(quantile(MED$WFTD[MED$Cycle==5],0.75,na.rm=T)),unlist(quantile(MED$WD[MED$Cycle==5],0.75,na.rm=T)),unlist(quantile(MED$WDFT[MED$Cycle==5],0.75)))
Q2_6<-c(unlist(quantile(MED$DFT[MED$Cycle==6],0.75,na.rm=T)),unlist(quantile(MED$WFT[MED$Cycle==6],0.75,na.rm=T)),unlist(quantile(MED$WFTD[MED$Cycle==6],0.75,na.rm=T)),unlist(quantile(MED$WD[MED$Cycle==6],0.75,na.rm=T)),unlist(quantile(MED$WDFT[MED$Cycle==6],0.75)))
Q2_7<-c(unlist(quantile(MED$DFT[MED$Cycle==7],0.75,na.rm=T)),unlist(quantile(MED$WFT[MED$Cycle==7],0.75,na.rm=T)),unlist(quantile(MED$WFTD[MED$Cycle==7],0.75,na.rm=T)),unlist(quantile(MED$WD[MED$Cycle==7],0.75,na.rm=T)),unlist(quantile(MED$WDFT[MED$Cycle==7],0.75)))
Q2_8<-c(unlist(quantile(MED$DFT[MED$Cycle==8],0.75,na.rm=T)),unlist(quantile(MED$WFT[MED$Cycle==8],0.75,na.rm=T)),unlist(quantile(MED$WFTD[MED$Cycle==8],0.75,na.rm=T)),unlist(quantile(MED$WD[MED$Cycle==8],0.75,na.rm=T)),unlist(quantile(MED$WDFT[MED$Cycle==8],0.75)))
Q2_9<-c(unlist(quantile(MED$DFT[MED$Cycle==9],0.75,na.rm=T)),unlist(quantile(MED$WFT[MED$Cycle==9],0.75,na.rm=T)),unlist(quantile(MED$WFTD[MED$Cycle==9],0.75,na.rm=T)),unlist(quantile(MED$WD[MED$Cycle==9],0.75,na.rm=T)),unlist(quantile(MED$WDFT[MED$Cycle==9],0.75)))
Q2_10<-c(unlist(quantile(MED$DFT[MED$Cycle==10],0.75,na.rm=T)),unlist(quantile(MED$WFT[MED$Cycle==10],0.75,na.rm=T)),unlist(quantile(MED$WFTD[MED$Cycle==10],0.75,na.rm=T)),unlist(quantile(MED$WD[MED$Cycle==10],0.75,na.rm=T)),unlist(quantile(MED$WDFT[MED$Cycle==10],0.75)))
Q2_11<-c(unlist(quantile(MED$DFT[MED$Cycle==11],0.75,na.rm=T)),unlist(quantile(MED$WFT[MED$Cycle==11],0.75,na.rm=T)),unlist(quantile(MED$WFTD[MED$Cycle==11],0.75,na.rm=T)),unlist(quantile(MED$WD[MED$Cycle==11],0.75,na.rm=T)),unlist(quantile(MED$WDFT[MED$Cycle==11],0.75)))

mean(c(sd(MED$WFT[MED$Cycle==11]),sd(MED$WFT[MED$Cycle==10]),sd(MED$WFT[MED$Cycle==9]),sd(MED$WFT[MED$Cycle==8]),sd(MED$WFT[MED$Cycle==7]),sd(MED$WFT[MED$Cycle==6]),sd(MED$WFT[MED$Cycle==5]),sd(MED$WFT[MED$Cycle==4]),sd(MED$WFT[MED$Cycle==3]),sd(MED$WFT[MED$Cycle==2]),sd(MED$WFT[MED$Cycle==1]),sd(MED$WFT[MED$Cycle==0])))
mean(c(sd(MED$WD[MED$Cycle==11],na.rm=T),sd(MED$WD[MED$Cycle==10],na.rm=T),sd(MED$WD[MED$Cycle==9]),sd(MED$WD[MED$Cycle==8]),sd(MED$WD[MED$Cycle==7]),sd(MED$WD[MED$Cycle==6]),sd(MED$WD[MED$Cycle==5]),sd(MED$WD[MED$Cycle==4]),sd(MED$WD[MED$Cycle==3]),sd(MED$WD[MED$Cycle==2]),sd(MED$WD[MED$Cycle==1]),sd(MED$WD[MED$Cycle==0])))
mean(c(sd(MED$WFTD[MED$Cycle==11],na.rm=T),sd(MED$WFTD[MED$Cycle==10],na.rm=T),sd(MED$WFTD[MED$Cycle==9]),sd(MED$WFTD[MED$Cycle==8]),sd(MED$WFTD[MED$Cycle==7]),sd(MED$WFTD[MED$Cycle==6]),sd(MED$WFTD[MED$Cycle==5]),sd(MED$WFTD[MED$Cycle==4]),sd(MED$WFTD[MED$Cycle==3]),sd(MED$WFTD[MED$Cycle==2]),sd(MED$WFTD[MED$Cycle==1]),sd(MED$WFTD[MED$Cycle==0])))
mean(c(sd(MED$WDFT[MED$Cycle==11]),sd(MED$WDFT[MED$Cycle==10]),sd(MED$WDFT[MED$Cycle==9]),sd(MED$WDFT[MED$Cycle==8]),sd(MED$WDFT[MED$Cycle==7]),sd(MED$WDFT[MED$Cycle==6]),sd(MED$WDFT[MED$Cycle==5]),sd(MED$WDFT[MED$Cycle==4]),sd(MED$WDFT[MED$Cycle==3]),sd(MED$WDFT[MED$Cycle==2]),sd(MED$WDFT[MED$Cycle==1]),sd(MED$WDFT[MED$Cycle==0])))

pdf(file = "/Users/katya/Google Drive/PhD/Hydrophobicity/ForGraphs/Hydrophobicity.pdf",   # The directory you want to save the file in
    width = 6.3, # The width of the plot in inches
    height = 4) # The height of the plot in inches   
ggplot(MEDmelt, aes( factor(as.character(Cycle), levels=c("0","1", "2", "3", "4",'5','6','7','8','9','10','11')), value, fill=variable))+geom_boxplot(ymin=c(M0,M1,M2,M3,M4,M5,M6,M7,M8,M9,M10,M11),ymax=c(Max0,Max1,Max2,Max3,Max4,Max5,Max6,Max7,Max8,Max9,Max10,Max11),middle=c(C0,C1,C2,C3,C4,C5,C6,C7,C8,C9,C10,C11),lower=as.matrix(as.data.frame(c(Q1_0,Q1_1,Q1_2,Q1_3,Q1_4,Q1_5,Q1_6,Q1_7,Q1_8,Q1_9,Q1_10,Q1_11))),upper=as.matrix(as.data.frame(c(Q2_0,Q2_1,Q2_2,Q2_3,Q2_4,Q2_5,Q2_6,Q2_7,Q2_8,Q2_9,Q2_10,Q2_11))),width=0.7,lwd=0.25, outlier.shape=NA) +
  scale_fill_manual(values=c("#ffffd4", "#fed98e", "#fe9929","#d95f0e", "#993404"))+theme_classic()+theme(panel.border = element_rect(colour = "black", fill=NA, size=1.5),axis.text = element_text(size=12), axis.title=element_text(size=12), legend.text = element_text(size = 12))+
  ylab('MED [% ethanol]')+xlab('Cycle')+geom_hline(yintercept=6.5, linetype="dashed", color = "gray")+geom_hline(yintercept=17.5, linetype="dashed", color = "black")+
  scale_colour_manual(values=c("#ffffd4", "#fed98e", "#fe9929","#d95f0e", "#993404"))+
  theme(legend.text=element_text(size=20))+
  theme(legend.key.size = unit(1,"line"))+
  theme(axis.text = element_text(size=12))+
  theme(legend.position = "none")

dev.off()


plot_Data <- ddply(MEDmelt, .(Cycle), mutate, Q1=quantile(value, 1/4), Q3=quantile(value, 3/4), IQR=Q3-Q1, upper.limit=Q3+1.5*IQR, lower.limit=Q1-1.5*IQR)
  ggplot()+geom_point(data=plot_Data[plot_Data$value > plot_Data$upper.limit | plot_Data$value < plot_Data$lower.limit,], aes(x=factor(Cycle), y=value, col='purple'))


#ffffd4
#fed98e
#fe9929
#d95f0e
#993404

ggplot()+geom_boxplot( aes(df2$Cycle, df2$value, group=df2$Cycle), middle = SumDT$means, lower=unlist(SumDT$q025), upper=unlist(SumDT$q975))+xlab('Cycle')+ylab('days')+theme_classic(base_size = 20)+
  theme(axis.text=element_text(size=16))+scale_x_continuous(breaks = scales::pretty_breaks(n = 10))+coord_flip()+ylim(-3,190)+
  theme(panel.border = element_rect(colour = "black", fill=NA, size=1))+
  stat_summary(fun.y = mean, geom = "errorbar")

pdf(file = "/Users/katya/Google Drive/PhD/Hydrophobicity/ForGraphs/HydrophobicityDays.pdf",   # The directory you want to save the file in
    width = 6.5, # The width of the plot in inches
    height = 3) # The height of the plot in inches        
MEDdays<-read.csv('/Users/katya/Google Drive/PhD/Hydrophobicity/Hydrophobicity Final/ResultsRDays.csv', header=T)


MEDdays$means<-rowMeans(MEDdays[,3:7], na.rm=T)
for (i in 1:12){
  MEDdays$q025[i]<-quantile(MEDdays[i,3:7],.25, na.rm=T)
  MEDdays$q975[i]<-quantile(MEDdays[i,3:7],.75, na.rm=T)}



MEDmeltD<-melt(MEDdays, id='Days')
MEDmeltD$Cycle<-rep(c(0:11),6)
MEDmeltD$Batch<-c(rep(1,12),rep(2,12),rep(3,12),rep(4,12),rep(5,12),rep(6,12))
MEDmeltD<-MEDmeltD[!is.na(MEDmeltD$value),]

ggplot(MEDmeltD, aes( Days, value, fill=interaction(variable, Days)))+geom_boxplot(width=6, lwd=0.2) + xlim(-3,190)+
  scale_fill_manual(values=rep(c("#ffffd4", "#fed98e", "#fe9929","#d95f0e", "#993404"),30))+theme_classic()+theme(panel.border = element_rect(colour = "black", fill=NA, size=1),axis.text = element_text(size=16), axis.title=element_text(size=16), legend.text = element_text(size = 16))+
  ylab('MED [% ethanol]')+xlab('Cycle')+geom_hline(yintercept=6.5, linetype="dashed", color = "gray")+geom_hline(yintercept=17.5, linetype="dashed", color = "black")+ theme(legend.position = "none")
dev.off()

ggplot(MEDmeltD, aes(middle = MEDmeltD$means,lower=unlist(MEDmeltD$q025), upper=unlist(MEDmeltD$q975), Days, value, fill=interaction(variable, Days)))+geom_boxplot(width=6, lwd=0.2) + xlim(-3,190)+
  scale_fill_manual(values=rep(c("#ffffd4", "#fed98e", "#fe9929","#d95f0e", "#993404"),30))+theme_classic()+theme(panel.border = element_rect(colour = "black", fill=NA, size=1),axis.text = element_text(size=16), axis.title=element_text(size=16), legend.text = element_text(size = 16))+
  ylab('MED [% ethanol]')+xlab('Cycle')+geom_hline(yintercept=6.5, linetype="dashed", color = "gray")+geom_hline(yintercept=17.5, linetype="dashed", color = "black")+ theme(legend.position = "none")


library(reshape)
Carbon<-read.csv('/Users/katya/Google Drive/PhD/Hydrophobicity/Hydrophobicity Final/CarbonCycle.csv', header=T)
Carbon2<-Carbon[Carbon$Flag!=1 & Carbon$FourthCycle!=1,]
Error<-abs(Carbon$Carbon[Carbon$Flag==1 & Carbon$FourthCycle!=1]-Carbon$Carbon[Carbon$Flag2==1 & Carbon$FourthCycle!=1])
quantile(Error,.025)
quantile(Error,.975)

Carbon2$means<-rowMeans(MEDdays[,3:7], na.rm=T)
for (i in 1:12){
  Carbon2$q025[i]<-quantile(Carbon2[i,3:7],.25, na.rm=T)
  Carbon2$q975[i]<-quantile(Carbon2[i,3:7],.75, na.rm=T)}


pdf(file = "/Users/katya/Google Drive/PhD/Hydrophobicity/ForGraphs/Carbon.pdf",   # The directory you want to save the file in
    width = 6.5, # The width of the plot in inches
    height = 3) # The height of the plot in inches     
ggplot(Carbon2, aes( factor(Cycle), Carbon, fill=Treatment))+geom_boxplot(width=0.7, lwd=0.25) +
  scale_fill_manual(values=c("#ffffd4", "#fed98e", "#fe9929","#d95f0e", "#993404"))+theme_classic()+theme(panel.border = element_rect(colour = "black", fill=NA, size=1.5),axis.text = element_text(size=12), axis.title=element_text(size=12), legend.text = element_text(size = 12))+
  ylab('SOM [%]')+xlab('Cycle')+
  scale_colour_manual(values=c("#ffffd4", "#fed98e", "#fe9929","#d95f0e", "#993404"))+
  theme(legend.text=element_text(size=12))+
  theme(legend.key.size = unit(1,"line"))+
  theme(axis.text = element_text(size=12))+
  theme(legend.position = "none")
dev.off()
#geom_smooth(aes(x=Cycle,y=Carbon),method = "lm", se = FALSE)

#2008 wftx7 WDx1 ->2009 WDx1 wftx2
#2009 wd wftx8 wd ->2010 wd
#2010 wd wftx5 wdx3 ->2011 wftd wft
#2011 wftd wftx2 wftd wdx5 ->2012 wd wft
#2012 wd wftx3 wftdx1 wdx6
#2013 wd wd wd wd wft wft wft wd wd wd wd 
#2014 wftx5 wftdx1 -> 2015 wd wftx4
#2015 wd wftx7  wftd  wd ->2016 wd
#2016 wd wftx2     wftdx1  wdx2 ->2017 

     # 1    2     3     4     5    6     7    8    9   10   11
#2008 wft  wft   wft    wft  wft  wft    wft  wd   wd  wft  wft
#2009 wd   wft   wft    wft  wft  wft    wft  wft  wft  wd  wd
#2010 wd   wft   wft    wft  wft  wft    wd   wd   wd  wftd wft
#2011 wftd wft   wft    wftd wd  wd      wd   wd   wd  wd   wft
#2012 wd   wft   wft    wft  wftd  wd    wd   wd   wd  wd   wd
#2013 wd   wd    wd     wd   wft  wft    wft  wd   wd  wd   wd 
#2014 wft  wft   wft    wft  wft  wftd   wd   wft  wft wft  wft
#2015 wd   wft   wft    wft  wft  wft    wft  wft  wftd wd  wd

#good to plot 2012
#2008   wft  wft  wft  wft  wft wft   wft  wd | wd   wftd  wft
#2009   wd   wftd wft  wft  wft wft   wft  wft  wftd wd |  wd
#2010   wd   wft  wft  wft  wft wft   wftd wd   wd   wft   wft 
#2011   wft  wft  wft  wft  wft wft   wft  wft  wft  wftd  wftd  
#2012   wd   wft  wft  wft  wftd wd   wd   wd   wd   wd    wd 
#2013   wd   wd   wd   wd   wd   wftd wft  wftd wd   wd    wd  
#2014   wft  wft  wft  wft  wft  wftd|wd   wft  wft  wft   wft
#2015   wd   wft  wft  wft  wft  wft  wft  wft  wftd wd|   wd 
#2016

MEDdays<-read.csv('/Users/katya/Google Drive/PhD/Hydrophobicity/Hydrophobicity Final/ResultsRDays.csv', header=T)
MEDmeltD<-melt(MEDdays, id='Days')
MEDmeltD$Cycle<-rep(c(0:11),6)
MEDmeltD$Batch<-c(rep(1,12),rep(2,12),rep(3,12),rep(4,12),rep(5,12),rep(6,12))
MEDmeltD<-MEDmeltD[!is.na(MEDmeltD$value),]


Cycle2Date<-as.data.frame(c(rep(MEDmeltD$value[MEDmeltD$variable=='WFT'&MEDmeltD$Cycle==1],3),rep(MEDmeltD$value[MEDmeltD$variable=='WD'&MEDmeltD$Cycle==1],5)))
names(Cycle2Date)<-'Cycle1'
Cycle2Date$Cycle2<-c(rep(MEDmeltD$value[MEDmeltD$variable=='WFT'&MEDmeltD$Cycle==2],6),rep(MEDmeltD$value[MEDmeltD$variable=='WD'&MEDmeltD$Cycle==2],1),rep(MEDmeltD$value[MEDmeltD$variable=='WFTD'&MEDmeltD$Cycle==2],1))
Cycle2Date$Cycle3<-c(rep(MEDmeltD$value[MEDmeltD$variable=='WFT'&MEDmeltD$Cycle==3],7),rep(MEDmeltD$value[MEDmeltD$variable=='WD'&MEDmeltD$Cycle==3],1))
Cycle2Date$Cycle4<-c(rep(MEDmeltD$value[MEDmeltD$variable=='WFT'&MEDmeltD$Cycle==4],7),rep(MEDmeltD$value[MEDmeltD$variable=='WD'&MEDmeltD$Cycle==4],1))
Cycle2Date$Cycle5<-c(rep(MEDmeltD$value[MEDmeltD$variable=='WFT'&MEDmeltD$Cycle==5],6),rep(MEDmeltD$value[MEDmeltD$variable=='WFTD'&MEDmeltD$Cycle==5],1),rep(MEDmeltD$value[MEDmeltD$variable=='WD'&MEDmeltD$Cycle==5],1))
Cycle2Date$Cycle6<-c(rep(MEDmeltD$value[MEDmeltD$variable=='WFT'&MEDmeltD$Cycle==6],5),rep(MEDmeltD$value[MEDmeltD$variable=='WFTD'&MEDmeltD$Cycle==6],2),rep(MEDmeltD$value[MEDmeltD$variable=='WD'&MEDmeltD$Cycle==6],1))
Cycle2Date$Cycle7<-c(rep(MEDmeltD$value[MEDmeltD$variable=='WFT'&MEDmeltD$Cycle==7],5),rep(MEDmeltD$value[MEDmeltD$variable=='WFTD'&MEDmeltD$Cycle==7],1),rep(MEDmeltD$value[MEDmeltD$variable=='WD'&MEDmeltD$Cycle==7],2))
Cycle2Date$Cycle8<-c(rep(MEDmeltD$value[MEDmeltD$variable=='WFT'&MEDmeltD$Cycle==8],4),rep(MEDmeltD$value[MEDmeltD$variable=='WFTD'&MEDmeltD$Cycle==8],1),rep(MEDmeltD$value[MEDmeltD$variable=='WD'&MEDmeltD$Cycle==8],3))
Cycle2Date$Cycle9<-c(rep(MEDmeltD$value[MEDmeltD$variable=='WFT'&MEDmeltD$Cycle==9],2),rep(MEDmeltD$value[MEDmeltD$variable=='WFTD'&MEDmeltD$Cycle==9],2),rep(MEDmeltD$value[MEDmeltD$variable=='WD'&MEDmeltD$Cycle==9],4))
Cycle2Date$Cycle10<-Cycle2Date$Cycle1*NA
Cycle2Date$Cycle10[1:47]<-c(rep(MEDmeltD$value[MEDmeltD$variable=='WFT'&MEDmeltD$Cycle==10],2),rep(MEDmeltD$value[MEDmeltD$variable=='WFTD'&MEDmeltD$Cycle==10],2),rep(MEDmeltD$value[MEDmeltD$variable=='WD'&MEDmeltD$Cycle==10],4))
Cycle2Date$Cycle11<-Cycle2Date$Cycle1*NA
Cycle2Date$Cycle11[1:44]<-c(rep(MEDmeltD$value[MEDmeltD$variable=='WFT'&MEDmeltD$Cycle==11],3),rep(MEDmeltD$value[MEDmeltD$variable=='WFTD'&MEDmeltD$Cycle==11],1),rep(MEDmeltD$value[MEDmeltD$variable=='WD'&MEDmeltD$Cycle==11],4))
#Cycle2Date$Cycle0<-Cycle2Date$Cycle1*NA
#Cycle2Date$Cycle0[1:18]<-(c(rep(MEDmeltD$value[MEDmeltD$variable=='WFT'&MEDmeltD$Cycle==0],1),rep(MEDmeltD$value[MEDmeltD$variable=='WD'&MEDmeltD$Cycle==0],1),rep(MEDmeltD$value[MEDmeltD$variable=='WFTD'&MEDmeltD$Cycle==0],1)))

Cycle2Date<-as.data.frame(t(Cycle2Date))
Cycle2Date$Days<-c(SumDT$median[1:11])
Cycle2DateMelt<-melt(Cycle2Date, id='Days')

pdf(file = "/Users/katya/Google Drive/PhD/Hydrophobicity/ForGraphs/Cycle2Days.pdf",   # The directory you want to save the file in
    width = 6.5, # The width of the plot in inches
    height = 3) # The height of the plot in inches 

ggplot(Cycle2DateMelt, aes( Days, value, group=Days, fill="#fec44f"))+geom_boxplot(width=8)+scale_x_continuous(breaks = scales::pretty_breaks(n = 10))+xlim(-3,565)+
  scale_fill_manual(values="#fec44f")+theme_classic()+theme(panel.border = element_rect(colour = "black", fill=NA, size=1),axis.text = element_text(size=12), axis.title=element_text(size=16), legend.text = element_text(size = 12))+
  ylab('MED [% ethanol]')+xlab('Cycle')+geom_hline(yintercept=6.5, linetype="dashed", color = "gray")+geom_hline(yintercept=17.5, linetype="dashed", color = "black")+ theme(legend.position = "none")
dev.off()

pdf(file = "/Users/katya/Google Drive/PhD/Hydrophobicity/ForGraphs/GraphicalAbstract2V2.pdf",   # The directory you want to save the file in
    width = 2.36*4, # The width of the plot in inches
    height = 1.96*4) # The height of the plot in inches 

ggplot(MEDmeltD, aes( Days, value, group=Days, fill="#fec44f"))+geom_boxplot(middle=medianMED,lower=unlist(Q1MED), upper=unlist(Q2MED), ymin=minMED, ymax=maxMED, outlier.shape=NA, width=6, lwd=0.5) + xlim(-3,400)+
  scale_fill_manual(values="#fec44f")+theme_classic()+theme(panel.border = element_rect(colour = "black", fill=NA, size=1),axis.text = element_text(size=12), axis.title=element_text(size=12), legend.text = element_text(size = 12))+
  ylab('MED [% ethanol]')+xlab('Cycle')+geom_hline(yintercept=6.5, linetype="dashed", color = "gray")+geom_hline(yintercept=17.5, linetype="dashed", color = "black")+ theme(legend.position = "none")

dev.off()

ggplot(Carbon2)+geom_point(aes(x=Cycle,y=Carbon))+  geom_smooth(aes(x=Cycle,y=Carbon),method = "lm", se = FALSE)


pdf(file = "/Users/katya/Google Drive/PhD/Hydrophobicity/ForGraphs/Cycle2DaysV2.pdf",   # The directory you want to save the file in
    width = 6.5, # The width of the plot in inches
    height = 3) # The height of the plot in inches 
MEDdays<-read.csv('/Users/katya/Google Drive/PhD/Hydrophobicity/Hydrophobicity Final/Cycle2Days.csv', header=T)
MEDmeltD<-melt(MEDdays, id='Days')
MEDmeltD<-MEDmeltD[!is.na(MEDmeltD$value),]

8.5  17.5  38.0  52.5 124.0 136.5 213.5 258.5 298.5 339.0 363.5

medianMED<-c(median(MEDmeltD$value[MEDmeltD$Days==0]),median(MEDmeltD$value[MEDmeltD$Days==8.5]),median(MEDmeltD$value[MEDmeltD$Days==17.5]),median(MEDmeltD$value[MEDmeltD$Days==38.0 ]),median(MEDmeltD$value[MEDmeltD$Days==52.5]),median(MEDmeltD$value[MEDmeltD$Days==124]),median(MEDmeltD$value[MEDmeltD$Days==136.5]),median(MEDmeltD$value[MEDmeltD$Days==213.5]),median(MEDmeltD$value[MEDmeltD$Days==258.5]),median(MEDmeltD$value[MEDmeltD$Days==298.5]),median(MEDmeltD$value[MEDmeltD$Days==339.0]),median(MEDmeltD$value[MEDmeltD$Days==363.5]))
Q1MED<-c(quantile(MEDmeltD$value[MEDmeltD$Days==0],0.25),quantile(MEDmeltD$value[MEDmeltD$Days==8.5],0.25),quantile(MEDmeltD$value[MEDmeltD$Days==17.5],0.25),quantile(MEDmeltD$value[MEDmeltD$Days==38.0 ],0.25),quantile(MEDmeltD$value[MEDmeltD$Days==52.5],0.25),quantile(MEDmeltD$value[MEDmeltD$Days==124],0.25),quantile(MEDmeltD$value[MEDmeltD$Days==136.5],0.25),quantile(MEDmeltD$value[MEDmeltD$Days==213.5],0.25),quantile(MEDmeltD$value[MEDmeltD$Days==258.5],0.25),quantile(MEDmeltD$value[MEDmeltD$Days==298.5],0.25),quantile(MEDmeltD$value[MEDmeltD$Days==339.0],0.25),quantile(MEDmeltD$value[MEDmeltD$Days==363.5],0.25))
Q2MED<-c(quantile(MEDmeltD$value[MEDmeltD$Days==0],0.75),quantile(MEDmeltD$value[MEDmeltD$Days==8.5],0.75),quantile(MEDmeltD$value[MEDmeltD$Days==17.5],0.75),quantile(MEDmeltD$value[MEDmeltD$Days==38.0 ],0.75),quantile(MEDmeltD$value[MEDmeltD$Days==52.5],0.75),quantile(MEDmeltD$value[MEDmeltD$Days==124],0.75),quantile(MEDmeltD$value[MEDmeltD$Days==136.5],0.75),quantile(MEDmeltD$value[MEDmeltD$Days==213.5],0.75),quantile(MEDmeltD$value[MEDmeltD$Days==258.5],0.75),quantile(MEDmeltD$value[MEDmeltD$Days==298.5],0.75),quantile(MEDmeltD$value[MEDmeltD$Days==339.0],0.75),quantile(MEDmeltD$value[MEDmeltD$Days==363.5],0.75))

maxMED<-c(max(MEDmeltD$value[MEDmeltD$Days==0]),max(MEDmeltD$value[MEDmeltD$Days==8.5]),max(MEDmeltD$value[MEDmeltD$Days==17.5]),max(MEDmeltD$value[MEDmeltD$Days==38.0 ]),max(MEDmeltD$value[MEDmeltD$Days==52.5]),max(MEDmeltD$value[MEDmeltD$Days==124]),max(MEDmeltD$value[MEDmeltD$Days==136.5]),max(MEDmeltD$value[MEDmeltD$Days==213.5]),max(MEDmeltD$value[MEDmeltD$Days==258.5]),max(MEDmeltD$value[MEDmeltD$Days==298.5]),max(MEDmeltD$value[MEDmeltD$Days==339.0]),max(MEDmeltD$value[MEDmeltD$Days==363.5]))
minMED<-c(min(MEDmeltD$value[MEDmeltD$Days==0]),min(MEDmeltD$value[MEDmeltD$Days==8.5]),min(MEDmeltD$value[MEDmeltD$Days==17.5]),min(MEDmeltD$value[MEDmeltD$Days==38.0 ]),min(MEDmeltD$value[MEDmeltD$Days==52.5]),min(MEDmeltD$value[MEDmeltD$Days==124]),min(MEDmeltD$value[MEDmeltD$Days==136.5]),min(MEDmeltD$value[MEDmeltD$Days==213.5]),min(MEDmeltD$value[MEDmeltD$Days==258.5]),min(MEDmeltD$value[MEDmeltD$Days==298.5]),min(MEDmeltD$value[MEDmeltD$Days==339.0]),min(MEDmeltD$value[MEDmeltD$Days==363.5]))



ggplot(MEDmeltD, aes( Days, value, group=Days, fill="#fec44f"))+geom_boxplot(middle=medianMED,lower=unlist(Q1MED), upper=unlist(Q2MED), ymin=minMED, ymax=maxMED, outlier.shape=NA, width=6, lwd=0.5) + xlim(-3,400)+
  scale_fill_manual(values="#fec44f")+theme_classic()+theme(panel.border = element_rect(colour = "black", fill=NA, size=1),axis.text = element_text(size=12), axis.title=element_text(size=12), legend.text = element_text(size = 12))+
  ylab('MED [% ethanol]')+xlab('Cycle')+geom_hline(yintercept=6.5, linetype="dashed", color = "gray")+geom_hline(yintercept=17.5, linetype="dashed", color = "black")+ theme(legend.position = "none")
dev.off()
  
ggplot(Cycle2Date, aes( c(SumDT$median[2:11],0), Cycle2Date[1:11], fill="#fec44f"))+geom_boxplot(width=6, lwd=0.5) + xlim(-3,450)+
  scale_fill_manual(values="#fec44f")+theme_classic()+theme(panel.border = element_rect(colour = "black", fill=NA, size=1),axis.text = element_text(size=16), axis.title=element_text(size=16), legend.text = element_text(size = 16))+
  ylab('MED [% ethanol]')+xlab('Cycle')+geom_hline(yintercept=6.5, linetype="dashed", color = "gray")+geom_hline(yintercept=17.5, linetype="dashed", color = "black")+ theme(legend.position = "none")

names(MEDmeltD)[2]<-"Treatment"
MEDCarbon<-right_join(Carbon, MEDmeltD, by=c("Batch","Treatment","Cycle"))
plot(MEDCarbon$Carbon,MEDCarbon$value)
summary(lm(MEDCarbon$Carbon~MEDCarbon$value))
cor(MEDCarbon$Carbon[!is.na(MEDCarbon$Carbon & !is.na(MEDCarbon$value))],MEDCarbon$value[!is.na(MEDCarbon$Carbon & !is.na(MEDCarbon$value))])

tmp$year<-year(tmp$Date)
tmp$month<-year(tmp$Date)
tmp$wy = ifelse((tmp$month >= 10), tmp$year+1, tmp$year)

TMP3ind$year<-year(TMP3ind$Date)
TMP3ind$month<-year(TMP3ind$Date)
TMP3ind$wy = ifelse((TMP3ind$month >= 10), TMP3ind$year+1, TMP3ind$year)
#linear regression
Pyr<-aggregate(tmp$PRECIPITATION[tmp$PRECIPITATION!=-999], by=list(tmp$wy[tmp$PRECIPITATION!=-999]), FUN=sum)$x[1:8]
Tyr<-aggregate(tmp$TAirInterp, by=list(tmp$wy), FUN=mean)$x[1:8]
Smaxyr<-aggregate(tmp$SnowD.x, by=list(tmp$wy), FUN=max)$x[1:8]
SP<-Smaxyr/Pyr


FT<-TMP3ind[(TMP3ind$WDFT=='T' ) & TMP3ind$SnowD.x<10 & TMP3ind$VWC10.x>0.06, ]
FT$ones<-FT$AirT.x*0+1
FTyr<-aggregate(FT$ones, by=list(FT$wy), FUN=sum)

WD<-TMP3ind[(TMP3ind$WDFT=='D'|TMP3ind$WDFT=='WD' ), ]
WD$ones<-WD$AirT.x*0+1
WDyr<-aggregate(WD$ones, by=list(WD$wy), FUN=sum)

test10Full<-lm(unlist(SumDT[6,1:8])~Tyr+SP)
test10Full<-lm(unlist(SumDT[6,1:8])~Tyr+SP+FTyr$x[1:8]+yr$x[1:8])
test10Full<-lm(unlist(FTyr$x[1:8])~SP)
summary(test10Full)

library(rsq)
#partial r^2 for each variable
tmp2<-as.numeric(unlist(rsq.partial(test10Full)[3]))
tmp1<-as.character(unlist(rsq.partial(test10Full)[2]))
tmp<-as.data.frame(tmp1)
tmp$tmp2<-tmp2
tmp<-tmp[order(-tmp2),]
par(mfrow=c(1,2),mar=c(6.1,3.1,4.1,1))
barplot(tmp$tmp2, names.arg=tmp$tmp1, las=2, main='partial r^2, 10 hr')
