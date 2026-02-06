#############
# WEATHER DATASET 
#############

rm(list = ls()) #clear workspace
setwd("C:/Users/jijia/Desktop/Jijiaxin/VŠ/02_Master/03_Zweite_WS_25-26/DSMA/seminar paper/new_code/more_relaxed")


# extracting weather data
extractweather=function(mindate=min(dataset$date),maxdate=max(dataset$date),
                        latrange=range(dataset$business_lat),longrange=range(dataset$business_long),
                        resol=.5,getdata=FALSE,
                        wear=ifelse("weatherPRCPSNWDSNOWTMAXTMINTOBS.RData"%in%list.files(),"available","navailable")){
  wdatacond=wear=="navailable"
  if(getdata | wdatacond){
    require("doParallel")
    
    cl <- makeCluster(detectCores())
    registerDoParallel(cl)

    stations=read.delim("Weather_stations_NOOA.txt",header = F,quote="",sep="")[,1:3]
    colnames(stations)=c("Station","lat","long")
    stations=stations[strtrim(stations$Station,2)=="US",]
    stations$lat=as.numeric(stations$lat)
    stations$long=as.numeric(stations$long)
    stations=stations[!is.na(stations$lat)|!is.na(stations$long),]
    
    latseq=c(seq(latrange[1],latrange[2],by=resol),latrange[2])
    longseq=c(seq(longrange[1],longrange[2],by=resol),longrange[2])
    
    wear=NULL
    k=0
    torunlist=NULL
    for(lat in 1:(length(latseq)-1)){#(length(latseq)-1)
      for(lon in 1:(length(longseq)-1)){
        k=k+1
        torunlist=rbind(torunlist,c(lat,lon))
      }
    }
    wear=foreach(i=1:k,.noexport=ls(),.export=c("latseq","longseq","stations","torunlist","mindate","maxdate"))%dopar%
      {  
        # find the station(s) within the boxes
        lat=torunlist[i,1]
        lon=torunlist[i,2]
        rangelat=c(latseq[lat+1],latseq[lat])
        rangelong=c(longseq[lon],longseq[lon+1])
        indx=(stations$lat>rangelat[2])&(stations$lat<rangelat[1])&(stations$long>rangelong[1])&(stations$long<rangelong[2])
        stations_temp=stations[indx,]
        stations_t=paste(stations_temp$Station,collapse=",")
        temp=paste0("dataset=daily-summaries&dataTypes=PRCP,SNWD,SNOW,TMAX,TMIN,TOBS",
                    "&stations=",stations_t,"&startDate=",mindate,"","&endDate=",maxdate)#,
        #"","&boundingBox=",paste(latseq[lat+1],longseq[lon],latseq[lat],longseq[lon+1],sep=","))##90,-180,-90,180
        valid_url <- TRUE
        a=tryCatch(read.csv(url(paste0("https://www.ncei.noaa.gov/access/services/data/v1?",temp))),error=function(e) {valid_url<<-FALSE})
        toreturn=NULL
        if(valid_url)
          toreturn=list(range=cbind(rangelat,rangelong),data=read.csv(url(paste0("https://www.ncei.noaa.gov/access/services/data/v1?",temp))))
        return(toreturn)
        #print(c(lat,lon,valid_url))
      }
    
    
    stopCluster(cl)
    save(file="weatherPRCPSNWDSNOWTMAXTMINTOBS.RData",list=c("wear"))
  }else{
    if(wear=="available"){
      load("weatherPRCPSNWDSNOWTMAXTMINTOBS.RData")
    }
  }
  return(wear)
}

weardailyavg=function(wear){
  # this function converts the extracted weather data into daily level data.
  if("weather_data.RData"%in%list.files()){
    load(file="weather_data.RData")
  }else{
    require("doParallel")
    
    cl <- makeCluster(detectCores())
    registerDoParallel(cl)
    clusterCall(cl,function(x) {library(dplyr)})
    wear_avg=NULL
    k=0
    wear_avg=foreach(i=1:length(wear),.noexport=ls(),.export=c("wear"),.packages = c("dplyr"))%dopar%
      {
        if(is.null(wear[[i]])){
          temp=NULL
        }else{
          temp=wear[[i]]$data %>%
            group_by(DATE) %>%
            summarize(PRCP=mean(PRCP,na.rm = T),SNOW=mean(SNOW,na.rm = T),SNWD=mean(SNWD,na.rm = T),
                      TMAX=mean(TMAX,na.rm = T),TMIN=mean(TMIN,na.rm = T),TOBS=mean(TOBS,na.rm = T))
          temp=list(range=wear[[i]]$range,data=temp)}
        return(temp)
        
      }
    stopCluster(cl)
    weather=NULL
    k=0
    for(i in 1:length(wear_avg)){
      if(is.null(wear[[i]]))
        next
      k=k+1
      weather[[k]]=wear_avg[[i]]
      weather[[k]]$data$DATE=as.Date(weather[[k]]$data$DATE)
    }
    save(file="weather_data.RData",list=c("weather"))
  }
  return(weather)
}

# period 2012-2022
min_date <- "2012-01-01"
max_date <- "2022-12-31"
tampa_lat_range <- c(27.5, 28.2)   
tampa_long_range <- c(-82.7, -82.2) 

# functions with specific arguments:
weather_data_tampa <- extractweather(
  mindate = min_date,
  maxdate = max_date,
  latrange = tampa_lat_range,
  longrange = tampa_long_range,
  resol = 0.25, 
  getdata = TRUE)

weatherdaily=weardailyavg(weather_data_tampa)


# getting the latitude and longitude square 
weather_df_list <- list()

for (i in 1:length(weatherdaily)) {
  # Skip nulls
  if (!is.null(weatherdaily[[i]])) {
    temp_data <- weatherdaily[[i]]$data
    box_range <- weatherdaily[[i]]$range
    
    # box coordinates as new columns to the daily averaged data
    temp_data$lat_max <- box_range[1, 1]
    temp_data$lat_min <- box_range[2, 1]
    temp_data$long_min <- box_range[1, 2]
    temp_data$long_max <- box_range[2, 2]
    
    weather_df_list[[i]] <- temp_data
  }}

# combining
weather_df_wide <- bind_rows(weather_df_list)

weather_df_wide <- weather_df_wide %>%
  mutate(DATE = as.Date(DATE)) %>%
  mutate(
    TMAX = TMAX / 10,
    TMIN = TMIN / 10,
    TOBS = TOBS / 10) %>%
  rename(PRCP_avg = PRCP, SNOW_avg = SNOW, SNWD_avg = SNWD)


# saving the data 
write_csv(weather_df_wide, "data9_weather_df_tampa.csv")





