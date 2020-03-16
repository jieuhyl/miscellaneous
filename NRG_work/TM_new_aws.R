
cat("\f")
rm(list = ls())

prog_name <- "Automated_FCST"

log_con <- file(paste0("~/usr/skb/fcast/automated_fcst/log/", prog_name, "_log.txt"))

sink(log_con, append=TRUE)
sink(log_con, append=TRUE, type="message")

#=====================================================================================================================
#when re-running model we need to build all the tables first: start here
proj_dir <- "~/usr/skb"
maintenance_folder <- paste0(proj_dir, "/maintenance_prog/")

lib_pth<- paste0(proj_dir, "/sys/")

source(paste0(lib_pth,"r_lib.R"))
source(paste0(lib_pth,"sys_para.R"))
source(paste0(lib_pth,"my_msci.R"))
source(paste0(lib_pth,"m_func_lib.R"))
Sys.setenv(RSTUDIO_PANDOC="/usr/lib/rstudio-server/bin/pandoc")

#Run Monday morning using Sunday data
if(weekdays(today())== "Tuesday"){fcast_dt <- today()
print(fcast_dt)} else if (weekdays(today())== "Wednesday"){fcast_dt <- today()-2
print(fcast_dt)} else if (weekdays(today())== "Monday"){fcast_dt <- today()
#Run Thursday morning using Wednesday data
print(fcast_dt)} else if (weekdays(today())== "Thursday"){fcast_dt <- today()
print(fcast_dt)} else if (weekdays(today())== "Friday"){fcast_dt <- today() -1
print(fcast_dt)} else if (weekdays(today())== "Saturday"){fcast_dt <- today() -2
print(fcast_dt)} else if (weekdays(today())== "Sunday"){fcast_dt <- today() -3

print(fcast_dt)}
#for thanksgiving
if(fcast_dt == '2018-12-24') { fcast_dt = as.Date('2018-12-25')}
if(fcast_dt == '2018-12-31') { fcast_dt = as.Date('2019-01-01')}
#if(fcast_dt == '2019-11-25') { fcast_dt = as.Date('2019-11-28')}


#fcast_dt = as.Date("2020-02-04") 
#fcast_dt = as.Date("2020-01-20") 
fcast_dt0<-fcast_dt
print(fcast_dt)
fc_day<-weekdays(fcast_dt)

if (fc_day=="Friday"){days_os<-4
} else {
  if (fc_day=="Wednesday"){days_os<-3} else {
    if (fc_day=="Saturday"){days_os<-5} else {
      if (fc_day=="Sunday"){days_os<-6} else{days_os<-1}
    }
  }
}



# tk raw data -----------------------------------------------------------------
# we need to pull data sooner than it is populated in mm4

if (fc_day=="Monday" ){
  tk_16.df<- my.msci.r("o_tk_sun_mm4_2008_17")
  tk_LD.df<-my.msci.r("o_tk_sun_mm4_LD")  
  tk_16.df$WDATE=as.Date(tk_16.df$WDATE)
  tk_16.df$WDATE[weekdays(tk_16.df$WDATE)=="Saturday"]=tk_16.df$WDATE[weekdays(tk_16.df$WDATE)=="Saturday"]+1
  tk_LD.df <- dplyr::mutate(tk_LD.df,w2date=toupper(lubridate::wday(WDATE, label=TRUE)), WDATE= ifelse(w2date==DAY,WDATE, as.character(as.Date(WDATE)+days(1))))
  tk_LD.df$WDATE=as.Date(tk_LD.df$WDATE)
} else {
  if (fc_day=="Tuesday" | fc_day=="Wednesday") {
    tk_16.df<-my.msci.r("o_tk_cum_mm4_2008_17")
    tk_LD.df<-my.msci.r("o_tk_cum_mm4_LD")
    tk_16.df$WDATE=as.Date(tk_16.df$WDATE)
    tk_16.df$WDATE[weekdays(tk_16.df$WDATE)=="Monday"]=tk_16.df$WDATE[weekdays(tk_16.df$WDATE)=="Monday"]+1
    tk_LD.df$WDATE=as.Date(tk_LD.df$WDATE)
    tk_LD.df$WDATE[weekdays(tk_LD.df$WDATE)=="Monday"]=tk_LD.df$WDATE[weekdays(tk_LD.df$WDATE)=="Monday"]+1
  } else { # Thu
    tk_16.df<- my.msci.r("o_tk_daily_mm4_2008_17")
    tk_LD.df<-my.msci.r("o_tk_daily_mm4_LD")
    tk_16.df$WDATE=as.Date(tk_16.df$WDATE)
    tk_16.df$WDATE[weekdays(tk_16.df$WDATE)=="Tuesday"]=tk_16.df$WDATE[weekdays(tk_16.df$WDATE)=="Tuesday"]+1
    tk_LD.df <- dplyr::mutate(tk_LD.df,w2date=toupper(lubridate::wday(WDATE, label=TRUE)), WDATE= ifelse(w2date==DAY,WDATE, as.character(as.Date(WDATE)+days(1))))
    tk_LD.df$WDATE=as.Date(tk_LD.df$WDATE)
  }
} 


tk_c<-c("STUDYNO",  "WDATE",  "KEYS",  "AUDIENCE",  "ODATE",	"STUDIO",	"MVID",	"REPTYPE",	"TOTAL","MALE","FEMALE", "TU25","TO25", "BLK","HSP","B1216","MU25","MO25","FU25","FO25","B1724","FREQ")
tk_measure<-c("FIRST CHOICE","DEFINITE INTERST","TOTAL AWARENESS","UNAIDED AWARENESS","UNAID INTENT"
              ,"UNSTOPPABLE","DEF. NOT INTRESTD")

# SKB: 17Apr17
chr_cols <- c("TOTAL","MALE","FEMALE","TU25","TO25","BLK","HSP","B1216","MU25","MO25","FU25","FO25","B1724","FREQ")
tk_LD.df[, chr_cols] <-  sapply(tk_LD.df[, chr_cols ], function(x) as.numeric(as.character(x)))

tk_16.df<-subset(tk_16.df, REPTYPE %in% tk_measure, select=tk_c)
tk_LD.df<-subset(tk_LD.df, REPTYPE %in% tk_measure, select=tk_c)

if(nrow(tk_LD.df)>0){
  tk.df<-rbind.fill(tk_16.df,tk_LD.df)
} else{
  tk.df<-tk_16.df
}

#tk.df$WDATE<-as.Date(tk.df$WDATE)
tk.df$ODATE<-m.char2date.f(tk.df$ODATE)

tk.df <- dplyr::mutate(tk.df, open_day=toupper(lubridate::wday(ODATE, label=TRUE)) )

tk.df$ODATE[which(tk.df$open_day == "THURS")] <- tk.df$ODATE[which(tk.df$open_day == "THURS")] + 1

#just saves file
save(tk.df, file=paste0(proj_dir, "/BO/Auto_fcast/code/Daily/tk/tk_",fcast_dt,".RData"))


nm_mv_bo.df<-my.msci.r("my_nm_mv_bo")
nm_mv_bo.df$daate<-m.char2date.f(nm_mv_bo.df$daate)
nm_mv_bo.df$lastupdate<-m.char2date.f(nm_mv_bo.df$lastupdate)

#addl tables
res <- dbSendQuery(or_conn, "
                   SELECT MVID, EXTRACT(YEAR FROM COALESCE(ALT_TRACK_OPEN_DATE, WIDE_DATE, ALT_DAATE, DAATE)) AS YEAR,EXTRACT(MONTH FROM COALESCE(ALT_TRACK_OPEN_DATE, WIDE_DATE, ALT_DAATE, DAATE)) AS MONTH, EXTRACT(DAY FROM COALESCE(ALT_TRACK_OPEN_DATE, WIDE_DATE, ALT_DAATE, DAATE)) AS RDAY  
                   FROM CALENDAR1")

CAL=fetch(res, n=-1)   
releasedate=as.Date(paste(CAL$YEAR,CAL$MONTH,CAL$RDAY,sep="-"))
CAL$RELEASE=releasedate
c=c("MVID","RELEASE")
CAL=CAL[is.na(CAL$RELEASE)==FALSE,]
CAL$RELEASE[weekdays(CAL$RELEASE)=='Monday']=CAL$RELEASE[weekdays(CAL$RELEASE)=='Monday']+4
CAL$RELEASE[weekdays(CAL$RELEASE)=='Tuesday']=CAL$RELEASE[weekdays(CAL$RELEASE)=='Tuesday']+3
CAL$RELEASE[weekdays(CAL$RELEASE)=='Wednesday']=CAL$RELEASE[weekdays(CAL$RELEASE)=='Wednesday']+2
CAL$RELEASE[weekdays(CAL$RELEASE)=='Thursday']=CAL$RELEASE[weekdays(CAL$RELEASE)=='Thursday']+1
CAL$RELEASE[weekdays(CAL$RELEASE)=='Saturday']=CAL$RELEASE[weekdays(CAL$RELEASE)=='Saturday']-1
CAL$RELEASE[weekdays(CAL$RELEASE)=='Sunday']=CAL$RELEASE[weekdays(CAL$RELEASE)=='Sunday']-2

#Organic Intent 1 Aug 17
OI_file=paste0(proj_dir, "/fcast/automated_fcst/socialdata/OI_",fcast_dt,".Rdata")
if(!file.exists(OI_file)) {
  res = dbSendQuery(or_conn, "SELECT CUM_MVID, C180_O_VOLUME, CUM_O_VOLUME, MEASUREDATE  FROM NRGSOCIAL_AGGERGATE_DAILY ")
  OI=fetch(res, n=-1)   
  OI$MEASUREDATE=as.Date(OI$MEASUREDATE)
  OI$VOLUME=as.numeric(OI$CUM_O_VOLUME)
  OI$TOTALVOLUME=as.numeric(OI$C180_O_VOLUME)
  OI$MVID=OI$CUM_MVID
  OI=OI[is.na(OI$VOLUME)==FALSE,]
  save(OI,file=paste0(proj_dir, "/fcast/automated_fcst/socialdata/OI_",fcast_dt,".Rdata"))
} else {
  load(OI_file)
}

res <- dbSendQuery(or_conn, "
                   SELECT mvid, VIDLINK, mvname, G_N_1, G_N_2, CM_TYPE_OF_FILM, COMPANY_FRANCHISE, STUDIO_GROUP, THREE_D, RATING, FILM_FRANCHISE_SEQ, CM_TARGET_AUDIENCE
                   FROM MVID")

MVID<-fetch(res, n=-1)

# RT ----------------------------------------------------------------------
rt.df<-my.msci.r("o_rotten_tomatoes")

hist_rt.df <- read.xls(paste0(proj_dir, "/BO/RT/RT_hist_current.xlsx"), sheet = "JG", header = TRUE)

rt.df$MEASUREDATE2<-m.char2date.f(rt.df$MEASUREDATE)
hist_rt.df$Release<-m.char2date.f(hist_rt.df$Release)
hist_rt.df$RT<-hist_rt.df$RT*100

# code left to serve as template for future adjustment
if(fcast_dt0==as.Date('2015-12-23')){ # 2015 Xmas
  rt.df$MEASUREDATE2[rt.df$MEASUREDATE2=='2015-12-23']<-as.Date('2015-12-24')
  
} else if(fcast_dt0==as.Date('2015-12-24')){ # 2015 Xmas
  rt.df$MEASUREDATE2[rt.df$MEASUREDATE2=='2015-12-24']<-as.Date('2015-12-25')
  
} else if(fcast_dt0==as.Date('2015-12-30')){ # 2016 New Year
  rt.df$MEASUREDATE2[rt.df$MEASUREDATE2=='2015-12-30']<-as.Date('2015-12-31')
  
} else if(fcast_dt0==as.Date('2015-12-31')){ # 2016 New Year
  rt.df$MEASUREDATE2[rt.df$MEASUREDATE2=='2015-12-31']<-as.Date('2016-01-01')
  
} 

rt.df <- subset(rt.df, CRITICSCORE>0 & AUDIENCESCORE>0)

rt_mv.df <- sqldf("select b.daate,b.mvname, a.mvid, b.genre, b.mpaa, b.seq, b.WKNDGRS/1000000 as opg_bo
                  , avg(CRITICSCORE) as RT
                  , avg(AUDIENCESCORE) as audience from 'rt.df' a inner join 'nm_mv_bo.df' b on a.mvid=b.mvid where a.MEASUREDATE2=b.daate  group by 1,2,3,4,5,6,7")


mt <- min(rt_mv.df$daate)

hist_rt.df<-subset(hist_rt.df, Release<mt)

hist_rt_mv.df<-sqldf("select distinct a.*,c.daate, c.genre, c.mpaa, c.seq, c.mvname
                     , c.WKNDGRS/1000000 as opg_bo
                     , c.WKNDDAY from 'hist_rt.df' a inner join 'nm_mv_bo.df' c 
                     on a.MVID=c.MVID")

rt<-rbind.fill(hist_rt_mv.df,rt_mv.df)

rt1<-subset(rt, (opg_bo>0 | daate>=today()-5 ) & RT>0 & daate>='2010-01-01', select=c(MVID, RT))

# social idx --------------------------------------------------------------
social_file=paste0(proj_dir, "/fcast/automated_fcst/socialdata/my_social_",fcast_dt,".Rdata")
if(!file.exists(social_file)) {
  my_social_idx.df<-my.msci.r("my_social_index")
  my_social_idx.df$daate<-m.char2date.f(my_social_idx.df$daate)
  my_social_idx.df$M_dt<-m.char2date.f(my_social_idx.df$M_dt)
  my_social_idx.df$ReleaseDate<-m.char2date.f(my_social_idx.df$ReleaseDate)
  my_social_idx.df$MeasureDate<-m.char2date.f(my_social_idx.df$MeasureDate)
  
  # date adj
  if(fcast_dt0==as.Date('2015-12-23')){ # 2015 Xmas
    my_social_idx.df$MeasureDate[my_social_idx.df$MeasureDate=='2015-12-23']<-as.Date('2015-12-24')
    
  } else if(fcast_dt0==as.Date('2015-12-24')){ # 2015 Xmas
    my_social_idx.df$MeasureDate[my_social_idx.df$MeasureDate=='2015-12-24']<-as.Date('2015-12-25')
    
  } else if(fcast_dt0==as.Date('2015-12-30')){ # 2016 New Year
    my_social_idx.df$MeasureDate[my_social_idx.df$MeasureDate=='2015-12-30']<-as.Date('2015-12-31')
    
  } else if(fcast_dt0==as.Date('2015-12-31')){ # 2016 New Year
    my_social_idx.df$MeasureDate[my_social_idx.df$MeasureDate=='2015-12-31']<-as.Date('2016-01-01')
    
  } 
  
  my_social_idx.df<- ddply(my_social_idx.df, c("mvid","M_dt"), function(x) tail(x,1))
  save(my_social_idx.df,file=paste0(proj_dir, "/fcast/automated_fcst/socialdata/my_social_",fcast_dt,".Rdata"))
} else {
  load(social_file)
}


# price, old forecast, window and seasonality -------------------------------------------------------------

tkt_price.df <- read.xls (paste0(proj_dir, "/BO/general_data_repo/tkt_price.xlsx"), sheet = "tkt_price", header = TRUE)

Tk_season.df <- read.xls (paste0(proj_dir, "/BO/general_data_repo/Tk_seasonal.xlsx"), sheet = "Season", header = TRUE)

# old_fcast.df <- read.xls (paste0(proj_dir, "/BO/shiny/Model2.0/data/forecast_model20.xlsx"), sheet = "trk", header = TRUE)

#skb 31 July 

fw.df <- read.xls (paste0(proj_dir, "/BO/shiny/Model2.0/data/forecast_model20.xlsx"), sheet = "window", header = TRUE)

# This table only goes till 2016: SKB

load(paste0(proj_dir, "/BO/general_data_repo/cal_sys_long.Rdata"))

nm_mv_bo.df<-sqldf("select a.*, b.Kids,b.College from 'nm_mv_bo.df' a left join 'cal_sys_long.df' b on a.daate=b.dt")


# tk data collect all the movies that we need to forecast now -----------------------------------------------------------------
#load(paste0(proj_dir, "/BO/Auto_fcast/code/Daily/tk/tk_",fcast_dt,".RData"))

tk.df<-sqldf("select a.*, b.daate from 'tk.df' a inner join 'nm_mv_bo.df' b on a.MVID=b.MVID")
tk.df$ODATE[is.na(tk.df$ODATE)]=tk.df$daate[is.na(tk.df$ODATE)]
tk.df$ODATE[weekdays(tk.df$ODATE)=='Tuesday']=tk.df$ODATE[weekdays(tk.df$ODATE)=='Tuesday']+3
tk.df$ODATE[weekdays(tk.df$ODATE)=='Wednesday']=tk.df$ODATE[weekdays(tk.df$ODATE)=='Wednesday']+2
tk.df$ODATE[weekdays(tk.df$ODATE)=='Thursday']=tk.df$ODATE[weekdays(tk.df$ODATE)=='Thursday']+1
tk.df$ODATE[weekdays(tk.df$ODATE)=='Saturday']=tk.df$ODATE[weekdays(tk.df$ODATE)=='Saturday']-1
tk.df$ODATE[weekdays(tk.df$ODATE)=='Sunday']=tk.df$ODATE[weekdays(tk.df$ODATE)=='Sunday']-2

#collects all titles that need forecasting
mvid_source <- my.msci.r("my_nm_mv_bo")
# mvid_source$daate[mvid_source$mvid==62373]='2017-11-24'
# mvid_source$daate[mvid_source$mvid==70684]='2017-11-24'
#add I-TONYA 70863
# mvid_source$daate[mvid_source$mvid==70863]='2018-02-02'
# mvid_source$DOM_CAL_RELEASE_PATTERN[mvid_source$mvid==70863]='W'
# change ready player one from thurs to friday
mvid_source$daate[mvid_source$mvid==64496]='2018-03-30' 
mvid_source$daate[mvid_source$mvid==70010]='2018-08-17'
#change superfly one and  first purge from wednesday to friday on thursday if needed
mvid_source$daate[mvid_source$mvid==72904]='2018-06-15'
mvid_source$daate[mvid_source$mvid==69778]='2018-07-06'
mvid_source$daate[mvid_source$mvid==73073]='2018-07-13'
mvid_source$daate[mvid_source$mvid==69126]='2018-08-10'
mvid_source$daate[mvid_source$mvid==72947]='2018-08-10' 
mvid_source$daate[mvid_source$mvid==72109]='2018-08-31'
#change thanksgiving dates
mvid_source$daate[mvid_source$mvid==62376]='2018-11-23'
mvid_source$daate[mvid_source$mvid==72111]='2018-11-23'
mvid_source$daate[mvid_source$mvid==69007]='2018-11-23'
mvid_source$daate[mvid_source$mvid==73406]='2018-11-23'
mvid_source$daate[mvid_source$mvid==67097]='2018-12-21'
mvid_source$daate[mvid_source$mvid==69232]='2018-12-28'
mvid_source$daate[mvid_source$mvid==73084]='2018-12-28'

mvid_source$daate[mvid_source$mvid==67544]='2019-02-15'
mvid_source$daate[mvid_source$mvid==74981]='2019-02-15'
mvid_source$daate[mvid_source$mvid==70223]='2019-02-15'

mvid_source$daate[mvid_source$mvid==65093]='2019-04-19'  
mvid_source$daate[mvid_source$mvid==73559]='2019-04-19'
mvid_source$daate[mvid_source$mvid==73676]='2019-06-28'  
mvid_source$daate[mvid_source$mvid==73311]='2019-06-28'
mvid_source$daate[mvid_source$mvid==69101]='2019-07-05'  
mvid_source$daate[mvid_source$mvid==75529]='2019-07-05'
mvid_source$daate[mvid_source$mvid==70119]='2019-08-16' 
mvid_source$daate[mvid_source$mvid==77169]='2019-08-23'
mvid_source$daate[mvid_source$mvid==75929]='2019-11-29'
mvid_source$daate[mvid_source$mvid==74731]='2019-11-29'
mvid_source$daate[mvid_source$mvid==77927]='2019-12-27'
mvid_source$daate[mvid_source$mvid==74681]='2019-12-27'
mvid_source$daate[mvid_source$mvid==67670]='2019-12-27'
mvid_source$daate[mvid_source$mvid==63609]='2020-01-17'
mvid_source$daate[mvid_source$mvid==69959]='2020-01-17'
# add Bad Samaritan and Tully
mvid_source$DOM_CAL_RELEASE_PATTERN[mvid_source$mvid==70647]='W'
mvid_source$DOM_CAL_RELEASE_PATTERN[mvid_source$mvid %in% c(74211,73073,69126)]='W'
mvid_source$DOM_CAL_RELEASE_PATTERN[mvid_source$mvid==73135]='W'
mvid_source$DOM_CAL_RELEASE_PATTERN[mvid_source$mvid==70225]='W'
#exclude FRONT RUNNER, THE 
mvid_source$DOM_CAL_RELEASE_PATTERN[mvid_source$mvid==74304]='L'  
#mvid_source$DOM_CAL_RELEASE_PATTERN[mvid_source$mvid==73454]='L' 
mvid_source$DOM_CAL_RELEASE_PATTERN[mvid_source$mvid==74206]='L' 
mvid_source$DOM_CAL_RELEASE_PATTERN[mvid_source$mvid==74115]='L'
mvid_source$DOM_CAL_RELEASE_PATTERN[mvid_source$mvid==74860]='L'  
mvid_source$DOM_CAL_RELEASE_PATTERN[mvid_source$mvid==65093]='W' #penguins 
mvid_source$DOM_CAL_RELEASE_PATTERN[mvid_source$mvid==75953]='L' 
mvid_source$DOM_CAL_RELEASE_PATTERN[mvid_source$mvid==71368]='L' 
mvid_source$DOM_CAL_RELEASE_PATTERN[mvid_source$mvid==76015]='W' #brian banks
mvid_source$DOM_CAL_RELEASE_PATTERN[mvid_source$mvid==75878]='W'
mvid_source$DOM_CAL_RELEASE_PATTERN[mvid_source$mvid==71368]='W'
mvid_source$DOM_CAL_RELEASE_PATTERN[mvid_source$mvid==77494]='L'
mvid_source$DOM_CAL_RELEASE_PATTERN[mvid_source$mvid==76556]='W'
mvid_source$DOM_CAL_RELEASE_PATTERN[mvid_source$mvid==46113]='L'
mvid_source$DOM_CAL_RELEASE_PATTERN[mvid_source$mvid==78932]='L' 
mvid_source$DOM_CAL_RELEASE_PATTERN[mvid_source$mvid==77498]='L' 
mvid_source$DOM_CAL_RELEASE_PATTERN[mvid_source$mvid==78932]='W'

if (weekdays(fcast_dt)=='Thursday'){
  mvid_source2 <- dplyr::select(mvid_source, mvid, mvname, DOM_CAL_RELEASE_PATTERN, daate) %>% 
    filter(DOM_CAL_RELEASE_PATTERN %in% c("L,W","W","E,W","E,L,W") & daate <= fcast_dt + 22 & daate >= fcast_dt + 1 ) 
}


if (weekdays(fcast_dt)=='Monday'){
  mvid_source2 <- dplyr::select(mvid_source, mvid, mvname, DOM_CAL_RELEASE_PATTERN, daate) %>% 
    filter((DOM_CAL_RELEASE_PATTERN %in% c("L,W","W","E,W","E,L,W") & daate <= fcast_dt + 18 & daate >= fcast_dt + 4 )) 
}

if (weekdays(fcast_dt)=='Tuesday'){
  mvid_source2 <- dplyr::select(mvid_source, mvid, mvname, DOM_CAL_RELEASE_PATTERN, daate) %>% 
    filter((DOM_CAL_RELEASE_PATTERN %in% c("L,W","W","E,W","E,L,W") & daate <= fcast_dt + 17 & daate >= fcast_dt + 3 )) 
}

#mvid_source2=mvid_source2[!(mvid_source2$mvid %in% c(69231,70154,70226,70863)),]
#mvid_source2=mvid_source2[!(mvid_source2$mvid %in% c(70223, 74981)),]



#RERUN MODEL HERE WHEN NEW INPUT HAS BEEN PROVIDED =============================================================================================================
load(paste0(proj_dir, "/BO/Auto_fcast/data/Fc_db/old_fcast.df.Rdata"))

old_fcast.df <- filter(old_fcast.df, Forecast_Date != fcast_dt)

# old_fcast.df$M3_Pred[old_fcast.df$MVID==68970 & old_fcast.df$Forecast_Date == '2018-07-16']=49.37
# old_fcast.df$M3_LB[old_fcast.df$MVID==68970 & old_fcast.df$Forecast_Date == '2018-07-16']=46.91
# old_fcast.df$M3_UB[old_fcast.df$MVID==68970 & old_fcast.df$Forecast_Date == '2018-07-16']=51.90


mvid_trk <- mvid_source2[, "mvid"]
#2017
#mvid_trk = mvid_source[mvid_source$mvid %in% c(67355,63624,64874,47465,66036,69396,49532,64832,47140,68042,68379,65963,65517,47557,62154,64436,48386,48656,63446,63637,46739,46739,69412,64030) | mvid_source$mvname %in% c('HIDDEN FIGURES','UNDERWORLD: BLOOD WARS','THE BYE BYE MAN','MONSTER TRUCKS','PATRIOTS DAY','SLEEPLESS','SPLIT (2017)','XXX: RETURN OF XANDER CAGE',"A DOG'S PURPOSE",'RESIDENT EVIL: THE FINAL CHAPTER','RINGS','FIFTY SHADES DARKER','JOHN WICK: CHAPTER 2','THE LEGO BATMAN MOVIE','FIST FIGHT','THE GREAT WALL','GET OUT','LOGAN','THE SHACK','KONG','BEAUTY AND THE BEAST (2017)','CHIPS (2017)','LIFE (2017)','POWER RANGERS','THE BOSS BABY','GHOST IN THE SHELL (2017)','GOING IN STYLE (2017)','SMURFS: THE LOST VILLAGE','THE FATE OF THE FURIOUS','THE CIRCLE (2017)','HOW TO BE A LATIN LOVER','GUARDIANS OF THE GALAXY VOL. 2','KING ARTHUR: LEGEND OF THE SWORD','SNATCHED','ALIEN: COVENANT','DIARY OF A WIMPY KID: THE LONG HAUL','EVERYTHING, EVERYTHING','BAYWATCH','PIRATES OF THE CARIBBEAN: DEAD MEN TELL NO TALES','CAPTAIN UNDERPANTS: THE FIRST EPIC MOVIE','WONDER WOMAN','THE MUMMY (2017)','47 METERS DOWN','ALL EYEZ ON ME','CARS 3','ROUGH NIGHT','TRANSFORMERS: THE LAST KNIGHT','BABY DRIVER','DESPICABLE ME 3','THE HOUSE','SPIDER-MAN: HOMECOMING','THE BIG SICK','WAR FOR THE PLANET OF THE APES','DUNKIRK','GIRLS TRIP','VALERIAN AND THE CITY OF A THOUSAND PLANETS'),"mvid"]
#titles= mvid_source[mvid_source$mvid %in% c(67355,63624,64874,47465,66036,69396,49532,64832,47140,68042,68379,65963,65517,47557,62154,64436,48386,48656,63446,63637,46739,46739,69412,64030) | mvid_source$mvname %in% c('HIDDEN FIGURES','UNDERWORLD: BLOOD WARS','THE BYE BYE MAN','MONSTER TRUCKS','PATRIOTS DAY','SLEEPLESS','SPLIT (2017)','XXX: RETURN OF XANDER CAGE',"A DOG'S PURPOSE",'RESIDENT EVIL: THE FINAL CHAPTER','RINGS','FIFTY SHADES DARKER','JOHN WICK: CHAPTER 2','THE LEGO BATMAN MOVIE','FIST FIGHT','THE GREAT WALL','GET OUT','LOGAN','THE SHACK','KONG','BEAUTY AND THE BEAST (2017)','CHIPS (2017)','LIFE (2017)','POWER RANGERS','THE BOSS BABY','GHOST IN THE SHELL (2017)','GOING IN STYLE (2017)','SMURFS: THE LOST VILLAGE','THE FATE OF THE FURIOUS','THE CIRCLE (2017)','HOW TO BE A LATIN LOVER','GUARDIANS OF THE GALAXY VOL. 2','KING ARTHUR: LEGEND OF THE SWORD','SNATCHED','ALIEN: COVENANT','DIARY OF A WIMPY KID: THE LONG HAUL','EVERYTHING, EVERYTHING','BAYWATCH','PIRATES OF THE CARIBBEAN: DEAD MEN TELL NO TALES','CAPTAIN UNDERPANTS: THE FIRST EPIC MOVIE','WONDER WOMAN','THE MUMMY (2017)','47 METERS DOWN','ALL EYEZ ON ME','CARS 3','ROUGH NIGHT','TRANSFORMERS: THE LAST KNIGHT','BABY DRIVER','DESPICABLE ME 3','THE HOUSE','SPIDER-MAN: HOMECOMING','THE BIG SICK','WAR FOR THE PLANET OF THE APES','DUNKIRK','GIRLS TRIP','VALERIAN AND THE CITY OF A THOUSAND PLANETS'),c("mvid","mvname")]
#2018
#mvid_trk = mvid_source[mvid_source$mvid %in% c(63874,68041,67709,70087,69577,68997,69872,71941,63417,68300,70764,63445,66037,48914,70781,69653,69321,67543,35978,68855,70873,69229,67963,48792,65579,64496,70541,67197,69965,72216,66312,70682,70904,48912,70728,72094,68062,72459,68845,64237,70915,72293,66836,72904,62802,70222,64345,69354,71162,69778,65042,65429,69893,63954,70327),"mvid"]

mvid_trk2 <- paste(mvid_trk, collapse = ", ")

query <- paste0("select s.COMPSETNAME
                , pm.MVID PrimaryMVID
                , pm.MVNAME PrimaryTitle
                , cm.MVID CompMVID
                , cm.MVNAME CompTitle
                , cm.G_N_1 PrimaryGenre
                , cm.G_N_2 SecondaryGenre
                , (CASE WHEN cm.G_N_2 in ('ANIMATED') THEN '1' ELSE '0' END) ANIMATION
                , (CASE WHEN cm.THREE_D in ('Y') THEN '1' ELSE '0' END) THREE_D
                , cm.RATING as mpaa
                , cm.DOM_CAL_RELEASE_PATTERN
                , cm.BASEDON as basedon
                , cm.SQ as seq
                , cm.PROD_BUDGET as bgt
                , bo.WKNDGRS
                , bo.SCRNS
                , bo.DAATE BO_DAATE
                , bo.WKNDDAY
                , cal.ALT_DAATE CAL_ATL_DAATE
                , cal.DAATE CAL_DAATE
                , cal.WIDE_DATE CAL_WIDE_DATE
                , cal.PATTERN CAL_PATTERN
                , COALESCE(cal.ALT_TRACK_OPEN_DATE, cal.WIDE_DATE, cal.ALT_DAATE, cal.DAATE) as COMPREL
                -- , (NEXT_DAY(T.WDATE, 'Sun') - NEXT_DAY(COALESCE(CAL.ALT_TRACK_OPEN_DATE, CAL.WIDE_DATE, CAL.ALT_DAATE, CAL.DAATE), 'Sun')) / 7 AS WINDOW 
                , tk.wdate
                , tk.AUDIENCE
                , tk.SEQ as SEQ1
                , tk.REPTYPE
                , tk.TOTAL
                from TK_COMPARISON_TITLES t
                inner join TK_COMPARISON_SETS s on s.COMPSETID = t.COMPSETID
                inner join TK_DAILY_MM4 tk on tk.mvid = t.COMPMVID and tk.AUDIENCE = 'REGULAR' and tk.SEQ = 1
                inner join MVID cm on cm.MVID = t.COMPMVID
                inner join MVID pm on pm.MVID = t.PRIMARYMVID
                left outer join d_boxfg bo on bo.MVID = t.COMPMVID
                left outer join calendar1 cal on cal.mvid = t.COMPMVID
                where t.PrimaryMVID in (", noquote(mvid_trk2), ") and t.CompSetID = 2")

res <- dbSendQuery(or_conn, query)

comps <- fetch(res, n=-1)

#remove comps here
comps = comps[!(comps$PRIMARYMVID == 63445 & comps$COMPMVID == 46900),]
comps = comps[!(comps$PRIMARYMVID == 69050),]

# # great wall and assasins creed
comps = comps[!(comps$PRIMARYMVID == 66312 & comps$COMPMVID == 45334),]
comps = comps[!(comps$PRIMARYMVID == 66312 & comps$COMPMVID == 62154),]

# # for quiet place, remove horror seq, and obo less than 14m, 
# comps = comps[!(comps$PRIMARYMVID == 69965 & comps$COMPMVID == 41722),]
# comps = comps[!(comps$PRIMARYMVID == 69965 & comps$COMPMVID == 41714),]
# comps = comps[!(comps$PRIMARYMVID == 69965 & comps$COMPMVID == 63506),]
# #comps = comps[!(comps$PRIMARYMVID == 69965 & comps$WKNDGRS <= 14000000),]
# 
# # for ready player one: 64496
# comps = comps[!(comps$PRIMARYMVID == 64496 & comps$COMPMVID == 43332),]
# comps = comps[!(comps$PRIMARYMVID == 64496 & comps$COMPMVID == 45507),]
# comps = comps[!(comps$PRIMARYMVID == 64496 & comps$COMPMVID == 62341),]
# comps = comps[!(comps$PRIMARYMVID == 64496 & comps$COMPMVID == 63417),]
# comps = comps[!(comps$PRIMARYMVID == 64496 & comps$COMPMVID == 46900),]
# comps = comps[!(comps$PRIMARYMVID == 64496 & comps$COMPMVID == 44393),]
# comps = comps[!(comps$PRIMARYMVID == 64496 & comps$COMPMVID == 48792),]



comps$COMPREL[hour(comps$COMPREL)==23]=comps$COMPREL[hour(comps$COMPREL)==23]+3600
comps$WDATE[hour(comps$WDATE)==23]=comps$WDATE[hour(comps$WDATE)==23]+3600
comps$COMPREL=as.Date(comps$COMPREL)
comps$WDATE=as.Date(comps$WDATE)
comps$COMPREL[weekdays(comps$COMPREL)=='Wednesday']=comps$COMPREL[weekdays(comps$COMPREL)=='Wednesday']+2

comps=sqldf("select T.*, C.daate as RELEASE_DATE from 'comps' T left join 'mvid_source' C on T.PRIMARYMVID=C.MVID")
comps$RELEASE_DATE=as.Date(comps$RELEASE_DATE)
comps$RELEASE_DATE[weekdays(comps$RELEASE_DATE)=='Monday']=comps$RELEASE_DATE[weekdays(comps$RELEASE_DATE)=='Monday']-3
comps$RELEASE_DATE[weekdays(comps$RELEASE_DATE)=='Tuesday']=comps$RELEASE_DATE[weekdays(comps$RELEASE_DATE)=='Tuesday']+3
comps$RELEASE_DATE[weekdays(comps$RELEASE_DATE)=='Wednesday']=comps$RELEASE_DATE[weekdays(comps$RELEASE_DATE)=='Wednesday']+2
comps$RELEASE_DATE[weekdays(comps$RELEASE_DATE)=='Thursday']=comps$RELEASE_DATE[weekdays(comps$RELEASE_DATE)=='Thursday']+1
if(fc_day == 'Tuesday') {
  comps$days=as.numeric(comps$RELEASE_DATE-fcast_dt)
} else {
  comps$days=as.numeric(comps$RELEASE_DATE-fcast_dt)+1  
}

#comps$days=2
valid_column_names <- make.names(names=names(comps), unique=TRUE, allow_ = TRUE)

names(comps) <- valid_column_names

comps <-  dplyr::select(comps, PRIMARYMVID, PRIMARYTITLE, COMPMVID,COMPTITLE, PRIMARYGENRE,SECONDARYGENRE, WKNDGRS, SCRNS, RELEASE_DATE,WKNDDAY, days,COMPREL)
comps=unique(comps)
comps = sqldf("select T.*, tk.TOTAL as FC, tk.WDATE from 'comps' T left join 'tk.df' tk on T.COMPMVID=tk.mvid where tk.AUDIENCE='REGULAR' and tk.REPTYPE='FIRST CHOICE' ")
comps = comps[(as.Date(comps$COMPREL)-as.Date(comps$WDATE))==comps$days,!(names(comps)=='WDATE')]
comps = sqldf("select T.*, tk.TOTAL as UA, tk.WDATE from 'comps' T left join 'tk.df' tk on T.COMPMVID=tk.mvid where tk.AUDIENCE='REGULAR' and tk.REPTYPE='UNAIDED AWARENESS' ")
comps = comps[as.Date(comps$COMPREL)-as.Date(comps$WDATE)==comps$days,!(names(comps)=='WDATE')]
comps = sqldf("select T.*, tk.TOTAL as DI, tk.WDATE from 'comps' T left join 'tk.df' tk on T.COMPMVID=tk.mvid where tk.AUDIENCE='REGULAR' and tk.REPTYPE='DEFINITE INTERST' ")
comps = comps[as.Date(comps$COMPREL)-as.Date(comps$WDATE)==comps$days,!(names(comps)=='WDATE')]
comps = sqldf("select T.*, tk.TOTAL as TA, tk.WDATE from 'comps' T left join 'tk.df' tk on T.COMPMVID=tk.mvid where tk.AUDIENCE='REGULAR' and tk.REPTYPE='TOTAL AWARENESS' ")
comps = comps[as.Date(comps$COMPREL)-as.Date(comps$WDATE)==comps$days,!(names(comps)=='WDATE')]
comps=comps[is.na(comps$PRIMARYMVID)==FALSE,]

comps = unique(comps)
comps$RELEASE_DATE[comps$PRIMARYMVID==69412]='2017-07-14'
comps$RELEASE_DATE[comps$PRIMARYMVID==64958]='2017-01-06'
comps$RELEASE_DATE[comps$PRIMARYMVID==66460]='2017-01-13'


fctitle=tk.df[tk.df$AUDIENCE=='REGULAR' & tk.df$REPTYPE=='UNAIDED AWARENESS',]
fctitle$ODATE[weekdays(fctitle$ODATE)=='Tuesday']=fctitle$ODATE[weekdays(fctitle$ODATE)=='Tuesday']+3
fctitle$ODATE[weekdays(fctitle$ODATE)=='Wednesday']=fctitle$ODATE[weekdays(fctitle$ODATE)=='Wednesday']+2
fctitle$ODATE[weekdays(fctitle$ODATE)=='Thursday']=fctitle$ODATE[weekdays(fctitle$ODATE)=='Thursday']+1
fctitle=fctitle[is.na(fctitle$WDATE)==FALSE,]

comps = sqldf("select T.*, f.TOTAL as PUA, f.WDATE from 'comps' T left join 'fctitle' f on T.PRIMARYMVID=f.mvid")
#comps <- comps[!is.na(comps$WDATE), ]
#comps=comps[comps$days==(comps$RELEASE_DATE-comps$WDATE),]
comps=comps[comps$days==(comps$RELEASE_DATE-comps$WDATE),!(names(comps)=='WDATE')]



fctitle=tk.df[tk.df$AUDIENCE=='REGULAR' & tk.df$REPTYPE=='DEFINITE INTERST',]
fctitle$ODATE[weekdays(fctitle$ODATE)=='Tuesday']=fctitle$ODATE[weekdays(fctitle$ODATE)=='Tuesday']+3
fctitle$ODATE[weekdays(fctitle$ODATE)=='Wednesday']=fctitle$ODATE[weekdays(fctitle$ODATE)=='Wednesday']+2
fctitle$ODATE[weekdays(fctitle$ODATE)=='Thursday']=fctitle$ODATE[weekdays(fctitle$ODATE)=='Thursday']+1

comps = sqldf("select T.*, f.TOTAL as PDI, f.WDATE from 'comps' T left join 'fctitle' f on T.PRIMARYMVID=f.mvid")
comps=comps[comps$days==(comps$RELEASE_DATE-comps$WDATE),!(names(comps)=='WDATE')]

fctitle=tk.df[tk.df$AUDIENCE=='REGULAR' & tk.df$REPTYPE=='UNAID INTENT',]
fctitle$ODATE[weekdays(fctitle$ODATE)=='Tuesday']=fctitle$ODATE[weekdays(fctitle$ODATE)=='Tuesday']+3
fctitle$ODATE[weekdays(fctitle$ODATE)=='Wednesday']=fctitle$ODATE[weekdays(fctitle$ODATE)=='Wednesday']+2
fctitle$ODATE[weekdays(fctitle$ODATE)=='Thursday']=fctitle$ODATE[weekdays(fctitle$ODATE)=='Thursday']+1

comps = sqldf("select T.*, f.TOTAL as PTA, f.WDATE from 'comps' T left join 'fctitle' f on T.PRIMARYMVID=f.mvid")
comps=comps[comps$days==(comps$RELEASE_DATE-comps$WDATE),!(names(comps)=='WDATE')]

fctitle=tk.df[tk.df$AUDIENCE=='REGULAR' & tk.df$REPTYPE=='FIRST CHOICE',]
fctitle$ODATE[weekdays(fctitle$ODATE)=='Tuesday']=fctitle$ODATE[weekdays(fctitle$ODATE)=='Tuesday']+3
fctitle$ODATE[weekdays(fctitle$ODATE)=='Wednesday']=fctitle$ODATE[weekdays(fctitle$ODATE)=='Wednesday']+2
fctitle$ODATE[weekdays(fctitle$ODATE)=='Thursday']=fctitle$ODATE[weekdays(fctitle$ODATE)=='Thursday']+1

comps = sqldf("select T.*, f.TOTAL as PFC, f.WDATE from 'comps' T left join 'fctitle' f on T.PRIMARYMVID=f.mvid")
comps=comps[comps$days==(comps$RELEASE_DATE-comps$WDATE),!(names(comps)=='WDATE')]
comps = unique(comps)

comps$diff=abs(comps$FC-comps$PFC)+abs(comps$UA-comps$PUA)+abs(comps$DI-comps$PDI)+abs(comps$TA-comps$PTA)
comps$diff[comps$PRIMARYMVID==comps$COMPMVID]=1

comps2 <- unique(comps) %>% group_by(PRIMARYTITLE) %>%
  summarise(SCRNS_median = round(median(SCRNS, na.rm= TRUE)), 
            mindiff = min(diff, na.rm= TRUE)) %>% left_join(unique(comps),by=c("PRIMARYTITLE") )

guess=comps2[comps2$mindiff==comps2$diff & is.na(comps2$PRIMARYMVID)==FALSE,]

comps2=sqldf("select T.*, g.WKNDGRS as OPO_median from 'comps2' T left join 'guess' g on T.PRIMARYMVID=g.PRIMARYMVID ")

#initialize
comps2$category_guess="B"
comps2$category_guess[comps2$OPO_median < 10^7] <- "D" 
comps2$category_guess[comps2$OPO_median >= 10^7 & comps2$OPO_median < 50*10^6 ] <- "C" 
comps2$category_guess[comps2$OPO_median >= 50*10^6 & comps2$OPO_median < 100*10^6 ] <- "B" 
comps2$category_guess[comps2$OPO_median >= 100*10^6 ] <- "A" 

if(length(which(mvid_trk %in% rt.df$MVID))>0){
  rt_scores <- filter(rt.df, MVID %in% (mvid_trk)) %>% group_by(MVID) %>% mutate(rt_max= max(MEASUREDATE2)) %>% filter(rt_max==MEASUREDATE2) %>% dplyr::select(CRITICSCORE, MVID)
  comps3 <-  left_join(comps2, rt_scores, by= c('PRIMARYMVID'='MVID'))
} else {
  comps3=comps2
  comps3$CRITICSCORE=NA
}

comps3$wknd_days <- comps3$WKNDDAY

comps4 <- select(comps3, PRIMARYMVID, PRIMARYTITLE, wknd_days, SCRNS_median, category_guess,
                 CRITICSCORE, COMPTITLE, PRIMARYGENRE,SECONDARYGENRE,WKNDGRS,SCRNS,RELEASE_DATE, everything())

comps4$RELEASE_DATE <- as.Date(comps4$RELEASE_DATE)

comps4 <- as.data.frame(comps4)



#writes out parameters file for simulation update by users
parametrs_file = paste0("/mnt/WinMount/msci_BO_Fcast/Model_Parameter_Updt/Parameters_Used ", fcast_dt , " .xlsx")

# ************#
wb<-createWorkbook(type="xlsx")

sheet <- xlsx::createSheet(wb, sheetName = "Parameter Review")

csdollar <- CellStyle(wb, dataFormat=DataFormat("$###,##0.00"))

xlsx::addDataFrame(comps4, sheet, startRow=1, startColumn=1, row.names=FALSE)

xlsx::autoSizeColumn(sheet, colIndex= c(2, 7:8, 11))




# ************ #
#if file already exists, it will not re-write
if(!file.exists(parametrs_file)){xlsx::saveWorkbook(wb, parametrs_file)}

# rm(comps4)

comps4 <- read.xlsx(parametrs_file, sheetIndex = 1, stringsAsFactors = FALSE)



Fc_db<-data.frame()
Fc_meeting_main<-data.frame()
Fc_meeting_main_SI<-data.frame()

c_t<-vector()
c_t_SI<-vector()
input_para <- data.frame()
cols_4_lmdf <- c("daate","mvname", "opg_bo", "mvid","WDATE","genre")

#mvid_trk=mvid_trk[mvid_trk != c(72284)] add miracle season
mvid_trk = mvid_trk[which(!mvid_trk %in% c(74221,73930,74304,74206,69232,73084  ))]
comps4=comps4[is.na(comps4$PRIMARYMVID)==FALSE,]

for(i in 1:length(mvid_trk)){ 
  
  #i <- mvid_trk[1]
  official_fcast<-10
  
  fcast_mv <- mvid_trk[i]
  
  if(fcast_mv == 78932 ){ # downhill
    official=0
  } else if(fcast_mv == 75843 ){ # fantasy
    official=19.4
  } else if(fcast_mv == 73186 ){ # sonic
    official=64
  } else if(fcast_mv == 76223 ){ # photo
    official=14.3
  } else if(fcast_mv == 76209 ){ # brahms
    official=9.8
  } else if(fcast_mv == 71808 ){ # call
    official=17.5
  } else if(fcast_mv == 71563 ){ # man
    official=23.3
  } 
  
  
  
  #official = 0
  RT_est <-  comps4[ (comps4$PRIMARYMVID== mvid_trk[i] ), "CRITICSCORE"]
  #
  RT_est <- as.numeric(unlist(unique(RT_est[[1]])))
  
  
  if(fcast_mv == 72113 ){RT_est = 43} # rhythm
  if(fcast_mv == 77873 ){RT_est = 76} # gentleman
  if(fcast_mv == 74082 ){RT_est = 24} # like
  
  
  
  RT_est
  
  adj_<-1.0
  
  wknd_days <-  comps4[ (comps4$PRIMARYMVID== mvid_trk[i] ) , "wknd_days"]
  #& is.na(comps4$WKNDGRS)
  wknd_days <- as.numeric(unlist(unique(wknd_days[[1]])))
  wknd_days=3
  
  if(is.na(CAL$RELEASE[CAL$MVID==fcast_mv])==TRUE){
    r_dt <- as.Date(mvid_source$daate[mvid_source$mvid==mvid_trk[i]])  
  } else {
    r_dt <- CAL$RELEASE[CAL$MVID==mvid_trk[i]] 
  }
  
  
  fcast_cat <-  comps4[(comps4$PRIMARYMVID== mvid_trk[i] ) , "category_guess"]
  #& is.na(comps4$WKNDGRS)
  fcast_cat <- unlist(unique(  fcast_cat[[1]]))
  
  
  if(fcast_mv == 75843 ) {fcast_cat = 'C'} #fantasy
  if(fcast_mv == 73186 ) {fcast_cat = 'C'} #sonic
  if(fcast_mv == 76223 ) {fcast_cat = 'D'} #photo
  if(fcast_mv == 76209 ) {fcast_cat = 'D'} #brahms
  if(fcast_mv == 71808 ) {fcast_cat = 'C'} #call
  if(fcast_mv == 71563 ) {fcast_cat = 'C'} #man
  
  
  
  
  short_mv <-  comps4[comps4$PRIMARYMVID== mvid_trk[i], "PRIMARYTITLE"]
  short_mv <- unlist(unique(short_mv))
  short_mv <-  gsub("[^[:alnum:] ]|\\s", "", short_mv, perl = TRUE)
  
  fcast_scrns <-  comps4[(comps4$PRIMARYMVID== mvid_trk[i] ) , "SCRNS_median"]
  #& is.na(comps4$WKNDGRS)
  fcast_scrns <- unlist(unique(fcast_scrns[[1]]))
  
  
  if(fcast_mv == 73186) {fcast_scrns = 4100} # sonic
  if(fcast_mv == 75843) {fcast_scrns = 2700} # do
  if(fcast_mv == 75843) {fcast_scrns = 2500} # photo
  if(fcast_mv == 72113) {fcast_scrns = 3000} # rhythm
  
  
  
  mv_genre <- comps4[comps4$PRIMARYMVID== mvid_trk[i] , "PRIMARYGENRE"]
  
  mv_genre <- unlist(unique(mv_genre))
  
  fcast_genre <- paste0("('", unlist(mv_genre), collapse="','", "')")
  fcast_genre <- gsub("')'|'\\('", "'",fcast_genre)
  #if(fcast_mv == 65093){
  #  fcast_genre = "('FAMILY')"
  #}
  
  
  comp_titles <- comps4[comps4$PRIMARYMVID== mvid_trk[i] , "COMPTITLE"]
  comp_titles <- unique(comp_titles)
  
  plot_comp <- as.character(unlist(comp_titles))
  
  minobo= min(comps4[comps4$PRIMARYMVID== mvid_trk[i] & is.na(comps4$WKNDGRS)==FALSE, "WKNDGRS"])
  maxobo= max(comps4[comps4$PRIMARYMVID== mvid_trk[i] & is.na(comps4$WKNDGRS)==FALSE, "WKNDGRS"])
  
  cat(plot_comp)
  
  input_para <- data.frame(short_mv= short_mv, fcast_mv=fcast_mv,RT_est=RT_est, r_dt= r_dt, fcast_cat =fcast_cat, fcast_scrns=fcast_scrns, fcast_genre=fcast_genre
                           , wknd_days=wknd_days )
  
  write.csv(input_para, file= paste0(proj_dir, "/fcast/automated_fcst/outputs/ParaMeters_Used_for_", short_mv , ".csv" ))
  
  #write.xlsx(input_para, file= paste0("/mnt/WinMount/msci_BO_Fcast/training_titles/Input_parameters_", short_mv, ".xls"), sheetName = short_mv, row.names = FALSE )
  
  
  source(paste0(proj_dir, "/fcast/automated_fcst/GB_GT_1022.R"))
  Fc_output
  summary(fit_)
  input_file <- paste0(proj_dir, "/BO/Auto_fcast/code/Daily/report/0_report_si_M2M3.Rmd")
  output_file <-  paste0(proj_dir, "/BO/Auto_fcast/code/Daily/report/",short_mv,"_M3_",fcast_dt,"_UI.html")
  render(input= input_file, output_file= output_file)
  
  c_t_SI<-c(c_t_SI,paste0(proj_dir, "/BO/Auto_fcast/code/Daily/report/",short_mv,"_M3_",fcast_dt,"_UI.html"))
  
  
}


# save results --------------------------------------------------------------------

save(Fc_db, file=paste0(proj_dir, "/BO/Auto_fcast/data/Fc_db_M2M3_",fcast_dt,".RData")) 
save(Fc_db, file=paste0(proj_dir, "/BO/Auto_fcast/data/Fc_db_M2M3.RData")) 

library(dplyr)
Fc_meeting_main_SI <-  dplyr::arrange(Fc_meeting_main_SI, daate)
save(Fc_meeting_main_SI, file=paste0(proj_dir, "/BO/Auto_fcast/data/Fc_meeting_main_SI_M2M3",fcast_dt,".RData")) 
save(Fc_meeting_main_SI, file=paste0(proj_dir, "/BO/Auto_fcast/data/Fc_meeting_main_SI_M2M3.RData")) 

save(c_t, file=paste0(proj_dir, "/BO/Auto_fcast/data/c_t_M2M3_",fcast_dt,".RData")) 
save(c_t_SI, file=paste0(proj_dir, "/BO/Auto_fcast/data/c_t_M2M3_",fcast_dt,"_SI.RData"))

write.csv(Fc_meeting_main, file=paste0(proj_dir, "/BO/Auto_fcast/code/Daily/report/1_Fc_meeting_main_M2M3_",fcast_dt,".csv"))
write.csv(Fc_db, file=paste0(proj_dir, "/BO/Auto_fcast/code/Daily/report/1_Fc_db_M2M3_",fcast_dt,".csv"))

# skb 31 july start
Fc_db <- dplyr::rename(Fc_db, Forecast_Date = Forecast.Date )
Fc_db <- dplyr::rename(Fc_db, Opening_Date = Opening.Date )

old_fcast.df <- rbind.fill(old_fcast.df, Fc_db)

unique_rows <- !duplicated(old_fcast.df[c("Forecast_Date","MVID")])

old_fcast.df <- old_fcast.df[unique_rows,]

save(old_fcast.df, file= paste0(proj_dir, "/BO/Auto_fcast/data/Fc_db/old_fcast.df.Rdata"))

# skb 31 july END
# only run if we want to send an email
# email me -------------------------------------------------------------------

library(knitr)
setwd(paste0(proj_dir, "/BO/Auto_fcast/code/Daily/report/"))
knit2html(paste0(proj_dir, "/BO/Auto_fcast/code/Daily/report/0_email_SI_M3_no_official.Rmd"),output=paste0(proj_dir, "/BO/Auto_fcast/code/Daily/report/0_email_SI_M2M3.html"),fragment.only = TRUE, options="")

library(mailR)
#recipients <- c("Sanjeev Baniwal<sanjeev.baniwal@nrgmr.com>","Natasha Ericta<natasha.ericta@nrgmr.com>")
#recipients = c("usbo-forecast@nrgmr.com", "sanjeev.baniwal@nrgmr.com")
#recipients = c("David.Gilison@nrgmr.com", "sanjeev.baniwal@nrgmr.com")
#recipients <- c("Jie Hu<jie.hu@nrgmr.com>", "John Paez <john.paez@nrgmr.com>", "Behi Shamsai <behi.shamsai@nrgmr.com>" )
recipients <- c("Jie Hu<jie.hu@nrgmr.com>")

(attach_files<-c_t_SI)
send.mail(from = "US Box Office FC<autogenerated@nrgmr.com>",
          
          to = recipients,
          subject=paste0("Model Forecast Official Version_Shared_acct - ", today()," update from ", weekdays(fcast_dt), " Data"),
          body = paste0(proj_dir, "/BO/Auto_fcast/code/Daily/report/0_email_SI_M2M3.html"),
          html = TRUE,
          encoding = "utf-8",
          smtp = list(host.name = "smtp.mailgun.org", port = 25, ssl=TRUE, user.name = "r-email@mg.nrgmr.com",   passwd = "97Kgq*d!PbX$"),   authenticate = TRUE, 
          send = TRUE,
          attach.files = attach_files)



#=====================================================================================================================
all_cons <- dbListConnections(MySQL())
for(con in all_cons){dbDisconnect(con)}


sink() 
sink(type="message")


log_file <- readLines(log_con)

error_flag <- grep('Error', log_file)

length(error_flag)

if (length(error_flag) > 0){ source(paste0(maintenance_folder, "/email_alert_prog.R"))
}  else {source(paste0(maintenance_folder, "email_alert_prog_SUCCESS.R"))}





