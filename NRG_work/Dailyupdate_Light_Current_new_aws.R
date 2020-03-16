# 12.23.15 pull last day tracking
# runs at 5:50 AM PST

# header ------------------------------------------------------------------

rm(list=ls())

prog_name <- "Dailyupdt_LIGHT_CURRENT"

log_con <- file(paste0("~/usr/skb/maintenance_prog/completion_msg/", prog_name, "_log.txt"))

sink(log_con, append=TRUE)
sink(log_con, append=TRUE, type="message")

maintenance_folder <- "~/usr/skb/maintenance_prog/"

lib_pth<-"~/usr/skb/sys/"
source(paste0(lib_pth,"r_lib.R"))
source(paste0(lib_pth,"sys_para.R"))
source(paste0(lib_pth,"my_msci.R"))
source(paste0(lib_pth,"m_func_lib.R"))

t0<-Sys.time()
d0<-Sys.Date()
SYSDATE <- d0

# ********Adjust for 4 day weekends****************
holidays=as.Date(holidayNYSE(year(Sys.Date())))
if((d0-1) %in% holidays & weekdays(d0-1) == 'Monday') { SYSDATE = d0-1 }

#adjust for thanksgiving
if(d0 == '2018-11-21') {SYSDATE <- as.Date('2018-11-22')}
#adjust for xmas & NY
if(d0 == '2018-12-24') {SYSDATE <- as.Date('2018-12-25')}
if(d0 == '2018-12-31') {SYSDATE <- as.Date('2019-01-01')}
#if(d0 == '2019-11-25') {SYSDATE <- as.Date('2019-11-28')}
#if(d0 == '2019-11-27') {SYSDATE <- as.Date('2019-11-28')}
# *************************************************
wkdy<-weekdays(SYSDATE)

# MY_W --------------------------------------------------------------------

MY_msci_W = function (my_table_,data_frame_)
{
  
  dbWriteTable(
    mydb, 
    name = my_table_, 
    value = as.data.frame(data_frame_), 
    overwrite=TRUE, 
    row.names=FALSE
  )  
  
}


### Tracking ----------------------------------------------------------------
if(d0==as.Date('2017-05-30')){ # Mon adj for Labor Day on 29 May 2017
  
  res = dbSendQuery(or_conn, paste("select distinct * from oracle.tk_daily_mm4 where wdate =to_date('", SYSDATE-2,"', 'YYYY-MM-DD')", sep="")) 
  o_tk_LD.df = fetch(res, n=-1)	
  
  MY_msci_W("o_tk_sun_mm4_ld", o_tk_LD.df)
  
  
} else if(wkdy=='Tuesday'){ # Cume normal
  res = dbSendQuery(or_conn, paste("select distinct * from oracle.tk_cumulative_mm4 where wdate >= to_date('2018-01-01','YYYY-MM-DD') ", sep="")) 
  o_tk_LD.df = fetch(res, n=-1)
  
  MY_msci_W("o_tk_cum_mm4_ld", o_tk_LD.df)
  
} else if(wkdy=='Monday'){ # Mon
  res = dbSendQuery(or_conn, paste("select distinct * from oracle.tk_daily_mm4 where wdate >= to_date('2018-01-01','YYYY-MM-DD') ", sep="")) 
  o_tk_LD.df = fetch(res, n=-1)	
  o_tk_LD.df = o_tk_LD.df[o_tk_LD.df$DAY=='SUN',]
  MY_msci_W("o_tk_sun_mm4_ld", o_tk_LD.df)
  
} else if(wkdy=='Thursday'){ # thu normal
  res = dbSendQuery(or_conn, paste("select distinct * from oracle.tk_daily_mm4 where wdate >= to_date('2018-01-01','YYYY-MM-DD') ", sep="")) 
  o_tk_LD.df = fetch(res, n=-1)	
  o_tk_LD.df = o_tk_LD.df[o_tk_LD.df$DAY=='WED',]
  MY_msci_W("o_tk_daily_mm4_ld", o_tk_LD.df)
  
} else{ # Sun, Fri, wed normal ue a extra table
  res = dbSendQuery(or_conn, paste("select distinct * from oracle.tk_daily_mm4 where wdate =to_date('", SYSDATE-1,"', 'YYYY-MM-DD')", sep="")) 
  o_tk_LD.df = fetch(res, n=-1)
  MY_msci_W("o_tk_daily_xtra_mm4_ld", o_tk_LD.df)
  
}

dbDisconnect(mydb)
dbDisconnect(or_conn)
all_cons <- dbListConnections(MySQL())
for(con in all_cons){dbDisconnect(con)}


# end

sink() 
sink(type="message")


log_file <- readLines(log_con)

error_flag <- grep('Error', log_file)

if (length(error_flag) > 0){ source(paste0(maintenance_folder, "email_alert_prog.R"))
}  else {source(paste0(maintenance_folder, "email_alert_prog_SUCCESS.R"))}


