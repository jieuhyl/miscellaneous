# remove OI from bayesian

# Generic Sanjeev 23 June 17
library(timeDate)
library(glmnet)
set.seed(fcast_mv)

od<-sqldf(paste0("select distinct daate from 'nm_mv_bo.df' where mvid=",fcast_mv))

#why this adjustment? actually does nothing because of earlier adjustment in days
if(fc_day == 'Tuesday'){
  fc_days<-(r_dt-fcast_dt)
} else {
  fc_days<-(r_dt-fcast_dt)+days_os  
}

#validation
#fc_days = 2

wk<-floor(fc_days/7)

nm_comps.df<-sqldf(paste0(
  "SELECT distinct
  pm.daate as primarydt
  , pm.mvname AS primarytitle
  , pm.mvid as primarymvid
  , pm.genre as primarygenre
  , pm.animation as primaryanimation
  , pm.mpaa as primarympaa
  , pm.runtime as primaryruntime
  , pm.basedon as primarybasedon
  , pm.seq as primaryseq
  
  
  , cm.*
  
  FROM 'nm_mv_bo.df' pm 
  INNER JOIN 'nm_mv_bo.df' cm ON ((cm.genre in ", fcast_genre, " and cm.DOM_CAL_RELEASE_PATTERN='W' and cm.WKNDGRS>=1000000) or (cm.mvid=",fcast_mv,"))
  WHERE pm.mvid=",fcast_mv, 
  " ORDER by pm.daate  desc, pm.mvname, cm.daate  desc, cm.mvname")
)


nm_comp_tk.df<-sqldf("select distinct * from 'tk.df' where MVID in (select MVID from 'nm_comps.df')")

nm_comp_tk.df$ODATE[nm_comp_tk.df$MVID==fcast_mv]<-r_dt # release date adj.

odate<-sqldf("select MVID, count(distinct ODATE) as n_odate from 'nm_comp_tk.df' group by 1")

t<-subset(odate, n_odate==1)

nm_comp_tk2.df<-subset(nm_comp_tk.df, ODATE-WDATE==fc_days & MVID %in% (t$MVID))

nm_comp_tk2.df=sqldf("select a.*, b.G_N_1, b.G_N_2, b.MVNAME from 'nm_comp_tk2.df' a left join 
                     'MVID' b on a.mvid=b.MVID")
if(fcast_mv %in% c(70092)){
  nm_comp_tk2.df<-subset(nm_comp_tk2.df,MVID == fcast_mv | (G_N_1 %in% c("ACTION DRAMA",unique(comps4$PRIMARYGENRE[comps4$PRIMARYMVID==fcast_mv])) ))
} else if(fcast_mv %in% c(354660)){
  nm_comp_tk2.df<-subset(nm_comp_tk2.df,MVID == fcast_mv | (G_N_1 %in% c("ROMANTIC DRAMA","ROMANTIC COMEDY","HUMN INTST DRMA")  ))
} else if(fcast_mv %in% c(69768)){
  nm_comp_tk2.df<-subset(nm_comp_tk2.df,MVID==fcast_mv | (G_N_1 %in% c("HORROR","SUSPNS/THRILLER") ))
} else if(fcast_mv %in% c(700340)){
  nm_comp_tk2.df<-subset(nm_comp_tk2.df,MVID==fcast_mv | (G_N_1 %in% c("HUMN INTST CMDY","ZANY COMEDY") & G_N_2 %in% unique(comps4$SECONDARYGENRE[comps4$PRIMARYMVID==fcast_mv & comps4$PRIMARYGENRE %in% c("HUMN INTST CMDY","ZANY COMEDY")]) ))
} else if(fcast_mv==703270){
  nm_comp_tk2.df<-subset(nm_comp_tk2.df,MVID==fcast_mv | (G_N_1 != "FAMILY" & G_N_2 %in% unique(comps4$SECONDARYGENRE[comps4$PRIMARYMVID==fcast_mv & comps4$PRIMARYGENRE != "FAMILY"]) ))
} else if(fcast_mv%in% c(72109,70650,73128,65093)){
  nm_comp_tk2.df<-subset(nm_comp_tk2.df,MVID==fcast_mv | (G_N_1 %in% unique(comps4$PRIMARYGENRE[comps4$PRIMARYMVID==fcast_mv]) ))
} else {
  nm_comp_tk2.df<-subset(nm_comp_tk2.df,MVID==fcast_mv | (G_N_1 %in% unique(comps4$PRIMARYGENRE[comps4$PRIMARYMVID==fcast_mv]) & G_N_2 %in% unique(comps4$SECONDARYGENRE[comps4$PRIMARYMVID==fcast_mv])))
}

nm_comp_tk3.df<-sqldf("select a.*, b.WDATE, b.AUDIENCE, b.REPTYPE, b.TOTAL, b.MALE,b.FEMALE,b.MU25,b.MO25,b.FU25,b.FO25,b.TU25,b.TO25,b.BLK, b.HSP, b.B1216,b.FREQ from 'nm_comps.df' a left join 
                      'nm_comp_tk2.df' b on a.mvid=b.MVID")

nm_comp_tk3.df$daate[nm_comp_tk3.df$mvid==fcast_mv]<-r_dt

# tk reshape -----------------------------------------------------------------

nm_comp_tks_r.df<-subset(nm_comp_tk3.df, AUDIENCE %in% c("REGULAR"))

nm_comp_tks_r.df<-sqldf("select distinct mvname, mvid,daate,  WDATE,genre,animation,  mpaa,three_d,seq, studio,WKNDDAY,WKNDGRS,HOLIDAY,SCRNS,AUDIENCE,REPTYPE, 
                        max(TOTAL*10) as TOTAL, max(MALE*10) as MALE,max(FEMALE*10) as FEMALE, max(MU25*10) as MU25,max(MO25*10) as MO25,max(FU25*10) as FU25,max(FO25*10) as FO25,max(TU25*10) as TU25,max(TO25*10) as TO25, max(BLK*10) as BLK, max(HSP*10) as HSP, max(B1216*10) as B1216, max(FREQ*10) as FREQ 
                        from 'nm_comp_tks_r.df' group by 1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16")

if(fcast_mv==67078){
  nm_comp_tks_r.df=subset(nm_comp_tks_r.df, mvid==fcast_mv | (WKNDGRS >= 32070000 ))
}
#for validation only
#nm_comp_tks_r.df=subset(nm_comp_tks_r.df, mvid==fcast_mv | daate < r_dt)

nm_comp_tks_r.df$opg_bo<-nm_comp_tks_r.df$WKNDGRS/1000000

nm_comp_tks_r.df<-subset(nm_comp_tks_r.df,select=-c(WKNDGRS))

nm_comp_tks_BLK.df<-subset(nm_comp_tk3.df, AUDIENCE %in% c("AFRICAN-AMERICAN (12 - 59)"))

nm_comp_tks_BLK.df<-sqldf("select distinct mvname, mvid,daate,  WDATE,genre,animation,  mpaa,three_d,seq, studio,WKNDDAY,WKNDGRS,HOLIDAY,SCRNS,AUDIENCE,REPTYPE, 
                          max(TOTAL*10) as TOTAL_BLK 
                          from 'nm_comp_tks_BLK.df' group by 1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16")

nm_comp_tks_LT.df<-subset(nm_comp_tk3.df, AUDIENCE %in% c("LATINOS (12 - 59)"))

nm_comp_tks_LT.df<-sqldf("select distinct mvname, mvid,daate,  WDATE,genre,animation,  mpaa,three_d,seq, studio,WKNDDAY,WKNDGRS,HOLIDAY,SCRNS,AUDIENCE,REPTYPE, 
                         max(TOTAL*10) as TOTAL_LT 
                         from 'nm_comp_tks_LT.df' group by 1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16")

nm_comp_tks_POC.df<-subset(nm_comp_tk3.df, AUDIENCE %in% c("PARENTS OF CHILD (4 - 11)"))

nm_comp_tks_POC.df<-sqldf("select distinct mvname, mvid,daate,  WDATE,genre,animation,  mpaa,three_d,seq, studio,WKNDDAY,WKNDGRS,HOLIDAY,SCRNS,AUDIENCE,REPTYPE, 
                          max(TOTAL*10) as TOTAL_POC 
                          from 'nm_comp_tks_POC.df' group by 1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16")

nm_comp_tks_r_BLK_LT_POC.df<-sqldf("select a.*,b.TOTAL_BLK,c.TOTAL_LT,d.TOTAL_POC from 'nm_comp_tks_r.df' a left join 'nm_comp_tks_BLK.df' b on
                                   a.daate=b.daate and a.mvid=b.mvid and a.WDATE=b.WDATE and a.REPTYPE=b.REPTYPE
                                   left join 'nm_comp_tks_LT.df' c on 
                                   a.daate=c.daate and a.mvid=c.mvid and a.WDATE=c.WDATE and a.REPTYPE=c.REPTYPE
                                   left join 'nm_comp_tks_POC.df' d on 
                                   a.daate=d.daate and a.mvid=d.mvid and a.WDATE=d.WDATE and a.REPTYPE=d.REPTYPE
                                   ")

nm_comp_tks_r_BLK_LT_POC.df$BLK[which(nm_comp_tks_r_BLK_LT_POC.df$TOTAL_BLK>=0)]<-nm_comp_tks_r_BLK_LT_POC.df$TOTAL_BLK[which(nm_comp_tks_r_BLK_LT_POC.df$TOTAL_BLK>=0)]

nm_comp_tks_r_BLK_LT_POC.df$HSP[which(nm_comp_tks_r_BLK_LT_POC.df$TOTAL_LT>=0)]<-nm_comp_tks_r_BLK_LT_POC.df$TOTAL_LT[which(nm_comp_tks_r_BLK_LT_POC.df$TOTAL_LT>=0)]

nm_comp_tks_r_BLK_LT2.df<-nm_comp_tks_r_BLK_LT_POC.df
nm_comp_tks_r_BLK_LT2.df$gender_R<-nm_comp_tks_r_BLK_LT2.df$MU25/nm_comp_tks_r_BLK_LT2.df$TOTAL*0.5*100
nm_comp_tks_r_BLK_LT2.df$female_R<-nm_comp_tks_r_BLK_LT2.df$FU25/nm_comp_tks_r_BLK_LT2.df$TOTAL*0.5*100
nm_comp_tks_r_BLK_LT2.df$age_R<-nm_comp_tks_r_BLK_LT2.df$MO25/nm_comp_tks_r_BLK_LT2.df$TOTAL*0.5*100
nm_comp_tks_r_BLK_LT2.df$ageo_R<-nm_comp_tks_r_BLK_LT2.df$FO25/nm_comp_tks_r_BLK_LT2.df$TOTAL*0.5*100

t_ttl<-dcast(nm_comp_tks_r_BLK_LT2.df, daate+mvname+mvid+WDATE+genre+animation+mpaa+three_d+seq+WKNDDAY+HOLIDAY+opg_bo+SCRNS+AUDIENCE ~ REPTYPE, value.var="TOTAL")

t_ttl<-dcast(nm_comp_tks_r.df, daate+mvname+mvid+WDATE+genre+animation+mpaa+three_d+seq+WKNDDAY+HOLIDAY+opg_bo+SCRNS+AUDIENCE ~ REPTYPE, value.var="TOTAL")

t_ttl$fc_r<-t_ttl$"FIRST CHOICE"
t_ttl$ua_r<-t_ttl$"UNAIDED AWARENESS"
t_ttl$ta_r<-t_ttl$"TOTAL AWARENESS"
t_ttl$di_r<-t_ttl$"DEFINITE INTERST"

t_ttl$ui_r<-t_ttl$"UNAID INTENT"
t_ttl$ust_r<-t_ttl$"UNSTOPPABLE"

t_ttl2<-dcast(nm_comp_tks_r.df, daate+mvname+mvid+WDATE+genre+animation+mpaa+three_d+seq+WKNDDAY+HOLIDAY+opg_bo+SCRNS+AUDIENCE ~ REPTYPE, value.var="FREQ")

t_ttl$fcf_r<-t_ttl2$"FIRST CHOICE"
t_ttl$uaf_r<-t_ttl2$"UNAIDED AWARENESS"
t_ttl$taf_r<-t_ttl2$"TOTAL AWARENESS"
t_ttl$dif_r<-t_ttl2$"DEFINITE INTERST"

t_ttl$uif_r<-t_ttl2$"UNAID INTENT"
t_ttl$ustf_r<-t_ttl2$"UNSTOPPABLE"


t_gender<-dcast(nm_comp_tks_r_BLK_LT2.df, daate+mvname+mvid+WDATE+genre+animation+mpaa+three_d+seq+WKNDDAY+HOLIDAY+opg_bo+SCRNS+AUDIENCE ~ REPTYPE, value.var="gender_R")

t_gender$fc_gender<-t_gender$"FIRST CHOICE"
t_gender$ua_gender<-t_gender$"UNAIDED AWARENESS"
t_gender$ta_gender<-t_gender$"TOTAL AWARENESS"
t_gender$di_gender<-t_gender$"DEFINITE INTERST"

t_gender$ui_gender<-t_gender$"UNAID INTENT"
t_gender$ust_gender<-t_gender$"UNSTOPPABLE"

t_female<-dcast(nm_comp_tks_r_BLK_LT2.df, daate+mvname+mvid+WDATE+genre+animation+mpaa+three_d+seq+WKNDDAY+HOLIDAY+opg_bo+SCRNS+AUDIENCE ~ REPTYPE, value.var="female_R")

t_female$fc_female<-t_female$"FIRST CHOICE"
t_female$ua_female<-t_female$"UNAIDED AWARENESS"
t_female$ta_female<-t_female$"TOTAL AWARENESS"
t_female$di_female<-t_female$"DEFINITE INTERST"

t_female$ui_female<-t_female$"UNAID INTENT"
t_female$ust_female<-t_female$"UNSTOPPABLE"


t_age<-dcast(nm_comp_tks_r_BLK_LT2.df, daate+mvname+mvid+WDATE+genre+animation+mpaa+three_d+seq+WKNDDAY+HOLIDAY+opg_bo+SCRNS+AUDIENCE ~ REPTYPE, value.var="age_R")

t_age$fc_age<-t_age$"FIRST CHOICE"
t_age$ua_age<-t_age$"UNAIDED AWARENESS"
t_age$ta_age<-t_age$"TOTAL AWARENESS"
t_age$di_age<-t_age$"DEFINITE INTERST"

t_age$ui_age<-t_age$"UNAID INTENT"
t_age$ust_age<-t_age$"UNSTOPPABLE"

t_ageo<-dcast(nm_comp_tks_r_BLK_LT2.df, daate+mvname+mvid+WDATE+genre+animation+mpaa+three_d+seq+WKNDDAY+HOLIDAY+opg_bo+SCRNS+AUDIENCE ~ REPTYPE, value.var="ageo_R")

t_ageo$fc_ageo<-t_ageo$"FIRST CHOICE"
t_ageo$ua_ageo<-t_ageo$"UNAIDED AWARENESS"
t_ageo$ta_ageo<-t_ageo$"TOTAL AWARENESS"
t_ageo$di_ageo<-t_ageo$"DEFINITE INTERST"

t_ageo$ui_ageo<-t_ageo$"UNAID INTENT"
t_ageo$ust_ageo<-t_ageo$"UNSTOPPABLE"

t_BLK<-dcast(nm_comp_tks_r_BLK_LT2.df, daate+mvname+mvid+WDATE+genre+animation+mpaa+three_d+seq+WKNDDAY+HOLIDAY+opg_bo+SCRNS+AUDIENCE ~ REPTYPE, value.var="BLK")

t_BLK$fc_BLK<-t_BLK$"FIRST CHOICE"
t_BLK$ua_BLK<-t_BLK$"UNAIDED AWARENESS"
t_BLK$ta_BLK<-t_BLK$"TOTAL AWARENESS"
t_BLK$di_BLK<-t_BLK$"DEFINITE INTERST"

t_BLK$ui_BLK<-t_BLK$"UNAID INTENT"
t_BLK$ust_BLK<-t_BLK$"UNSTOPPABLE"

t_HSP<-dcast(nm_comp_tks_r_BLK_LT2.df, daate+mvname+mvid+WDATE+genre+animation+mpaa+three_d+seq+WKNDDAY+HOLIDAY+opg_bo+SCRNS+AUDIENCE ~ REPTYPE, value.var="HSP")

t_HSP$fc_HSP<-t_HSP$"FIRST CHOICE"
t_HSP$ua_HSP<-t_HSP$"UNAIDED AWARENESS"
t_HSP$ta_HSP<-t_HSP$"TOTAL AWARENESS"
t_HSP$di_HSP<-t_HSP$"DEFINITE INTERST"

t_HSP$ui_HSP<-t_HSP$"UNAID INTENT"
t_HSP$ust_HSP<-t_HSP$"UNSTOPPABLE"

t_TN<-dcast(nm_comp_tks_r_BLK_LT2.df, daate+mvname+mvid+WDATE+genre+animation+mpaa+three_d+seq+WKNDDAY+HOLIDAY+opg_bo+SCRNS+AUDIENCE ~ REPTYPE, value.var="B1216")

t_TN$fc_TN<-t_TN$"FIRST CHOICE"
t_TN$ua_TN<-t_TN$"UNAIDED AWARENESS"
t_TN$ta_TN<-t_TN$"TOTAL AWARENESS"
t_TN$di_TN<-t_TN$"DEFINITE INTERST"

t_TN$ui_TN<-t_TN$"UNAID INTENT"
t_TN$ust_TN<-t_TN$"UNSTOPPABLE"

t_POC<-dcast(nm_comp_tks_r_BLK_LT2.df, daate+mvname+mvid+WDATE+genre+animation+mpaa+three_d+seq+WKNDDAY+HOLIDAY+opg_bo+SCRNS+AUDIENCE ~ REPTYPE, value.var="TOTAL_POC")

t_POC$fc_POC<-t_POC$"FIRST CHOICE"
t_POC$ua_POC<-t_POC$"UNAIDED AWARENESS"
t_POC$ta_POC<-t_POC$"TOTAL AWARENESS"
t_POC$di_POC<-t_POC$"DEFINITE INTERST"

t_POC$ui_POC<-t_POC$"UNAID INTENT"
t_POC$ust_POC<-t_POC$"UNSTOPPABLE"

t1.df<-sqldf("select a.*,b.fc_gender,b.ta_gender,b.ua_gender,b.di_gender,b.ui_gender,bb.fc_female,bb.ta_female,bb.ua_female,bb.di_female,bb.ui_female, c.ua_age, c.ta_age, c.fc_age, c.di_age, c.ui_age,cc.ua_ageo, cc.ta_ageo, cc.fc_ageo, cc.di_ageo,cc.ui_ageo,
             d.ua_BLK, d.ta_BLK, d.fc_BLK, d.di_BLK,d.ui_BLK, e.ua_HSP, e.ta_HSP, e.fc_HSP, e.di_HSP,e.ui_HSP,
             f.ua_TN, f.ta_TN, f.fc_TN, f.di_TN
             ,g.ua_POC, g.ta_POC, g.fc_POC, g.di_POC, g.ui_POC
             
             from t_ttl a left join t_gender b on a.mvname=b.mvname
             left join t_female bb on a.mvname=bb.mvname 
             left join t_age c on a.mvname=c.mvname
             left join t_ageo cc on a.mvname=cc.mvname
             left join t_BLK d on a.mvname=d.mvname
             left join t_HSP e on a.mvname=e.mvname
             left join t_TN f on a.mvname=f.mvname
             left join t_POC g on a.mvname=g.mvname
             ")
t1.df$fc_r[t1.df$fc_r==0]=.001
t1.df$ua_r[t1.df$ua_r==0]=.001
t1.df$ta_r[t1.df$ta_r==0]=.001
t1.df$di_r[t1.df$di_r==0]=.001
t1.df$fcf_r[t1.df$fcf_r==0]=.001
t1.df$uaf_r[t1.df$uaf_r==0]=.001
t1.df$taf_r[t1.df$taf_r==0]=.001
t1.df$dif_r[t1.df$dif_r==0]=.001
t1.df$fc_gender[t1.df$fc_gender==0]=.001
t1.df$ua_gender[t1.df$ua_gender==0]=.001
t1.df$ta_gender[t1.df$ta_gender==0]=.001
t1.df$di_gender[t1.df$di_gender==0]=.001
t1.df$fc_female[t1.df$fc_female==0]=.001
t1.df$ua_female[t1.df$ua_female==0]=.001
t1.df$di_female[t1.df$di_female==0]=.001
t1.df$fc_age[t1.df$fc_age==0]=.001
t1.df$ua_age[t1.df$ua_age==0]=.001
t1.df$di_age[t1.df$di_age==0]=.001
t1.df$fc_ageo[t1.df$fc_ageo==0]=.001
t1.df$ua_ageo[t1.df$ua_ageo==0]=.001
t1.df$di_ageo[t1.df$di_ageo==0]=.001

t1.df$Yr<-year(t1.df$daate)
t1.df$Mth<-month(t1.df$daate)

t1.df<-sqldf("select a.*, b.avg_price from 't1.df' a left join 'tkt_price.df' b on a.Yr-1=b.Yr")

t1.df<-sqldf("select a.*, b.Kids,b.College from 't1.df' a left join 'cal_sys_long.df' b on a.daate=b.dt")

t1.df<-sqldf("select a.*, b.FC_Season,b.UA_Season from 't1.df' a left join 'Tk_season.df' b on a.Mth=b.Mth")

# RT ------------------------------------------------------------------

t1.df<-sqldf("select a.*, b.RT from 't1.df' a left join rt1 b on a.MVID=b.MVID")

fc_days0<-r_dt-fcast_dt
#validation
#fc_days0=1
t1_daily.df<-sqldf(paste0("select a.*, b.avg_ptat,b.avg_wiki, b.avg_twit,b.SocialIndex from 't1.df' a left join 'my_social_idx.df' b on a.MVID=b.MVID and a.daate=b.MeasureDate+", fc_days0))

t1_daily.df$seq_v<-0
t1_daily.df$seq_v[t1_daily.df$seq=='SEQUEL']<-1

t1_daily.df$mpaa_v<-0
t1_daily.df$mpaa_v[t1_daily.df$mpaa=='R']<-1

t1_daily.df$obo2<-t1_daily.df$opg_bo

#adjustment for 4 day weekends
t1_daily.df$obo2[t1_daily.df$WKNDDAY %in% c(4) & t1_daily.df$HOLIDAY %in% c("MARTIN LUTHER KING","PRESIDENT'S DAY","FOURTH OF JULY","MEMORIAL DAY","LABOR DAY") ]=t1_daily.df$opg_bo[t1_daily.df$WKNDDAY %in% c(4) & t1_daily.df$HOLIDAY %in% c("MARTIN LUTHER KING","PRESIDENT'S DAY","FOURTH OF JULY","MEMORIAL DAY","LABOR DAY") ]*.80
t1_daily.df$obo2[t1_daily.df$WKNDDAY %in% c(4) & t1_daily.df$HOLIDAY %in% c("CHRISTMAS" ) ]=t1_daily.df$opg_bo[t1_daily.df$WKNDDAY %in% c(4) & t1_daily.df$HOLIDAY %in% c("CHRISTMAS" ) ]*.66

# Need to update the logic: Natasha 1 AUG 17
holidays=as.Date(holidayNYSE(year(Sys.Date())))
if(month(r_dt) == 12 & day(r_dt) > 26){
  holidays=as.Date(holidayNYSE(year(Sys.Date())+1))
}

t1_daily.df$monday=t1_daily.df$daate+3
for(i in 1:nrow(t1_daily.df)) {
  if(weekdays(t1_daily.df$daate[i])=='Sunday') {
    t1_daily.df$monday[i]=t1_daily.df$daate[i][weekdays(t1_daily.df$daate[i])=='Sunday']+1
  } else if(weekdays(t1_daily.df$daate[i])=='Saturday') {
    t1_daily.df$monday[i]=t1_daily.df$daate[i][weekdays(t1_daily.df$daate[i])=='Saturday']+2
  } else if(weekdays(t1_daily.df$daate[i])=='Thursday') {
    t1_daily.df$monday[i]=t1_daily.df$daate[i][weekdays(t1_daily.df$daate[i])=='Thursday']+4
  } else if(weekdays(t1_daily.df$daate[i])=='Wednesday') {
    t1_daily.df$monday[i]=t1_daily.df$daate[i][weekdays(t1_daily.df$daate[i])=='Wednesday']+5
  } else if(weekdays(t1_daily.df$daate[i])=='Tuesday') {
    t1_daily.df$monday[i]=t1_daily.df$daate[i][weekdays(t1_daily.df$daate[i])=='Tuesday']+6
  } else if(weekdays(t1_daily.df$daate[i])=='monday[i]') {
    t1_daily.df$monday[i]=t1_daily.df$daate[i][weekdays(t1_daily.df$daate[i])=='monday[i]']+7
  } }

t1_daily.df$WKNDDAY[is.na(t1_daily.df$WKNDDAY)==TRUE ]=3
t1_daily.df$WKNDDAY[is.na(t1_daily.df$WKNDDAY)==TRUE & t1_daily.df$monday %in% holidays]=4
t1_daily.df$WKNDDAY[is.na(t1_daily.df$WKNDDAY)==TRUE & (t1_daily.df$monday-1) == holidays[length(holidays)] ]=4
t1_daily.df$WKNDDAY[is.na(t1_daily.df$WKNDDAY)==TRUE & t1_daily.df$daate == holidays[length(holidays)] & day(t1_daily.df$daate) == 'Thursday']=4
t1_daily.df$WKNDDAY[is.na(t1_daily.df$WKNDDAY)==TRUE & t1_daily.df$daate == holidays[length(holidays)] & day(t1_daily.df$daate) == 'Wednesday']=5
t1_daily.df$WKNDDAY[is.na(t1_daily.df$WKNDDAY)==TRUE & t1_daily.df$monday %in% holidays & month(t1_daily.df$daate)==12]=5
t1_daily.df$WKNDDAY[is.na(t1_daily.df$WKNDDAY)==TRUE & (t1_daily.df$daate-1) %in% holidays & month(t1_daily.df$daate)==11]=5

t1_daily.df$opg_bo_bk<-t1_daily.df$opg_bo
t1_daily.df$opg_bo<-t1_daily.df$obo2 # weekend adjusted obo

t1_daily.df$action=t1_daily.df$comedy=t1_daily.df$drama=t1_daily.df$romance=t1_daily.df$family=t1_daily.df$horror=t1_daily.df$suspense=0

t1_daily.df$action[t1_daily.df$genre %in% c("ACTION COMEDY","ACTION ADVENTURE","ACTION DRAMA")]=1
t1_daily.df$comedy[t1_daily.df$genre %in% c("ACTION COMEDY","ZANY COMEDY","ROMANTIC COMEDY","HUMN INTST CMDY")]=1
t1_daily.df$drama[t1_daily.df$genre %in% c("ROMANTIC DRAMA","HUMN INTST DRMA","HUMAN INTST DRMA")]=1
t1_daily.df$romance[t1_daily.df$genre %in% c("ROMANTIC DRAMA","ROMANTIC COMEDY")]=1
t1_daily.df$family[t1_daily.df$genre %in% c("FAMILY")]=1
t1_daily.df$horror[t1_daily.df$genre %in% c("HORROR")]=1
t1_daily.df$suspense[t1_daily.df$genre %in% c("SUSPNS/THRILLER")]=1

t1_daily.df=sqldf("select a.*, c.CM_TYPE_OF_FILM, c.COMPANY_FRANCHISE, c.STUDIO_GROUP, c.FILM_FRANCHISE_SEQ from 't1_daily.df' a left join 'MVID' c 
                  on a.MVID=c.MVID")

t1_daily.df$PG=t1_daily.df$R=t1_daily.df$PG13=t1_daily.df$G=0
t1_daily.df$PG[t1_daily.df$mpaa=='PG']=1
t1_daily.df$R[t1_daily.df$mpaa=='R']=1
t1_daily.df$PG13[t1_daily.df$mpaa %in% c("PG-13",'PG13')]=1
t1_daily.df$G[t1_daily.df$mpaa=='G']=1

t1_daily.df$newip=t1_daily.df$exist=t1_daily.df$reboot=0
t1_daily.df$newip[!(t1_daily.df$CM_TYPE_OF_FILM %in% c('NEW FILM/ EXISTING IP','REBOOT','SEQUEL'))]=1
t1_daily.df$exist[t1_daily.df$CM_TYPE_OF_FILM %in% c('NEW FILM/ EXISTING IP','REBOOT','SEQUEL')]=1
t1_daily.df$reboot[t1_daily.df$CM_TYPE_OF_FILM == 'REBOOT']=1

t1_daily.df$dc=t1_daily.df$marvel=t1_daily.df$tyler=0
t1_daily.df$dc[t1_daily.df$COMPANY_FRANCHISE %in% c('Lego/DC','DC Entertainment','DC')]=1
t1_daily.df$marvel[t1_daily.df$COMPANY_FRANCHISE %in% c('Marvel')]=1
t1_daily.df$tyler[t1_daily.df$COMPANY_FRANCHISE %in% c('Tyler Perry')]=1

t1_daily.df$sony=t1_daily.df$wb=t1_daily.df$uni=t1_daily.df$fox=t1_daily.df$par=t1_daily.df$disney=t1_daily.df$lions=0
t1_daily.df$sony[t1_daily.df$STUDIO_GROUP=='SONY']=1
t1_daily.df$wb[t1_daily.df$STUDIO_GROUP=='WB']=1
t1_daily.df$uni[t1_daily.df$STUDIO_GROUP=='UNI']=1
t1_daily.df$fox[t1_daily.df$STUDIO_GROUP=='FOX']=1
t1_daily.df$par[t1_daily.df$STUDIO_GROUP=='PAR']=1
t1_daily.df$disney[t1_daily.df$STUDIO_GROUP=='DIS']=1
t1_daily.df$lions[t1_daily.df$STUDIO_GROUP=='LIONS']=1

t1_daily.df$franchise=t1_daily.df$bigfranchise=0
t1_daily.df$franchise[t1_daily.df$FILM_FRANCHISE_SEQ >= 1 & t1_daily.df$FILM_FRANCHISE_SEQ <= 3]=1
t1_daily.df$bigfranchise[t1_daily.df$FILM_FRANCHISE_SEQ > 3 ]=1
t1_daily.df$month=month(t1_daily.df$daate)
t1_daily.df$month1=t1_daily.df$month2=t1_daily.df$month3=t1_daily.df$month4=t1_daily.df$month5=t1_daily.df$month6=t1_daily.df$month7=t1_daily.df$month8=t1_daily.df$month9=t1_daily.df$month10=t1_daily.df$month11=t1_daily.df$month12=0
t1_daily.df$month1[t1_daily.df$month==1]=1
t1_daily.df$month2[t1_daily.df$month==2]=1
t1_daily.df$month3[t1_daily.df$month==3]=1
t1_daily.df$month4[t1_daily.df$month==4]=1
t1_daily.df$month5[t1_daily.df$month==5]=1
t1_daily.df$month6[t1_daily.df$month==6]=1
t1_daily.df$month7[t1_daily.df$month==7]=1
t1_daily.df$month8[t1_daily.df$month==8]=1
t1_daily.df$month9[t1_daily.df$month==9]=1
t1_daily.df$month10[t1_daily.df$month==10]=1
t1_daily.df$month11[t1_daily.df$month==11]=1
t1_daily.df$month12[t1_daily.df$month==12]=1
t1_daily.df$holiday=0
t1_daily.df$holiday[t1_daily.df$monday %in% holidays | t1_daily.df$daate %in% holidays | (t1_daily.df$daate-1) %in% holidays]=1
names(t1_daily.df)[length(names(t1_daily.df))]="holiday_flag"

t1_daily.df=sqldf("select T.*, o.VOLUME, o.TOTALVOLUME from 't1_daily.df' T left join 'OI' o on T.mvid=o.mvid and T.WDATE=o.MEASUREDATE")
t1_daily.df$VOLUME=as.numeric(t1_daily.df$VOLUME)
t1_daily.df$TOTALVOLUME=as.numeric(t1_daily.df$TOTALVOLUME)

# Bayesian ----------------------------------------------------------------
#do not include Rings in training
Lmdf_og=t1_daily.df
Lmdf<-subset(t1_daily.df, daate<fcast_dt & fc_r>0 & ua_r>0 & fc_age > 0 & ua_age > 0 & ua_age != Inf & fc_age != Inf)
Lmdf=Lmdf[!(Lmdf$mvid %in% c(62521)),]
#remove GOLD from Home Again comps
if(fcast_mv == 69990){
  Lmdf=Lmdf[!(Lmdf$mvid %in% c(35394,64768,68950)),]  
} else if(fcast_mv == 64567){
  Lmdf=Lmdf[!(Lmdf$mvid %in% c(44889,43645)),]  
} else if(fcast_mv == 69079){
  Lmdf=Lmdf[!(Lmdf$mvid %in% c(68767)),]  
} else if(fcast_mv == 69965){
  Lmdf=Lmdf[!(Lmdf$seq=="SEQUEL"),]
  Lmdf=Lmdf[!(Lmdf$opg_bo <=14),] 
} else if(fcast_mv == 72109){
  Lmdf=Lmdf[Lmdf$PG13 == 1,]
} else if (fcast_mv == 73128){
  Lmdf=Lmdf[Lmdf$R==1,]
}  

Lmdf<-subset(Lmdf, !(mvid %in% c(fcast_mv)))
Lmdf$cat<-'A'
Lmdf$cat[Lmdf$opg_bo<100]<-'B'
Lmdf$cat[Lmdf$opg_bo<50]<-'C'
Lmdf$cat[Lmdf$opg_bo<10]<-'D'

Lmdf$catA=Lmdf$catB=Lmdf$catC=Lmdf$catD=0
Lmdf$catA[Lmdf$cat=='A']=1
Lmdf$catB[Lmdf$cat=='B']=1
Lmdf$catC[Lmdf$cat=='C']=1
Lmdf$catD[Lmdf$cat=='D']=1

# 02/04/2020
#write.xlsx(Lmdf[,cols_4_lmdf], file= paste0("/mnt/WinMount/msci_BO_Fcast/training_titles/Training_ds_", short_mv, ".xls"),sheetName = "Titles used to Train", row.names = FALSE )

Fcdf<-subset(t1_daily.df, mvid==fcast_mv)  
#Fcdf<-subset(t1_daily.df, mvid==31612)  
#change weekend days
Fcdf$WKNDDAY[Fcdf$mvid %in% c(72109,69232,73084,70223,74981,69101)]=6
Fcdf$WKNDDAY[Fcdf$mvid %in% c(67097)]=7
Fcdf$WKNDDAY[Fcdf$mvid %in% c(69146,62373,72904,69778,72947,70010,69007,62376,73406,72111,49171,66336,70143,72436,67544,73559,65093,73311,75529,70119,77169,74681,67670,77927)]=5
Fcdf$WKNDDAY[Fcdf$mvid %in% c(66181,48914,69838,64237,71223,67611,67095,75944,74305,77619,75878,75843,73186,76223)]=4

Fcdf$SCRNS<-fcast_scrns
#Fcdf$SCRNS=1500
Fcdf$cat=fcast_cat
#Fcdf$cat[Fcdf$Group_C == maxprob]='C'
#Fcdf$cat[Fcdf$Group_B == maxprob]='B'
#Fcdf$cat[Fcdf$Group_A == maxprob]='A'
Fcdf$catA=Fcdf$catB=Fcdf$catC=Fcdf$catD=0
Fcdf$catA[Fcdf$cat=='A']=1
Fcdf$catB[Fcdf$cat=='B']=1
Fcdf$catC[Fcdf$cat=='C']=1
Fcdf$catD[Fcdf$cat=='D']=1
#fcast_cat=Fcdf$cat
if(fcast_cat=="A"){
  Lmdf$cat<-'A'
  # Lmdf$cat[Lmdf$opg_bo<100]<-'B'
  Lmdf$cat[Lmdf$opg_bo<50]<-'C'
  Lmdf$cat[Lmdf$opg_bo<10]<-'D'
  print(fcast_cat)
  
} else { if(fcast_cat=="B"){
  Lmdf$cat<-'A'
  Lmdf$cat[Lmdf$opg_bo<100]<-'B'
  #Lmdf$cat[Lmdf$opg_bo<50]<-'C'
  Lmdf$cat[Lmdf$opg_bo<10]<-'D'
  print(fcast_cat)
  
} else {if(fcast_cat=="C"){
  Lmdf$cat<-'A'
  #Lmdf$cat[Lmdf$opg_bo<100]<-'B'
  Lmdf$cat[Lmdf$opg_bo<50]<-'C'
  Lmdf$cat[Lmdf$opg_bo<10]<-'D'
  print(fcast_cat)
  
} else{ if(fcast_cat=="D"){
  Lmdf$cat<-'A'
  Lmdf$cat[Lmdf$opg_bo<100]<-'B'
  #Lmdf$cat[Lmdf$opg_bo<50]<-'C'
  Lmdf$cat[Lmdf$opg_bo<10]<-'D'
  print(fcast_cat)
  
}}}}

#if missing ua:
Fcdf$ua_gender[is.na(Fcdf$ua_gender)]=Fcdf$ua_r
Fcdf$ua_age[is.na(Fcdf$ua_age)]=Fcdf$ua_r
Fcdf$ua_ageo[is.na(Fcdf$ua_ageo)]=Fcdf$ua_r
Fcdf$ua_female[is.na(Fcdf$ua_female)]=Fcdf$ua_r
wknd_days = Fcdf$WKNDDAY

#tully
# Fcdf$ua_r=.1
# Fcdf$di_r=2.8
# Fcdf$ta_r=3.2
# Fcdf$fc_r=0.30995/2
# Fcdf$di_gender=3.5
# Fcdf$di_female=2.6
# Fcdf$di_age=3.0
# Fcdf$di_ageo=2.4
# Fcdf$ua_gender=.1
# Fcdf$ua_female=0
# Fcdf$ua_age=.1
# Fcdf$ua_ageo=.2
# Fcdf$fc_gender=Fcdf$fc_r
# Fcdf$fc_female=Fcdf$fc_r
# Fcdf$fc_age=Fcdf$fc_r
# Fcdf$fc_ageo=Fcdf$fc_r
# Fcdf$franchise=1
# Fcdf$exist=1
# Fcdf$newip=0
# Fcdf$comedy=1
# Fcdf$drama=0
# Fcdf$month4=0
# Fcdf$month5=1
# Fcdf$ua_POC=NA
# Fcdf$SocialIndex=86
# Fcdf$TOTALVOLUME=NA
# Fcdf$wb=0
# Fcdf$uni=1

#Fcdf$VOLUME=118004
#Fcdf$TOTALVOLUME=97180
# 05/16/15 median three cats
if(length(unique(Lmdf$cat))>1){
  fit_=lm(log(opg_bo)~cat+
            log(di_r)+log(ta_r)+log(ua_r)+log(fc_r)+
            log(SCRNS)+log(avg_price), data=Lmdf)
  
} else {
  fit_=lm(log(opg_bo)~
            log(di_r)+log(ta_r)+log(ua_r)+log(fc_r)+
            log(SCRNS)+log(avg_price), data=Lmdf)
}

fit_ <- stepAIC(fit_ ,direction="both")

summary(fit_)

rse<-sqrt(deviance(fit_)/df.residual(fit_))

Fcdf$avg_price<-8.89

Pred=predict(fit_, Fcdf, interval="predict", level=0.8)

cutoff <- 4/((nrow(Lmdf)-length(fit_$coefficients)-2))
lev = 3*(length(coef(fit_)) - 1 + 1)/nrow(Lmdf)
if(!(fcast_mv %in% c(69768,70092,47096,73917))){
  Lmdf=Lmdf[  abs(rstudent(fit_)) <= 3 & hatvalues(fit_) <= lev ,]
}


lrows=nrow(Lmdf)

#adjust TOTALVOLUME
if(Fcdf$mvid == 69126) {Fcdf$TOTALVOLUME = NA}

#for category:
trainset=cbind(log(Lmdf$di_r),log(Lmdf$ta_r),log(Lmdf$ua_r),log(Lmdf$fc_r),log(Lmdf$di_age),log(Lmdf$ua_age),log(Lmdf$fc_age),
               log(Lmdf$di_ageo),log(Lmdf$ua_ageo),log(Lmdf$fc_ageo),log(Lmdf$di_gender),log(Lmdf$ua_gender),log(Lmdf$fc_gender),log(Lmdf$di_female),log(Lmdf$ua_female),log(Lmdf$fc_female),Lmdf$R,
               log(Lmdf$SCRNS),log(Lmdf$avg_price),Lmdf$franchise,Lmdf$bigfranchise,Lmdf$reboot,Lmdf$exist,Lmdf$newip,Lmdf$tyler,Lmdf$marvel,Lmdf$dc,Lmdf$seq_v,Lmdf$family,Lmdf$animation,Lmdf$disney,
               Lmdf$action,Lmdf$comedy,Lmdf$drama,Lmdf$romance,Lmdf$horror,Lmdf$suspense,Lmdf$month1,Lmdf$month2,Lmdf$month3,Lmdf$month4,Lmdf$month5,Lmdf$month6,Lmdf$month7,Lmdf$month8,Lmdf$month9,Lmdf$month10,Lmdf$month11,Lmdf$month12)
lcols=dim(trainset)[2]
trainset <- mapply(trainset, FUN=as.numeric)
trainset = matrix(data=trainset,ncol=lcols, nrow=lrows)

fit_ = cv.glmnet(trainset,log(Lmdf$opg_bo),alpha=1)

validset=cbind(log(Fcdf$di_r),log(Fcdf$ta_r),log(Fcdf$ua_r),log(Fcdf$fc_r),log(Fcdf$di_age),log(Fcdf$ua_age),log(Fcdf$fc_age),
               log(Fcdf$di_ageo),log(Fcdf$ua_ageo),log(Fcdf$fc_ageo),log(Fcdf$di_gender),log(Fcdf$ua_gender),log(Fcdf$fc_gender),log(Fcdf$di_female),log(Fcdf$ua_female),log(Fcdf$fc_female),Fcdf$R,
               log(Fcdf$SCRNS),log(Fcdf$avg_price),Fcdf$franchise,Fcdf$bigfranchise,Fcdf$reboot,Fcdf$exist,Fcdf$newip,Fcdf$tyler,Fcdf$marvel,Fcdf$dc,Fcdf$seq_v,Fcdf$family,Fcdf$animation,Fcdf$disney,
               Fcdf$action,Fcdf$comedy,Fcdf$drama,Fcdf$romance,Fcdf$horror,Fcdf$suspense,Fcdf$month1,Fcdf$month2,Fcdf$month3,Fcdf$month4,Fcdf$month5,Fcdf$month6,Fcdf$month7,Fcdf$month8,Fcdf$month9,Fcdf$month10,Fcdf$month11,Fcdf$month12)
validset <- mapply(validset, FUN=as.numeric)
validset = matrix(data=validset,ncol=lcols, nrow=1)

pred=predict(fit_, validset, s = "lambda.1se")
base=exp(pred)

#===============
# For Category A
trainset=cbind(Lmdf$catA,Lmdf$catB,Lmdf$catC,Lmdf$catD,log(Lmdf$di_r),log(Lmdf$ta_r),log(Lmdf$ua_r),log(Lmdf$fc_r),log(Lmdf$di_age),log(Lmdf$ua_age),log(Lmdf$fc_age),
               log(Lmdf$di_ageo),log(Lmdf$ua_ageo),log(Lmdf$fc_ageo),log(Lmdf$di_gender),log(Lmdf$ua_gender),log(Lmdf$fc_gender),log(Lmdf$di_female),log(Lmdf$ua_female),log(Lmdf$fc_female),Lmdf$R,
               log(Lmdf$SCRNS),log(Lmdf$avg_price),Lmdf$franchise,Lmdf$bigfranchise,Lmdf$reboot,Lmdf$exist,Lmdf$newip,Lmdf$tyler,Lmdf$marvel,Lmdf$dc,Lmdf$seq_v,Lmdf$family,Lmdf$animation,Lmdf$disney,
               Lmdf$action,Lmdf$comedy,Lmdf$drama,Lmdf$romance,Lmdf$horror,Lmdf$suspense,Lmdf$month1,Lmdf$month2,Lmdf$month3,Lmdf$month4,Lmdf$month5,Lmdf$month6,Lmdf$month7,Lmdf$month8,Lmdf$month9,Lmdf$month10,Lmdf$month11,Lmdf$month12)
lcols=dim(trainset)[2]
trainset <- mapply(trainset, FUN=as.numeric)
trainset = matrix(data=trainset,ncol=lcols, nrow=lrows)


fit_ = cv.glmnet(trainset,log(Lmdf$opg_bo),alpha=1)


#Fcdf$catA,Fcdf$catB,Fcdf$catC,Fcdf$catD,
validset=cbind(1,0,0,0,log(Fcdf$di_r),log(Fcdf$ta_r),log(Fcdf$ua_r),log(Fcdf$fc_r),log(Fcdf$di_age),log(Fcdf$ua_age),log(Fcdf$fc_age),
               log(Fcdf$di_ageo),log(Fcdf$ua_ageo),log(Fcdf$fc_ageo),log(Fcdf$di_gender),log(Fcdf$ua_gender),log(Fcdf$fc_gender),log(Fcdf$di_female),log(Fcdf$ua_female),log(Fcdf$fc_female),Fcdf$R,
               log(Fcdf$SCRNS),log(Fcdf$avg_price),Fcdf$franchise,Fcdf$bigfranchise,Fcdf$reboot,Fcdf$exist,Fcdf$newip,Fcdf$tyler,Fcdf$marvel,Fcdf$dc,Fcdf$seq_v,Fcdf$family,Fcdf$animation,Fcdf$disney,
               Fcdf$action,Fcdf$comedy,Fcdf$drama,Fcdf$romance,Fcdf$horror,Fcdf$suspense,Fcdf$month1,Fcdf$month2,Fcdf$month3,Fcdf$month4,Fcdf$month5,Fcdf$month6,Fcdf$month7,Fcdf$month8,Fcdf$month9,Fcdf$month10,Fcdf$month11,Fcdf$month12)
validset <- mapply(validset, FUN=as.numeric)
validset = matrix(data=validset,ncol=lcols, nrow=1)

pred=predict(fit_, validset, s = "lambda.1se")
Pred_A=exp(pred)

#===============
# For Category B
trainset=cbind(Lmdf$catA,Lmdf$catB,Lmdf$catC,Lmdf$catD,log(Lmdf$di_r),log(Lmdf$ta_r),log(Lmdf$ua_r),log(Lmdf$fc_r),log(Lmdf$di_age),log(Lmdf$ua_age),log(Lmdf$fc_age),
               log(Lmdf$di_ageo),log(Lmdf$ua_ageo),log(Lmdf$fc_ageo),log(Lmdf$di_gender),log(Lmdf$ua_gender),log(Lmdf$fc_gender),log(Lmdf$di_female),log(Lmdf$ua_female),log(Lmdf$fc_female),Lmdf$R,
               log(Lmdf$SCRNS),log(Lmdf$avg_price),Lmdf$franchise,Lmdf$bigfranchise,Lmdf$reboot,Lmdf$exist,Lmdf$newip,Lmdf$tyler,Lmdf$marvel,Lmdf$dc,Lmdf$seq_v,Lmdf$family,Lmdf$animation,Lmdf$disney,
               Lmdf$action,Lmdf$comedy,Lmdf$drama,Lmdf$romance,Lmdf$horror,Lmdf$suspense,Lmdf$month1,Lmdf$month2,Lmdf$month3,Lmdf$month4,Lmdf$month5,Lmdf$month6,Lmdf$month7,Lmdf$month8,Lmdf$month9,Lmdf$month10,Lmdf$month11,Lmdf$month12)
lcols=dim(trainset)[2]
trainset <- mapply(trainset, FUN=as.numeric)
trainset = matrix(data=trainset,ncol=lcols, nrow=lrows)


fit_ = cv.glmnet(trainset,log(Lmdf$opg_bo),alpha=1)


#Fcdf$catA,Fcdf$catB,Fcdf$catC,Fcdf$catD,
validset=cbind(0,1,0,0,log(Fcdf$di_r),log(Fcdf$ta_r),log(Fcdf$ua_r),log(Fcdf$fc_r),log(Fcdf$di_age),log(Fcdf$ua_age),log(Fcdf$fc_age),
               log(Fcdf$di_ageo),log(Fcdf$ua_ageo),log(Fcdf$fc_ageo),log(Fcdf$di_gender),log(Fcdf$ua_gender),log(Fcdf$fc_gender),log(Fcdf$di_female),log(Fcdf$ua_female),log(Fcdf$fc_female),Fcdf$R,
               log(Fcdf$SCRNS),log(Fcdf$avg_price),Fcdf$franchise,Fcdf$bigfranchise,Fcdf$reboot,Fcdf$exist,Fcdf$newip,Fcdf$tyler,Fcdf$marvel,Fcdf$dc,Fcdf$seq_v,Fcdf$family,Fcdf$animation,Fcdf$disney,
               Fcdf$action,Fcdf$comedy,Fcdf$drama,Fcdf$romance,Fcdf$horror,Fcdf$suspense,Fcdf$month1,Fcdf$month2,Fcdf$month3,Fcdf$month4,Fcdf$month5,Fcdf$month6,Fcdf$month7,Fcdf$month8,Fcdf$month9,Fcdf$month10,Fcdf$month11,Fcdf$month12)
validset <- mapply(validset, FUN=as.numeric)
validset = matrix(data=validset,ncol=lcols, nrow=1)

pred=predict(fit_, validset, s = "lambda.1se")
Pred_B=exp(pred)

#===============
# For Category C
trainset=cbind(Lmdf$catA,Lmdf$catB,Lmdf$catC,Lmdf$catD,log(Lmdf$di_r),log(Lmdf$ta_r),log(Lmdf$ua_r),log(Lmdf$fc_r),log(Lmdf$di_age),log(Lmdf$ua_age),log(Lmdf$fc_age),
               log(Lmdf$di_ageo),log(Lmdf$ua_ageo),log(Lmdf$fc_ageo),log(Lmdf$di_gender),log(Lmdf$ua_gender),log(Lmdf$fc_gender),log(Lmdf$di_female),log(Lmdf$ua_female),log(Lmdf$fc_female),Lmdf$R,
               log(Lmdf$SCRNS),log(Lmdf$avg_price),Lmdf$franchise,Lmdf$bigfranchise,Lmdf$reboot,Lmdf$exist,Lmdf$newip,Lmdf$tyler,Lmdf$marvel,Lmdf$dc,Lmdf$seq_v,Lmdf$family,Lmdf$animation,Lmdf$disney,
               Lmdf$action,Lmdf$comedy,Lmdf$drama,Lmdf$romance,Lmdf$horror,Lmdf$suspense,Lmdf$month1,Lmdf$month2,Lmdf$month3,Lmdf$month4,Lmdf$month5,Lmdf$month6,Lmdf$month7,Lmdf$month8,Lmdf$month9,Lmdf$month10,Lmdf$month11,Lmdf$month12)
lcols=dim(trainset)[2]
trainset <- mapply(trainset, FUN=as.numeric)
trainset = matrix(data=trainset,ncol=lcols, nrow=lrows)


fit_ = cv.glmnet(trainset,log(Lmdf$opg_bo),alpha=1)


#Fcdf$catA,Fcdf$catB,Fcdf$catC,Fcdf$catD,
validset=cbind(0,0,1,0,log(Fcdf$di_r),log(Fcdf$ta_r),log(Fcdf$ua_r),log(Fcdf$fc_r),log(Fcdf$di_age),log(Fcdf$ua_age),log(Fcdf$fc_age),
               log(Fcdf$di_ageo),log(Fcdf$ua_ageo),log(Fcdf$fc_ageo),log(Fcdf$di_gender),log(Fcdf$ua_gender),log(Fcdf$fc_gender),log(Fcdf$di_female),log(Fcdf$ua_female),log(Fcdf$fc_female),Fcdf$R,
               log(Fcdf$SCRNS),log(Fcdf$avg_price),Fcdf$franchise,Fcdf$bigfranchise,Fcdf$reboot,Fcdf$exist,Fcdf$newip,Fcdf$tyler,Fcdf$marvel,Fcdf$dc,Fcdf$seq_v,Fcdf$family,Fcdf$animation,Fcdf$disney,
               Fcdf$action,Fcdf$comedy,Fcdf$drama,Fcdf$romance,Fcdf$horror,Fcdf$suspense,Fcdf$month1,Fcdf$month2,Fcdf$month3,Fcdf$month4,Fcdf$month5,Fcdf$month6,Fcdf$month7,Fcdf$month8,Fcdf$month9,Fcdf$month10,Fcdf$month11,Fcdf$month12)
validset <- mapply(validset, FUN=as.numeric)
validset = matrix(data=validset,ncol=lcols, nrow=1)

pred=predict(fit_, validset, s = "lambda.1se")
Pred_C=exp(pred)

#===============
# For Category D
trainset=cbind(Lmdf$catA,Lmdf$catB,Lmdf$catC,Lmdf$catD,log(Lmdf$di_r),log(Lmdf$ta_r),log(Lmdf$ua_r),log(Lmdf$fc_r),log(Lmdf$di_age),log(Lmdf$ua_age),log(Lmdf$fc_age),
               log(Lmdf$di_ageo),log(Lmdf$ua_ageo),log(Lmdf$fc_ageo),log(Lmdf$di_gender),log(Lmdf$ua_gender),log(Lmdf$fc_gender),log(Lmdf$di_female),log(Lmdf$ua_female),log(Lmdf$fc_female),Lmdf$R,
               log(Lmdf$SCRNS),log(Lmdf$avg_price),Lmdf$franchise,Lmdf$bigfranchise,Lmdf$reboot,Lmdf$exist,Lmdf$newip,Lmdf$tyler,Lmdf$marvel,Lmdf$dc,Lmdf$seq_v,Lmdf$family,Lmdf$animation,Lmdf$disney,
               Lmdf$action,Lmdf$comedy,Lmdf$drama,Lmdf$romance,Lmdf$horror,Lmdf$suspense,Lmdf$month1,Lmdf$month2,Lmdf$month3,Lmdf$month4,Lmdf$month5,Lmdf$month6,Lmdf$month7,Lmdf$month8,Lmdf$month9,Lmdf$month10,Lmdf$month11,Lmdf$month12)
lcols=dim(trainset)[2]
trainset <- mapply(trainset, FUN=as.numeric)
trainset = matrix(data=trainset,ncol=lcols, nrow=lrows)


fit_ = cv.glmnet(trainset,log(Lmdf$opg_bo),alpha=1)


#Fcdf$catA,Fcdf$catB,Fcdf$catC,Fcdf$catD,
validset=cbind(0,0,0,1,log(Fcdf$di_r),log(Fcdf$ta_r),log(Fcdf$ua_r),log(Fcdf$fc_r),log(Fcdf$di_age),log(Fcdf$ua_age),log(Fcdf$fc_age),
               log(Fcdf$di_ageo),log(Fcdf$ua_ageo),log(Fcdf$fc_ageo),log(Fcdf$di_gender),log(Fcdf$ua_gender),log(Fcdf$fc_gender),log(Fcdf$di_female),log(Fcdf$ua_female),log(Fcdf$fc_female),Fcdf$R,
               log(Fcdf$SCRNS),log(Fcdf$avg_price),Fcdf$franchise,Fcdf$bigfranchise,Fcdf$reboot,Fcdf$exist,Fcdf$newip,Fcdf$tyler,Fcdf$marvel,Fcdf$dc,Fcdf$seq_v,Fcdf$family,Fcdf$animation,Fcdf$disney,
               Fcdf$action,Fcdf$comedy,Fcdf$drama,Fcdf$romance,Fcdf$horror,Fcdf$suspense,Fcdf$month1,Fcdf$month2,Fcdf$month3,Fcdf$month4,Fcdf$month5,Fcdf$month6,Fcdf$month7,Fcdf$month8,Fcdf$month9,Fcdf$month10,Fcdf$month11,Fcdf$month12)
validset <- mapply(validset, FUN=as.numeric)
validset = matrix(data=validset,ncol=lcols, nrow=1)

pred=predict(fit_, validset, s = "lambda.1se")
Pred_D=exp(pred)
#==============
set.seed(fcast_mv)
trainset=cbind(Lmdf$catA,Lmdf$catB,Lmdf$catC,Lmdf$catD,log(Lmdf$di_r),log(Lmdf$ta_r),log(Lmdf$ua_r),log(Lmdf$fc_r),log(Lmdf$di_age),log(Lmdf$ua_age),log(Lmdf$fc_age),
               log(Lmdf$di_ageo),log(Lmdf$ua_ageo),log(Lmdf$fc_ageo),log(Lmdf$di_gender),log(Lmdf$ua_gender),log(Lmdf$fc_gender),log(Lmdf$di_female),log(Lmdf$ua_female),log(Lmdf$fc_female),Lmdf$R,
               log(Lmdf$SCRNS),log(Lmdf$avg_price),Lmdf$franchise,Lmdf$bigfranchise,Lmdf$reboot,Lmdf$exist,Lmdf$newip,Lmdf$tyler,Lmdf$marvel,Lmdf$dc,Lmdf$seq_v,Lmdf$family,Lmdf$animation,Lmdf$disney,
               Lmdf$action,Lmdf$comedy,Lmdf$drama,Lmdf$romance,Lmdf$horror,Lmdf$suspense,Lmdf$month1,Lmdf$month2,Lmdf$month3,Lmdf$month4,Lmdf$month5,Lmdf$month6,Lmdf$month7,Lmdf$month8,Lmdf$month9,Lmdf$month10,Lmdf$month11,Lmdf$month12)
lcols=dim(trainset)[2]
trainset <- mapply(trainset, FUN=as.numeric)
trainset = matrix(data=trainset,ncol=lcols, nrow=lrows)

fit_ = cv.glmnet(trainset,log(Lmdf$opg_bo),alpha=1)

lam.best=fit_$lambda.1se
coefs=as.vector(coef(fit_, s=lam.best))
cnames=c("Intercept","catA","catB","catC","catD","di_r","ta_r","ua_r","fc_r","di_age","ua_age","fc_age","di_ageo","ua_ageo","fc_ageo","di_gender","ua_gender","fc_gender","di_female","ua_female","fc_female","R",
         "SCRNS","avg_price","franchise","bigfranchise","reboot","exist","newip","tyler","marvel","dc","seq_v","family","animation","disney",
         "action","comedy","drama","romance","horror","suspense","month1","month2","month3","month4","month5","month6","month7","month8","month9","month10","month11","month12")


rse<-sqrt(deviance(fit_$glmnet.fit)[which(fit_$lambda==lam.best)]/(nrow(Lmdf)-length(coefs[abs(coefs)>0])))
#Fcdf$catA,Fcdf$catB,Fcdf$catC,Fcdf$catD,
validset=cbind(Fcdf$catA,Fcdf$catB,Fcdf$catC,Fcdf$catD,log(Fcdf$di_r),log(Fcdf$ta_r),log(Fcdf$ua_r),log(Fcdf$fc_r),log(Fcdf$di_age),log(Fcdf$ua_age),log(Fcdf$fc_age),
               log(Fcdf$di_ageo),log(Fcdf$ua_ageo),log(Fcdf$fc_ageo),log(Fcdf$di_gender),log(Fcdf$ua_gender),log(Fcdf$fc_gender),log(Fcdf$di_female),log(Fcdf$ua_female),log(Fcdf$fc_female),Fcdf$R,
               log(Fcdf$SCRNS),log(Fcdf$avg_price),Fcdf$franchise,Fcdf$bigfranchise,Fcdf$reboot,Fcdf$exist,Fcdf$newip,Fcdf$tyler,Fcdf$marvel,Fcdf$dc,Fcdf$seq_v,Fcdf$family,Fcdf$animation,Fcdf$disney,
               Fcdf$action,Fcdf$comedy,Fcdf$drama,Fcdf$romance,Fcdf$horror,Fcdf$suspense,Fcdf$month1,Fcdf$month2,Fcdf$month3,Fcdf$month4,Fcdf$month5,Fcdf$month6,Fcdf$month7,Fcdf$month8,Fcdf$month9,Fcdf$month10,Fcdf$month11,Fcdf$month12)
validset <- mapply(validset, FUN=as.numeric)
validset = matrix(data=validset,ncol=lcols, nrow=1)

pred=predict(fit_, validset, s = "lambda.1se")

Pred[1,1]=pred
Pred[1,2]=pred-qnorm(.9)*rse
Pred[1,3]=pred+qnorm(.9)*rse

Fc_output=data.frame(y=exp(Pred[1,1:3]))

Fc_output

Fcdf$opg_bo=exp(Pred[1,1])

Fcdf$RT<-RT_est
# Fcdf$TOTALVOLUME=32844
# Fcdf$VOLUME=34252

# prior -------------------------------------------------------------------

vPrior<-exp(predict(fit_, trainset, s = "lambda.1se"))
Lmdf<-cbind(Lmdf,vPrior)
names(Lmdf)[ncol(Lmdf)]='vPrior'
Fcdf$vPrior=Fcdf$opg_bo
B_sim<-NULL
RSE=rse

# 0-Seasonality -----------------------------------------------------------------------

source(paste0(proj_dir, "/BO/Auto_fcast/code/Daily/Test/G_Posterior_Season_091415.R")) # seasonality
#Fcdf$posterior_season<-Fcdf$vPrior
#Lmdf$posterior_season<-Lmdf$vPrior 
#Lmdf_pre0<-Lmdf

# 1- Rotten Tomatoes ------------------------------------------------------

if(!is.na(Fcdf$RT) & Fcdf$RT <= max(Lmdf$RT[is.na(Lmdf$RT)==FALSE])){
  #if(Fcdf$ua_HSP>200 & Fcdf$fc_HSP>200 & Fcdf$genre=='SKIP'){
  
  source(paste0(proj_dir, "/BO/Auto_fcast/code/Daily/Test/G_Posterior_RT_091515.R")) # Rotten Tomatoes
  
} else {
  Fcdf$posterior_RT<-Fcdf$vPrior
  Lmdf$posterior_RT<-Lmdf$vPrior
}

# 2 - Social --------------------------------------------------------------

if(!is.na(Fcdf$SocialIndex) & !(Fcdf$genre=='HORROR' & Fcdf$SocialIndex >= 200)){
  #if(Fcdf$ua_HSP>200 & Fcdf$fc_HSP>200 & Fcdf$genre=='SKIP'){
  
  source(paste0(proj_dir, "/BO/Auto_fcast/code/Daily/Test/G_Posterior_Social_091515.R")) # Social
  
} else {
  Fcdf$posterior_SI<-Fcdf$vPrior
  Lmdf$posterior_SI<-Lmdf$vPrior
} 


# 3 - Parent -----------------------------------------------------------------

if(is.na(Fcdf$ui_POC)==FALSE & Fcdf$ui_POC <= max(Lmdf$ui_POC[is.na(Lmdf$ui_POC)==FALSE]) & is.na(Fcdf$ua_POC)==FALSE & Fcdf$ua_POC>0 & Fcdf$fc_POC>0 & Fcdf$ua_POC <= max(Lmdf$ua_POC[is.na(Lmdf$ua_POC)==FALSE]) & Fcdf$fc_POC <= max(Lmdf$fc_POC[is.na(Lmdf$fc_POC)==FALSE]) & (Fcdf$genre=='FAMILY' | ((Fcdf$G==1 | Fcdf$PG==1 | Fcdf$PG13==1) & Fcdf$genre=='ACTION ADVENTURE'))){
  #if(Fcdf$genre=='SKIP'){  
  
  source(paste0(proj_dir, "/BO/Auto_fcast/code/Daily/Test/G_Posterior_POC_091515.R")) # POC
  
} else {
  Fcdf$posterior_POC<-Fcdf$vPrior
  Lmdf$posterior_POC<-Lmdf$vPrior
}

# 4 - UI --------------------------------------------------------------

(n_ui_r<-nrow(subset(Lmdf,ui_r>0 & ui_gender>0)))

if(Fcdf$ui_r>0 & n_ui_r>3 & Fcdf$ui_r <= max(Lmdf$ui_r[is.na(Lmdf$ui_r)==FALSE])){
  #if(Fcdf$ua_HSP>200 & Fcdf$fc_HSP>200 & Fcdf$genre=='SKIP'){
  
  source(paste0(proj_dir, "/BO/Auto_fcast/code/Daily/Test/G_Posterior_UI_020316.R")) # Social
  
} else {
  Fcdf$posterior_UI<-Fcdf$vPrior
  Lmdf$posterior_UI<-Lmdf$vPrior
} 


# # 5 - Organic Intent
# 
# library(bayesm)
# #
# if(!(is.na(Fcdf$TOTALVOLUME)) & Fcdf$TOTALVOLUME <= max(Lmdf$TOTALVOLUME[is.na(Lmdf$TOTALVOLUME)==FALSE])){
#   
#   # data --------------------------------------------------------------------
#   err=tryCatch({
#     
#     Lmdf_pre<-Lmdf
#     
#     y_1 <- log(Lmdf_pre$opg_bo)
#     x_1 <- cbind(matrix(1,nrow(Lmdf_pre),1),log(Lmdf_pre$vPrior), log(Lmdf_pre$VOLUME), log(Lmdf_pre$TOTALVOLUME))
#     
#     row_subset<-(Lmdf_pre$VOLUME>0 & is.na(Lmdf_pre$VOLUME)==FALSE & Lmdf_pre$TOTALVOLUME>0 & is.na(Lmdf_pre$TOTALVOLUME)==FALSE)
#     
#     #row_subset<-(Lmdf$RT>0)
#     
#     y_1<-y_1[row_subset]
#     y_1<-y_1[!is.na(y_1)]
#     
#     x_1<-x_1[row_subset,]
#     x_1<-x_1[!is.na(x_1[,1]),]
#     
#     Lmdf<-subset(Lmdf_pre,Lmdf_pre$VOLUME>0 & is.na(Lmdf_pre$VOLUME)==FALSE & Lmdf_pre$TOTALVOLUME>0 & is.na(Lmdf_pre$TOTALVOLUME)==FALSE)
#     
#     
#     # Bayesian training ----------------------------------------------------------------
#     
#     
#     dt_1 <- list(y=y_1,X=x_1)
#     
#     betabar_1 <- c(0, 1, 0,0)
#     
#     n<-length(betabar_1)+0
#     
#     A_1 <- 0.2 * diag(n)
#     n_1<-rse
#     
#     ssq_1 <- var(y_1)
#     Prior_1 <- list(betabar=betabar_1, A=A_1, nu=n_1, ssq=ssq_1)
#     
#     iter <- 10000
#     slice <- 1 
#     
#     MCMC <- list(R=iter, keep=slice)
#     
#     sim_1 <- runiregGibbs(dt_1, Prior_1, MCMC)
#     
#     burn_in<-1001
#     
#     sim_est<-as.matrix(sim_1$betadraw[burn_in:iter,])
#     sigma_est<-as.matrix(sim_1$sigmasqdraw[burn_in:iter])
#     
#     # posterior forecast ----------------------------------------------------------------
#     
#     fc_1 <- c(1,log(Fcdf$vPrior), log(Fcdf$VOLUME), log(Fcdf$TOTALVOLUME))
#     
#     f_posterior<-as.data.frame(exp(sim_est %*% fc_1))
#     
#     summary(f_posterior)
#     quantile(f_posterior$V1, c(.1, .5, .9), na.rm = T)
#     
#     
#     
#     # validate  ---------------------------------------------------------------
#     
#     
#     v<-as.data.frame(exp(sim_est %*% t(x_1)))
#     
#     vPosterior<-colMeans(v, na.rm = TRUE, dims = 1)
#     
#     #Lmdf<-cbind(Lmdf,vPosterior)
#     
#     abe_prior<-abs(Lmdf$opg_bo-Lmdf$vPrior)
#     MAPE_prior<-mean(abe_prior)/mean(Lmdf$opg_bo)
#     
#     abe_posterior<-abs(Lmdf$opg_bo-vPosterior)
#     MAPE_posterior<-mean(abe_posterior)/mean(Lmdf$opg_bo)
#     
#     MAPE_prior
#     MAPE_posterior
#     mean(f_posterior$V1)
#     
#     
#     if(MAPE_posterior<MAPE_prior){
#       Fcdf$posterior_OI<-mean(f_posterior$V1)
#       Fcdf$vPrior<-Fcdf$posterior_OI
#       Lmdf$posterior_OI<-vPosterior
#       Lmdf$vPrior<-vPosterior
#       B_sim<-f_posterior$V1
#       rse<-sqrt(mean(sigma_est))
#       coe<-colMeans(sim_est, na.rm = TRUE, dims = 1)
#     } else {
#       Fcdf$posterior_OI<-Fcdf$vPrior
#       Lmdf$posterior_OI<-Lmdf$vPrior  
#     }
#     
#     Lmdf_pre0<-sqldf("select a.*,b.posterior_OI from Lmdf_pre a left join Lmdf b on a.mvid=b.mvid")
#     
#     Lmdf_pre0$vPrior[is.na(Lmdf_pre0$posterior_OI) %in% FALSE]<-Lmdf_pre0$posterior_OI[is.na(Lmdf_pre0$posterior_OI) %in% FALSE]
#     
#     Lmdf<-Lmdf_pre0
#     
#   }, error=function(cond){return(1)})
#   
#   
#   if(length(err) == 1 ){
#     Fcdf$posterior_OI<-Fcdf$vPrior
#     Lmdf$posterior_OI<-Lmdf$vPrior 
#     Lmdf_pre0<-sqldf("select a.*,b.posterior_OI from Lmdf_pre a left join Lmdf b on a.mvid=b.mvid")
#     Lmdf_pre0$vPrior[is.na(Lmdf_pre0$posterior_OI) %in% FALSE]<-Lmdf_pre0$posterior_OI[is.na(Lmdf_pre0$posterior_OI) %in% FALSE]
#   }
#   
#   
# } else {
#   Fcdf$posterior_OI<-Fcdf$vPrior
#   Lmdf$posterior_OI<-Lmdf$vPrior 
#   Lmdf_pre0<-sqldf("select a.*,b.posterior_OI from Lmdf_pre a left join Lmdf b on a.mvid=b.mvid")
#   Lmdf_pre0$vPrior[is.na(Lmdf_pre0$posterior_OI) %in% FALSE]<-Lmdf_pre0$posterior_OI[is.na(Lmdf_pre0$posterior_OI) %in% FALSE]
# } 
# 

# 6 - AA/HSP


# data --------------------------------------------------------------------
# if(!(is.na(Fcdf$fc_BLK)) & Fcdf$fc_BLK <= max(Lmdf$fc_BLK[is.na(Lmdf$fc_BLK)==FALSE]) & !(is.na(Fcdf$fc_HSP)) & Fcdf$fc_HSP <= max(Lmdf$fc_HSP[is.na(Lmdf$fc_HSP)==FALSE]) &
#    Fcdf$ui_BLK <= max(Lmdf$ui_BLK[is.na(Lmdf$ui_BLK)==FALSE]) & Fcdf$ui_HSP <= max(Lmdf$ui_HSP[is.na(Lmdf$ui_HSP)==FALSE]) & is.na(Fcdf$ua_BLK)==FALSE &
#    Fcdf$ua_BLK <= max(Lmdf$ua_BLK[is.na(Lmdf$ua_BLK)==FALSE]) & Fcdf$ua_HSP <= max(Lmdf$ua_HSP[is.na(Lmdf$ua_HSP)==FALSE]) &
#    Fcdf$di_BLK <= max(Lmdf$di_BLK[is.na(Lmdf$di_BLK)==FALSE]) & Fcdf$di_HSP <= max(Lmdf$di_HSP[is.na(Lmdf$di_HSP)==FALSE])){
# 
# err=tryCatch({
# 
#   Lmdf_pre<-Lmdf
# 
#   y_1 <- log(Lmdf_pre$opg_bo)
#   x_1 <- cbind(matrix(1,nrow(Lmdf_pre),1),log(Lmdf_pre$vPrior), log(Lmdf_pre$fc_BLK),log(Lmdf_pre$ui_BLK),log(Lmdf_pre$ua_BLK),log(Lmdf_pre$di_BLK),log(Lmdf_pre$fc_HSP),log(Lmdf_pre$ui_HSP),log(Lmdf_pre$ua_HSP),log(Lmdf_pre$di_HSP))
# 
#   row_subset<-(is.na(Lmdf_pre$ui_HSP)==FALSE & is.na(Lmdf_pre$ui_BLK)==FALSE)
# 
#   #row_subset<-(Lmdf$RT>0)
# 
#   y_1<-y_1[row_subset]
#   y_1<-y_1[!is.na(y_1)]
# 
#   x_1<-x_1[row_subset,]
#   x_1<-x_1[!is.na(x_1[,1]),]
# 
#   Lmdf<-subset(Lmdf_pre,is.na(Lmdf_pre$ui_HSP)==FALSE & is.na(Lmdf_pre$ui_BLK)==FALSE)
# 
# 
#   # Bayesian training ----------------------------------------------------------------
# 
# 
#   dt_1 <- list(y=y_1,X=x_1)
# 
#   betabar_1 <- c(0, 1, 0,0,0,0,0,0,0,0)
# 
#   n<-length(betabar_1)+0
# 
#   A_1 <- 0.2 * diag(n)
#   n_1<-rse
# 
#   ssq_1 <- var(y_1)
#   Prior_1 <- list(betabar=betabar_1, A=A_1, nu=n_1, ssq=ssq_1)
# 
#   iter <- 10000
#   slice <- 1
# 
#   MCMC <- list(R=iter, keep=slice)
# 
#   sim_1 <- runiregGibbs(dt_1, Prior_1, MCMC)
# 
#   burn_in<-1001
# 
#   sim_est<-as.matrix(sim_1$betadraw[burn_in:iter,])
#   sigma_est<-as.matrix(sim_1$sigmasqdraw[burn_in:iter])
# 
#   # posterior forecast ----------------------------------------------------------------
# 
#   fc_1 <- c(1,log(Fcdf$vPrior), log(Fcdf$fc_BLK),log(Fcdf$ui_BLK),log(Fcdf$ua_BLK),log(Fcdf$di_BLK),log(Fcdf$fc_HSP),log(Fcdf$ui_HSP),log(Fcdf$ua_HSP),log(Fcdf$di_HSP))
# 
#   f_posterior<-as.data.frame(exp(sim_est %*% fc_1))
# 
#   summary(f_posterior)
#   quantile(f_posterior$V1, c(.1, .5, .9))
# 
# 
# 
#   # validate  ---------------------------------------------------------------
# 
# 
#   v<-as.data.frame(exp(sim_est %*% t(x_1)))
# 
#   vPosterior<-colMeans(v, na.rm = TRUE, dims = 1)
# 
#   #Lmdf<-cbind(Lmdf,vPosterior)
# 
#   abe_prior<-abs(Lmdf$opg_bo-Lmdf$vPrior)
#   MAPE_prior<-mean(abe_prior)/mean(Lmdf$opg_bo)
# 
#   abe_posterior<-abs(Lmdf$opg_bo-vPosterior)
#   MAPE_posterior<-mean(abe_posterior)/mean(Lmdf$opg_bo)
# 
#   MAPE_prior
#   MAPE_posterior
#   mean(f_posterior$V1)
# 
# 
#   if(MAPE_posterior<MAPE_prior){
#     Fcdf$posterior_ETH<-mean(f_posterior$V1)
#     Fcdf$vPrior<-Fcdf$posterior_ETH
#     Lmdf$posterior_ETH<-vPosterior
#     Lmdf$vPrior<-vPosterior
#     B_sim<-f_posterior$V1
#     rse<-sqrt(mean(sigma_est))
#     coe<-colMeans(sim_est, na.rm = TRUE, dims = 1)
#   } else {
#     Fcdf$posterior_ETH<-Fcdf$vPrior
#     Lmdf$posterior_ETH<-Lmdf$vPrior
#   }
# 
#   Lmdf_pre0<-sqldf("select a.*,b.posterior_ETH from Lmdf_pre a left join Lmdf b on a.mvid=b.mvid")
# 
#   Lmdf_pre0$vPrior[is.na(Lmdf_pre0$posterior_ETH) %in% FALSE]<-Lmdf_pre0$posterior_ETH[is.na(Lmdf_pre0$posterior_ETH) %in% FALSE]
# 
#   Lmdf<-Lmdf_pre0
# 
# }, error=function(cond){return(1)})
# 
# 
# if(length(err) == 1 ){
#   Fcdf$posterior_ETH<-Fcdf$vPrior
#   Lmdf$posterior_ETH<-Lmdf$vPrior
#   Lmdf_pre0<-sqldf("select a.*,b.posterior_ETH from Lmdf_pre a left join Lmdf b on a.mvid=b.mvid")
#   Lmdf_pre0$vPrior[is.na(Lmdf_pre0$posterior_ETH) %in% FALSE]<-Lmdf_pre0$posterior_ETH[is.na(Lmdf_pre0$posterior_ETH) %in% FALSE]
# }
# 
# } else {
#   Fcdf$posterior_ETH<-Fcdf$vPrior
#   Lmdf$posterior_ETH<-Lmdf$vPrior
#   Lmdf_pre0<-sqldf("select a.*,b.posterior_ETH from Lmdf_pre a left join Lmdf b on a.mvid=b.mvid")
#   Lmdf_pre0$vPrior[is.na(Lmdf_pre0$posterior_ETH) %in% FALSE]<-Lmdf_pre0$posterior_ETH[is.na(Lmdf_pre0$posterior_ETH) %in% FALSE]
# }


# 6 - Age


# data --------------------------------------------------------------------
# if(!(is.na(Fcdf$fc_age)) & Fcdf$fc_age <= max(Lmdf$fc_age[is.na(Lmdf$fc_age)==FALSE]) & !(is.na(Fcdf$fc_ageo)) & Fcdf$fc_ageo <= max(Lmdf$fc_ageo[is.na(Lmdf$fc_ageo)==FALSE]) &
#    Fcdf$ta_age <= max(Lmdf$ta_age[is.na(Lmdf$ta_age)==FALSE]) & Fcdf$ta_ageo <= max(Lmdf$ta_ageo[is.na(Lmdf$ta_ageo)==FALSE]) & is.na(Fcdf$ua_age)==FALSE &
#    Fcdf$ua_age <= max(Lmdf$ua_age[is.na(Lmdf$ua_age)==FALSE]) & Fcdf$ua_ageo <= max(Lmdf$ua_ageo[is.na(Lmdf$ua_ageo)==FALSE]) &
#    Fcdf$di_age <= max(Lmdf$di_age[is.na(Lmdf$di_age)==FALSE]) & Fcdf$di_ageo <= max(Lmdf$di_ageo[is.na(Lmdf$di_ageo)==FALSE])){
# 
# err=tryCatch({
# 
#   Lmdf_pre<-Lmdf
# 
#   y_1 <- log(Lmdf_pre$opg_bo)
#   x_1 <- cbind(matrix(1,nrow(Lmdf_pre),1),log(Lmdf_pre$vPrior), log(Lmdf_pre$fc_age),log(Lmdf_pre$ta_age),log(Lmdf_pre$ua_age),log(Lmdf_pre$di_age),log(Lmdf_pre$fc_ageo),log(Lmdf_pre$ta_ageo),log(Lmdf_pre$ua_ageo),log(Lmdf_pre$di_ageo))
# 
#   row_subset<-(Lmdf_pre$fc_age>0 & is.na(Lmdf_pre$fc_age)==FALSE)
# 
#   #row_subset<-(Lmdf$RT>0)
# 
#   y_1<-y_1[row_subset]
#   y_1<-y_1[!is.na(y_1)]
# 
#   x_1<-x_1[row_subset,]
#   x_1<-x_1[!is.na(x_1[,1]),]
# 
#   Lmdf<-subset(Lmdf_pre,Lmdf_pre$fc_age>0 & is.na(Lmdf_pre$fc_age)==FALSE)
# 
# 
#   # Bayesian training ----------------------------------------------------------------
# 
# 
#   dt_1 <- list(y=y_1,X=x_1)
# 
#   betabar_1 <- c(0, 1, 0,0,0,0,0,0,0,0)
# 
#   n<-length(betabar_1)+0
# 
#   A_1 <- 0.2 * diag(n)
#   n_1<-rse
# 
#   ssq_1 <- var(y_1)
#   Prior_1 <- list(betabar=betabar_1, A=A_1, nu=n_1, ssq=ssq_1)
# 
#   iter <- 10000
#   slice <- 1
# 
#   MCMC <- list(R=iter, keep=slice)
# 
#   sim_1 <- runiregGibbs(dt_1, Prior_1, MCMC)
# 
#   burn_in<-1001
# 
#   sim_est<-as.matrix(sim_1$betadraw[burn_in:iter,])
#   sigma_est<-as.matrix(sim_1$sigmasqdraw[burn_in:iter])
# 
#   # posterior forecast ----------------------------------------------------------------
# 
#   fc_1 <- c(1,log(Fcdf$vPrior), log(Fcdf$fc_age),log(Fcdf$ta_age),log(Fcdf$ua_age),log(Fcdf$di_age),log(Fcdf$fc_ageo),log(Fcdf$ta_ageo),log(Fcdf$ua_ageo),log(Fcdf$di_ageo))
# 
#   f_posterior<-as.data.frame(exp(sim_est %*% fc_1))
# 
#   summary(f_posterior)
#   quantile(f_posterior$V1, c(.1, .5, .9))
# 
# 
# 
#   # validate  ---------------------------------------------------------------
# 
# 
#   v<-as.data.frame(exp(sim_est %*% t(x_1)))
# 
#   vPosterior<-colMeans(v, na.rm = TRUE, dims = 1)
# 
#   #Lmdf<-cbind(Lmdf,vPosterior)
# 
#   abe_prior<-abs(Lmdf$opg_bo-Lmdf$vPrior)
#   MAPE_prior<-mean(abe_prior)/mean(Lmdf$opg_bo)
# 
#   abe_posterior<-abs(Lmdf$opg_bo-vPosterior)
#   MAPE_posterior<-mean(abe_posterior)/mean(Lmdf$opg_bo)
# 
#   MAPE_prior
#   MAPE_posterior
#   mean(f_posterior$V1)
# 
# 
#   if(MAPE_posterior<MAPE_prior){
#     Fcdf$posterior_AGE<-mean(f_posterior$V1)
#     Fcdf$vPrior<-Fcdf$posterior_AGE
#     Lmdf$posterior_AGE<-vPosterior
#     Lmdf$vPrior<-vPosterior
#     B_sim<-f_posterior$V1
#     rse<-sqrt(mean(sigma_est))
#     coe<-colMeans(sim_est, na.rm = TRUE, dims = 1)
#   } else {
#     Fcdf$posterior_AGE<-Fcdf$vPrior
#     Lmdf$posterior_AGE<-Lmdf$vPrior
#   }
# 
#   Lmdf_pre0<-sqldf("select a.*,b.posterior_AGE from Lmdf_pre a left join Lmdf b on a.mvid=b.mvid")
# 
#   Lmdf_pre0$vPrior[is.na(Lmdf_pre0$posterior_AGE) %in% FALSE]<-Lmdf_pre0$posterior_AGE[is.na(Lmdf_pre0$posterior_AGE) %in% FALSE]
# 
#   Lmdf<-Lmdf_pre0
# 
# }, error=function(cond){return(1)})
# 
# 
# if(length(err) == 1 ){
#   Fcdf$posterior_AGE<-Fcdf$vPrior
#   Lmdf$posterior_AGE<-Lmdf$vPrior
#   Lmdf_pre0<-sqldf("select a.*,b.posterior_AGE from Lmdf_pre a left join Lmdf b on a.mvid=b.mvid")
#   Lmdf_pre0$vPrior[is.na(Lmdf_pre0$posterior_AGE) %in% FALSE]<-Lmdf_pre0$posterior_AGE[is.na(Lmdf_pre0$posterior_AGE) %in% FALSE]
# }
# 
# } else {
#   Fcdf$posterior_AGE<-Fcdf$vPrior
#   Lmdf$posterior_AGE<-Lmdf$vPrior
#   Lmdf_pre0<-sqldf("select a.*,b.posterior_AGE from Lmdf_pre a left join Lmdf b on a.mvid=b.mvid")
#   Lmdf_pre0$vPrior[is.na(Lmdf_pre0$posterior_AGE) %in% FALSE]<-Lmdf_pre0$posterior_AGE[is.na(Lmdf_pre0$posterior_AGE) %in% FALSE]
# }
# # 7 - Teen


# data --------------------------------------------------------------------
# if(!(is.na(Fcdf$fc_TN)) & Fcdf$fc_TN <= max(Lmdf$fc_TN[is.na(Lmdf$fc_TN)==FALSE]) &
#    Fcdf$ta_TN <= max(Lmdf$ta_TN[is.na(Lmdf$ta_TN)==FALSE]) & is.na(Fcdf$ua_TN)==FALSE &
#    Fcdf$ua_TN <= max(Lmdf$ua_TN[is.na(Lmdf$ua_TN)==FALSE]) &
#    Fcdf$di_TN <= max(Lmdf$di_TN[is.na(Lmdf$di_TN)==FALSE]) ){
# 
# err=tryCatch({
# 
#   Lmdf_pre<-Lmdf
# 
#   y_1 <- log(Lmdf_pre$opg_bo)
#   x_1 <- cbind(matrix(1,nrow(Lmdf_pre),1),log(Lmdf_pre$vPrior), log(Lmdf_pre$fc_TN),log(Lmdf_pre$ta_TN),log(Lmdf_pre$ua_TN),log(Lmdf_pre$di_TN))
# 
#   row_subset<-(Lmdf_pre$fc_TN>0 & is.na(Lmdf_pre$fc_TN)==FALSE)
# 
#   #row_subset<-(Lmdf$RT>0)
# 
#   y_1<-y_1[row_subset]
#   y_1<-y_1[!is.na(y_1)]
# 
#   x_1<-x_1[row_subset,]
#   x_1<-x_1[!is.na(x_1[,1]),]
# 
#   Lmdf<-subset(Lmdf_pre,Lmdf_pre$fc_TN>0 & is.na(Lmdf_pre$fc_TN)==FALSE)
# 
# 
#   # Bayesian training ----------------------------------------------------------------
# 
# 
#   dt_1 <- list(y=y_1,X=x_1)
# 
#   betabar_1 <- c(0, 1, 0,0,0,0)
# 
#   n<-length(betabar_1)+0
# 
#   A_1 <- 0.2 * diag(n)
#   n_1<-rse
# 
#   ssq_1 <- var(y_1)
#   Prior_1 <- list(betabar=betabar_1, A=A_1, nu=n_1, ssq=ssq_1)
# 
#   iter <- 10000
#   slice <- 1
# 
#   MCMC <- list(R=iter, keep=slice)
# 
#   sim_1 <- runiregGibbs(dt_1, Prior_1, MCMC)
# 
#   burn_in<-1001
# 
#   sim_est<-as.matrix(sim_1$betadraw[burn_in:iter,])
#   sigma_est<-as.matrix(sim_1$sigmasqdraw[burn_in:iter])
# 
#   # posterior forecast ----------------------------------------------------------------
# 
#   fc_1 <- c(1,log(Fcdf$vPrior), log(Fcdf$fc_TN),log(Fcdf$ta_TN),log(Fcdf$ua_TN),log(Fcdf$di_TN))
# 
#   f_posterior<-as.data.frame(exp(sim_est %*% fc_1))
# 
#   summary(f_posterior)
#   quantile(f_posterior$V1, c(.1, .5, .9))
# 
# 
# 
#   # validate  ---------------------------------------------------------------
# 
# 
#   v<-as.data.frame(exp(sim_est %*% t(x_1)))
# 
#   vPosterior<-colMeans(v, na.rm = TRUE, dims = 1)
# 
#   #Lmdf<-cbind(Lmdf,vPosterior)
# 
#   abe_prior<-abs(Lmdf$opg_bo-Lmdf$vPrior)
#   MAPE_prior<-mean(abe_prior)/mean(Lmdf$opg_bo)
# 
#   abe_posterior<-abs(Lmdf$opg_bo-vPosterior)
#   MAPE_posterior<-mean(abe_posterior)/mean(Lmdf$opg_bo)
# 
#   MAPE_prior
#   MAPE_posterior
#   mean(f_posterior$V1)
# 
# 
#   if(MAPE_posterior<MAPE_prior){
#     Fcdf$posterior_TN<-mean(f_posterior$V1)
#     Fcdf$vPrior<-Fcdf$posterior_TN
#     Lmdf$posterior_TN<-vPosterior
#     Lmdf$vPrior<-vPosterior
#     B_sim<-f_posterior$V1
#     rse<-sqrt(mean(sigma_est))
#     coe<-colMeans(sim_est, na.rm = TRUE, dims = 1)
#   } else {
#     Fcdf$posterior_TN<-Fcdf$vPrior
#     Lmdf$posterior_TN<-Lmdf$vPrior
#   }
# 
#   Lmdf_pre0<-sqldf("select a.*,b.posterior_TN from Lmdf_pre a left join Lmdf b on a.mvid=b.mvid")
# 
#   Lmdf_pre0$vPrior[is.na(Lmdf_pre0$posterior_TN) %in% FALSE]<-Lmdf_pre0$posterior_TN[is.na(Lmdf_pre0$posterior_TN) %in% FALSE]
# 
#   Lmdf<-Lmdf_pre0
# 
# }, error=function(cond){return(1)})
# 
# 
# if(length(err) == 1 ){
#   Fcdf$posterior_TN<-Fcdf$vPrior
#   Lmdf$posterior_TN<-Lmdf$vPrior
#   Lmdf_pre0<-sqldf("select a.*,b.posterior_TN from Lmdf_pre a left join Lmdf b on a.mvid=b.mvid")
#   Lmdf_pre0$vPrior[is.na(Lmdf_pre0$posterior_TN) %in% FALSE]<-Lmdf_pre0$posterior_TN[is.na(Lmdf_pre0$posterior_TN) %in% FALSE]
# }
# 
# } else {
#   Fcdf$posterior_TN<-Fcdf$vPrior
#   Lmdf$posterior_TN<-Lmdf$vPrior
#   Lmdf_pre0<-sqldf("select a.*,b.posterior_TN from Lmdf_pre a left join Lmdf b on a.mvid=b.mvid")
#   Lmdf_pre0$vPrior[is.na(Lmdf_pre0$posterior_TN) %in% FALSE]<-Lmdf_pre0$posterior_TN[is.na(Lmdf_pre0$posterior_TN) %in% FALSE]
# }

#c=c("opg_bo","posterior_season","posterior_RT","posterior_SI","posterior_POC","posterior_UI","posterior_OI","posterior_GEN")
c=c("opg_bo","posterior_season","posterior_RT","posterior_SI","posterior_POC","posterior_UI","posterior_GEN")

#Fcdf$vPrior=rowMeans(Fcdf[,names(Fcdf) %in% c]) ,"posterior_AGE","posterior_TN"
# mark

# if(which(fcast_mv == mvid_trk)==1){
#    stackpred=Fcdf
# } else {
#    stackpred=rbind(stackpred,Fcdf)
# }

# categorization
#install.packages("gbm")
library("gbm")

# create group
Lmdf$Group <- ifelse(Lmdf$obo2<10, 1,
                     ifelse(Lmdf$obo2>=10 & Lmdf$obo2<50, 2, 
                            ifelse(Lmdf$obo2>=50 & Lmdf$obo2<100, 3, 4)))
if(length(unique(Lmdf$Group)) == 1){
  if(max(Lmdf$opg_bo) < 100 & max(Lmdf$opg_bo) >= 50){
    Lmdf$Group <- ifelse(Lmdf$obo2<10, 1,
                         ifelse(Lmdf$obo2>=10 & Lmdf$obo2<50, 2, 
                                ifelse(Lmdf$obo2>=50 & Lmdf$obo2 < max(Lmdf$opg_bo), 3, 4)))
  } else if(max(Lmdf$opg_bo) < 50 & max(Lmdf$opg_bo) >= 10) {
    Lmdf$Group <- ifelse(Lmdf$obo2<10, 1,
                         ifelse(Lmdf$obo2>=10 & Lmdf$obo2< max(Lmdf$opg_bo), 2, 
                                ifelse(Lmdf$obo2>= max(Lmdf$opg_bo) & Lmdf$obo2 < 100, 3, 4)))
  } else if(max(Lmdf$opg_bo) < 10){
    Lmdf$Group <- ifelse(Lmdf$obo2< max(Lmdf$opg_bo), 1,
                         ifelse(Lmdf$obo2>= max(Lmdf$opg_bo) & Lmdf$obo2<50, 2, 
                                ifelse(Lmdf$obo2>= max(Lmdf$opg_bo) & Lmdf$obo2 < 100, 3, 4)))
  }
}
# classification
Lmdf$ui_r[is.na(Lmdf$ui_r)]=Lmdf$ua_r[is.na(Lmdf$ui_r)]*.25
#Lmdf$ui_r[Lmdf$ui_r==0]=0.1

if(nrow(Lmdf)*.9 < 20) {
  bagsize=1  
} else {
  bagsize=.9
}


# + (seq_v)
if(length(unique(Lmdf$seq_v))==1) {
  if(is.na(Fcdf$fc_POC)==FALSE){
    mod_gb <- gbm(factor(Group) ~ (di_r) + (ta_r) + (ua_r) + (fc_r) + (di_age) + (ua_age) + (fc_age) + (di_ageo) + (ua_ageo) + (fc_ageo) + (di_gender) + (ua_gender) + (fc_gender) + (di_female) + (ua_female) + (fc_female) + 
                    (SCRNS) + (exist) + (ui_r) + (fc_POC) + (ua_POC),
                  data=Lmdf, shrinkage = 0.05, bag.fraction = bagsize, n.minobsinnode =3, distribution="multinomial")
    
  } else {
    mod_gb <- gbm(factor(Group) ~ (di_r) + (ta_r) + (ua_r) + (fc_r) + (di_age) + (ua_age) + (fc_age) + (di_ageo) + (ua_ageo) + (fc_ageo) + (di_gender) + (ua_gender) + (fc_gender) + (di_female) + (ua_female) + (fc_female) +
                    (SCRNS) + (exist) + (ui_r) ,
                  data=Lmdf, shrinkage = 0.05, bag.fraction = bagsize, n.minobsinnode =3, distribution="multinomial")
    
  } } else {
    if(is.na(Fcdf$fc_POC)==FALSE){
      mod_gb <- gbm(factor(Group) ~ (di_r) + (ta_r) + (ua_r) + (fc_r) + (di_age) + (ua_age) + (fc_age) + (di_ageo) + (ua_ageo) + (fc_ageo) + (di_gender) + (ua_gender) + (fc_gender) + (di_female) + (ua_female) + (fc_female) + 
                      (SCRNS) + (exist) + (seq_v) + (ui_r) + (fc_POC) + (ua_POC),
                    data=Lmdf, shrinkage = 0.05, bag.fraction = bagsize, n.minobsinnode =3, distribution="multinomial")
      
    } else {
      mod_gb <- gbm(factor(Group) ~ (di_r) + (ta_r) + (ua_r) + (fc_r) + (di_age) + (ua_age) + (fc_age) + (di_ageo) + (ua_ageo) + (fc_ageo) + (di_gender) + (ua_gender) + (fc_gender) + (di_female) + (ua_female) + (fc_female) +
                      (SCRNS) + (exist) + (seq_v) + (ui_r) ,
                    data=Lmdf, shrinkage = 0.05, bag.fraction = bagsize, n.minobsinnode =3, distribution="multinomial")
    }
  }



# df_test=cbind.data.frame(log(Fcdf$di_r),log(Fcdf$ta_r),log(Fcdf$ua_r),log(Fcdf$fc_r),log(Fcdf$di_age),log(Fcdf$ua_age),log(Fcdf$fc_age),
#                           log(Fcdf$di_ageo),log(Fcdf$ua_ageo),log(Fcdf$fc_ageo),log(Fcdf$di_gender),log(Fcdf$ua_gender),log(Fcdf$fc_gender),log(Fcdf$di_female),log(Fcdf$ua_female),log(Fcdf$fc_female),Fcdf$R,
#                           log(Fcdf$SCRNS),log(Fcdf$avg_price),Fcdf$holiday,log(Fcdf$WKNDDAY),Fcdf$franchise,Fcdf$bigfranchise,Fcdf$reboot,Fcdf$exist,Fcdf$newip,Fcdf$tyler,Fcdf$seq_v)
# 

pred_group_percentage <- predict(mod_gb, Fcdf, n.trees = 50, type="response")

if(length(unique(Lmdf$Group))==2){
  if(c(1) %in% Lmdf$Group & c(2) %in% Lmdf$Group){
    Fcdf$Group_D <- round(pred_group_percentage[ , 1, 1], 2)
    Fcdf$Group_C <- round(pred_group_percentage[ , 2, 1], 2)
    Fcdf$Group_B <- 0
    Fcdf$Group_A <- 0
  } else if(c(1) %in% Lmdf$Group & c(3) %in% Lmdf$Group){
    Fcdf$Group_D <- round(pred_group_percentage[ , 1, 1], 2)
    Fcdf$Group_B <- round(pred_group_percentage[ , 2, 1], 2)
    Fcdf$Group_C <- 0
    Fcdf$Group_A <- 0
  } else if(c(1) %in% Lmdf$Group & c(4) %in% Lmdf$Group){
    Fcdf$Group_D <- round(pred_group_percentage[ , 1, 1], 2)
    Fcdf$Group_A <- round(pred_group_percentage[ , 2, 1], 2)
    Fcdf$Group_C <- 0
    Fcdf$Group_B <- 0
  } else if(c(2) %in% Lmdf$Group & c(3) %in% Lmdf$Group){
    Fcdf$Group_C <- round(pred_group_percentage[ , 1, 1], 2)
    Fcdf$Group_B <- round(pred_group_percentage[ , 2, 1], 2)
    Fcdf$Group_D <- 0
    Fcdf$Group_A <- 0
  } else if(c(2) %in% Lmdf$Group & c(4) %in% Lmdf$Group){
    Fcdf$Group_C <- round(pred_group_percentage[ , 1, 1], 2)
    Fcdf$Group_A <- round(pred_group_percentage[ , 2, 1], 2)
    Fcdf$Group_D <- 0
    Fcdf$Group_B <- 0
  } else if(c(3) %in% Lmdf$Group & c(4) %in% Lmdf$Group){
    Fcdf$Group_B <- round(pred_group_percentage[ , 1, 1], 2)
    Fcdf$Group_A <- round(pred_group_percentage[ , 2, 1], 2)
    Fcdf$Group_D <- 0
    Fcdf$Group_C <- 0
  } 
} else if(length(unique(Lmdf$Group))==3){
  if(c(1) %in% Lmdf$Group & c(2) %in% Lmdf$Group & c(3) %in% Lmdf$Group){
    Fcdf$Group_D <- round(pred_group_percentage[ , 1, 1], 2)
    Fcdf$Group_C <- round(pred_group_percentage[ , 2, 1], 2)
    Fcdf$Group_B <- round(pred_group_percentage[ , 3, 1], 2)
    Fcdf$Group_A <- 0
  } else if(c(1) %in% Lmdf$Group & c(2) %in% Lmdf$Group & c(4) %in% Lmdf$Group){
    Fcdf$Group_D <- round(pred_group_percentage[ , 1, 1], 2)
    Fcdf$Group_C <- round(pred_group_percentage[ , 2, 1], 2)
    Fcdf$Group_A <- round(pred_group_percentage[ , 3, 1], 2)
    Fcdf$Group_B <- 0
  } else if(c(2) %in% Lmdf$Group & c(3) %in% Lmdf$Group & c(4) %in% Lmdf$Group){
    Fcdf$Group_C <- round(pred_group_percentage[ , 1, 1], 2)
    Fcdf$Group_B <- round(pred_group_percentage[ , 2, 1], 2)
    Fcdf$Group_A <- round(pred_group_percentage[ , 3, 1], 2)
    Fcdf$Group_D <- 0
  }
} else if(length(unique(Lmdf$Group))==4){
  Fcdf$Group_D <- round(pred_group_percentage[ , 1, 1], 2)
  Fcdf$Group_C <- round(pred_group_percentage[ , 2, 1], 2)
  Fcdf$Group_B <- round(pred_group_percentage[ , 3, 1], 2)
  Fcdf$Group_A <- round(pred_group_percentage[ , 4, 1], 2) 
}

#Fcdf$Group_D <- (1 - Fcdf$Group_A - Fcdf$Group_B - Fcdf$Group_C)

maxprob=max(Fcdf[,c("Group_D","Group_C","Group_B","Group_A")])
Fcdf$final=sum(Fcdf$vPrior,sum(Pred_A*Fcdf$Group_A,Pred_B*Fcdf$Group_B,Pred_C*Fcdf$Group_C,Pred_D*Fcdf$Group_D))/2

#if under official forecast, no adjustment; if over official forecast & 
if(official != 0 & Fcdf$vPrior/official > 1.05){
  B_sim<-B_sim * ((Fcdf$vPrior+official)/2)/(Fcdf$vPrior)
  Fcdf$vPrior= (Fcdf$vPrior+official)/2
  Fcdf$obo=Fcdf$vPrior
  
  Fc_output=data.frame(y=exp(Pred[1,1:3]))
} else if (official != 0 & Fcdf$vPrior/official < .865) {
  B_sim<-B_sim * ((Fcdf$vPrior+official)/2)/(Fcdf$vPrior)
  Fcdf$vPrior= (Fcdf$vPrior+official)/2
  Fcdf$obo=Fcdf$vPrior
  
}

if(Fcdf$mvid == 69959 ){ # do
  adj_=1.02
} else if(Fcdf$mvid == 63609){# bad
  adj_=1.025
} else if(Fcdf$mvid == 74082){ # boss
  adj_=1.035
} else if(Fcdf$mvid == 75247){ #  downtown
  adj_=1
} else {
  adj_=1
}

Fcdf$vPrior<-Fcdf$vPrior*adj_
B_sim<-B_sim*adj_

if(fcast_mv == 70282){
  Fcdf$SCRNS=1600
  fcast_scrns=1600
} else if(fcast_mv == 69497 ){
  Fcdf$SCRNS=800
  fcast_scrns=800
}

if(fcast_mv == mvid_trk[1]){
  stacknrow=nrow(Lmdf)
} else {
  stacknrow=rbind(stacknrow,nrow(Lmdf))
}

if(which(fcast_mv == mvid_trk)==1){
  stackbase=cbind(Fcdf[,c("mvid","mvname","cat","obo2","opg_bo","posterior_season","posterior_RT","posterior_SI","posterior_POC","posterior_UI","vPrior",
                          "Group_D", "Group_C", "Group_B", "Group_A","final")],
                  as.numeric(base), as.numeric(Pred_D), as.numeric(Pred_C),as.numeric(Pred_B),as.numeric(Pred_A))
} else {
  stackbase=rbind(stackbase,cbind(Fcdf[,c("mvid","mvname","cat","obo2","opg_bo","posterior_season","posterior_RT","posterior_SI","posterior_POC","posterior_UI","vPrior",
                                          "Group_D", "Group_C", "Group_B", "Group_A","final")],
                                  as.numeric(base), as.numeric(Pred_D), as.numeric(Pred_C),as.numeric(Pred_B),as.numeric(Pred_A)))
}



source(paste0(proj_dir, "/BO/Auto_fcast/code/plot_comps_M2M3.R"))

