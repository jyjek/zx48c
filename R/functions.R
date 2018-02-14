#' Demand forecast
#' This function calculate demand forecast for next periods
#' using 5 methods and choose minimal MAPE
#' @param y dataset with historical filtered sales
#' @return data frame with forecast
#' @import dplyr
#' @importFrom ISOweek ISOweekday
#' @export

globalVariables(c("n1","ALL","LM","TM","LmM","long","fact","week","sales","short","short_M","long_M","kof",
                  "trend","SM","SmM","type","price"))

forecast <- function(y){
  n1<-ISOweekday(as.Date(max(y$date)))

  if(n1==7){
    y$week <- as.numeric( format(y$date-1, "%U"))
  }else {
    y$week <- as.numeric( format(y$date-(n1+1), "%U"))}

  if(as.numeric(max(y$week))>51){
    if(as.numeric(max(y$week))==52){
      y$week[y$week==1]<-53;y$week[y$week==2]<-54;y$week[y$week==3]<-55
      y$week[y$week==4]<-56;y$week[y$week==5]<-57;y$week[y$week==6]<-58
    }
    if(as.numeric(max(y$week))==53){
      y$week[y$week==1]<-54;y$week[y$week==2]<-55;y$week[y$week==3]<-56
      y$week[y$week==4]<-57;y$week[y$week==5]<-58;y$week[y$week==6]<-59
    }
  }
  m1<-max(y$week)
  wm1<-y%>%
    subset(week>m1-4)%>%
    dplyr::group_by(SKU,week)%>%
    dplyr::summarise(sales=sum(sales_num))%>%
    dplyr::mutate(sales=ifelse(
      (mean(sales)<sd(sales)**2 & (sales>mean(sales)+sd(sales) | sales<sd(sales)-mean(sales)) ),NA,sales),
      sales=ifelse(sales==0,NA,sales))%>%
    dplyr::group_by(SKU)%>%
    dplyr::summarise(short=mean(sales,na.rm=T),
              short_M=median(sales,na.rm=T))%>%
    dplyr::mutate(short=ifelse(is.na(short) | is.nan(short),0,short),
           short_M=ifelse(is.na(short_M) | is.nan(short_M),0,short_M))

  st1<-y%>%
    subset(week>m1-6)%>%
    dplyr::group_by(SKU,week)%>%
    dplyr::summarise(sales=sum(sales_num))%>%
    dplyr::mutate(sales=ifelse(
      (sales>sd(sales)+mean(sales) | sales<sd(sales)-mean(sales)),NA,sales),
      sales=ifelse(sales==0,NA,sales))%>%
    dplyr::group_by(SKU)%>%
    dplyr::summarise(long=mean(sales,na.rm=T),
              long_M=median(sales,na.rm=T))%>%
    dplyr::mutate(long=ifelse(is.na(long) | is.nan(long),0,long),
           long_M=ifelse(is.na(long_M) | is.nan(long_M),0,long_M))

  at1<-y%>%
    subset(week>m1-6)%>%
    dplyr::group_by(SKU,week)%>%
    dplyr::summarise(sales=sum(sales_num))%>%
    dplyr::mutate(kof=c(0.3,0.4,0.5,0.6,0.8,1),
           mean=mean(sales),
           sd=sd(sales),
           sales=ifelse(sales>2*sd(sales)+mean(sales),0,sales),
           sales=sales*kof,
           kof=ifelse(sales==0,0,kof))%>%
    dplyr:: group_by(SKU)%>%
    dplyr::summarise(trend=sum(sales)/sum(kof))%>%
    dplyr::mutate(trend=ifelse(is.nan(trend),0,trend))

  progn<-y%>%
    subset(week==m1)%>%
    dplyr::group_by(SKU)%>%
    dplyr::summarise(fact=sum(sales_num))%>%
    dplyr::full_join(at1,by="SKU")%>%
    dplyr::full_join(st1,by="SKU")%>%
    dplyr::full_join(wm1,by="SKU")%>%
    dplyr::mutate(TM=abs(fact-trend)/fact*100,TM=ifelse(is.nan(TM),0,TM),
           LM=abs(fact-long)/fact*100,LM=ifelse(is.nan(LM),0,LM),
           LmM=abs(fact-long_M)/fact*100,LmM=ifelse(is.nan(LmM),0,LmM),
           SM=abs(fact-short)/fact*100,SM=ifelse(is.nan(SM),0,SM),
           SmM=abs(fact-short_M)/fact*100,SmM=ifelse(is.nan(SmM),0,SmM),
           min=pmin(TM,LM,LmM,SM,SmM),
           type=case_when(
             pmin(TM,LM,LmM,SM,SmM)==TM~"Trend", pmin(TM,LM,LmM,SM,SmM)==LM~"Long",
             pmin(TM,LM,LmM,SM,SmM)==LmM~"Long_M", pmin(TM,LM,LmM,SM,SmM)==SM~"Short",
             pmin(TM,LM,LmM,SM,SmM)==SmM~"Short_M"),
           ALL=case_when(
             pmin(TM,LM,LmM,SM,SmM)==TM~trend, pmin(TM,LM,LmM,SM,SmM)==LM~long,
             pmin(TM,LM,LmM,SM,SmM)==LmM~long_M, pmin(TM,LM,LmM,SM,SmM)==SM~short,
             pmin(TM,LM,LmM,SM,SmM)==SmM~short_M),
           ALL=if_else(is.infinite(min),pmin(trend,long,long_M,short,short_M),ALL),
           min=if_else(is.infinite(min),100,min))%>%
    dplyr::select(SKU,ALL,fact,type,min)
  return(progn)
}

