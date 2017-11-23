#' Filter historical data
#' This function filter historical data from upper values and replace that to mean
#' @param z0 dataset with historical sales
#' @return data frame with clear data
#' @import dplyr
#' @importFrom stats median
#' @importFrom stats sd
#' @export

filt <- function(z0){

  q0<-z0%>%
    #dplyr::select(SKU,date,sales_num)%>%
    group_by(SKU)%>%
    mutate(sd=sd(sales_num),
           new=ifelse(sales_num>mean(sales_num)+1.5*sd(sales_num),NA,sales_num ),
           new_sales=ifelse(is.na(new),mean(new,na.rm=T)+sd,new))%>%
    dplyr::select(-sales_num,-sd,-new)%>%
    dplyr::rename(sales_num=new_sales)
  return(q0)
}


#' Filter historical data from promo actions
#' This function filter historical data from  days with promo actions
#' @param z0 dataset with historical sales
#' @return data frame with clear data
#' @import dplyr
#' @export
filtNA<- function(z0){
  d<-z0%>%
    group_by(SKU)%>%
    mutate(gift=if_else(is.na(gift),0,gift),
           main=if_else(is.na(main),0,main),
           present_1c=if_else(is.na(present_1c),0,present_1c),
           sales_num=ifelse((present_1c==1 | gift==1 | main==1 | aval %in% c("S","A","O","N","ZZ") ),
                            NA,sales_num),
           sales_num=ifelse(is.na(sales_num),mean(sales_num,na.rm=T),sales_num),
           sales_num=if_else(is.nan(sales_num),0,sales_num))%>%
    #dplyr::select(date,SKU,sales_num)%>%
    group_by(SKU,date)%>%
    mutate(sales_num=mean(sales_num))

  return(d)}

#' Week seasonality
#' This function analyze last 8 weeks sales and calculate
#' coefficients of day inside week for each group of goods
#' @param z0 dataset with historical sales
#' @return data frame with percentage of the day in week
#' @import dplyr
#' @import tidyr
#' @importFrom lubridate days
#' @importFrom ISOweek ISOweekday
#' @export

days_koef<-function(z0){
  qr<-z0%>%
    dplyr::filter(date>max(date)-days(56))%>%
    dplyr::select(date,SKU,sales_num)%>%
    mutate(sales_num=ifelse(is.na(sales_num),0,sales_num),
           DateISO=ISOweekday(date))%>%
    left_join(q2,by="SKU")%>%
    dplyr::select(-dayRes)%>%
    group_by(classe,idclasse,DateISO)%>%
    summarise(sales_num=sum(sales_num))%>%
    mutate(koef=sales_num/sum(sales_num),
           koef=ifelse(is.nan(koef),1/7,koef))%>%
    dplyr::select(-sales_num)%>%
    spread(DateISO,koef)%>%
    dplyr::rename(category_name=classe,

                  category_id=idclasse)
  return(qr)
}

#' is.nan
#' Function analog to is.na for Nan values
#'
#' @param x variable for diagnostic
#' @return
#'
#' @export
#'
is.nan.data.frame <- function(x)
  do.call(cbind, lapply(x, is.nan))

#' Demand forecast
#' This function calculate demand forecast for next periods
#' using 5 methods and choose minimal MAPE
#' @param y dataset with historical filtered sales
#' @return data frame with forecast
#' @import dplyr
#' @importFrom ISOweek ISOweekday
#' @export
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
    group_by(SKU,week)%>%
    summarise(sales=sum(sales_num))%>%
    mutate(sales=ifelse(
      (mean(sales)<sd(sales)**2 & (sales>mean(sales)+sd(sales) | sales<sd(sales)-mean(sales)) ),NA,sales),
      sales=ifelse(sales==0,NA,sales))%>%
    group_by(SKU)%>%
    summarise(short=mean(sales,na.rm=T),
              short_M=median(sales,na.rm=T))%>%
    mutate(short=ifelse(is.na(short) | is.nan(short),0,short),
           short_M=ifelse(is.na(short_M) | is.nan(short_M),0,short_M))

  st1<-y%>%
    subset(week>m1-6)%>%
    group_by(SKU,week)%>%
    summarise(sales=sum(sales_num))%>%
    mutate(sales=ifelse(
      (sales>sd(sales)+mean(sales) | sales<sd(sales)-mean(sales)),NA,sales),
      sales=ifelse(sales==0,NA,sales))%>%
    group_by(SKU)%>%
    summarise(long=mean(sales,na.rm=T),
              long_M=median(sales,na.rm=T))%>%
    mutate(long=ifelse(is.na(long) | is.nan(long),0,long),
           long_M=ifelse(is.na(long_M) | is.nan(long_M),0,long_M))

  at1<-y%>%
    subset(week>m1-6)%>%
    group_by(SKU,week)%>%
    summarise(sales=sum(sales_num))%>%
    mutate(kof=c(0.3,0.4,0.5,0.6,0.8,1),
           mean=mean(sales),
           sd=sd(sales),
           sales=ifelse(sales>2*sd(sales)+mean(sales),0,sales),
           sales=sales*kof,
           kof=ifelse(sales==0,0,kof))%>%
    group_by(SKU)%>%
    summarise(trend=sum(sales)/sum(kof))%>%
    mutate(trend=ifelse(is.nan(trend),0,trend))

  progn<-y%>%
    subset(week==m1)%>%
    group_by(SKU)%>%
    summarise(fact=sum(sales_num))%>%
    full_join(at1,by="SKU")%>%
    full_join(st1,by="SKU")%>%
    full_join(wm1,by="SKU")%>%
    mutate(TM=abs(fact-trend)/fact*100,TM=ifelse(is.nan(TM),0,TM),
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

