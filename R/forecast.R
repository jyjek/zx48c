globalVariables(c("n1","ALL","long","fact","week","sales","short","short_M","long_M","kof","trend_MAPE","short_M_MAPE","long_MAPE","long_M_MAPE","short_MAPE",
                  "trend","type","price","new","new_sales","SKU","sales_num","category","koef","DateISO","war","transf","fils","type_f",
                  "abc","cs","nor_sales","nor_qnt","sls","sum_sls","top","sum_sal","type","isAction","mn","balance_num","inn","ss","."))

#' Demand forecast
#'
#' This function make data transfom, filtration and calculate forecast
#'
#' @param z0 dataset with historical sales
#' @param catg dataset with classificators of SKU
#' @param transf type of forecast ("sku" or "fills")
#' @param hist avaliable of hist_comp function (for "cast" of history)
#' @param comp type of transform ("zero","drop")
#' @param filt type of filtration ("both","upper","lower")
#' @param A .9 coefficients for ABC group
#' @param B .8 coefficients for ABC group
#' @param C .7 coefficients ABC group
#' @return data frame with forecast
#' @importFrom  dplyr %>%
#' @export

run_forecast<-function(z0,catg,transf="sku",hist=T,comp="zero",filt="both",A=.9,B=.8,C=.7){
  if(!transf %in% c("sku","fills")){
    stop('Wrong "transf" value. It must be "sku","fills"')
  }
  if(!filt %in% c("both","upper","lower")){
    stop('Wrong "filt" value. It must be "both","lower" or "upper"')
  }
  if(!comp %in% c("zero","drop")){
    stop('Wrong "comp" value. It must be "zero" or "drop"')
  }
  if(A<0 | A>1){
    stop('Wrong "A" value. It must from 0 to 1')
  }
  if(B<0 | B>1){
    stop('Wrong "B" value. It must from 0 to 1')
  }
  if(C<0 | C>1){
    stop('Wrong "C" value. It must from 0 to 1')
  }
  if(A<B | A<C | B<C){
    stop('Wrong ABC value. It will must be A>B>C')
  }

  ifelse(hist,data<-z0%>%data_tranform(type=transf)%>%hist_compl(type=comp),data<-z0%>%data_tranform(type=transf))
  ds<-days_koef(data,catg)%>%
    inner_join(data.frame(date=seq(max(data$date)+1,max(data$date)+7,by="day"))%>%
                 mutate(DateISO=ISOweek::ISOweekday(date)),by="DateISO")

  fcst<-data%>%dplyr::filter(date>max(date)-lubridate::days(42))%>%
    filtNA(type=transf)%>%filt(type=filt,type_f=transf)%>%forecast()%>%
    dplyr::left_join(catg,by="SKU")%>%
    dplyr::inner_join(ds,by="category")%>%
    dplyr::left_join(Saf_Stock(data,A,B,C),by="SKU")%>%
    dplyr::mutate(forecast=round(koef*ALL,3),
                  ss=round(koef*ss,3))%>%
    dplyr::select(date,SKU,forecast,type,min,ss)
  return(fcst)
}



#' Demand forecast
#'
#' This function calculate demand forecast for next periods
#' using 5 methods and choose minimal MAPE
#'
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
    dplyr::mutate(kof=c(0.3,0.4,0.5,0.6,0.8,1)[(6-dplyr::n_distinct(y$week)+1):6],
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
    dplyr::mutate_at(vars(trend:short_M), .funs=funs(MAPE=abs(fact-.)/fact*100))%>%
    dplyr::mutate_all(funs(replace(.,is.nan(.),0)))%>%
    dplyr::mutate(min=pmin(trend_MAPE,short_M_MAPE,long_MAPE,long_M_MAPE,short_MAPE),
           type=case_when(
             pmin(trend_MAPE,short_M_MAPE,long_MAPE,long_M_MAPE,short_MAPE)==trend_MAPE~"Trend", pmin(trend_MAPE,short_M_MAPE,long_MAPE,long_M_MAPE,short_MAPE)==long_MAPE~"Long",
             pmin(trend_MAPE,short_M_MAPE,long_MAPE,long_M_MAPE,short_MAPE)==long_M_MAPE~"Long_M", pmin(trend_MAPE,short_M_MAPE,long_MAPE,long_M_MAPE,short_MAPE)==short_MAPE~"Short",
             pmin(trend_MAPE,short_M_MAPE,long_MAPE,long_M_MAPE,short_MAPE)==short_M_MAPE~"Short_M"),
           ALL=case_when(
             pmin(trend_MAPE,short_M_MAPE,long_MAPE,long_M_MAPE,short_MAPE)==trend_MAPE~trend, pmin(trend_MAPE,short_M_MAPE,long_MAPE,long_M_MAPE,short_MAPE)==long_MAPE~long,
             pmin(trend_MAPE,short_M_MAPE,long_MAPE,long_M_MAPE,short_MAPE)==long_M_MAPE~long_M, pmin(trend_MAPE,short_M_MAPE,long_MAPE,long_M_MAPE,short_MAPE)==short_MAPE~short,
             pmin(trend_MAPE,short_M_MAPE,long_MAPE,long_M_MAPE,short_MAPE)==short_M_MAPE~short_M),
           ALL=if_else(is.infinite(min),pmin(trend,long_M,short_M,long,short),ALL),
           min=if_else(is.infinite(min),100,min))%>%
    dplyr::select(SKU,ALL,fact,type,min)
  return(progn)
}





