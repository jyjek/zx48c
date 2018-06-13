#' ABC analyses
#'
#' This function make ABC analyses for each SKU
#'
#'\itemize{
#' \item abc
#' \item cs
#' \item nor_sales
#' \item nor_qnt
#' \item sls
#' \item sum_sls
#' \item top
#' \item sum_sal
#'}
#'
#' @param dt dataset with historical sales
#' @param dl coefficients for sales number and money
#' @param type type of forecast ("sku" or "fills")
#' @return data frame with ABC group for each SKU
#' @import dplyr
#' @import ABCanalysis
#' @importFrom scales rescale
#' @importFrom lubridate days
#' @importFrom ISOweek ISOweekday
#' @export

#globalVariables(c("abc","cs","nor_sales","nor_qnt","sls","sum_sls","top","sum_sal"))

my_abc<-function(dt,dl=c(.5,.5),type="sku"){
  if(type=="sku"){ var<-c("SKU")}
  if(type=="fills"){ var<-c("SKU","fills")}
  q1<-dt%>%
    dplyr::select_at(c(var,"date","sales_num","price"))%>%
    dplyr::rename(date=date)%>%
    dplyr:: mutate(price=as.numeric(price),
                   sls=price*sales_num,
                   price=dplyr::if_else(is.na(price),mean(price,na.rm = T),price))%>%
    dplyr::filter(date>max(date)-lubridate::days(42))%>%
    dplyr::group_by_at(var)%>%
    dplyr::summarise(sum_sal=sum(sales_num,na.rm=T),
                     sum_sls=sum(sls,na.rm = T))%>%
    dplyr::mutate(nor_sales=rescale(sum_sls,to=c(0,1)),
                  nor_qnt=rescale(sum_sal,to=c(0,1)),
                  top=nor_sales*dl[1]+nor_qnt*dl[2],
                  top=rescale(top,to=c(0,1)))%>%
    arrange(desc(top))%>%
    dplyr::mutate(cs=cumsum(top)/sum(top))

  Al=calculatedABCanalysis(q1$top)$ABlimit
  Bl=calculatedABCanalysis(q1$top)$BClimit

  tot<-q1%>%
    dplyr::mutate(abc=dplyr::if_else(cs<=Al,"A",
                                     dplyr::if_else(cs<=Bl,"B","C")))%>%
    dplyr::select_at(c(var,"abc"))
  return(tot)
}

#' Safety Stock
#'
#' This function make Safety Stock value for each SKU
#'
#'\itemize{
#' \item abc
#' \item cs
#' \item nor_sales
#' \item nor_qnt
#' \item sls
#' \item sum_sls
#' \item top
#' \item sum_sal
#'}
#'
#' @param z0 dataset with historical sales
#' @param A coefficients for ABC group
#' @param B coefficients for ABC group
#' @param C coefficients ABC group
#' @param type type of forecast ("sku" or "fills")
#' @return data frame with ABC group for each SKU
#' @importFrom dplyr %>%
#' @importFrom stats qnorm
#' @export

Saf_Stock<-function(z0,A=.9,B=.8,C=.7,type_f="sku"){
  if(type_f=="sku"){ var<-c("SKU")}
  if(type_f=="fills"){ var<-c("SKU","fills")}
  SS<-z0%>%
    dplyr::inner_join(my_abc(.,type=type_f),by=var)%>%
    dplyr::mutate(koef=dplyr::case_when(abc=="A" ~ A,
                                        abc=="B" ~ B,
                                        abc=="C" ~ C))%>%
    dplyr::group_by_at(var)%>%
    dplyr::mutate(mean=mean(sales_num,na.rm=T),
                  sd=sd(sales_num,na.rm=T))%>%
    dplyr::filter(date==max(date))%>%
    dplyr::group_by_at(c(var,"koef"))%>%
    dplyr::mutate(ss=qnorm(koef,mean=mean,sd=sd,lower.tail=T))%>%
    dplyr::ungroup()%>%
    dplyr::select_at(c(var,"ss"))
  return(SS)
}
