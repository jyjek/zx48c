#' ABC analyses
#' This function make ABC analyses for each SKU
#' @param dt dataset with historical sales
#' @return data frame with ABC group for each SKU
#' @import dplyr
#' @import ABCanalysis
#' @importFrom lubridate days
#' @importFrom ISOweek ISOweekday
#' @export

my_abc<-function(dt,dl=c(.5,.5)){
  q1<-dt%>%
    dplyr::select(date,SKU,sales_num,price)%>%
    dplyr::rename(date=date)%>%
    dplyr:: mutate(#date=ymd_hms(date),
      price=as.numeric(price),
      sls=price*sales_num)%>%
    dplyr::filter(date>max(date)-days(42))%>%
    dplyr::group_by(SKU)%>%
    dplyr::summarise(sum_sal=sum(sales_num,na.rm=T),
              sum_sls=sum(sls,na.rm = T))%>%
    dplyr::mutate(nor_sales=scales::rescale(sum_sls,to=c(0,1)),
           nor_qnt=scales::rescale(sum_sal,to=c(0,1)),
           top=nor_sales*dl[1]+nor_qnt*dl[2],
           top=rescale(top,to=c(0,1)))%>%
    arrange(desc(top))%>%
    dplyr::mutate(cs=cumsum(top)/sum(top))

  Al=calculatedABCanalysis(q1$top)$ABlimit
  Bl=calculatedABCanalysis(q1$top)$BClimit

  tot<-q1%>%
    dplyr::mutate(abc=dplyr::if_else(cs<=Al,"A",
                                     dplyr::if_else(cs<=Bl,"B","C")))%>%
    dplyr::select(SKU,abc)
  return(tot)
}
