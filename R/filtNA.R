#' Filter historical data from promo actions
#'
#' This function filter historical data from  days with promo actions
#'
#' @param z0 dataset with historical sales
#' @return data frame with clear data
#' @importFrom dplyr %>%
#' @export
#'
filtNA<- function(z0){

  d<-z0%>%
    dplyr::group_by(SKU)%>%
    dplyr::mutate(
      sales_num=ifelse(isAction==1,NA,sales_num),
      sales_num=ifelse(is.na(sales_num),mean(sales_num,na.rm=T),sales_num),
      sales_num=if_else(is.nan(sales_num),0,sales_num))%>%
    #dplyr::select(date,SKU,sales_num)%>%
    dplyr::group_by(SKU,date)%>%
    dplyr::mutate(sales_num=mean(sales_num))

  return(d)}
