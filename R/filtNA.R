#' Filter historical data from promo actions
#'
#' This function filter historical data from  days with promo actions
#'
#' @param z0 dataset with historical sales
#' @param type type of forecast ("sku" or "fills")
#' @return data frame with clear data
#' @importFrom dplyr %>%
#' @export
#'
filtNA<- function(z0,type="sku"){
  if(type=="sku"){ var<-c("SKU")}
  if(type=="fills"){ var<-c("SKU","fills")}

 return(z0%>%
    dplyr::group_by_at(var)%>%
    dplyr::mutate(
      sales_num=ifelse(isAction==1,NA,sales_num),
      sales_num=ifelse(is.na(sales_num),mean(sales_num,na.rm=T),sales_num),
      sales_num=if_else(is.nan(sales_num),0,sales_num))%>%
    #dplyr::select(date,SKU,sales_num)%>%
    dplyr::group_by_at(c(var,"date"))%>%
    dplyr::mutate(sales_num=mean(sales_num)))%>%
    dplyr::as_data_frame()
}
