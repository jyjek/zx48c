#' Week seasonality
#'
#' This function analyze last 8 weeks sales and calculate
#' coefficients of day inside week for each group of goods
#'
#' @param z0 dataset with historical sales
#' @param cat dataset with classificators
#' @return data frame with percentage of the day in week
#' @importFrom dplyr %>%
#' @importFrom ISOweek ISOweekday
#' @export

#globalVariables(c("SKU","sales_num","category","koef","DateISO"))

days_koef<-function(z0,cat){
  cat<-cat%>%magrittr::set_colnames(c("SKU","category"))
     dubl<-cat%>%
        dplyr::group_by(SKU)%>%
        dplyr::summarise(count=n_distinct(category))%>%
        dplyr::filter(count>1)%>%pull(SKU)
  if(length(dubl)>0) message(glue::glue("
                                          Warning: {length(dubl)} SKU present in few categories.
                                                   This SKUs removed from calculating. Forecast can be UNCORRECT"))
    cat<-cat%>%
      dplyr::filter(!SKU %in% dubl)
  qr<-z0%>%
    dplyr::filter(date>max(date)-lubridate::days(56))%>%
    dplyr::select(date,SKU,sales_num)%>%
    dplyr::ungroup()%>%
    dplyr::mutate(sales_num=ifelse(is.na(sales_num),0,sales_num),
                  DateISO=ISOweekday(date),
                  SKU=as.numeric(SKU))%>%
    dplyr::left_join(cat,by="SKU")%>%
    dplyr::group_by(category,DateISO)%>%
    dplyr::summarise(sales_num=sum(sales_num))%>%
    dplyr::mutate(koef=sales_num/sum(sales_num),
                  koef=ifelse(is.nan(koef),1/7,koef))%>%
    dplyr::select(-sales_num)
  return(qr)
}
