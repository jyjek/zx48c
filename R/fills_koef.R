#' Fills part
#'
#' This function analyze  sales and calculate
#' coefficients for each FillID
#'
#' @param z0 dataset with historical sales
#' @return data frame with percentage of the day in week
#' @importFrom dplyr %>%
#' @importFrom lubridate days
#' @export


fills_koef<-function(z0){
  qr<-z0%>%
    data_tranform(type="fills")%>%
    dplyr::filter(date>=(max(date)-lubridate::days(42)))%>%
    dplyr::group_by(SKU,fills)%>%
    dplyr::summarise(tot=sum(sales_num ))%>%
    dplyr::ungroup()%>%
    dplyr::group_by(SKU)%>%
    dplyr::mutate(proc=tot/sum(tot),
                  proc=ifelse(is.na(proc),0,proc))%>%
    dplyr::select(-tot)
  return(qr)
}
