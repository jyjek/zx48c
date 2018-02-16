#' Data Transform
#'
#' This function transformating data to tidy data
#'
#' @param z0 dataset with historical sales
#' @return data frame with tidy dataset
#' @importFrom dplyr %>%
#' @export

data_tranform<-function(z0){
  #if(ncol(z0)<7){z0$isAction<-0}
    q<-z0%>%
      magrittr::set_colnames(c("date","SKU","inn","sales_num","balance_num","price","isAction"))%>%
      dplyr::mutate(SKU=as.numeric(SKU),
                    inn=as.numeric(inn),
                    sales_num=as.numeric(sales_num),
                    balance_num=as.numeric(balance_num),
                    price=as.numeric(price),
                    date=lubridate::ymd(date))%>%
      dplyr::filter(date>max(date)-lubridate::days(56))
  return(q)
    }
