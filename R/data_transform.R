#' Data Transform
#'
#' This function transformating data to tidy data
#'
#' @param z0 dataset with historical sales
#' @return data frame with tidy dataset
#' @importFrom dplyr %>%
#' @export

  data_tranform<-function(z0){
    q<-z0%>%
      magrittr::set_colnames(c("date","SKU","in","sales_num","balance_num","price"))%>%
      dplyr::mutate(SKU=as.character(SKU),
                    `in`=as.numeric(`in`),
                    sales_num=as.numeric(sales_num),
                    balance_num=as.numeric(balance_num),
                    price=as.numeric(price),
                    date=lubridate::ymd(date))
  return(q)
    }
