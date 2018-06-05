#' Data Transform
#'
#' This function transformating data to tidy data
#'
#' @param z0 dataset with historical sales
#' @param type type of inputed dataset. it's sku or fills detalization
#' @return data frame with tidy dataset
#' @importFrom dplyr %>%
#' @importFrom glue glue
#' @export

data_tranform<-function(z0,type="sku"){

  if(!type %in% c("sku","fills")){
    stop('Wrong "type" value. It must be "sku","fils"')
  }

  if(type=="sku" & ncol(z0)!=7) message(glue::glue("
                                                   Warning: Dataset has non standart number of columns!
                                                   Check for present 'isAction' column."))

  if(type=="fills" & ncol(z0)!=8) message(glue::glue("
                                                     Warning: Dataset has non standart number of columns!
                                                     Check for present 'isAction' column."))
  if(type=="sku"){
    q<-z0%>%
      magrittr::set_colnames(c("date","SKU","inn","sales_num","balance_num","price","isAction"))%>%
      dplyr::mutate(SKU=as.numeric(SKU),
                    inn=as.numeric(inn),
                    sales_num=as.numeric(sales_num),
                    balance_num=as.numeric(balance_num),
                    price=as.numeric(price),
                    date=lubridate::ymd(date))%>%
      dplyr::filter(date>max(date)-lubridate::days(56))
  }
  if(type=="fills"){
    q<-z0%>%
      magrittr::set_colnames(c("date","fils","SKU","inn","sales_num","balance_num","price","isAction"))%>%
      dplyr::mutate(SKU=as.numeric(SKU),
                    fils=as.numeric(fils),
                    inn=as.numeric(inn),
                    sales_num=as.numeric(sales_num),
                    balance_num=as.numeric(balance_num),
                    price=as.numeric(price),
                    date=lubridate::ymd(date))%>%
      dplyr::filter(date>max(date)-lubridate::days(56))
  }
  return(q)
}
