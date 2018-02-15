#' Complete History
#'
#' This function make complete history when some data is dropped
#'
#' @param z0 dataset with historical sales
#' @param type type of dropped rows
#' @return data frame with full historical dataset
#' @importFrom dplyr %>%
#' @export

hist_compl<-function(z0,type="zero"){
  if(type=="zero"){
    q<-z0%>%
      tidyr::complete(date,SKU,fill = list(`in`=0,isAction=0,sales_num=0))%>%
      dplyr::group_by(SKU)%>%
      dplyr::mutate(balance_num=dplyr::if_else(is.na(balance_num),lag(balance_num,order_by=date),balance_num),
                    price=dplyr::if_else(is.na(price),round(mean(price,na.rm=T),2),price),
                    balance_num=dplyr::if_else(is.na(balance_num),mean(balance_num,na.rm = T),balance_num))
  }
  if(type=="drop"){
    q<-z0%>%
      tidyr::complete(date,SKU,   fill = list(`in`=0,isAction=0))%>%
      dplyr::group_by(SKU)%>%
      dplyr::mutate(sales_num=dplyr::if_else(is.na(sales_num),round(mean(sales_num,na.rm = T),2),sales_num),
                    price=dplyr::if_else(is.na(price),round(mean(price,na.rm=T),2),price),
                    balance_num=dplyr::if_else(is.na(balance_num),mean(balance_num,na.rm = T),balance_num))
  }
 return(q)
}

