#' Filter historical data from promo actions
#'
#' This function filter historical data from  days with promo actions
#'
#' @param z0 dataset with historical sales
#' @return data frame with clear data
#' @import dplyr
#' @export
#'
filtNA<- function(z0){

  d<-z0%>%
    dplyr::group_by(SKU)%>%
    dplyr::mutate(#gift=if_else(is.na(gift),0,gift),
      #main=if_else(is.na(main),0,main),
      #present_1c=if_else(is.na(present_1c),0,present_1c),
      #sales_num=ifelse((present_1c==1 | gift==1 | main==1 | aval %in% c("S","A","O","N","ZZ") ),
      #                  NA,sales_num),
      sales_num=ifelse(is.na(sales_num),mean(sales_num,na.rm=T),sales_num),
      sales_num=if_else(is.nan(sales_num),0,sales_num))%>%
    #dplyr::select(date,SKU,sales_num)%>%
    dplyr::group_by(SKU,date)%>%
    dplyr::mutate(sales_num=mean(sales_num))

  return(d)}
