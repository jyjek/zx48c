#' Filter historical data
#' This function filter historical data from upper values and replace that to mean
#' @param z0 dataset with historical sales
#' @return data frame with clear data
#' @import dplyr
#' @importFrom stats median
#' @importFrom stats sd
#' @export

filt <- function(z0){

  q0<-z0%>%
    #dplyr::select(SKU,date,sales_num)%>%
    dplyr::group_by(SKU)%>%
    mutate(sd=sd(sales_num),
           new=ifelse(sales_num>mean(sales_num)+1.5*sd(sales_num),NA,sales_num ),
           new_sales=ifelse(is.na(new),mean(new,na.rm=T)+sd,new))%>%
    dplyr::select(-sales_num,-sd,-new)%>%
    dplyr::rename(sales_num=new_sales)
  return(q0)
}
