#' Filter historical data
#'
#' This function filter historical data from upper values and replace that to mean
#'
#' @param z0 dataset with historical sales
#' @param type type of filtration ("both","upper","lower")
#' @param type_f type of forecast ("sku" or "fills")
#' @return data frame with clear data
#' @importFrom  dplyr %>%
#' @importFrom stats median
#' @importFrom stats sd
#' @export


filt <- function(z0,type="both",type_f="sku"){
  if(!type %in% c("both","upper","lower")){
    stop('Wrong "filt" value. It must be "both","lower" or "upper"')
  }
  if(type_f=="sku"){ var<-c("SKU")}
  if(type_f=="fills"){ var<-c("SKU","fills")}

  if(type=="upper"){
    q0<-z0%>%
      dplyr::select_at(c(var,"date","sales_num","balance_num"))%>%
      dplyr::group_by_at(var)%>%
      dplyr::mutate(sd=sd(sales_num,na.rm=T),
                    new=ifelse(sales_num>mean(sales_num,na.rm=T)+1.5*sd(sales_num,na.rm=T),NA,sales_num ),
                    new_sales=ifelse(is.na(new),mean(new,na.rm=T)+sd,new))%>%
      dplyr::select(-sales_num,-sd,-new)%>%
      dplyr::rename(sales_num=new_sales)%>%
      dplyr::as_data_frame()
  }
  if(type=="both"){
    q0<-z0%>%
      dplyr::select_at(c(var,"date","sales_num","balance_num"))%>%
      dplyr::group_by_at(var)%>%
      dplyr::mutate(mn=mean(sales_num,na.rm=T),
                    sales_num=ifelse(balance_num<mn,NA,sales_num),
                    new=ifelse(sales_num>mean(sales_num,na.rm=T)+1.5*sd(sales_num,na.rm=T),NA,sales_num),
                    new_sales=ifelse(is.na(new),mean(new,na.rm=T)+sd(new,na.rm=T),new))%>%
      dplyr::select_at(c(var,"date","new_sales"))%>%
      dplyr::rename(sales_num=new_sales)%>%
      dplyr::as_data_frame()
  }
  if(type=="lower"){
    q0<-z0%>%
      dplyr::select_at(c(var,"date","sales_num","balance_num"))%>%
      dplyr::group_by_at(var)%>%
      dplyr::mutate(new=ifelse(balance_num<mean(sales_num,na.rm = T),NA,sales_num),
                    new_sales=ifelse(is.na(new),mean(new,na.rm=T)+sd(new,na.rm=T),new),
                    new_sales=ifelse(is.nan(new_sales),0,new_sales))%>%
      dplyr::select_at(c(var,"date","new_sales"))%>%
      dplyr::rename(sales_num=new_sales)%>%
      dplyr::as_data_frame()
  }
  q0<-q0%>%
    dplyr::mutate(sales_num=ifelse(is.na(sales_num),0,sales_num))%>%
    dplyr::as_data_frame()
  return(q0)
}
