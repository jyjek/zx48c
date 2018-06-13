#' Complete History
#'
#' This function make complete history when some data is dropped
#'
#' @param z0 dataset with historical sales
#' @param type  "zero" or "drop". Type of dropped rows
#' @param transf "sku" or "fills" for sku or fills detalization
#' @return data frame with full historical dataset
#' @importFrom dplyr %>%
#' @export

hist_compl<-function(z0,type="zero",transf="sku"){
  if(!type %in% c("zero","drop")){
    stop('Wrong "type" value. It must be "zero" or "drop"')
  }
  if(transf=="sku"){ var<-c("SKU")}
  if(transf=="fills"){ var<-c("SKU","fills")}

  war<-z0%>%
    dplyr::group_by_at(var)%>%
    dplyr::summarise(count=n_distinct(date))%>%
    dplyr::filter(count<56)%>%
    dplyr::pull(SKU)%>%unique()
  if(length(war)>0) message(glue::glue("Warning: {length(war)} SKU has short history"))

  if(length(war)>0){
    if(type=="zero"){
      q<-z0%>%
        dplyr::filter(SKU %in% war)%>%
        tidyr::complete_(c(var,"date"),fill = list(inn=0,isAction=0,sales_num=0))%>%
        arrange(date)%>%
        # tidyr::complete(date,SKU,fill = list(inn=0,isAction=0,sales_num=0,isAction=0))%>%
        dplyr::group_by_at(var)%>%
        tidyr::fill(balance_num)%>%
        dplyr::mutate(price=dplyr::if_else(is.na(price),round(mean(price,na.rm=T),2),price),
                      balance_num=dplyr::if_else(is.na(balance_num),mean(balance_num,na.rm = T),balance_num),
                      balance_num=dplyr::if_else(is.nan(balance_num),0,balance_num))%>%
        dplyr::bind_rows(.,z0%>%dplyr::filter(!SKU %in% war))%>%
        dplyr::as_data_frame()
    }
    if(type=="drop"){
      q<-z0%>%
        dplyr::filter(SKU %in% war)%>%
        tidyr::complete_(c(var,"date"),fill = list(inn=0,isAction=0))%>%
        arrange(date)%>%
        # tidyr::complete(date,SKU,fill = list(inn=0,isAction=0))%>%
        dplyr::group_by_at(var)%>%
        dplyr::mutate(sales_num=dplyr::if_else(is.na(sales_num),round(mean(sales_num,na.rm = T),2),sales_num),
                      price=dplyr::if_else(is.na(price),round(mean(price,na.rm=T),2),price),
                      balance_num=dplyr::if_else(is.na(balance_num),mean(balance_num,na.rm = T),balance_num),
                      balance_num=dplyr::if_else(is.nan(balance_num),0,balance_num))%>%
        dplyr::bind_rows(.,z0%>%dplyr::filter(!SKU %in% war))%>%
        dplyr::as_data_frame()
    }
  }else{q=z0}
  return(q)
}

