library(dplyr)
q<-readxl::read_xlsx("Z:/60 - Marketing/shpiruk/EW_UI/231.xlsx")
cat<-readxl::read_xlsx("Z:/60 - Marketing/shpiruk/EW_UI/cat.xlsx")

cat<-cat%>%
  magrittr::set_colnames(c("SKU","category"))
q<-q[,1:6]
q$gsd<-0
q$Дата<-substr(q$Дата,0,10)


alr=q%>%group_by(article)%>%
  summarise(count=n_distinct(`Дата`))%>%
  filter(count<300)%>%
  pull(article)
if(length(alr)>0) warning(paste("WARNING:",length(alr),"SKU has short history"))


library(microbenchmark)
library(zx48c)
run_forecast(q,cat)


microbenchmark::microbenchmark(x1=run_forecast(q,cat),
                               x2=run_forecast(q,cat,hist=T),
                               times=20)


q<-readxl::read_xlsx("Z:/60 - Marketing/shpiruk/EW_UI/mti.xlsx")
cat<-readxl::read_xlsx("Z:/60 - Marketing/shpiruk/EW_UI/cat_mti.xlsx")


q%>%
  group_by(article)%>%
  summarise(count=n())
