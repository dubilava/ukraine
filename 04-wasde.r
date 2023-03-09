library(data.table)
library(ggplot2)
library(ggthemes)
library(ggdark)
library(viridis)
library(Quandl)

# grain ----

Quandl.api_key('wKafyzjBGqK5r2mxYTJy')

dt <- data.table(Quandl.datatable('WASDE/DATA', code='WHEAT_WORLD_18', report_month='2022-05'))

sub_dt <- dt[region=="World 3/" & item%in%c("Production","Exports") & grepl("Est.",year,fixed=TRUE)]

sub_dt


dt <- data.table(Quandl.datatable('WASDE/DATA', code='WHEAT_WORLD_19', report_month='2022-07'))

sub_dt <- dt[region=="World 3/" & item%in%c("Production","Exports")]

sub_dt


change_dt <- sub_dt[,.(change=round(100*(value/shift(value)-1),2)),by=.(item)]

change_dt <- change_dt[complete.cases(change_dt)]

change_dt

ggplot(sub_dt,aes(x=value))+
  geom_dotplot(binwidth=20,fill="coral")+
  dark_theme_classic()





