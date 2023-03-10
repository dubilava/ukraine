library(data.table)
library(ggplot2)
library(ggthemes)
library(ggdark)

# grains ----

grain <- "wheat"

grain_dt <- fread(paste0("data/IGC_",grain,".csv"))
grain_dt[,`:=`(Date=as.Date(Date,format="%d/%m/%Y"))]

colnames(grain_dt) <- c("Date","Arg","Aus","BSR","Can","Fra","USH","USS","USD")

grain_lg <- melt(grain_dt,id.vars = c("Date"))

grain_sub <- grain_lg[Date>="2021-01-01" & Date<="2022-12-31"]

gg_grain <- ggplot(grain_sub,aes(x=Date,y=value,color=variable,linetype=variable))+
  geom_line(linewidth=.8)+
  scale_color_brewer(type="qual",palette="Set2")+
  labs(y=paste0("Price of ",grain," (fob, $/mt)"),x="Year",caption="Data from International Grains Council https://www.igc.int/en/default.aspx")+
  dark_theme_classic()+
  theme(legend.position = "right",legend.title=element_blank(),plot.caption = element_text(color="darkgray"),axis.title = element_text(size=16),axis.text = element_text(size=14),legend.text = element_text(size=14),legend.key.size = unit(0.8,"cm"))

gg_grain


ggsave("figures/wheat.png",gg_wheat,width=6.5,height=4.25,dpi="retina",device="png")
ggsave("figures/wheat.eps",gg_wheat,width=6.5,height=4.25,dpi="retina",device="eps")



# wheat production & export ----

dt <- fread("data/psd.csv")

crop <- "Wheat"

list_long <- c("Other","China","India","Australia","Argentina","Canada","USA","EU","Ukraine","Russia")
list_shrt <- substr(c("Other","China","India","Australia","Argentina","Canada","USA","EU","Ukraine","Russia"),1,3)


## production graph
outp <- c("Production")

crop_dt <- dt[Commodity==crop & Attribute%in%outp & Country%in%c("Argentina","Australia","Canada","European Union","Russia","Ukraine","United States","World","China","India")]

crop_dt$Commodity <- NULL
crop_dt$Attribute <- NULL

crop_dt[Country=="European Union"]$Country <- "EU"
crop_dt[Country=="United States"]$Country <- "USA"

crop_dt <- dcast(melt(crop_dt, id.vars = c("Country")), variable ~ Country)

crop_dt[,Other := (World-(Argentina+Australia+Canada+EU+Russia+Ukraine+USA+China+India))]

crop_dt[,year:=as.numeric(as.character(substr(variable,1,4)))]

crop_dt <- crop_dt[year>=2000]
crop_dt$variable <- NULL

crop_lg <- melt(crop_dt[,.(year=factor(year),Argentina=as.numeric(Argentina),Australia=as.numeric(Australia),Canada=as.numeric(Canada),China=as.numeric(China),EU=as.numeric(EU),India=as.numeric(India),Russia=as.numeric(Russia),Ukraine=as.numeric(Ukraine),USA=as.numeric(USA),Other=as.numeric(Other))],id.vars="year")

crop_lg$variable <- factor(crop_lg$variable,levels=list_long,labels=list_shrt)

if(sum(crop_lg[year==2021]$value)>50000){
  crop_lg$value <- crop_lg$value/1000
  unit <- "million mt"
}else{
  unit <- "thousand mt"
}

max_val <- sum(crop_lg[year==2021]$value)
min_inc <- pretty(0:max_val,n=10)[2]

production_sub <- crop_lg
colnames(production_sub) <- c("Year","Country","Production")

gg_prod <- ggplot(crop_lg, aes(x=year, y=value, fill=variable,group=variable)) + 
  geom_area(color="white",linewidth=.4)+
  geom_hline(yintercept = seq(min_inc,max_val,min_inc),color="white",linewidth=.3,linetype=3)+
  scale_x_discrete(breaks=seq(2000,2020,5))+ 
  scale_fill_manual(values=c("darkgray",rep("dimgray",2),rep("darkgray",5),"coral","indianred"))+
  labs(x="Year",y=paste0(outp," (",unit,")"),caption="Data from USDA/FAS PSD Online https://apps.fas.usda.gov/psdonline/app/index.html")+
  coord_cartesian(ylim=c(0,840))+
  dark_theme_classic()+
  theme(legend.position = "right",legend.title=element_blank(),plot.caption = element_text(color="darkgray"),axis.title = element_text(size=16),axis.text = element_text(size=14),legend.text = element_text(size=14),legend.key.size = unit(0.8,"cm"))

gg_prod

ggsave("figures/production.png",gg_prod,width=6.5,height=4.25,dpi="retina",device="png")
ggsave("figures/production.eps",gg_prod,width=6.5,height=4.25,dpi="retina",device="eps")


## production graph
outp <- c("Exports")

crop_dt <- dt[Commodity==crop & Attribute%in%outp & Country%in%c("Argentina","Australia","Canada","European Union","Russia","Ukraine","United States","World","China","India")]

crop_dt$Commodity <- NULL
crop_dt$Attribute <- NULL

crop_dt[Country=="European Union"]$Country <- "EU"
crop_dt[Country=="United States"]$Country <- "USA"

crop_dt <- dcast(melt(crop_dt, id.vars = c("Country")), variable ~ Country)

crop_dt[,Other := (World-(Argentina+Australia+Canada+EU+Russia+Ukraine+USA+China+India))]

crop_dt[,year:=as.numeric(as.character(substr(variable,1,4)))]

crop_dt <- crop_dt[year>=2000]
crop_dt$variable <- NULL

crop_lg <- melt(crop_dt[,.(year=factor(year),Argentina=as.numeric(Argentina),Australia=as.numeric(Australia),Canada=as.numeric(Canada),China=as.numeric(China),EU=as.numeric(EU),India=as.numeric(India),Russia=as.numeric(Russia),Ukraine=as.numeric(Ukraine),USA=as.numeric(USA),Other=as.numeric(Other))],id.vars="year")

crop_lg$variable <- factor(crop_lg$variable,levels=list_long,labels=list_shrt)

if(sum(crop_lg[year==2021]$value)>50000){
  crop_lg$value <- crop_lg$value/1000
  unit <- "million mt"
}else{
  unit <- "thousand mt"
}

max_val <- sum(crop_lg[year==2021]$value)
min_inc <- pretty(0:max_val,n=10)[2]

exports_sub <- crop_lg
colnames(exports_sub) <- c("Year","Country","Exports")

gg_expr <- ggplot(crop_lg, aes(x=year, y=value, fill=variable,group=variable)) + 
  geom_area(color="white",linewidth=.4)+
  geom_hline(yintercept = seq(min_inc,max_val,min_inc),color="white",linewidth=.3,linetype=3)+
  scale_x_discrete(breaks=seq(2000,2020,5))+
  scale_fill_manual(values=c("darkgray",rep("dimgray",2),rep("darkgray",5),"coral","indianred"))+
  labs(x="Year",y=paste0(outp," (",unit,")"))+#,caption="Data from USDA/FAS PSD Online\nhttps://apps.fas.usda.gov/psdonline/app/index.html")+
  coord_cartesian(ylim=c(0,210))+
  dark_theme_classic()+
  theme(legend.position = "right",legend.title=element_blank(),plot.caption = element_text(color="darkgray"),axis.title = element_text(size=16),axis.text = element_text(size=14),legend.text = element_text(size=14),legend.key.size = unit(0.8,"cm"))

gg_expr

ggsave("figures/exports.png",gg_expr,width=6.5,height=4.25,dpi="retina",device="png")
ggsave("figures/exports.eps",gg_expr,width=6.5,height=4.25,dpi="retina",device="eps")


prodexp_sub <- merge(production_sub,exports_sub,by=c("Country","Year"))

prodexp_sub$Country <- factor(prodexp_sub$Country,levels=list_shrt)




