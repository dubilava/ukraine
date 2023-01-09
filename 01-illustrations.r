library(data.table)
library(ggplot2)
library(ggrepel)

# wheat futures ----

wheat_dt <- fread("data/wheat_futures.csv")
wheat_dt[,`:=`(Date=as.Date(Date,format="%m/%d/%Y"))]
colnames(wheat_dt) <- c("Date","Close","Volume","Open","High","Low")

wheat_dt[,`:=`(Event=as.character(NA))]

wheat_dt[Date=="2022-02-24"]$Event <- "Invasion"
wheat_dt[Date=="2022-07-22"]$Event <- "Black Sea Grain Initiative"
wheat_dt[Date=="2022-11-17"]$Event <- "The Initiative Renewal"

wheat_dt[,`:=`(Event_Date=Date,Timeline=ifelse(!is.na(Event),600,as.numeric(NA)))]

wheat_dt[Event_Date=="2022-07-22"]$Timeline <- 550

wheat_dt[is.na(Event)]$Event_Date <- NA

gg_wheat <- ggplot(wheat_dt[Date>="2022-01-01" & Date<="2022-12-31"],aes(x=Date,y=Close))+
  geom_line(linewidth=.8,color="coral")+
  geom_segment(aes(x=Event_Date,xend=Event_Date,y=Timeline,yend=0),linewidth=0.8,col="black",na.rm=T)+
  geom_point(aes(x=Event_Date,y=Timeline),shape=21,size=3,stroke=1,col="black",fill="darkgray",na.rm=T)+
  geom_text(aes(x=Event_Date,y=Timeline,label=Event),nudge_y=35,na.rm=T)+
  geom_text(aes(x=Event_Date,y=Timeline,label=Date),size=3,nudge_x=30,na.rm=T)+
  coord_cartesian(ylim=c(500,1300),xlim=c(as.Date("2022-01-01"),as.Date("2023-01-15")))+
  labs(y="Wheat Futures Price (cents/bu)")+
  theme_classic()+
  theme(axis.title = element_text(size=14),axis.text = element_text(size=12))

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

gg_prod <- ggplot(crop_lg, aes(x=year, y=value, fill=variable,group=variable)) + 
  geom_area(color="white",size=.4)+
  geom_hline(yintercept = seq(min_inc,max_val,min_inc),color="white",linewidth=.3,linetype=3)+
  scale_x_discrete(breaks=seq(2000,2020,5))+
  scale_fill_manual(values=c("darkgray",rep("gray40",2),rep("darkgray",5),"coral","indianred"))+
  labs(x="Year",y=paste0(outp," (",unit,")"))+#,caption="Data from USDA/FAS PSD Online\nhttps://apps.fas.usda.gov/psdonline/app/index.html")+
  coord_cartesian(ylim=c(0,840))+
  theme_classic()+
  theme(legend.position = "right",legend.title=element_blank(),plot.caption = element_text(color="darkgray"),axis.title = element_text(size=14),axis.text = element_text(size=12),legend.text = element_text(size=12),legend.key.size = unit(0.8,"cm"))

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

gg_expr <- ggplot(crop_lg, aes(x=year, y=value, fill=variable,group=variable)) + 
  geom_area(color="white",size=.4)+
  geom_hline(yintercept = seq(min_inc,max_val,min_inc),color="white",linewidth=.3,linetype=3)+
  scale_x_discrete(breaks=seq(2000,2020,5))+
  scale_fill_manual(values=c("darkgray",rep("gray40",2),rep("darkgray",5),"coral","indianred"))+
  labs(x="Year",y=paste0(outp," (",unit,")"))+#,caption="Data from USDA/FAS PSD Online\nhttps://apps.fas.usda.gov/psdonline/app/index.html")+
  coord_cartesian(ylim=c(0,210))+
  theme_classic()+
  theme(legend.position = "right",legend.title=element_blank(),plot.caption = element_text(color="darkgray"),axis.title = element_text(size=14),axis.text = element_text(size=12),legend.text = element_text(size=12),legend.key.size = unit(0.8,"cm"))

ggsave("figures/exports.png",gg_expr,width=6.5,height=4.25,dpi="retina",device="png")
ggsave("figures/exports.eps",gg_expr,width=6.5,height=4.25,dpi="retina",device="eps")




# # load conflict data (acled)
# 
# load("acled_global_recent.RData")


