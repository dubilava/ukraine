library(data.table)
library(ggplot2)
library(viridis)

# wheat ----

grain_dt <- fread("data/IGC_rice.csv")
grain_dt[,`:=`(Date=as.Date(Date,format="%d/%m/%Y"))]


grain_dt[,`:=`(Event=as.character(NA))]

grain_dt[Date=="2022-02-24"]$Event <- "Invasion"
grain_dt[Date=="2022-05-12"]$Event <- "The Solidarity Lanes"
grain_dt[Date=="2022-07-22"]$Event <- "Black Sea Grain Initiative"
grain_dt[Date=="2022-11-17"]$Event <- "The Initiative Renewal"

grain_dt[,`:=`(Event_Date=Date,Timeline=ifelse(!is.na(Event),600,as.numeric(NA)))]

grain_dt[Event_Date=="2022-02-24"]$Timeline <- 550
grain_dt[Event_Date=="2022-05-12"]$Timeline <- 650
grain_dt[Event_Date=="2022-07-22"]$Timeline <- 550

grain_dt[is.na(Event)]$Event_Date <- NA

grain_lg <- melt(grain_dt,id.vars = c("Date","Event","Event_Date","Timeline"))

grain_sub <- grain_lg#[Date>="2022-01-01" & Date<="2022-12-31"]

gg_grain <- ggplot(grain_sub,aes(x=Date,y=value,color=variable,linetype=variable))+
  geom_line(linewidth=.8)+
  # scale_color_viridis_d()+
  # geom_segment(aes(x=Event_Date,xend=Event_Date,y=Timeline,yend=0),linewidth=0.8,col="black",na.rm=T)+
  # geom_point(aes(x=Event_Date,y=Timeline),shape=21,size=3,stroke=1,col="black",fill="darkgray",na.rm=T)+
  # geom_text(aes(x=Event_Date,y=Timeline,label=Event),nudge_y=35,na.rm=T)+
  # geom_text(aes(x=Event_Date,y=Timeline,label=Date),size=3,nudge_x=30,na.rm=T)+
  # coord_cartesian(ylim=c(0,600),xlim=c(as.Date("2022-01-01"),as.Date("2023-01-15")))+
  labs(y="Wheat Price (USD/mt)")+
  theme_classic()+
  theme(axis.title = element_text(size=16),axis.text = element_text(size=14),legend.title=element_blank())

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
  geom_area(color="white",size=.4)+
  geom_hline(yintercept = seq(min_inc,max_val,min_inc),color="white",linewidth=.3,linetype=3)+
  scale_x_discrete(breaks=seq(2000,2020,5))+
  scale_fill_manual(values=c("darkgray",rep("dimgray",2),rep("darkgray",5),"coral","indianred"))+
  labs(x="Year",y=paste0(outp," (",unit,")"))+#,caption="Data from USDA/FAS PSD Online\nhttps://apps.fas.usda.gov/psdonline/app/index.html")+
  coord_cartesian(ylim=c(0,840))+
  theme_classic()+
  theme(legend.position = "right",legend.title=element_blank(),plot.caption = element_text(color="darkgray"),axis.title = element_text(size=16),axis.text = element_text(size=14),legend.text = element_text(size=14),legend.key.size = unit(0.8,"cm"))

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
  geom_area(color="white",size=.4)+
  geom_hline(yintercept = seq(min_inc,max_val,min_inc),color="white",linewidth=.3,linetype=3)+
  scale_x_discrete(breaks=seq(2000,2020,5))+
  scale_fill_manual(values=c("darkgray",rep("dimgray",2),rep("darkgray",5),"coral","indianred"))+
  labs(x="Year",y=paste0(outp," (",unit,")"))+#,caption="Data from USDA/FAS PSD Online\nhttps://apps.fas.usda.gov/psdonline/app/index.html")+
  coord_cartesian(ylim=c(0,210))+
  theme_classic()+
  theme(legend.position = "right",legend.title=element_blank(),plot.caption = element_text(color="darkgray"),axis.title = element_text(size=16),axis.text = element_text(size=14),legend.text = element_text(size=14),legend.key.size = unit(0.8,"cm"))

ggsave("figures/exports.png",gg_expr,width=6.5,height=4.25,dpi="retina",device="png")
ggsave("figures/exports.eps",gg_expr,width=6.5,height=4.25,dpi="retina",device="eps")


prodexp_sub <- merge(production_sub,exports_sub,by=c("Country","Year"))

prodexp_sub$Country <- factor(prodexp_sub$Country,levels=list_shrt)


# ACLED ----

load("data/acled_global_recent.RData")

africa_dt <- acled_dt[region%in%c("Middle Africa","Eastern Africa","Southern Africa","Northern Africa","Western Africa")]#,"Middle East","Caucasus and Central Asia")]

africa_dt[admin1%in%c("Sool","Woqooyi Galbeed","Togdheer","Sanaag","Awdal")]$country <- "Somaliland"

africa_dt[,`:=`(month=as.Date(paste0(substr(event_date,1,7),"-01")))]

series_dt <- africa_dt[,.(events=.N),by=.(country,year,month)]
series_dt <- series_dt[order(country,year,month)]

average_dt <- series_dt[year %in% c(2020,2021)]
average_dt[,`:=`(year=2021,month=as.Date(paste0("2021",substr(month,5,10))))]

average_dt <- average_dt[,.(events=mean(events)),by=.(country,year,month)]

series_dt <- rbind(average_dt,series_dt[year==2022])

series_dt[country=="Central African Republic"]$country <- "CAR"
series_dt[country=="Democratic Republic of Congo"]$country <- "DRC"
series_dt[country=="Saint Helena, Ascension and Tristan da Cunha"]$country <- "St. Helena"

series_dt[,`:=`(events_lag=shift(events,12)),by=.(country)]
series_dt[,`:=`(events_ch=events-events_lag)]

check_outlier <- function(v, coef=1.5){
  quantiles <- quantile(v,probs=c(0.25,0.75),na.rm=T)
  IQR <- quantiles[2]-quantiles[1]
  res <- v < (quantiles[1]-coef*IQR) | v > (quantiles[2]+coef*IQR)
  return(res)
}

#apply this to our data
series_dt[,outlier:=check_outlier(events_ch),by=month]
series_dt[,label:=ifelse(outlier & abs(events_ch)>100,country,"")]

gg_bp <- ggplot(series_dt[year==2022],aes(x=month,y=events_ch,group=month))+
  geom_boxplot(na.rm=T,color="coral",size=.5) +
  geom_text_repel(aes(label=label),na.rm=T,size=3,max.overlaps=15) +
  labs(x="Month",y="Change from 2020-2021 average")+
  theme_classic()+
  theme(axis.title = element_text(size=16),axis.text = element_text(size=14))


ggsave("figures/boxplots.png",gg_bp,width=6.5,height=4.25,dpi="retina",device="png")
ggsave("figures/boxplots.eps",gg_bp,width=6.5,height=4.25,dpi="retina",device="eps")



aggregate_dt <- africa_dt[,.(events=.N),by=.(longitude,latitude,country,year)]

conflict_sub <- aggregate_dt[year>=2018]

save(wheat_sub,prodexp_sub,conflict_sub,file="ukraine.Rdata")

prepandemic_dt <- aggregate_dt[year%in%c(2015:2019),.(events=mean(events)),by=.(country)]

pandemic_dt <- aggregate_dt[year%in%c(2020:2021),.(events=mean(events)),by=.(country)]

war_dt <- aggregate_dt[year%in%c(2022),.(events=mean(events)),by=.(country)]

combined_dt <- Reduce(function(...) merge(...,by=c("country"),all=T),list(prepandemic_dt,pandemic_dt,war_dt))

colnames(combined_dt)[2:4] <- c("pre","pan","war")

combined_dt[is.na(combined_dt)] <- 0




## load the map of africa
africa <- ne_countries(scale="large",continent=c("africa"),returnclass="sf")
africa <- st_set_crs(africa, "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")

combined_dt$name_long <- combined_dt$country

combined_dt[country=="Democratic Republic of Congo"]$name_long <- "Democratic Republic of the Congo"

combined_dt[country=="Republic of Congo"]$name_long <- "Republic of the Congo"

combined_dt[country=="Ivory Coast"]$name_long <- "Côte d'Ivoire"

combined_dt[,`:=`(war_ch=(war-pan),war_pc=100*(war-pan)/pan)]

africaplus <- merge(africa,combined_dt,by="name_long",all.x=T)



africa2022_dt <- africa_dt[year==2022,.(events=.N),by=.(longitude,latitude)]

africa2022_dt[,`:=`(longitude=round(as.numeric(longitude),1),latitude=round(as.numeric(latitude),1))]

africa2022_dt <- africa2022_dt[,.(events=sum(events)),by=.(longitude,latitude)]



gg_ch <- ggplot(data = africaplus) +
  geom_sf(aes(fill=war_ch),color="dimgray",size=.2)+
  geom_point(data=africa2022_dt,aes(x=longitude,y=latitude,size=events,alpha=events),color="indianred")+
  coord_sf(xlim=c(-15,55),ylim=c(-35,37))+
  scale_fill_gradient2(low="powderblue",high="coral",midpoint=0,limits=c(-1020,1020),oob=squish)+
  labs(title="Change from 2020-2021 average")+
  theme_void()+
  theme(axis.line.x=element_blank(),axis.line.y=element_blank(),axis.title = element_blank(),axis.text = element_blank(),plot.title = element_text(hjust=.5,size=16,colour="gray35",face="bold"),legend.title = element_blank(),legend.text = element_text(hjust=1,size=14),legend.position = c(.87,.83),legend.key.height=unit(.75,'cm'),legend.key.width=unit(.5,'cm'),legend.direction = "vertical", legend.box = "horizontal")

# gg_pc <- ggplot(data = africaplus) +
#   geom_sf(aes(fill=war_pc),color="dimgray",size=.2)+
#   geom_point(data=africa2022_dt,aes(x=longitude,y=latitude,size=events,alpha=events),color="indianred")+
#   coord_sf(xlim=c(-15,51),ylim=c(-35,37))+
#   scale_fill_gradient2(low="powderblue",high="coral",midpoint=0,limits=c(-120,120),oob=squish)+
#   labs(title="Number and percent change in conflict incidents")+
#   theme_void()+
#   theme(axis.line.x=element_blank(),axis.line.y=element_blank(),axis.title = element_blank(),axis.text = element_blank(),plot.title = element_text(hjust=.5,size=16,colour="gray35",face="bold"),legend.title = element_blank(),legend.text = element_text(hjust=1,size=14),legend.position = c(.87,.83),legend.key.height=unit(.75,'cm'),legend.key.width=unit(.5,'cm'),legend.direction = "vertical", legend.box = "horizontal")


ggsave("figures/conflict_ch.png",gg_ch,width=6.5,height=6.5,dpi="retina",device="png")
ggsave("figures/conflict_ch.eps",gg_ch,width=6.5,height=6.5,dpi="retina",device="eps")



# # bsgi shipments ----
# 
# bsgi_dt <- fread("data/BSGI_shipments.csv")
# 
# unique(bsgi_dt$Commodity)
# 
# bsgi_dt <- bsgi_dt[Commodity=="Wheat"]
# 
# unique(bsgi_dt$Country)
# 
# shipments_total <- sum(bsgi_dt[,.(`Metric tons`)])
# 
# 
# bsgi_dt <- bsgi_dt[`UN region`=="Africa"]
# 
# bsgi_dt[,`:=`(Mo=month(as.Date(`Departure date`,format="%d-%b-%y")))]
# 
# bsgi_sum <- bsgi_dt[,.(Shipment=sum(`Metric tons`)),by=.(Country,Mo)]
# 
# bsgi_sum$Month <- month.name[bsgi_sum$Mo]
# bsgi_sum$Month <- format(bsgi_sum$Month,levels=unique(bsgi_sum$Month))
# 
# bsgi_sum <- bsgi_sum[order(Mo,Country)]
# 
# gg_ship <- ggplot(bsgi_sum, aes(x=Mo, y=Shipment, fill=Country,group=Country)) + 
#   geom_area(color="white",linewidth=.4)+
#   geom_hline(yintercept = seq(min_inc,max_val,min_inc),color="white",linewidth=.3,linetype=3)+
#   scale_x_discrete(breaks=seq(2000,2020,5))+
#   scale_fill_manual(values=c("darkgray",rep("gray40",2),rep("darkgray",5),"coral","indianred"))+
#   labs(x="Year",y=paste0(outp," (",unit,")"))+#,caption="Data from USDA/FAS PSD Online\nhttps://apps.fas.usda.gov/psdonline/app/index.html")+
#   coord_cartesian(ylim=c(0,840))+
#   theme_classic()+
#   theme(legend.position = "right",legend.title=element_blank(),plot.caption = element_text(color="darkgray"),axis.title = element_text(size=14),axis.text = element_text(size=12),legend.text = element_text(size=12),legend.key.size = unit(0.8,"cm"))
# 
# 
# 
# ggplot(bsgi_sum,aes(x=Mo,y=Shipment,fill=Country))+
#   geom_bar(stat = "identity",position = "dodge")
# 
# 
# # local wheat prices ----
# 
# 
# giews_dt <- fread("data/giews_prices.csv")
# giews_dt[,`:=`(Date=as.Date(Date,format="%m/%d/%Y"))]
# giews_dt <- giews_dt[Date>="2022-01-01"]
# colnames(giews_dt) <- c("Date","Mogadishu (Somalia)","Al-Fashir (Sudan)","Dongola (Sudan)","Kadugli (Sudan)","Khartoum (Sudan)")
# 
# giews_lg <- melt(giews_dt,id.vars="Date")
# 
# ggplot(giews_lg,aes(x=Date,y=value,color=variable,linetype=variable))+
#   geom_line(na.rm=T)+
#   coord_cartesian(ylim=c(0,1.5))
# 
# 
# 
# sorg_dt <- fread("data/giews_sorghum.csv")
# sorg_dt[,`:=`(Date=as.Date(Date,format="%m/%d/%Y"))]
# sorg_dt <- sorg_dt[Date>="2022-01-01"]
# colnames(sorg_dt) <- c("Date","Mogadishu (Somalia)","Al-Fashir (Sudan)","Dongola (Sudan)","Kadugli (Sudan)","Khartoum (Sudan)")
# 
# sorg_dt[,c(3:6)] <- sorg_dt[,c(3:6)]/90
# 
# sorg_lg <- melt(sorg_dt,id.vars="Date")
# 
# ggplot(sorg_lg,aes(x=Date,y=value,color=variable,linetype=variable))+
#   geom_line(na.rm=T)+
#   coord_cartesian(ylim=c(0,1.0))
# 
# 
# ## sudan ----
# sdn_dt <- fread("data/wfp_food_prices_sdn.csv")
# 
# sdn_dt <- sdn_dt[,.(date=as.Date(date,format="%Y-%m-%d"),market,latitude,longitude,commodity,unit,pricetype,price,usdprice)]
# 
# unique(sdn_dt$commodity)
# 
# sdn_dt <- sdn_dt[commodity=="Wheat"]
# 
# sdn_dt <- sdn_dt[order(-date)]
# 
# 
# ## egypt ----
# egy_dt <- fread("data/wfp_food_prices_egy.csv")
# 
# egy_dt <- egy_dt[,.(date=as.Date(date,format="%Y-%m-%d"),market,latitude,longitude,commodity,unit,pricetype,price,usdprice)]
# 
# unique(egy_dt$commodity)
# 
# egy_dt <- egy_dt[commodity=="Wheat flour"]
# 
# egy_dt <- egy_dt[order(-date)]
# 
# 
# ## somalia ----
# som_dt <- fread("data/wfp_food_prices_som.csv")
# 
# som_dt <- som_dt[,.(date=as.Date(date,format="%Y-%m-%d"),market,latitude,longitude,commodity,unit,pricetype,price,usdprice)]
# 
# unique(som_dt$commodity)
# 
# som_dt <- som_dt[commodity%in%c("Wheat flour","Wheat flour (imported)")]
# 
# som_dt <- som_dt[order(-date)]








# ggsave("figures/conflict_pc.png",gg_pc,width=6.5,height=6.5,dpi="retina",device="png")
# ggsave("figures/conflict_pc.eps",gg_pc,width=6.5,height=6.5,dpi="retina",device="eps")






# load the data
load("data/coeftab.RData")

# generate the radial plot
gg_coef <- ggplot(coeftab_dt,aes(x=season,y=est))+
  geom_ribbon(aes(ymin=est-1.96*se,ymax=est+1.96*se),fill="coral",alpha=.25)+
  geom_line(color="coral",linewidth=.6)+
  geom_hline(yintercept = seq(-12,10,4),color="dimgray",linewidth=.3,linetype=3) +
  geom_hline(yintercept = 0,color="dimgray",linewidth=.4,linetype=2) +
  scale_x_continuous(breaks = 0:11,labels=c("H(arvest)",paste0("H+",c(1:11))))+
  scale_y_continuous(breaks = seq(-8,10,4))+
  coord_polar(start=-pi*2)+
  labs(x="months after harvest (H)",y="% change in violence")+
  theme_classic()+
  theme(axis.line=element_blank(),plot.title = element_text(hjust=.5,size=16,colour="dimgray",face="bold"),axis.title = element_text(size=16),axis.text = element_text(size=14))

ggsave("figures/radial_violence.png",gg_coef,width=6.5,height=6.5,dpi="retina",device="png")
ggsave("figures/radial_violence.eps",gg_coef,width=6.5,height=6.5,dpi="retina",device=cairo_ps)










