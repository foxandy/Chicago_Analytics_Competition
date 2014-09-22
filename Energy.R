#The following code identifies out-of-control census blocks in the City of Chicago by their energy usage. The method calculates 
#z-scores by month and building type. The code also includes various ways to illustrate these trends.
#source data can be found at the City of Chicago Open Data Portal - Energy Usage 2010

energy<-read.csv("energy.csv",header=T,na.strings=c("","*","NA"))
energy_descriptors<-energy[,1:4]
energy_variables<-energy[,c(18:19,33:35,64:73)]
library(ggplot)

#move monthly data from columns to rows
for(i in 1:12){
  kwh<-energy[,4 + i]
  therms<-energy[,19+i]
  if(i<10) month=paste("2010_0",i,sep="")
  else month=paste("2010_",i,sep="")
  energy_temp<-cbind(energy_descriptors,month,kwh,therms,energy_variables)
  if(i==1) energy_cleaned<-energy_temp
  else energy_cleaned<-rbind(energy_cleaned,energy_temp)
}

#remove empty records
energy_cleaned$CENSUS.BLOCK[is.na(energy_cleaned$CENSUS.BLOCK)]<-"Other"
energy_cleaned<-energy_cleaned[!is.na(energy_cleaned$kwh),]
energy_cleaned<-energy_cleaned[!is.na(energy_cleaned$therms),]
energy_cleaned<-energy_cleaned[!is.na(energy_cleaned$KWH.TOTAL.SQFT),]
energy_cleaned<-energy_cleaned[!is.na(energy_cleaned$THERMS.TOTAL.SQFT),]

energy_cleaned$kwh<-as.numeric(energy_cleaned$kwh)
energy_cleaned$therms<-as.numeric(energy_cleaned$therms)
sqldf('SELECT BUILDING_TYPE, SUM(kwh), SUM(therms)
      FROM energy_cleaned GROUP BY BUILDING_TYPE')
energy_res<-energy_cleaned[which(energy_cleaned$BUILDING.TYPE=="Residential"),]
remove(kwh)
remove(therms)
remove(month)

#transform energy usage to kwh and therms per square foot
energy_res$kwh_per_sqft<-energy_res$kwh/energy_res$KWH.TOTAL.SQFT
energy_res$therms_per_sqft<-energy_res$therms/energy_res$THERMS.TOTAL.SQFT

#calculate mean and SD by building subtype and month over census blocks
means <- aggregate(cbind(energy_res$kwh_per_sqft,energy_res$therms_per_sqft) ~ month + BUILDING_SUBTYPE, data = energy_res, mean)
sds <- aggregate(cbind(energy_res$kwh_per_sqft,energy_res$therms_per_sqft) ~ month + BUILDING_SUBTYPE, data = energy_res, sd)
stats <-cbind(means,sds[,3],sds[,4])
colnames(stats)[3]="kwh_mean"
colnames(stats)[4]="therms_mean"
colnames(stats)[5]="kwh_sd"
colnames(stats)[6]="therms_sd"
library(lattice)
xyplot(stats$kwh_mean~stats$month,type=c('l','p'),groups=stats$BUILDING_SUBTYPE,data=stats,auto.key=T)
xyplot(stats$therms_mean~stats$month,type=c('l','p'),groups=stats$BUILDING_SUBTYPE,data=stats,auto.key=T)

#join raw data set with mean and SD
library(sqldf)
energy_join = sqldf('SELECT * 
             FROM energy_res as A, stats as B
             WHERE A.month = B.month AND A.BUILDING_SUBTYPE = B.BUILDING_SUBTYPE')

#some descriptive histograms
hist(energy_join$kwh_per_sqft,breaks=1000,xlim=c(0,2),
     main = "Chicago Electricity Usage per Square Foot",
     ylab="# of Census Blocks",xlab="KWH per Square Foot",cex.lab=1.2)
hist(log(energy_join$kwh_per_sqft+1),breaks=100,xlim=c(0,2))
hist(energy_join$therms_per_sqft,breaks=1000,xlim=c(0,0.5),
     main = "Chicago Natural Gas Usage per Square Foot",
     ylab="# of Census Blocks",xlab="Therms per Square Foot",cex.lab=1.2)
bwplot(~energy_join$kwh_per_sqft|energy_join$BUILDING_SUBTYPE)

#calculate the z scores for each census block's energy usage compared to peers
energy_join$z_kwh<-(energy_join$kwh_per_sqft - energy_join$kwh_mean)/energy_join$kwh_sd
energy_join$z_therms<-(energy_join$therms_per_sqft - energy_join$therms_mean)/energy_join$therms_sd
energy_res<-energy_join[,-c(25:30)]

hist(energy_res$z_kwh, xlim=c(-5,5), breaks = 100)
hist(energy_res$z_therms, xlim=c(-5,5), breaks=100)

kwh_outliers <- energy_res[which(energy_res$kwh_per_sqft > 10),]

###ALBANY PARK EXAMPLE
albany<-energy_res[which(energy_res$COMMUNITY_AREA_NAME=="Albany Park"),]
#albany<-albany[which(albany$z_kwh>2),]

xyplot(z_kwh~month,type=c('l','p'),groups=CENSUS_BLOCK,data=albany)

plot(energy_res$kwh_per_sqft,energy_res$kwh)

test<-sqldf('SELECT CENSUS_BLOCK, BUILDING_SUBTYPE, MIN(z_kwh) AS min_kwh, MAX(z_kwh) AS max_kwh
             FROM albany
             GROUP BY CENSUS_BLOCK, BUILDING_SUBTYPE')
test$range<-test$max_kwh - test$min_kwh
census<-sqldf('SELECT DISTINCT CENSUS_BLOCK FROM test WHERE range>3')

#tool to scroll through census blocks
custom.args=list()
for(i in 1:nrow(census)){
  custom.args<-append(custom.args,list(census[i,1]))
}
library(manipulate)
manipulate(
  xyplot(z_kwh~month,type=c('l','p'),groups=BUILDING_SUBTYPE,
         data=albany[which(albany$CENSUS_BLOCK==block),],
         ylim=c(-5,5),ylab="KWH Z-Score",xlab="Month",
         cex=1.5,pch=16,
         main=paste("Electricity Usage - Census Block",block),
         auto.key=list(columns=3)),
  block=picker(custom.args))

### ENERGY USE RANGE - ALL CHICAGO
ranges<-sqldf('SELECT CENSUS_BLOCK, BUILDING_SUBTYPE, COMMUNITY_AREA_NAME, 
               MIN(z_kwh) AS min_kwh, MAX(z_kwh) AS max_kwh,
               MIN(z_therms) AS min_therm, MAX(z_therms) AS max_therm,
               AVERAGE_STORIES, AVERAGE_BUILDING_AGE, ELECTRICITY_ACCOUNTS,
               GAS_ACCOUNTS, KWH_TOTAL_SQFT, THERMS_TOTAL_SQFT
             FROM energy_res
            GROUP BY CENSUS_BLOCK, BUILDING_SUBTYPE, COMMUNITY_AREA_NAME,
                     AVERAGE_STORIES, AVERAGE_BUILDING_AGE, ELECTRICITY_ACCOUNTS,
                     GAS_ACCOUNTS, KWH_TOTAL_SQFT, THERMS_TOTAL_SQFT')
ranges$kwh_range<-ranges$max_kwh - ranges$min_kwh
ranges$therm_range<-ranges$max_therm - ranges$min_therm
censusblocks<-sqldf('SELECT COMMUNITY_AREA_NAME, COUNT(DISTINCT CENSUS_BLOCK)
                    FROM energy_res
                    GROUP BY COMMUNITY_AREA_NAME')
ranges$kwh_range[which(ranges$kwh_range > 5)] <- 5
ranges$therm_range[which(ranges$therm_range > 5)] <- 5

#CHARTS

#HIGH-LEVEL
ranges$kwh_spike[which(ranges$kwh_range < 3)] <- 0
ranges$kwh_spike[which(ranges$kwh_range >= 3)] <- 1
ranges$therm_spike[which(ranges$therm_range < 3)] <- 0
ranges$therm_spike[which(ranges$therm_range >= 3)] <- 1
ranges$both_spike<-0
ranges$both_spike[which(ranges$kwh_spike == 1 & ranges$therm_spike == 1)] <- 1
ranges$kwh_spike[which(ranges$both_spike == 1)] <- 0
ranges$therm_spike[which(ranges$both_spike == 1)] <- 0

ranges_chi<-sqldf('SELECT COMMUNITY_AREA_NAME, COUNT(*) AS census_blocks,
                  SUM(kwh_spike) AS kwh_spikes, SUM(therm_spike) AS therm_spikes, SUM(both_spike) AS both_spikes
                  FROM ranges
                  GROUP BY COMMUNITY_AREA_NAME')

ranges_chi$kwh_proportion<-ranges_chi$kwh_spikes / ranges_chi$census_blocks
ranges_chi$therm_proportion<-ranges_chi$therm_spikes / ranges_chi$census_blocks
ranges_chi$both_proportion<-ranges_chi$both_spikes / ranges_chi$census_blocks
ranges_chi$dummy <- ranges_chi$kwh_proportion+ranges_chi$therm_proportion+ranges_chi$both_proportion
ranges_chi$total <- ranges_chi$kwh_spikes + ranges_chi$therm_spikes + ranges_chi$both_spikes

temp_kwh<-ranges_chi[c(1,6,9)]
temp_therm<-ranges_chi[c(1,7,9)]
temp_both<-ranges_chi[c(1,8,9)]

colnames(temp_kwh)[2]="proportion"
temp_kwh$type = 1
colnames(temp_therm)[2]="proportion"
temp_therm$type = 0
colnames(temp_both)[2]="proportion"
temp_both$type = 2
range_graph2<-rbind(temp_kwh,temp_therm,temp_both)
range_graph2<-range_graph2[order(-range_graph2$dummy),]ggplot(range_graph2, aes(x 
test<-range_graph2
range_graph2$type<-factor(range_graph2$type, labels = c("natural gas","electricity", "both"))
ggplot(range_graph2, aes(x = COMMUNITY_AREA_NAME, y = proportion, fill=type, order=type)) + geom_bar(aes(x = reorder(COMMUNITY_AREA_NAME, dummy)),stat="identity") + coord_flip() +
  ggtitle("Energy Variability Profile - All Chicago")+
  theme(axis.text.y = element_blank(), title = element_text(size=16),legend.title = element_blank(), legend.position="bottom", legend.text = element_text(size = 14)) +
  xlab("Community Area") + ylab("Proportion of Census Blocks") + 
  scale_fill_brewer(palette="Set2")
group1<-range_graph2[which(range_graph2$dummy<0.05),]
group2<-range_graph2[which(range_graph2$dummy>=0.05 & range_graph2$dummy<0.1),]
group3<-range_graph2[which(range_graph2$dummy>=0.1),]
#the good
ggplot(group1, aes(x = COMMUNITY_AREA_NAME, y = proportion, fill=type, order=type)) + geom_bar(aes(x = reorder(COMMUNITY_AREA_NAME, dummy)),stat="identity") + coord_flip() +
  ggtitle("Energy Variability Profile - All Chicago")+
  theme(title = element_text(size=16),legend.title = element_blank(), legend.position="bottom", legend.text = element_text(size = 14)) +
  xlab("Community Area") + ylab("Proportion of Census Blocks") + 
  scale_fill_brewer(palette="Set2")
#the bad
ggplot(group2, aes(x = COMMUNITY_AREA_NAME, y = proportion, fill=type, order=type)) + geom_bar(aes(x = reorder(COMMUNITY_AREA_NAME, dummy)),stat="identity") + coord_flip() +
  ggtitle("Energy Variability Profile - All Chicago")+
  theme(title = element_text(size=16),legend.title = element_blank(), legend.position="bottom", legend.text = element_text(size = 14)) +
  xlab("Community Area") + ylab("Proportion of Census Blocks") + 
  scale_fill_brewer(palette="Set2")
#the ugly
ggplot(group3, aes(x = COMMUNITY_AREA_NAME, y = proportion, fill=type, order=type)) + geom_bar(aes(x = reorder(COMMUNITY_AREA_NAME, dummy)),stat="identity") + coord_flip() +
  ggtitle("Energy Variability Profiles - High Variation")+
  theme(title = element_text(size=16),legend.title = element_blank(), legend.position="bottom", legend.text = element_text(size = 14)) +
  xlab("Community Area") + ylab("Proportion of Census Blocks Out-Of-Control'") + 
  scale_fill_brewer(palette="Set2")

#MID-LEVEL
library(ggplot2)
temp_kwh<-ranges[c(1:5,8)]
temp_therm<-ranges[c(1:3,6:7,9)]
colnames(temp_kwh)[4]="min"
colnames(temp_kwh)[5]="max"
colnames(temp_kwh)[6]="range"
temp_kwh$type = "electricity"
colnames(temp_therm)[4]="min"
colnames(temp_therm)[5]="max"
colnames(temp_therm)[6]="range"
temp_therm$type = "natural gas"
range_graph<-rbind(temp_kwh,temp_therm)

library(manipulate)
custom.args=list()
for(i in 1:nrow(censusblocks)){
  custom.args<-append(custom.args,list(censusblocks[i,1]))
}
library(RColorBrewer)
manipulate(
  ggplot(subset(range_graph, COMMUNITY_AREA_NAME == neighborhood), aes(x=range, fill = type)) + geom_density(alpha = 0.7) +
    ggtitle(paste("Energy Variability Profile - ",neighborhood))
    + theme(axis.text.y = element_blank(), axis.text.x = element_blank(),
            title = element_text(size=16),panel.background = element_rect(fill = 'white', colour = 'white'),
            legend.title = element_blank(), legend.position="bottom", legend.text = element_text(size = 14))
    + ylab("Proportion of Census Blocks")
    + xlab("Range of Fluctuation")
    + scale_fill_brewer(palette="Dark2")
    + geom_vline(aes(xintercept=3),linetype="dashed", colour = "black", size=1),
  neighborhood = picker(custom.args))

#EXAMPLE FOR DECK
block170<-subset(energy_res,CENSUS_BLOCK==170311404005005 & BUILDING_SUBTYPE=="Multi < 7")
block170<-block170[,c(2,5,23:24)]
write.csv(block170,"graph.csv")
write.csv(stats,"graph2.csv")

temp_kwh<-ranges_chi[c(1,3,10)]
temp_therm<-ranges_chi[c(1,4,10)]
temp_both<-ranges_chi[c(1,5,10)]

colnames(temp_kwh)[2]="proportion"
temp_kwh$type = 1
colnames(temp_therm)[2]="proportion"
temp_therm$type = 0
colnames(temp_both)[2]="proportion"
temp_both$type = 2
range_graph2<-rbind(temp_kwh,temp_therm,temp_both)
range_graph2<-range_graph2[order(-range_graph2$total),]
ggplot(range_graph2, aes(x = COMMUNITY_AREA_NAME, y = proportion, fill=type, order=type)) + geom_bar(aes(x = reorder(COMMUNITY_AREA_NAME, total)),stat="identity") + coord_flip() +
    ggtitle("Energy Variability Profile - All Chicago")+
    theme(title = element_text(size=16),legend.title = element_blank(), legend.position="bottom", legend.text = element_text(size = 14)) +
    xlab("Community Area") + ylab("Number of Census Blocks") + 
    scale_fill_brewer(palette="Set2")
sqldf('SELECT COUNT(DISTINCT COMMUNITY_AREA_NAME) FROM group3')
group3<-range_graph2[which(range_graph2$total>=62),]
ggplot(group3, aes(x = COMMUNITY_AREA_NAME, y = proportion, fill=type, order=type)) + geom_bar(aes(x = reorder(COMMUNITY_AREA_NAME, total)),stat="identity") + coord_flip() +
  ggtitle("Energy Variability Profile - High Variability")+
  theme(axis.text = element_text(size = 14),title = element_text(size=16),legend.title = element_blank(),   axis.title.y = element_blank(), legend.position="bottom", legend.text = element_text(size = 14)) +
  ylab("Number of Census Blocks 'Out of Control'") + 
  scale_fill_brewer(palette="Set2")
