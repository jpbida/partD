### Reads in Data from stdin on line at a time ###
### Load All Psych Data ###

### Create a list of import psych drugs 

data<-read.table(file="../data/psych.tab",sep="\t",header=TRUE,quote='"',comment.char="")
source("functions.r")

### ASSUMPTION: Setting missing BENE_COUNT data to 5.5 
### Looks like BENE_COUNT is missing. Has a minimum of 11 because aggregate data ####
### setting it to 5.5 for this analysis NEED TO THINK about this
###  >50% of data is missing

data=set_bene_count(data)

### ASSUMPTION: there is no total_day_supply data missing 
### This kicks out a warning and sets the value to zero 
data=set_total_day_supply(data)

### Looks like we could create a linear model to fit missing data ##
### would need lots of dummy variables for the Drug names         ##
jpeg(file="../results/missing_bene_count.jpeg",quality=100)
par(mfrow=c(2,2))
plot(y,x,xlab="BENE_COUNT",ylab="TOTAL_DAY_SUPPLY")
boxplot(split(x,y),col="blue")
boxplot(split(x,y),col="blue",xlim=c(0,25),ylim=c(0,10000))
dev.off()

### Get a list of all Drugs ###
drugs=as.character(unique(data$GENERIC_NAME))

### Get a list of all Specialties ##
specs = as.character(unique(data$SPECIALTY_DESC))

### Get prevelance of drug prescriptions ###

sprev = spec_prev(data,specs)
names(sprev)=c("SPECIALTY_DESC","TOT_SUPPLY_SPEC","TOTAL_BENE_SPEC")
dprev = drug_prev(data,drugs)

prev_all=merge(dprev,sprev,by="SPECIALTY_DESC",all.x=TRUE)

prev_all$PCT_SUPPLY=prev_all$TOTAL_DAY_SUPPLY/prev_all$TOT_SUPPLY_SPEC
prev_all$PCT_BENE=prev_all$BENE_COUNT/prev_all$TOTAL_BENE_SPEC
prev_all=prev_all[order(prev_all$PCT_SUPPLY),]

## Set Min, Median, Max for each drug ###
prev_stats=NULL
for(i in 1:length(drugs)){
prev_stats$GENERIC_NAME[i]=drugs[i]
prev_stats$N[i]=length(prev_all$PCT_SUPPLY[prev_all$GENERIC_NAME==drugs[i]])
prev_stats$MEDIAN_SUPPLY[i]=median(prev_all$PCT_SUPPLY[prev_all$GENERIC_NAME==drugs[i]])
prev_stats$MEAN_SUPPLY[i]=mean(prev_all$PCT_SUPPLY[prev_all$GENERIC_NAME==drugs[i]])
prev_stats$MIN_SUPPLY[i]=min(prev_all$PCT_SUPPLY[prev_all$GENERIC_NAME==drugs[i]])
prev_stats$MAX_SUPPLY[i]=max(prev_all$PCT_SUPPLY[prev_all$GENERIC_NAME==drugs[i]])
prev_stats$SD_SUPPLY[i]=(sd(prev_all$PCT_SUPPLY[prev_all$GENERIC_NAME==drugs[i]]))
}
prev_all=merge(prev_all,prev_stats,all.x=TRUE,by="GENERIC_NAME")

drugs=prev_all$GENERIC_NAME[order(prev_all$PCT_SUPPLY,decreasing=TRUE)]
drugs=as.data.frame(drugs)

## Outputs a table of drugs order by total usage by Psych ###
write.table(drugs,file="drugs.tab",col.names=FALSE,row.names=FALSE)


