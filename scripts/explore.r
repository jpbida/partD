### THIS WILL NOT RUN: Exploratory code Copy and paste bits and peices  ### 

## Load custom functions 
source("functions.r")

#data<-read.table(file="../data/PARTD_PRESCRIBER_PUF_NPI_DRUG_13.tab",sep="\t",header=TRUE,quote='"',comment.char="")

# Subset of data used to build out workflow 
data<-read.table(file="../data/subset.tab",sep="\t",header=TRUE,quote='"',comment.char="")

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
prev_all$ZSCORE = (prev_all$PCT_SUPPLY-prev_all$MEAN_SUPPLY) / prev_all$SD_SUPPLY 
### ASSUMPTION: Psychiatry & Neurology define Pysch drugs 
psy=prev_all$SPECIALTY_DESC=="Psychiatry & Neurology"
table1=prev_all[psy,]
table1=table1[table1$PCT_SUPPLY>0,]
table1=table1[order(table1$PCT_SUPPLY),]
jpeg(file="../results/pct_day_supply_psy.jpeg",quality=100,height=800,width=1200)
par(mai=c(1,3.5,1,1))
barplot(table1$PCT_SUPPLY,names.arg=table1$GENERIC_NAME,las=1,horiz=TRUE,cex.names=0.5,col="blue",border=FALSE,xlab="Percent of Total Day Supply Presribed by Psychiatry Specialties")
barplot(table1$MEAN_SUPPLY,horiz=TRUE,border=FALSE,col="red",add=TRUE)
legend(0.045,2,c("Median Supply Rate All Spec.","Supply Rate Pyschiatry & Neurology"),col=c("red","blue"),lty=1,lwd=10)
dev.off()


jpeg(file="../results/zscore_day_supply_psy.jpeg",quality=100,height=800,width=1200)
par(mai=c(1,3.5,1,1))
barplot(table1$ZSCORE,names.arg=table1$GENERIC_NAME,las=1,horiz=TRUE,cex.names=0.5,col="blue",border=FALSE,xlab="Zscore of Total Day Supply Presribed by Psychiatry Specialties")
dev.off()

### Compare Bar Total Supply Percentages for Psych Drugs ###
### Create matrix with rows = GENERIC_NAME percentage of total_supply and columns being a given NPI/SPECIALTY
drugs=table1$GENERIC_NAME[order(table1$ZSCORE,decreasing=TRUE)]
npi = data.frame(NPI=unique(data$NPI),group=1:length(unique(data$NPI)))
output=drug_prev_npi_group(data,drugs,npi)

#data$SPECIALTY_DESC
#data$GENERIC_NAME
#data$TOTAL_DAY_SUPPLY
#data$BENE_COUNT
 
### Provider Types 
  # Acute care, Specialty, Teaching, Tertiary 

### Visualize SPECIALTY_DESC v. GENERIC_NAME clustering with heatmap ##

### Look at Dosage and combinations between the two groups ###

### Use clustering to define a treatment ###

### Plot treatment types by geography ###
#
#Start by looking at the frequency counts for provider specialties (“specialty_desc”) and commonly prescribed medications (“generic_name”) by Psychiatrists -- these are generally what we would consider to be psychiatric medications.
map('county',fill=FALSE,col="grey",add=FALSE)
map('county',regions='wyoming,weston',fill=TRUE,col="blue",add=TRUE)



### Counts of unique SPECIALTY_DESC occurances (by NPI) ####
spec_cnts_u=table(data$SPECIALTY_DESC[!duplicated(data$NPI)])
par(mai=c(1,3.5,1,1))
jpeg(file="../results/specialty_count_unique.jpeg",quality=100)
barplot(spec_cnts_u[order(spec_cnts_u)],las=1,horiz=TRUE,cex.names=0.7,col="blue",border=FALSE,xlab="Speciality Counts Unique NPI")
dev.off()

### Total Counts
spec_cnts_a=table(data$SPECIALTY_DESC)
par(mai=c(1,3.5,1,1))
jpeg(file="../results/specialty_count_total.jpeg",quality=100)
barplot(spec_cnts_a[order(spec_cnts_a)],las=1,horiz=TRUE,cex.names=0.7,col="blue",border=FALSE,xlab="Speciality Counts All")
dev.off()

psy_k1=data$SPECIALTY_DESC=="Psychiatry" | data$SPECIALTY_DESC=="Psychiatry & Neurology"
psy_drugs=as.data.frame(table(data$GENERIC_NAME[psy_k1]))
psy_drugs=psy_drugs[psy_drugs$Freq>0,]
psy_drugs$keep=1
names(psy_drugs)=c("GENERIC_NAME","Freq","keep")
### Get Speciality counts for Psydrugs ###
data_pdrugs=merge(data,psy_drugs,all.x=TRUE,by="GENERIC_NAME")
data_pdrugs=data_pdrugs[!is.na(data_pdrugs$keep),]


### All Psy Drugs ###
spec_cnts_psy=table(data_pdrugs$SPECIALTY_DESC)
par(mai=c(1,3.5,1,1))
jpeg(file="../results/specialty_count_total_psy.jpeg",quality=100)
barplot(spec_cnts_psy[order(spec_cnts_psy)],las=1,horiz=TRUE,cex.names=0.7,col="blue",border=FALSE,xlab="Speciality Counts Psy Drugs")
dev.off()

### Unique NPI Psy Drugs ###
spec_cnts_psy_u=table(data_pdrugs$SPECIALTY_DESC[!duplicated(data_pdrugs$NPI)])
par(mai=c(1,3.5,1,1))
jpeg(file="../results/specialty_count_unique_psy.jpeg",quality=100)
barplot(spec_cnts_psy_u[order(spec_cnts_psy_u)],las=1,horiz=TRUE,cex.names=0.7,col="blue",border=FALSE,xlab="Speciality Counts Psy Drugs Unique NPI")
dev.off()

### Combine the bar plots ###
## Make sure its the same name ordering ###

u_all=as.data.frame(spec_cnts_u)
names(u_all)=c("DRUG","ALL")
u_psy=as.data.frame(spec_cnts_psy_u)
names(u_psy)=c("DRUG","PSY")
u_merg=merge(u_all,u_psy,by="DRUG",all=TRUE)

o=order(u_merg$ALL)
jpeg(file="../results/barplot_psy_v_other_unique_npi.jpeg",quality=100)
barplot(u_merg$ALL[o],names.arg=u_merg$DRUG[o],las=1,horiz=TRUE,cex.names=0.7,col="blue",border=FALSE,xlab="Speciality Counts Psy Drugs Unique NPI")
barplot(u_merg$PSY[o],horiz=TRUE,border=FALSE,col="red",add=TRUE)
legend(10,10,c("NPI Given Psy Drugs","NPI Given Other Drugs"),col=c("red","blue"),lty=1,lwd=10)
dev.off()
# 
#We’d then like to choose one of the following two questions to explore with this data:
#
#1. How do primary care physicians (PCPs) who prescribe psychiatric medications differ from psychiatrists in their treatment patterns? Define PCPs as anyone specializing in Internal Medicine, General Practice or Family Practice; and define psychiatrists as people who specialized in Psychiatry & Neurology.

### Define treatment as a combination of Psy Drugs ###
s=split(as.character(data_pdrugs$GENERIC_NAME),as.character(data_pdrugs$NPI))
ss=lapply(s,sort)
treatments=unlist(lapply(s,paste,collapse=":"))

### Create a matrix containing co-perscribe probabilities for PSY v. OTHER specialities ###

# data_pdrugs = All observations in data set containing PSY drugs
im_k=data_pdrugs$SPECIALTY_DESC=="Internal Medicine"
fp_k=data_pdrugs$SPECIALTY_DESC=="Family Practice"
gp_k=data_pdrugs$SPECIALTY_DESC=="General Practice"

psy_p=data_pdrugs$SPECIALTY_DESC=="Psychiatry"
psy_n=data_pdrugs$SPECIALTY_DESC=="Psychiatry & Neurology"

table(im_k)
table(fp_k)
table(gp_k)

other=im_k | fp_k | gp_k 
psy=psy_p | psy_n
npi_other=data.frame(NPI=unique(data_pdrugs$NPI[other]),other=1)
npi_psy=data.frame(NPI=unique(data_pdrugs$NPI[psy]),psy=1)

npi_all=merge(npi_psy,npi_other,all=TRUE,by="NPI")
npi_all$psy[is.na(npi_all$psy)]=0
npi_all$other[is.na(npi_all$other)]=0

table(npi_all$other,npi_all$psy)

### Remove all that are not the categories we defined all.y=TRUE ###
all=merge(data_pdrugs,npi_all,by="NPI",all.y=TRUE)

### Remove cases where an NPI was given same drug twice NEED TO THINK ABOUT THIS ###
# data_psy[data_psy$NPI==1003875014 & data_psy$GENERIC_NAME=="BUPROPION HCL",]
all=all[!duplicated(paste(all$GENERIC_NAME,all$NPI)),]

### Data Set #1 - NPI that only visited OTHER specialities and had Psy Drugs ###
data_other=all[all$psy==0 & all$other==1,]

### Data Set #2 - NPI that only visited PSY and had Psy Drugs ###
data_psy=all[all$psy==1 & all$other==0,]

### Data Set #3 - NPI that were prescribed PSY Drugs by both PSY and OTHER specialities ###
data_both=all[all$psy==1 & all$other==1,]

### Look at average number of unique drugs per patient ####
to=density(table(data_other$NPI))
tp=density(table(data_psy$NPI))
# tb=density(table(data_both$NPI))

plot(to,col="blue")
points(tp,col="red",type="l")

### Focus on Significant Co-Occurences ###

### Build Co-Perscibe probabilities ###
# NPI, DRUG -> DRUG, DRUG
#DRUG1,DRUG2,TOTAL_DAY_SUPPLY1/BENE_COUNT1,TOTAL_DAY_SUPPLY2/BENE_COUNT2


### Merge all the split data sets ###
p1=lapply(split(as.character(data_psy$GENERIC_NAME),as.character(data_psy$NPI)),pairup)
out=do.call("rbind",p1)

### Get Counts of all Drugs ###

## Some NPI have multiple's of same generic drug NEED TO THINK ABOUT  we removed them earlier ### 
# data_psy[data_psy$NPI==1003875014 & data_psy$GENERIC_NAME=="BUPROPION HCL",]

total1=as.data.frame(table(data_psy$GENERIC_NAME))
names(total1)=c("DRUG1","Total1")
total2=total1
names(total2)=c("DRUG2","Total2")
### Get Co-occurences ###

t=table(out$DRUG1,out$DRUG2)

### Probability of Drug in Column Being Prescribed Given Drug in Row has been Prescribed ###

d=as.data.frame(table(out$DRUG1,out$DRUG2))
m=as.matrix(table(out$DRUG1,out$DRUG2)) 

## Divide by total row ###
row_order=data.frame(DRUG1=row.names(m),order=1:length(row.names(m)))
total=merge(row_order,total1,by="DRUG1")
total$Total1[is.na(total$Total1)]=0.00001

print("Making Matrix")
for(i in 1:nrow(m)){
m[i,]=m[i,]/total$Total1[total$order==i]
}
print("Done")

m=m[order(rownames(m)),]
m=m[,order(colnames(m))]
m2=m
m2[m2<1]=0
m2[m2>=1]=1

#g=graph.adjacency(m2)

names(d)=c("DRUG1","DRUG2","Freq")
d=merge(d,total1,all.x=TRUE,by="DRUG1")
d=merge(d,total2,all.x=TRUE,by="DRUG2")
d$Prob=d$Freq/d$Total1

### Turn back into matrix ###


#
#2. How does geography correlate with other data attributes, such as provider density, provider specialties, medications being prescribed, or costs? You can find the geocoded location for most providers here (geocoding file). And it’s ok to only analyze a subset of the data.
#










