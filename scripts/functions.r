

set_bene_count = function(data) {
## Given data frame with missing bene_count data 
## Returns bene_count data filled in 
k=is.na(data$BENE_COUNT) 
data$BENE_COUNT[k]=5.5

data
}

set_total_day_supply = function (data) {
k=is.na(data$TOTAL_DAY_SUPPLY)

if(dim(table(k))>1){
print("WARNING: Missing TOTAL_DAY_SUPPLY found. Set to zero")
}
data$TOTAL_DAY_SUPPLY[k]=0

data
}

### Prevelance of Specialty ###
spec_prev = function (data,specs) { 
out=NULL
tot_bc=as.data.frame(unlist(lapply(split(data$BENE_COUNT,data$SPECIALTY_DESC),sum)))
names(tot_bc)=c("BENE_COUNT")
tot_bc$SPECIALTY_DESC=as.character(row.names(tot_bc))
row.names(tot_bc)=NULL

tot_ds=as.data.frame(unlist(lapply(split(data$TOTAL_DAY_SUPPLY,data$SPECIALTY_DESC),sum)))
names(tot_ds)=c("TOTAL_DAY_SUPPLY")
tot_ds$SPECIALTY_DESC=as.character(row.names(tot_ds))
row.names(tot_ds)=NULL

tall=merge(tot_ds,tot_bc,all=TRUE,by="SPECIALTY_DESC")
tall
} 

### Drug prevelance ##
drug_prev = function (data, drugs) { 
out=NULL

for(i in 1:length(drugs)){
### For each drug get the % BENE and % Count within specialities ###
print(paste("Parsing Drug: ",drugs[i],sep=""))
k=data$GENERIC_NAME==drugs[i]
tmp=data[k,]

tot_bc=as.data.frame(unlist(lapply(split(tmp$BENE_COUNT,tmp$SPECIALTY_DESC),sum)))
names(tot_bc)=c("BENE_COUNT")
tot_bc$SPECIALTY_DESC=as.character(row.names(tot_bc))
row.names(tot_bc)=NULL

tot_ds=as.data.frame(unlist(lapply(split(tmp$TOTAL_DAY_SUPPLY,tmp$SPECIALTY_DESC),sum)))
names(tot_ds)=c("TOTAL_DAY_SUPPLY")
tot_ds$SPECIALTY_DESC=as.character(row.names(tot_ds))
row.names(tot_ds)=NULL

tall=merge(tot_ds,tot_bc,all=TRUE,by="SPECIALTY_DESC")

tall$GENERIC_NAME=drugs[i]
if(is.null(out)){
out=tall
}else{
out=rbind(out,tall)
}
}

out
}

drug_prev_npi_group = function (data, drugs,npi) { 
output=NULL
m_bc=NULL
m_ds=NULL
m_dc=NULL
stats=NULL
## Creates a matrix containing the relative percentage of the total_day_supply for the drug list provided ###
##              NPI  
##         1   2   3   4 
## Drug1   10  10  30  50
## Drug2   50  30  20  50
## Drug3   40  30  20  0
## Drug4   0   30  30  0
# Foreach NPI calculate the relative percentage ##

drugs=data.frame(GENERIC_NAME=drugs)

### Get just the drugs of interest ###

tnpi=merge(drugs,data,by="GENERIC_NAME",all.x=TRUE)

for(j in unique(npi$group)) {
	## Get Just the NPI set ###
	tmp=merge(tnpi,npi[npi$group==j,],by="NPI",all.y=TRUE)
	##
       
        ### Get counts & Sums ##

print(paste("Parsing NPI Group: ",j,sep=""))
tot_bc=as.data.frame(unlist(lapply(split(tmp$BENE_COUNT,tmp$GENERIC_NAME),sum,na.rm=TRUE)))
names(tot_bc)=c("BENE_COUNT")
tot_bc$GENERIC_NAME=as.character(row.names(tot_bc))
tot_bc$NPI_GROUP=j
row.names(tot_bc)=NULL
total_bc=sum(tot_bc$BENE_COUNT,na.rm=TRUE)

df_bc=data.frame((tot_bc$BENE_COUNT/total_bc))
names(df_bc)=j
if(is.null(m_bc)){
m_bc=df_bc
}else{
m_bc=cbind(m_bc,df_bc)
}

tot_ds=as.data.frame(unlist(lapply(split(tmp$TOTAL_DAY_SUPPLY,tmp$GENERIC_NAME),sum)))
names(tot_ds)=c("TOTAL_DAY_SUPPLY")
tot_ds$GENERIC_NAME=as.character(row.names(tot_ds))
row.names(tot_ds)=NULL

total_ds=sum(tot_ds$TOTAL_DAY_SUPPLY,na.rm=TRUE)

df_ds=data.frame((tot_ds$TOTAL_DAY_SUPPLY/total_ds))
names(df_ds)=j
if(is.null(m_ds)){
m_ds=df_ds
}else{
m_ds=cbind(m_ds,df_ds)
}

tot_dc=as.data.frame(unlist(lapply(split(tmp$TOTAL_DRUG_COST,tmp$GENERIC_NAME),sum)))
names(tot_dc)=c("TOTAL_DRUG_COST")
tot_dc$GENERIC_NAME=as.character(row.names(tot_dc))
row.names(tot_dc)=NULL
total_dc=sum(tot_dc$TOTAL_DRUG_COST,na.rm=TRUE)


df_dc=data.frame((tot_dc$TOTAL_DRUG_COST/total_dc))
names(df_dc)=j
if(is.null(m_dc)){
m_dc=df_dc
}else{
m_dc=cbind(m_dc,df_dc)
}


tall=merge(tot_ds,tot_bc,all=TRUE,by="GENERIC_NAME")
tall=merge(tall,tot_dc,all=TRUE,by="GENERIC_NAME")

if(is.null(stats)){
stats=tall
}else{
print(names(stats))
print(names(tall))
stats=rbind(stats,tall)
}

### End of NPI loop
}


### End of function
output[[1]]=m_dc
output[[2]]=m_ds
output[[3]]=m_bc
output[[4]]=stats
output
}




pairup = function(v){
### Given a vector it returns a data frame with two columns containing all pairs ###
out=data.frame(DRUG1=v[1],DRUG2=v)
if(length(v)>1){
for(i in 2:length(v)){
d=data.frame(DRUG1=v[i],DRUG2=v)
out=rbind(out,d)
}
}
### Remove DRUG1=DRUG2 ###
out=out[out$DRUG1!=out$DRUG2,]
out
}

dummy_vars = function (cat) { 
### Given a categorical variable returns a 
### data frame containy dummy variables



}
