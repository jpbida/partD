supall=read.table(file="../data/supall.tab",sep="\t",header=FALSE)
supdrg=read.table(file="../data/supdrg.tab",sep="\t",header=FALSE)

names(supall)=c("state","spec","state_supply","state_cost")
names(supdrg)=c("state","spec","drg","supply","cost")

### Calculat Z-scores ###

all=merge(supdrg,supall,by=c("state","spec"),all.x=TRUE)
all$pct_supply=all$supply/all$state_supply
all$pct_supply=all$supply/all$state_supply
all$pct_cost=all$cost/all$state_cost


### Calculate the Average & SD For each Drug ###
### Percentage is based on the volume of the specialty ##
### We will take the zscore based on both specialties percentages ##
### ASSUMPTION: 
### Since there are equal number of psy and pcp we don't have too ##
### much on an issue using this approach 

ave_supply=as.data.frame(unlist(lapply(split(all$pct_supply,all$drg),mean)))

ave_cost=as.data.frame(unlist(lapply(split(all$pct_cost,all$drg),mean)))
sd_supply=as.data.frame(unlist(lapply(split(all$pct_supply,all$drg),sd)))
sd_cost=as.data.frame(unlist(lapply(split(all$pct_cost,all$drg),sd)))

ave_supply$drg=row.names(ave_supply)
ave_cost$drg=row.names(ave_cost)
sd_supply$drg=row.names(sd_supply)
sd_cost$drg=row.names(sd_cost)
row.names(ave_supply)=NULL
row.names(ave_cost)=NULL
row.names(sd_cost)=NULL
row.names(sd_supply)=NULL
names(ave_supply)=c("ave_pct_supply","drg")
names(ave_cost)=c("ave_pct_cost","drg")
names(sd_cost)=c("sd_pct_cost","drg")
names(sd_supply)=c("sd_pct_supply","drg")

all=merge(all,ave_supply,by="drg",all.x=TRUE)
all=merge(all,ave_cost  ,by="drg",all.x=TRUE)
all=merge(all,sd_supply,by="drg",all.x=TRUE)
all=merge(all,sd_cost  ,by="drg",all.x=TRUE)

## Calculate ZScores ###

all$supply_zscore=(all$pct_supply-all$ave_pct_supply) / all$sd_pct_supply
all$cost_zscore=(all$pct_cost-all$ave_pct_cost) / all$sd_pct_cost

drugs=c(
"CLONAZEPAM",
"QUETIAPINE FUMARATE",
"BUPROPION HCL",
"TRAZODONE HCL",
"RISPERIDONE",
"SERTRALINE HCL",
"FLUOXETINE HCL",
"ZOLPIDEM TARTRATE",
"DIVALPROEX SODIUM",
"LAMOTRIGINE")
for(j in 1:length(drugs)){
## Get the order of states ###
states_psych=as.character(unique(all$state[all$spec=="psych" & all$drg==drugs[j]]))
states_pcp=as.character(unique(all$state[all$spec=="pcp" & all$drg==drugs[j]]))
## Getting rid of states without psych (looks like non-state abrv) ##
keep1=data.frame(state=states_psych,keep1=1)
keep2=data.frame(state=states_pcp,keep2=1)

tmp=merge(all,keep1,by="state",all.x=TRUE)
tmp=merge(tmp,keep2,by="state",all.x=TRUE)
tmp=tmp[!is.na(tmp$keep1) & !is.na(tmp$keep2),]

pcp=tmp[tmp$spec=="pcp" & tmp$drg==drugs[j],]
psy=tmp[tmp$spec=="psych" & tmp$drg==drugs[j],]

jpeg(file=paste("../results/bar_plot",j,".jpeg",sep=""),quality=100,height=500,width=5000)

barplot(pcp$supply_zscore[order(pcp$state)],names.arg=as.character(pcp$state[order(pcp$state)]),col="red",ylim=c(-5,5),main=drugs[j])
barplot(psy$supply_zscore[order(psy$state)],names.arg=psy$state[order(psy$state)],col="blue",add=TRUE,density=25)
dev.off()

}
for(j in 1:length(drugs)){
## Get the order of states ###
states_psych=as.character(unique(all$state[all$spec=="psych" & all$drg==drugs[j]]))
states_pcp=as.character(unique(all$state[all$spec=="pcp" & all$drg==drugs[j]]))
## Getting rid of states without psych (looks like non-state abrv) ##
keep1=data.frame(state=states_psych,keep1=1)
keep2=data.frame(state=states_pcp,keep2=1)

tmp=merge(all,keep1,by="state",all.x=TRUE)
tmp=merge(tmp,keep2,by="state",all.x=TRUE)
tmp=tmp[!is.na(tmp$keep1) & !is.na(tmp$keep2),]

pcp=tmp[tmp$spec=="pcp" & tmp$drg==drugs[j],]
psy=tmp[tmp$spec=="psych" & tmp$drg==drugs[j],]

jpeg(file=paste("../results/cost_bar_plot",j,".jpeg",sep=""),quality=100,height=500,width=5000)

barplot(pcp$cost_zscore[order(pcp$state)],names.arg=as.character(pcp$state[order(pcp$state)]),col="red",ylim=c(-5,5),main=drugs[j])
barplot(psy$cost_zscore[order(psy$state)],names.arg=psy$state[order(psy$state)],col="blue",add=TRUE,density=25)
dev.off()

}


### Efficiency Plot ##

k=all$spec=="pcp"
plot(all$supply_zscore,all$cost_zscore,pch=16,col=4,xlab="PCT Supply Zscore",ylab="PCT Cost Zscore",main="Efficiency of Psych Treatment: PCP v. Psych")
points(all$supply_zscore[k],all$cost_zscore[k],pch=16,col=2)
abline(a=2,b=1,lty=2)
abline(a=-2,b=1,lty=2)

abline(a=1,b=1,lty=3)
abline(a=-1,b=1,lty=3)


### Plotting the efficiencies back onto the states ###
for(i in 1:nrow(all)){
x=all$supply_zscore[i]
y=all$cost_zscore[i]
e1=2+x
e2=1+x
e3=-1+x
e4=-2+x
eff=-2
if(y > e1){
eff=2
}else{
if(y > e2){
eff=1
}else{
if(y > e3){
eff=0
}else{
if(y > e4){
eff=-1
}else{
eff=-2
}
}


}


}
all$eff[i]=eff
}
eff=as.data.frame(unlist(lapply(split(all$eff,all$state),sum)))
m=abs(min(eff[,1]))
eff[,1]=eff[,1]+m+1
mx=max(eff[,1])
colors=heat.colors(mx)
library("maps")
map('state',fill=FALSE,col="grey",add=FALSE)
nm=data.frame(nm=row.names(eff))
n2=data.frame(nm=state.abb,name=state.name)
nms=merge(nm,n2,by="nm",all.x=TRUE)


##Getting rid of fake states ###
for(i in 1:nrow(eff)){
if(!is.na(nms$name[i])){
try(map('state',regions=nms$name[i],fill=TRUE,col=colors[eff[i,1]],add=TRUE))
}
}

