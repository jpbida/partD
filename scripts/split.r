### Split dataset into counties ###
### ran sort first then awk to get ids ##

library("maps")
### Split dataset up into US counties ###
geo=read.table(file="../data/prescriber_geocode.txt",sep=",",header=TRUE)
geo$county=map.where("county",geo$longitude,geo$latitude)
groups=NULL
groups$county=unique(geo$county)
groups$group=c(1:length(groups$county))

groups=as.data.frame(groups)

geo_all=merge(geo,groups,by="county",all.x=TRUE)

### only use ids with geodata ##
ids=read.table(file="../data/ids.txt")
names(ids)=c("count","npi")
splits=merge(ids,geo_all,by="npi",all.x=TRUE)
splits$county[is.na(splits$county)]="unknown"
splits=splits[order(splits$npi),]

cur=0
mod=500
for(i in 1:nrow(splits)){
if(i %% mod == 0){
## Delete the completed ##
#print(sum(splits$count[pmax((i-mod),1):(i-1)]))
delete=sum(splits$count[pmax((i-mod),1):(i-1)])
#print(paste("sed -i '' '1,",delete,"d' ../data/all_sorted.tab",sep=""))
system(paste("sed -i '' '1,",delete,"d' ../data/all_sorted.tab",sep=""))
print(paste("deleted: ",delete," ",splits$npi[i],sep=""))
cur=0
}
system(paste("head -n ",(cur+splits$count[i])," ../data/all_sorted.tab | tail -n ",splits$count[i]," >> ../data/county/county_",splits$group[i],".tab",sep=""))
cur=cur+splits$count[i]
}
