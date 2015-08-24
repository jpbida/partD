#! /usr/bin/env Rscript

# 1 NPI
# 2 NPPES_PROVIDER_LAST_ORG_NAME
# 3 NPPES_PROVIDER_FIRST_NAME
# 4 NPPES_PROVIDER_CITY
# 5 NPPES_PROVIDER_STATE
# 6 SPECIALTY_DESC
# 7 DESCRIPTION_FLAG
# 8 DRUG_NAME
# 9 GENERIC_NAME
# 10 BENE_COUNT
# 11 TOTAL_CLAIM_COUNT
# 12 TOTAL_DAY_SUPPLY
# 13 TOTAL_DRUG_COST
# 14 BENE_COUNT_GE65
# 15 BENE_COUNT_GE65_REDACT_FLAG
# 16 TOTAL_CLAIM_COUNT_GE65
# 17 GE65_REDACT_FLAG
# 18 TOTAL_DAY_SUPPLY_GE65
# 19 TOTAL_DRUG_COST_GE65
# mapper.R - Wordcount program in R
# script for Mapper (R-Hadoop integration)

splitIntoColumns <- function(line) unlist(strsplit(line, "\t"))

## Get Location ##


## Checks if specialty is in our list ##
getSpecialty <- function(col) {
        spec=""
	if( col[6]== "Psychiatry & Neurology" ){
        spec="psych"
	}else{
	if( col[6]== "Internal Medicine" | col[6]== "General Practice" | col[6]=="Family Practice"){
        spec="pcp"
	}else{
        spec="" 
	}
        }
spec
}

### Checks if location is in our list ###
getLocation <- function(col) {
	loc=col[5]
	loc
}

### Checks if drug is in our list ###
getPyschDrug <- function(col,drugs) {


drug=""
### This works if drugs list has more than 1 ###
if(nrow(as.data.frame(table(as.character(drugs)==as.character(col[9]))))>1){
drug=as.character(col[9])
}
drug
}

#drugs=read.table(file="/user/vagrant/drugs.tab")
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
#drugs=read.table(file="../drugs.tab")

### Take top 10 by prevalence 
## **** could wo with a single readLines or in blocks
con <- file("stdin", open = "r")
while (length(line <- readLines(con, n = 1, warn = FALSE)) > 0) {
    cols <- splitIntoColumns(line)
    spec=getSpecialty(cols)
    loc=getLocation(cols)
    drug=getPyschDrug(cols,drugs) 
    ### Output State-Spec-Drug / Total_Supply
    if(spec!="" & loc!="" & drug!=""){
    cat(paste(loc,spec,drug,sep=":"), "\t",cols[12],":",cols[13],"\n",sep="")
    }
}
close(con)
