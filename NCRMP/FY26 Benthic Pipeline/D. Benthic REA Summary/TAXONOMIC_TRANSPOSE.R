#### On Friday 11/21/2025 CC and TAO discussed the taxonomic approach in our REA data and decided
#(1) That existing "analysis ready" data from 13-23 shall remain unchanged
#(2) That the "Taxa_MASTER" file will shift from a "if species code is listed, keep it, else genus" to a more explicit
# pairing of SPCODE and TAXONCODE that varys from OBS_YEAR and REGION. i.e. currently we only have SPCODE in the Taxa_MASTER,
#and if you're code is not present, you roll to genus (i.e. genus by omission). To better track changes, especially with an increasing 
#number of species complex codes in use, we will move to a complete lookup for all raw SPCODEs that explicitly match in the TAXONCODE column 
#to: 1) the appropriate SPCODE, 2) the GENUS_CODE, 3) or an explicit NA (for taxa not present in the region)
#to map this we will write a new "Convert_to_Taxoncode_2025" function.
rm(list=ls())

library(tidyverse)

AD23_load=load("T:/Benthic/Data/REA Coral Demography & Cover/Raw from Oracle/ALL_REA_ADULTCORAL_RAW_2013-2023.rdata")
AD23raw=df
#build composite region-year to ensure only year-region combos with data are in lookup
AD23raw$YEAR_REGION=paste0(AD23raw$OBS_YEAR,"_",AD23raw$REGION)
TM23=read.csv("T:/Benthic/Data/Lookup Tables/2013-23_Taxa_MASTER.csv")
TM23$YEAR_REGION=paste0(TM23$OBS_YEAR,"_",TM23$REGION)

uSPCODE=AD23raw %>% filter(TAXONCODE!="") %>% pull(TAXONCODE) %>% unique() %>% sort() #267 Unique Codes
uYR=AD23raw %>% filter(TAXONCODE!="") %>% pull(YEAR_REGION) %>% unique() %>% sort() #23 Unique YR
YRmat=matrix(unlist(strsplit(uYR,"_")),nrow=23,ncol=2,byrow = T)
YRdf=data.frame(YEAR_REGION=uYR,OBS_YEAR=YRmat[,1],REGION=YRmat[,2])
uY=sort(unique(YRmat[,1]))
uR=sort(unique(YRmat[,2]))

#Shooting for a Lookup Table from TM23, that gives SPCODE, YEAR, REGION, DoesNotExist,RollToComplex,RollToGenus,TAXONCODE
SYR=expand.grid(SPCODE=uSPCODE,YEAR_REGION=uYR)
SYR=SYR %>% left_join(YRdf)

#SPCODE DoesNotExist by REGION (Year shouldn't matter)
NRin=data.frame(SPCODE=uSPCODE)
for(i in 1:length(uR)){
  RecName=paste0("NR_In_",uR[i])
  RecReg=AD23raw %>% filter(REGION==uR[i]) %>% pull(TAXONCODE) %>% unique()
  NRin=NRin %>% mutate(!!RecName :=ifelse(SPCODE%in%RecReg,0,1))
}
NRin=NRin %>% pivot_longer(cols = NR_In_MARIAN:NR_In_SAMOA,names_to = "REGION",
                           names_prefix = "NR_In_",values_to = "NotRecordedInRegion")
#Build SYR NotRecordedInRegion Column
SYR=SYR %>% left_join(NRin)

#Build SYR Bulk Code Column, and Correct TAXONCODE
SYR=SYR %>% mutate(BulkCode=ifelse(SPCODE%in%c("AAAA","SSSS"),1,0))

#Roll To Complex set
CompSet=TM23[grep(pattern = "complex",TM23$TAXON_NAME),]
CompSet=CompSet %>% rename(TAXONCODE=SPCODE) %>% select(-TAXAGROUP,-TAXON_NAME)
ComplexSPCODES=data.frame(SPCODE=c(
  AD23raw %>% filter(TAXONNAME=="Pocillopora grandis") %>% pull(TAXONCODE) %>% unique(),
  AD23raw %>% filter(TAXONNAME=="Pocillopora woodjonesi") %>% pull(TAXONCODE) %>% unique(),
  AD23raw %>% filter(TAXONNAME=="Pocillopora meandrina") %>% pull(TAXONCODE) %>% unique(),
  AD23raw %>% filter(TAXONNAME=="Pocillopora verrucosa") %>% pull(TAXONCODE) %>% unique(),
  AD23raw %>% filter(TAXONNAME=="Porites monticulosa") %>% pull(TAXONCODE) %>% unique(),
  AD23raw %>% filter(TAXONNAME=="Porites rus") %>% pull(TAXONCODE) %>% unique()),
  TAXONCODE=c("PGWC","PGWC","PMVC","PMVC","PMRC","PMRC"))
CompSet=CompSet %>% full_join(ComplexSPCODES,relationship = "many-to-many")
CompSet$RollToComplex=1

#Build SYR RollToComplex Column
ComplexLU=CompSet$TAXONCODE;names(ComplexLU)=CompSet$SPCODE
SYR=SYR %>% left_join(select(CompSet,-TAXONCODE))
SYR=SYR %>% mutate(RollToComplex=ifelse(is.na(RollToComplex),0,RollToComplex))

#Build SYR ReportAsSpecies Column
TM23sp=TM23 %>% filter(TAXAGROUP=="SPECIES") %>% select(-TAXAGROUP,-TAXON_NAME) %>% mutate(ReportAsSpecies=1)
SYR=SYR %>% left_join(TM23sp) %>% mutate(ReportAsSpecies=ifelse(is.na(ReportAsSpecies),0,ReportAsSpecies))

#Build SYR RollToGenus Column, and Correct TAXONCODE
SYR$RollToGenus=NA
SYR=SYR %>% mutate(RollToGenus=ifelse(NotRecordedInRegion==1|RollToComplex==1|BulkCode==1|ReportAsSpecies==1,0,1))
SYR=SYR %>% relocate(SPCODE,YEAR_REGION,OBS_YEAR,REGION,NotRecordedInRegion,BulkCode,RollToComplex,RollToGenus,ReportAsSpecies)

#SPCODE to GENUS Lookup
s2g=AD23raw %>% select(TAXONCODE,GENUS_CODE) %>% distinct() %>% rename(SPCODE=TAXONCODE) %>% arrange(SPCODE)
s2g[s2g$SPCODE=="AAAA","GENUS_CODE"]="AAAA"
s2gLU=s2g$GENUS_CODE;names(s2gLU)=s2g$SPCODE;

###Set Appropriate TAXONCODE From boolean columns
#First check that one and only one fate column is selected
SYR=SYR %>% rowwise() %>% mutate(checksum=sum(c_across(NotRecordedInRegion:ReportAsSpecies)))
table(SYR$checksum)

#92 entries with two bool col selected
#SYR %>% filter(checksum!=1) %>% View()
#Turns out a lot of OK species codes have never been recorded in the region in question. 
#We can dig into these later, but for now we'll allow these to be reported as species (but there not in the data, so really no harm here)
SYR=SYR %>% mutate(NotRecordedInRegion=ifelse(checksum==2,0,NotRecordedInRegion))
#recalc check
SYR=SYR %>% rowwise() %>% mutate(checksum=sum(c_across(NotRecordedInRegion:ReportAsSpecies)))
table(SYR$checksum)


#Now Assign NA TAXONCODE for NotRecorded
SYR$TAXONCODE="UNFILLED"
SYR=SYR %>% mutate(TAXONCODE=ifelse(NotRecordedInRegion==1,NA,TAXONCODE))
SYR=SYR %>% mutate(TAXONCODE=ifelse(BulkCode==1,SPCODE,TAXONCODE))
SYR=SYR %>% mutate(TAXONCODE=ifelse(RollToComplex==1,ComplexLU[SPCODE],TAXONCODE))
SYR=SYR %>% mutate(TAXONCODE=ifelse(RollToGenus==1,s2gLU[SPCODE],TAXONCODE))
SYR=SYR %>% mutate(TAXONCODE=ifelse(ReportAsSpecies==1,SPCODE,TAXONCODE))

#SYR %>% filter(NotRecordedInRegion==1) %>% View()
#SYR %>% filter(NotRecordedInRegion==0) %>% View()

Output23=SYR %>% filter(NotRecordedInRegion==0)

library(worrms)
sp2taxname=TM23[,c("SPCODE","TAXON_NAME")] %>% distinct() %>% arrange(SPCODE)
sp2taxname$VALID_TAXON_NAME=NULL
sp2taxname$WORMFLAG=NULL
for(i in 1:nrow(sp2taxname)){
  worm_search=try(wm_records_taxamatch(sp2taxname$TAXON_NAME[i]))
  if(class(worm_search)=="try-error"){
    sp2taxname$VALID_TAXON_NAME[i]="NO WORM ENTRY"
    sp2taxname$WORMFLAG=1
  }else{
    sp2taxname$VALID_TAXON_NAME[i]=worm_search[[1]]$valid_name
    sp2taxname$WORMFLAG[i]=ifelse(length(worm_search)>1,1,sp2taxname$WORMFLAG[i])
  }
  print(paste(i,"of",nrow(sp2taxname)))
}

write.csv(sp2taxname,"T:/Benthic/Data/Lookup Tables/REA TAXONOMY ROUGH WORK/SPCODE_TO_TAXON_NAME_rough.csv",row.names = F)

















YRS=expand.grid(SPCODE=AllRawCodesEver,YEAR_REGION=unique(AD23raw$YEAR_REGION))
#TM23=TM23 %>% rename(TAXONCODE=SPCODE)
YRSG=YRS %>% group_by(YEAR_REGION,SPCODE) %>% left_join(unique(AD23raw[,c("TAXONCODE","GENUS_CODE")]),by=c("SPCODE"="TAXONCODE")) %>% arrange(desc(YEAR_REGION))
YRSGTT=YRSG %>% group_by(YEAR_REGION,SPCODE,GENUS_CODE) %>% left_join(TM23) %>% arrange(desc(YEAR_REGION))
View(YRSGTT)

RawDataTaxonomy23=AD23raw %>% group_by(S_ORDER,GENUS,GENUS_CODE,TAXONCODE,TAXONNAME,RANK,SCIENTIFIC_NAME,REGION,OBS_YEAR) %>% 
  #  mutate(REGION_YEAR=paste0(OBS_YEAR,"_",REGION)) %>% 
  summarize(PRESENT=1) %>% 
  pivot_wider(names_from=c(OBS_YEAR,REGION),values_from = PRESENT,values_fill = 0) %>% 
  arrange(desc(S_ORDER),GENUS,RANK,TAXONCODE)
write.csv(RawDataTaxonomy23,"T:/Benthic/Data/Lookup Tables/TA2013-23_Taxa_RawData_TRANSPOSE_REVIEWEDTAO20251120.csv",row.names = F)






AD23c=read.csv("T:/Benthic/Data/REA Coral Demography & Cover/Analysis Ready Raw data/CoralBelt_Adults_raw_CLEANED_2023.csv")
RealizedTaxonomy23=AD23c %>% group_by(S_ORDER,TAXONCODE,TAXONNAME,REGION,OBS_YEAR) %>% 
  #  mutate(REGION_YEAR=paste0(OBS_YEAR,"_",REGION)) %>% 
  summarize(PRESENT=1) %>% 
  pivot_wider(names_from=c(OBS_YEAR,REGION),values_from = PRESENT,values_fill = 0) %>% 
  arrange(desc(S_ORDER),TAXONCODE)
write.csv(RealizedTaxonomy23,"T:/Benthic/Data/Lookup Tables/2013-23_Taxa_Realized_TRANSPOSE_REVIEWEDTAO20251120.csv",row.names = F)


# This takes our Taxonomic Master for comparison to the new one...
TT=read.csv("T:/Benthic/Data/Lookup Tables/2013-25_Taxa_MASTER_TRANSPOSE_DRAFT_REVIEWEDTAO20251120.csv")
head(TM23)
head(TT)

TM25=TT %>% pivot_longer(cols=starts_with("X"))
splist=strsplit(TM25$name,"_")
for(i in 1:length(splist)){
  thisi=unlist(splist[i])
  if(length(thisi)==2){
    TM25$OBS_YEAR[i]=substr(thisi[1],2,5) 
    TM25$REGION[i]=thisi[2]
  }else{
    TM25$OBS_YEAR[i]=paste0(substr(thisi[1],2,5),"_",thisi[2])
    TM25$REGION[i]=thisi[3]
  }
}
TM25=TM25[,names(TM23)]
write.csv(TM25,"T:/Benthic/Data/Lookup Tables/2013-25_Taxa_MASTER.csv")


