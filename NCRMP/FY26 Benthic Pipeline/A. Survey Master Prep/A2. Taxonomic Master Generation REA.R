#### On Friday 11/21/2025 CC and TAO discussed the taxonomic approach in our REA data and decided
#(1) That existing "analysis ready" data from 13-23 shall remain unchanged
#(2) That the "Taxa_MASTER" file will shift from a "if species code is listed, keep it, else genus" to a more explicit
# pairing of SPCODE and TAXONCODE that varys from OBS_YEAR and REGION. i.e. currently we only have SPCODE in the Taxa_MASTER,
#and if you're code is not present, you roll to genus (i.e. genus by omission). To better track changes, especially with an increasing 
#number of species complex codes in use, we will move to a complete lookup for all raw SPCODEs that explicitly match in the TAXONCODE column 
#to: 1) the appropriate SPCODE, 2) the GENUS_CODE, 3) or an explicit NA (for taxa not present in the region)
#to map this we will write a new "Convert_to_Taxoncode_2025" function.
rm(list=ls())
pacman::p_unload(pacman::p_loaded(), character.only = TRUE)

library(tidyverse)

#Load Raw data up until 23
AD23_load=load("T:/Benthic/Data/REA Coral Demography & Cover/Raw from Oracle/ALL_REA_ADULTCORAL_RAW_2013-2023.rdata")
AD23raw=df
#build composite region-year to ensure only year-region combos with data are in lookup
AD23raw$YEAR_REGION=paste0(AD23raw$OBS_YEAR,"_",AD23raw$REGION)

#Load Taxa Master up until 23
TM23=read.csv("T:/Benthic/Data/Lookup Tables/2013-23_Taxa_MASTER.csv")
TM23$YEAR_REGION=paste0(TM23$OBS_YEAR,"_",TM23$REGION)

#Load TM24
TM24=read.csv("T:/Benthic/Data/Lookup Tables/2013-25_Taxa_MASTER_TRANSPOSE_DRAFT_REVIEWEDTAO20251120.csv")
TM24.=TM24 %>% select(TAXON_NAME,SPCODE,TAXAGROUP,X2024_MHI,X2024_NWHI) %>% 
  pivot_longer(cols = X2024_MHI:X2024_NWHI,names_to =c("OBS_YEAR","REGION"),names_prefix = "X",values_to="Keep",names_sep="_") %>%
  filter(Keep==1)
TM24.$YEAR_REGION=paste0(TM24.$OBS_YEAR,"_",TM24.$REGION)

head(TM23)
head(TM24.)

TMall=rbind(TM23[,c("SPCODE", "REGION", "OBS_YEAR","YEAR_REGION","TAXON_NAME","TAXAGROUP")],
            TM24.[,c("SPCODE", "REGION", "OBS_YEAR","YEAR_REGION","TAXON_NAME","TAXAGROUP")])

head(TMall)

ADtax=AD23raw %>% select(TAXONCODE,TAXONNAME,YEAR_REGION,OBS_YEAR,REGION) %>% rename(SPCODE=TAXONCODE,TAXON_NAME=TAXONNAME) %>% distinct() 
ADtax=rbind(ADtax,TMall[,c("SPCODE","TAXON_NAME","YEAR_REGION","OBS_YEAR","REGION")])

uSPCODE=ADtax %>% filter(SPCODE!="") %>% pull(SPCODE) %>% unique() %>% sort() #267 Unique Codes
uYR=ADtax %>% filter(SPCODE!="") %>% pull(YEAR_REGION) %>% unique() %>% sort() #23 Unique YR
ulYR=unlist(strsplit(uYR,"_"));ulYR[1]=paste0(ulYR[1],"_",ulYR[2]);ulYR=ulYR[-2]
YRmat=matrix(ulYR,nrow=26,ncol=2,byrow = T)
YRdf=data.frame(YEAR_REGION=uYR,OBS_YEAR=YRmat[,1],REGION=YRmat[,2])
uY=sort(unique(YRmat[,1]))
uR=sort(unique(YRmat[,2]))

#Shooting for a Lookup Table from TMall, that gives SPCODE, YEAR, REGION, DoesNotExist,RollToComplex,RollToGenus,TAXONCODE
SYR=expand.grid(SPCODE=uSPCODE,YEAR_REGION=uYR)
SYR=SYR %>% left_join(YRdf)

#SPCODE DoesNotExist by REGION (Year shouldn't matter)
NRin=data.frame(SPCODE=uSPCODE)
for(i in 1:length(uR)){
  RecName=paste0("NR_In_",uR[i])
  RecReg=ADtax %>% filter(REGION==uR[i]) %>% pull(SPCODE) %>% unique()
  NRin=NRin %>% mutate(!!RecName :=ifelse(SPCODE%in%RecReg,0,1))
}
NRin=NRin %>% pivot_longer(cols = NR_In_MARIAN:NR_In_SAMOA,names_to = "REGION",
                           names_prefix = "NR_In_",values_to = "NotRecordedInRegion")
#Build SYR NotRecordedInRegion Column
SYR=SYR %>% left_join(NRin)

#Troubleshoot DIAS code - can be found anywhere...
SYR =SYR %>% mutate(NotRecordedInRegion=ifelse(SPCODE=="DIAS",0,NotRecordedInRegion))

#Build SYR Bulk Code Column, and Correct TAXONCODE
SYR=SYR %>% mutate(BulkCode=ifelse(SPCODE%in%c("AAAA","SSSS"),1,0))

#Roll To Complex set
CompSet=rbind(TMall[grep(pattern = "complex",TMall$TAXON_NAME),])

CompSet=CompSet %>% rename(TAXONCODE=SPCODE) %>% select(-TAXAGROUP,-TAXON_NAME)
ComplexSPCODES=data.frame(SPCODE=c(
  ADtax %>% filter(TAXON_NAME=="Pocillopora grandis") %>% pull(SPCODE) %>% unique(),
  ADtax %>% filter(TAXON_NAME=="Pocillopora woodjonesi") %>% pull(SPCODE) %>% unique(),
  ADtax %>% filter(TAXON_NAME=="Pocillopora meandrina") %>% pull(SPCODE) %>% unique(),
  ADtax %>% filter(TAXON_NAME=="Pocillopora verrucosa") %>% pull(SPCODE) %>% unique(),
  ADtax %>% filter(TAXON_NAME=="Porites monticulosa") %>% pull(SPCODE) %>% unique(),
  ADtax %>% filter(TAXON_NAME=="Porites rus") %>% pull(SPCODE) %>% unique()),
  TAXONCODE=c("PGWC","PGWC","PMVC","PMVC","PMRC","PMRC"))
CompSet=CompSet %>% full_join(ComplexSPCODES,relationship = "many-to-many")
CompSet$RollToComplex=1

#Build SYR RollToComplex Column
ComplexLU=CompSet$TAXONCODE;names(ComplexLU)=CompSet$SPCODE
SYR=SYR %>% left_join(select(CompSet,-TAXONCODE))
SYR=SYR %>% mutate(RollToComplex=ifelse(is.na(RollToComplex),0,RollToComplex))

#Build SYR ReportAsSpecies Column
TMallsp=TMall %>% filter(TAXAGROUP=="SPECIES") %>% select(-TAXAGROUP,-TAXON_NAME) %>% mutate(ReportAsSpecies=1)
SYR=SYR %>% left_join(TMallsp) %>% mutate(ReportAsSpecies=ifelse(is.na(ReportAsSpecies),0,ReportAsSpecies))

#Build SYR RollToGenus Column, and Correct TAXONCODE
SYR$RollToGenus=NA
SYR=SYR %>% mutate(RollToGenus=ifelse(NotRecordedInRegion==1|RollToComplex==1|BulkCode==1|ReportAsSpecies==1,0,1))
SYR=SYR %>% relocate(SPCODE,YEAR_REGION,OBS_YEAR,REGION,NotRecordedInRegion,BulkCode,RollToComplex,RollToGenus,ReportAsSpecies)

#SPCODE to GENUS Lookup
s2g=AD23raw %>% select(TAXONCODE,GENUS_CODE) %>% distinct() %>% rename(SPCODE=TAXONCODE) %>% arrange(SPCODE)
ExtraGs=TMall %>% filter(!SPCODE%in%s2g$SPCODE) %>% select(SPCODE,TAXON_NAME,TAXAGROUP) %>% distinct()
s2g[s2g$SPCODE=="AAAA","GENUS_CODE"]="AAAA"
ExtraGs.G=ExtraGs %>% filter(TAXAGROUP=="GENUS")
s2g=rbind(s2g,data.frame(SPCODE=ExtraGs.G$SPCODE,GENUS_CODE=ExtraGs.G$SPCODE))
ExtraGs.S=ExtraGs %>% filter(TAXAGROUP!="GENUS")
s2g=rbind(s2g,data.frame(SPCODE=ExtraGs.S$SPCODE,GENUS_CODE=c("EUSP","LESP","MOSP","SESP")))
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
table(SYR$TAXONCODE)


#SYR %>% filter(NotRecordedInRegion==1) %>% View()
#SYR %>% filter(NotRecordedInRegion==0) %>% View()

# #Validate/Curate TAXON_NAME using WORMS
# library(worrms)--+
# sp2taxname= ADtax %>% 
#   distinct() %>% arrange(SPCODE) %>% filter(SPCODE!="",SPCODE!="AAAA")
# sp2taxname$VALID_ORDER=NA
# sp2taxname$VALID_TAXON_NAME=NA
# sp2taxname$WORMFLAG="NO_FLAG"
# for(i in 1:nrow(sp2taxname)){
#   worm_search=try(wm_records_taxamatch(sp2taxname$TAXON_NAME[i]))
#   if(class(worm_search)=="try-error"){
#     sp2taxname$VALID_ORDER[i]="NO WORM ENTRY"
#     sp2taxname$VALID_TAXON_NAME[i]="NO WORM ENTRY"
#     sp2taxname$WORMFLAG=1
#   }else{
#     sp2taxname$VALID_ORDER[i]=worm_search[[1]]$order
#     sp2taxname$VALID_TAXON_NAME[i]=worm_search[[1]]$valid_name
#     sp2taxname$WORMFLAG[i]=ifelse(length(worm_search)>1,1,sp2taxname$WORMFLAG[i])
#   }
#   print(paste(i,"of",nrow(sp2taxname)))
# }
# 
# write.csv(sp2taxname,"T:/Benthic/Data/Lookup Tables/REA TAXONOMY ROUGH WORK/SPCODE_TO_TAXON_NAME_WORMS_VALIDATED.csv",row.names = F)

#CURATED IN EXCEL
#Drop duplicate codes - drop the one that doesn't match the worm entry
#Add Rank - Genus, Order, Species, Complex... Correct mispellings, leave updates disctinctions 
sp2taxname=read.csv("T:/Benthic/Data/Lookup Tables/REA TAXONOMY ROUGH WORK/SPCODE_TO_TAXON_NAME_WORMS_VALIDATED_CURATED_20251124.csv")
OutputALL=SYR %>% filter(NotRecordedInRegion==0)
head(OutputALL)
head(TMall)
head(sp2taxname)
OutputALL=OutputALL %>% left_join(sp2taxname,by=c("TAXONCODE"="SPCODE")) %>% 
  select(TAXON_NAME,VALID_TAXON_NAME,VALID_ORDER,SPCODE,REGION,OBS_YEAR,RANK,TAXONCODE)

OutputALL %>% filter(is.na(TAXON_NAME))
head(OutputALL)
OutputALL=OutputALL %>% rename(TAXAGROUP=RANK)
#OutputALL %>% filter(SPCODE%in%c("PMVC")) %>% print(n=99)

write.csv(OutputALL,"T:/Benthic/Data/Lookup Tables/2013-24_Taxa_MASTER_2025VERSION.csv",row.names = F)

#This ends the 23 output and keeps it clean. Now we need to add 24 for MHI and NWHI

TM24.[which(!TM24.$SPCODE%in%uSPCODE),]
############################################################

YRS=expand.grid(SPCODE=AllRawCodesEver,YEAR_REGION=unique(ADtax$YEAR_REGION))
#TMall=TMall %>% rename(TAXONCODE=SPCODE)
YRSG=YRS %>% group_by(YEAR_REGION,SPCODE) %>% left_join(unique(ADtax[,c("TAXONCODE","GENUS_CODE")]),by=c("SPCODE"="TAXONCODE")) %>% arrange(desc(YEAR_REGION))
YRSGTT=YRSG %>% group_by(YEAR_REGION,SPCODE,GENUS_CODE) %>% left_join(TM23) %>% arrange(desc(YEAR_REGION))
View(YRSGTT)

RawDataTaxonomy23=ADtax %>% group_by(S_ORDER,GENUS,GENUS_CODE,TAXONCODE,TAXONNAME,RANK,SCIENTIFIC_NAME,REGION,OBS_YEAR) %>% 
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


