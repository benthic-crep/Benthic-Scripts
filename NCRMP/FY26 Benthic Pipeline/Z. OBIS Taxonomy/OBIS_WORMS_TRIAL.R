library(robis)
library(worrms)
library(dplyr)
library(sf)

#Load Survey Domain KML
NCRMP_DOMAINS <- st_read("./NCRMP/FY26 Benthic Pipeline/Z. OBIS Taxonomy/OBIS Polygons.kml")
st_crs(NCRMP_DOMAINS)=4326
#Extract geometries as WKT text strings
spatial_data$wkt_Geometry <- st_as_text(st_geometry(spatial_data))
#Flatten to a standard data frame (removes spatial bounding metadata)
final_df <- st_drop_geometry(spatial_data)

OBIS_Col=c("scientificName","scientificNameAuthorship","taxonID","taxonRank",               
           "taxonomicStatus","acceptedNameUsage","acceptedNameUsageID",
           "kingdom","phylum","subphylum","class","order","suborder","family","genus","species",
           "kingdomid","phylumid","subphylumid","classid","orderid","suborderid","familyid","genusid","speciesid")

rss=NULL
for(iR in 1:nrow(final_df)){
  wkt_geometry=final_df$wkt_Geometry[iR]
  print(paste0("Starting with ",final_df$Name[iR],"!"))
  
  # Pull the complete taxon list for this region
  
  regional_taxa <- checklist(geometry = wkt_geometry)
  regional_scleractinia_spp = regional_taxa %>%
    filter(order=="Scleractinia",taxonRank == "Species",taxonomicStatus=="accepted") %>% 
    dplyr::select(all_of(OBIS_Col)) %>%
    arrange(scientificName) %>% distinct()
  print(paste0("Done with Regional Pull:",final_df$Name[iR],"!"))
  
  regional_scleractinia_spp$Region=final_df$Name[iR]
  rss=rbind(rss,regional_scleractinia_spp)
  print(paste0(final_df$Name[iR]," done!"))
}
write.csv(x = rss,
          file="./NCRMP/FY26 Benthic Pipeline/Z. OBIS Taxonomy/All_Region_Scleractinian_Species_List.csv")
#transition regional levels

rssW=rss %>%
  mutate(Presense=1) %>%
  pivot_wider(names_from = Region,values_from =Presense) %>% 
  arrange(taxonID)
keycols=OBIS_Col
pa_cols=names(rssW)[26:33]
rssWc=rssW %>% 
  group_by(across(all_of(keycols))) %>% 
  summarize(across(all_of(pa_cols), ~ sum(.x, na.rm = TRUE)), .groups = "drop")
View(rssWc)

#Add Depth
rssWc$N_occurrence=NA
rssWc$depth_q05=NA
rssWc$depth_q50=NA
rssWc$depth_q95=NA

for( iS in 1:nrow(rssWc)){
  print(paste0("pulling species ",iS," of ",nrow(rssWc),": ",
               rssWc$scientificName[iS]))
  this_species <- occurrence(taxonid = rssWc$taxonID[iS] )
  if(!"depth"%in%names(this_species)){
    this_species$depth=NA}else{
      hist(this_species$depth,30,
           main=this_species$scientificName[1],
           xlab="Depth (m)")
    }
  rssWc$N_occurrence[iS]=nrow(this_species)
  rssWc$depth_q05[iS]=quantile(this_species$depth,.05,na.rm=T)
  rssWc$depth_q50[iS]=quantile(this_species$depth,.5,na.rm=T)
  rssWc$depth_q95[iS]=quantile(this_species$depth,.95,na.rm=T)
}

write.csv(x = rssWc,
          file="./NCRMP/FY26 Benthic Pipeline/Z. OBIS Taxonomy/All_Region_Scleractinian_Species_List_Depth.csv")

rssWc=read.csv("./NCRMP/FY26 Benthic Pipeline/Z. OBIS Taxonomy/All_Region_Scleractinian_Species_List_Depth.csv")

rssWc_Shallow=rssWc %>% filter(Depth_Range%in%c("SHALLOW","SHALLOW_MESO","SHALLOW_DEEP","SHALLOW_UNK"))
write.csv(x = rssWc_Shallow,
          file="./NCRMP/FY26 Benthic Pipeline/Z. OBIS Taxonomy/All_Region_Scleractinian_Species_List_Shallow.csv")
dim(rssWc_Shallow)

#otax=rssWc_Shallow
###################################### WORMS,PQ,REA crosswalk
library(stringr)
otax=read.csv(file="./NCRMP/FY26 Benthic Pipeline/Z. OBIS Taxonomy/All_Region_Scleractinian_Species_List_Shallow.csv")
REAcode=read.csv("./NCRMP/FY26 Benthic Pipeline/Z. OBIS Taxonomy/2013-25_Taxa_MASTER_TRANSPOSE_DRAFT_REVIEWEDTAO20251120.csv")
PQcode=read.csv("./NCRMP/FY26 Benthic Pipeline/Z. OBIS Taxonomy/All_Photoquad_codes.csv")
rea=read.csv("T:/Benthic/Data/REA Coral Demography & Cover/Analysis Ready Raw data/CoralBelt_Adults_raw_CLEANED_2013.2024.csv")
covname=load("T:/Benthic/Data/REA Coral Demography & Cover/Analysis Ready Raw data/BIA_2010-2024_CLEANED.RData")
cov=BIA10_24
rea$REGION_YR=paste0(rea$REGION,"_",rea$OBS_YEAR)
cov$REGION_YR=paste0(cov$REGION,"_",cov$OBS_YEAR)
sort(unique(cov$REGION_YR))
code_usage_rea_sp=rea %>% group_by(REGION_YR,SPCODE) %>% summarize(N=length(SPCODE)) %>% pivot_wider(names_from = REGION_YR,values_from = N)
code_usage_rea_tax=rea %>% group_by(REGION_YR,TAXONCODE) %>% summarize(N=length(TAXONCODE)) %>% pivot_wider(names_from = REGION_YR,values_from = N)
code_usage_rea_gen=rea %>% group_by(REGION_YR,GENUS_CODE) %>% summarize(N=length(GENUS_CODE)) %>% pivot_wider(names_from = REGION_YR,values_from = N)
code_usage_cov_t1=cov %>% group_by(REGION_YR,TIER_1) %>% summarize(N=length(TIER_1)) %>% pivot_wider(names_from = REGION_YR,values_from = N)
code_usage_cov_t2b=cov %>% group_by(REGION_YR,TIER_2b) %>% summarize(N=length(TIER_2b)) %>% pivot_wider(names_from = REGION_YR,values_from = N)
code_usage_cov_t3=cov %>% group_by(REGION_YR,TIER_3) %>% summarize(N=length(TIER_3)) %>% pivot_wider(names_from = REGION_YR,values_from = N)


code_usage_rea_sp %>% filter(SPCODE=="LINC") %>% select(where(~ !any(is.na(.x))))
code_usage_rea_tax %>% filter(TAXONCODE=="SPIS"|TAXONCODE=="STYS") %>% select(where(~ !any(is.na(.x))))


#Cross-Walk SPCODE to otax
otax$SPCODE=REAcode$SPCODE[match(otax$scientificName,REAcode$TAXON_NAME)]
REA_genus=REAcode %>% filter(TAXAGROUP=="GENUS",str_sub(TAXON_NAME,-2,-1)=="sp")
otax$GENUS_CODE=REA_genus$SPCODE[match(otax$genus,REA_genus$GENUS_NAME)]
View(otax)

#CHECK MISSING otax
missing_code=which(!((REAcode$SPCODE%in%otax$SPCODE|REAcode$SPCODE%in%otax$GENUS_CODE)))
table(missing_code)

recordnames=names(otax)[3:27]
missingsp=REAcode$TAXON_NAME[missing_code]
for(i in 1:length(missingsp)){
  #accepted?
  thisid=wm_name2id(missingsp[i])
  wm_distribution(thisid)
  rec=wm_record(thisid)
  rec[recordnames]
}

#Pull gen and sp from regional taxa list. NAs will be investigated
genid=unique(otax[,c("genus","genusid")])
spid=unique(otax[,c("species","taxonomicStatus","speciesid")])

#Add AphiaIDs to REA
REAcode.=REAcode %>% 
  left_join(genid,by=c("GENUS_NAME"="genus"))%>% 
  left_join(spid,by=c("TAXON_NAME"="species"))


Gi=(which(is.na(REAcode.$genusid)))
Si=(which(is.na(REAcode.$speciesid)))

for(Gi_i in 1:length(Gi)){
  REAcode.$GENUS_NAME[Gi][Gi_i]
  wm_name2id()
  REAcode.$genusid[Gi][Gi_i]
}

REAcode.[Gi[1],]

###################################### PQ Codes Hierarchy
library(igraph)
library(ggraph)
library(tidygraph)
PQcode=read.csv("./NCRMP/FY26 Benthic Pipeline/Z. OBIS Taxonomy/All_Photoquad_codes_HeirarchyTrial.csv")
PQcodePC=read.csv("./NCRMP/FY26 Benthic Pipeline/Z. OBIS Taxonomy/All_Photoquad_codes_ParentChild_NoSTR.csv")

# Example: nested hierarchy of labels
# edges <- data.frame(
#   parent = c(PQcode$TIER_1,PQcode$TIER_2,PQcode$TIER_2b),
#   child  = c(PQcode$TIER_2,PQcode$TIER_2b,PQcode$TIER_3)
# ) %>% distinct()

edges <- data.frame(
  parent = c(PQcodePC$PARENT_CODE),
  child  = c(PQcodePC$CHILD_CODE)
) %>% distinct()


structurallabels=c("MASS","BR","TAB","FOL","ENC","COL","FREE")
edges %>% filter(parent%in%structurallabels|child%in%structurallabels)

g <- graph_from_data_frame(edges, directed = TRUE)
drop_sl=which(which_loop(g))

# edges.=edges[-drop_sl,]
# g. <- graph_from_data_frame(edges., directed = TRUE)

E(g.)[which_loop(g.)]

# Find strongly connected components with more than 1 node = cycle
comp <- components(g, mode = "strong")
cyclic_nodes <- V(g)[comp$membership %in% which(comp$csize > 1)]
cyclic_nodes

is_tree(g)          # TRUE if strictly hierarchical (no cross-links/cycles)
root <- V(g.)[degree(g., mode = "in") == 0]  # find root node(s)
root

tg <- as_tbl_graph(g) %>%
  mutate(depth = node_distance_from(nodes = which(degree(g, mode = "in") == 0)))
library(ggrepel)

# Extract layout coordinates as a data frame ggrepel can use
layout_df <- create_layout(tg, layout = "unrooted")
layout_df$rank=PQcodePC$CHILD_RANK[match(layout_df$name,PQcodePC$CHILD_CODE)]
layout_df$rank[is.na(layout_df$rank)]=PQcodePC$PARENT.RANK[match(layout_df$name[is.na(layout_df$rank)],
                                                                 PQcodePC$PARENT_CODE)]
ggraph(layout_df) +
  geom_edge_diagonal(color = "grey60") +
  geom_node_point(aes(color = factor(rank)), size = 2) +
  geom_text_repel(
    data = layout_df,
    aes(x = x, y = y, label = name),
    size = 2,
    max.overlaps = Inf,
    box.padding = 0.4,
    segment.color = "grey50",
    segment.size = 0.3
  ) +
  scale_color_viridis_d(option = "plasma", name = "Rank") +
  theme_void() +
  theme(legend.position = "none")




this_genus <- occurrence(taxonid = gentax$genusid )
this_genus_sp=st_as_sf(this_genus,coords = c("decimalLongitude","decimalLatitude"),crs=st_crs(4326))
gen_over=st_intersects(NCRMP_DOMAINS,this_genus_sp)
occr_i=unique(unlist(gen_over))
this_genus_sp_NCRMP=this_genus_sp[occr_i,]
unique(this_genus_sp_NCRMP$scientificName)
