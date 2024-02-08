ImagePoints2SiteProportion=function(pointdata,target_category,labelset){
  pd=pointdata %>% filter(target_category %in% labelset)
  
}