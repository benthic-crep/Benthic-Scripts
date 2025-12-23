rm(list = ls())

library(dplyr)
library(stringr)

setwd("V:/ANALYSIS/CORALNET/ESA_CNet/")
old <- read.csv("RA2301_Annotations.csv")
old_as <- old[old$Island %in% c("Tutuila", "Ofu & Olosega", "Tau"),]
icra.image.list <- unique(old_as$Name[old_as$Label == "*ISSP"])
old_icra <- old_as[old_as$Name %in% icra.image.list,]

new_icra <- old_icra
new_icra$Label[which(new_icra$Label != "*ISSP" & new_icra$Label != "*ACBR")] <- "Other"
table(new_icra$Label)
new_icra <-new_icra[,c("Name","Row", "Column", "Label")]

images <- read.csv("image_list.csv") 
for (i in 1:length(icra.image.list)) {
  images.AS <- images %>% filter(grepl(icra.image.list[i], V1))
  file.copy(images.AS$V1, "V:/ANALYSIS/CORALNET/ESA_CNet/All_Images/")
}

write.csv(new_icra, "new_annotations.csv", row.names = F)

