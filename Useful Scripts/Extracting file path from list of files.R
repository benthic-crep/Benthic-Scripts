#This script extracts the file path from a list of file names. We use this list of file names and paths to export benthic
#images from a variety of folders into 1 folder. 


rm(list=ls())

setwd("C:/Users/Courtney.S.Couch/Documents/Courtney's Files/R Files/ESD/BIA") #set working directory for list of file names
cnet<-read.csv("ImageListCoralNetPaper.csv") #read in file

setwd("T:/Cruise/CruiseData/HA1503_MHI_RFS/Optical")   # SetWD to top of the tree where potential images are
x<-data.frame(filepath=list.files(".",pattern = ".JPG", recursive = TRUE, full.names = TRUE), name="Blank") #create a list of file paths and file names
x$filepath<-as.character(x$filepath)
x$name<-basename(x$filepath)

#Extract only the file names and paths of interest
MHI2015<-as.data.frame(x[x$name %in% cnet$Image_Name,]$filepath);colnames(MHI2015)<-"filepath" # this will remove the full file directory path

#Add working directory file path to the list of file
MHI2015$dir<-getwd()
MHI2015$filepath2<-paste(MHI2015$dir,MHI2015$filepath)
MHI2015$filepath2<-gsub(" ./","/",MHI2015$filepath2)

setwd("T:/Cruise/CruiseData/HA1602_MHI/Optical")
x<-data.frame(filepath=list.files(pattern = ".JPG", recursive = TRUE, full.names = TRUE), name="Blank") #create a list of file paths and file names
x$filepath<-as.character(x$filepath)
x$name<-basename(x$filepath)

#Extract only the file names and paths of interest
MHI201602<-as.data.frame(x[x$name %in% cnet$Image_Name,]$filepath);colnames(MHI201602)<-"filepath"
MHI201602$dir<-getwd()
MHI201602$filepath2<-paste(MHI201602$dir,MHI201602$filepath)
MHI201602$filepath2<-gsub(" ./","/",MHI201602$filepath2)


setwd("T:/Cruise/CruiseData/HA1606_MHI/Optical")
x<-data.frame(filepath=list.files(pattern = ".JPG", recursive = TRUE, full.names = TRUE), name="Blank") #create a list of file paths and file names
x$filepath<-as.character(x$filepath)
x$name<-basename(x$filepath)

#Extract only the file names and paths of interest
MHI201606<-as.data.frame(x[x$name %in% cnet$Image_Name,]$filepath);colnames(MHI201606)<-"filepath"
MHI201606$dir<-getwd()
MHI201606$filepath2<-paste(MHI201606$dir,MHI201606$filepath)
MHI201606$filepath2<-gsub(" ./","/",MHI201606$filepath2)


setwd("T:/Cruise/CruiseData/HA1501_AmSamoa/Optical")
x<-data.frame(filepath=list.files(pattern = ".JPG", recursive = TRUE, full.names = TRUE), name="Blank") #create a list of file paths and file names
x$filepath<-as.character(x$filepath)
x$name<-basename(x$filepath)

#Extract only the file names and paths of interest
SAMOA2015<-as.data.frame(x[x$name %in% cnet$Image_Name,]$filepath);colnames(SAMOA2015)<-"filepath"
SAMOA2015$dir<-getwd()
SAMOA2015$filepath2<-paste(SAMOA2015$dir,SAMOA2015$filepath)
SAMOA2015$filepath2<-gsub(" ./","/",SAMOA2015$filepath2)

all.data<-rbind(MHI2015,MHI201602,MHI201606,SAMOA2015)


#Check to make sure you have all the files of interest from original list
nrow(cnet)
nrow(all.data)

write.csv(all.data,file="C:/Users/Courtney.S.Couch/Documents/Courtney's Files/R Files/ESD/BIA/ImageListCoralNetPaper_path.csv")
