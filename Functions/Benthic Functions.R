# reshape library inclues the cast() function used below
library(reshape2)
library(ggplot2) ## to create the diver vs diver graphs





######################################################
# function SiteNumLeadingZeros, adds leasing zeros to SITE code numeric parts to make eg OAH-1 become OAH-001
# this function therefore makes it easier to sort site names meaningfully and also removes the problem of eg MAR-22 site being treated in csv output as if it means March 22nd
# some site names have letter in the second 3 portion eg GAR-R3 .. those sites are not changed, because there re very few of those and those are generally well sorted anyway
# (and, it seems harder to work out which situations those are and how to deal with all possible variants .. therefore code just runs for situations where there are only digits in the part of the site name after the hyphen) 
#####################################################
SiteNumLeadingZeros <- function(site_names)
{	
  tmp<-levels(site_names)
  for (i in 1:length(tmp)) {
    s<-tmp[i]
    if (nchar(s)<9) {   # only change values where name length is too short ()
      ss<-strsplit(as.character(tmp[i]),"-")
      s1<-ss[[1]][1]
      s2<-ss[[1]][2]
      if (length(x=grep("[A-Z]",unlist(strsplit(toupper(s2),""))))==0) 
      {
        tmp[i]<-paste(s1, formatC(as.numeric(s2), width=5, flag="0"), sep="-")
      }
    }
  }
  levels(site_names)<-tmp
  
  return(site_names)
} #SiteNumLeadingZeros
