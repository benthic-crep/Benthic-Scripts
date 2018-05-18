# functions

zone_ord <- c("Forereef", "Backreef", "Lagoon")

reg_name <- function(x){ # this function returns the full name of an abbreviated region
  if(x == "PRIAs"){y = "Pacific Remote Islands Areas"}
  if(x == "MARIAN"){y = "Mariana Archipelago"}
  if(x == "MHI"){y = "Main Hawaiian Islands"}
  if(x == "NWHI"){y = "Northwestern Hawaiian Islands"}
  if(x == "SAMOA"){y = "American Samoa"}
  if(x == "NMAR"){y = "northern Mariana Archipelago"}
  if(x == "SMAR"){y = "southern Mariana Archipelago"}
  
  return(y)
}

island_order <- function(x){ # this function will return a list of the island names properly ordered for figures
  PRIAorder <- c("Johnston", "Baker", "Howland", "Jarvis", "Kingman", "Palmyra")
  NMARorder <- c("FDP", "Maug", "Asuncion", "Alamagan", "Pagan", "Agrihan", "Guguan", "Sarigan")
  SMARorder <- c("Saipan", "Tinian", "Aguijan", "Rota", "Guam")
  samoaorder <- c("Ofu & Olosega", "Rose", "Swains", "Tau", "Tutuila")
  MHIorder <- c("Hawaii", "Maui", "Kahoolawe", "Lanai", "Molokai", "Oahu", "Kauai", "Niihau")
  NWHIorder <- c("Necker", "French Frigate", "Gardner", "Maro", "Laysan", "Lisianski", "Pearl & Hermes", "Midway", "Kure")
  MARIANorder <- c("FDP", "Maug", "Asuncion", "Alamagan", "Pagan", "Agrihan", "Guguan", "Sarigan", "Saipan", "Tinian", "Aguijan", "Rota", "Guam")
  
  if(x == "Pacific Remote Island Areas" | x == "PRIAs"){y = PRIAorder}
  if(x == "northern Mariana Islands" | x == "NMAR"){y = NMARorder}
  if(x == "southern Mariana Islands" | x == "SMAR"){y = SMARorder}
  if(x == "American Samoa" | x == "SAMOA"){y = samoaorder}
  if(x == "Main Hawaiian Islands" | x == "MHI"){y = MHIorder}
  if(x == "Northwestern Hawaiian Islands" | x == "NWHI"){y = NWHIorder}
  if(x == "Mariana Archipelago" | x == "MARIAN"){y = NWHIorder}
  
  
  return(y)
} 


gen_name_fun <- function(gen){ # function to return genera name given abbreviation
  
  gen <- as.character(gen)
  lookup <-  read.csv("T:/Benthic/Data/SpGen_Reference/AllGenList.csv")
  genera <- as.character(lookup[ which(lookup$Genus.code == gen),]$Genus)
  return(genera)  

}

spe_name_fun <- function(spe){ # function to return genera name given abbreviation
  
  spe <- as.character(spe)
  lookup <-  read.csv("T:/Benthic/Data/SpGen_Reference/AllSpList.csv")
  spec <- as.character(lookup[ which(lookup$Species.code == spe),]$Species)
  return(spec)  
  
}
