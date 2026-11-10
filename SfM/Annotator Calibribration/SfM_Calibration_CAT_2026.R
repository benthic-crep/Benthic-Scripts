# SfM Calibration using CAT output
# By Corinne
# Nov 2026
library(tidyr)
library(dplyr)
library(ggplot2)


rm(list= ls())

setwd("C:/Users/corinne.amir/Documents/GitHub/Benthic-Scripts/SfM/Annotator Calibribration/Raw Data")
raw <- read.csv("cat_export_2026-11-09.csv")
# # Auto-load prior pipeline output
# source(here("R", "01_RawtoSurvivalTransition_AGLO.R"))

# Throw out sites that are still in progress:
inprog <- c("PAL-5253","TUT-6318","PAL-5196","TUT-6325","TUT-6286")
raw <- raw %>% filter(!site %in% inprog)


# Fill in rows with missing data, update column names if needed
v2 <- raw

names(v2) <- toupper(names(v2))
v2 <- v2 %>%
          dplyr::rename(COLONYLENGTH = LINE_LENGTH_M, COLONYID = COLONY_ID) %>%
          mutate(ANALYST = recode(ANALYST,
            "LEERM" = "RML",
            "MW" = "MNW",
            "CORINNE" = "CGA",
            "MIA.LAMIRAND" = "MSL")) %>%
          select(-any_of(c("LOCALID", "CREATED_BY", "PROJECT_REGION","TYPE","PROJECT_YEAR","ANNOTATION_TIME_SECONDS")))
                     
v2$SEGAREA <- ifelse(v2$JUVENILE == "-1",1,2.5)
v2$ISLAND <- paste0(substr(v2$SITE, 1, 3))
v2$REGION <- ifelse(v2$ISLAND %in% c("TUT","ROS","TAU","OFU"),"SAMOA","PRIAs")

#### If some column classes = logical, Run this function that removes logical NAs ####
RemoveLogicalNA <- function(b)
{	
  if (is.logical(b) == "TRUE") {   
    b[is.logical(b)] <- "NA"
    b <- as.factor(b)
  }
  return(b)
}
#### end function ####

#Add "NA" to columns with class = logical (whole column filled with italized, shaded "NA")
str(raw) # no columns are logical = skip this step

raw$EXTENT_2 <- RemoveLogicalNA(raw$EXTENT_3)
raw$EXTENT_3 <- RemoveLogicalNA(raw$EXTENT_3)
raw$CON_2 <- RemoveLogicalNA(raw$CON_2)
raw$CON_3 <- RemoveLogicalNA(raw$CON_3)
raw$SEV_2 <- RemoveLogicalNA(raw$SEV_2)
raw$SEV_3 <- RemoveLogicalNA(raw$SEV_3)


# Ad hoc check for what is missing or erroneous 
# calibration segments: KIN-5192-5 (A=MNW), PAL-5261-10 (A=MSL), ROS-5365-5 (A=RML), TAU-1344-0 (A=SJD), TUT-6171-10 (A=MNW)
cal_exceptions <- tribble(
  ~SITE,       ~ANALYST,
  "TUT-6171",  "MNW",
  "ROS-5365",  "RML",
  "TAU-1344",  "SJD",
  "KIN-5192",  "MNW",
  "PAL-5261",  "MSL"
)

v2 <- v2 %>% # NOT WORKING
  left_join(cal_exceptions %>% mutate(.EXCEPT = TRUE), by = c("SITE", "ANALYST")) %>%
  mutate(TRANSECT = if_else(is.na(.EXCEPT), "B", as.character(TRANSECT))) %>%
  select(-.EXCEPT) %>%
  filter(!(SITE == "KIN-5192" & TRANSECT == "A" & ANALYST == "MNW")) %>%
  mutate(SEGMENT = case_when(
    SITE == "PAL-5261" & ANALYST == "CGA"  ~ 10, 
    SITE == "KIN-5192" & ANALYST != "MNW"  ~ 5,
    SITE == "ROS-5365" & ANALYST != "RML"  ~ 5,
    SITE == "TAU-1344" & ANALYST != "SJD"  ~ 0,
    SITE == "TUT-6171" & ANALYST != "MNW"  ~ 10,
    TRUE ~ SEGMENT)) %>%
  mutate(TRANSECT = case_when(SITE == "ROS-5365" & ANALYST == "RML" ~ "A", TRUE ~ TRANSECT)) #%>%
  # mutate(SEGMENT = case_when(
  #   SITE == "KIN-5192" & TRANSECT == "B"    ~ 5,
  #   SITE == "TUT-6171" & TRANSECT == "B"    ~ 10,
  #   SITE == "PAL-5261" & TRANSECT == "B"    ~ 10,
  #   SITE == "ROS-5365" & TRANSECT == "B"    ~ 5,
  #   SITE == "TAU-1344" & TRANSECT == "B"    ~ 0,
  #   TRUE                                    ~ SEGMENT   # everything else unchanged
  # ))

unique(v2$SPCODE) #compare species code to an existing list (add later)
unique(v2$ANALYST)

# Look at the data
seg_analysts <- v2 %>%
  group_by(SITE, TRANSECT, SEGMENT) %>%
  summarise(
    ANALYST      = paste(sort(unique(ANALYST)), collapse = ", "),
    N_ANNOTATIONS = n(),
    .groups = "drop"
  ) %>%
  arrange(SITE, TRANSECT, SEGMENT)
seg_analysts


# Separate out calibration belts and finish calibration QC
cal_incl <- tribble(
  ~SITE,       ~SEGMENT,
  "TUT-6171",  10,
  "ROS-5365",  5,
  "TAU-1344",  0,
  "KIN-5192",  5,
  "PAL-5261",  10
)
cal <- v2 %>% semi_join(cal_incl, by = c("SITE", "SEGMENT"))

# Look at the data
seg_analysts <- cal %>%
  group_by(SITE, TRANSECT, SEGMENT) %>%
  summarise(
    ANALYST      = paste(sort(unique(ANALYST)), collapse = ", "),
    N_ANNOTATIONS = n(),
    .groups = "drop"
  ) %>%
  arrange(SITE, TRANSECT, SEGMENT)
seg_analysts

lapply(cal,unique)

# Add general codes
x <- cal
source("C:/Users/Corinne.Amir/Documents/GitHub/Benthic-Scripts/Functions/Benthic_Functions_newApp_vTAOfork.R")
genlookup<-read.csv("T:/Benthic/Data/Lookup Tables/Genus_lookup.csv")
x<-CreateGenusCode(x,genlookup) 

# general RD code function not working so add from scratch
genrd <- tribble(
  ~GENRD, ~RD,
  "PRED", c("FISH","COTS","GAST","PRED"),
  "DZGN", "TLS",
  "DAMG", "DAMG"
) %>% tidyr::unnest(RD)

x <- x %>%  
  left_join(genrd %>% dplyr::rename(GENRD1 = GENRD), by = c("RDCAUSE1" = "RD")) %>%
  left_join(genrd %>% dplyr::rename(GENRD2 = GENRD), by = c("RDCAUSE2" = "RD")) %>%
  left_join(genrd %>% dplyr::rename(GENRD3 = GENRD), by = c("RDCAUSE3" = "RD"))


######### Plot Calibration ##############

#adult colony density
adult_density <- x %>%
  dplyr::filter(JUVENILE == 0) %>%
  dplyr::group_by(SITE, ANALYST) %>%
  dplyr::summarise(
    N_ADULTS = dplyr::n(),
    AREA_M2  = dplyr::n_distinct(SEGMENT) * mean(SEGAREA),
    .groups  = "drop"
  ) %>%
  dplyr::mutate(
    DENSITY = N_ADULTS / AREA_M2,
    SE      = sqrt(N_ADULTS) / AREA_M2    # Poisson counting error
  )

adult_density_tot <- adult_density %>%
  dplyr::group_by(ANALYST) %>%
  dplyr::summarise(
    N_SITES = dplyr::n(),
    MEAN    = mean(DENSITY),
    SE      = sd(DENSITY) / sqrt(dplyr::n()),
    .groups = "drop"
  )

p1 <- ggplot(adult_density, aes(ANALYST, DENSITY, fill = ANALYST)) +
  geom_col() +
  geom_errorbar(aes(ymin = DENSITY - SE, ymax = DENSITY + SE), width = 0.2) +
  facet_wrap(~SITE) +
  labs(x = NULL, y = "Adult colonies per m²",
       title = "Adult colony density by annotator") +
  theme_bw() +
  theme(legend.position = "none")
p1

p2 <- ggplot(adult_density_tot, aes(ANALYST, MEAN, fill = ANALYST)) +
  geom_col() +
  geom_errorbar(aes(ymin = MEAN - SE, ymax = MEAN + SE), width = 0.2) +
  labs(x = NULL, y = "Adult colonies per m²",
       title = "Adult colony density by annotator") +
  theme_bw() +
  theme(legend.position = "none")
p2



#juvenile colony density
juv_density <- x %>%
  dplyr::filter(JUVENILE == -1) %>%
  dplyr::group_by(SITE, ANALYST) %>%
  dplyr::summarise(
    N_ADULTS = dplyr::n(),
    AREA_M2  = dplyr::n_distinct(SEGMENT) * mean(SEGAREA),
    .groups  = "drop"
  ) %>%
  dplyr::mutate(
    DENSITY = N_ADULTS / AREA_M2,
    SE      = sqrt(N_ADULTS) / AREA_M2    # Poisson counting error
  )

juv_density_tot <- juv_density %>%
  dplyr::group_by(ANALYST) %>%
  dplyr::summarise(
    N_SITES = dplyr::n(),
    MEAN    = mean(DENSITY),
    SE      = sd(DENSITY) / sqrt(dplyr::n()),
    .groups = "drop"
  )

p1 <- ggplot(juv_density, aes(ANALYST, DENSITY, fill = ANALYST)) +
  geom_col() +
  geom_errorbar(aes(ymin = DENSITY - SE, ymax = DENSITY + SE), width = 0.2) +
  facet_wrap(~SITE) +
  labs(x = NULL, y = "Juvenile colonies per m²",
       title = "Juvenile colony density by annotator") +
  theme_bw() +
  theme(legend.position = "none")
p1

p2 <- ggplot(juv_density_tot, aes(ANALYST, MEAN, fill = ANALYST)) +
  geom_col() +
  geom_errorbar(aes(ymin = MEAN - SE, ymax = MEAN + SE), width = 0.2) +
  labs(x = NULL, y = "Juvenile colonies per m²",
       title = "Juvenile colony density by annotator") +
  theme_bw() +
  theme(legend.position = "none")
p2


#average length
mean_length <- x %>%
  dplyr::group_by(SITE, ANALYST) %>%
  dplyr::summarise(MEAN = (mean(COLONYLENGTH))*100,
                   SE   = sd(COLONYLENGTH*100) / sqrt(dplyr::n()),
                   .groups = "drop")

mean_length_tot <- x %>%
  dplyr::group_by(ANALYST) %>%
  dplyr::summarise(MEAN = (mean(COLONYLENGTH))*100,
                   SE   = sd(COLONYLENGTH*100) / sqrt(dplyr::n()),
                   .groups = "drop")

p3 <- ggplot(mean_length, aes(ANALYST, MEAN, fill = ANALYST)) +
  geom_col() +
  geom_errorbar(aes(ymin = MEAN - SE, ymax = MEAN + SE), width = 0.2) +
  facet_wrap(~SITE) +
  labs(x = NULL, y = "Mean colony length (cm, mean ± SE)",
       title = "Mean colony size by annotator and site") +
  theme_bw() +
  theme(legend.position = "none")
p3

p4 <- ggplot(mean_length_tot, aes(ANALYST, MEAN, fill = ANALYST)) +
  geom_col() +
  geom_errorbar(aes(ymin = MEAN - SE, ymax = MEAN + SE), width = 0.2) +
  labs(x = NULL, y = "Mean colony length (cm, mean ± SE)",
       title = "Mean colony size") +
  theme_bw() +
  theme(legend.position = "none")
p4



#average old dead
avg_od <- x %>%
  dplyr::group_by(SITE, ANALYST) %>%
  dplyr::summarise(MEAN = mean(OLD_DEAD),
                   SE   = sd(COLONYLENGTH) / sqrt(dplyr::n()),
                   .groups = "drop")

avg_od_tot <- x %>%
  dplyr::group_by(ANALYST) %>%
  dplyr::summarise(MEAN = mean(COLONYLENGTH),
                   SE   = sd(COLONYLENGTH) / sqrt(dplyr::n()),
                   .groups = "drop")

p5 <- ggplot(mean_length, aes(ANALYST, MEAN, fill = ANALYST)) +
  geom_col() +
  geom_errorbar(aes(ymin = MEAN - SE, ymax = MEAN + SE), width = 0.2) +
  facet_wrap(~SITE) +
  labs(x = NULL, y = "percent",
       title = "Mean colony size by annotator and site") +
  theme_bw() +
  theme(legend.position = "none")
p5

p6 <- ggplot(mean_length_tot, aes(ANALYST, MEAN, fill = ANALYST)) +
  geom_col() +
  geom_errorbar(aes(ymin = MEAN - SE, ymax = MEAN + SE), width = 0.3) +
  labs(x = NULL, y = "percent",
       title = "Average old dead") +
  theme_bw() +
  theme(legend.position = "none")
p6

# genrd occurence
grd_counts <- x %>%
  dplyr::select(SITE, ANALYST, GENRD1, GENRD2, GENRD3) %>%
  dplyr::mutate(dplyr::across(dplyr::starts_with("GENRD"), as.character)) %>%
  tidyr::pivot_longer(dplyr::starts_with("GENRD"),
                      names_to = "SLOT", values_to = "GENRD") %>%
  dplyr::filter(!is.na(GENRD), GENRD != "") %>%
  dplyr::count(SITE, ANALYST, GENRD, name = "N") %>%
  filter(GENRD != "NONE")

p7 <- ggplot(grd_counts, aes(ANALYST, N, fill = GENRD)) +
  geom_col(position = position_dodge(preserve = "single")) +
  facet_wrap(~SITE) +
  labs(x = "Recent mortality cause", y = "Number of occurrences",
       fill = "GENRD",
       title = "Recent mortality causes recorded by annotator and site") +
  theme_bw()
p7


#genrd mean percentage
genrd_mean <- x %>%
  dplyr::select(SITE, ANALYST, GENRD1, GENRD2, GENRD3, RD_1, RD_2, RD_3) %>%
  dplyr::rename(RDVAL1 = RD_1, RDVAL2 = RD_2, RDVAL3 = RD_3) %>%
  dplyr::mutate(dplyr::across(dplyr::starts_with("GENRD"), as.character),
                dplyr::across(dplyr::starts_with("RDVAL"), as.numeric)) %>%
  tidyr::pivot_longer(
    cols = c(dplyr::starts_with("GENRD"), dplyr::starts_with("RDVAL")),
    names_to = c(".value", "SLOT"),
    names_pattern = "(GENRD|RDVAL)(\\d)"
  ) %>%
  dplyr::filter(!is.na(GENRD), GENRD != "", !is.na(RDVAL)) %>%
  dplyr::group_by(SITE, ANALYST, GENRD) %>%
  dplyr::summarise(N       = dplyr::n(),
                   MEAN_RD = mean(RDVAL),
                   SE      = sd(RDVAL) / sqrt(dplyr::n()),
                   .groups = "drop")

p7 <- ggplot(genrd_mean, aes(ANALYST, MEAN_RD, fill = GENRD)) +
  geom_col(position = position_dodge(width = 0.9, preserve = "single")) +
  geom_errorbar(aes(ymin = MEAN_RD - SE, ymax = MEAN_RD + SE),
                position = position_dodge(width = 0.9, preserve = "single"),
                width = 0.2, na.rm = TRUE) +
  facet_wrap(~SITE, scales = "free_y") +
  labs(x = "", y = "Mean % (± SE)",
       fill = "Annotator",
       title = "Mean recent dead by general category") +
  theme_bw()
p7

# predation prevalence 
grd_counts <- x %>%
  filter(GENRD1 == "PRED" | GENRD2 == "PRED" | GENRD3 == "PRED") %>%
  dplyr::select(SITE, ANALYST, RDCAUSE1, RDCAUSE2, RDCAUSE3) %>%
  dplyr::mutate(dplyr::across(dplyr::starts_with("RDCAUSE"), as.character)) %>%
  tidyr::pivot_longer(dplyr::starts_with("RDCAUSE"),
                      names_to = "SLOT", values_to = "RDCAUSE") %>%
  dplyr::filter(!is.na(RDCAUSE), RDCAUSE != "") %>%
  dplyr::count(SITE, ANALYST, RDCAUSE, name = "N") %>%
  filter(RDCAUSE != "NONE" | RDCAUSE == "DAMG")

p <- ggplot(grd_counts, aes(ANALYST, N, fill = RDCAUSE)) +
  geom_col(position = position_dodge(preserve = "single")) +
  facet_wrap(~SITE) +
  labs(x = "", y = "Number of occurrences",
       fill = "RDCAUSE",
       title = "Predation Occurrences recorded by annotator and site") +
  theme_bw()
p

p8 <- ggplot(grd_counts, aes(ANALYST, N, fill = RDCAUSE)) +
  stat_summary(fun = mean, geom = "col", position = position_dodge(width = 0.9)) +
  stat_summary(fun.data = mean_se, geom = "errorbar",
               position = position_dodge(width = 0.9), width = 0.2) +
  labs(x = "", y = "Number of occurrences",
       fill = "RDCAUSE",
       title = "Predation Occurrences recorded by annotator and site") +
  theme_bw()
p8


# predation mean percentage
grd_counts <- x %>%
  filter(GENRD1 == "PRED" | GENRD2 == "PRED" | GENRD3 == "PRED") %>%
  dplyr::select(SITE, ANALYST, RDCAUSE1, RDCAUSE2, RDCAUSE3) %>%
  dplyr::mutate(dplyr::across(dplyr::starts_with("RDCAUSE"), as.character)) %>%
  tidyr::pivot_longer(dplyr::starts_with("RDCAUSE"),
                      names_to = "SLOT", values_to = "RDCAUSE") %>%
  dplyr::filter(!is.na(RDCAUSE), RDCAUSE != "") %>%
  dplyr::count(SITE, ANALYST, RDCAUSE, name = "N") %>%
  filter(RDCAUSE != "NONE" | RDCAUSE == "DAMG")

p7 <- ggplot(grd_counts, aes(ANALYST, N, fill = RDCAUSE)) +
  geom_col(position = position_dodge(preserve = "single")) +
  facet_wrap(~SITE) +
  labs(x = "", y = "Number of occurrences",
       fill = "RDCAUSE",
       title = "Predation Occurrences recorded by annotator and site") +
  theme_bw()
p7


# bleaching prevalence

#Species diversity 

# genus diversity


