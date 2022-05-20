rm(list=ls())

library(raster)
library(readr)

dir = Sys.info()[7]

gpw_pop <- stack(paste0("/Users/", dir, "/Desktop/gpw_2.5min/gpw_v4_population_density_rev11_2pt5_min.nc"))

gpw_pop1 <- gpw_pop[["X1"]] #2000 pop density estimate
gpw_pop2 <- gpw_pop[["X2"]] #2005 pop density estimate
gpw_pop3 <- gpw_pop[["X3"]] #2010 pop density estimate
gpw_pop4 <- gpw_pop[["X4"]] #2015 pop density estimate
#gpw_pop5 <- gpw_pop[["X5"]] #2020 pop density estimate

gpw_pop = stack(gpw_pop1, gpw_pop2, gpw_pop3, gpw_pop4)
gpw_pop = mean(gpw_pop)

plot(log10(gpw_pop))

df = read.csv("T:/Benthic/Projects/Juvenile Project/JuvProject_SITE_weights_AllYears.csv")


df = df[complete.cases(df[,c("LONGITUDE", "LATITUDE")]), ]
df_sp = df
coordinates(df_sp) = ~LONGITUDE + LATITUDE

crs(df_sp) = crs(gpw_pop)

# bring in the EDS ExpandingExtract.R here.
source(paste0("/Users/", dir, "/Documents/GitHub/env_data_summary/scripts/HelperCode/ExpandingExtract.R"))
       
this_Ex = ExpandingExtract(gpw_pop, df_sp, Dists = c(0, 50, 100, 1000, 2000, 4000))
colnames(this_Ex)[1] = "HumanDen"
df = cbind(df, this_Ex)
nohum<-c("BAK","HOW","JAR","KIN","MAR","LIS","LAY","FFS","PHR","KUR")

df<-df %>%
  mutate(HumanDen = dplyr::case_when(ISLAND =="Wake" ~ 150/25,
                                     ISLAND =="Midway" ~ 40/25,
                                     ISLAND =="Palmyra" ~ 20/25,
                                     ISLAND =="Tau" ~ 790/25,
                                     ISLANDCODE %in% nohum ~ 0,
                                     SITE %in% c("SAI-00817","SAI-01096") ~ 650.917,
                                     SEC_NAME == "TUT_AUNUU_B" ~ 264.8486,
                                     SITE %in% c("TUT-01828","TUT-01854") ~ 462.214,
                                     TRUE ~ as.numeric(HumanDen)))

write_csv(df, file = "T:/Benthic/Projects/Juvenile Project/JuvProject_SITE_weights_AllYears_wHD.csv")
       