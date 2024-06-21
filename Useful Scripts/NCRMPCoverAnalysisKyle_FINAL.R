library(dplyr)
library(tidyr)
library(ggplot2)
library(survey)

options(dplyr.summarise.inform = FALSE)

# read in data
cover = read.csv("C:/Users/courtney.s.couch/Documents/GitHub/December-2023-StRS-Stats-Workshop/Data/BenthicCover_SITE_analysisready.csv")


# prepare data for analysis (filtered to Main Hawaiian Islands)
mhi = cover %>% rename(SECTOR = PooledSector_Viztool) %>% #change name of column to "SECTOR"
  distinct(SITEVISITID, SITE, .keep_all = TRUE) %>% #filter duplicates 
  mutate(NH = ifelse(is.na(NH) & DEPTH_BIN =="Mid", 50, #manually specify a NH value for one of the strata that didn't have NH
                      ifelse(is.na(NH),25, NH))) %>%
  group_by(ANALYSIS_YEAR, SECTOR, SEC_NAME, STRATA) %>% mutate(NH = NH / n()) %>% #calculate NH (total possible sites) by year, sector, sec name and strata
  group_by(ANALYSIS_YEAR, SECTOR, STRATA) %>% mutate(NH = sum(NH), n = n()) %>% #add up NHs and n (sites) for sectors that were pooled
  filter(REGION == "MHI") %>% #remove strata with only 1 site and just include MHI sites
  mutate(sw = NH / n, #Calculate survey weight
         STRAT_CONC = paste(ANALYSIS_YEAR, REGION, ISLAND, SECTOR, STRATA, sep = "_"), #create new column with concatenated strata variable
         ANALYSIS_YEAR = as.character(ANALYSIS_YEAR), #change to character
         SECTOR = as.character(SECTOR), #change to character
         STRATA = as.character(STRATA)) %>% #change to character
  mutate(across(c(CORAL, CCA, MA, TURF), ~./100)) %>% # convert percent variables to proportions so we can use binomial
  # remove islands with incomplete years
  group_by(ISLAND) %>% filter(n_distinct(ANALYSIS_YEAR) == 4) %>% # only include islands that have 4 survey years
  ungroup() #converts to standard dataframe

#options(survey.lonely.psu = "remove")
options(survey.lonely.psu = "adjust")

## define survey design
des = svydesign(id = ~1, strata = ~STRAT_CONC, weights = ~sw, data = mhi)
## replicate-weights survey design
# repdes = as.svrepdesign(des)

## calculate island/year means and SE 
# standard design
isl_yearmean = svyby(~CORAL, ~ISLAND + ANALYSIS_YEAR, design = des, svymean)

# replicate-weights design
# isl_yearmean = svyby(~CORAL, ~ISLAND + ANALYSIS_YEAR, design = repdes, svymean)

# plot weighted mean coral cover by island and year
ggplot(isl_yearmean, aes(x = ANALYSIS_YEAR, y = CORAL, fill = ANALYSIS_YEAR)) +
  geom_bar(stat = "identity", position = position_dodge2(preserve = "single"), width = 1, color = "black") +
  geom_errorbar(aes(ymin = CORAL - se, ymax = CORAL + se), width = 0.2) +
  facet_wrap(~ISLAND, nrow = 1) +
  guides(fill = "none") +
  theme_bw() +
  theme(panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_text(angle = -90, hjust = 0))

# fit Gaussian model
mod.1 = svyglm(CORAL ~ ISLAND * ANALYSIS_YEAR, design = des)
summary(mod.1)

#Test for significance of your fixed effects
car::Anova(mod.1, type = 3, test.statistic = "F")


# fit binomial model
mod.2 = svyglm(CORAL ~ ISLAND + ANALYSIS_YEAR, design = des, family = "binomial") # you will get an warning because it's expecting a 0/1, but it's ok the math will still work out
mod.3 = svyglm(CORAL ~ ISLAND * ANALYSIS_YEAR, design = des, family = "binomial")

# compare models (ignore eff.p and deltabar- telling you how AIC was adjusted)
AIC(mod.2, mod.3)

#guidance on AIC is that you should be using a combination of significance of predictors/likelihood ratio tests and AIC, use AIC carefully by itself

car::Anova(mod.1, type = 3, test.statistic = "F")


# ------------------------------------------------------------------------------------------------------------
# residual diagnostics

# Residuals tells you the difference between what the value was vs. what the model told you it was
# standardize by dividing by SD so you can identify certain points that were of concern

#resids = svydiags::svystdres(mod.3, stvar = "STRAT_CONC")$stdresids 
resids = scale(mod.3$residuals)
resids = (mod.3$residuals - mean(mod.3$residuals))/sd(mod.3$residuals)

#Should we be concerned about this plot? 
#In standard normal model, residuals should fall withing 2SD of mean
#Kyle, explain why we we should plot fitted residuals rather than use straight up plot(resids)
#When working with survey design don't use plot(mod.3)

plot(fitted(mod.3), resids) 
hist(resids)

#

# VIF of min and max depth
1 / (1 - with(mhi, cor(new_MIN_DEPTH_M, new_MAX_DEPTH_M, use = "pairwise.complete.obs"))^2)
# highly correlated; would only include one in model

# plot max depth vs. proportion coral cover
ggplot(mhi, aes(x = new_MAX_DEPTH_M, y = CORAL)) + geom_point() + geom_smooth() + theme_bw()
# no evidence of non-linear trend, but let's add it to the model

# add max depth as linear term
mod.3.1 = svyglm(CORAL ~ ISLAND * ANALYSIS_YEAR + new_MAX_DEPTH_M, design = des, family = "binomial")
summary(mod.3.1)
# add max depth as 2nd degree polynomial
mod.3.2 = svyglm(CORAL ~ ISLAND * ANALYSIS_YEAR + poly(new_MAX_DEPTH_M, 2, raw = TRUE), design = des, family = "binomial") 

# compare by AIC
AIC(mod.3.1, mod.3.2)
summary(mod.3.2)
# evidence of a non-linear trend in presence of other covariates that was not apparent in simple 2D plot

# increase to a 3rd degree polynomial
mod.3.3 = svyglm(CORAL ~ ISLAND * ANALYSIS_YEAR + poly(new_MAX_DEPTH_M, 3, raw = TRUE), design = des, family = "binomial")
# compare by AIC
AIC(mod.3.2, mod.3.3)
# no evidence of improved fit with 3rd degree-- we settle on 2nd degree polynomial

# summarize
anova(mod.3.2)
car::Anova(mod.3.2, type = 3, test.statistic = "F")
t(sapply(attr(mod.3.2$terms, "term.labels"), function(x) regTermTest(mod.3.2, x, method = "WorkingWald")[c("df", "ddf", "p")]))

# pseudo R-squared
jtools::summ(mod.3.2)

# planned comparisons
#Testing 2019 vs. 2010-12 in Hawaii
#hawaii 2019 vs. maui 2019
#need to identify the reference levels for each categorical list 
#Confidence intervals that overlap zero are not significant
contr = multcomp::glht(mod.3.2, linfct = c("ANALYSIS_YEAR2019 = 0",
                                           "ANALYSIS_YEAR2019 + ISLANDMaui:ANALYSIS_YEAR2019 = 0",
                                           "ANALYSIS_YEAR2019 + ISLANDKauai:ANALYSIS_YEAR2019 = 0"))
summary(contr)
round(100 * (exp(confint(contr)$confint) - 1), 1) #back transform contrasts to response scale for interpretiblity 
