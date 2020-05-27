# rFIA demo code, exploring rFIA package
# rFIA package documentation: https://rfia.netlify.app/#about

#install.packages("rFIA")

library(rFIA)
library(tidyverse)

# No vignette, look at example usage on this site: https://rfia.netlify.app/#about

data(fiaRI)

class(fiaRI) # Of class "FIA.Database", mode "list" - 19 slots within the list

str(fiaRI)

names(fiaRI)  # Print the names of the tables in the FIA.Database object


#-----------------------------
# Example from https://rfia.netlify.app/courses/plt_est/

############################################
## Calculate attributes at the plot level ##
############################################

data(fiaRI)

# Unique IDs for individual plots

a <- 168263223020004  # One forested condition
b <- 168263219020004  # Two forested, one non-forested condition

# Subset the PLOT, COND, and TREE tables for plot A
plot_a <- filter(fiaRI$PLOT, CN == a)
cond_a <- filter(fiaRI$COND, PLT_CN == a)
tree_a <- filter(fiaRI$TREE, PLT_CN == a)

# Subset the PLOT, COND, and TREE tables for plot B
plot_b <- filter(fiaRI$PLOT, CN == b)
cond_b <- filter(fiaRI$COND, PLT_CN == b)
tree_b <- filter(fiaRI$TREE, PLT_CN == b)

## COND_STATUS_CD indicates the basic land classification of a 
## condition, whether it is forested, non-forest, water, etc...
## COND_STATUS_CD = 1 indicates forested

cond_a$COND_STATUS_CD

cond_b$COND_STATUS_CD

# FORTYPCD indicates the forest type of the condition
# PHYSCLCD indicates the Physiographic class (e.g. mesic moist slope)

cond_b$FORTYPCD

cond_b$PHYSCLCD

# Basic estimation procedures

tbl_a <- plot_a %>%
  mutate(PLT_CN = CN) %>%
  left_join(cond_a, by = 'PLT_CN') %>%
  left_join(tree_a, by = c('PLT_CN', 'CONDID'))
    
tbl_b <- plot_b %>%
  mutate(PLT_CN = CN) %>%
  left_join(cond_b, by = 'PLT_CN') %>%
  left_join(tree_b, by = c('PLT_CN', 'CONDID'))



# Calculate AGB and AGC per acre, divide by 2000 to convert lb/acre to tons/acre

# Plot A
all_a <- tbl_a %>%
  group_by(PLT_CN) %>%
  summarize(BIO_AG_ACRE = sum(DRYBIO_AG * TPA_UNADJ / 2000, na.rm = TRUE),
            CARB_AG_ACRE = sum(CARBON_AG * TPA_UNADJ / 2000, na.rm = TRUE))
# Plot B
all_b <- tbl_b %>%
  group_by(PLT_CN) %>%
  summarize(BIO_AG_ACRE = sum(DRYBIO_AG * TPA_UNADJ / 2000, na.rm = TRUE),
            CARB_AG_ACRE = sum(CARBON_AG * TPA_UNADJ / 2000, na.rm = TRUE))

# Can use biomass convenience function to produce same result

rFIA_all <- biomass(fiaRI, byPlot = TRUE, treeType = "all")

filter(rFIA_all, PLT_CN == a)

#----------------
# Domain Indicators - Vectors that indicate whether a tree
# (or plot, condition, etc.) is within our domain of interest.

tbl_a <- tbl_a %>%
  mutate(tDI = if_else(SPCD == 833 & DIA > 12, 1, 0))

tbl_a$tDI

# Count number of trees that meet the criteria

sum(tbl_a$tDI, na.rm = T)


tbl_b <- tbl_b %>%
  mutate(tDI = if_else(SPCD == 833 & DIA > 12, 1, 0))

tbl_b$tDI

sum(tbl_b$tDI, na.rm = T)

# We can use our domain indicator to produce estimates for any tpes of tree

ro12_a <- tbl_a %>%
  group_by(PLT_CN) %>%
  summarize(BIO_AG_ACRE = sum(DRYBIO_AG * TPA_UNADJ * tDI / 2000, na.rm = TRUE),
            CARB_AG_ACRE = sum(CARBON_AG * TPA_UNADJ * tDI / 2000, na.rm = TRUE))

ro12_b <- tbl_b %>%
  group_by(PLT_CN) %>%
  summarize(BIO_AG_ACRE = sum(DRYBIO_AG * TPA_UNADJ * tDI / 2000, na.rm = TRUE),
            CARB_AG_ACRE = sum(CARBON_AG * TPA_UNADJ * tDI / 2000, na.rm = TRUE))

# Checking against rFIA

rFIA_ro12 <- biomass(fiaRI, byPlot = TRUE, treeType = 'all', treeDomain = SPCD == 833 & DIA > 12)

ro12_a
filter(rFIA_ro12, PLT_CN == a)

ro12_b
filter(rFIA_ro12, PLT_CN == b)

# Grouping by other attributes contained within the FIA Database, e.g., forest type

for_a <- tbl_a %>%
  group_by(PLT_CN, FORTYPCD) %>%
  summarize(BIO_AG_ACRE = sum(DRYBIO_AG * TPA_UNADJ / 2000, na.rm = TRUE),
            CARB_AG_ACRE = sum(CARBON_AG * TPA_UNADJ / 2000, na.rm = TRUE))

for_b <- tbl_b %>%
  group_by(PLT_CN, FORTYPCD) %>%
  summarize(BIO_AG_ACRE = sum(DRYBIO_AG * TPA_UNADJ / 2000, na.rm = TRUE),
            CARB_AG_ACRE = sum(CARBON_AG * TPA_UNADJ / 2000, na.rm = TRUE))

rFIA_for <- biomass(fiaRI, byPlot = TRUE, treeType = 'all', grpBy = FORTYPCD)

filter(rFIA_for, PLT_CN == a) 

for_a

#--------------------------------------------------------------

################################
## Population level estimates ##
################################

data(fiaRI)
db <- fiaRI

ids <- db$POP_EVAL %>%
  select('CN', 'END_INVYR', 'EVALID') %>%
  inner_join(select(db$POP_EVAL_TYP, c('EVAL_CN', 'EVAL_TYP')), by = c('CN' = 'EVAL_CN')) %>%
  filter(EVAL_TYP %in% c('EXPCURR', 'EXPVOL'))

db <- clipFIA(db, evalid = ids$EVALID)

## Select only the columns we need from each table, to keep things slim
PLOT <- select(db$PLOT, CN, MACRO_BREAKPOINT_DIA)
COND <- select(db$COND, PLT_CN, CONDID, CONDPROP_UNADJ, PROP_BASIS, COND_STATUS_CD, OWNGRPCD)
TREE <- select(db$TREE, PLT_CN, CONDID, SUBP, TREE, STATUSCD, DRYBIO_AG, CARBON_AG, TPA_UNADJ, DIA, SPCD)
POP_ESTN_UNIT <- select(db$POP_ESTN_UNIT, CN, EVAL_CN, AREA_USED, P1PNTCNT_EU)
POP_EVAL <- select(db$POP_EVAL, EVALID, EVAL_GRP_CN, ESTN_METHOD, CN, END_INVYR, REPORT_YEAR_NM)
POP_EVAL_TYP <- select(db$POP_EVAL_TYP, EVAL_TYP, EVAL_CN)
POP_PLOT_STRATUM_ASSGN <- select(db$POP_PLOT_STRATUM_ASSGN, STRATUM_CN, PLT_CN)
POP_STRATUM <- select(db$POP_STRATUM, ESTN_UNIT_CN, EXPNS, P2POINTCNT, 
                      ADJ_FACTOR_MICR, ADJ_FACTOR_SUBP, ADJ_FACTOR_MACR, CN, P1POINTCNT)

## Join the tables
data <- PLOT %>%
  mutate(PLT_CN = CN) %>%
  left_join(COND, by = 'PLT_CN') %>%
  left_join(TREE, by = c('PLT_CN', 'CONDID')) %>%
  left_join(POP_PLOT_STRATUM_ASSGN, by = 'PLT_CN') %>%
  left_join(POP_STRATUM, by = c('STRATUM_CN' = 'CN')) %>%
  left_join(POP_ESTN_UNIT, by = c('ESTN_UNIT_CN' = 'CN')) %>%
  left_join(POP_EVAL, by = c('EVAL_CN' = 'CN')) %>%
  left_join(POP_EVAL_TYP, by = 'EVAL_CN')

data <- data %>%
  mutate(
    ## AREA
    aAdj = case_when(
      ## When NA, stay NA
      is.na(PROP_BASIS) ~ NA_real_,
      ## If the proportion was measured for a macroplot,
      ## use the macroplot value
      PROP_BASIS == 'MACR' ~ as.numeric(ADJ_FACTOR_MACR),
      ## Otherwise, use the subpplot value
      PROP_BASIS == 'SUBP' ~ ADJ_FACTOR_SUBP),
    ## TREE
    tAdj = case_when(
      ## When DIA is na, adjustment is NA
      is.na(DIA) ~ ADJ_FACTOR_SUBP,
      ## When DIA is less than 5", use microplot value
      DIA < 5 ~ ADJ_FACTOR_MICR,
      ## When DIA is greater than 5", use subplot value
      DIA >= 5 ~ ADJ_FACTOR_SUBP
    ))

## Build a domain indicator for land type and live trees
## Land type (all forested area)
data$aDI <- if_else(data$COND_STATUS_CD == 1, 1, 0)
## Live trees only (on forested area)
data$tDI <- if_else(data$STATUSCD == 1, 1, 0) * data$aDI

## Now, let's just rename the END_INVYR column to 'YEAR'
## for a pretty output like rFIA
data <- data %>%
  mutate(YEAR = END_INVYR) %>%
  ## remove any NAs
  filter(!is.na(YEAR))


#--------------------------

## Estimate Tree totals
tre_bio <- data %>%
  filter(EVAL_TYP == 'EXPVOL') %>%
  ## Make sure we only have unique observations of plots, trees, etc.
  distinct(ESTN_UNIT_CN, STRATUM_CN, PLT_CN, CONDID, SUBP, TREE, .keep_all = TRUE) %>%
  ## Plot-level estimates first (multiplying by EXPNS here)
  group_by(YEAR, ESTN_UNIT_CN, ESTN_METHOD, STRATUM_CN, PLT_CN) %>%
  summarize(bioPlot = sum(DRYBIO_AG * TPA_UNADJ * tAdj * tDI * EXPNS  / 2000, na.rm = TRUE),
            carbPlot = sum(CARBON_AG * TPA_UNADJ * tAdj * tDI * EXPNS  / 2000, na.rm = TRUE)) %>%
  ## Now we simply sum the values of each plot (expanded w/ EXPNS)
  ## to obtain population totals
  group_by(YEAR) %>%
  summarize(BIO_AG_TOTAL = sum(bioPlot, na.rm = TRUE),
            CARB_AG_TOTAL = sum(carbPlot, na.rm = TRUE))

## Estimate Area totals
area_bio <- data %>%
  filter(EVAL_TYP == 'EXPCURR') %>%
  ## Make sure we only have unique observations of plots, trees, etc.
  distinct(ESTN_UNIT_CN, STRATUM_CN, PLT_CN, CONDID, .keep_all = TRUE) %>%
  ## Plot-level estimates first (multiplying by EXPNS here)
  group_by(YEAR, ESTN_UNIT_CN, ESTN_METHOD, STRATUM_CN, PLT_CN) %>%
  summarize(forArea = sum(CONDPROP_UNADJ * aAdj * aDI * EXPNS, na.rm = TRUE)) %>%
  ## Now we simply sum the values of each plot (expanded w/ EXPNS)
  ## to obtain population totals
  group_by(YEAR) %>%
  summarize(AREA_TOTAL = sum(forArea, na.rm = TRUE))

bio <- left_join(tre_bio, area_bio) %>%
  mutate(BIO_AG_ACRE = BIO_AG_TOTAL / AREA_TOTAL,
         CARB_AG_ACRE = CARB_AG_TOTAL / AREA_TOTAL) %>%
  ## Reordering the columns
  select(YEAR, BIO_AG_ACRE, CARB_AG_ACRE, BIO_AG_TOTAL, CARB_AG_TOTAL, AREA_TOTAL)

biomass(clipFIA(fiaRI), totals = TRUE)

# Adding grouping variables

tre_bioGrp <- data %>%
  filter(EVAL_TYP == 'EXPVOL') %>%
  ## Make sure we only have unique observations of plots, trees, etc.
  distinct(ESTN_UNIT_CN, STRATUM_CN, PLT_CN, CONDID, SUBP, TREE, .keep_all = TRUE) %>%
  ## Plot-level estimates first (multiplying by EXPNS here)
  group_by(YEAR, OWNGRPCD, ESTN_UNIT_CN, ESTN_METHOD, STRATUM_CN, PLT_CN) %>%
  summarize(bioPlot = sum(DRYBIO_AG * TPA_UNADJ * tAdj * tDI * EXPNS  / 2000, na.rm = TRUE),
            carbPlot = sum(CARBON_AG * TPA_UNADJ * tAdj * tDI * EXPNS  / 2000, na.rm = TRUE)) %>%
  ## Now we simply sum the values of each plot (expanded w/ EXPNS)
  ## to obtain population totals
  group_by(YEAR, OWNGRPCD) %>%
  summarize(BIO_AG_TOTAL = sum(bioPlot, na.rm = TRUE),
            CARB_AG_TOTAL = sum(carbPlot, na.rm = TRUE))

## Estimate Area totals
area_bioGrp <- data %>%
  filter(EVAL_TYP == 'EXPCURR') %>%
  ## Make sure we only have unique observations of plots, trees, etc.
  distinct(ESTN_UNIT_CN, STRATUM_CN, PLT_CN, CONDID, .keep_all = TRUE) %>%
  ## Plot-level estimates first (multiplying by EXPNS here)
  group_by(YEAR, OWNGRPCD, ESTN_UNIT_CN, ESTN_METHOD, STRATUM_CN, PLT_CN) %>%
  summarize(forArea = sum(CONDPROP_UNADJ * aAdj * aDI * EXPNS, na.rm = TRUE)) %>%
  ## Now we simply sum the values of each plot (expanded w/ EXPNS)
  ## to obtain population totals
  group_by(YEAR, OWNGRPCD) %>%
  summarize(AREA_TOTAL = sum(forArea, na.rm = TRUE))

## Now we can simply join these two up, and produce ratio estimates
bioGrp <- left_join(tre_bioGrp, area_bioGrp) %>%
  mutate(BIO_AG_ACRE = BIO_AG_TOTAL / AREA_TOTAL,
         CARB_AG_ACRE = CARB_AG_TOTAL / AREA_TOTAL) %>%
  ## Reordering the columns
  select(YEAR, OWNGRPCD, BIO_AG_ACRE, CARB_AG_ACRE, BIO_AG_TOTAL, CARB_AG_TOTAL, AREA_TOTAL)

biomass(clipFIA(fiaRI), totals = TRUE, grpBy = OWNGRPCD)


# Estimating with Sampling Errors

unique(db$POP_EVAL$ESTN_METHOD)

## Estimate Tree attributes -- plot level
tre_pop <- data %>%
  filter(EVAL_TYP == 'EXPVOL') %>%
  ## Make sure we only have unique observations of plots, trees, etc.
  distinct(ESTN_UNIT_CN, STRATUM_CN, PLT_CN, CONDID, SUBP, TREE, .keep_all = TRUE) %>%
  ## Plot-level estimates first (note we are NOT using EXPNS here)
  group_by(YEAR, ESTN_UNIT_CN, STRATUM_CN, PLT_CN) %>%
  summarize(bioPlot = sum(DRYBIO_AG * TPA_UNADJ * tAdj * tDI  / 2000, na.rm = TRUE),
            carbPlot = sum(CARBON_AG * TPA_UNADJ * tAdj * tDI  / 2000, na.rm = TRUE)) 

## Estimate Area attributes -- plot level
area_pop <- data %>%
  filter(EVAL_TYP == 'EXPCURR') %>%
  ## Make sure we only have unique observations of plots, trees, etc.
  distinct(ESTN_UNIT_CN, STRATUM_CN, PLT_CN, CONDID, .keep_all = TRUE) %>%
  ## Plot-level estimates first (multiplying by EXPNS here)
  ## Extra grouping variables are only here so they are carried through
  group_by(YEAR, P1POINTCNT, P1PNTCNT_EU, P2POINTCNT, AREA_USED, ESTN_UNIT_CN, STRATUM_CN, PLT_CN) %>%
  summarize(forArea = sum(CONDPROP_UNADJ * aAdj * aDI, na.rm = TRUE))

## Joining the two tables
bio_pop_plot <- left_join(tre_pop, area_pop)

## Strata level
bio_pop_strat <- bio_pop_plot %>%
  group_by(YEAR, ESTN_UNIT_CN, STRATUM_CN) %>%
  summarize(aStrat = mean(forArea, na.rm = TRUE), # Area mean
            bioStrat = mean(bioPlot, na.rm = TRUE), # Biomass mean
            carbStrat = mean(carbPlot, na.rm = TRUE), # Carbon mean
            ## We don't want a vector of these values, since they are repeated
            P2POINTCNT = first(P2POINTCNT),
            AREA_USED = first(AREA_USED),
            ## Strata weight, relative to estimation unit
            w = first(P1POINTCNT) / first(P1PNTCNT_EU),
            ## Strata level variances
            aVar = (sum(forArea^2) - sum(P2POINTCNT * aStrat^2)) / (P2POINTCNT * (P2POINTCNT-1)),
            bioVar = (sum(bioPlot^2) - sum(P2POINTCNT * bioStrat^2)) / (P2POINTCNT * (P2POINTCNT-1)),
            carbVar = (sum(carbPlot^2) - sum(P2POINTCNT * carbStrat^2)) / (P2POINTCNT * (P2POINTCNT-1)),
            ## Strata level co-varainces
            bioCV = (sum(forArea*bioPlot) - sum(P2POINTCNT * aStrat *bioStrat)) / (P2POINTCNT * (P2POINTCNT-1)),
            carbCV = (sum(forArea*carbPlot) - sum(P2POINTCNT * aStrat *carbStrat)) / (P2POINTCNT * (P2POINTCNT-1)))

## Moving on to the estimation unit
bio_pop_eu <- bio_pop_strat %>%
  group_by(YEAR, ESTN_UNIT_CN) %>%
  summarize(aEst = sum(aStrat * w, na.rm = TRUE) * first(AREA_USED), # Mean Area
            bioEst = sum(bioStrat * w, na.rm = TRUE) * first(AREA_USED), # Mean biomass
            carbEst = sum(carbStrat * w, na.rm = TRUE) * first(AREA_USED), # Mean carbon
            ## Estimation unit variances
            aVar = (first(AREA_USED)^2 / sum(P2POINTCNT)) * 
              (sum(P2POINTCNT*w*aVar) + sum((1-w)*(P2POINTCNT/sum(P2POINTCNT))*aVar)),
            bioVar = (first(AREA_USED)^2 / sum(P2POINTCNT)) * 
              (sum(P2POINTCNT*w*bioVar) + sum((1-w)*(P2POINTCNT/sum(P2POINTCNT))*bioVar)),
            carbVar = (first(AREA_USED)^2 / sum(P2POINTCNT)) * 
              (sum(P2POINTCNT*w*carbVar) + sum((1-w)*(P2POINTCNT/sum(P2POINTCNT))*carbVar)),
            ## Estimation unit covariances
            bioCV = (first(AREA_USED)^2 / sum(P2POINTCNT)) * 
              (sum(P2POINTCNT*w*bioCV) + sum((1-w)*(P2POINTCNT/sum(P2POINTCNT))*bioCV)),
            carbCV = (first(AREA_USED)^2 / sum(P2POINTCNT)) * 
              (sum(P2POINTCNT*w*carbCV) + sum((1-w)*(P2POINTCNT/sum(P2POINTCNT))*carbCV)))

## sum across Estimation Units for totals
bio_pop <- bio_pop_eu %>%
  group_by(YEAR) %>%
  summarize(AREA_TOTAL = sum(aEst, na.rm = TRUE),
            BIO_AG_TOTAL = sum(bioEst, na.rm = TRUE),
            CARB_AG_TOTAL = sum(carbEst, na.rm = TRUE),
            ## Ratios
            BIO_AG_ACRE = BIO_AG_TOTAL / AREA_TOTAL,
            CARB_AG_ACRE = CARB_AG_TOTAL / AREA_TOTAL,
            ## Total samping errors
            AREA_TOTAL_SE = sqrt(sum(aVar, na.rm = TRUE)) / AREA_TOTAL * 100,
            BIO_AG_TOTAL_SE = sqrt(sum(bioVar, na.rm = TRUE)) / BIO_AG_TOTAL * 100, 
            CARB_AG_TOTAL_SE = sqrt(sum(carbVar, na.rm = TRUE)) / CARB_AG_TOTAL * 100,
            ## Ratio variances
            bioAcreVar = (1 / AREA_TOTAL^2) * (sum(bioVar) + (BIO_AG_ACRE^2)*sum(aVar) - 2*BIO_AG_ACRE*sum(bioCV)),
            carbAcreVar = (1 / AREA_TOTAL^2) * (sum(carbVar) + (CARB_AG_ACRE^2)*sum(aVar) - 2*CARB_AG_ACRE*sum(carbCV)),
            BIO_AG_ACRE_SE = sqrt(sum(bioAcreVar, na.rm = TRUE)) / BIO_AG_ACRE * 100,
            CARB_AG_ACRE_SE = sqrt(sum(carbAcreVar, na.rm = TRUE)) / CARB_AG_ACRE * 100) %>%
  ## Re ordering, dropping variance
  select(YEAR, BIO_AG_ACRE, CARB_AG_ACRE, BIO_AG_TOTAL, CARB_AG_TOTAL, AREA_TOTAL, BIO_AG_ACRE_SE, 
         CARB_AG_ACRE_SE, BIO_AG_TOTAL_SE, CARB_AG_TOTAL_SE, AREA_TOTAL_SE)

# Estimating with sampling errors by grouping variables

## All groups we want to estimate 
grps <- data %>%
  group_by(YEAR, SPCD) %>%
  summarize()

tDI <- data$tDI
aDI <- data$aDI

bio_pop <- list()

for (i in 1:nrow(grps)){
  
  ## Tree domain indicator
  data$tDI <- tDI * if_else(data$SPCD == grps$SPCD[i], 1, 0) * if_else(data$YEAR == grps$YEAR[i], 1, 0)
  ## No need to modify the area domain indicator for SPCD, because
  ## we want to estimate all forested area within the year
  data$aDI <- aDI * if_else(data$YEAR == grps$YEAR[i], 1, 0)
  
  ## SAME AS ABOVE, JUST IN A LOOP NOW --> SIMILAR TO USING GROUP_BY
  ## Estimate Area attributes -- plot level
  tre_pop <- data %>%
    filter(EVAL_TYP == 'EXPVOL') %>%
    ## Make sure we only have unique observations of plots, trees, etc.
    distinct(ESTN_UNIT_CN, STRATUM_CN, PLT_CN, CONDID, SUBP, TREE, .keep_all = TRUE) %>%
    ## Plot-level estimates first (note we are NOT using EXPNS here)
    group_by(ESTN_UNIT_CN, STRATUM_CN, PLT_CN) %>%
    summarize(bioPlot = sum(DRYBIO_AG * TPA_UNADJ * tAdj * tDI  / 2000, na.rm = TRUE),
              carbPlot = sum(CARBON_AG * TPA_UNADJ * tAdj * tDI  / 2000, na.rm = TRUE)) 
  
  ## Estimate Area attributes -- plot level
  area_pop <- data %>%
    filter(EVAL_TYP == 'EXPCURR') %>%
    ## Make sure we only have unique observations of plots, trees, etc.
    distinct(ESTN_UNIT_CN, STRATUM_CN, PLT_CN, CONDID, .keep_all = TRUE) %>%
    ## Plot-level estimates first (multiplying by EXPNS here)
    ## Extra grouping variables are only here so they are carried through
    group_by(P1POINTCNT, P1PNTCNT_EU, P2POINTCNT, AREA_USED, ESTN_UNIT_CN, STRATUM_CN, PLT_CN) %>%
    summarize(forArea = sum(CONDPROP_UNADJ * aAdj * aDI, na.rm = TRUE))
  
  ## Joining the two tables
  bio_pop_plot <- left_join(tre_pop, area_pop)
  
  ## Now we can follow through with the remaining estimation procedures
  ## Strata level
  bio_pop_strat <- bio_pop_plot %>%
    group_by(ESTN_UNIT_CN, STRATUM_CN) %>%
    summarize(aStrat = mean(forArea, na.rm = TRUE), # Area mean
              bioStrat = mean(bioPlot, na.rm = TRUE), # Biomass mean
              carbStrat = mean(carbPlot, na.rm = TRUE), # Carbon mean
              ## We don't want a vector of these values, since they are repeated
              P2POINTCNT = first(P2POINTCNT),
              AREA_USED = first(AREA_USED),
              ## Strata weight, relative to estimation unit
              w = first(P1POINTCNT) / first(P1PNTCNT_EU),
              ## Strata level variances
              aVar = (sum(forArea^2) - sum(P2POINTCNT * aStrat^2)) / (P2POINTCNT * (P2POINTCNT-1)),
              bioVar = (sum(bioPlot^2) - sum(P2POINTCNT * bioStrat^2)) / (P2POINTCNT * (P2POINTCNT-1)),
              carbVar = (sum(carbPlot^2) - sum(P2POINTCNT * carbStrat^2)) / (P2POINTCNT * (P2POINTCNT-1)),
              ## Strata level co-varainces
              bioCV = (sum(forArea*bioPlot) - sum(P2POINTCNT * aStrat *bioStrat)) / (P2POINTCNT * (P2POINTCNT-1)),
              carbCV = (sum(forArea*carbPlot) - sum(P2POINTCNT * aStrat *carbStrat)) / (P2POINTCNT * (P2POINTCNT-1)))
  
  ## Moving on to the estimation unit
  bio_pop_eu <- bio_pop_strat %>%
    group_by(ESTN_UNIT_CN) %>%
    summarize(aEst = sum(aStrat * w, na.rm = TRUE) * first(AREA_USED), # Mean Area
              bioEst = sum(bioStrat * w, na.rm = TRUE) * first(AREA_USED), # Mean biomass
              carbEst = sum(carbStrat * w, na.rm = TRUE) * first(AREA_USED), # Mean carbon
              ## Estimation unit variances
              aVar = (first(AREA_USED)^2 / sum(P2POINTCNT)) * 
                (sum(P2POINTCNT*w*aVar) + sum((1-w)*(P2POINTCNT/sum(P2POINTCNT))*aVar)),
              bioVar = (first(AREA_USED)^2 / sum(P2POINTCNT)) * 
                (sum(P2POINTCNT*w*bioVar) + sum((1-w)*(P2POINTCNT/sum(P2POINTCNT))*bioVar)),
              carbVar = (first(AREA_USED)^2 / sum(P2POINTCNT)) * 
                (sum(P2POINTCNT*w*carbVar) + sum((1-w)*(P2POINTCNT/sum(P2POINTCNT))*carbVar)),
              ## Estimation unit covariances
              bioCV = (first(AREA_USED)^2 / sum(P2POINTCNT)) * 
                (sum(P2POINTCNT*w*bioCV) + sum((1-w)*(P2POINTCNT/sum(P2POINTCNT))*bioCV)),
              carbCV = (first(AREA_USED)^2 / sum(P2POINTCNT)) * 
                (sum(P2POINTCNT*w*carbCV) + sum((1-w)*(P2POINTCNT/sum(P2POINTCNT))*carbCV)))
  
  ## Finally, sum across Estimation Units for totals
  bio_pop_total <- bio_pop_eu %>%
    summarize(AREA_TOTAL = sum(aEst, na.rm = TRUE),
              BIO_AG_TOTAL = sum(bioEst, na.rm = TRUE),
              CARB_AG_TOTAL = sum(carbEst, na.rm = TRUE),
              ## Ratios
              BIO_AG_ACRE = BIO_AG_TOTAL / AREA_TOTAL,
              CARB_AG_ACRE = CARB_AG_TOTAL / AREA_TOTAL,
              ## Total samping errors
              AREA_TOTAL_SE = sqrt(sum(aVar, na.rm = TRUE)) / AREA_TOTAL * 100,
              BIO_AG_TOTAL_SE = sqrt(sum(bioVar, na.rm = TRUE)) / BIO_AG_TOTAL * 100, 
              CARB_AG_TOTAL_SE = sqrt(sum(carbVar, na.rm = TRUE)) / CARB_AG_TOTAL * 100,
              ## Ratio variances
              bioAcreVar = (1 / AREA_TOTAL^2) * (sum(bioVar) + (BIO_AG_ACRE^2)*sum(aVar) - 2*BIO_AG_ACRE*sum(bioCV)),
              carbAcreVar = (1 / AREA_TOTAL^2) * (sum(carbVar) + (CARB_AG_ACRE^2)*sum(aVar) - 2*CARB_AG_ACRE*sum(carbCV)),
              BIO_AG_ACRE_SE = sqrt(sum(bioAcreVar, na.rm = TRUE)) / BIO_AG_ACRE * 100,
              CARB_AG_ACRE_SE = sqrt(sum(carbAcreVar, na.rm = TRUE)) / CARB_AG_ACRE * 100) %>%
    ## Re ordering, dropping variance
    select(BIO_AG_ACRE, CARB_AG_ACRE, BIO_AG_TOTAL, CARB_AG_TOTAL, AREA_TOTAL, BIO_AG_ACRE_SE, 
           CARB_AG_ACRE_SE, BIO_AG_TOTAL_SE, CARB_AG_TOTAL_SE, AREA_TOTAL_SE)
  
  ## Saving the output in our list...
  bio_pop[[i]] <- bio_pop_total
}

##Convert back to a data frame and rejoin with grps

bio_pop <- bind_rows(bio_pop)
bio_pop_sp <- bind_cols(grps, bio_pop)

head(biomass(clipFIA(fiaRI), totals = TRUE, bySpecies = TRUE))
