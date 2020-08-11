# A script to examine the 22 leftover plots

library(readxl)
library(rgdal)
library(RSQLite)
library(tidyverse)

#----------------------------
# Unzip password protected databases via terminal

# cd /media/jbukoski/9E25-21B8/cafia
# unzip fia_data.zip

#----------------------------
# Specify SQL driver and paths to databases

sqlite.driver <- dbDriver("SQLite")

flashPath <- "/media/jbukoski/9E25-21B8/cafia/"
#flashPath <- 'evan's flash drive path here"

periodicDat <- paste0(flashPath, "CA_occ2_3.db")
r5Dat <- paste0(flashPath, "R5.db")
linkDat <- paste0(flashPath, "LinkConf.db")
annualDat <- paste0(flashPath, "AnnualData.db")
vegDat <- paste0(flashPath, "veg.db")
r5tagNums <- paste0(flashPath, "r5_tag_num.db")


# Function to extract the tables as a list

getTables <- function(database, myList) {
  
  tables <- dbListTables(database)
  
  for(table in tables) {
    myList[[table]] <- dbReadTable(database, table)
  }
  
  return(myList)
  
}


#-------------------------------------
# Read in confidential Link Table

db <- dbConnect(sqlite.driver, dbname = linkDat)

linkTable <- list()
linkTable <- getTables(db, linkTable)

dbDisconnect(db)

#------------------------------------
# Read in confidential Annual Data tables

db <- dbConnect(sqlite.driver, dbname = annualDat)

annTables <- list()
annTables <- getTables(db, annTables)

dbDisconnect(db)

# Add additional vegetation tables to the same database

db <- dbConnect(sqlite.driver, dbname = vegDat)

annTables <- getTables(db, annTables)

dbDisconnect(db)

#-------------------------------
# Periodic Data

db <- dbConnect(sqlite.driver, dbname = periodicDat)

prdcTables <- list()
prdcTables <- getTables(db, prdcTables)

dbDisconnect(db)

#---------------------------------
# R5 Data

db <- dbConnect(sqlite.driver, dbname = r5Dat)

r5Tables <- list()
r5Tables <- getTables(db, r5Tables)

dbDisconnect(db)

# Add tree tag numbers

db <- dbConnect(sqlite.driver, dbname = r5tagNums)

r5Tables <- getTables(db, r5Tables)

dbDisconnect(db)

#---------------------------------
# Remove confidential databases from the flashdrive
# Run following lines uncommented in the terminal


# rm *.db

#--------------------------------
# Clean up environment a bit

rm(db, sqlite.driver)
rm(annualDat, linkDat, periodicDat, r5Dat, r5tagNums, vegDat)

#--------------------------------
# Continue on with analysis
# Now have four lists correspondign to four databases

names(linkTable)  # Confidential link table
names(annTables)  # Annual FIA data
names(prdcTables)  # Periodic FIA data
names(r5Tables)  # Region 5 FIA data

# Load in list of confirmed single burn and reburn plots

singles <- read_csv("./data/processed/conf_singles.csv") %>%
  mutate(burn = "single")
reburns <- read_csv("./data/processed/conf_reburns.csv") %>%
  mutate(burn = "reburn")

conf_plts <- bind_rows(singles, reburns) %>%
  rename_all(toupper)

n_distinct(conf_plts$plot_fiadb)

# 1114 confirmed plots with single burns or reburns

#-------------------------------
# Linking the region 5 data

# Need to use the forest level code (FORE) and the plot level code (PLOT)
# to link the tables. FORE and R5_PLT_ID are a bit hidden. Can subtract
# away region 5 indicator (500) to get FORE code whereas plot code may be
# a 5 or 6 digit long number. 

r5_linkData <- linkTable$LINK_CONF %>%
  filter(!is.na(R5)) %>%
  filter(PLOT_FIADB %in% conf_plts$plot_fiadb) %>%
  mutate(FORE = NFS_ADFORCD - 500,
         R5_PLT_ID = ifelse(NFS_PLT_NUM_PNWRS < 100000, 
                            NFS_PLT_NUM_PNWRS - (FORE * 1000), 
                            NFS_PLT_NUM_PNWRS - (FORE * 10000))) %>%
  select(BURN_CLASS:NFS_PLT_NUM_PNWRS, FORE, R5_PLT_ID, 
         PERIODIC_PLOT_NBR_PNWRS:OAK_DEATH_PLOT_YN)

n_distinct(r5_linkData$PLOT_FIADB)

# 205 confirmed plots supposedly with R5 plot match.

# Link on both FORE & PLOT - can add PLOT_FIADB to the R5_VEG_DATA_ALL df

r5_linkData %>%
  select(STATECD, COUNTYCD, PLOT_FIADB, NFS_ADFORCD, NFS_PLT_NUM_PNWRS, FORE, R5_PLT_ID) %>%
  anti_join(r5Tables$R5_VEGETATION_DATA_ALL, by = c("FORE", "R5_PLT_ID" = "PLOT")) %>%
  arrange(FORE, R5_PLT_ID) %>%
  pull(PLOT_FIADB) %>%
  unique() %>%
  length()

# 55 confirmed plots that do not link to the R5_VEG_DATA_ALL table, not quite sure why
  

prePost <- conf_plts %>%
  left_join(select(linkTable$LINK_CONF, PLOT_FIADB, ANNUAL_PLOT, R5, PERIODIC_PLOT)) %>%     # Join link table data to see what plots will have prefire data and postfire data
  group_by(PLOT_FIADB) %>%
  filter(INVYR < 2019) %>%
  mutate(pre_fire = ifelse((max(FIA_FIRE) > 1995 & min(MEASYEAR) < max(FIA_FIRE)) |(max(FIA_FIRE) > 1995 & PERIODIC_PLOT == "Y"), "Y", "N"),
         post_fire = ifelse(max(MEASYEAR) > max(FIA_FIRE), "Y", "N"))

prePost %>%
  filter(pre_fire == "Y" & post_fire == "Y") %>%
  filter(ANNUAL_PLOT == "Y" & PERIODIC_PLOT == "N") %>%
  group_by(BURN) %>%
  summarize(n = n_distinct(PLOT_FIADB))

#--------------------------------------------------------
########################################
## Calculate biomass in annual tables ##
########################################

library(lubridate)

# Just summing by plot, inventory year, and visit

dat <- conf_plts %>%
  left_join(annTables$TREE, by = c("PLOT_FIADB", "CONDID", "INVYR")) %>%
  group_by(PLOT_FIADB) %>%
  mutate(visit = ifelse(max(FIA_FIRE) <= MEASYEAR, "POST", "PRE")) %>%
  select(BURN, PLOT_FIADB:FIA_FIRE, visit, DIA:CARBON_AG) %>% 
  group_by(PLOT_FIADB, INVYR, visit) %>%
  summarize(carbon = sum(CARBON_AG * TPA_UNADJ * 0.453592, na.rm = T),  # CARBON_AG is in lbs (p 193 of FIA v 8.0), 0.45... converts to kg 
            c_mgha = carbon / 0.404686 / 1000) %>%    # Converts from per Acre to per ha
  arrange(PLOT_FIADB, INVYR)

# Most of the difference values are within -100 to 0 (i.e., a loss of live
# biomass from pre to post fire)

dat2 %>%
  mutate(pt_col = diff > 0) %>%
  ggplot() +
  geom_point(aes(x = diff, y = PLOT_FIADB_FCT, col = as.factor(pt_col))) +
  theme_bw()

#---------------------------------------------
##########################################
## Calculate biomass in periodic tables ##
##########################################

# DBH3 - dbh of current inventory in mm (CA91)
# INV3_ABV_GRND_WB_TR	- Above ground woody biomass at the current inventory (kg / tree)

biomass91 <- prdcTables$CA91_TREE_CRRNT %>%
  left_join(prdcTables$CA91_TREE, by = c("PLOT_FIADB", "LINE")) %>%
  filter(DBH3 >= 25.4) %>%   # Filter to records that are greater than 1" DBH, match annual
  group_by(PLOT_FIADB) %>%
  summarize(c_mgha = sum(INV3_ABV_GRND_WB_AC / 0.404686 / 1000, na.rm = T)) %>%  # INV3_ABV_GRND_WB_AC is in kg/acre, convert to MG/ha here
  mutate(visit = "PRE", INVYR = 1991)


annl_w_prdc <- dat %>% 
  select(-carbon) %>%
  bind_rows(biomass91) %>%
  arrange(PLOT_FIADB, INVYR) %>% 
  left_join(select(conf_plts, PLOT_FIADB, INVYR, BURN, FIRE = FIA_FIRE)) %>% 
  filter(!is.na(INVYR)) %>%
  pivot_longer(cols = c(FIRE, INVYR), names_to = "VISIT", values_to = "YEAR") %>%
  distinct() %>% 
  select(PLOT_FIADB, BURN, VISIT, visit, YEAR, c_mgha) %>%
  filter(!is.na(YEAR)) %>% 
  group_by(PLOT_FIADB) %>%
  mutate(BURN = first(na.omit(BURN))) %>%
  ungroup() %>%
  mutate(VISIT = ifelse(VISIT == "INVYR", visit, VISIT)) %>%
  select(-visit) %>%
  mutate(c_mgha = ifelse(VISIT == "FIRE", NA, c_mgha)) %>%
  distinct() %>%
  group_by(PLOT_FIADB) %>%
  filter(("PRE" %in% VISIT) & ("POST" %in% VISIT)) %>%
  ungroup()


dat2plt <- annl_w_prdc %>%
  group_by(PLOT_FIADB) %>%
  arrange(PLOT_FIADB, YEAR) %>%
  mutate(diff = last(na.omit(c_mgha)) - first(na.omit(c_mgha)),
         perc_diff = ifelse((c_mgha != diff), 100 * diff / (first(na.omit(c_mgha)) + 0.00000001), 0),
         perc_diff = ifelse(!is.finite(perc_diff), NA, perc_diff)) %>%
  ungroup()
  
#---------------------------------
# Early modeling

library(lme4)

dat4stats <- dat2plt %>%
  pivot_wider(names_from = VISIT, values_from = c_mgha) %>%
  select(-FIRE, perc_diff) %>%
  filter(!(is.na(PRE) & is.na(POST))) %>%
  group_by(PLOT_FIADB) %>%
  mutate(PRE = first(na.omit(PRE)),
         POST = first(na.omit(POST))) %>%
  select(-YEAR)
  
  
dat4stats %>%
  filter(perc_diff < 0) %>%
  ggplot() +
  geom_boxplot(aes(x = BURN, y = PRE), col = "red", alpha = 0.1, width = 0.3) +
  geom_boxplot(aes(x = BURN, y = POST), col = "blue", alpha = 0.1, width = 0.3) +
  theme_bw() +
  ylab("Biomass C (Mg/ha)") +
  xlab("Burn Class")

dat4stats %>%
  filter(perc_diff < 0) %>%
  ggplot() +
  facet_grid(. ~ BURN) +
  geom_histogram(aes(perc_diff))

mdl1 <- glm(POST ~ BURN + PRE, data = dat4stats)

summary(mdl1)

dat2plt %>%
  filter(!is.na(perc_diff) & perc_diff < 0) %>%
  group_by(BURN) %>%
  summarize(avg_diff = mean(perc_diff, na.rm = T),
            sdv_diff = sd(perc_diff, na.rm = T)) %>% View

# 225 periodic plots that link to annual plots - slightly less for single.
# Question is how many single plots do we have pre- and post-fire data for

conf_plts

conf_plts %>%
  filter(BURN == "single") %>%
  group_by(PLOT_FIADB) %>%
  mutate(n_invyr = n_distinct(INVYR)) %>%
  filter(n_invyr > 1) %>%
  mutate(VISIT = ifelse(MEASYEAR < FIA_FIRE, "PRE", "POST")) %>%
  View


