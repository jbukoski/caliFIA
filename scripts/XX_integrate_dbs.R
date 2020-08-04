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

conf_plts <- bind_rows(singles, reburns)

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



r5_linkData %>%
  select(STATECD, COUNTYCD, PLOT_FIADB, NFS_ADFORCD, NFS_PLT_NUM_PNWRS, FORE, R5_PLT_ID) %>%
  anti_join(r5Tables$`R5 LIST OF CONDITIONS AND PLOTS IN IDB`, by = c("NFS_ADFORCD" = "FOREST_OR_BLM_DISTRICT", "R5_PLT_ID" = "PLOT")) %>%
  arrange(FORE, R5_PLT_ID)


r5Tables$R5_VEGETATION_DATA_ALL %>%
  filter(FORE == 1) %>%
  pull(PLOT) %>%
  unique() %>%
  sort()

# 
# 32021 rows with a left_join
# 154 rows without a match (out of 589 original rows)

linkTable$LINK_CONF %>% 
  filter(ANNUAL_PLOT == "Y" & PERIODIC_PLOT == "Y") %>%
  pull(PLOT_FIADB) %>%
  unique() %>%
  length()


r5Tables$`R5 LIST OF CONDITIONS AND PLOTS IN IDB` %>%
  filter(FOREST_OR_BLM_DISTRICT == 511) %>%
  pull(CNTY) %>%
  unique()

colnames(r5Tables$`R5 LIST OF CONDITIONS AND PLOTS IN IDB`)



linkTable$LINK_CONF %>%
  filter(PLOT_FIADB %in% r5Tables$R5_VEGETATION_DATA_ALL$PLOT)

annTables[["PLOT"]] %>%
  filter(PLOT_FIADB %in% r5Tables[["R5 LIST OF CONDITIONS AND PLOTS IN IDB"]]$PLOT_ID)

# Can link to CA91 periodic inventories using PLOT_FIADB

linkTable[["LINK_CONF"]] %>%
  filter(PLOT_FIADB %in% prdcTables[["CA81_TREE"]]$PLOT_FIADB) %>%
  View



