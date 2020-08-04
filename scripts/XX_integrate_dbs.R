# A script to examine the 22 leftover plots

library(readxl)
library(rgdal)
library(RSQLite)
library(tidyverse)

#----------------------------
# Unzip password protected databases via terminal

# cd /media/jbukoski/9E25-21B8/cafia
# unzip LinkConf.zip
# unzip AnnualData.zip
# unzip CA_occ2_3.zip
# unzip r5_tag_num.zip

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

# cd /media/jbukoski/9E25-21B8/cafia
# rm LinkConf.db
# rm AnnualData.db
# rm CA_occ2_3.db
# rm r5_tag_num.db

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

#-------------------------------
# Linking the region 5 data

# Need to use the forest level code (FORE) and the plot level code (PLOT)
# to link the tables. FORE and R5_PLT_ID are a bit hidden. Can subtract
# away region 5 indicator (500) to get FORE code whereas plot code may be
# a 5 or 6 digit long number. 

r5_linkData <- linkTable$LINK_CONF %>%
  filter(!is.na(R5)) %>%
  mutate(FORE = NFS_ADFORCD - 500,
         R5_PLT_ID = ifelse(NFS_PLT_NUM_PNWRS < 100000, 
                            NFS_PLT_NUM_PNWRS - (FORE * 1000), 
                            NFS_PLT_NUM_PNWRS - (FORE * 10000))) %>%
  select(BURN_CLASS:NFS_PLT_NUM_PNWRS, FORE, R5_PLT_ID, 
         PERIODIC_PLOT_NBR_PNWRS:OAK_DEATH_PLOT_YN)

# Link on both FORE & PLOT

r5_linkData %>%
  select(STATECD, COUNTYCD, PLOT_FIADB, FORE, R5_PLT_ID) %>%
  left_join(r5Tables$R5_VEGETATION_DATA_ALL, by = c("FORE", "R5_PLT_ID" = "PLOT"))


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



