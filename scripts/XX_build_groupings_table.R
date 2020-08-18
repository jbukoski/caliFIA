#############################################
## Script to build a table that links with ##
## plots and provides plot metadata.      ##
#############################################

library(tidyverse)
library(RSQLite)

#-------------------------
# Confidential database

# unzip August_update.zip

sqlite.driver <- dbDriver("SQLite")

filename <- "/media/jbukoski/9E25-21B8/cafia/August_update.db"
db <- dbConnect(sqlite.driver, dbname = filename)

dbListTables(db)

plots <- dbReadTable(db, "FireYearsField") %>%
  filter(STATECD == 6) %>%
  rename_all(tolower) %>%
  rename(anncd = dstrbcd1,
         fia_fire = dstrbyr1) %>%
  select(statecd, countycd, plot_fiadb, invyr, measyear, condid)

dbDisconnect(db)

# rm August_update.db

#--------------------
# Public database

filename2 <- "./data/PNW_PUBLIC_SQLite.db"
db2 <- dbConnect(sqlite.driver, dbname = filename2)

pnwFIADBplot <- dbReadTable(db2, "PLOT") %>%
  rename_all(tolower) %>%
  rename(plot_fiadb = plot)

pnwFIADBcond <- dbReadTable(db2, "COND") %>%
  rename_all(tolower) %>%
  rename(plot_fiadb = plot)

forTypRef <- dbReadTable(db2, "REF_FOREST_TYPE") %>% 
  rename(FORTYPCD = REF_FORTYPCD) %>%
  rename_all(tolower)

dbDisconnect(db2)


#--------------------------------------



OWNGRPCDref <- matrix(c(11, "National Forest.", 
                        12, "National Grassland and/or Prairie.", 
                        13, "Other Forest Service Land.", 
                        21, "National Park Service.", 
                        22, "Bureau of Land Management.", 
                        23, "Fish and Wildlife Service.", 
                        24, "Departments of Defense/Energy.", 
                        25, "Other Federal.", 
                        31, "State including State public universities.", 
                        32, "Local (County, Municipality, etc.) including water authorities.", 
                        33, "Other non-federal public.", 
                        46, "Undifferentiated private and Native American."), ncol = 2, byrow = TRUE) %>% 
  as.data.frame() %>% 
  rename( "owncd" = "V1", "owncd_text" = "V2") %>%
  mutate(owncd = as.numeric(as.character(owncd)))

join_cols <- c("statecd", "countycd", "plot_fiadb", "invyr")

plots %>%
  left_join(pnwFIADBplot, by = c(join_cols, "measyear")) %>%
  left_join(pnwFIADBcond, by = c(join_cols, "condid")) %>%
  select(statecd, countycd, plot_fiadb, invyr, measyear, condid, owncd, fortypcd) %>%
  distinct() %>%
  left_join(OWNGRPCDref) %>%
  left_join(forTypRef) %>%
  arrange(plot_fiadb, invyr, owncd, fortypcd)
  





write_csv(forTypRef, "./data/processed/forestTypeRef.csv")