# A script to pull together the disparate data sources that Jeremy
# has been sending into a cohhesive data frame with candidates for
# plots with reburn and single burn events.

library(readxl)
library(RSQLite)
library(tidyverse)

#-------------------------------
# Load in tables from databases, first from Jeremy

filename <- "./data/TeamPotts2.db"
sqlite.driver <- dbDriver("SQLite")
db <- dbConnect(sqlite.driver, dbname = filename)

db_cond_lb <- dbReadTable(db, "COND_LVL_LOOK_BACKS_PNW") %>%
  filter(STATECD == 6) %>%
  rename_all(tolower)

db_plot_lb <- dbReadTable(db, "PlotLevelFireDisturbPNWFromAnnualVisitLookbacks") %>%
  filter(STATECD == 6) %>%
  rename_all(tolower)

all_plots_list <- dbReadTable(db, "ALL_PLOTS_LIST_w_EvidenceSource")
ag_fixed_fire <- rename_all(dbReadTable(db, "AG_FIXED_FIRE_YRS_To_1960_forested_ever"), tolower)
xtra_plts <- dbReadTable(db, "ExtraDistPltsFromAGfix")     # Additional 10 plots
plot <- rename_all(dbReadTable(db, "PLOT"), tolower)
cond <- rename_all(dbReadTable(db, "COND"), tolower)

dbDisconnect(db)

#-------------

perimDat <- read_excel("./data/FireOverlay2.xlsx") %>%
  arrange(plot_fiadb, FireYear) %>%
  rename_all(tolower) %>%
  rename(perim_fire = fireyear)

fersDat <- read_csv("./data/CA_FERS.csv", col_names = T, cols(PLOT_FIADB = col_double(), FIRE_YEAR = col_double())) %>%
  rename_all(tolower) %>%
  rename(fers_fire = fire_year)

cntyCds <- read_csv("./data/ca_cnty_cds.csv", col_names = T, cols(CNTY_NAME = col_character(), COUNTYCD = col_double())) %>%
  rename_all(tolower)

#------------------------------
# Build a more complete dataframe of plots with burns.
# Pull in the CA81 and CA91 periodic inventory burn data from the
# ag_fixed_fire table.
# Keep db_cond_lb records over ag_fixed_fire table because they
# are mostly just dropping repeat recording of fire (e.g., 2003 and 2007
# measurements reporting a 2002 fire, they would drop the 2007 fire record)
# and this is not important for our work.

full_plot_list <- db_cond_lb %>%
  rename(fia_fire = dstrbyr1, anncd = dstrbcd1) %>% 
  mutate(inventory = NA, oc = NA) %>%
  select(statecd, plot_fiadb, inventory, oc, measyear, condid, fia_fire, anncd, intensity) %>%
  bind_rows(filter(select(ag_fixed_fire, -anntxt), inventory != "Annual")) %>%
  filter(fia_fire >= 1960) %>%
  left_join(perimDat, by = "plot_fiadb") %>%
  left_join(fersDat, by = "plot_fiadb") %>%
  arrange(plot_fiadb, measyear, fia_fire) %>%
  select(plot_fiadb:condid, anncd, intensity, fia_fire, perim_fire, fers_fire)

# Manually compared db_cond_lb multiple burns against the andy gray table of 
# reburns, and found the following plots that would appear to have erroneous
# dates based on fire perimeter data:

fix_plots <- c(66183, 68079, 71345, 77207, 77536, 80148, 86863, 87669, 88254, 
               90197, 90428, 92879, 96064, 99757)

jjb_fixed_plts <- db_cond_lb %>%
  arrange(plot_fiadb) %>%
  filter(plot_fiadb %in% fix_plots, invyr <= 2013) %>%
  left_join(perimDat, by = "plot_fiadb") %>%
  left_join(fersDat, by = "plot_fiadb") %>%
  group_by(plot_fiadb) %>%
  mutate(dstrbyr1 = case_when(plot_fiadb == 66183 ~ fers_fire,
                              plot_fiadb == 68079 ~ dstrbyr1,
                              plot_fiadb == 71345 ~ fers_fire,
                              plot_fiadb == 77207 ~ 2002,     # Confirmed by fire perimeter
                              plot_fiadb == 77536 ~ dstrbyr1,  # Both dates confirmed by fire perimeter
                              plot_fiadb == 80148 & dstrbyr1 == 2008 ~ 2006, # adjust based on fire perimeter
                              plot_fiadb == 80148 & dstrbyr1 == 1990 ~ 1987, # adjust based on fire perimeter
                              plot_fiadb == 86863 ~ perim_fire,
                              plot_fiadb == 88254 ~ perim_fire,
                              plot_fiadb == 90197 ~ 1992,     # Use perimeter fire date
                              plot_fiadb == 90428 ~ perim_fire,
                              plot_fiadb == 92879 ~ perim_fire,
                              plot_fiadb == 96064 ~ dstrbyr1,
                              plot_fiadb == 99757 ~ perim_fire,
                              TRUE ~ dstrbyr1)) %>%
  filter(!(plot_fiadb == 87669 & invyr == 2011)) %>%
  ungroup() %>%
  mutate(inventory = "Annual", oc = NA, intensity = "1") %>%
  rename(disturbyr = dstrbyr1, anncd = dstrbcd1) %>%
  select(statecd, plot_fiadb, inventory, oc, measyear, condid, disturbyr, anncd, intensity) %>%
  distinct() %>%
  group_by(plot_fiadb) %>%
  mutate(n_dstrbyr = n_distinct(disturbyr)) %>%
  filter((n_dstrbyr == 1 & measyear == min(measyear)) | 
           (n_dstrbyr > 1 & measyear == max(measyear) & disturbyr > min(measyear)) |
           plot_fiadb %in% c(80148, 87669)) %>%
  ungroup() %>%
  select(-n_dstrbyr)


dat2join <- db_cond_lb %>%
  mutate(inventory = "Annual", oc = NA, intensity = "1") %>%
  rename(disturbyr = dstrbyr1, anncd = dstrbcd1) %>%
  select(statecd, plot_fiadb, inventory, oc, measyear, condid, disturbyr, anncd, intensity)
  
# Join the ag_fixed fire plots - those that were adjusted above, the adjusted
# plots from above, and all records with measurement years greater than or
# equal to 2013.

all_dat <- ag_fixed_fire %>%
  select(-anntxt) %>%
  filter(!(plot_fiadb %in% fix_plots)) %>%
  bind_rows(jjb_fixed_plts) %>%
  bind_rows(filter(dat2join, measyear >= 2013)) %>%
  arrange(plot_fiadb) %>%
  select(-oc) %>%
  distinct()

#----------------------------------
# Filter to just those candidates for plots with reburn data

reburns <- all_dat %>%
  group_by(plot_fiadb) %>%
  mutate(n_fia_fires = n_distinct(disturbyr)) %>%
  ungroup() %>%
  filter(n_fia_fires > 1) %>%
  left_join(perimDat, by = "plot_fiadb") %>%
  left_join(fersDat, by = "plot_fiadb")

write_csv(reburns, "./data/processed/reburns.csv")

#----------------------------------
# Filter to just those candidates for plots with single burns

singles <- all_dat %>%
  group_by(plot_fiadb) %>%
  mutate(n_fia_fires = n_distinct(disturbyr)) %>%
  ungroup() %>%
  filter(n_fia_fires == 1) %>%
  left_join(perimDat, by = "plot_fiadb") %>%
  left_join(fersDat, by = "plot_fiadb") %>%
  select(-statecd) %>%
  rename(fia_fire = disturbyr, n_fiafires = n_fia_fires)

write_csv(singles, "./data/processed/singles.csv")

#----------------------------------
# Owner Group Code reference table

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
  rename( "OWNCD" = "V1", "Description" = "V2") %>%
  mutate(OWNCD = as.numeric(as.character(OWNCD)))

write_csv(OWNGRPCDref, "./data/processed/ownGrpCdRef.csv")

#---------------------------------
# Forest Type reference table (from public database)

filename2 <- "./data/PNW_PUBLIC_SQLite.db"
sqlite.driver <- dbDriver("SQLite")
db2 <- dbConnect(sqlite.driver, dbname = filename2)

pnwFIADBcond <- dbReadTable(db2, "COND")

forTypRef <- dbReadTable(db2, "REF_FOREST_TYPE") %>% 
  rename(FORTYPCD = REF_FORTYPCD) %>%
  rename_all(tolower)

dbDisconnect(db2)

write_csv(forTypRef, "./data/processed/forestTypeRef.csv")

#---------------------------------
# Clean up files

rm(list = ls())
