# A script to determine which reburn plots are "confirmed"
# by fire perimeters, FERS data, or other sources.

library(readxl)
library(RSQLite)
library(tidyverse)

#----------------------
# Database from Jeremy

filename <- "./data/TeamPotts2.db"
sqlite.driver <- dbDriver("SQLite")
db <- dbConnect(sqlite.driver, dbname = filename)

plot <- rename_all(dbReadTable(db, "PLOT"), tolower)
cond <- rename_all(dbReadTable(db, "COND"), tolower)

dbDisconnect(db)

# Load in complementary tables

ForTypRef <- rename_all(read_csv("./data/processed/forestTypeRef.csv"), tolower)

cntyCds <- read_csv("./data/ca_cnty_cds.csv", col_names = T, 
                    cols(CNTY_NAME = col_character(), COUNTYCD = col_double())) %>%
  rename_all(tolower)

# Load in the table of single burn plots

singles <- read_csv("./data/processed/singles.csv") %>%
  mutate(intensity = as.character(intensity))

#--------------------------
#Joined data to process

jndData <- singles %>%
  group_by(plot_fiadb) %>%
  mutate(n_fiafires = n_distinct(disturbyr),
         n_perimfires = n_distinct(perim_fire),
         n_fersfires = n_distinct(fers_fire)) %>%
  mutate(match = disturbyr %in% perim_fire | disturbyr %in% fers_fire,
         n_match = n_distinct(match)) %>%
  ungroup() %>%
  left_join(plot) %>%
  left_join(cond, by = c("statecd", "plot_fiadb", "condid", "invyr")) %>%
  left_join(ForTypRef) %>%
  select(plot_fiadb, inventory, invyr, measyear, condid, anncd, intensity, 
         fia_fire = disturbyr, perim_fire, fers_fire, n_fiafires, n_perimfires, 
         n_fersfires, match, n_match, countycd, fortypcd, owngrpcd, swhw)

dat2process <- jndData

# 1844 candidate single burn plots
n_distinct(dat2process$plot_fiadb)

#------------------------------------------

singleBurns <- dat2process %>%
  group_by(plot_fiadb) %>%
  filter(match == TRUE) %>%
  filter(n_perimfires <= 1 & n_fersfires <= 1)

n_distinct(singleBurns$plot_fiadb)

singleBurns %>%
  left_join(cntyCds) %>%
  group_by(cnty_name, swhw) %>%
  summarize(n = n_distinct(plot_fiadb)) %>%
  arrange(swhw, -n, cnty_name) %>%
  View
