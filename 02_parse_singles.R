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

dat2process <- singles %>%
  group_by(plot_fiadb) %>%
  mutate(n_fiafires = n_distinct(fia_fire),
         n_perimfires = n_distinct(perim_fire),
         n_fersfires = n_distinct(fers_fire)) %>%
  mutate(match = fia_fire %in% perim_fire | fia_fire %in% fers_fire,
         n_match = n_distinct(match)) %>%
  ungroup() %>%
  left_join(plot) %>%
  left_join(cond, by = c("statecd", "plot_fiadb", "condid", "invyr")) %>%
  left_join(ForTypRef) %>%
  select(plot_fiadb, inventory, invyr, measyear, condid, anncd, intensity, 
         fia_fire, perim_fire, fers_fire, n_fiafires, n_perimfires, 
         n_fersfires, match, n_match, countycd, fortypcd, owngrpcd, swhw)

# 1844 candidate single burn plots
n_distinct(dat2process$plot_fiadb)

#------------------------------------------
# Identify plots with confirmed burn dates by perimeter years

confSingles <- dat2process %>%
  group_by(plot_fiadb) %>%
  filter(match == TRUE) %>%
  select(plot_fiadb, invyr, measyear, condid, anncd, fia_fire) %>%
  arrange(plot_fiadb, measyear, fia_fire) %>%
  mutate(df = "cnfrmd", 
         fireYrSrc = "FIADB",
         confidence = 3,
         evidence = "burn year confirmed by fire perimeter year",
         notes = NA
  ) %>%
  ungroup() %>%
  distinct()
  
n_distinct(confSingles$plot_fiadb)

#-----------------------------------------
# Filtering through remaining plots

# Rule 1, if the plot has a single fia fire and single perimeter fire, and the 
# perimeter burn is within 5 or fewer years of the FIA burn, use the perimeter
# burn and assign confidence level of 2.


sngl_p1 <- dat2process %>%
  filter(!(plot_fiadb %in% confSingles$plot_fiadb)) %>%
  group_by(plot_fiadb) %>%
  mutate(n_fiafires = n_distinct(fia_fire),
         n_perimfires = n_distinct(perim_fire)) %>%
  filter(n_fiafires == 1 & n_perimfires == 1) %>%
  mutate(diff = abs(fia_fire - perim_fire)) %>%
  filter(diff <= 5) %>%
  mutate(fireYrSrc = "PERIM",
         fia_fire = perim_fire) %>%
  ungroup()
  

sngl_l1 <- sngl_p1 %>%
  select(colnames(confSingles[1:6])) %>%
  unique() %>%
  mutate(df = "prtly_cnfrmd_sngl",
         confidence = 2,
         evidence = "sngl rule 1 - if only 1 fia burn and 1 perim burn and 
         the difference b/n them is <= 5 yrs, keep perimeter burn yr",
         notes = NA)

confSingles <- add_row(confSingles, sngl_l1)
  
# Rule 2,

sngl_p2 <- dat2process %>%
  filter(!(plot_fiadb %in% confSingles$plot_fiadb)) %>%
  filter(n_fiafires == 1 & n_perimfires >= 1)
