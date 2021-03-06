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

singles <- read_csv("./data/processed/singles2.csv") %>%
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

# 1818 candidate single burn plots, was 1844 before August update
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
  
# Rule 2, For plots with one fia fire, multiple perimeter fires, and one
# perimeter fire is 5 or fewer years different from the fia fire, keep the
# perimeter fire.

sngl_p2 <- dat2process %>%
  filter(!(plot_fiadb %in% confSingles$plot_fiadb)) %>%
  filter(n_fiafires == 1 & n_perimfires >= 1) %>%
  mutate(diff = abs(fia_fire - perim_fire)) %>%
  group_by(plot_fiadb) %>%
  filter(diff == min(diff), perim_fire < measyear, diff <= 5) %>%
  mutate(fireYrSrc = "PERIM",
         fia_fire = perim_fire)

sngl_l2 <- sngl_p2 %>%
  select(colnames(confSingles[1:6])) %>%
  unique() %>%
  mutate(df = "prtly_cnfrmd_sngl",
         confidence = 2,
         evidence = "sngl rule 2 - if only 1 fia burn, more than 1 perim burn 
         and the difference b/n the fia burn and one of the perim burns is <=
         5 yrs, keep closest perimeter burn yr",
         notes = NA)

confSingles <- add_row(confSingles, sngl_l2)

# Rule 3, if only 1 fia burn, more than 1 perim burn and the difference b/n
# the fia burn and one of the perim burns is >5 & <= 10 yrs, keep closest 
# perimeter burn year.

sngl_p3 <- dat2process %>%
  filter(!(plot_fiadb %in% confSingles$plot_fiadb)) %>%
  filter(!is.na(perim_fire)) %>%
  filter(perim_fire < measyear) %>%
  mutate(diff = abs(fia_fire - perim_fire)) %>%
  group_by(plot_fiadb) %>%
  filter(diff == min(diff)) %>%
  filter(diff <= 10) %>%
  mutate(fireYrSrc = "PERIM",
         fia_fire = perim_fire)

sngl_l3 <- sngl_p3 %>%
  select(colnames(confSingles[1:6])) %>%
  unique() %>%
  mutate(df = "prtly_cnfrmd_sngl",
         confidence = 1,
         evidence = "sngl rule 3 - if only 1 fia burn, more than 1 perim burn 
         and the difference b/n the fia burn and one of the perim burns is >5 & 
         <= 10 yrs, keep closest perimeter burn yr",
         notes = NA)

confSingles <- add_row(confSingles, sngl_l3)

# Rule 4,

sngl_p4 <- dat2process %>%
  filter(!(plot_fiadb %in% confSingles$plot_fiadb))

#--------------------------
# Get final confirmed / unconfirmed plot counts

# Total of 688 unconfirmed single burn plots
n_distinct(sngl_p4$plot_fiadb)

# Total of 1130 confirmed single burn plots
n_distinct(confSingles$plot_fiadb)

#--------------------------
# Write out confirmed singles

write_csv(confSingles, "./data/processed/conf_singles.csv")

#--------------------------
# Clean up

rm(list = ls())

