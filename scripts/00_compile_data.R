# A script to pull together the disparate data sources that Jeremy
# has been sending into a cohhesive data frame with candidates for
# plots with reburn and single burn events.

library(readxl)
library(RSQLite)
library(tidyverse)

#-------------------------------
# Load in tables from databases, first from Jeremy
# Pull in plot and condition tables as well as Andy Gray fixed fire dates data

filename <- "./data/TeamPotts2.db"
sqlite.driver <- dbDriver("SQLite")
db <- dbConnect(sqlite.driver, dbname = filename)

plot <- rename_all(dbReadTable(db, "PLOT"), tolower)
# cond <- rename_all(dbReadTable(db, "COND"), tolower)

xtra_plts <- dbReadTable(db, "ExtraDistPltsFromAGfix")     # Additional 10 plots

ag_fixed_fire <- rename_all(dbReadTable(db, "AG_FIXED_FIRE_YRS_To_1960_forested_ever"), tolower) %>%
  left_join(select(plot, statecd, countycd, plot_fiadb, measyear, invyr)) %>%
  rename(fia_fire = disturbyr)

dbDisconnect(db)

#--------------------------------
# Updated database from Jeremy that he provided August 14th 2020

filename <- "/media/jbukoski/9E25-21B8/cafia/August_update.db"
sqlite.driver <- dbDriver("SQLite")
db <- dbConnect(sqlite.driver, dbname = filename)

dbListTables(db)

db_august <- dbReadTable(db, "FireYearsField") %>%
  filter(STATECD == 6) %>%
  rename_all(tolower) %>%
  rename(anncd = dstrbcd1,
         fia_fire = dstrbyr1)

perimDat_august <- dbReadTable(db, "FA_overlay_fire_years") %>%
  rename_all(tolower) %>%
  rename(perim_fire = fireyear,
         perim_src = source)

# Additional dataset to join - Fire Effects and Recovery Study and cnty 

fersDat <- read_csv("./data/CA_FERS.csv", col_names = T, cols(PLOT_FIADB = col_double(), FIRE_YEAR = col_double())) %>%
  rename_all(tolower) %>%
  rename(fers_fire = fire_year)

#------------------------------

# Add source data back into the ag_fixed_fire table

ag_fixed_fire <- ag_fixed_fire %>%
  left_join(select(db_august, statecd, countycd, plot_fiadb, invyr, source))

# Manually compared db_cond_lb multiple burns against the andy gray table of 
# reburns, and found the following plots that would appear to have erroneous
# dates based on fire perimeter data:

fix_plots <- c(66183, 68079, 71345, 77207, 77536, 80148, 86863, 87669, 88254, 
               90197, 90428, 92879, 96064, 99757)

jjb_fixed_plts <- db_august %>%
  arrange(plot_fiadb) %>%
  filter(plot_fiadb %in% fix_plots, invyr <= 2013) %>%
  left_join(perimDat_august, by = c("statecd", "countycd", "plot_fiadb")) %>%
  left_join(fersDat, by = "plot_fiadb") %>%
  mutate(perim_fire = as.numeric(perim_fire)) %>%
  group_by(plot_fiadb) %>%
  mutate(fia_fire = case_when(plot_fiadb == 66183 ~ fers_fire,
                              plot_fiadb == 68079 ~ fia_fire,
                              plot_fiadb == 71345 ~ fers_fire,
                              plot_fiadb == 77207 ~ 2002,     # Confirmed by fire perimeter
                              plot_fiadb == 77536 ~ fia_fire,  # Both dates confirmed by fire perimeter
                              plot_fiadb == 80148 & fia_fire == 2008 ~ 2006, # adjust based on fire perimeter
                              plot_fiadb == 80148 & fia_fire == 1990 ~ 1987, # adjust based on fire perimeter
                              plot_fiadb == 86863 ~ perim_fire,
                              plot_fiadb == 88254 ~ perim_fire,
                              plot_fiadb == 90197 ~ 1992,     # Use perimeter fire date
                              plot_fiadb == 90428 ~ perim_fire,
                              plot_fiadb == 92879 ~ perim_fire,
                              plot_fiadb == 96064 ~ fia_fire,
                              plot_fiadb == 99757 ~ perim_fire,
                              TRUE ~ fia_fire)) %>%
  filter(!(plot_fiadb == 87669 & invyr == 2011)) %>%
  ungroup() %>%
  mutate(inventory = "Annual", oc = NA, intensity = "1") %>%
  select(statecd, plot_fiadb, inventory, invyr, oc, measyear, condid, fia_fire, anncd, intensity) %>%
  distinct() %>%
  group_by(plot_fiadb) %>%
  mutate(n_fiafire = n_distinct(fia_fire)) %>%
  filter((n_fiafire == 1 & measyear == min(measyear)) | 
           (n_fiafire > 1 & measyear == max(measyear) & fia_fire > min(measyear)) |
           plot_fiadb %in% c(80148, 87669)) %>%
  ungroup() %>%
  select(-n_fiafire)

#-------------------------------------------
# Join the various datasets together

full_data <- db_august %>%
  mutate(inventory = "Annual", oc = NA) %>%
  select(statecd, countycd, plot_fiadb, inventory, oc, invyr, measyear, condid, fia_fire, anncd, intensity, source) %>%
  anti_join(ag_fixed_fire, by = c("statecd", "plot_fiadb", "invyr", "measyear")) %>%    # Drop all plot visits that are in the AG dataset
  bind_rows(ag_fixed_fire) %>%    # Replace those observations with the AG data, AG data has 33 plots not in full_plot
  filter(!(plot_fiadb %in% fix_plots)) %>%
  bind_rows(jjb_fixed_plts) %>%
  left_join(perimDat_august, by = c("statecd", "plot_fiadb")) %>%
  left_join(fersDat, by = "plot_fiadb") %>%
  filter((fia_fire >= 1960 | is.na(fia_fire))) %>%
  filter(!(is.na(fia_fire) & is.na(perim_fire))) %>%
  filter(!(is.na(fia_fire) & perim_fire < 1960)) %>%
  arrange(plot_fiadb, measyear, fia_fire) %>%
  select(statecd, countycd = countycd.x, plot_fiadb:condid, anncd, intensity, fia_fire, perim_fire, fers_fire, source) %>%
  distinct()

full_data_a <- full_data %>%
  filter(!is.na(fia_fire)) %>%
  group_by(plot_fiadb) %>%
  mutate(n_fiafires = n_distinct(fia_fire)) %>%
  ungroup()

full_data_b <- full_data %>%
  filter(source == "FIRE_CD_PNWRS" & !is.na(perim_fire)) %>%
  group_by(plot_fiadb) %>%
  mutate(n_perimfires = n_distinct(perim_fire))

#----------------------------------
# Write out CSVs for the plots with FIA records

reburns <- full_data_a %>%
  filter(n_fiafires > 1)

singles <- full_data_a %>%
  filter(n_fiafires == 1)

#write_csv(reburns, "./data/processed/reburns.csv")
write_csv(reburns, "./data/processed/reburns2.csv")

#write_csv(singles, "./data/processed/singles.csv")
write_csv(singles, "./data/processed/singles2.csv")

# Write out CSVs for plots with only evidence of fire (FIRE_CD_PNWRS == Y) and 
# fire perimeter records (i.e., no FIA fire disturbance yr records)

write_csv(full_data_b, "./data/processed/perim_burns_only.csv")

#---------------------------------
# Clean up files

rm(list = ls())
