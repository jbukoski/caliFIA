# A script to examine the TREE and DWM data

library(readxl)
library(rgdal)
library(RSQLite)
library(tidyverse)

# Load in the table of single burn plots

singles <- read_csv("./data/processed/conf_singles.csv")
reburns <- read_csv("./data/processed/conf_reburns.csv")

# Go into the public database to get what tree records we have

filename <- "./data/PNW_PUBLIC_SQLite.db"
sqlite.driver <- dbDriver("SQLite")
db <- dbConnect(sqlite.driver, dbname = filename)

sort(dbListTables(db))

plot <- rename_all(dbReadTable(db, "PLOT"), tolower)
cond <- rename_all(dbReadTable(db, "COND"), tolower)
tree <- dbReadTable(db, "Tree")
seedling <- rename_all(dbReadTable(db, "SEEDLING"), tolower)

dbDisconnect(db)

#------------------------------------------

rbrn_plts <- unique(reburns$plot_fiadb)

mytrees <- tree %>%
  rename_all(tolower) %>%
  filter(plot %in% rbrn_plts) %>%
  select(countycd:cn, condid, subp, tree, spcd, spgrpcd, statuscd, tpa_unadj, carbon_ag, carbon_bg, dia, ht, 
         damage_agent_cd1:damage_agent_cd3, agentcd, reconcilecd)

rbrn_trees <- reburns %>% 
  left_join(mytrees, by = c("plot_fiadb" = "plot", "invyr", "condid")) %>%
  mutate(timeSncFire = measyear - fia_fire) %>%
  group_by(plot_fiadb, invyr, condid, statuscd) %>%
  mutate(agc_mgha = sum(carbon_ag * tpa_unadj, na.rm = T) * 0.00112085) %>%
  select(plot_fiadb:fia_fire, statuscd, cycle, subcycle, agc_mgha, reconcilecd) %>%
  distinct() %>%
  ungroup() %>%
  group_by(plot_fiadb) %>%
  filter(!any(is.na(cycle))) %>%
  arrange(plot_fiadb, invyr)
  
 
rbrn_trees %>%
  View










View(mytrees)

exmpl_dat <- data.frame(plot = c(123, 123, 234, 234, 345, 345, 456, 456),
                        invyr = c(2000, 2008, 2003, 2006, 2009, 2015, 2004, 2014),
                        visit = rep(c(1,2), 4),
                        ag_ttl = c(150, 200, 80, 100, 40, 50, 100, 105),
                        treatment = rep(1, 8))

sim_sngls <- data.frame(plot = c(567, 567, 789, 789, 890, 890, 111, 111),
                        invyr = c(2000, 2008, 2003, 2006, 2009, 2015, 2004, 2014),
                         visit = rep(c(1,2), 4),
                         ag_ttl = c(200, 23, 140, 100, 80, 75, 100, 50),
                         treatment = rep(0, 8))

dat <- bind_rows(sim_sngls, exmpl_dat) %>%
  select(-invyr) %>%
  spread(visit, ag_ttl) %>%
  mutate(treatment = as.factor(treatment)) %>%
  rename(v1 = "1",
         v2 = "2")



