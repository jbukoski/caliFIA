
library(readxl)
library(RSQLite)
library(tidyverse)

filename <- "./data/TeamPotts2.db"
sqlite.driver <- dbDriver("SQLite")
db <- dbConnect(sqlite.driver, dbname = filename)

dbListTables(db)

db_cond_lb <- dbReadTable(db, "COND_LVL_LOOK_BACKS_PNW")
db_plot_lb <- dbReadTable(db, "PlotLevelFireDisturbPNWFromAnnualVisitLookbacks")
all_plots_list <- dbReadTable(db, "ALL_PLOTS_LIST_w_EvidenceSource")
ag_fixed_fire <- dbReadTable(db, "AG_FIXED_FIRE_YRS_To_1960_forested_ever")
xtra_plts <- dbReadTable(db, "ExtraDistPltsFromAGfix")     # Additional 10 plots
plot <- dbReadTable(db, "PLOT")
cond <- dbReadTable(db, "COND")

dbDisconnect(db)

# Load in forest type information from the public database.

filename2 <- "./data/PNW_PUBLIC_SQLite.db"
sqlite.driver <- dbDriver("SQLite")
db2 <- dbConnect(sqlite.driver, dbname = filename2)

ForTypRef <- dbReadTable(db2, "REF_FOREST_TYPE") %>% 
  rename(FORTYPCD = REF_FORTYPCD)

dbDisconnect(db2)

#--------------------------------

ag_fixed_fire %>%
  group_by(PLOT_FIADB) %>%
  mutate(n_FIAfire = n_distinct(disturbyr)) %>%
  ungroup() %>%
  filter(n_FIAfire > 1) %>%
  pull(PLOT_FIADB) %>%
  n_distinct()


cntyCds <- read_csv("./data/ca_cnty_cds.csv", col_names = T, 
                    cols(CNTY_NAME = col_character(), COUNTYCD = col_double())) %>%
  mutate(region = ifelse(COUNTYCD %in% c(15, 23, 93, 105, 89, 103, 45, 21, 11, 33), "klamath", 
                       ifelse(COUNTYCD %in% c(63, 91, 57, 61, 17, 5, 3, 9, 109, 51, 43, 39, 19, 27, 107, 29), "sierra", "other")))

full_plt_dat <- cond %>%
  filter(plot_fiadb %in% cnfrmd_plt_ids)  %>%
  left_join(plot, by = c("plot_fiadb" = "PLOT_FIADB")) %>%
  left_join(ForTypRef, by = "FORTYPCD") %>%
  left_join(cntyCds, by = "COUNTYCD")

full_plt_dat %>%
  group_by(region, SWHW) %>%
  summarize(n = n_distinct(plot_fiadb)) %>%
  arrange(SWHW, -n, region)
