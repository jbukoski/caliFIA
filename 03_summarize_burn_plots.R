# A script to summarize the reburn and single burn plots

library(readxl)
library(RSQLite)
library(tidyverse)

# Load in the table of single burn plots

singles <- read_csv("./data/processed/conf_singles.csv")
reburns <- read_csv("./data/processed/conf_reburns.csv")

# Load in PLOT & COND table from Jeremy to get plot traits

filename <- "./data/TeamPotts2.db"
sqlite.driver <- dbDriver("SQLite")
db <- dbConnect(sqlite.driver, dbname = filename)

plot <- rename_all(dbReadTable(db, "PLOT"), tolower)
cond <- rename_all(dbReadTable(db, "COND"), tolower)
ForTypRef <- rename_all(read_csv("./data/processed/forestTypeRef.csv"), tolower)
cntyCds <- read_csv("./data/ca_cnty_cds.csv", col_names = T, cols(CNTY_NAME = col_character(), COUNTYCD = col_double())) %>%
  rename_all(tolower)

dbDisconnect(db)

#-----------------------------

cols <- colnames(singles)

sngls_jnd <- singles %>%
  left_join(plot, by = c("plot_fiadb", "invyr", "measyear")) %>%
  left_join(cond, by = c("plot_fiadb", "condid", "invyr")) %>%
  left_join(ForTypRef, "fortypcd") %>%
  left_join(cntyCds) %>%
  select(cols, countycd, cnty_name, fortypcd, swhw, owngrpcd)

sngls_jnd

sngls_jnd %>%
  group_by(owngrpcd, swhw) %>%
  summarize(n_plts = n_distinct(plot_fiadb)) %>%
  arrange(swhw, owngrpcd, -n_plts) %>%
  View

#-----------------------
# Reburn plots

rbrns_jnd <- reburns %>%
  left_join(plot, by = c("plot_fiadb", "invyr", "measyear")) %>%
  left_join(cond, by = c("plot_fiadb", "condid", "invyr")) %>%
  left_join(ForTypRef, "fortypcd") %>%
  left_join(cntyCds) %>%
  select(cols, countycd, cnty_name, fortypcd, swhw, owngrpcd)

rbrns_jnd %>%
  group_by(owngrpcd, swhw) %>%
  summarize(n_plts = n_distinct(plot_fiadb)) %>%
  arrange(swhw, owngrpcd, -n_plts) %>%
  View

#-----------------------
# Evan's code for generating maps

CAcounty = readOGR("./data/ca-county-boundaries/CA_Counties/CA_Counties_TIGER2016.shp")

# Build a function to calculate totals for singles or reburns
# Takes in a df (either reburns or singles)

prepDatforPlot <- function(df) {

  plot_join <- df %>% 
    left_join(cond) %>%
    left_join(plot, by = c("plot_fiadb", "invyr", "measyear")) %>%
    left_join(ForTypRef) %>%
    left_join(select(cntyCds, cnty_name, countycd), by = "countycd") %>%
    distinct(plot_fiadb, .keep_all = TRUE) %>%
    select(1:11, owngrpcd, fortypcd, countycd, cnty_name, swhw) %>%
    filter(!is.na(plot_fiadb), !is.na(countycd)) # loses 4 plots
  
  plts_per_cnty <- plot_join %>% 
    group_by(swhw, cnty_name) %>%
    mutate(sw_n = ifelse(swhw == "1_Softwoods", n_distinct(plot_fiadb), 0),
           hw_n = ifelse(swhw == "2_Hardwoods", n_distinct(plot_fiadb), 0),
           ns_n = ifelse(swhw == "3_Nonstocked", n_distinct(plot_fiadb), 0),
           na_n = ifelse(is.na(swhw), n_distinct(plot_fiadb), 0)) %>%
    ungroup() %>%
    group_by(cnty_name) %>%
    mutate(ttl_n = n_distinct(plot_fiadb)) %>%
    group_by(cnty_name) %>%
    mutate(sw_n = max(sw_n, na.rm = T), 
           hw_n = max(hw_n, na.rm = T),
           ns_n = max(ns_n, na.rm = T),
           na_n = max(na_n, na.rm = T),
           ttl_n = max(ttl_n, na.rm = T)) %>%
    arrange(cnty_name)
  
  cntyJoin <- cntyData %>% 
    left_join(select(plts_per_cnty, countycd, sw_n, hw_n, ns_n, na_n, ttl_n)) %>%
    distinct() %>%
    mutate(cnty_name = as.factor(cnty_name))
  
  return(cntyJoin)
  
}

dat2plot <- CAcounty

dat2plot@data <- CAcounty@data %>%
  left_join(prepDatforPlot(singles), by = c("NAME" = "cnty_name")) %>%
  left_join(prepDatforPlot(reburns), by = c("NAME" = "cnty_name"), suffix = c("_sngl", "_rbrn")) %>%
  as.data.frame()

# using tmap function qtm, you can display plot density easily

sw_sngl <- qtm(dat2plot, "sw_n_sngl", fill.n = 8) +
  tm_layout(main.title = "Single Softwoods")

hw_sngl <- qtm(dat2plot, "hw_n_sngl", fill.n = 8) +
  tm_layout(main.title = "Single Hardwoods")

sw_rbrn <- qtm(dat2plot, "sw_n_rbrn", fill.n = 8) +
  tm_layout(main.title = "Reburn Softwoods")

hw_rbrn <- qtm(dat2plot, "hw_n_rbrn", fill.n = 8) +
  tm_layout(main.title = "Reburn Hardwoods")

plt <- tmap_arrange(sw_sngl, hw_sngl, sw_rbrn, hw_rbrn, ncol = 2, nrow = 2)

tmap_save(plt, filename = "./figs/plot_maps.jpg", height = 8, width = 8, units = "in")

