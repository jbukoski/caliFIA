# A script to summarize the reburn and single burn plots

library(ggthemes)
library(readxl)
library(rgdal)
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
  select(cols, countycd, cnty_name, fortypcd, swhw, owngrpcd, cycle, subcycle)

rbrns_jnd %>%
  group_by(owngrpcd, swhw) %>%
  summarize(n_plts = n_distinct(plot_fiadb)) %>%
  arrange(swhw, owngrpcd, -n_plts) %>%
  View

#-----------------------
# Evan's code for generating maps

library(rgdal)
library(tmap)

CAcounty = readOGR("./data/ca-county-boundaries/CA_Counties/CA_Counties_TIGER2016.shp")

cntyData <- CAcounty@data %>% 
  as.data.frame() %>%
  select(NAME, COUNTYFP) %>%
  rename_all(tolower) %>%
  mutate(name = as.character(name)) %>%
  left_join(cntyCds, by = c("name" = "cnty_name"))
  

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


#----------------------------------------------
# Plot burn dates and inventory dates for single burn plots and reburn plots

sngls2plt <- sngls_jnd %>%
  select(plot_fiadb, invyr, fia_fire, swhw, countycd, cnty_name) %>%
  group_by(plot_fiadb) %>%
  filter("1_Softwoods" %in% swhw, !is.na(swhw), invyr < 2020) %>%
  distinct() %>%
  ungroup() %>%
  group_by(cnty_name) %>%
  mutate(pltsPerCnty = n_distinct(plot_fiadb)) %>%
  ungroup() %>%
  filter(pltsPerCnty > 2) %>%
  mutate(plot_fiadb = factor(plot_fiadb)) %>%
  arrange(countycd, plot_fiadb, fia_fire)


rbrns2plt <- rbrns_jnd %>%
  select(plot_fiadb, invyr, measyear, fia_fire, swhw, countycd, cnty_name) %>%
  group_by(plot_fiadb) %>%
  filter("1_Softwoods" %in% swhw) %>%
  mutate(diff = max(fia_fire) - min(fia_fire)) %>%
  distinct() %>%
  ungroup() %>%
  mutate(invyr = ifelse(is.na(invyr), measyear, invyr)) %>%
  mutate(plot_fiadb = factor(plot_fiadb)) %>%
  arrange(countycd, plot_fiadb, fia_fire)
  

fig1_sngls <- sngls2plt %>%
  filter(cnty_name == "Siskiyou") %>%
  ggplot(.) +
  facet_wrap(cnty_name ~ ., scales = "free_y") +
  geom_point(aes(y = reorder(plot_fiadb, fia_fire), x = fia_fire, col = swhw)) +
  geom_point(aes(y = reorder(plot_fiadb, fia_fire), x = invyr, col = swhw), shape = "triangle", alpha = 0.5) +
  ylab("Plot code") +
  xlab("Burn year") +
  scale_x_continuous(breaks = seq(1960, 2020, by = 10)) +
  ggtitle("Plots with confirmed single burns") +
  theme_bw() +
  theme(legend.position = "bottom")

fig2_rbrns <- rbrns2plt %>%
  filter(cnty_name == "Siskiyou") %>%
  ggplot(.) +
  facet_wrap(cnty_name ~ ., scales = "free_y") +
  geom_point(aes(y = reorder(plot_fiadb, fia_fire), x = fia_fire, col = swhw)) +
  geom_point(aes(y = reorder(plot_fiadb, fia_fire), x = invyr, col = swhw), shape = "triangle", alpha = 0.5) +
  ylab("Plot code") +
  xlab("Burn year") +
  scale_x_continuous(breaks = seq(1960, 2020, by = 10)) +
  ggtitle("Plots with confirmed reburns") +
  theme_bw() +
  theme(legend.position = "bottom")

ggsave("./figs/fig1_sngls.jpg", fig1_sngls, device = "jpeg", width = 6, height = 6, units = "in")
ggsave("./figs/fig2_rbrns.jpg", fig2_rbrns, device = "jpeg", width = 6, height = 6, units = "in")
