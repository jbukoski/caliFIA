# A script to examine the 22 leftover plots

library(readxl)
library(rgdal)
library(RSQLite)
library(tidyverse)

# Go into the public database to get what tree records we have

periodicDat <- "/media/jbukoski/9E25-21B8/cafia/CA_occ2_3.db"

sqlite.driver <- dbDriver("SQLite")
db <- dbConnect(sqlite.driver, dbname = periodicDat)

sort(dbListTables(db))

plot <- rename_all(dbReadTable(db, "PARTIAL_SOFT_PLOT"), tolower)
cond <- rename_all(dbReadTable(db, "PARTIAL_SOFT_COND"), tolower)

dbDisconnect(db)

  
dat <- plot %>%
  left_join(cond, by = c("plot_fiadb", "invyr")) %>%
  select(plot_fiadb, invyr, measyear, condid, notes.x, notes.y, plot_narrative, 
         dstrbyr1, dstrbyr2, dstrbyr3, hist_dstrbyr1_pnwrs, hist_dstrbyr2_pnwrs, hist_dstrbyr3_pnwrs) %>%
  arrange(plot_fiadb, invyr)

View(dat)

dat <- read_excel("./data/plotsForJeremy.xlsx")

dat2 <- dat %>%
  distinct()

write_csv(dat2, "./data/plotsForJeremy.csv")
