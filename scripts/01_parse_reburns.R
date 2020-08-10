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

# Load in complementary databases

ForTypRef <- read_csv("./data/processed/forestTypeRef.csv") %>%
  rename_all(tolower)

cntyCds <- read_csv("./data/ca_cnty_cds.csv", col_names = T, 
                    cols(CNTY_NAME = col_character(), COUNTYCD = col_double()))

# Load in table of reburns (main table)

singles <- read_csv("./data/processed/singles.csv") %>%
  mutate(intensity = as.character(intensity))

reburns <- read_csv("./data/processed/reburns.csv") %>%
  mutate(intensity = as.character(intensity))

#--------------------------
#Joined data to process

dat2process <- reburns %>%
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
         n_fersfires, match, n_match, fortypcd, owngrpcd, swhw)

# 344 candidate reburn plots
n_distinct(dat2process$plot_fiadb)

#---------------------------------------------------
#####################
## Confirmed Plots ##
#####################

# Confirmed dataframe, both FIA fire years confirmed by fire perimeter or FERS
# fire years.

confirmed <- dat2process %>%
  filter(match == TRUE, n_match == 1, n_fiafires == 2) %>%
  select(plot_fiadb, invyr, measyear, condid, anncd, fia_fire) %>%
  arrange(plot_fiadb, measyear, fia_fire) %>%
  mutate(df = "cnfrmd", 
         fireYrSrc = "FIADB",
         confidence = 3,
         evidence = "both years confirmed by fire perimeter years",
         notes = NA
         ) %>%
  distinct()

#---------------------------------------------------
############################
## Partly Confirmed Plots ##
############################

prtly_cnfrmd <- dat2process %>%
  filter(n_match == 2)

length(unique(prtly_cnfrmd$plot_fiadb))

# Rule 1, for plots with 2 FIA fires, 1 perimeter fire year, and the most 
# recent burn was confirmed by the perimeter fire year, keep the plot FIA
# years as long as the distance between the fire years is greater than 5 
# years. No second perimeter fire year to confirm.

prtly_p1 <- prtly_cnfrmd %>%
  group_by(plot_fiadb) %>%
  filter(n_fiafires == 2 & n_perimfires == 1 & 
           (max(fia_fire) %in% perim_fire | max(fia_fire) %in% fers_fire) & 
           (!(min(fia_fire) %in% perim_fire) | !(min(fia_fire %in% fers_fire)))) %>%
  mutate(dstncBtwnFires1 = abs(fia_fire - perim_fire),
         dstncBtwnFires2 = abs(fia_fire - fers_fire)) %>%
  ungroup() %>%
  rowwise() %>%
  mutate(dstncBtwnFires = max(dstncBtwnFires1, dstncBtwnFires2, na.rm = T)) %>%
  select(-dstncBtwnFires1, -dstncBtwnFires2) %>%
  ungroup() %>%
  group_by(plot_fiadb) %>%
  mutate(n_visit = as.numeric(factor(measyear)),
         keep = match,
         keep = ifelse(match == FALSE & dstncBtwnFires > 5, TRUE, keep),
         n_keep = n_distinct(keep)) %>%
  ungroup()

prtly_list1 <- prtly_p1 %>%
  filter(keep == TRUE & n_keep == 1) %>%
  select(colnames(confirmed[1:6])) %>%
  unique() %>%
  mutate(fireYrSrc = "FIADB",
         df = "prtly_cnfrmd",
         confidence = 3,
         evidence = "rule 1 - if most recent burn confirmed perimeter and 
         distance between burns > 5 yrs, keep both",
         notes = NA)

confirmed <- add_row(confirmed, prtly_list1)

# Rule 2, for plots with two inventories and two disturbance years, but the burn
# years are less than 5 years apart and the second burn year is both (a)
# unconfirmed and (b) precedes the second burn year, adjust the second burn
# year to the confirmed fire perimeter burn year.
# Will shift plots to "single burn" category.

prtly_p2 <- prtly_p1 %>%
  filter(n_keep == 2) %>%
  arrange(plot_fiadb, measyear) %>%
  group_by(plot_fiadb) %>%
  mutate(n_records = n(),
         keep = ifelse(n_records == 2 & fia_fire == min(fia_fire) & n_visit == 2, "FERS", "FIADB"),
         fia_fire = ifelse(keep == "FERS", fers_fire, fia_fire)) %>%
  group_by(plot_fiadb) %>%
  mutate(n_fires = n_distinct(fia_fire))

prtly_list2 <- prtly_p2 %>%
  filter(n_fires == 2) %>%
  rename(fireYrSrc = keep) %>%
  select(colnames(confirmed[1:6])) %>%
  mutate(df = "prtly_cnfrmd",
         confidence = 0,
         evidence = "rule 2",
         notes = NA) %>%
  unique()
  

#confirmed <- add_row(confirmed, prtly_list2)

# Rule 3, for plots that have 2 FIA fire years, 2 perimeter fire years, the 
# more recent burn confirmed by a fire perimeter year, the historical burn 
# before 1990, and the distance between the oldest burns is <= 5 years, assign
# the oldest perimeter burn year to the historical fire year slot.

prtly_p3a <- prtly_cnfrmd %>%
  filter(!(plot_fiadb %in% confirmed$plot_fiadb)) %>%
  filter(n_fiafires == 2 & n_perimfires == 2) %>%
  group_by(plot_fiadb) %>%
  mutate(n_perimfires = n_distinct(perim_fire),
         dstnc_minFireYr = abs(min(fia_fire) - min(perim_fire))) %>%
  filter((min(fia_fire) < 1990) & 
         (max(fia_fire) == max(perim_fire) & 
         (min(fia_fire) != min(perim_fire))) & 
         dstnc_minFireYr <= 5) %>%
  mutate(fireYrSrc = ifelse(match == TRUE, "FIADB", "PERIM"),
         fia_fire = ifelse(match == TRUE, fia_fire, min(perim_fire))) %>%
  ungroup()

prtly_list3a <- prtly_p3a %>%
  select(colnames(confirmed[1:6]), fireYrSrc) %>%
  mutate(df = "prtly_cnfrmd",
         confidence = 2,
         evidence = "rule 3a - most recent burn confirmed by perim, 
         older burn less than 5 yrs diff from perim data",
         notes = NA) %>%
  unique()
  
confirmed <- add_row(confirmed, prtly_list3a)  

# Rule 3A, for plots that have 2 FIA fire years, 2 perimeter fire years, the 
# more recent burn confirmed by a fire perimeter year, the historical burn 
# before 1990, and the distance between the oldest burns is > 5 years & < 10
# years, assign the oldest perimeter burn year to historical fire year slot.

prtly_p3b <- prtly_cnfrmd %>%
  filter(!(plot_fiadb %in% confirmed$plot_fiadb)) %>%
  filter(n_fiafires == 2 & n_perimfires == 2) %>%
  group_by(plot_fiadb) %>%
  mutate(n_perimfires = n_distinct(perim_fire),
         dstnc_minFireYr = abs(min(fia_fire) - min(perim_fire))) %>%
  filter((min(fia_fire) < 1990) & 
           (max(fia_fire) == max(perim_fire) & 
              (min(fia_fire) != min(perim_fire))) & 
           dstnc_minFireYr > 5 & dstnc_minFireYr <= 10) %>%
  mutate(fireYrSrc = ifelse(match == TRUE, "FIADB", "PERIM"),
         fia_fire = ifelse(match == TRUE, fia_fire, min(perim_fire))) %>%
  ungroup()

prtly_list3b <- prtly_p3b %>%
  select(colnames(confirmed[1:6]), fireYrSrc) %>%
  mutate(df = "prtly_cnfrmd",
         confidence = 2,
         evidence = "rule 3b - most recent burn confirmed by perim, older 
         burn >5 & <10 yrs diff from perim data",
         notes = NA) %>%
  unique()

confirmed <- add_row(confirmed, prtly_list3b) 

# Rule 3c, for plots that have 2 FIA fire years, 2 perimeter fire years, the 
# more recent burn confirmed by a fire perimeter year, the historical burn 
# before 1990, and the distance between the oldest burns is > 10 years, 
# assign the oldest perimeter burn year to historical fire year slot.

prtly_p3c <- prtly_cnfrmd %>%
  filter(!(plot_fiadb %in% confirmed$plot_fiadb)) %>%
  filter(n_fiafires == 2 & n_perimfires == 2) %>%
  group_by(plot_fiadb) %>%
  mutate(n_perimfires = n_distinct(perim_fire),
         dstnc_minFireYr = abs(min(fia_fire) - min(perim_fire))) %>%
  filter((min(fia_fire) < 1990) & 
           (max(fia_fire) == max(perim_fire) & 
              (min(fia_fire) != min(perim_fire))) & 
           dstnc_minFireYr > 10) %>%
  mutate(fireYrSrc = ifelse(match == TRUE, "FIADB", "PERIM"),
         fia_fire = ifelse(match == TRUE, fia_fire, min(perim_fire))) %>%
  ungroup()

prtly_list3c <- prtly_p3c %>%
  select(colnames(confirmed[1:6]), fireYrSrc) %>%
  mutate(df = "prtly_cnfrmd",
         confidence = 1,
         evidence = "rule 3c - most recent burn confirmed by perim, oldest 
         burn > 10 yrs diff from perim data",
         notes = NA) %>%
  unique()

confirmed <- add_row(confirmed, prtly_list3c)

# Rule 4, for plots with 2 inventory years, 2 FIA fire years, 2 perimeter 
# fire yers and the difference between the oldest FIA fire and oldest perim
# fire year <= 10 years, & oldest burn > 1990, substitute min(perim_fire)
# for min(fia_fire).

prtly_p4 <- prtly_cnfrmd %>%
  filter(!(plot_fiadb %in% confirmed$plot_fiadb)) %>%
  group_by(plot_fiadb) %>%
  filter(!any(fia_fire == 9999)) %>%
  mutate(n_measyear = n_distinct(measyear)) %>%
  filter(n_measyear == 2 & n_fiafires == 2 & n_perimfires == 2) %>%
  mutate(dstncBtwnMinFireYrs = abs(min(fia_fire) - min(perim_fire))) %>%
  filter(dstncBtwnMinFireYrs < 10 & any(match == FALSE & fia_fire == min(fia_fire))) %>%
  mutate(fireYrSrc = ifelse(match == FALSE, "PERIM", "FIADB"),
         fia_fire = ifelse(fireYrSrc == "PERIM", min(perim_fire), fia_fire))

prtly_list4 <- prtly_p4 %>%
  select(colnames(confirmed[1:6]), fireYrSrc) %>%
  mutate(df = "prtly_cnfrmd",
         confidence = 2,
         evidence = "rule 4 - plots with older fia burns since 1990, most recent burn 
         confirmed by perim, older burn less than 5 yrs diff from perim data",
         notes = NA) %>%
  unique()

confirmed <- add_row(confirmed, prtly_list4)

# Rule 5, fire plots with 2 FIA fires and 2 or more perimeter fire years, calculate
# the minimum distance between the FIA fires and perimeter fire year candidates and
# keep FIADB burn yr if exact match, otherwise substitute perimeter fire year

prtly_p5 <- prtly_cnfrmd %>%
  filter(!(plot_fiadb %in% confirmed$plot_fiadb)) %>%
  filter(n_fiafires == 2 & n_perimfires >= 2) %>%
  group_by(plot_fiadb) %>%
  filter(!any(fia_fire == 9999)) %>%
  group_by(plot_fiadb, measyear, condid, anncd, fia_fire) %>%
  mutate(diff = abs(fia_fire - perim_fire)) %>%
  filter(diff == min(diff)) %>%
  group_by(plot_fiadb) %>%
  mutate(n_perimfires = n_distinct(perim_fire)) %>%
  filter(!any(diff >= 10) & min(fia_fire) <= 2000 & n_perimfires == 2) %>%
  mutate(fireYrSrc = ifelse(fia_fire == perim_fire, "FIADB", "PERIM"),
         fia_fire = ifelse(fireYrSrc == "PERIM", perim_fire, fia_fire))

prtly_list5 <- prtly_p5 %>%
  select(colnames(confirmed[1:6]), fireYrSrc) %>%
  mutate(df = "prtly_cnfrmd",
         confidence = 2,
         evidence = "rule 5 - Similar to rule 3b, but catching plots that were not caught in those filters",
         notes = NA) %>%
  unique()

confirmed <- add_row(confirmed, prtly_list5)

# Rule 6, For plots with two perimeter fire years, two FIA fire years
# with one FIA fire yr confirmed and one FIA fire yr as 9999, replace the
# 9999 fire year with the second perimeter fire year.

prtly_p6 <- prtly_cnfrmd %>%
  filter(!(plot_fiadb %in% confirmed$plot_fiadb)) %>%
  group_by(plot_fiadb) %>%
  filter(n_perimfires == 2 & any(fia_fire == 9999)) %>%
  mutate(fireYrSrc = ifelse(fia_fire == 9999, "PERIM", "FIADB"),
         fia_fire = ifelse(fireYrSrc == "PERIM", min(perim_fire), fia_fire))

prtly_list6 <- prtly_p6 %>%
  select(colnames(confirmed[1:6]), fireYrSrc) %>%
  mutate(df = "prtly_cnfrmd",
         confidence = 1,
         evidence = "rule 6 - substitute second perimeter year for 9999 disturbance year values",
         notes = NA) %>%
  unique()

confirmed <- add_row(confirmed, prtly_list6)

#-----------------------------

prtly_p7 <- prtly_cnfrmd %>%
  filter(!(plot_fiadb %in% confirmed$plot_fiadb)) %>%
  filter(n_fiafires == 2) %>%
  group_by(plot_fiadb) %>%
  mutate(fia_diff = max(fia_fire) - min(fia_fire)) %>%
  filter(fia_diff <= 5, plot_fiadb != 79088) %>%
  filter(min(fia_fire) %in% perim_fire) %>%
  mutate(fireYrSrc = ifelse(fia_fire == perim_fire, "FIADB", "PERIM")) %>%
  filter(fireYrSrc == "FIADB") %>%
  mutate(n_fiafires = n_distinct(fia_fire)) %>%
  distinct()

new_singles <- singles %>%
  bind_rows(prtly_p7)
  
#View(new_singles)

#-----------------------------

prtly_p8 <- prtly_cnfrmd %>%
  filter(!(plot_fiadb %in% confirmed$plot_fiadb) & !(plot_fiadb %in% new_singles$plot_fiadb))

#View(prtly_p8)

# How to handle fires with just one burn?

singlePerim <- prtly_p8 %>%
  filter(n_perimfires == 1)

n_distinct(singlePerim$plot_fiadb)


# Remaining with multiple fire perimeter burn years:

prtly_forAndy <- prtly_p8 %>%
  select(plot_fiadb:n_fersfires)%>%
  mutate(fireYrSrc = NA) %>%
  left_join(plot) %>%
  left_join(cond, by = c("plot_fiadb", "condid", "invyr")) %>%
  left_join(ForTypRef) %>%
  left_join(rename_all(cntyCds, tolower)) %>%
  mutate(fireYrSrc = "", confidence = "", evidence = "", notes = "") %>%
  select(colnames(prtly_p6[c(1:13, 16:19)]), confidence, evidence, notes) %>%
  group_by(plot_fiadb)

n_distinct(prtly_forAndy$plot_fiadb)

write_csv(distinct(filter(prtly_forAndy, "1_Softwoods" %in% swhw)), "./data/forAndy/01_prtly_confirmed_SW.csv")
write_csv(distinct(filter(prtly_forAndy, !("1_Softwoods" %in% swhw))), "./data/forAndy/03_prtly_confirmed_other.csv")

#-----------------------------------
#######################
## Unconfirmed Plots ##
#######################

uncnfrmd <- dat2process %>%
  filter(match == FALSE, n_match == 1) 

length(unique(uncnfrmd$plot_fiadb))

# Rule 1, If plots have two fia fire years, two perimeter fire years, the
# maximum of the perimeter fire year is not greater than the maximum of the inventory
# year, and the years do not match exactly, substitute the perimeter fire years
# for the FIA fire years.

un_p1 <- uncnfrmd %>%
  filter(n_fiafires == 2 & n_perimfires == 2) %>%
  group_by(plot_fiadb) %>%
  filter(max(perim_fire) <= max(invyr)) %>%
  mutate(max_diff = abs(max(perim_fire) - max(fia_fire)),
         min_diff = abs(min(perim_fire) - min(fia_fire))) %>%
  mutate(fireYrSrc = "PERIM") %>%
  mutate(fia_fire = ifelse(fia_fire == max(fia_fire), max(perim_fire), min(perim_fire)))

un_l1 <- un_p1 %>%
  select(colnames(confirmed[1:6]), fireYrSrc) %>%
  mutate(df = "prtly_cnfrmd", 
         confidence = 1,
         evidence = "rule 1 (unconfirmed) - substitute perimeter years for fia fire years if max(perim) < max(invyr) ",
         notes = NA)  %>%
  unique()

confirmed <- add_row(confirmed, un_l1)

#-------------------

un_p2 <- uncnfrmd %>%
  filter(!(plot_fiadb %in% confirmed$plot_fiadb)) %>%
  select(plot_fiadb:n_fersfires) %>%
  #filter(n_fiafires == 2) %>%
  group_by(plot_fiadb) %>%
  mutate(fia_diff = max(fia_fire) - min(fia_fire))

# Remaining unconfirmed plots

un_p2 <- uncnfrmd %>%
  filter(!(plot_fiadb %in% confirmed$plot_fiadb)) %>%
  select(plot_fiadb:n_fersfires) %>%
  mutate(fireYrSrc = NA) %>%
  left_join(plot) %>%
  left_join(cond, by = c("plot_fiadb", "condid", "invyr")) %>%
  left_join(ForTypRef) %>%
  left_join(rename_all(cntyCds, tolower)) %>%
  mutate(fireYrSrc = "", confidence = "", evidence = "", notes = "") %>%
  select(colnames(prtly_p6[c(1:13, 16:19)]), confidence, evidence, notes) %>%
  group_by(plot_fiadb)

write_csv(distinct(filter(un_p2, "1_Softwoods" %in% swhw)), "./data/forAndy/02_unconfirmed_SW.csv")
write_csv(distinct(filter(un_p2, !("1_Softwoods" %in% swhw))), "./data/forAndy/04_unconfirmed_other.csv")


#-------------------------
# Write out the confirmed reburns

write_csv(arrange(confirmed, plot_fiadb, measyear, fia_fire), "./data/processed/conf_reburns.csv")
