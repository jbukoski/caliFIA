# Data exploration with CA FIA dataset


#-----------------------------
# Load FIA library

library(rFIA)

#-----------------------------
# Download all FIA data for California, uncomment to download data 

# dat <- getFIA( states = "CA", dir = './data/')

#-----------------------------
# After downloading data and saving to file, can simply load data

dat <- readFIA("./data/")

#-----------------------------
# View the names of the tables within the object

names(dat)

# Extracting tables from the FIA.Database object returns data frames

plotDat <- dat$PLOT

# Extract number of plots in CA

length(unique(plotDat$PLOT))

# Extract years of inventory from CA

sort(unique(plotDat$INVYR))

# Odd that there's a



