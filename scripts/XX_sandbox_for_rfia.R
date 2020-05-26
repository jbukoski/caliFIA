# rFIA demo code, exploring rFIA package
# rFIA package documentation: https://rfia.netlify.app/#about

#install.packages("rFIA")

library(rFIA)
library(tidyverse)

# No vignette, look at example usage on this site: https://rfia.netlify.app/#about

data(fiaRI)

class(fiaRI) # Of class "FIA.Database", mode "list" - 19 slots within the list

str(fiaRI)

names(fiaRI)  # Print the names of the tables in the FIA.Database object


#-----------------------------
# Example from https://rfia.netlify.app/courses/plt_est/

data(fiaRI)

# Unique IDs for individual plots

a <- 168263223020004  # One forested condition
b <- 168263219020004  # Two forested, one non-forested condition

# Subset the PLOT, COND, and TREE tables for plot A
plot_a <- filter(fiaRI$PLOT, CN == a)
cond_a <- filter(fiaRI$COND, PLT_CN == a)
tree_a <- filter(fiaRI$TREE, PLT_CN == a)

# Subset the PLOT, COND, and TREE tables for plot B
plot_b <- filter(fiaRI$PLOT, CN == b)
cond_b <- filter(fiaRI$COND, PLT_CN == b)
tree_b <- filter(fiaRI$TREE, PLT_CN == b)

## COND_STATUS_CD indicates the basic land classification of a 
## condition, whether it is forested, non-forest, water, etc...
## COND_STATUS_CD = 1 indicates forested

cond_a$COND_STATUS_CD

cond_b$COND_STATUS_CD

# FORTYPCD indicates the forest type of the condition
# PHYSCLCD indicates the Physiographic class (e.g. mesic moist slope)

cond_b$FORTYPCD

cond_b$PHYSCLCD

# Basic estimation procedures

tbl_a <- plot_a %>%
  mutate(PLT_CN = CN) %>%
  left_join(cond_a, by = 'PLT_CN') %>%
  left_join(tree_a, by = c('PLT_CN', 'CONDID'))
    
tbl_b <- plot_b %>%
  mutate(PLT_CN = CN) %>%
  left_join(cond_b, by = 'PLT_CN') %>%
  left_join(tree_b, by = c('PLT_CN', 'CONDID'))


