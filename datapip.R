library(RSQLite)
library(dbplyr)
library(tidyverse)
library(caret)
library(rpart.plot)
library(knitr)
library(kableExtra)

# Set up
drv = dbDriver('SQLite')
dir = './'
dbFilename = 'FPA_FOD_20170508.sqlite'
db = dbConnect(drv, dbname = file.path(dir, dbFilename))
data = tbl(db, "Fires") %>% collect()

# Export data frames
write.csv(data, 'data.csv')


