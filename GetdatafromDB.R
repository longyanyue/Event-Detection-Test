library(RPostgreSQL)
require(caTools)
require(gplots)
drv <- dbDriver("PostgreSQL")
con <- dbConnect(drv, host='localhost', port='5432', dbname='postgres test',
                 user='linhao', password='1988130')

## Submit and execute the query
dataset_pH <- dbGetQuery(con, "SELECT * FROM \"sw\".\"Sydney2_pH1\"")
dataset_pH <- dataset_pH[1:30000,]
## fetch all elements from the result set
dbListTables(con)

## Submit and execute the query
event1 <- dbGetQuery(con, "SELECT * FROM \"events\".\"Event4\"")

## Closes the connection
dbDisconnect(con)
## Frees all the resources on the driver
dbUnloadDriver(drv)