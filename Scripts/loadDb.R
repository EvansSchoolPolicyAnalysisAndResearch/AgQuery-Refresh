library(duckdb)
library(duckplyr)
library(readxl)

con <- dbConnect(duckdb(), dbdir="Data/database.duckdb", read_only=F)
indicators <- read_excel("Data/EPAR_UW_335_AgDev_Indicator_Estimates.xlsx", 
                                          sheet = "Estimates by Instrument")
dbWriteTable(con, "indicators", indicators)

dbDisconnect(con)