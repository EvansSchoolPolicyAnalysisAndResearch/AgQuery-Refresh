library(duckdb)
library(duckplyr)
library(readxl)

if(file.exists("Data/database.duckdb")){
  file.remove("Data/database.duckdb")
}
con <- dbConnect(duckdb(), dbdir="Data/database.duckdb")
indicators <- read_excel("Data/AgDev_Indicator_Estimates.xlsx", 
                                          sheet = "Sheet1")
indicators <- indicators |> mutate(across(mean:max, as.numeric))
indicators <- indicators |> mutate(across(mean:max, ~ signif(.x, 4)))
dbWriteTable(con, "indicators", indicators)

dbDisconnect(con, shutdown=TRUE)

