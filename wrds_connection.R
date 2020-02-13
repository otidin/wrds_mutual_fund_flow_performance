# wrds connection via RPostgres ---------------------------------------------------------
# install.packages("RPostgres")
# install.packages("tictoc")
library(RPostgres)
library(tictoc)

wrds <- dbConnect(Postgres(),
                  host='wrds-pgdata.wharton.upenn.edu',
                  port=9737,
                  dbname='wrds',
                  sslmode='require',
                  user='username')

sql <- "select * from CRSP.MSF"#the query to be passed to WRDS
# sql <- "select * from CRSPQ.MONTHLY_RETURNS"
# sql <- "select * from CRSP.MONTHLY_TNA_RET_NAV" #the query to be passed to WRDS
# sql <- "select * from CRSP.FUND_HDR" #the query to be passed to WRDS
# sql <- "select * from CRSP.MSF"
