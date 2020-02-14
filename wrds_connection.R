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


names <-                 c("FUND_HDR", # 15 sec
                           "FUND_STYLE", # 17 sec
                           "FUND_FEES", # 50 sec
                           "FUND_NAMES", #
                           "MONTHLY_RETURNS", #
                           "MONTHLY_NAV", #
                           "MONTHLY_TNA", #
                           "MONTHLY_TNA_RET_NAV") #


names <-                 c("FUND_HDR", # 15 sec
                           "FUND_STYLE", # 17 sec
                           "FUND_FEES", # 50 sec
                           "FUND_NAMES", #
                           "MONTHLY_RETURNS", #
                           "MONTHLY_NAV", #
                           "MONTHLY_TNA", #
                           "MONTHLY_TNA_RET_NAV") #

i=1
sql<-paste("select * from CRSP.",names[i], sep="")
tic()
res <- dbSendQuery(wrds, sql)
data<- dbFetch(res, n=40)
# data<- dbFetch(res)
toc()
dbClearResult(res)
View(data)


for(i in 1:8) {
  sql<-paste("select * from CRSP.",names[i], sep="")
  tic()
  res <- dbSendQuery(wrds, sql)
  data[i]<- dbFetch(res, n=100)
  toc()
  dbClearResult(res)
  View(data)
}

