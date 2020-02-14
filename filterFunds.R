names <-c("FUND_HDR", # 15 sec
          "FUND_STYLE", # 17 sec
          "FUND_FEES", # 50 sec
          "FUND_NAMES", #
          "MONTHLY_RETURNS", #
          "MONTHLY_NAV", #
          "MONTHLY_TNA", #
          "MONTHLY_TNA_RET_NAV") #

library(readxl)
j=1
FUND_HDR <-read.csv(paste(names[j],".csv", sep=""))
j=2
FUND_STYLES <-read.csv(paste(names[j],".csv", sep=""))
j=3
FUND_FEES <-read.csv(paste(names[j],".csv", sep=""))
j=4
FUND_NAMES <-read.csv(paste(names[j],".csv", sep=""))
j=5
MONTHLY_RETURNS <-read.csv(paste(names[j],".csv", sep=""))
j=6
MONTHLY_NAV <-read.csv(paste(names[j],".csv", sep=""))
j=7
MONTHLY_TNA<-read.csv(paste(names[j],".csv", sep=""))
j=8
MONTHLY_TNA_RET_NAV<-read.csv(paste(names[j],".csv", sep=""))

# filter starting date ----------------------------------------------------
# caldt after 2000 take it

date_threshold=20000000
which(FUND_HDR$first_offer_dt>date_threshold)
#how many of them
length(which(FUND_HDR$first_offer_dt>date_threshold))
#what percentage of all
100*length(which(FUND_HDR$first_offer_dt>date_threshold))/dim(FUND_HDR)[1]
#apply filter
selected<-FUND_HDR$crsp_fundno[which(FUND_HDR$first_offer_dt>date_threshold)]
#View filtered data
reduced<-FUND_HDR[FUND_HDR$crsp_fundno %in% selected,]
View(reduced)

# ETF and ETN flag filter -------------------------------------------------
#package for is.empty function
# install.packages("sjmisc")
# library(sjmisc)
# discard both etfand etn flag ones
which(FUND_HDR$et_flag =="")

#how many of them
length(which(FUND_HDR$et_flag ==""))
vari1<-"dsf"

#what percentage of all
100*length(which(FUND_HDR$et_flag ==""))/dim(FUND_HDR)[1]

#apply filter
selected<-intersect(selected,FUND_HDR$crsp_fundno[which(FUND_HDR$et_flag =="")])

#View filtered data
reduced<-FUND_HDR[FUND_HDR$crsp_fundno %in% selected,]
View(reduced)

# retail institutional filter ---------------------------------------------
# check retail inst discrepancy
retail<-'N'
institutional<-'Y'
# inst and retail filter
filtered<-which(FUND_HDR$retail_fund ==retail & FUND_HDR$inst_fund==institutional)
#how many of them
length(filtered)
#what percentage of all
100*length(filtered)/dim(FUND_HDR)[1]
#apply filter
selected<-intersect(selected,FUND_HDR$crsp_fundno[filtered])
#View filtered data
View(FUND_HDR[FUND_HDR$crsp_fundno %in% selected,])

# index_fund_flags B D E --------------------------------------------------
# discard all index_fund_flags B D E

# inst and retail filter
filtered<-which(FUND_HDR$index_fund_flag=='')
#how many of them
length(filtered)
#what percentage of all
100*length(filtered)/dim(FUND_HDR)[1]
#apply filter
length(selected)
selected<-intersect(selected,FUND_HDR$crsp_fundno[filtered])
length(selected)
#View filtered data
reduced<-FUND_HDR[FUND_HDR$crsp_fundno %in% selected,]
View(reduced)


# open to investment filter -----------------------------------------------
# open_to_inv is YES is good

summary(FUND_HDR$open_to_inv)

# open to investment filter
filtered<-which(FUND_HDR$open_to_inv=='Y')
#how many of them
length(filtered)
#what percentage of all
100*length(filtered)/dim(FUND_HDR)[1]
#apply filter
selected<-intersect(selected,FUND_HDR$crsp_fundno[filtered])
#View filtered data
reduced<-FUND_HDR[FUND_HDR$crsp_fundno %in% selected,]
View(reduced)
length(selected)


# life time filtering -----------------------------------------------------
# this is for ymd function
#
install.packages("lubridate")

library(lubridate)
#lifetime threshold in years
lifetime_threshold=2

lifetime<-ymd(FUND_HDR$end_dt)-ymd(FUND_HDR$first_offer_dt)
hist(as.numeric(lifetime)/360,200)

# funds older than lifetime_threshold years
filtered<-which((lifetime>lifetime_threshold*360-1))
#how many of them
length(filtered)
#what percentage of all
100*length(filtered)/dim(FUND_HDR)[1]
#apply filter
selected<-intersect(selected,FUND_HDR$crsp_fundno[filtered])
#View filtered data
reduced<-FUND_HDR[FUND_HDR$crsp_fundno %in% selected,]
View(reduced)
length(selected)



# dead or live —— liquidation or merge ------------------------------------

# Liquidation or merge in living ones
summary(reduced$delist_cd[which(reduced$dead_flag=="N")])
# Liquidation or merge in dead ones
summary(reduced$delist_cd[which(reduced$dead_flag=="Y")])
# 3 dead funding without any explanation
which(reduced$delist_cd[which(reduced$dead_flag=="Y")]=="")


# merge_fundno gives info into which fund is merged. for some merged fundings it does not show info (NA)

# merge fundno information
length(which(reduced$merge_fundno!=""))

length(is.na(reduced$merge_fundno))

# merged funding number
length(which(reduced$delist_cd=="M"))

# how many merged but not funding number information ()
length(which(reduced$delist_cd=="M"))-length(which(reduced$merge_fundno!=""))

# how many merged A not funding number information ()
which(is.na(reduced$merge_fundno) & reduced$delist_cd=="M")
which(is. & reduced$delist_cd=="M")

# this is showing one mistake sample, with merge_fundno but with liquidation code rather than merge
View(as.data.frame(reduced$delist_cd[which(!is.na(reduced$merge_fundno))]))


# crsp object code filtering ----------------------------------------------
# think of all sub groups
# install.packages("made4")
obj_code_list <- c("EDCI","EDCL","EDCM","EDCS","EDSA","EDSC","EDSF” “EDSG","EDSH","EDSI","EDSM","EDSN","EDSO","EDSR","EDSS","EDST","EDSU","EDYB","EDYG","EDYH","EDYI","EDYS")


summary(FUND_STYLES$crsp_obj_cd %in% obj_code_list)

# there is no repeat in FUND_HDR
duplicated((FUND_HDR$crsp_fundno))
summary(duplicated((FUND_HDR$crsp_fundno)))

# there are repeats in FUND_STYLES due to crsp object code changes
duplicated((FUND_STYLES$crsp_fundno))
summary(duplicated((FUND_STYLES$crsp_fundno)))

# in FUND_HDR there are some fundnos that does not exist in FUND_STYLES
FUND_HDR$crsp_fundno[which(FUND_HDR$crsp_fundno %notin% intersect(unique(FUND_STYLES$crsp_fundno),unique(FUND_HDR$crsp_fundno)))]


# crsp obj code filtering
filtered<-which(FUND_STYLES$crsp_obj_cd %in% obj_code_list)
#how many of them
length(filtered)
#what percentage of all
100*length(unique(FUND_STYLES$crsp_fundno[filtered]))/length(unique(FUND_STYLES$crsp_fundno))
#apply filter
selected<-intersect(selected,unique(FUND_STYLES$crsp_fundno[filtered]))
#View filtered data
reduced<-FUND_HDR[FUND_HDR$crsp_fundno %in% selected,]
View(reduced)
reducedStyles<-FUND_STYLES[which(FUND_STYLES$crsp_fundno %in% selected),]
View(reducedStyles)
dim(reduced)[1]
