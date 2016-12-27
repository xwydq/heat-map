options(stringsAsFactors=FALSE, scipen=10, digits=4)
library(readxl)
library(DBI)
library(ROracle)
library(dplyr)
library(lubridate)
library(stringr)

setwd("~/R_test/remap")

source("tools_func_loc.R")

userdat <- read_excel("user.xlsx")
userdat$ADDRESS_CLEAN <- with(userdat, paste0(常驻国家, 市))
userdat$ADDRESS_CLEAN <- with(userdat, paste(常驻国家, 市, sep="%20"))

userdat$LAT <- NA
userdat$LNG <- NA

for(i in 1:nrow(userdat)){
  print(i)
  vip_addr <- userdat$ADDRESS_CLEAN[i]
  vip_addr_geo <- get_loc_rep(vip_addr)
  
  userdat$LAT[i] <- vip_addr_geo$lat
  userdat$LNG[i] <- vip_addr_geo$lng
}

userdat2 <- userdat %>%
  filter(is.na(LAT))

userdat1 <- userdat %>%
  filter(!is.na(LAT)) %>%
  rename(TIME_IN=加入时间, COUNTRY=常驻国家, CITY=市)

summary(userdat1)

write.csv(userdat1, "userdat.csv", row.names = FALSE)


# ##################
# school <- read_excel("school.xlsx")
# names(school) <- c("TIME_IN", "SCHOOL_NM", "BUSINESS")
# 
# school <- school %>%
#   filter(!is.na(SCHOOL_NM)) %>%
#   filter(!is.na(BUSINESS))
# 
# 
# userdat$ADDRESS_CLEAN <- with(userdat, paste0(常驻国家, 市))
# userdat$ADDRESS_CLEAN <- with(userdat, paste(常驻国家, 市, sep="%20"))