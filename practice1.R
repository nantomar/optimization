x=data.frame(x=1:5,x2=6:10)
y=data.frame(z=11:15)
cbind(x,y)
x=rbind(x,  c(1,6))
library(dplyr)
distinct(x)
select(,contains("sal"))
# make two vectors and combine them as columns in a data.frame
sport <- c("Hockey", "Baseball", "Football")
league <- c("NHL", "MLB", "NFL")
trophy <- c("Stanley Cup", "Commissioner's Trophy", "Vince Lombardi Trophy")
trophies1 <- cbind(sport, league, trophy)
trophies1
# make another data.frame using data.frame()
trophies2 <- data.frame(sport=c("Basketball", "Golf"), league=c("NBA", "PGA"),
                        trophy=c("Larry Brien Championship Trophy", "Wanamaker Trophy"),
                        stringsAsFactors=FALSE)
trophies2
# combine them into one data.frame with rbind

