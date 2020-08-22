# reset to default
closeAllConnections()
rm(list=ls())

setwd("C:\\Users\\benny\\OneDrive\\Desktop\\github\\CAPM")

library(data.table)

ETF_18_19 <- fread("TAQ_20182019.csv.gz", header=TRUE,
                   colClasses=c(DATE='character',
                                CPrc='numeric',
                                symbol='character',
                                total_vol_m = 'numeric',
                                total_dollar_m = 'numeric'))
ETF_18_19$DATE <- as.Date(as.character(ETF_18_19$DATE),format='%Y%m%d')
ETF_19_20 <- fread("TAQ_20192020.csv.gz", header=TRUE,
                   colClasses=c(DATE='character',
                                CPrc='numeric',
                                symbol='character',
                                total_vol_m = 'numeric',
                                total_dollar_m = 'numeric'))
ETF_19_20$DATE <- as.Date(as.character(ETF_19_20$DATE),format='%Y%m%d')


riskless_18_19 <- data.table(read.csv("Riskless_2018-19.csv", header=TRUE, na.strings = "."))
riskless_18_19$DATE <- as.Date(as.character(riskless_18_19$DATE))
#get rid of blank days
riskless_18_19 <- riskless_18_19[as.vector(!is.na(riskless_18_19[,2]))]
riskless_18_19$DGS1MO <- riskless_18_19$DGS1MO/365
riskless_18_19 <- riskless_18_19[-1,]

riskless_19_20 <- data.table(read.csv("Riskless_2019-20.csv", header=TRUE, na.strings = "."))
riskless_19_20$DATE <- as.Date(as.character(riskless_19_20$DATE))
riskless_19_20 <- riskless_19_20[as.vector(!is.na(riskless_19_20[,2]))]
riskless_19_20$DGS1MO <- riskless_19_20$DGS1MO/365
riskless_19_20 <- riskless_19_20[-1,]

will_18_19 <- data.table(read.csv("WILL5000PRFC(2018-2019).csv", header=TRUE, na.strings = "."))
will_18_19$DATE <- as.Date(as.character(will_18_19$DATE))
will_18_19 <- will_18_19[as.vector(!is.na(will_18_19[,2]))]
will_18_19[ , Returns := (WILL5000PRFC / shift(WILL5000PRFC)-1)*100]
will_18_19 <- will_18_19[-1,]

will_19_20 <- data.table(read.csv("WILL5000PRFC(2019-2020).csv", header=TRUE, na.strings = "."))
will_19_20$DATE <- as.Date(as.character(will_19_20$DATE))
will_19_20 <- will_19_20[as.vector(!is.na(will_19_20[,2]))]
will_19_20[ , Returns := (WILL5000PRFC / shift(WILL5000PRFC)-1)*100]
will_19_20 <- will_19_20[-1,]

ETF_18_19$symbol = as.factor(ETF_18_19$symbol)
ETF_19_20$symbol = as.factor(ETF_19_20$symbol)

ETF_18_19$symbol = as.factor(ETF_18_19$symbol)
ETF_19_20$symbol = as.factor(ETF_19_20$symbol)

# The order of calculations in the for loop
# This is how the graphs will be ordered in the future
sectors = c("IYZ", 
            "XLY", 
            "XLP", 
            "XLE", 
            "XLF", 
            "XLV", 
            "XLI",
            "XLK",
            "GDX",
            "VNQ",
            "XLU")

i=0
alphabeta_18_19 = data.frame() #column 1 alpha column 2 beta
for (sym in sectors) {
  i = i + 1
  stocki <- ETF_18_19[ETF_18_19$symbol == sym]
  stocki[ , Returns := (CPrc / shift(CPrc)-1)*100]
  stocki <- stocki[-1, ]
  
  excess_market = will_18_19$Returns - riskless_18_19$DGS1MO
  excess_stock = as.numeric(as.matrix(stocki[,7])) - riskless_18_19$DGS1MO
  model = lm(excess_stock ~ excess_market)
  alphabeta_18_19[i,1] <- summary(model)$coefficients[1,1]
  alphabeta_18_19[i,2] <- summary(model)$coefficients[2,1]
}

i=0
alphabeta_19_20 = data.frame()
#loop for 2019-2020
for (sym in sectors) {
  i = i + 1
  stocki <- ETF_19_20[ETF_19_20$symbol == sym]
  stocki[ , Returns := (CPrc / shift(CPrc)-1)*100]
  stocki <- stocki[-1, ]
  
  excess_market = will_19_20$Returns - riskless_19_20$DGS1MO
  excess_stock = as.numeric(as.matrix(stocki[,7])) - riskless_19_20$DGS1MO
  model = lm(excess_stock ~ excess_market)
  alphabeta_19_20[i,1] <- summary(model)$coefficients[1,1]
  alphabeta_19_20[i,2] <- summary(model)$coefficients[2,1]
}
rownames(alphabeta_18_19) = sectors
colnames(alphabeta_18_19) = c("alpha","beta")
rownames(alphabeta_19_20) = sectors
colnames(alphabeta_19_20) = c("alpha","beta")

percentchange = (alphabeta_19_20 / alphabeta_18_19 -1)*100

SectorNames = c("Communication Services",
                                       "Consumer Discretionary",
                                       "Consumer Staples",
                                       "Energy",
                                       "Financials",
                                       "Health Care",
                                       "Industrials",
                                       "Information Technology",
                                       "Materials",
                                       "Real Estate",
                                       "Utilities")
alphabeta_18_19$sector = SectorNames
alphabeta_19_20$sector = SectorNames
percentchange$sector = SectorNames

mean_variance_18_19 = data.frame()
mean_variance_18_19[1,1] = mean(alphabeta_18_19[,1])
mean_variance_18_19[2,1] = mean(alphabeta_18_19[,2])                                                    
mean_variance_18_19[1,2] = var(alphabeta_18_19[,1])
mean_variance_18_19[2,2] = var(alphabeta_18_19[,2]) 
rownames(mean_variance_18_19) = c("alpha","beta")
colnames(mean_variance_18_19) = c("mean","variance")

mean_variance_19_20 = data.frame()
mean_variance_19_20[1,1] = mean(alphabeta_19_20[,1])
mean_variance_19_20[2,1] = mean(alphabeta_19_20[,2])                                                    
mean_variance_19_20[1,2] = var(alphabeta_19_20[,1])
mean_variance_19_20[2,2] = var(alphabeta_19_20[,2]) 
rownames(mean_variance_19_20) = c("alpha","beta")
colnames(mean_variance_19_20) = c("mean","variance")

scale = (mean_variance_19_20/mean_variance_18_19 - 1) * 100

pdf("alpha20182019.pdf")
plot(c(1:11),alphabeta_18_19[,1],col = 'red', 
     ylim = c(min(alphabeta_18_19[,1],alphabeta_19_20[,1])
              ,max(alphabeta_18_19[,1],alphabeta_19_20[,1])),
     xlab = "sectors",
     ylab = "alpha",
     main = "alpha 2018 2019"
     )
text(c(1:11),y = alphabeta_18_19[,1]-0.02,
     labels = sectors, col = 'black')
dev.off()
pdf("alpha20192020.pdf")
plot(c(1:11),alphabeta_19_20[,1],col = 'red', 
     ylim = c(min(alphabeta_18_19[,1],alphabeta_19_20[,1])
              ,max(alphabeta_18_19[,1],alphabeta_19_20[,1])),
     xlab = "sectors",
     ylab = "alpha",
     main = "alpha 2019 2020")
text(c(1:11),y = alphabeta_19_20[,1]-0.02,
     labels = sectors, col = 'black')
dev.off()

pdf("beta20182019.pdf")
plot(c(1:11),alphabeta_18_19[,2],col = 'blue', 
     ylim = c(min(alphabeta_18_19[,2],alphabeta_19_20[,2])
              ,max(alphabeta_18_19[,2],alphabeta_19_20[,2])),
     xlab = "sectors",
     ylab = "beta",
     main = "beta 2018 2019")
text(c(1:11),y = alphabeta_18_19[,2]-0.04,
     labels = sectors, col = 'black')
dev.off()
pdf("beta20192020.pdf")
plot(c(1:11),alphabeta_19_20[,2],col = 'blue', 
     ylim = c(min(alphabeta_18_19[,2],alphabeta_19_20[,2])
              ,max(alphabeta_18_19[,2],alphabeta_19_20[,2])),
     xlab = "sectors",
     ylab = "beta",
     main = "beta 2019 2020")
text(c(1:11),y = alphabeta_19_20[,2]-0.04,
     labels = sectors, col = 'black')
dev.off()