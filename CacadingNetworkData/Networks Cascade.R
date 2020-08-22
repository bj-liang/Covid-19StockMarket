closeAllConnections()
rm(list=ls())

setwd("C:\\Users\\benny\\OneDrive\\Desktop\\github\\CacadingNetworkData")

library(data.table)
lookup = fread("NewSectors.csv", 
                       header = TRUE, colClasses = c(SYM = "character", sector = "character"))

# couldn't figure out how to load this in properly
# so I just manually did it
stock_ordering = c()
for (i in seq(nrow(lookup))) {
  stock_ordering[i] = as.character(lookup[i,1])
}

# stock_ordering = factor(stock_ordering)
# levelsexcel = levels(TAQ$SYM_ROOT)
# levelsstockordering = levels(stock_ordering)

# # Make sure all the data is accounted for
# setequal(levelsexcel, levelsstockordering)


TAQ <- fread("01Dec201904May2020(TAQ).csv.gz", header=TRUE,
           colClasses=c(SYM_ROOT='character'
                        ,CPrc='numeric'
                        ,DATE='character'))
TAQ$DATE <- as.Date(as.character(TAQ$DATE),format='%Y%m%d')
TAQ$SYM_ROOT = factor(TAQ$SYM_ROOT)

correlationMatrix = data.frame()
m = 0
for (symrow in stock_ordering) {
  print(paste("row: ", symrow))
  m = m + 1
  n = 0 
  tmp_c = TAQ[TAQ$SYM_ROOT == symrow]
  for (symcolumn in stock_ordering) {
    print(symcolumn)
    n = n + 1
    tmp_r = TAQ[TAQ$SYM_ROOT == symcolumn]
    correlationMatrix[m,n] = cor(tmp_c$CPrc,tmp_r$CPrc)
  }
}
theta = 0.90
adjacency_matrix = correlationMatrix
adjacency_matrix[adjacency_matrix > theta] <- 1
adjacency_matrix[adjacency_matrix <= theta] <- 0

get_returns <-function(enddate
                       , sym
                       , startdate = as.Date("2019-12-02"))
{
  tmp = TAQ[TAQ$SYM_ROOT == sym]
  startprice = tmp[tmp$DATE == startdate]$CPrc 
  endprice = tmp[tmp$DATE == enddate]$CPrc 
  (endprice/startprice - 1)*100
}


returndates = c("2020-01-02", 
                "2020-02-03", 
                "2020-02-24",
                "2020-03-02",
                "2020-03-23",
                "2020-04-13",
                "2020-05-01")

nodeColors = data.frame()
for (day in returndates) {
  s = 0
  node = data.frame()
  print(day)
  for (sym in stock_ordering) {
    s = s + 1
    node[s,1] = sym
    node[s,2] = lookup[lookup$SYM == sym]$Sector
    node[s,3] = get_returns(as.Date(day), sym)
    node[s,4] = as.character(day, format = '%Y.%m.%d')
  }
  nodeColors = rbind(nodeColors, node)
}
# copy = nodeColors
# nodeColors = copy

bottom = -20 # you can change this up if you want to tinker with graphs
top = 20

red = nodeColors$V3 < bottom
green = nodeColors$V3 >= bottom & nodeColors$V3 <= top
blue = nodeColors$V3 > top

#this registers a new color column
nodeColors$color[red] = "red"
nodeColors$color[green] = "green"
nodeColors$color[blue] = "blue"


# testing don't delete
# x = c()
# y = c()
# i = 0
# for (symrow in stock_ordering) {
#   i = i+1
#   tmp_c = TAQ[TAQ$SYM_ROOT == symrow]
#   x[i] = length(TAQ[TAQ$SYM_ROOT == symrow]$CPrc)
#   y[i] = any(is.na(TAQ[TAQ$SYM_ROOT == symrow]$CPrc))
#   if (x[i] != 107) {print(symrow)}
#   }
# x
# y

