library(plyr)
library(dplyr)
library(gridExtra)
library(maps)
library(ggplot2)

#Loading datasets
eurostatData <- read.csv("earn_ses14_21/earn_ses14_21_1_Data.csv", stringsAsFactors=FALSE)
estostatData <- read.csv("WS621sm2.csv", stringsAsFactors=FALSE, header=FALSE, sep=";")


#Working with eurostat dataset

# choosing subset of data not divided by sex
dataTotal <- subset(eurostatData, SEX == "Total")
# choosing subset of data excluding groups of countries
dataTotalByCountry <- subset(dataTotal, GEO!=grep("Euro+",GEO,value=TRUE))
# converting earnings column to numeric values
earnings <- as.numeric(sub(",", "",dataTotalByCountry$Value))
# earnings <- dataTotalByCountry$Value

# print(earnings)
#printing earnings in a bar chart
# countries <- (dataTotalByCountry$GEO)
# png(file = "barchart_months_earnings.png")
# barplot(height=earnings, width=35, xlab="Country", ylab="Mean earnings", main="Mean monthly earnings in Europe by country", names.arg=countries, col="blue", las=2, cex.names=.5)
# dev.off()

png(file = "hist_monthly_earnings.png")
hist(earnings, xlab="Mean earnings", ylab="Frequency", main="Mean monthly earnings in Europe by country", col="blue")
dev.off()

#Finding mean
mean.result <- mean(earnings)
# print(paste("Mean: ", mean.result))

#Finding standard deviation
sd.result <- sd(earnings)
# print(paste("Standard deviation: ", sd.result))

#Finding median
median.result <- median(earnings)
# print(paste("Median: ", median.result))

# finding data instance with maximum earnings
maxCountryEarnings <-subset(dataTotalByCountry, earnings == max(earnings))
# print(maxCountryEarnings)

# finding data instance with minimum earnings
minCountryEarnings <-subset(dataTotalByCountry, earnings == min(earnings))
# print(minCountryEarnings)

# table with statistics
stats <- data.frame(mean.result, sd.result, median.result, minCountryEarnings$Value, maxCountryEarnings$Value)
colnames(stats) <- c("Mean", "SD", "Median", "Min", "Max")




#  Working with Estonian dataset
aveDecile <- estostatData[[3]]
deciles <- estostatData[4:12]
# Comparing european mean earnings to estonian deciles. 
findDecile <- function(x) {
	i=1
	while(deciles[[i]] < x){
		i <- i+1
		if (i==10){
			break
		}
	}
	return (i)
}
decile = sapply(earnings, findDecile)
numCountriesEarningsHigherNinthDecile = length(decile[sapply(decile, function(x) x==10)])


# Refining country names to match names from maps dataset
region <- revalue(dataTotalByCountry$GEO, c("North Macedonia" = "Macedonia", "United Kingdom" = "UK", "Czechia" = "Czech Republic", "Germany (until 1990 former territory of the FRG)" = "Germany"))
# Converting temporarily for merging with NA values
decile <- as.character(decile)
# Creating dataframe for diplaying in maps
eurData <- data.frame(region, decile)
# print(eurData)



# Using tutorial for drawing a nice map from the following article: https://medium.com/@jules.beley/making-a-map-with-eu-data-on-r-erasmus-exchanges-by-country-3f5734dcd4ff
# Working with maps
world <- map_data("world")
# Meging map table and europe data
tojoin <- as.data.frame(matrix(
          nrow = length(table(world$region)),
          ncol = 2,
          as.factor(NA),
          dimnames = list(names(table(world$region)), colnames(eurData))
  ))
tojoin$region <- rownames(tojoin)

all <- full_join(eurData, tojoin)
all <- all[order(all$region), ]

# Removing duplicates
for (i in (1:251)) {
  if (all$region[i] == all$region[i + 1]) {
    all <- all[-c(i + 1), ]
  }
}


all$decile = as.numeric(all$decile)
mapbig <- inner_join(world, all, by = "region")

# Plotting map
worldmap <- ggplot() + theme(
  panel.background = element_rect(fill = "lightcyan1",
                                  color = NA),
  panel.grid = element_blank(),
  axis.text.x = element_blank(),
  axis.text.y = element_blank(),
  axis.ticks = element_blank(),
  axis.title.x = element_blank(),
  axis.title.y = element_blank())

# Limiting to europe by coordinate limits
europe <- worldmap + coord_fixed(xlim = c(-21, 42.5),
                                 ylim = c(36, 70.1),
                                 ratio = 1.5)

cnames <- aggregate(cbind(long, lat) ~ region, data=mapbig, 
                    FUN=function(x)mean(range(x)))
# Hiding non-european countries names
hideNonEuropeanNames <- function(x) {
	if (!(x %in% region)){
        x=""
    } else {
        x=x
    }
}
cnames$region=sapply(cnames$region, hideNonEuropeanNames)
   
# Drawing the map
europe2 <- europe+ 
            geom_polygon(data = mapbig, aes(fill = decile,
                                     x = long,
                                     y = lat,
                                     group = group),
                                 color = "grey70") +
            geom_text(data=cnames, aes(long, lat, label = region), size=2 ) +
            scale_fill_gradient(name = "Deciles", low = "#80aaff", high = "#003399", na.value = "grey50") +
 labs(title = "Mean monthly earnings of European countries in terms of Estonian deciles")  


ggsave("europeMap.png")
