# Setting my working directory to where my data files are located.
setwd("D:/DODATAHW/")
getwd()

#Download the Url to my Directory.
site <- "https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2FGDP.csv"
download.file(site,destfile="./getdata_data_GDP.csv")
site <- "https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2FEDSTATS_Country.csv" 
download.file(site,destfile="./getdata_data_Country.csv")

#Importing the Country data file.
Country=df<-read.csv("getdata_data_Country.csv", header = TRUE, sep=",",na.strings=c("", "NA"))
head(Country) #Checking the data.


#Set column names, for cleaner import
names<- c("CountryCode","Ranking", "Delete", "Economy", "GDP","Delete","Delete","Delete","Delete","Delete")
#Importing the GDP data file with the column names.
GDP<-read.csv("getdata_data_GDP.csv", header = FALSE, sep=",", skip=5, col.names = names,na.strings=c("", "NA"))
head(GDP)#Checking the data.

#Cleaning GDP data.
#Deleting columns with NAs or blank.
GDP$Delete<-NULL
GDP$Delete.1<-NULL
GDP$Delete.2<-NULL
GDP$Delete.3<-NULL
GDP$Delete.4<-NULL
GDP$Delete.5<-NULL
head(GDP)#Checking the data.

#Calculating the Number of missing value from deleting the above variables.
#Deleted 6 variables and each variable has 326 obs.
6*326 
# subset GDP to confirm GDP estimates, which are those with a ranking.
GDP<-GDP[1:190,]


#Question 1
#Merge the data based on the country shortcode.
Mergeddata<-merge(Country, GDP, by="CountryCode")
head(Mergeddata)

#Number of IDs that matched
dim(Mergeddata)


#Question 2
##Converting GDP values to numeric values.
Mergeddata$GDP<-as.character(Mergeddata$GDP)
Mergeddata$GDP<-as.numeric(gsub(",", "", Mergeddata$GDP))
##Sorting data by GDP.
Sorteddata<-Mergeddata[order(Mergeddata$GDP),]
#The 13th country on the list
Sorteddata[13,1:2]


#Question 3
#Subset the data where Income group equal High income: OECD and taking the mean.
mean(as.numeric(as.character(Mergeddata[Mergeddata$Income.Group =="High income: OECD",]$Ranking)))
#Subset the data where Income group equal High income: nonOECD and taking the mean.
mean(as.numeric(as.character(Mergeddata[Mergeddata$Income.Group =="High income: nonOECD",]$Ranking)))


#Question 4
#Show distribution of the GDP value for all of the countries. Use ggplot2 to create your plot by Income Group.
#Plotting GDP for all countries by Income.Group.
# install ggplot2
library(ggplot2)
ggplot(Mergeddata, aes(Income.Group, GDP),labels)+geom_point(aes(colour= factor(Mergeddata$Income.Group)))+
  ggtitle("GDP for each Country")+ theme(axis.title.x=element_blank(), axis.text.x=element_blank(),axis.ticks.x=element_blank())+ 
  theme(legend.position="bottom")

#Question 5
summary(Mergeddata$Income.Group)

#Question 6
#Cut the GDP ranking into 5 separate quantile groups. Make a table versus Income.Group.How many countries are Lower middle income but among the 38 nations with highest GDP?

#Making the 5 quartile groups.
breaks <- quantile(as.numeric(as.character(Mergeddata$Ranking)), probs = seq(0, 1, 0.2), na.rm = TRUE)
#Making the quartiles a column in the data.
Mergeddata$quantileGDP <- cut(as.numeric(as.character(Mergeddata$Ranking)), breaks = breaks)
##Selecting the data that has Income.Group equal to Lower middle income.
x<-Mergeddata[Mergeddata$Income.Group=="Lower middle income",]
#Getting the number of countries in the Lower middle income group that are in each quartile.
by(x, x$quantileGDP, nrow)
