#Advanced Data Reporting: Assignment #1 
#KatieSerrano, 2/6/19 

#Installing needed packages

install.packages("rio") 
install.packages("tidyverse")
install.packages("readxl")
install.packages("dplyr")
install.packages("janitor")

library (tidyverse)
library (readxl)
library (dplyr)
library(rio)
library(janitor)
library(here)

install.packages("formattable")
library(formattable)

#Load Data
download.file("arkansas_crime.xls")

arkansas_crime.xlx <- rio::import("arkansas_crime.xls")


#Rename

colnames(arkansas_crime.xlx)[1] <- "cities"
colnames(arkansas_crime.xlx)[2] <- "Population"
colnames(arkansas_crime.xlx)[3] <- "ViolentCrime"
colnames(arkansas_crime.xlx)[4] <- "Murder and nonnegligent manslaughter"
colnames(arkansas_crime.xlx)[5] <- "Rape1"
colnames(arkansas_crime.xlx)[6] <- "Robbery"
colnames(arkansas_crime.xlx)[7] <- "Aggravated Assault"
colnames(arkansas_crime.xlx)[8] <- "Property Crime"
colnames(arkansas_crime.xlx)[9] <- "Burglary"
colnames(arkansas_crime.xlx)[10] <- "Larceny-theft"
colnames(arkansas_crime.xlx)[11] <- "motor vehicle theft"
colnames(arkansas_crime.xlx)[12] <- "arson"

#Make "R friendly"

arkansas_crime.xlx <- janitor::clean_names(arkansas_crime.xlx)



#How many rows?
nrow(arkansas_crime.xlx)

#How many columns?
ncol(arkansas_crime.xlx)

summary(arkansas_crime.xlx)

#Convert numbers to "numeric" data


#Check you have the right names you want to convert
colnames(arkansas_crime.xlx[2:12])
arkansas_crime.xlx[2:12] <- lapply(arkansas_crime.xlx[2:12], as.numeric)

glimpse(arkansas_crime.xlx)  



#Create a new column with ViolentCrime per 10,000 residents

#ie. ArkCensus$Pct2017 <- ((ArkCensus$x2017-ArkCensus$x2016)/(ArkCensus$x2016))

data$newcolumn<-((arkansas_crime.xlx$violent_crime/population))

arkansas_crime.xlx$newcolumn<-NA

colnames(arkansas_crime.xlx)[13] <- "violent_crime_10k"
arkansas_crime.xlx$violent_crime_10k <- ((arkansas_crime.xlx$violent_crime/arkansas_crime.xlx$population))
arkansas_crime.xlx$violent_crime_10k <- ((arkansas_crime.xlx$violent_crime_10k*10000))

#Sort the table descending with the highest crime rate on top  
arkansas_crime.xlx <- arkansas_crime.xlx[order(-arkansas_crime.xlx$violent_crime_10k),]

#PART 2



#Create separate table with just the top five counties' crime rate

Top5 <- arkansas_crime.xlx%>%select(cities, violent_crime_10k)%>%filter(violent_crime_10k > 163.36032)



#Export 
write.csv(Top5,"Top5.csv")
write.csv(arkansas_crime.xlx)
arkansas_crime_final <- arkansas_crime.xlx
write.csv(arkansas_crime_final)
write.csv(arkansas_crime_final, "arkansas_crime_final.csv")



#Create a chart

#load software - Select NO when asked to restart
install.packages("ggplot2")
install.packages("dplyr")
install.packages("usethis")
install.packages("forcats")
install.packages("gplot")

library(ggplot2)
library(dplyr)
library(usethis)
library(forcats)

#Create Chart
ggplot(data = Top5, aes(x = cities, y = violent_crime_10k, group = cities, color = cities, fill=cities)) +
  geom_col(position="dodge") 


ggplot(Top5, aes(x=cities, y=violent_crime_10k, fill=cities)) + 
  geom_col(position="dodge") +
  theme(axis.text.x = element_text(angle = 45, vjust = 1.2, hjust = 1.1))

Top5Chart <- ggplot(data = Top5, aes(x = reorder(cities, -violent_crime_10k), y = violent_crime_10k, color = cities, fill=cities))  +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(title = "Arkansas Cities with Highest Violent Crime Rate per 10k Residents", 
       subtitle = "Source: FBI Uniform Crime Data, 2017",
       caption = "Graphic by Katie Serrano",
       x="Cities",
       y="Violent Crime Rate per 10k Residents")
plot(Top5Chart)

#Done!!!


