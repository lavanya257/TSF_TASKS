#Author- Lavanya Jain

install.packages("ggplot2")
install.packages("tidyverse")
install.packages("plotly")
library(ggplot2)
library(plotly)
library(tidyverse)
ssdata <- read.csv("C:\\Users\\user\\Downloads\\SampleSuperstore.csv")

#1.Preparing data for analysis
str(ssdata)
summary(ssdata)
is.null(ssdata)
duplicated(ssdata)
ssdatanew <- ssdata %>% distinct()

#1.1.Sales has unusual maximum value, so replacing the value by average sales
maxSales <- max(ssdatanew$Sales)
ssdatanew$Sales <- replace(ssdatanew$Sales, ssdatanew$Sales==maxSales, mean(ssdatanew$Sales))
summary(ssdatanew$Sales)

#1.2.Removing postal code and country
ssdatanew <- ssdatanew %>% select(-c(Country, Postal.Code))


#2.GRAPHICAL ANALYSIS

#2.1.Plotting sales and quantity to know which shipment mode is most prominent
p <- ggplot(data = ssdatanew, aes(x = Quantity, y = Sales, fill = Ship.Mode) )+ geom_bar(stat = "identity") 
ggplotly(p)
#The graph shows that most of the sales have been triggered by the standard class of shipment mode.

#2.2.Plotting sales and profit to know which mode of shipment gives the most profit
p1 <- ggplot(data = ssdatanew, aes(x = Sales, y = Profit, color = Ship.Mode)) + geom_point()
ggplotly(p1)
#The graph shows that more profits have been availed from the standard shipment class, but the range of profits remain low.

#2.3.Plotting sales and discount
p2 <- ggplot() + geom_point(data = ssdatanew, aes(x = Discount, y = Sales, color = Ship.Mode)) 
ggplotly(p2)

#2.4.Plotting profit and discount
p3 <- ggplot() + geom_bar(data = ssdatanew, aes(x = Discount, y = Profit, fill = Ship.Mode), stat = "identity") 
ggplotly(p3)
#The more discounts have been offered and redeemed, the lesser profits the segments have achieved. 
#Products with no discounts show high range of profits but as the discount range increases, more loss and less profit can be seen.

#2.5.Plotting sub category and profit
p4 <-ggplot() + geom_bar(data = ssdatanew, aes(x = Sub.Category, y = Profit, fill = Region), stat = "identity") + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
ggplotly(p4)
#The graph shows that most losses have been incurred by the Binders industry mainly in the Central region.
#Machines and Tables industry also show losses.

#2.6.Plotting category and sales
p5 <- ggplot() + geom_bar(data = ssdatanew, aes(x = Category, y = Sales, fill = Region), stat = "identity") + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
ggplotly(p5)
#Most Sales have been made by the technology category, followed by Furniture and office supplies. 
#Mostly sales have been made from the West and East regions.

#2.7.Plotting category and profit
p6 <- ggplot() + geom_bar(data = ssdatanew, aes(x = Category, y = Profit, fill = Region), stat = "identity") + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
ggplotly(p6)
#The furniture category incurs most losses followed by Office Supplies and technology category.

#2.8.PLotting profits and sales category wise
p7 <- ggplot() + geom_point(data = ssdatanew, aes(x = Sales, y = Profit, color = Category)) 
ggplotly(p7)
#The graph shows that for all categories the points more or less cluster around the same area. So sales to profit ratio is almost same across categories except for few data points.

#3.REMARKS
#Same day shipment if receives more discounts can trigger sales/profits. 
#Discounts should be based on the Sales otherwise unnecessary discounts with low sales can witness huge losses.
#Binders, Machines and tables sub category/ industry area should be focused upon more to strengthen them.
#Office Supplies and the Furniture industries do not seem to boom in the Central Region (with quite large losses).


