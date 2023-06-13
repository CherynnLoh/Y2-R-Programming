#Group 30
#Loh Wan Ning	TP065926
#Gui Wei Quan	TP064301
#Tey Xin Ying	TP066247
#Toong Xin Yi	TP064251
#Wong Kah Ying	TP064640


#import House_Rent_Dataset into R
getwd()
houseRental= read.csv("C:\\APU\\Year 2 Sem 1\\Programming for Data Analytics\\Assignment\\House_Rent_Dataset.csv",header = TRUE)
houseRental

#install packages
install.packages("ggplot2")
install.packages("plotrix")
install.packages("plyr")
install.packages("dplyr")
install.packages("hrbrthemes")
install.packages("tidyverse")
install.packages("lessR")

library(ggplot2)
library(plotrix)
library(plyr)
library(dplyr)
library(hrbrthemes)
library(tidyverse)


#Data Cleaning
#Set Header Names
names(houseRental)= c("Date_Posted", "BHK", "Rent_Price", "Size", "Floor", "Area_Type", 
                      "Area_Locality", "City", "Furnishing_Status", "Tenant_Preferred", 
                      "Bathroom", "Contact_Point")

#Clean Date & Convert to Date Format
houseRental$Date_Posted <- as.Date(houseRental$Date_Posted, "%m/%d/%Y")

#Clean Rent % Convert to Numeric 
houseRental$Rent_Price <- as.numeric(houseRental$Rent_Price)

#Data Pre-Processing
#Display all data in console
options(max.print = 50000)

#Check if there is any missing values in the dataset
anyNA(houseRental)

#Display the structure of the dataset
str(houseRental)

#Display the dimension of the dataset
dim(houseRental)

#Read first 6 lines
head(houseRental)

#Read last 6 lines
tail(houseRental)

#View all the data in the dataset in console
View(houseRental)

#View only the headers in the dataset
names(houseRental)

#Display how the data is being stored
class(houseRental)

#Display how many columns in the dataset
ncol(houseRental)

#Display how many rows in the dataset
nrow(houseRental)

#Summary of all elements
summary(houseRental)


#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#Question 1: The relationship between the rental price and furnishing status.
#NAME Tey Xin Ying
#TP066247
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

#maximum of rental price
max(houseRental$Rent)

#minimum of rental price
min(houseRental$Rent)

#Range of rental amount
table(houseRental$Rent)

#Summarize rental amount attribute
summary(houseRental$Rent)

#Analysis 1-1: The percentage and numbers of furnishing status in different cities
Q1i=houseRental%>%
  count(Furnishing_Status, name="num",sort = TRUE)
Q1i

piepercent <- paste(Q1i$Furnishing_Status, " ", round(100*(Q1i$num/sum(Q1i$num)),2), "%", "(", Q1i$num, ")")
pie3D(Q1i$num, labels = piepercent,explode=0.1, main = "Furnishing Status in Diffrent Cities", 
      col = rainbow(length(Q1i$num)))
legend(("topright"), c("Bangalore, Chennai, Hyderabad, Mumbai, Delhi, Kolkata",
                       "Bangalore, Chennai, Hyderabad, Mumbai, Delhi, Kolkata",
                       "Bangalore, Chennai, Hyderabad, Mumbai, Delhi, Kolkata"),
       cex = 0.7, ncol = 1, fill = rainbow(length(Q1i$num)))



#Analysis 1-2: The average of furnishing status and rent price
Q1ii <- houseRental %>%
  group_by(Furnishing_Status) %>%
  summarise(avg = mean(Rent_Price))
Q1ii

piepercent <-paste(Q1ii$Furnishing_Status, " ",round(Q1ii$avg,2))
pie(Q1ii$avg,labels=piepercent,main="Average of furnishing status and rent price",radius=0.8,clockwise=TRUE,
    col=rainbow(length(Q1ii$avg)))
legend("topright",c("Furnished","Semi-Furnished","Unfurnished"),cex=0.8,fill = rainbow(length(Q1ii$avg)))


#Analysis 1-3: The percentage and numbers of furnished units in different cities
Q1iii <- houseRental %>%
  filter(Furnishing_Status == "Furnished") %>%
  group_by(Furnishing_Status) %>%
  count(City, name = "Units", sort = TRUE) %>%
  mutate(TotalUnitsPercentage = paste(round((Units / sum(Units)) * 100, 2), "%"))
Q1iii

piepercent <-paste(" ", Q1iii$City, Q1iii$TotalUnitsPercentage)
pie(Q1iii$Units,labels=piepercent,main="Percentage of furnished unit in different cities",
    radius=0.8,clockwise=TRUE,col=rainbow(length(Q1iii$TotalUnitsPercentage)))
legend("topleft",c("Mumbai","Chennai","Bangalore","Delhi","Hyderabad"),cex=0.8,
       fill = rainbow(length(Q1iii$TotalUnitsPercentage)))


#Analysis 1-4: The percentage and numbers of semi-furnished units in different cities
Q1iv <- houseRental %>%
  filter(Furnishing_Status == "Semi-Furnished") %>%
  group_by(Furnishing_Status) %>%
  count(City, name = "Units", sort = TRUE) %>%
  mutate(TotalUnitsPercentage = paste(round((Units / sum(Units)) * 100, 2), "%"))
Q1iv

piepercent <-paste(" ", Q1iv$City, Q1iv$TotalUnitsPercentage)
pie(Q1iv$Units,labels=piepercent,main="Percentage of semi-furnished unit in different cities",
    radius=0.8,clockwise=TRUE,col=rainbow(length(Q1iv$TotalUnitsPercentage)))
legend("topleft",c("Mumbai","Chennai","Bangalore","Delhi","Hyderabad"),cex=0.8,
       fill = rainbow(length(Q1iv$TotalUnitsPercentage)))


#Analysis 1-5: The percentage and numbers of unfurnished units in different cities
Q1v <- houseRental %>%
  filter(Furnishing_Status == "Unfurnished") %>%
  group_by(Furnishing_Status) %>%
  count(City, name = "Units", sort = TRUE) %>%
  mutate(TotalUnitsPercentage = paste(round((Units / sum(Units)) * 100, 2), "%"))
Q1v

piepercent <-paste(" ", Q1v$City, Q1v$TotalUnitsPercentage)
pie(Q1v$Units,labels=piepercent,main="Percentage of unfurnished unit in different cities",
    radius=0.8,clockwise=TRUE,col=rainbow(length(Q1v$TotalUnitsPercentage)))
legend("topright",c("Mumbai","Chennai","Bangalore","Delhi","Hyderabad"),cex=0.8,
       fill = rainbow(length(Q1v$TotalUnitsPercentage)))


#Analysis 1-6: The top 5 highest average rental price of furnished furnishing unit 
Q1vi <- houseRental %>%
  filter(Furnishing_Status=="Furnished") %>%
  group_by(City) %>%
  summarise(rental_price = sum(Rent_Price)/n()) %>%
  arrange(desc(rental_price)) %>%
  head(n=5)
Q1vi

Q1vi.data = data.frame(
  City = c(Q1vi$City),
  rental_price = round(Q1vi$rental_price,2)
)
Q1vi.data

ggplot(Q1vi.data,aes(x=City,y=rental_price)) + 
  geom_bar(stat = "identity",width=0.5,fill="#DAC7F9")+
  geom_text(aes(label=rental_price))+
  labs(title="The top 5 highest average rental price of furnished furnishing unit", 
       x="City", y="Rental Price")+
  theme(plot.background = element_rect(fill="#E7F8FC"))

#Analysis 1-7: The top 5 highest average rental price of semi-furnished furnishing unit 
Q1vii <- houseRental %>%
  filter(Furnishing_Status=="Semi-Furnished") %>%
  group_by(City) %>%
  summarise(rent_price = sum(Rent_Price)/n()) %>%
  arrange(desc(rent_price)) %>%
  head(n=5)
Q1vii

Q1vii.data = data.frame(
  City = c(Q1vii$City),
  rent_price = round(Q1vii$rent_price,2)
)
Q1vii.data

ggplot(Q1vii.data,aes(x=City,y=rent_price)) + 
  geom_bar(stat = "identity",width=0.5,fill="#97BFFC")+
  geom_text(aes(label=rent_price))+
  labs(title="The top 5 highest average rental price of semi-furnished furnishing unit", 
       x="City", y="Rental Price")+
  theme(plot.background = element_rect(fill="#E7F8FC"))

#Analysis 1-8: The top 5 highest average rental price of unfurnished furnishing unit 
Q1viii <- houseRental %>%
  filter(Furnishing_Status=="Unfurnished") %>%
  group_by(City) %>%
  summarise(rent_price = sum(Rent_Price)/n()) %>%
  arrange(desc(rent_price)) %>%
  head(n=5)
Q1viii

ggplot(Q1viii.data,aes(x=City,y=rent_price)) + 
  geom_bar(stat = "identity",width=0.5,fill="#ADFC97")+
  geom_text(aes(label=rent_price))+
  labs(title="The top 5 highest average rental price of unfurnished furnishing unit", 
       x="City", y="Rental Price")+
  theme(plot.background = element_rect(fill="#E7F8FC"))

#Conclusion
Q1viii.data = data.frame(
  City = c(Q1viii$City),
  rent_price = round(Q1viii$rent_price,2)
)
Q1viii.data
#===========================================================================


#=============================================================================
##QUESTION 2: How does the size of house affect the rental price?
#Toong Xin Yi
#TP064251
#=============================================================================
##Analysis 1: Distribution of Number of rentals per House Size
Q2A1 <- houseRental %>%
  group_by(Size) %>%
  summarise(count=n())
Q2A1

#freqpoly
ggplot(Q2A1,aes(Size))+
  geom_freqpoly()+
  labs(title= "Distribution of Rentals Available for each House Size",x= "Size(sqft)")+
  theme_ipsum()+
  theme(plot.title = element_text(hjust =0.5,size = 15, face = "bold"))


#=============================================================================
##Analysis 2: Pie Chart of House Size Categorization
p1 <- nrow(houseRental[houseRental$Size <= 500,])
p1
p2 <- nrow(houseRental[houseRental$Size <= 1000 ,]) -p1
p2
p3 <- nrow(houseRental[houseRental$Size <= 2000,])-p1-p2
p3
p4 <- nrow(houseRental[houseRental$Size > 2000,]) 
p4

count <- c(p1,p2,p3,p4)
labels <- c("0-500 sqft", "501-1000 sqft","1001-2000 sqft","2001-8000 sqft")
pie_labels <- paste0(round(100 * count/sum(count), 2), "%","(",count,")")

#pie
pie(count,pie_labels,radius=1,main="House Size Categorization",
    col = rainbow(4),clockwise = TRUE)
legend("bottomleft", labels, cex = 0.8, fill = rainbow(4))


#=============================================================================
##Analysis 3: Median of house size 
Q2A3 <- houseRental %>%
  group_by(Size)
Q2A3


#boxplot
ggplot(Q2A3,aes(y=Size,x=1))+
  geom_boxplot()+
  stat_summary(
    aes(label=sprintf("%i", ..y..)),
    geom="text", 
    fun.y = function(y) boxplot.stats(y)$stats) +
  labs(title="House Size",y= "Size(sqft)")+
  theme_ipsum()+
  theme(plot.title = element_text(hjust =0.5,size = 15, face = "bold")) +
  stat_summary(fun=mean, geom='point', shape=20)


#=============================================================================
##Analysis 4: Find the house allocated between 0 and 1000 sqft
Q2A4 <- houseRental %>%
  group_by (Size) %>%
  filter (Size<=1000)%>%
  count()
Q2A4

#histogram
ggplot(Q2A4,aes(x= Size))+
  geom_histogram(fill="gray", color="pink")+
  labs(title="Number of Houses Between 0 and 1000 sqft",x= "Size(sqft)")+
  theme_ipsum()+
  theme(plot.title = element_text(hjust =0.5,size = 15, face = "bold"))

#extra feature: density plot
ggplot(Q2A4,aes(x=Size))+
  geom_density(aes(y=..count..),colour="black",
               fill="yellow", alpha = 0.6)+
  labs(title="Number of Houses Between 0 and 1000 sqft",x= "Size(sqft)", y = "Count")+
  theme(plot.title = element_text(hjust =0.5,size = 15, face = "bold"))

#=============================================================================
##Analysis 5: Find the average rent price of each house size
Q2A5 <- houseRental %>%
  group_by (Size) %>%
  summarise(AvgPrice = mean(Rent_Price))
Q2A5

#scatterplot
ggplot(Q2A5, aes(x = Size, y = AvgPrice)) + 
  geom_point() +
  geom_smooth() +
  labs(title="Average Rental Price for each House Size",x= "Size (sqft)", y="Average Rental Price(RM)")+
  theme_ipsum()+
  theme(plot.title = element_text(hjust =0.5,size = 15, face = "bold"))


#=============================================================================
##Analysis 6: Average Price Rate per SQFT for each Size
Q2A6 <- houseRental %>%
  group_by (Size) %>%
  summarise(AvgPriceRate = mean(Rent_Price)/Size)
Q2A6

#scatterplot
ggplot(Q2A6, aes(x = Size, y = AvgPriceRate)) + 
  geom_point() +
  geom_smooth() +
  labs(title="Average Price Rate for each Size",x= "Size (sqft)", y="Price Rate (RM/sqft)")+
  theme_ipsum()+
  theme(plot.title = element_text(hjust =0.5,size = 15, face = "bold"))


#=============================================================================
##Analysis 7: Price rate per SQFT of each month 
Q2A7 <- houseRental %>%
  group_by(Month = lubridate::floor_date(Date_Posted, 'month'))%>%
  summarise(PriceRate = mean (Rent_Price / Size))
Q2A7

#Line graph
ggplot(Q2A7, aes(x=Month,y=PriceRate))+
  geom_line(color="gray")+
  geom_point(size=4, color="pink")+
  geom_text(aes(label=format(round (PriceRate,2)),vjust=-0.75))+
  labs(title="Average Rental Price Rate per Month",y="Price Rate (RM/sqft)")+
  theme_ipsum()+
  theme(plot.title = element_text(hjust =0.5,size = 15, face = "bold"))


#=============================================================================
##Analysis 8: Average house size of each city
Q2A8 <- houseRental %>%
  group_by(City) %>%
  summarise(AvgSize = mean(Size))
Q2A8

#Bar Chart
ggplot(Q2A8,aes(x=City,y=AvgSize,fill=City))+
  geom_bar(stat ="identity")+
  labs(title="Average House Size in Cities",y="Size (sqft)")+
  theme_ipsum()+
  theme(plot.title = element_text(hjust =0.5,size = 15, face = "bold"))


#=============================================================================
##Analysis 9: Price rate per SQFT of each city
Q2A9 <- houseRental %>%
  group_by(City)%>%
  summarise(PriceRate = mean(Rent_Price / Size))
Q2A9

#extra:lollipop
ggplot(Q2A9,aes(x=City,y=PriceRate,fill =City,color=City))+
  geom_point()+
  geom_segment(aes(x=City,xend=City,y=0,yend=PriceRate))+
  geom_text(aes(label=format(round (PriceRate,2)),vjust=-0.75))+
  labs(title="Average Rental Price Rate in Cities",y="Price rate (RM/sqft)")+
  theme_ipsum()+
  theme(plot.title = element_text(hjust =0.5,size = 15, face = "bold"))


#=============================================================================
##Analysis 10: Relationship between Size and Price in each City
Q2A10 <- houseRental %>%
  group_by(City)%>%
  summarise(Size,Rent_Price)
Q2A10

#scatterplot
ggplot(Q2A10, aes(x=Size,y=Rent_Price,color=City))+
  geom_point()+
  facet_wrap(~City)+
  labs(title="Relationship between House Size and Price in Cities",x = "Size (sqft)",y="Price (RM)")+
  theme_ipsum()+
  theme(plot.title = element_text(hjust =0.5,size = 15, face = "bold"))


#=============================================================================
##Analysis 11: Does BHK affect the house size? 
Q2A11 <- houseRental %>%
  group_by(BHK) %>%
  summarise(AvgSize= mean(Size))
Q2A11

#Bar Chart
ggplot(Q2A11,aes(x=BHK,y=AvgSize,fill=BHK))+
  geom_bar(stat = "identity")+
  labs(title="Relationship between BHK and House Size",y="Average Size (sqft)")+
  theme_ipsum()+
  theme(plot.title = element_text(hjust =0.5,size = 15, face = "bold"))


#=============================================================================
##Analysis 12: Does BHK affect the Price Rate? 
Q2A12 <- houseRental %>%
  group_by(BHK) %>%
  summarise(AvgPriceRate= mean(Rent_Price)/mean(Size))
Q2A12

#Bar Chart
ggplot(Q2A12,aes(x=BHK,y=AvgPriceRate,fill=BHK))+
  geom_bar(stat = "identity")+
  labs(title="Relationship between BHK and Rental Price Rate",y="Average Price Rate (RM/sqft)")+
  theme_ipsum()+
  theme(plot.title = element_text(hjust =0.5,size = 15, face = "bold"))


#=============================================================================
##Analysis 13: Does number of Bathroom affect the house size? 
Q2A13 <- houseRental %>%
  group_by(Bathroom) %>%
  summarise(AvgSize= mean(Size))
Q2A13

#lollipop
ggplot(Q2A13,aes(x=Bathroom,y=AvgSize,color=Bathroom))+
  geom_point()+
  geom_segment(aes(x=Bathroom,xend=Bathroom,y=0,yend=AvgSize))+
  labs(title="Relationship between Number of Bathroom and House Size",x="Number of Bathroom",y="Average Size (sqft)")+
  theme_ipsum()+
  theme(plot.title = element_text(hjust =0.5,size = 15, face = "bold"))


#=============================================================================
##Analysis 14: Does number of Bathroom affect the Price rate? 
Q2A14 <- houseRental %>%
  group_by(Bathroom) %>%
  summarise(AvgPriceRate= mean(Rent_Price)/mean(Size))
Q2A14

#lollipop
ggplot(Q2A14,aes(x=Bathroom,y=AvgPriceRate,color=Bathroom))+
  geom_point()+
  geom_segment(aes(x=Bathroom,xend=Bathroom,y=0,yend=AvgPriceRate))+
  labs(title="Relationship between Number of Bathroom and Rental Price Rate",x="Number of Bathroom",y="Average Price Rate (RM/sqft)")+
  theme_ipsum()+
  theme(plot.title = element_text(hjust =0.5,size = 13, face = "bold"))


#=============================================================================
##Analysis 15: Average Price Rate for the preferred tenant
Q2A15<- houseRental %>%
  group_by(Tenant_Preferred) %>%
  summarise(AvgPriceRate = mean(Rent_Price)/ mean(Size))
Q2A15

#Bar Chart
ggplot(Q2A15,aes(x=AvgPriceRate,y=Tenant_Preferred,fill=Tenant_Preferred))+
  geom_bar(stat = "identity")+
  geom_text(aes(label=format(round (AvgPriceRate,2)), face = "bold", hjust = 1.5)) +
  labs(title="Relationship between Prefered Tenant and Rental Price Rate",
       x="Average Price Rate (RM/sqft)",y="Preferred Tenants")+
  theme_ipsum()+
  theme(plot.title = element_text(hjust =0.5,size = 15, face = "bold"))


#=============================================================================
##Analysis 16: Average Price Rate for the Area Type
Q2A16<- houseRental %>%
  group_by(Area_Type) %>%
  summarise(AvgPriceRate = mean(Rent_Price)/ mean(Size))
Q2A16

#Bar Chart
ggplot(Q2A16,aes(x=AvgPriceRate,y=Area_Type, fill=Area_Type))+
  geom_bar(stat = "identity")+
  geom_text(aes(label=format(round (AvgPriceRate,2)), face = "bold", hjust =1.25)) +
  labs(title="Relationship between Area Type and Rental Price Rate",
       x="Average Price Rate (RM/sqft)",y="Area Type")+
  theme_ipsum()+
  theme(plot.title = element_text(hjust =0.5,size = 15, face = "bold"))
#=============================================================================


#==================================================================================#
#Question 3:The relationship between the rental price and area type
#Wong Kah Ying
#TP064640
#====================================================================================
#Select rental price and area type 
Q3.1<-
  houseRental%>%
  group_by(Area_Type)%>%
  summarise(Average_Rent_Price = round(mean(Rent_Price)))
Q3.1

#Code of Lolipop Chart with data label and Title
ggplot(Q3.1,aes(x= Area_Type,y = Average_Rent_Price,color=Area_Type)) +
  geom_point(color="Purple")+
  geom_segment(aes(x= Area_Type, xend= Area_Type,y=0, yend = Average_Rent_Price)) +
  geom_text(aes(label = Average_Rent_Price)) +
  ggtitle("Average Rent Price of each Area Chart")

#============================================================
#Q3.2 Analyse monthly average rental price in different area
#============================================================
Q3.2<-
  houseRental %>%
  group_by(Area_Type,months(Date_Posted))%>%
  summarize(Average_Rent_Price=round(mean(Rent_Price)))
names(Q3.2)<-c("Area","Month","Average_Rent_Price")
Q3.2

##Code of Bar Chart with data label and Title
ggplot(Q3.2,aes(fill = Month,y = Average_Rent_Price, x = Area)) +
  geom_bar(position="dodge",stat ="identity") +
  geom_text(aes(label = Average_Rent_Price))+
  facet_wrap(~Month)+
  ggtitle("Average Rent Price of Each Area Changes in Different Months Bar Chart")

ggplot(Q3.2,aes(fill = Month,y = Average_Rent_Price, x = Area)) +
  geom_bar(position="dodge",stat ="identity") +
  ggtitle("Average Rent Price of Each Area Changes in Different Months Grouped Bar Chart")

#============================================================
#Q3.3 Analyze the maximum price in different Area type
#============================================================
Q3.3<-
  houseRental%>%
  group_by(Area_Type)%>%
  summarize(HighestPrice = max(Rent_Price))
Q3.3
#Code of Bar Chart with data label and Title
ggplot(Q3.3,aes(x=Area_Type,y=HighestPrice,fill=Area_Type))+
  geom_bar(stat="identity")+
  geom_text(aes(label = HighestPrice))+
  ggtitle("Maximum Rental Price of Area ")
#============================================================
#Q3.4 Analyze the minimum price in different Area
#============================================================
Q3.4<-
  houseRental%>%
  group_by(Area_Type)%>%
  summarize(LowestPrice = min(Rent_Price))
Q3.4

ggplot(Q3.4,aes(x=Area_Type,y=LowestPrice,fill=Area_Type))+
  geom_bar(stat="identity")+
  geom_text(aes(label = LowestPrice))+
  labs(title="Minimum Rental Price of Area Type")

Q3.4 %>%
  mutate(name = fct_reorder(Area_Type,LowestPrice)) %>%
  ggplot( aes(x=Area_Type, y=LowestPrice, fill=Area_Type)) +
  geom_bar(stat="identity", alpha=.6, width=.4) +
  geom_text(aes(label = LowestPrice))+
  coord_flip() +
  xlab("") +
  theme_bw()+labs(title="Minimum Rental Price of Area Type")


#============================================================
#Q3.5 Analyze Median rental price in all area-boxplot
#============================================================
Q3.5<-
  houseRental%>%
  group_by(Area_Type)%>%
  summarize(Rent_Price)

Q3.5

Q3.5.1<-houseRental%>%
  group_by(Area_Type)%>%
  summarize(Median_Price=median(Rent_Price))
Q3.5.1

ggplot(Q3.5,aes(x=Rent_Price,y=Area_Type,fill=Area_Type))+
  coord_cartesian(xlim = c(0, 60000))+
  geom_boxplot(alpha=1.0)+
  scale_fill_brewer(palette="BuPu")+
  labs(x="MedianPrice",title="Median Rental Price of Area Type")

ggplot(Q3.5.1,aes(x=Area_Type,y=Median_Price,fill=Area_Type))+
  geom_bar(stat="identity")+
  geom_text(aes(label = Median_Price))+
  labs(title="Median Rental Price of Area Type")
#============================================================
#Q3.6 what is the number of each area more than rental price 250000- Pie
#============================================================
Q3.6<-
  houseRental%>%
  filter(Rent_Price>25000)%>%
  group_by(Area_Type)%>%
  summarize(count_area=n())
Q3.6
v1<-c(Q3.6$Area_Type)
v2<-c(Q3.6$count_area)
piepercent<-round(100*v2/sum(v2),1)

pie(v2,label = piepercent,main="The Percentage of Area Type more than 25000 rental price",col = rainbow(length(x)))
legend("topright", v1, cex = 0.8,fill = rainbow(length(x)))

#============================================================
#Q3.7 The percentage less than rental price 25000 out of all area
#============================================================
Q3.7<-
  houseRental%>%
  filter(Rent_Price<25000)%>%
  group_by(Area_Type)%>%
  summarize(count_area=n())
Q3.7
v3<-c(Q3.7$Area_Type)
v4<-c(Q3.7$count_area)
piepercent<-round(100*v4/sum(v4),1)

pie(v4,label = piepercent,main="The percentage sum of different area less than rental price 250000",
    col = rainbow(length(v3)))
legend("bottomright", v3, cex = 1.0,fill = rainbow(length(v3)))

#============================================================
#Q3.8 The total rental price of each area-bar
#============================================================
Q3.8<-
  houseRental%>%
  group_by(Area_Type)%>%
  summarize(Total_Rental=sum(Rent_Price))
Q3.8

ggplot(Q3.8, aes(x=Area_Type,y=Total_Rental,fill=Area_Type)) + 
  geom_bar(stat="identity",alpha=0.6) +
  geom_text(aes(label=Total_Rental))+
  labs(title="Relationship between Area Type and Total Price", x=" Area", y="Total")


#Q3.9 The Total Number of each area - Doughnut Chart
Q3.9<-
  houseRental%>%
  group_by(Area_Type)%>%
  summarize(Total_Number =n())
Q3.9
#Find Percentage
Q3.9$fraction<-Q3.9$Total_Number/sum(Q3.9$Total_Number)
#Find the cumulative percentage (Top of the rectangle)
Q3.9$ymax<-cumsum(Q3.9$fraction)
#Find the bottom of each rectangle
Q3.9$ymin<-c(0,head(Q3.9$ymax,n=-1))
#Find label position
Q3.9$labelPosition <- (Q3.9$ymax + Q3.9$ymin) / 2
#Compute good label
Q3.9$label <- paste0(Q3.9$Area_Type, "\n value: ", Q3.9$Total_Number)
ggplot(Q3.9, aes(ymax=ymax, ymin=ymin, xmax=4, xmin=3, fill=Area_Type)) +
  geom_rect() +
  geom_label( x=3.5, aes(y=labelPosition, label=label), size=6) +
  scale_fill_brewer(palette=4) +
  coord_polar(theta="y") +
  xlim(c(2, 4)) +
  theme_void() +
  theme(legend.position = "none")+
  ggtitle("The Total Number of Each Area")
#===================================================================================


#==================================================================================#
#Question 4: The relationship between the Rent_Price,City and Area_Locality
#Loh Wan Ning
#TP065926
#====================================================================================
#Analysis 1: The highest rental price in Each City
Q4A1 = houseRental%>%
  filter(City == "Kolkata"|City == "Mumbai"|City == "Bangalore"|City == "Delhi"|City == "Chennai"|City == "Hyderabad") %>%
  group_by(City)%>%
  summarise(maxPrice = max(Rent_Price))
Q4A1

#Analysis 1- Bar Chart
ggplot(Q4A1, aes(x=City, y=maxPrice,fill = City))+ 
  geom_bar(stat = "identity",width = 0.5)+
  scale_fill_brewer(palette="Set1")+
  geom_text(aes(label=maxPrice))+
  labs(title="Highest Rental Price in Cities",x="City",y="Rental Price",fill ="City")

#==============================================================================
#Analysis 2: The lowest rental price in Each City
Q4A2 = houseRental%>%
  filter(City == "Kolkata"|City == "Mumbai"|City == "Bangalore"|
           City == "Delhi"|City == "Chennai"|City == "Hyderabad") %>%
  group_by(City)%>%
  summarise(minRent = min(Rent_Price))
Q4A2

#Analysis 2- Bar Chart
ggplot(Q4A2, aes(x=City, y=minRent,fill = City)) + 
  geom_bar(stat = "identity",width = 0.5)+
  scale_fill_hue(c = 70) +
  theme(legend.position="none")+
  geom_text(aes(label=minRent))+
  labs(title="Lowest Rental Price in Cities",x="City",y="Rental Price",fill ="City")+
  coord_flip()

#=============================================================================
#Analysis 3: The top 3 lowest average rental price in Delhi’s locality
Q4A3 = houseRental%>%
  filter(City == "Delhi")%>%
  group_by(Area_Locality)%>%
  summarise(avgprice = sum(Rent_Price) / n()) %>%
  arrange(avgprice) %>%
  head(n = 3)
Q4A3

#=============================================================================
#Analysis 4: The top 3 highest average rental price in Delhi’s locality
Q4A4 = houseRental%>%
  filter(City == "Delhi")%>%
  group_by(Area_Locality)%>%
  summarise(avgprice = sum(Rent_Price) / n()) %>%
  arrange(desc(avgprice)) %>%
  head(n = 3)
Q4A4

#=============================================================================
#Merge Analysis 3 & Analysis4 into a Table
tableq434 <- rbind(Q4A3, Q4A4)
names(tableq434) = c("AreaLocality", "AveragePrice")
tableq434

#Make bar graph for A3-A4
tableq434 %>%
  mutate(AreaLocality = fct_reorder(AreaLocality, AveragePrice)) %>%
  ggplot(aes(x = AreaLocality, y = AveragePrice,fill = AreaLocality)) +
  geom_bar(stat = "identity",width = 0.5) +
  scale_fill_brewer(palette="Set1")+
  geom_text(aes(label=AveragePrice))+
  labs(x = "Average Price", y = "Area Locality", 
       title = "Chennai's Top 6 Area Locality with the Highest and Lowest Average Price ")

#=============================================================================
#Analysis 5: The median of top 10 rental price in Hyderabad's locality 
Q4A5=houseRental%>%
  filter(City == "Hyderabad")%>%
  group_by(Area_Locality)%>%
  summarise(rentHyde = Rent_Price) 
Q4A5

#Analysis 5 - Box Plot
ggplot(Q4A5,aes(y=rentHyde,x=1))+
  geom_boxplot()+
  coord_cartesian(ylim = c(0, 100000))+
  labs(y="Median Price")

#=============================================================================
#Analysis 6:The average rental price in different cities
Q4A6 = houseRental%>%
  filter(City == "Kolkata"|City == "Mumbai"|City == "Bangalore"|
           City == "Delhi"|City == "Chennai"|City == "Hyderabad") %>%
  group_by(City)%>%
  summarise(avgRent = sum(Rent_Price)/ n())
Q4A6

#Analysis 6 - Pie Chart
Q4A6 <- Q4A6%>% 
  arrange(desc(City)) %>%
  mutate(prop = avgRent / sum(Q4A6$avgRent) *100) %>%
  mutate(ypos = cumsum(prop)- 0.5*prop )
Q4A6

ggplot(Q4A6,aes(x="",y =prop ,fill=City))+
  geom_bar(stat="identity", width=1, color="white")+
  coord_polar("y", start=0)+
  theme_void()+ 
  geom_text(aes(y = ypos, label = round(avgRent)))+
  ggtitle("The Average Rental Price")

#=============================================================================       
#Analysis 7: The top 5 highest rental price in Mumbai’s locality
Q4A7 = houseRental%>%
  filter(City == "Mumbai")%>%
  group_by(Area_Locality)%>%
  summarise(MumbaiP = max(Rent_Price))%>%
  arrange(desc(MumbaiP))%>%
  head(n = 5)
Q4A7          

#=============================================================================
#Analysis 8: The top 5 lowest rental price in Mumbai’s locality
Q4A8 = houseRental%>%
  filter(City == "Mumbai")%>%
  group_by(Area_Locality)%>%
  summarise(MumbaiP = min(Rent_Price))%>%
  arrange(MumbaiP)%>%
  head(n = 5)
Q4A8 

#=============================================================================
#Merge Analysis 7 & Analysis 8 into a Table
tableq478 <- rbind(Q4A7, Q4A8)
names(tableq478) = c("AreaLocality", "MumbaiP")
tableq478

tableq478 %>%
  mutate(AreaLocality = fct_reorder(AreaLocality, MumbaiP)) %>%
  ggplot(aes(x = AreaLocality, y = MumbaiP,fill = AreaLocality)) +
  geom_bar(stat = "identity",width = 0.5) +
  scale_fill_brewer(palette="Spectral")+
  geom_text(aes(label=MumbaiP))+
  labs(y = "Rental Price", x = "Area Locality", 
       title = "Mumbai's Top 6 Area Locality with the Highest and Lowest Price ")

#=============================================================================
#Analysis 9: The median of rental price in Bangalore and Kolkata
Q4A9 = houseRental%>%
  filter(City == "Bangalore")%>%
  group_by(City)%>%
  summarise(rent = Rent_Price)
Q4A9

Q4A9_1 = houseRental%>%
  filter(City == "Kolkata")%>%
  group_by(City)%>%
  summarise(rent = Rent_Price)
Q4A9_1

#=============================================================================
#Merge Analysis 9 - BoxPlot
tableQ49 <- rbind(Q4A9, Q4A9_1)
names(tableQ49) = c("AreaLocality", "rent")
tableQ49

ggplot(tableQ49, aes(x=as.factor(AreaLocality), y=rent)) + 
  geom_boxplot(fill="slateblue", alpha=0.2) + 
  labs(y="Median Rental Price",x="Area Locality")+
  coord_cartesian(ylim = c(0, 80000))
#=============================================================================



#==================================================================================#
#Question 5:The relationship between the date, BHK and tenant preferred.
#Gui Wei Quan
#TP064031 
#====================================================================================
#5.1 The relationship between BHK and Bachelor preferred
Q5_1=
  houseRental %>% 
  filter (Tenant_Preferred == "Bachelors")%>%
  group_by(BHK,Tenant_Preferred)%>%
  summarise(Amount = sum(Tenant_Preferred =="Bachelors"))
print(Q5_1)

ggplot(Q5_1, aes(x=BHK, y=Amount)) + 
  geom_bar(stat = "identity",color = "blue",fill= "lightblue2")+
  geom_text(aes(label =Amount))+
  ggtitle("The Amout of Bachelors Tenant Preferred in Different BHK in Bar Chart")

#=====================================================================================

#5.2 The relationship between BHK and Family preferred
Q5_2=
  houseRental %>% 
  filter (Tenant_Preferred == "Family")%>%
  group_by(BHK,Tenant_Preferred)%>%
  summarise(Amount = sum(Tenant_Preferred =="Family"))
print(Q5_2)


pie(Q5_2$Amount,Q5_2$Amount,col = rainbow(6))+
  title("The Amout of Family Tenant Preferred in Different BHK in pie Chart")+
  legend("topright",legend =Q5_2$BHK,fill=rainbow(6))

#=====================================================================================

#5.3 The relationship between BHK and Bachelors/Family preferred
Q5_3=
  houseRental %>% 
  filter (Tenant_Preferred == "Bachelors/Family")%>%
  group_by(BHK,Tenant_Preferred)%>%
  summarise(Amount = sum(Tenant_Preferred =="Bachelors/Family"))
print(Q5_3)

ggplot(Q5_3, aes(x=BHK, y=Amount)) + 
  geom_line() +
  ggtitle("The Amout of Bachelors/Family Tenant Preferred in Different BHK in Bar Chart") +
  geom_text(aes(label =Amount))+
  theme_ipsum() +
  xlab("BHK") + ylab("Amount")


#=====================================================================================


#5.4	The relationship between city and Bachelor preferred to rent.
Q5_4=
  houseRental %>% 
  filter (Tenant_Preferred == "Bachelors")%>%
  group_by(City,Tenant_Preferred)%>%
  summarise(Amount = sum(Tenant_Preferred =="Bachelors"))
print(Q5_4)


#=====================================================================================

#5.5	The relationship between city and Family preferred to rent.
Q5_5=
  houseRental %>% 
  filter (Tenant_Preferred == "Family")%>%
  group_by(City,Tenant_Preferred)%>%
  summarise(Amount = sum(Tenant_Preferred =="Family"))
print(Q5_5)


#=====================================================================================

#5.6	The relationship between city and Bachelor/Family preferred to rent.
Q5_6=
  houseRental %>% 
  filter (Tenant_Preferred == "Bachelors/Family")%>%
  group_by(City,Tenant_Preferred)%>%
  summarise(Amount = sum(Tenant_Preferred =="Bachelors/Family"))
print(Q5_6)

#Graph for Q5_4~Q5_6

QH5_46<- rbind(Q5_4,Q5_5,Q5_6)%>%arrange(desc(Amount))
print(QH5_46)                   

# Note we convert the QH5_46 variable to in order to fill by Tenant_Preferred

ggplot (QH5_46, aes(x=City, y=Amount , fill=Tenant_Preferred)) +
  geom_bar (stat="identity", position ="dodge")+
  geom_text(aes(label =Amount))+
  ggtitle("The Amount of Any Tenant Preferred in Different City in Bar Chart")

#=====================================================================================

#5.7	The relationship between City and BHK preferred.
Q5_7A=
  houseRental %>% 
  filter(City == "Bangalore")%>%
  group_by(BHK,City)%>%
  summarise(Amount = sum(City == "Bangalore"))

print(Q5_7A)

Q5_7B=
  houseRental %>% 
  filter(City == "Chennai")%>%
  group_by(BHK,City)%>%
  summarise(Amount = sum(City == "Chennai"))

print(Q5_7B)

Q5_7C=
  houseRental %>% 
  filter(City == "Delhi")%>%
  group_by(BHK,City)%>%
  summarise(Amount = sum(City == "Delhi"))

print(Q5_7C)


Q5_7D=
  houseRental %>% 
  filter(City == "Hyderabad")%>%
  group_by(BHK,City)%>%
  summarise(Amount = sum(City == "Hyderabad"))

print(Q5_7D)

Q5_7E=
  houseRental %>% 
  filter(City == "Kolkata")%>%
  group_by(BHK,City)%>%
  summarise(Amount = sum(City == "Kolkata"))

print(Q5_7E)

Q5_7F=
  houseRental %>% 
  filter(City == "Mumbai")%>%
  group_by(BHK,City)%>%
  summarise(Amount = sum(City == "Mumbai"))

print(Q5_7F)


#Combine all the QH5_7A until QH5_7F data and make a Graph 

QH5_7All<- rbind(Q5_7A,Q5_7B,Q5_7C,Q5_7D,Q5_7E,Q5_7F)%>%arrange(City)


print(QH5_7All,n =40)


# Note we convert the QH5_711 variable to in order to fill by BHK.

ggplot (QH5_7All, aes(x=City, y=Amount , fill=BHK )) +
  geom_bar (stat="identity", position ="dodge")+
  geom_text(aes(label = Amount))+
  ggtitle("The relationship between city and BHK preferred")


#=====================================================================================

#Q5_8.The relationship between Area_type and Tenant_Preffered.

Q5_8=
  houseRental%>%
  group_by(Area_Type,Tenant_Preferred)%>%
  summarise(Amount = sum(Area_Type=="Built Area",
                         Area_Type=="Carpet Area",
                         Area_Type=="Super Area"))


print(Q5_8)


ggplot (Q5_8, aes(x=Tenant_Preferred, y=Amount , fill=Area_Type)) +
  geom_bar (stat="identity", position ="dodge")+
  geom_text(aes(label =Amount))+
  scale_fill_manual(values=c("#FCB5AC",
                             "#3D5B59",
                             "#B5E5CF"))+
  ggtitle("The Amount of Any Tenant Preferred in Different Area_Type in Bar Chart")






















