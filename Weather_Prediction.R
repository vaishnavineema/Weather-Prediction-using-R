#Setting up work directory
setwd("C:\\Users\\ADMIN\\Desktop\\DATA ANALYTICS")
weather <- read.csv("weather.csv")

#Changing column names for readability
names(weather) <- c("Date", "UTtime", "Temperature_K", "Relative_Humidity", "Pressure" , "Wind_Speed" , "Wind_Direction", "Rainfall" , "Shortwave_Irradiation")
summary(weather)

#Formatting the Date field in the dataset
a <- as.Date(weather$Date,format="%Y-%d-%m") # Produces NA when format is not "%Y/%d/%m"
b <- as.Date(weather$Date,format="%d-%m-%Y") # Produces NA when format is not "%d.%m.%Y"
a[is.na(a)] <- b[!is.na(b)] # Combine both while keeping their ranks
weather$Date <- a # Put it back in your dataframe

#Correlation between attributes
library(corrplot)
library(RColorBrewer)
corrplot(cor(weather[,-c(1,2)]), type="upper", order="hclust",
         col=brewer.pal(n=8, name="RdYlBu"))

#Finding the mean of each observations for a day i.e. scaling the data
library(sqldf)
weather_scaled <- sqldf("SELECT Date, avg(Temperature_K) as Temperature, avg(Relative_Humidity) as RelativeHumidity , avg(Pressure) as Pressure, avg(Wind_Speed) as Wind_Speed , avg(Wind_Direction) as Wind_Direction , avg(Rainfall) as Rainfall, avg(Shortwave_Irradiation) as Shortwave_Irradiation FROM weather GROUP BY Date")

#Breaking Date into day, month and year columns
weather_scaled <- data.frame(weather_scaled[,],
                 year = as.numeric(format(weather_scaled$Date, format = "%Y")),
                 month = as.numeric(format(weather_scaled$Date, format = "%m")),
                 day = as.numeric(format(weather_scaled$Date, format = "%d")))

a <- weather_scaled

#Correlation between attributes
library(corrplot)
library(RColorBrewer)
corrplot(cor(weather_scaled[,-c(1,9,10,11)]), type="upper", order="hclust",
         col=brewer.pal(n=8, name="RdYlBu"))

#EDA
library(ggplot2)
ggplot(weather_scaled, aes(weather_scaled$month, weather_scaled$Temperature)) + geom_point() + geom_smooth()
ggplot(weather_scaled, aes(weather_scaled$month, weather_scaled$RelativeHumidity)) + geom_point() + geom_smooth()
ggplot(weather_scaled, aes(weather_scaled$month, weather_scaled$Pressure)) + geom_point() + geom_smooth()
ggplot(weather_scaled, aes(weather_scaled$month, weather_scaled$Wind_Speed)) + geom_point() + geom_smooth()
ggplot(weather_scaled, aes(weather_scaled$month, weather_scaled$Wind_Direction)) + geom_point() + geom_smooth()
ggplot(weather_scaled, aes(weather_scaled$month, weather_scaled$Rainfall)) + geom_point() + geom_smooth()
ggplot(weather_scaled, aes(weather_scaled$month, weather_scaled$Shortwave_Irradiation)) + geom_point() + geom_smooth()

#We can see clear patterns in the attributes when plotted across month of the year
#This pattern can be used to make decision trees in order to categorize the data into summer, pleasant and monsoon
weather_scaled$Temperature <- scale(weather_scaled$Temperature)
weather_scaled$RelativeHumidity <- scale( weather_scaled$RelativeHumidity)
weather_scaled$Pressure <- scale(weather_scaled$Pressure)
weather_scaled$Wind_Speed <- scale(weather_scaled$Wind_Speed)
weather_scaled$Wind_Direction <- scale(weather_scaled$Wind_Direction)
weather_scaled$Rainfall <- scale(weather_scaled$Rainfall)
weather_scaled$Shortwave_Irradiation <- scale(weather_scaled$Shortwave_Irradiation)

#Clustering to find the optimal number of seasons in Goa
#Clustering
library(cluster)    # clustering algorithms
library(factoextra) # clustering algorithms & visualization

set.seed(123)
final <- kmeans(weather_scaled[c(2,4,6,8)], 3, nstart = 25)
fviz_cluster(final, data = weather_scaled[c(2,4,6,8)])

weather_scaled$Season <- cbind(weather_scaled$Season, final$cluster)

#Model
#Adding a column named Season to train the model
weather_scaled$Season <- ifelse(weather_scaled$Season == 1, "Summer", ifelse(weather_scaled$Season == 2, "Pleasant", "Rainy"))

weather_scaled[c(2:8)] <- a[c(2:8)]
  
#Training model
alpha = 0.7
intrain <- sample(1:nrow(weather_scaled), alpha * nrow(weather_scaled))
weather_train <- weather_scaled[intrain,]
weather_test <- weather_scaled[-intrain,]

#Building A Tree
library(rpart)
library(rpart.plot)
tree <- rpart(Season ???., data=weather_train[c(1:12)], method = "class", control = rpart.control(cp = 0, maxdepth = 8,minsplit = 120))
rpart.plot(tree, box.palette = "YlGnBl")

#Pruning
plotcp(tree)
tree <- prune(tree, cp = 0.12 )

#Predicting
pred <- predict(tree, weather_test[c(1:12)], type = 'class')
cm <- table(unlist(weather_test[c(12)]), pred)
Accuracy <- sum(diag(cm))/length(pred)

#GlobalWarming
ggplot(weather_scaled, aes(weather_scaled$Date, weather_scaled$Temperature)) + geom_point() + geom_smooth()
