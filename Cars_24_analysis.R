#Name: Mohammed Saif Wasay
#NUID: 002815958
#ALY 6000 Introduction to Analytics

cat("\014") # clears console
rm(list = ls()) # clears global environment
try(dev.off(dev.list()["RStudioGD"]), silent = TRUE) # clears plots
try(p_unload(p_loaded(), character.only = TRUE), silent = TRUE) #clears packages
options(scipen = 100) # disables scientific notation for entire R session

#Loading Libraries
library(pacman)
p_load(tidyverse)
library(dplyr)

#reading cars24 dataset 
cars <- read.csv("data/cars_24_combined.csv")

head(cars,10)
names(cars)

#Making Column names in readable format
p_load(janitor)
cars <- clean_names(cars)
names(cars)

#Removing Unnecessary column x
cars <- subset(cars, select = -c(x))

glimpse(cars)

#Checking Data Types of the columns
sapply(cars, class)

#Checking for Missing data
sapply(cars, function(x) sum(x == ""))
sapply(cars, function(x) sum(x == "nan"))

#Checking for Nan values in data
colSums(is.na(cars))

#Since the Missing and NaN Values are relatively less removing NaN and Missing Values
cars <- drop_na(cars)
cars <- subset(cars, rowSums(cars == "") == 0)
cars_clean <- subset(cars, rowSums(cars == "nan") == 0)

#Checking NaN and Missing after cleaning data
sapply(cars_clean, function(x) sum(x == ""))
colSums(is.na(cars_clean))
sapply(cars_clean, function(x) sum(x == "nan"))

summary(cars_clean)

cars_count <- cars_clean %>% group_by(car_name) %>% 
  summarise(count = n(),
            mean_price  = mean(price),
            median_price = median(price)) %>% 
  arrange(desc(count))

cars_count

p_load(ggthemes)
#Counts of fuel type
fuel_type <- ggplot(cars_clean, aes(x = fuel)) + 
  geom_bar(fill = "blue") +
  geom_text(stat = "count", aes(label = ..count..), vjust = -0.5) +
  labs(title = "Bar Chart of Fuel Types", x = "Fuel Type", y = "Count") +
  theme_tufte()
fuel_type

#Counts of Transmission type
drive_type <- ggplot(cars_clean, aes(x = drive)) + 
  geom_bar(fill = "blue") +
  geom_text(stat = "count", aes(label = ..count..), vjust = -0.5) +
  labs(title = "Bar Chart of Transmission Types", x = "Transmission Type", y = "Count") +
  theme_tufte()
drive_type

# Create a boxplot of price by car type
ggplot(cars_clean, aes(x = type, y = price)) +
  geom_boxplot(fill = "orange") +
  labs(title = "Boxplot of Price by Car Type", x = "Car type", y = "Price") +
  theme_economist()

#Histogram for checking most popular car type per year
ggplot(cars_clean, aes(x = year, fill = type)) +
  geom_bar() +
  labs(title = "Barchart of Total Cars by Car Type", x = "Car Type", y = "Count") +
  theme_economist()

#Scatter Plot of Car prices based KM driven
ggplot(cars_clean, aes(x = distance, y = price, color = owner)) + geom_point() + 
  labs(title = "Scatter Plot of Distance Driven vs. Price", x = "KM Driven", y = "Price") +
  theme_tufte()


#Adding Brand Column and State
cars_clean$Brand <- sapply(strsplit(cars_clean$car_name, " "), function(x) x[1])
cars_clean$State <- sapply(strsplit(cars_clean$location, "-"), function(x) x[1])
cars_clean <- subset(cars_clean, State != "22")
cars_clean


#Brands Summaries
brand_summary <- cars_clean %>% group_by(Brand) %>% arrange(desc(price)) %>%
  summarise(count = n(), 
            mean_price = mean(price), 
            median_price = median(price)) %>% 
              arrange(desc(count))

brand_summary

#Most Cars per State
state_counts <- cars_clean %>% group_by(State) %>% summarise(cars_count = n()) %>%
  arrange(State, desc(cars_count))

state_counts

#Pie Chart of Total Cars per state
custom_colors <- c(
  "#336699", "#FF5733", "#CC3300", "#660000",
  "#009933", "#FF9900", "#996699", "#FFCC00",
  "#9900CC", "#0099CC", "#66CC33", "#FF6666",
  "#993366", "#00CC99", "#CC0033", "#FF9999"
)

ggplot(state_counts, aes(x = "", y = cars_count, fill = State)) +
  geom_bar(width = 1, stat = "identity") +
  coord_polar("y", start = 0) +
  scale_fill_manual(values = custom_colors) + theme_tufte() +
  labs(title = "Total Cars per State")

#Ranking Most Cars in a Year for each Brand 
brand_per_year <- cars_clean %>% group_by(Brand, year) %>% summarise(count = n()) %>%
   mutate(RankType =rank(-1 * count, ties.method = "min")) %>% arrange(RankType)

brand_per_year

#Top Brands each Year
top_selling_brands <- brand_per_year %>%
  group_by(year) %>%
  arrange(year, desc(count)) %>%
  slice(1) %>%
  ungroup()
top_selling_brands

ggplot(top_selling_brands, aes(x = factor(year), y = count, fill = Brand)) +
  geom_bar(stat = "identity", position = "dodge") +
  theme_tufte() +
  labs(title = "Most Selling Brand for Each Year", x = "Year", y = "Count")

#Type of Cars sold in a year by type
car_type_year <- cars_clean %>% group_by(type, year) %>% summarise(count = n()) %>%
  arrange(desc(count))
car_type_year

#Line Plot for Cars count per year
line_plt <- ggplot(car_type_year, aes (x = year, y = count, color = type)) + 
  geom_line(size = 1) + 
  labs(x = "Year", y = "Number of Cars", title = "Total Number of Cars Per Year ") + 
  theme_tufte()
line_plt

#Cars sold in a year by type
brand_type_counts <- cars_clean %>% group_by(Brand, type) %>% summarise(cars_count = n()) %>%
  arrange(Brand, type, desc(cars_count))

brand_type_counts

# Creating grouped bar chart for Type and Brand
ggplot(brand_type_counts, aes(x = type, y = cars_count, fill = Brand)) +
  geom_bar(stat = "identity", position = "dodge") +
  theme_tufte() +
  labs(title = "Grouped Bar Chart of Type and Brand by Count", x = "Type", y = "Count")

# Creating a grouped bar chart for Type and Distance driven
ggplot(cars_clean, aes(x = type, y = distance, fill = fuel)) +
  geom_bar(stat = "identity", position = "dodge") +
  theme_tufte() +
  labs(title = "Grouped Bar Chart of Type and Driven by Fuel Type", x = "Type", y = "Distance Driven")

