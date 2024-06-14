#Load Libraries Needed for Project 
library(quantmod)
library(tidyverse)
library(janitor)
library(skimr)
library(lubridate)
library(e1071)
library(psych)
library(DataExplorer)
library(outliers)
library(stats)
library(rmarkdown)


# Data Preparations


# Getting the Data from the Quantmod Yahoo Finance API
# Define the stock symbols of the top 4 technology service and banking and financial services companies
symbols <- c("AAPL", "JPM", "MSFT", "BAC", "META", "WFC", "GOOG", "HSBC")

# Set the start and end dates
start_date <- as.Date("2019-01-01")
end_date <- as.Date("2023-12-31")

# Fetch the stock data from the API
getSymbols(symbols, src = "yahoo", from = start_date, to = end_date)

# Data Preprocessing 

# Passing the fetched data stored as objects into the dataframe for ease of manipulation 
apple_stockdata <- as.data.frame(AAPL)
jpmorgan_stockdata <- as.data.frame(JPM)
microsoft_stockdata <- as.data.frame(MSFT)
bankofAmerica_stockdata <- as.data.frame(BAC)
meta_stockdata <- as.data.frame(META)
wellsfargo_stockdata <- as.data.frame(WFC)
google_stockdata <- as.data.frame(GOOG)
hsbc_stockdata <- as.data.frame(HSBC)

# Handle inaccurate data type format 
# Make the first variable which is stored as a row to be a column and assign it the date as column name
apple_stockdata <- data.frame(date = rownames(apple_stockdata), 
                               apple_stockdata, row.names = NULL)
jpmorgan_stockdata <- data.frame(date = rownames(jpmorgan_stockdata), 
                                jpmorgan_stockdata, row.names = NULL)
microsoft_stockdata <- data.frame(date = rownames(microsoft_stockdata), 
                                   microsoft_stockdata, row.names = NULL)
bankofAmerica_stockdata <- data.frame(date = rownames(bankofAmerica_stockdata), 
                                bankofAmerica_stockdata, row.names = NULL)
meta_stockdata <- data.frame(date = rownames(meta_stockdata), 
                                      meta_stockdata, row.names = NULL)
wellsfargo_stockdata <- data.frame(date = rownames(wellsfargo_stockdata), 
                             wellsfargo_stockdata, row.names = NULL)
google_stockdata <- data.frame(date = rownames(google_stockdata), 
                                   google_stockdata, row.names = NULL)
hsbc_stockdata <- data.frame(date = rownames(hsbc_stockdata), 
                               hsbc_stockdata, row.names = NULL)

# Renaming column names for consistency 
apple_stockdata <- apple_stockdata %>% rename(
  Date = date,
  Open = AAPL.Open,
  High = AAPL.High,
  Low  = AAPL.Low,
  Close = AAPL.Close,
  Volume = AAPL.Volume,
  Adju_Close = AAPL.Adjusted
)

jpmorgan_stockdata <- jpmorgan_stockdata %>% rename(
  Date = date,
  Open = JPM.Open,
  High = JPM.High,
  Low  = JPM.Low,
  Close = JPM.Close,
  Volume = JPM.Volume,
  Adju_Close = JPM.Adjusted
)

microsoft_stockdata <- microsoft_stockdata %>% rename(
  Date = date,
  Open = MSFT.Open,
  High = MSFT.High,
  Low  = MSFT.Low,
  Close = MSFT.Close,
  Volume = MSFT.Volume,
  Adju_Close = MSFT.Adjusted
)

bankofAmerica_stockdata <- bankofAmerica_stockdata %>% rename(
  Date = date,
  Open = BAC.Open,
  High = BAC.High,
  Low  = BAC.Low,
  Close = BAC.Close,
  Volume = BAC.Volume,
  Adju_Close = BAC.Adjusted
)

meta_stockdata <- meta_stockdata %>% rename(
  Date = date,
  Open = META.Open,
  High = META.High,
  Low  = META.Low,
  Close = META.Close,
  Volume = META.Volume,
  Adju_Close = META.Adjusted
)

wellsfargo_stockdata <- wellsfargo_stockdata %>% rename(
  Date = date,
  Open = WFC.Open,
  High = WFC.High,
  Low  = WFC.Low,
  Close = WFC.Close,
  Volume = WFC.Volume,
  Adju_Close = WFC.Adjusted
)

google_stockdata <- google_stockdata %>% rename(
  Date = date,
  Open = GOOG.Open,
  High = GOOG.High,
  Low  = GOOG.Low,
  Close = GOOG.Close,
  Volume = GOOG.Volume,
  Adju_Close = GOOG.Adjusted
)

hsbc_stockdata <- hsbc_stockdata %>% rename(
  Date = date,
  Open = HSBC.Open,
  High = HSBC.High,
  Low  = HSBC.Low,
  Close = HSBC.Close,
  Volume = HSBC.Volume,
  Adju_Close = HSBC.Adjusted
)

# Checking for Missing Values in each individual stock dataset 
skim_without_charts(apple_stockdata)
skim_without_charts(bankofAmerica_stockdata)
skim_without_charts(jpmorgan_stockdata)
skim_without_charts(hsbc_stockdata)
skim_without_charts(meta_stockdata)
skim_without_charts(google_stockdata)
skim_without_charts(wellsfargo_stockdata)
skim_without_charts(microsoft_stockdata)

# Adding Stock symbol as a column name in each dataset before combining into a big dataset
apple_stockdata$Symbol <- "AAPL"
jpmorgan_stockdata$Symbol <- "JPM"
microsoft_stockdata$Symbol <- "MSFT"
bankofAmerica_stockdata$Symbol <- "BAC"
meta_stockdata$Symbol <- "META"
wellsfargo_stockdata$Symbol <- "WFC"
google_stockdata$Symbol <- "GOOG"
hsbc_stockdata$Symbol <- "HSBC"

# Combining all data into a single dataframe 
stock_market_data <- apple_stockdata
stock_market_data <- rbind(stock_market_data, jpmorgan_stockdata)
stock_market_data <- rbind(stock_market_data, microsoft_stockdata)
stock_market_data <- rbind(stock_market_data, bankofAmerica_stockdata)
stock_market_data <- rbind(stock_market_data, meta_stockdata)
stock_market_data <- rbind(stock_market_data, wellsfargo_stockdata)
stock_market_data <- rbind(stock_market_data, google_stockdata)
stock_market_data <- rbind(stock_market_data, hsbc_stockdata)

# Add Sector and Company Column based on the Stock Symbols
sector_company_lookup <- data.frame(
  Symbol = c("AAPL", "JPM", "MSFT", "BAC", "META", "WFC", "GOOG", "HSBC"),
  Sector = c("Technology", "Financials", "Technology", "Financials", "Technology", "Financials", "Technology", "Financials"),
  Company = c("Apple Inc.", "JPMorgan Chase & Co.", 
              "Microsoft Corporation", "Bank of America Corporation", 
              "Meta Platforms, Inc.", "Wells Fargo" , "Google", "HSBC Holdings plc")
)
 
# Using Left Joint too add the Sector and Company name based on the symbol
# as primary key in existing dataframe and foreign key in newly created dataframe
stock_market_data <- merge(stock_market_data, sector_company_lookup, by = "Symbol", all.x = TRUE)

# Reorder the columns so that the Date column is first
stock_market_data <-stock_market_data[, c("Date", "Open", "High", "Low"
                               , "Close", "Volume", "Adju_Close","Symbol", "Sector", "Company")]




# Feature Engineering to create new columns for date,  Daily market return, and volatility
# Convert Date Column stored as a character to a date data type
#stockmarket_data <- base::as.Date(stock_market_data$Date)
#stock_market_data <- cbind(stock_market_data, stockmarket_data)
#stock_market_data <- stock_market_data %>% select(-Date) %>%
#rename(Date = stockmarket_data)

stock_market_data$Date <- as.Date(stock_market_data$Date)



# 1 Extracting Daily Market Return  and Volatility variables
stock_market_data <- stock_market_data %>% 
  mutate(Market_Return = (Close-Open)/ Close) %>%
  mutate(Volatility = ((High-Low) / Close) * 100) 
# 2 Extract Year, Month, Quarter, Day, and Day of week
stock_market_data <- stock_market_data %>%
  mutate(Year = format(as.Date(Date), "%Y")) %>% 
  mutate(Month = format(as.Date(Date), "%B")) %>% 
  mutate(Day = format(as.Date(Date), "%d")) %>% 
  mutate(Day_of_Week = format(as.Date(Date), "%A"))
stock_market_data$Quarter <- paste("Q", quarter(stock_market_data$Date), sep = "")


# Changing the order of variables 
stock_market_data$Quarter <- ordered(stock_market_data$Quarter, 
                                     levels=c("Q1", "Q2", "Q3", "Q4" 
                                     ))
stock_market_data$Month <- ordered(stock_market_data$Month, 
                                   levels=c("January", "February", "March", 
                                            "April", "May", "June", 
                                            "July", "August", "September", 
                                            "October", "November", "December"))
stock_market_data$Day_of_Week <- ordered(stock_market_data$Day_of_Week, 
                                         levels=c("Monday", "Tuesday", 
                                                  "Wednesday", "Thursday", 
                                                  "Friday"
                                         ))

# Detect Outliers in Data with Box Plot
# Checking for possible outliers
numeric_var <- stock_market_data %>% select(Open,High,Low,Close, Adju_Close, Volume, Volatility, Market_Return)
outlier(numeric_var)
# changing the format from a wide to long data
numeric_long_data <- numeric_var %>% 
  pivot_longer(cols = c(Open, High, Low, Close, Adju_Close, Volume, Volatility, Market_Return), 
               names_to = "Metrics", values_to = "Value")
# Create the box plot for all numeric variables
ggplot(numeric_long_data, aes(x = Metrics, y = Value, fill=Metrics)) + 
  geom_boxplot() +
  labs(title = "Outlier Detection Across Numeric Metrics", x = "Metrics", y = "Value") +
  facet_wrap(~Metrics, scales = "free_y") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))





# Understand The Features and Characteristics of the Data with basic Descriptive Statistics
head(stock_market_data) # Returns First 6 rows of the dataset
tail(stock_market_data) # Returns the last 6 rows of the dataset
skim_without_charts(stock_market_data) #  a summary of Features and makeup of the dataset
summary(stock_market_data) # For Basic Descriptive Statistics 
str(stock_market_data) # To understand the characteristics of each variables
glimpse(stock_market_data) # Get a view brief view of the dataset


# Statistical Methods For Analysis and Insights

#Calculate skewness and kurtosis to understand distribution of numeric variables
apply(numeric_var[, sapply(numeric_var, is.numeric)], 2, skewness)
apply(numeric_var[, sapply(numeric_var, is.numeric)], 2, kurtosis)
# Plotting Histogram and Density chart for distribution of numeric variables
ggplot(numeric_long_data, aes(x = Value, fill=Metrics)) +
  geom_histogram(aes(y = ..density..), bins = 30, color = "black") +
  geom_density(alpha = 0.2, size=0.7) +
  facet_wrap(~ Metrics, scales = "free") +
  labs(title = "Density and Distribution Across Numeric Metrics", x = "Value", y = "Density") +
  theme_minimal()

 
# Distribution of Categorical Variables 
character_long_data <- stock_market_data %>%
  select(Symbol, Sector, Company,Year, Month, 
         Day, Day_of_Week, Quarter) %>%
  gather("Variable", "Value", Symbol:Quarter)
# Create a Bar Plot for frequency distrubtion of categorical Variables
ggplot(character_long_data, aes(x=Value, fill=Value)) +
  geom_bar(position = "dodge", show.legend = FALSE) +
  facet_wrap(~ Variable, scales = "free") +
  labs(title = "Frequency Distribution of Categorical Variables", 
       x = "Category", 
       y = "Frequency") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


   
# Summary of some descriptive Statistics for key metrics based on sector
# 1 for Close Price
stock_market_data %>% group_by(Sector) %>%
  summarise(Min  = min(Close),
            Max = max(Close),
            Average = mean(Close),
            Variance = var(Close),
            Standard_deviation = sd(Close)
            ) %>% 
  pivot_longer(cols = -Sector, 
               names_to = "Statistic", 
               values_to = "Value") %>%
  ggplot(aes(x = Sector, y = Value, fill = Sector)) + 
  geom_col(position = "dodge", width = 0.4) +
  scale_fill_manual(values = 
                      c("Financials" = "#467ab0", 
                        "Technology" = "#009933")) +
  facet_wrap(~Statistic, scales = "free") +
  theme_minimal() +
  labs(title = "Close Price Summary Statistics by Sector",
       x = "Sector",
       y = "Value") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
# 2 for Volume
stock_market_data %>% group_by(Sector) %>%
  summarise(Min  = min(Volume),
            Max = max(Volume),
            Average = mean(Volume),
            Variance = var(Volume),
            Standard_deviation = sd(Volume)
            ) %>% 
  pivot_longer(cols = -Sector, 
               names_to = "Statistic", 
               values_to = "Value") %>%
  ggplot(aes(x = Sector, y = Value, fill = Sector)) + 
  geom_col(position = "dodge", width = 0.4) +
  scale_fill_manual(values = 
                      c("Financials" = "#467ab0", 
                        "Technology" = "#009933")) +
  facet_wrap(~Statistic, scales = "free") +
  theme_minimal() +
  labs(title = "Trading Volume Summary Statistics by Sector",
       x = "Sector",
       y = "Value") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
# 3 for Volatility
stock_market_data %>% group_by(Sector) %>%
  summarise(Min  = min(Volatility),
            Max = max(Volatility),
            Average = mean(Volatility),
            Variance = var(Volatility),
            Standard_deviation = sd(Volatility)
            ) %>% 
  pivot_longer(cols = -Sector, 
               names_to = "Statistic", 
               values_to = "Value") %>%
  ggplot(aes(x = Sector, y = Value, fill = Sector)) + 
  geom_col(position = "dodge", width = 0.4) +
  scale_fill_manual(values = 
                      c("Financials" = "#467ab0", 
                        "Technology" = "#009933")) +
  facet_wrap(~Statistic, scales = "free") +
  theme_minimal() +
  labs(title = "Sectoral Volatility Breakdown: A Statistical Overview",
       x = "Sector",
       y = "Value") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
# 4 for Market_Return
stock_market_data %>% group_by(Sector) %>%
  summarise(Min  = min(Market_Return),
            Max = max(Market_Return),
            Average = mean(Market_Return),
            Variance = var(Market_Return),
            Standard_deviation = sd(Market_Return)
            ) %>% 
  pivot_longer(cols = -Sector, 
               names_to = "Statistic", 
               values_to = "Value") %>%
  ggplot(aes(x = Sector, y = Value, fill = Sector)) + 
  geom_col(position = "dodge", width = 0.4) +
  scale_fill_manual(values = 
                      c("Financials" = "#467ab0", 
                        "Technology" = "#009933")) +
  facet_wrap(~Statistic, scales = "free") +
  theme_minimal() +
  labs(title = "Comparative Sector Returns: A Statistical Overview",
       x = "Sector",
       y = "Value") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
  




# Calculate Correlation Coefficient to understand how variables relate with each other 
cor(numeric_var)
# Plotting scatter plot to show some key correlation results 
# 1 Open Vs Close
ggplot(data = stock_market_data, aes(x=Open, y=Close, color= Sector)) +
  geom_point(alpha=0.5) + 
  theme_minimal() +
  scale_color_manual(values = 
                      c("Financials" = "#467ab0", 
                        "Technology" = "#009933")) +
   labs(title = "Daily Price Journey: A Comparison of Open vs. Close Prices", 
        x="Open",
        y="Close") +
  facet_wrap(~ Sector)
# 2 High Vs Low
ggplot(data = stock_market_data, aes(x=High, y=Low, color= Sector)) +
  geom_point(alpha=0.5) + 
  theme_minimal() +
  scale_color_manual(values = 
                      c("Financials" = "#467ab0", 
                        "Technology" = "#009933")) +
   labs(title = "Daily Price Range Explored: High Prices Compared to Low Prices", 
        x="High",
        y="Low") +
  facet_wrap(~ Sector)
# 3 Close Vs Market Return
ggplot(data = stock_market_data, aes(x=Close, y=Market_Return, color= Sector)) +
  geom_point(alpha=0.5) + 
  geom_smooth(size=0.5) +
  theme_minimal() +
  scale_color_manual(values = 
                      c("Financials" = "#467ab0", 
                        "Technology" = "#009933")) +
   labs(title = "Exploring the Correlation: Close Price and Market Returns", 
        x="Close",
        y="Market Return") +
  facet_wrap(~ Sector)
# 4 Volume Vs Market Return
ggplot(data = stock_market_data, aes(x=Volume, y=Market_Return, color= Sector)) +
  geom_point(alpha=0.5) + 
  geom_smooth(size=0.5) +
  theme_minimal() +
  scale_color_manual(values = 
                      c("Financials" = "#467ab0", 
                        "Technology" = "#009933")) +
   labs(title = "Investigating Trading Volume's Influence on Daily Market Returns", 
        x="Volume",
        y="Market Return") +
  facet_wrap(~ Sector)
# 5 Volume Vs Volatility
ggplot(data = stock_market_data, aes(x=Volume, y=Volatility, color= Sector)) +
  geom_point(alpha=0.5) + 
  geom_smooth(size=0.5) +
  theme_minimal() +
  scale_color_manual(values = 
                      c("Financials" = "#467ab0", 
                        "Technology" = "#009933")) +
   labs(title = "Volume vs. Volatility: Mapping the Dynamics of the Market", 
        x="Volume",
        y="Volatility") +
  facet_wrap(~ Sector)
# 6 High Vs Volatility
ggplot(data = stock_market_data, aes(x=High, y=Volatility, color= Sector)) +
  geom_point(alpha=0.5) + 
  theme_minimal() +
  scale_color_manual(values = 
                      c("Financials" = "#467ab0", 
                        "Technology" = "#009933")) +
   labs(title = "High Prices and Market Volatility: Analyzing Their Connection", 
        x="High",
        y="Volatility") +
  facet_wrap(~ Sector)
# 7 Market Return Vs Volatility
ggplot(data = stock_market_data, aes(x=Market_Return, y=Volatility, color= Sector)) +
  geom_point(alpha=0.5) +
  geom_smooth(size=0.5) +
  theme_minimal() +
  scale_color_manual(values = 
                      c("Financials" = "#467ab0", 
                        "Technology" = "#009933")) +
   labs(title = "Risk and Reward: Analyzing the Relationship Between Daily Market Return and Volatility", 
        x="Market Return",
        y="Volatility") +
  facet_wrap(~ Sector)
  
#correlation  btw year taking year as an independent variable and other variables to understand trend 
as.numeric(stock_market_data$Year) %>%
  cor(stock_market_data[sapply(stock_market_data, is.numeric)]) 
# 1. Year vs Close Price
ggplot(stock_market_data, aes(x = as.numeric(Year), y = Close, color = Sector)) + 
  geom_point(alpha = 0.5) +  # Scatter plot
  geom_smooth(method = "lm", se = FALSE, color = "#696969", size=0.5) +
  scale_color_manual(values = 
                       c("Financials" = "#467ab0", 
                         "Technology" = "#009933")) +
  facet_wrap(~Sector) +  # Separate plots for each sector
  labs(title = "Trend Analysis: Yearly Close Price Evolution by Sector",
       x = "Year",
       y = "Close Price") +
  theme_minimal()
# 2. Year vs Volatility
ggplot(stock_market_data, aes(x = as.numeric(Year), y = Volatility, color = Sector)) + 
  geom_point(alpha = 0.5) +  # Scatter plot
  geom_smooth(method = "lm", se = FALSE, color = "#696969", size=0.5) +
  scale_color_manual(values = 
                       c("Financials" = "#467ab0", 
                         "Technology" = "#009933")) +
  facet_wrap(~Sector) +  # Separate plots for each sector
  labs(title = "Trend Analysis: Yearly Market Volatility by Sector",
       x = "Year",
       y = "Volatility") +
  theme_minimal()
# 3. Year vs Volume
ggplot(stock_market_data, aes(x = as.numeric(Year), y = Volume, color = Sector)) + 
  geom_point(alpha = 0.5) +  # Scatter plot
  geom_smooth(method = "lm", se = FALSE, color = "#696969", size=0.5) +
  scale_color_manual(values = 
                       c("Financials" = "#467ab0", 
                         "Technology" = "#009933")) +
  facet_wrap(~Sector) +  # Separate plots for each sector
  labs(title = "Trend Analysis: Trading Volume Over Years by Sector",
       x = "Year",
       y = "Volume") +
  theme_minimal()

# Correlation for Average Market Returns of Sectors across Year
stock_market_data %>%
  group_by(Year, Sector) %>%
  summarise(Avg_Market_Return = mean(Market_Return, na.rm = TRUE), .groups = 'drop') %>%
  spread(Sector, Avg_Market_Return) %>%
  select(-Year) %>%  # Remove the Year column to ensure 'x' is numeric
  cor(use = "pairwise.complete.obs")
# Plotting a scatter plot to show relationship
  stock_market_data %>% group_by(Year, Sector) %>%
  summarise(Avg_Market_Return = mean(Market_Return, na.rm = TRUE), .groups = 'drop') %>%
 pivot_longer(cols = Avg_Market_Return, names_to = "Metrics", values_to = "Values" ) %>%
  ggplot(aes(x = Year, y = Values, color = Sector)) +
   geom_point(size = 3, alpha=0.7) +  # Scatter points
   geom_line(aes(group = Sector), size = 0.8) +  # Line to connect points
   scale_color_manual(values = c("Technology" = "#009933", "Financials" = "#467ab0")) +
   labs(title = "Yearly Average Market Returns: Technology vs. Financials",
        x = "Year",
        y = "Average Market Return") +
   theme_minimal()
   






# Seasonal Analysis
# 1 Exploring total and average Volume in relation to month, year, day_of-week and Quarter
# Volume based by Sector for Each Year 
stock_market_data %>%
  group_by(Sector, Year) %>%
  summarise(Total_Volume = sum(Volume, na.rm = TRUE),
            Average_Volume = mean(Volume, na.rm = TRUE), .groups = 'drop') %>%
  pivot_longer(cols = c(Average_Volume, Total_Volume), names_to = "Measure", values_to = "Value") %>%
  mutate(Year = as.factor(Year)) %>%
  ggplot(aes(x=Year, y=Value, fill=Sector)) + 
  geom_col(position="dodge", width=0.5) +
  scale_fill_manual(values = 
                      c("Financials" = "#467ab0", 
                        "Technology" = "#009933")) +
  labs(title = "Annual Trading Volume Analysis by Sector", 
       x = "Year", 
       y = "Volume") +
  facet_wrap(~Measure, scales = "free_y") + 
  theme_minimal() +
  theme(axis.text.x = element_text(angle=45, vjust=1, hjust=1)) 
# Volume based by Sector for Each Quarter 
stock_market_data %>%
  group_by(Sector, Quarter) %>%
  summarise(Total_Volume = sum(Volume, na.rm = TRUE),
            Average_Volume = mean(Volume, na.rm = TRUE), .groups = 'drop') %>%
  pivot_longer(cols = c(Average_Volume, Total_Volume), names_to = "Measure", values_to = "Value") %>%
  mutate(Quarter = as.factor(Quarter)) %>%
  ggplot(aes(x=Quarter, y=Value, fill=Sector)) + 
  geom_col(position="dodge", width=0.5) +
  scale_fill_manual(values = 
                      c("Financials" = "#467ab0", 
                        "Technology" = "#009933")) +
  labs(title = "Quarterly Volume Patterns Across Sectors", 
       x = "Quarter", 
       y = "Volume") +
  facet_wrap(~Measure, scales = "free_y") + 
  theme_minimal() +
  theme(axis.text.x = element_text(angle=45, vjust=1, hjust=1)) 
# Volume based by Sector for Each Month 
stock_market_data %>%
  group_by(Sector, Month) %>%
  summarise(Total_Volume = sum(Volume, na.rm = TRUE),
            Average_Volume = mean(Volume, na.rm = TRUE), .groups = 'drop') %>%
  pivot_longer(cols = c(Average_Volume, Total_Volume), names_to = "Measure", values_to = "Value") %>%
  mutate(Month = as.factor(Month)) %>%
  ggplot(aes(x=Month, y=Value, fill=Sector)) + 
  geom_col(position="dodge", width=0.8) +
  scale_fill_manual(values = 
                      c("Financials" = "#467ab0", 
                        "Technology" = "#009933")) +
  labs(title = "Monthly Sector Volume Dynamics", 
       x = "Month", 
       y = "Volume") +
  facet_wrap(~Measure, scales = "free_y") + 
  theme_minimal() +
  theme(axis.text.x = element_text(angle=45, vjust=1, hjust=1)) 
# Volume based by Sector for Each Day of the Week
stock_market_data %>%
  group_by(Sector, Day_of_Week) %>%
  summarise(Total_Volume = sum(Volume, na.rm = TRUE),
            Average_Volume = mean(Volume, na.rm = TRUE), .groups = 'drop') %>%
  pivot_longer(cols = c(Average_Volume, Total_Volume), names_to = "Measure", values_to = "Value") %>%
  mutate(Day_of_Week = as.factor(Day_of_Week)) %>%
  ggplot(aes(x=Day_of_Week, y=Value, fill=Sector)) + 
  geom_col(position="dodge", width=0.7, size=0.5) +
  scale_fill_manual(values = 
                      c("Financials" = "#467ab0", 
                        "Technology" = "#009933")) +
  labs(title = "Weekly Volume Patterns by Sector", 
       x = "Days of Week", 
       y = "Volume") +
  facet_wrap(~Measure, scales = "free_y") + 
  theme_minimal() +
  theme(axis.text.x = element_text(angle=45, vjust=1, hjust=1)) 

# 2 Exploring total and average Volatility in relation to month, year, day_of-week and Quarter
# Volatility based by Sector for Each Year 
stock_market_data %>%
  group_by(Sector, Year) %>%
  summarise(Total_Volatility = sum(Volatility, na.rm = TRUE),
            Average_Volatility = mean(Volatility, na.rm = TRUE), .groups = 'drop') %>%
  pivot_longer(cols = c(Average_Volatility, Total_Volatility), names_to = "Measure", values_to = "Value") %>%
  mutate(Year = as.factor(Year)) %>%
  ggplot(aes(x=Year, y=Value, fill=Sector)) + 
  geom_col(position="dodge", width=0.5) +
  scale_fill_manual(values = 
                      c("Financials" = "#467ab0", 
                        "Technology" = "#009933")) +
  labs(title = "Annual Volatility Analysis by Sector", 
       x = "Year", 
       y = "Volatility") +
  facet_wrap(~Measure, scales = "free_y") + 
  theme_minimal() +
  theme(axis.text.x = element_text(angle=45, vjust=1, hjust=1)) 
# Volatility based by Sector for Each Quarter 
stock_market_data %>%
  group_by(Sector, Quarter) %>%
  summarise(Total_Volatility = sum(Volatility, na.rm = TRUE),
            Average_Volatility = mean(Volatility, na.rm = TRUE), .groups = 'drop') %>%
  pivot_longer(cols = c(Average_Volatility, Total_Volatility), names_to = "Measure", values_to = "Value") %>%
  mutate(Quarter = as.factor(Quarter)) %>%
  ggplot(aes(x=Quarter, y=Value, fill=Sector)) + 
  geom_col(position="dodge", width=0.5) +
  scale_fill_manual(values = 
                      c("Financials" = "#467ab0", 
                        "Technology" = "#009933")) +
  labs(title = "Quarterly Sector Volatility Analysis", 
       x = "Quarter", 
       y = "Volatility") +
  facet_wrap(~Measure, scales = "free_y") + 
  theme_minimal() +
  theme(axis.text.x = element_text(angle=45, vjust=1, hjust=1)) 
# Volatility based by Sector for Each Month 
stock_market_data %>%
  group_by(Sector, Month) %>%
  summarise(Total_Volatility = sum(Volatility, na.rm = TRUE),
            Average_Volatility = mean(Volatility, na.rm = TRUE), .groups = 'drop') %>%
  pivot_longer(cols = c(Average_Volatility, Total_Volatility), names_to = "Measure", values_to = "Value") %>%
  mutate(Month = as.factor(Month)) %>%
  ggplot(aes(x=Month, y=Value, fill=Sector)) + 
  geom_col(position="dodge", width=0.8) +
  scale_fill_manual(values = 
                      c("Financials" = "#467ab0", 
                        "Technology" = "#009933")) +
  labs(title = "Monthly Volatility Patterns Across Sectors", 
       x = "Month", 
       y = "Volatility") +
  facet_wrap(~Measure, scales = "free_y") + 
  theme_minimal() +
  theme(axis.text.x = element_text(angle=45, vjust=1, hjust=1)) 
# Volatility based by Sector for Each Day of the Week
stock_market_data %>%
  group_by(Sector, Day_of_Week) %>%
  summarise(Total_Volatility = sum(Volatility, na.rm = TRUE),
            Average_Volatility = mean(Volatility, na.rm = TRUE), .groups = 'drop') %>%
  pivot_longer(cols = c(Average_Volatility, Total_Volatility), names_to = "Measure", values_to = "Value") %>%
  mutate(Day_of_Week = as.factor(Day_of_Week)) %>%
  ggplot(aes(x=Day_of_Week, y=Value, fill=Sector)) + 
  geom_col(position="dodge", width=0.7, size=0.5) +
  scale_fill_manual(values = 
                      c("Financials" = "#467ab0", 
                        "Technology" = "#009933")) +
  labs(title = "Weekly Volatility Insights by Sector", 
       x = "Days of Week", 
       y = "Volatility") +
  facet_wrap(~Measure, scales = "free_y") + 
  theme_minimal() +
  theme(axis.text.x = element_text(angle=45, vjust=1, hjust=1)) 

# 3 Exploring total and average Daily Market_Return in relation to month, year, day_of-week and Quarter
# Market Return based by Sector for Each Year 
stock_market_data %>%
  group_by(Sector, Year) %>%
  summarise(Total_Market_Return = sum(Market_Return, na.rm = TRUE),
            Average_Market_Return = mean(Market_Return, na.rm = TRUE), .groups = 'drop') %>%
  pivot_longer(cols = c(Average_Market_Return, Total_Market_Return), names_to = "Measure", values_to = "Value") %>%
  mutate(Year = as.factor(Year)) %>%
  ggplot(aes(x=Year, y=Value, fill=Sector)) + 
  geom_col(position="dodge", width=0.5) +
  scale_fill_manual(values = 
                      c("Financials" = "#467ab0", 
                        "Technology" = "#009933")) +
  labs(title = "Yearly Market Return Comparison by Sector", 
       x = "Year", 
       y = "Market Return") +
  facet_wrap(~Measure, scales = "free_y") + 
  theme_minimal() +
  theme(axis.text.x = element_text(angle=45, vjust=1, hjust=1)) 
# Market Return based by Sector for Each Quarter 
stock_market_data %>%
  group_by(Sector, Quarter) %>%
  summarise(Total_Market_Return = sum(Market_Return, na.rm = TRUE),
            Average_Market_Return = mean(Market_Return, na.rm = TRUE), .groups = 'drop') %>%
  pivot_longer(cols = c(Average_Market_Return, Total_Market_Return), names_to = "Measure", values_to = "Value") %>%
  mutate(Quarter = as.factor(Quarter)) %>%
  ggplot(aes(x=Quarter, y=Value, fill=Sector)) + 
  geom_col(position="dodge", width=0.5) +
  scale_fill_manual(values = 
                      c("Financials" = "#467ab0", 
                        "Technology" = "#009933")) +
  labs(title = "Sector Returns: A Quarterly Perspective", 
       x = "Quarter", 
       y = "Market Return") +
  facet_wrap(~Measure, scales = "free_y") + 
  theme_minimal() +
  theme(axis.text.x = element_text(angle=45, vjust=1, hjust=1)) 
# Market Return based by Sector for Each Month 
stock_market_data %>%
  group_by(Sector, Month) %>%
  summarise(Total_Market_Return = sum(Market_Return, na.rm = TRUE),
            Average_Market_Return = mean(Market_Return, na.rm = TRUE), .groups = 'drop') %>%
  pivot_longer(cols = c(Average_Market_Return, Total_Market_Return), names_to = "Measure", values_to = "Value") %>%
  mutate(Month = as.factor(Month)) %>%
  ggplot(aes(x=Month, y=Value, fill=Sector)) + 
  geom_col(position="dodge", width=0.8) +
  scale_fill_manual(values = 
                      c("Financials" = "#467ab0", 
                        "Technology" = "#009933")) +
  labs(title = "Exploring Monthly Returns by Sector", 
       x = "Month", 
       y = "Market Return") +
  facet_wrap(~Measure, scales = "free_y") + 
  theme_minimal() +
  theme(axis.text.x = element_text(angle=45, vjust=1, hjust=1)) 
# Market Return based by Sector for Each Day of the Week
stock_market_data %>%
  group_by(Sector, Day_of_Week) %>%
  summarise(Total_Market_Return = sum(Market_Return, na.rm = TRUE),
            Average_Market_Return = mean(Market_Return, na.rm = TRUE), .groups = 'drop') %>%
  pivot_longer(cols = c(Average_Market_Return, Total_Market_Return), names_to = "Measure", values_to = "Value") %>%
  mutate(Day_of_Week = as.factor(Day_of_Week)) %>%
  ggplot(aes(x=Day_of_Week, y=Value, fill=Sector)) + 
  geom_col(position="dodge", width=0.7, size=0.5) +
  scale_fill_manual(values = 
                      c("Financials" = "#467ab0", 
                        "Technology" = "#009933")) +
  labs(title = "Day-of-Week Returns Analysis by Sector", 
       x = "Days of Week", 
       y = "Market Return") +
  facet_wrap(~Measure, scales = "free_y") + 
  theme_minimal() +
  theme(axis.text.x = element_text(angle=45, vjust=1, hjust=1)) 



#Time Series Analysis 
# 1 Trend of Average Close Price Over the Years
stock_market_data %>% group_by(Date, Sector) %>%
  summarise(Average_Close = mean(Close)) %>%
  ggplot(aes(x=Date, y=Average_Close, color=Sector))+
  geom_line() +
  scale_color_manual(values = 
                      c("Financials" = "#467ab0", 
                        "Technology" = "#009933")) +
  labs(title = "Sectorial Trends: Average Close Price Evolution Over Time", 
       x="Date",y="Average Close Price") +
  facet_wrap( ~ Sector) +
  theme_minimal()
# 2 Trend of Average Volume Over the Years 
stock_market_data %>% group_by(Date, Sector) %>%
  summarise(Average_Volume = mean(Volume)) %>%
  ggplot(aes(x=Date, y=Average_Volume, color=Sector))+
  geom_line() +
  geom_smooth( size=0.7, color="#696969") +
  scale_color_manual(values = 
                       c("Financials" = "#467ab0", 
                         "Technology" = "#009933")) +
  labs(title = "Trading Volume Trends by Sector Over Time", 
       x="Date",y="Average Volume") +
  facet_wrap( ~ Sector) +
  theme_minimal()
# 3 Trend of Average Volatility Over the Years 
stock_market_data %>% group_by(Date, Sector) %>%
  summarise(Average_Volatility = mean(Volatility)) %>%
  ggplot(aes(x=Date, y=Average_Volatility, color=Sector))+
  geom_line() +
  geom_smooth( size=0.7, color="#696969") +
  scale_color_manual(values = 
                       c("Financials" = "#467ab0", 
                         "Technology" = "#009933")) +
  labs(title = "Volatility Patterns Across Sectors Over Time", x="Date",y="Average Volatility") +
  facet_wrap( ~ Sector) +
  theme_minimal()
# 4 Trend of Average Daily Market Return Over the Years 
stock_market_data %>% group_by(Date, Sector) %>%
  summarise(Average_Market_Return = mean(Market_Return)) %>%
  ggplot(aes(x=Date, y=Average_Market_Return, color=Sector))+
  geom_line() +
  scale_color_manual(values = 
                       c("Financials" = "#467ab0", 
                         "Technology" = "#009933")) +
  labs(title = "Comparative Analysis: Daily Market Returns by Sector Over Years", x="Date",y="Average Market Return") +
  facet_wrap( ~ Sector) +
  theme_minimal()

# On a Stock Level
# 1 Trend of Average Close Price Over the Years for each stocks
stock_market_data %>% group_by(Date,Sector,Symbol) %>%
  summarise(Average_Close = mean(Close)) %>%
  ggplot(aes(x=Date, y=Average_Close, color=Sector)) +
  geom_line() +
  scale_color_manual(values = 
                       c("Financials" = "#467ab0", 
                         "Technology" = "#009933")) +
  labs(title = "Individual Stock Performance: 
       A Year-Over-Year Close Price Analysis", 
       x="Date",y="Average Close Price") +
  facet_wrap(~ Symbol, scales = "free_y", ncol = 2 ) +
  theme_minimal()
# 2 Trend of Average Volume Over the Years 
stock_market_data %>% group_by(Date, Sector, Symbol) %>%
  summarise(Average_Volume = mean(Volume)) %>%
  ggplot(aes(x=Date, y=Average_Volume, color=Sector)) +
  geom_line() +
  geom_smooth( size=0.9, color="#696969") +
  scale_color_manual(values = 
                       c("Financials" = "#467ab0", 
                         "Technology" = "#009933")) +
  labs(title = "Trend in Volume Over the years for each stock",
       x="Date",y="Average Volume") +
  facet_wrap( ~ Symbol, scales = "free_y", ncol = 2) +
  theme_minimal()
# 3 Trend of Average Volatility Over the Years 
stock_market_data %>% group_by(Date, Sector, Symbol) %>%
  summarise(Average_Volatility = mean(Volatility)) %>%
  ggplot(aes(x=Date, y=Average_Volatility, color=Sector)) +
  geom_line() +
  geom_smooth( size=0.9, color="#696969") +
  scale_color_manual(values = 
                       c("Financials" = "#467ab0", 
                         "Technology" = "#009933")) +
  labs(title = "Trend in Volatility Over the years for each stock", 
       x="Date",y="Average Volatility") +
  facet_wrap( ~ Symbol, scales = "free_y", ncol = 2) +
  theme_minimal()
# 4 Trend of Average Daily Market Return Over the Years 
stock_market_data %>% group_by(Date, Sector, Symbol) %>%
  summarise(Average_Market_Return = mean(Market_Return)) %>%
  ggplot(aes(x=Date, y=Average_Market_Return, color=Sector)) +
  geom_line() +
  scale_color_manual(values = 
                       c("Financials" = "#467ab0", 
                         "Technology" = "#009933")) +
  labs(title = "Yearly Market Return Trends Across Stocks", 
       x="Date",y="Average Market Return") +
  facet_wrap( ~ Symbol,scales = "free_y", ncol = 2) +
  theme_minimal()


















