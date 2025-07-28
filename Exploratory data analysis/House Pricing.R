library(tidyverse)

HousePrices = read_csv("C:/Users/sthas/OneDrive/Documents/Stage 2 - Sem 2/ST5014CEM Data Science for Developers/Assignment/Coursework(DataScience_SantoshShrestha_230181)/Cleaned Data/Cleaned_House_Prices.csv")


#Extracting Year from Date------------
library(lubridate)

HousePrices = HousePrices %>%
  mutate(Year = year(ymd(Date)))

unique(HousePrices$Year)
#-------------------------------------





# average by County & Year
county_year_avg = HousePrices %>%
  group_by(County, Year) %>%
  summarise(AveragePrice = mean(Price, na.rm = TRUE), .groups = "drop")

#Line graph: Average price 2021‑2024, both counties in same panel
ggplot(county_year_avg, aes(x = Year, y = AveragePrice, colour = County)) +
  geom_line(size = 1.2) +
  geom_point(size = 2) +
  scale_y_continuous(labels = scales::comma) +
  labs(title = "Average House Prices by County (2021–2024)",
       x = "Year", y = "Average Price") +
  theme_minimal()






# average by County in 2023 only
county_2023_avg = county_year_avg %>%
  filter(Year == 2023)

#Bar chart: Average price in 2023, both counties

ggplot(county_2023_avg, aes(x = County, y = AveragePrice, fill = County)) +
  geom_col(width = 0.3) +
  scale_y_continuous(labels = scales::comma) +
  labs(title = "Average House Prices by County (2023)",
       x = NULL, y = "Average Price") +
  theme_minimal() +
  theme(legend.position = "none")






#Box‑plots: 2021‑2024 distribution for each county in separate panels

ggplot(county_year_avg, aes(x = "", y = AveragePrice)) +
  geom_boxplot(fill = "skyblue", outlier.alpha = 0.2) +
  facet_wrap(~ County, ncol = 1) +
  scale_y_continuous(labels = scales::comma) +
  labs(title = "Distribution of House Prices (2021–2024)",
       y = "Price", x = "") +
  theme_minimal()
