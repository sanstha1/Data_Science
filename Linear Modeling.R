library(tidyverse)
library(scales)


#-----------------------------------House Price vs Download Speed for both Counties in single diagram (include linear model summary report and correlation)---------------------------------------------------------------

HousePrices = read_csv("C:/Users/sthas/OneDrive/Documents/Stage 2 - Sem 2/ST5014CEM Data Science for Developers/Assignment/Coursework(DataScience_SantoshShrestha_230181)/Cleaned Data/Cleaned_House_Prices.csv") %>%
  select(shortPostcode, Price)

BroadBandSpeed = read_csv("C:/Users/sthas/OneDrive/Documents/Stage 2 - Sem 2/ST5014CEM Data Science for Developers/Assignment/Coursework(DataScience_SantoshShrestha_230181)/Cleaned Data/Cleaned_BroadBand_Speed.csv") %>%
  select(shortPostcode, Median_Download)

Town = read_csv("C:/Users/sthas/OneDrive/Documents/Stage 2 - Sem 2/ST5014CEM Data Science for Developers/Assignment/Coursework(DataScience_SantoshShrestha_230181)/Cleaned Data/Towns.csv") %>%
  select(shortPostcode, Town, County)

CombinedData = HousePrices %>%
  inner_join(BroadBandSpeed, by = "shortPostcode") %>%
  inner_join(Town, by = "shortPostcode") %>%
  filter(!is.na(Price) & !is.na(Median_Download) & !is.na(Town))


set.seed(123)
SampleData = CombinedData %>%
  sample_n(100)


SampleModel = lm(Price ~ Median_Download, data = SampleData)


SampleData = SampleData %>%
  mutate(
    Predicted = predict(SampleModel),
    Residual = Price - Predicted
  )

ggplot(SampleData, aes(x = Median_Download, y = Price)) +
  geom_point(aes(color = Town), size = 2.5, alpha = 0.8) +  # Actual points by town
  geom_smooth(method = "lm", se = FALSE, color = "black", size = 1.2) +  # Black regression line
  geom_segment(aes(xend = Median_Download, yend = Predicted), color = "black", alpha = 0.6) +  # Residuals
  labs(
    title = "House Price vs Download Speed",
    x = "Median Download Speed (Mbps)",
    y = "House Price (£)",
    colour = "Town"
  ) +
  scale_y_continuous(labels = scales::comma) +
  theme_minimal()


FullModel = lm(Price ~ Median_Download, data = CombinedData)


correlation = cor(CombinedData$Price, CombinedData$Median_Download, use = "complete.obs")
cat("Correlation between Price and Download Speed:", correlation, "\n")

summary(FullModel)


#--------------------------------House price vs Drug rates (2023) per 10000 people for both counties in single diagram (include linear model summary report and correlation)-----------------------------------------


library(tidyverse)
library(lubridate)
library(scales)
library(stringr)


HousePrices = read_csv("C:/Users/sthas/OneDrive/Documents/Stage 2 - Sem 2/ST5014CEM Data Science for Developers/Assignment/Coursework(DataScience_SantoshShrestha_230181)/Cleaned Data/Cleaned_House_Prices.csv") %>%
  mutate(
    Year = year(ymd(Date)),
    County = str_trim(str_to_upper(County)),
    shortPostcode = str_trim(str_to_upper(shortPostcode))
  ) %>%
  filter(Year == 2023) %>%
  select(shortPostcode, Price, County)


Crime = read_csv("C:/Users/sthas/OneDrive/Documents/Stage 2 - Sem 2/ST5014CEM Data Science for Developers/Assignment/Coursework(DataScience_SantoshShrestha_230181)/Cleaned Data/Cleaned_Crime_Dataset.csv") %>%
  mutate(
    Year = as.integer(substr(Month, 1, 4)),
    County = str_replace(County, " Police$", ""),
    County = str_trim(str_to_upper(County))
  ) %>%
  filter(Year == 2023, CrimeType == "Drugs") %>%
  group_by(County) %>%
  summarise(DrugCrimes = n(), .groups = "drop")


Population = tibble(
  County = c("SOUTH YORKSHIRE", "WEST YORKSHIRE"),
  Population = c(1417000, 2342000)
)


CrimeRate = inner_join(Crime, Population, by = "County") %>%
  mutate(DrugRatePer10k = DrugCrimes / Population * 10000)


Town = read_csv("C:/Users/sthas/OneDrive/Documents/Stage 2 - Sem 2/ST5014CEM Data Science for Developers/Assignment/Coursework(DataScience_SantoshShrestha_230181)/Cleaned Data/Towns.csv") %>%
  mutate(
    shortPostcode = str_trim(str_to_upper(shortPostcode)),
    County = str_trim(str_to_upper(County))
  ) %>%
  select(shortPostcode, Town, County)


HousePrices_Town = inner_join(HousePrices, Town, by = c("shortPostcode", "County"))


CombinedData = inner_join(HousePrices_Town, CrimeRate, by = "County") %>%
  filter(!is.na(Price), !is.na(DrugRatePer10k), !is.na(Town))


CombinedData = CombinedData %>%
  mutate(DrugRatePer10k_jitter = DrugRatePer10k + runif(n(), -0.05, 0.05))


FullModel = lm(Price ~ DrugRatePer10k, data = CombinedData)


CombinedData = CombinedData %>%
  mutate(
    Predicted = predict(FullModel),
    Residual = Price - Predicted
  )


ggplot(CombinedData, aes(x = DrugRatePer10k_jitter, y = Price)) +
  geom_point(aes(color = Town), size = 2.5, alpha = 0.8) +                             
  geom_smooth(aes(x = DrugRatePer10k), method = "lm", se = FALSE, color = "black", size = 1.2) + 
  geom_segment(aes(xend = DrugRatePer10k, yend = Predicted), color = "black", alpha = 0.6) +     
  labs(
    title = "House Price vs Drug Crime Rate per 10,000 (2023)",
    x = "Drug Crime Rate per 10,000",
    y = "House Price (£)",
    colour = "Town"
  ) +
  scale_y_continuous(labels = comma) +
  theme_minimal() +
  theme(legend.position = "right")


cat("\n--- Linear Model Summary Report ---\n")
print(summary(FullModel))


correlation = cor(CombinedData$Price, CombinedData$DrugRatePer10k, use = "complete.obs")
cat("\n--- Correlation Analysis ---\n")
cat("Correlation between House Price and Drug Crime Rate per 10,000:", correlation, "\n")














#--------------------------------------Attainment 8 score vs House Price for both counties in single diagram (include linear model summary report and correlation)-----------------------------------------------------
library(tidyverse)
library(lubridate)
library(scales)


HousePrices = read_csv("C:/Users/sthas/OneDrive/Documents/Stage 2 - Sem 2/ST5014CEM Data Science for Developers/Assignment/Coursework(DataScience_SantoshShrestha_230181)/Cleaned Data/Cleaned_House_Prices.csv") %>%
  mutate(
    Year = year(ymd(Date)),
    County = str_to_title(County)
  ) %>%
  filter(Year == 2023) %>%
  select(Price, County)


School_2021_2022 = read_csv("C:/Users/sthas/OneDrive/Documents/Stage 2 - Sem 2/ST5014CEM Data Science for Developers/Assignment/Coursework(DataScience_SantoshShrestha_230181)/Cleaned Data/Cleaned_School_2021-2022.csv") %>%
  mutate(Year = 2022L)

School_2022_2023 = read_csv("C:/Users/sthas/OneDrive/Documents/Stage 2 - Sem 2/ST5014CEM Data Science for Developers/Assignment/Coursework(DataScience_SantoshShrestha_230181)/Cleaned Data/Cleaned_School_2022-2023.csv") %>%
  mutate(Year = 2023L)

School_2023_2024 = read_csv("C:/Users/sthas/OneDrive/Documents/Stage 2 - Sem 2/ST5014CEM Data Science for Developers/Assignment/Coursework(DataScience_SantoshShrestha_230181)/Cleaned Data/Cleaned_School_2023-2024.csv") %>%
  mutate(Year = 2024L)


AllSchools = bind_rows(School_2021_2022, School_2022_2023, School_2023_2024) %>%
  mutate(
    County = case_when(
      toupper(TOWN) %in% c("SOUTH YORKSHIRE", "WEST YORKSHIRE") ~ str_to_title(TOWN),
      toupper(ADDRESS3) %in% c("SOUTH YORKSHIRE", "WEST YORKSHIRE") ~ str_to_title(ADDRESS3),
      TRUE ~ NA_character_
    ),
    EBACCAPS = as.numeric(EBACCAPS)
  ) %>%
  filter(!is.na(County), !is.na(EBACCAPS), Year == 2023) %>%
  select(County, EBACCAPS)


CombinedData = inner_join(HousePrices, AllSchools, by = "County") %>%
  rename(Attainment8 = EBACCAPS)


ggplot(CombinedData) +
  geom_point(aes(x = Attainment8, y = Price, colour = County), alpha = 0.5, size = 1.5) +
  geom_smooth(aes(x = Attainment8, y = Price, colour = County), method = "lm", se = TRUE) +
  labs(
    title = "Attainment 8 Score vs House Price",
    x = "Attainment 8 Score",
    y = "House Price (£)",
    colour = "County"
  ) +
  scale_y_continuous(labels = comma) +
  theme_minimal()

cor_value = cor(CombinedData$Attainment8, CombinedData$Price, use = "complete.obs")
cat("\n--- Correlation between House Price and Attainment 8 Score ---\n")
cat("Correlation between House Price and Attainment 8 Score:", sprintf("%.4f", cor_value), "\n")


model = lm(Price ~ Attainment8 * County, data = CombinedData)
cat("\n--- Linear Model Summary Report ---\n")
summary(model)





library(tidyverse)
library(lubridate)
library(scales)
library(stringr)

HousePrices = read_csv("C:/Users/sthas/OneDrive/Documents/Stage 2 - Sem 2/ST5014CEM Data Science for Developers/Assignment/Coursework(DataScience_SantoshShrestha_230181)/Cleaned Data/Cleaned_House_Prices.csv") %>%
  mutate(
    Year = year(ymd(Date)),
    County = str_trim(str_to_upper(County)),
    shortPostcode = str_trim(str_to_upper(shortPostcode))
  ) %>%
  filter(Year == 2023) %>%
  select(shortPostcode, Price, County)

Town = read_csv("C:/Users/sthas/OneDrive/Documents/Stage 2 - Sem 2/ST5014CEM Data Science for Developers/Assignment/Coursework(DataScience_SantoshShrestha_230181)/Cleaned Data/Towns.csv") %>%
  mutate(
    shortPostcode = str_trim(str_to_upper(shortPostcode)),
    County = str_trim(str_to_upper(County)),
    Town = str_to_title(Town)
  ) %>%
  select(shortPostcode, Town, County)

HousePrices_Town = inner_join(HousePrices, Town, by = c("shortPostcode", "County"))

School_2021_2022 = read_csv("C:/Users/sthas/OneDrive/Documents/Stage 2 - Sem 2/ST5014CEM Data Science for Developers/Assignment/Coursework(DataScience_SantoshShrestha_230181)/Cleaned Data/Cleaned_School_2021-2022.csv") %>%
  mutate(Year = 2022L)

School_2022_2023 = read_csv("C:/Users/sthas/OneDrive/Documents/Stage 2 - Sem 2/ST5014CEM Data Science for Developers/Assignment/Coursework(DataScience_SantoshShrestha_230181)/Cleaned Data/Cleaned_School_2022-2023.csv") %>%
  mutate(Year = 2023L)

School_2023_2024 = read_csv("C:/Users/sthas/OneDrive/Documents/Stage 2 - Sem 2/ST5014CEM Data Science for Developers/Assignment/Coursework(DataScience_SantoshShrestha_230181)/Cleaned Data/Cleaned_School_2023-2024.csv") %>%
  mutate(Year = 2024L)

AllSchools = bind_rows(School_2021_2022, School_2022_2023, School_2023_2024) %>%
  mutate(
    County = case_when(
      toupper(TOWN) %in% c("SOUTH YORKSHIRE", "WEST YORKSHIRE") ~ str_to_upper(TOWN),
      toupper(ADDRESS3) %in% c("SOUTH YORKSHIRE", "WEST YORKSHIRE") ~ str_to_upper(ADDRESS3),
      TRUE ~ NA_character_
    ),
    EBACCAPS = as.numeric(EBACCAPS)
  ) %>%
  filter(!is.na(County), !is.na(EBACCAPS)) %>%
  select(County, Year, EBACCAPS)

CombinedData = HousePrices_Town %>%
  inner_join(AllSchools, by = "County") %>%
  rename(Attainment8 = EBACCAPS) %>%
  mutate(
    Attainment8_jitter = Attainment8 + runif(n(), -0.1, 0.1)
  )

FullModel = lm(Price ~ Attainment8, data = CombinedData)

CombinedData = CombinedData %>%
  mutate(
    Predicted = predict(FullModel),
    Residual = Price - Predicted
  )

ggplot(CombinedData, aes(x = Attainment8_jitter, y = Price, color = Town)) +
  geom_point(alpha = 0.8, size = 2.5) +
  geom_smooth(aes(x = Attainment8), method = "lm", se = FALSE, color = "black", linewidth = 1.2) +
  geom_segment(aes(xend = Attainment8, yend = Predicted), color = "black", alpha = 0.5) +
  labs(
    title = "House Price vs Attainment 8 Score",
    x = "Attainment 8 Score",
    y = "House Price (£, 2023)",
    colour = "Town"
  ) +
  scale_y_continuous(labels = comma) +
  theme_minimal() +
  theme(legend.position = "right")

cat("\n--- Linear Model Summary Report ---\n")
print(summary(FullModel))

correlation = cor(CombinedData$Price, CombinedData$Attainment8, use = "complete.obs")
cat("\n--- Correlation Analysis ---\n")
cat("Correlation between House Price and Attainment 8 Score:", correlation, "\n")













#--------------------------------Attainment 8 scores vs Drug Offense rates per 10000 people---------------------------------------------------------------------------------------------------------

library(tidyverse)
library(lubridate)
library(scales)

School_2022_2023 = read_csv("C:/Users/sthas/OneDrive/Documents/Stage 2 - Sem 2/ST5014CEM Data Science for Developers/Assignment/Coursework(DataScience_SantoshShrestha_230181)/Cleaned Data/Cleaned_School_2022-2023.csv") %>%
  mutate(
    County = case_when(
      toupper(TOWN) %in% c("SOUTH YORKSHIRE", "WEST YORKSHIRE") ~ str_to_title(TOWN),
      toupper(ADDRESS3) %in% c("SOUTH YORKSHIRE", "WEST YORKSHIRE") ~ str_to_title(ADDRESS3),
      TRUE ~ NA_character_
    ),
    EBACCAPS = as.numeric(EBACCAPS)
  ) %>%
  filter(!is.na(County), !is.na(EBACCAPS)) %>%
  group_by(County) %>%
  summarise(Attainment8 = mean(EBACCAPS, na.rm = TRUE))

Crime = read_csv("C:/Users/sthas/OneDrive/Documents/Stage 2 - Sem 2/ST5014CEM Data Science for Developers/Assignment/Coursework(DataScience_SantoshShrestha_230181)/Cleaned Data/Cleaned_Crime_Dataset.csv") %>%
  mutate(
    Year = as.integer(substr(Month, 1, 4)),
    County = str_replace(County, " Police$", ""),
    County = str_to_title(County)
  ) %>%
  filter(Year == 2023, CrimeType == "Drugs") %>%
  group_by(County) %>%
  summarise(DrugCrimes = n())

Population = tibble(
  County = c("South Yorkshire", "West Yorkshire"),
  Population = c(1417000, 2342000)
)

DrugRates = inner_join(Crime, Population, by = "County") %>%
  mutate(DrugRatePer10k = DrugCrimes / Population * 10000)

Combined = inner_join(School_2022_2023, DrugRates, by = "County")

ggplot(Combined, aes(x = DrugRatePer10k, y = Attainment8, colour = County)) +
  geom_point(size = 3, alpha = 0.6) +
  geom_smooth(method = "lm", se = TRUE) +
  labs(
    title = "Attainment 8 Score vs Drug Offense Rate per 10,000 People (2023)",
    x = "Drug Offense Rate per 10,000",
    y = "Attainment 8 Score",
    colour = "County"
  ) +
  theme_minimal()

cor_value = cor(Combined$DrugRatePer10k, Combined$Attainment8)
cat("\n--- Correlation between Drug Rate and Attainment 8 Score ---\n")
cat("Correlation coefficient:", round(cor_value, 4), "\n")

model = lm(Attainment8 ~ DrugRatePer10k * County, data = Combined)
cat("\n--- Linear Model Summary ---\n")
summary(model)







#-----------------------------------Average Download speed vs Drug Offense Rate per 10000 people for both counties in single diagram (include linear model summary report and correlation)------------------------------------------------

library(tidyverse)

# Load Broadband Data
HousePrices = read_csv("C:/Users/sthas/OneDrive/Documents/Stage 2 - Sem 2/ST5014CEM Data Science for Developers/Assignment/Coursework(DataScience_SantoshShrestha_230181)/Cleaned Data/Cleaned_House_Prices.csv") %>%
  select(shortPostcode, County,Date)


view(HousePrices)

BroadBandSpeed = read_csv("C:/Users/sthas/OneDrive/Documents/Stage 2 - Sem 2/ST5014CEM Data Science for Developers/Assignment/Coursework(DataScience_SantoshShrestha_230181)/Cleaned Data/Cleaned_BroadBand_Speed.csv") %>%
  select(shortPostcode, Median_Download)


CombinedData = inner_join(HousePrices, BroadBandSpeed, by = "shortPostcode") %>%
  filter(!is.na(Median_Download))

# Load Crime Data
Crime = read_csv("C:/Users/sthas/OneDrive/Documents/Stage 2 - Sem 2/ST5014CEM Data Science for Developers/Assignment/Coursework(DataScience_SantoshShrestha_230181)/Cleaned Data/Cleaned_Crime_Dataset.csv") %>%
  mutate(
    County = str_replace(County, " Police$", ""),
    County = str_to_title(County)
  ) %>%
  filter(CrimeType == "Drugs") %>%
  group_by(County, Year = as.integer(substr(Month, 1, 4))) %>%
  summarise(DrugCrimes = n(), .groups = "drop")

# Load Population Data (manually or from file)
Population = tibble(
  County = c("South Yorkshire", "West Yorkshire"),
  Population = c(1410000, 2320000)  # Replace with actual data if needed
)

# Join & Calculate Drug Offense Rate per 10k
CrimeWithPop = inner_join(Crime, Population, by = "County") %>%
  mutate(DrugRatePer10k = (DrugCrimes / Population) * 10000)

# Average Download Speed by County
BroadbandCounty = CombinedData %>%
  group_by(County) %>%
  summarise(Avg_Download = mean(Median_Download, na.rm = TRUE), .groups = "drop")

# Average Drug Rate per County
CrimeRateCounty = CrimeWithPop %>%
  group_by(County) %>%
  summarise(Avg_DrugRatePer10k = mean(DrugRatePer10k, na.rm = TRUE), .groups = "drop")

BroadbandCounty = BroadbandCounty %>%
  mutate(County = str_to_title(County))

CrimeRateCounty = CrimeRateCounty %>%
  mutate(County = str_to_title(County))

FinalData = inner_join(BroadbandCounty, CrimeRateCounty, by = "County")

print(FinalData)


ggplot(FinalData, aes(x = Avg_Download, y = Avg_DrugRatePer10k, label = County, color = County)) +
  geom_point(size = 3) +
  geom_smooth(method = "lm", se = TRUE) +
  labs(title = "Avg Download Speed vs Drug Offense Rate per 10,000 People",
       x = "Average Download Speed (Mbps)",
       y = "Average Drug Offense Rate (per 10,000 People)")

# Correlation
cor_value = cor(FinalData$Avg_Download, FinalData$Avg_DrugRatePer10k)
cat("\n--- Correlation ---\n")
cat("Correlation between Download Speed and Drug Offense Rate:", round(cor_value, 4), "\n")

# Linear Model Summary
model = lm(Avg_DrugRatePer10k ~ Avg_Download * County, data = FinalData)
cat("\n--- Linear Model Summary ---\n")
summary(model)








#----------------------------------------------------Average download speed vs Attainment 8 score for both counties in single diagram (include linear model summary report and correlation)---------------------------------------------------------------

library(tidyverse)


HousePrices = read_csv("C:/Users/sthas/OneDrive/Documents/Stage 2 - Sem 2/ST5014CEM Data Science for Developers/Assignment/Coursework(DataScience_SantoshShrestha_230181)/Cleaned Data/Cleaned_House_Prices.csv") %>%
  select(shortPostcode, County)

BroadBandSpeed = read_csv("C:/Users/sthas/OneDrive/Documents/Stage 2 - Sem 2/ST5014CEM Data Science for Developers/Assignment/Coursework(DataScience_SantoshShrestha_230181)/Cleaned Data/Cleaned_BroadBand_Speed.csv") %>%
  select(shortPostcode, Median_Download)

CombinedData = inner_join(HousePrices, BroadBandSpeed, by = "shortPostcode") %>%
  filter(!is.na(Median_Download)) %>%
  mutate(County = str_to_title(County))  


School_2021_2022 = read_csv("C:/Users/sthas/OneDrive/Documents/Stage 2 - Sem 2/ST5014CEM Data Science for Developers/Assignment/Coursework(DataScience_SantoshShrestha_230181)/Cleaned Data/Cleaned_School_2021-2022.csv") %>% mutate(Year = 2022L)
School_2022_2023 = read_csv("C:/Users/sthas/OneDrive/Documents/Stage 2 - Sem 2/ST5014CEM Data Science for Developers/Assignment/Coursework(DataScience_SantoshShrestha_230181)/Cleaned Data/Cleaned_School_2022-2023.csv") %>% mutate(Year = 2023L)
School_2023_2024 = read_csv("C:/Users/sthas/OneDrive/Documents/Stage 2 - Sem 2/ST5014CEM Data Science for Developers/Assignment/Coursework(DataScience_SantoshShrestha_230181)/Cleaned Data/Cleaned_School_2023-2024.csv") %>% mutate(Year = 2024L)

AllSchools = bind_rows(School_2021_2022, School_2022_2023, School_2023_2024) %>%
  mutate(
    County = case_when(
      str_to_upper(TOWN) %in% c("SOUTH YORKSHIRE", "WEST YORKSHIRE") ~ str_to_title(TOWN),
      str_to_upper(ADDRESS3) %in% c("SOUTH YORKSHIRE", "WEST YORKSHIRE") ~ str_to_title(ADDRESS3),
      TRUE ~ NA_character_
    ),
    Attainment8 = as.numeric(EBACCAPS)
  ) %>%
  filter(!is.na(County), !is.na(Attainment8)) %>%
  select(County, Attainment8)

# Summarize school attainment by county
SchoolCountySummary = AllSchools %>%
  group_by(County) %>%
  summarise(Avg_Attainment8 = mean(Attainment8, na.rm = TRUE), .groups = "drop")

# Summarize broadband download speed by county
BroadbandCounty = CombinedData %>%
  group_by(County) %>%
  summarise(Avg_Download = mean(Median_Download, na.rm = TRUE), .groups = "drop")

# Join broadband and school data by County
FinalData = inner_join(BroadbandCounty, SchoolCountySummary, by = "County")

print(FinalData)

# Plot
ggplot(FinalData, aes(x = Avg_Download, y = Avg_Attainment8, label = County, color = County)) +
  geom_point(size = 3) +
  geom_smooth(method = "lm", se = TRUE) +
  labs(
    title = "Average Download Speed vs Attainment 8 Score",
    x = "Average Download Speed (Mbps)",
    y = "Average Attainment 8 Score"
  )

# Correlation
cor_value = cor(FinalData$Avg_Download, FinalData$Avg_Attainment8)
cat("\n--- Correlation ---\n")
cat("Correlation between Download Speed and Attainment 8 Score:", round(cor_value, 4), "\n")

# Linear Model Summary
model = lm(Avg_Attainment8 ~ Avg_Download * County, data = FinalData)
cat("\n--- Linear Model Summary ---\n")
summary(model)


