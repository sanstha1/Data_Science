
#-----------------------------------House Price vs Download Speed for both Counties in single diagram (include linear model summary report and correlation)---------------------------------------------------------------
library(tidyverse)
library(scales)

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
library(ggrepel)

install.packages("ggrepel")


School_2022_2023 = read_csv("C:/Users/sthas/OneDrive/Documents/Stage 2 - Sem 2/ST5014CEM Data Science for Developers/Assignment/Coursework(DataScience_SantoshShrestha_230181)/Cleaned Data/Cleaned_School_2022-2023.csv") %>%
  mutate(
    County = case_when(
      str_detect(str_to_upper(TOWN), "SOUTH YORKSHIRE") ~ "South Yorkshire",
      str_detect(str_to_upper(TOWN), "WEST YORKSHIRE") ~ "West Yorkshire",
      str_detect(str_to_upper(ADDRESS3), "SOUTH YORKSHIRE") ~ "South Yorkshire",
      str_detect(str_to_upper(ADDRESS3), "WEST YORKSHIRE") ~ "West Yorkshire",
      TRUE ~ NA_character_
    ),
    Town = str_to_title(LANAME),
    EBACCAPS = as.numeric(EBACCAPS)
  ) %>%
  filter(!is.na(County), !is.na(EBACCAPS), !is.na(Town)) %>%
  group_by(County, Town) %>%
  summarise(Attainment8 = mean(EBACCAPS, na.rm = TRUE), .groups = "drop")

Crime = read_csv("C:/Users/sthas/OneDrive/Documents/Stage 2 - Sem 2/ST5014CEM Data Science for Developers/Assignment/Coursework(DataScience_SantoshShrestha_230181)/Cleaned Data/Cleaned_Crime_Dataset.csv") %>%
  mutate(
    Year = as.integer(substr(Month, 1, 4)),
    County = str_replace(County, " Police$", ""),
    County = str_to_title(County)
  ) %>%
  filter(Year == 2023, CrimeType == "Drugs") %>%
  group_by(County) %>%
  summarise(DrugCrimes = n(), .groups = "drop")

Town = read_csv("C:/Users/sthas/OneDrive/Documents/Stage 2 - Sem 2/ST5014CEM Data Science for Developers/Assignment/Coursework(DataScience_SantoshShrestha_230181)/Cleaned Data/Towns.csv")



Population = tibble(
  County = c("South Yorkshire", "West Yorkshire"),
  Population = c(1417000, 2342000)
)


DrugRates = inner_join(Crime, Population, by = "County") %>%
  mutate(DrugRatePer10k = DrugCrimes / Population * 10000)


Combined = inner_join(School_2022_2023, DrugRates, by = "County") %>%
  mutate(
    Attainment8_jitter = Attainment8 + runif(n(), -0.1, 0.1)
  )


model = lm(Attainment8 ~ DrugRatePer10k * County, data = Combined)


Combined = Combined %>%
  mutate(
    Predicted = predict(model),
    Residual = Attainment8 - Predicted
  )


ggplot(Combined, aes(x = DrugRatePer10k, y = Attainment8_jitter, colour = Town)) +
  geom_point(size = 3, alpha = 0.8) +
  geom_smooth(aes(y = Attainment8), method = "lm", se = TRUE, color = "black") +
  geom_segment(aes(xend = DrugRatePer10k, yend = Predicted), color = "gray40", alpha = 2.9) +
  labs(
    title = "Attainment 8 Score vs Drug Offense Rate per 10,000 People (2023)",
    x = "Drug Offense Rate per 10,000",
    y = "Attainment 8 Score",
    colour = "Town"
  ) +
  theme_minimal() 


cor_value = cor(Combined$DrugRatePer10k, Combined$Attainment8)
cat("\n--- Correlation between Drug Rate and Attainment 8 Score ---\n")
cat("Correlation coefficient:", round(cor_value, 4), "\n")

cat("\n--- Linear Model Summary ---\n")
summary(model)









#-----------------------------------Average Download speed vs Drug Offense Rate per 10000 people for both counties in single diagram (include linear model summary report and correlation)------------------------------------------------


library(tidyverse)
library(stringr)
library(ggrepel)


Crime = read_csv("C:/Users/sthas/OneDrive/Documents/Stage 2 - Sem 2/ST5014CEM Data Science for Developers/Assignment/Coursework(DataScience_SantoshShrestha_230181)/Cleaned Data/Cleaned_Crime_Dataset.csv") %>%
  mutate(
    County = str_replace(County, " Police$", ""),
    County = str_to_title(County),
    CrimeType = as.character(CrimeType),
    Town_Clean = str_trim(str_extract(LSOAname, "^[A-Za-z ]+"))
  ) %>%
  filter(CrimeType == "Drugs")

Town = read_csv("C:/Users/sthas/OneDrive/Documents/Stage 2 - Sem 2/ST5014CEM Data Science for Developers/Assignment/Coursework(DataScience_SantoshShrestha_230181)/Cleaned Data/Towns.csv") %>%
  mutate(
    Town_Clean = str_to_title(str_trim(Town)),
    County = str_to_title(County)
  )

CrimeTown = Crime %>%
  group_by(County, Town_Clean, Year = as.integer(substr(Month, 1, 4))) %>%
  summarise(DrugCrimes = n(), .groups = "drop")

TownPop = Town %>%
  select(County, Town_Clean, Population = Population2023)

CrimeWithPopTown = inner_join(CrimeTown, TownPop, by = c("County", "Town_Clean")) %>%
  mutate(DrugRatePer10k = (DrugCrimes / Population) * 10000) %>%
  group_by(County, Town_Clean) %>%
  summarise(Avg_DrugRatePer10k = mean(DrugRatePer10k, na.rm = TRUE), .groups = "drop")

BroadBandSpeed = read_csv("C:/Users/sthas/OneDrive/Documents/Stage 2 - Sem 2/ST5014CEM Data Science for Developers/Assignment/Coursework(DataScience_SantoshShrestha_230181)/Cleaned Data/Cleaned_BroadBand_Speed.csv") %>%
  select(shortPostcode, Median_Download)

TownBroadband = inner_join(Town, BroadBandSpeed, by = "shortPostcode")

AvgDownloadTown = TownBroadband %>%
  group_by(County, Town_Clean) %>%
  summarise(Avg_Download = mean(Median_Download, na.rm = TRUE), .groups = "drop")

FinalTownData = inner_join(AvgDownloadTown, CrimeWithPopTown, by = c("County", "Town_Clean")) %>%
  filter(!is.na(Avg_Download), !is.na(Avg_DrugRatePer10k))

model_town = lm(Avg_DrugRatePer10k ~ Avg_Download, data = FinalTownData)

FinalTownData = FinalTownData %>%
  mutate(
    fitted = predict(model_town),
    residual = Avg_DrugRatePer10k - fitted
  )

large_resid = FinalTownData %>% 
  filter(abs(residual) > 1) %>%
  mutate(label = paste0(Town_Clean, "\nResidual: ", round(residual, 2)))

ggplot(FinalTownData, aes(x = Avg_Download, y = Avg_DrugRatePer10k, color = County)) +
  geom_jitter(size = 3, width = 0.1, height = 0.1) +
  geom_smooth(method = "lm", se = TRUE, color = "black", size = 1) +
  geom_segment(aes(xend = Avg_Download, yend = fitted), linetype = "dashed",max.overlaps = Inf ,color = "black") +  
  geom_text_repel(data = large_resid, aes(label = label), size = 3, color = "black") +
  labs(title = "Avg Download Speed vs Drug Offense Rate per 10,000 People",
       x = "Average Download Speed (Mbps)",
       y = "Drug Offense Rate (per 10,000 People)",
       color = "County") +
  theme_minimal()


cor_value_town = cor(FinalTownData$Avg_Download, FinalTownData$Avg_DrugRatePer10k)
cat("\n--- Correlation ---\n")
cat("Correlation between Download Speed and Drug Offense Rate:", round(cor_value_town, 4), "\n\n")

cat("--- Linear Model Summary ---\n")
print(summary(model_town))









#----------------------------------------------------Average download speed vs Attainment 8 score for both counties in single diagram (include linear model summary report and correlation)---------------------------------------------------------------


library(tidyverse)
library(ggrepel)


Town = read_csv("C:/Users/sthas/OneDrive/Documents/Stage 2 - Sem 2/ST5014CEM Data Science for Developers/Assignment/Coursework(DataScience_SantoshShrestha_230181)/Cleaned Data/Towns.csv")

BroadBandSpeed = read_csv("C:/Users/sthas/OneDrive/Documents/Stage 2 - Sem 2/ST5014CEM Data Science for Developers/Assignment/Coursework(DataScience_SantoshShrestha_230181)/Cleaned Data/Cleaned_BroadBand_Speed.csv") %>%
  select(shortPostcode, Median_Download)

HousePrices = read_csv("C:/Users/sthas/OneDrive/Documents/Stage 2 - Sem 2/ST5014CEM Data Science for Developers/Assignment/Coursework(DataScience_SantoshShrestha_230181)/Cleaned Data/Cleaned_House_Prices.csv") %>%
  select(shortPostcode, County, District) %>%
  mutate(County = str_to_title(County))


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
  select(County, Town = TOWN, Attainment8)


SchoolTownSummary = AllSchools %>%
  group_by(County, Town) %>%
  summarise(Avg_Attainment8 = mean(Attainment8, na.rm = TRUE), .groups = "drop")


BroadbandTown = CombinedData %>%
  group_by(County) %>%
  summarise(Avg_Download = mean(Median_Download, na.rm = TRUE), .groups = "drop")


FinalTownData = inner_join(BroadbandTown, SchoolTownSummary, by = "County") %>%
  filter(!is.na(Avg_Download), !is.na(Avg_Attainment8))


model_town = lm(Avg_Attainment8 ~ Avg_Download * County, data = FinalTownData)


FinalTownData = FinalTownData %>%
  mutate(
    fitted = fitted(model_town),
    residual = residuals(model_town)
  )


large_resid = FinalTownData %>%
  filter(abs(residual) > 1) %>%
  mutate(label = paste0(Town, "\nResidual: ", round(residual, 2)))


ggplot(FinalTownData, aes(x = Avg_Download, y = Avg_Attainment8, color = County)) +
  geom_point(size = 3) +
  geom_smooth(method = "lm", se = TRUE, color = "black", size = 1) +  
  geom_segment(aes(xend = Avg_Download, yend = fitted), linetype = "dashed", color = "black") +
  geom_text_repel(data = large_resid, aes(label = label), size = 3, color = "black") +
  labs(
    title = "Average Download Speed vs Attainment 8 Score",
    x = "Average Download Speed (Mbps)",
    y = "Average Attainment 8 Score",
    color = "County"
  ) +
  theme_minimal()


cor_value_town = cor(FinalTownData$Avg_Download, FinalTownData$Avg_Attainment8)
cat("\n--- Correlation ---\n")
cat("Correlation between Download Speed and Attainment 8 Score:", round(cor_value_town, 4), "\n")

cat("\n--- Linear Model Summary ---\n")
summary(model_town)


