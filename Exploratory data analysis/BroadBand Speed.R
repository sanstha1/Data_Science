library(tidyverse)

Broadband_speed = read_csv("C:/Users/sthas/OneDrive/Documents/Stage 2 - Sem 2/ST5014CEM Data Science for Developers/Assignment/Coursework(DataScience_SantoshShrestha_230181)/Cleaned Data/Cleaned_BroadBand_Speed.csv")
Towns = read_csv("C:/Users/sthas/OneDrive/Documents/Stage 2 - Sem 2/ST5014CEM Data Science for Developers/Assignment/Coursework(DataScience_SantoshShrestha_230181)/Cleaned Data/Towns.csv")





# Cleaning shortPostcode for reliable joining
Broadband_speed = Broadband_speed %>%
  mutate(shortPostcode = str_trim(toupper(shortPostcode)))

Towns = Towns %>%
  mutate(shortPostcode = str_trim(toupper(shortPostcode)))


# Merging datasets
BroadbandMerged = Broadband_speed %>%
  left_join(Towns, by = "shortPostcode")



#Boxplots for average download speed for both counties in separate chart (District vs Speed(Mbps))

# Boxplot for West Yorkshire Districts
BroadbandMerged %>%
  filter(str_detect(tolower(County), "west yorkshire"),
         !is.na(Avg_Download),
         !is.na(District)) %>%
  ggplot(aes(x = reorder(District, Avg_Download, FUN = median), y = Avg_Download)) +
  geom_boxplot(fill = "green") +
  labs(title = "West Yorkshire: Download Speed by District",
       x = "District", y = "Avg Download Speed (Mbps)") +
  coord_flip() +
  theme_minimal()

# Boxplot for South Yorkshire Districts
BroadbandMerged %>%
  filter(str_detect(tolower(County), "south yorkshire"),
         !is.na(Avg_Download),
         !is.na(District)) %>%
  ggplot(aes(x = reorder(District, Avg_Download, FUN = median), y = Avg_Download)) +
  geom_boxplot(fill = "orange") +
  labs(title = "South Yorkshire: Download Speed by District",
       x = "District", y = "Avg Download Speed (Mbps)") +
  coord_flip() +
  theme_minimal()







#Barchart for both counties (Two barcharts) download speeds (variable Town vs Speed)

# Bar Chart for West Yorkshire Towns
BroadbandMerged %>%
  filter(str_detect(tolower(County), "west yorkshire"),
         !is.na(Avg_Download),
         !is.na(TownCity)) %>%
  ggplot(aes(x = reorder(TownCity, Avg_Download), y = Avg_Download)) +
  geom_col(fill = "steelblue") +
  labs(title = "West Yorkshire: Avg Download Speed by Town",
       x = "Town", y = "Avg Download Speed (Mbps)") +
  scale_y_continuous(labels = scales::label_number()) +
  coord_flip() +
  theme_minimal()

# Bar chart for towns in South Yorkshire
BroadbandMerged %>%
  filter(str_detect(tolower(County), "south yorkshire"),
         !is.na(Avg_Download),
         !is.na(TownCity)) %>%
  ggplot(aes(x = reorder(TownCity, Avg_Download), y = Avg_Download)) +
  geom_col(fill = "darkorange") +
  labs(title = "South Yorkshire: Avg Download Speed by Town",
       x = "Town", y = "Avg Download Speed (Mbps)") +
  theme_minimal()
