library(tidyverse)

# Load your data from a CSV file
data <- read_csv("natural-disasters.csv")

filterData <- data %>%
  filter(Entity %in% c("India", "United Kingdom", "United States", "Canada")) %>%
  select(Entity, Year, `Number of people affected by floods`, `Number of people left homeless from floods`)

# Filter data for India
india_data <- data %>% filter(Entity == "India")

# Select relevant columns and reshape the data
india_long <- india_data %>%
  select(Year, `Insured damages against drought`, `Insured damages against floods`,
         `Insured damages against earthquakes`) %>%
  pivot_longer(cols = starts_with("Insured damages"),
               names_to = "DisasterType",
               values_to = "InsuredDamages") %>%
  mutate(DisasterType = str_replace(DisasterType, "Insured damages against ", ""))

# Filter data for India starting from the year 1980 and remove NA values
india_long_filtered <- india_long %>%
  filter(Year >= 1980 & !is.na(InsuredDamages))

# Line plot to show trends over time
ggplot(india_long_filtered, aes(x = Year, y = InsuredDamages, color = DisasterType, group = DisasterType)) +
  geom_line() +
  geom_point() +
  labs(title = "Trends in Insured Damages in India for Various Disasters",
       x = "Year",
       y = "Insured Damages (in monetary units)",
       color = "Disaster Type") +
  theme_minimal() +
  scale_y_continuous(labels = scales::comma)



avgData <- filterData %>%
  group_by(Entity) %>%
  summarize(
    avgAffected = mean(`Number of people affected by floods`, na.rm = TRUE),
    avgHomeless = mean(`Number of people left homeless from floods`, na.rm = TRUE)
  ) %>%
  pivot_longer(cols = c(avgAffected, avgHomeless), names_to = "Legends", values_to = "Value")

ggplot(avgData, aes(x = Entity, y = Value, fill = Legends)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  scale_y_log10(labels = scales::comma_format()) +
  labs(x = "Country", 
       y = "Log(Average Number of People)", 
       title = "Comparison of Flood Impact: Affected vs Homeless") +
  theme_minimal()
