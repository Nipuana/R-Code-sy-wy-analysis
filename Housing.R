# -------------------------
# LOAD LIBRARIES
# -------------------------
library(tidyverse)
library(lubridate)
library(ggplot2)
library(scales)
library(viridis)
library(dplyr)


# -------------------------
# READ CSV FILES FOR 2021–2023
# -------------------------
housing_2021 <- read_csv("C:/Users/user/Desktop/Sems/4th sem/Data Science/Project folder/datasets/housing_2021.csv", col_names = FALSE)
housing_2022 <- read_csv("C:/Users/user/Desktop/Sems/4th sem/Data Science/Project folder/datasets/housing_2022.csv", col_names = FALSE)
housing_2023 <- read_csv("C:/Users/user/Desktop/Sems/4th sem/Data Science/Project folder/datasets/housing_2023.csv", col_names = FALSE)

# -------------------------
# ASSIGN PROPER COLUMN NAMES
# -------------------------
colnames(housing_2021)[1:16] <- c("Transaction_ID", "Price", "Date", "Postcode",
                                  "Property_Type", "Old_New", "Duration", "PAON", "SAON",
                                  "Street", "Locality", "Town", "District", "County",
                                  "PPD_Category_Type", "Record_Status")
colnames(housing_2022) <- colnames(housing_2021)
colnames(housing_2023) <- colnames(housing_2021)

# -------------------------
# ADD YEAR AND SELECT RELEVANT COLUMNS
# -------------------------
housing_2021 <- housing_2021 %>% mutate(Year = 2021) %>% select(Price, Date, Postcode, District, County, Year)
housing_2022 <- housing_2022 %>% mutate(Year = 2022) %>% select(Price, Date, Postcode, District, County, Year)
housing_2023 <- housing_2023 %>% mutate(Year = 2023) %>% select(Price, Date, Postcode, District, County, Year)

# -------------------------
# COMBINE ALL YEARS
# -------------------------
housing_all <- bind_rows(housing_2021, housing_2022, housing_2023)

# -------------------------
# CLEAN AND FILTER ONLY YORKSHIRE DISTRICTS
# -------------------------
valid_districts <- c(
  "Barnsley", "Bradford", "Calderdale", "Doncaster",
  "Kirklees", "Leeds", "Rotherham", "Sheffield", "Wakefield"
)

housing_clean <- housing_all %>%
  filter(!is.na(Price), !is.na(Postcode), !is.na(District)) %>%
  mutate(
    Date = as_date(Date),
    District = str_to_title(District)
  ) %>%
  filter(District %in% valid_districts)

# -------------------------
# LINE PLOT: PRICE TREND OVER YEARS PER DISTRICT
# -------------------------
trend_data <- housing_clean %>%
  group_by(Year, District) %>%
  summarise(Avg_Price = mean(Price, na.rm = TRUE), .groups = "drop")

ggplot(trend_data, aes(x = Year, y = Avg_Price, color = District)) +
  geom_line(size = 1) +
  labs(title = "Yearly Housing Price Trend by District (West & South Yorkshire)",
       x = "Year", y = "Average Price (£)") +
  theme_minimal()

# -------------------------
# BAR PLOT: AVERAGE HOUSE PRICE PER DISTRICT
# -------------------------
avg_price <- housing_clean %>%
  group_by(District) %>%
  summarise(Avg_Price = mean(Price, na.rm = TRUE)) %>%
  arrange(desc(Avg_Price))

ggplot(avg_price, aes(x = reorder(District, -Avg_Price), y = Avg_Price)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  labs(title = "Average House Price per District (2021–2023)",
       x = "District", y = "Average Price (£)") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# -------------------------
# BOXPLOT: PRICE DISTRIBUTION PER DISTRICT (SEPARATE FOR EACH COUNTY)
# -------------------------

# Step 1: Assign region to each district
housing_clean <- housing_clean %>%
  mutate(
    Region = case_when(
      District %in% c("Leeds", "Bradford", "Wakefield", "Kirklees", "Calderdale") ~ "West Yorkshire",
      District %in% c("Sheffield", "Rotherham", "Barnsley", "Doncaster") ~ "South Yorkshire",
      TRUE ~ NA_character_
    )
  )

# Step 2: Filter for only West & South Yorkshire
yorkshire_data <- housing_clean %>%
  filter(Region %in% c("West Yorkshire", "South Yorkshire"))

# Step 3: Remove extreme outliers using IQR
filtered_data <- yorkshire_data %>%
  group_by(District) %>%
  mutate(
    Q1 = quantile(Price, 0.25, na.rm = TRUE),
    Q3 = quantile(Price, 0.75, na.rm = TRUE),
    IQR = Q3 - Q1,
    Lower = Q1 - 1.5 * IQR,
    Upper = Q3 + 1.5 * IQR
  ) %>%
  filter(Price >= Lower, Price <= Upper) %>%
  ungroup()

# Step 4: Create two separate datasets
west_data <- filtered_data %>% filter(Region == "West Yorkshire")
south_data <- filtered_data %>% filter(Region == "South Yorkshire")

# Step 5: Plot for West Yorkshire
ggplot(west_data, aes(x = reorder(District, Price, FUN = median), y = Price, fill = District)) +
  geom_boxplot(outlier.colour = "red", outlier.shape = 1, outlier.size = 1.5, alpha = 0.7) +
  scale_fill_viridis_d(option = "C") +
  labs(
    title = "House Price Distribution in West Yorkshire (2021–2023)",
    subtitle = "Extreme outliers removed by IQR method",
    x = "District",
    y = "House Price (£)"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    legend.position = "none",
    plot.title = element_text(face = "bold", hjust = 0.5),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

# Step 6: Plot for South Yorkshire
ggplot(south_data, aes(x = reorder(District, Price, FUN = median), y = Price, fill = District)) +
  geom_boxplot(outlier.colour = "red", outlier.shape = 1, outlier.size = 1.5, alpha = 0.7) +
  scale_fill_viridis_d(option = "C") +
  labs(
    title = "House Price Distribution in South Yorkshire (2021–2023)",
    subtitle = "Extreme outliers removed by IQR method",
    x = "District",
    y = "House Price (£)"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    legend.position = "none",
    plot.title = element_text(face = "bold", hjust = 0.5),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

# -------------------------
# AFFORDABILITY SCORE PER DISTRICT
# -------------------------
housing_scores <- housing_clean %>%
  group_by(District) %>%
  summarise(Avg_Price = mean(Price, na.rm = TRUE)) %>%
  mutate(Housing_Score = 10 - scales::rescale(Avg_Price, to = c(0, 10)))

# Preview the scores
head(housing_scores)

# write_csv(housing_clean, "C:/Users/user/Desktop/Sems/4th sem/Data Science/Project folder/Cleaned_datasets/cleaned_housing_yorkshire_district.csv")
