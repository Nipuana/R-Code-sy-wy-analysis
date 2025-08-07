# loading libraries
library(tidyverse)
library(stringr)


# Reading performance and coverage datasets

broadband_perf <- read_csv("C:/Users/user/Desktop/Sems/4th sem/Data Science/Project folder/datasets/broadband_performance.csv")
broadband_cov <- read_csv("C:/Users/user/Desktop/Sems/4th sem/Data Science/Project folder/datasets/broadband_coverage.csv")
postcode_lsoa <- read_csv("C:/Users/user/Desktop/Sems/4th sem/Data Science/Project folder/datasets/Postcode to LSOA.csv", 
                          col_names = TRUE,
                          locale = locale(encoding = "ISO-8859-1"))


# Standardize postcode format to match others (insert space before last 3 characters)
broadband_perf <- broadband_perf %>%
  rename(postcode = `postcode`, median_speed = `Median download speed (Mbit/s)`) %>%
  filter(!is.na(median_speed)) %>%
  mutate(postcode = str_replace_all(str_to_upper(postcode), "(\\w{3})$", " \\1"))

broadband_cov <- broadband_cov %>%
  rename(postcode = `postcode`,
         SFBB = `SFBB availability (% premises)`,
         UFBB = `UFBB availability (% premises)`,
         FTTP = `FTTP availability (% premises)`) %>%
  mutate(postcode = str_replace_all(str_to_upper(postcode), "(\\w{3})$", " \\1"))

postcode_lsoa <- postcode_lsoa %>%
  rename(postcode = pcds, 
         lsoa = lsoa21cd, 
         district = ladnm) %>%
  select(postcode, lsoa, district)


# Merge performance and coverage datasets
broadband_combined <- inner_join(broadband_perf, broadband_cov, by = "postcode")


# Merge with postcode-lsoa-district mapping
broadband_full <- inner_join(broadband_combined, postcode_lsoa, by = "postcode")


# Define target districts
sy_wy_districts <- c("Barnsley", "Doncaster", "Rotherham", "Sheffield",   # South Yorkshire
                     "Bradford", "Calderdale", "Kirklees", "Leeds", "Wakefield")  # West Yorkshire

# Filter postcode list to SY and WY
postcode_filtered <- postcode_lsoa %>%
  filter(district %in% sy_wy_districts)

# Merge broadband performance and coverage first
broadband_combined <- inner_join(broadband_perf, broadband_cov, by = "postcode")

#  Left join to keep all postcodes from SY/WY, even if missing broadband data
broadband_full <- postcode_filtered %>%
  left_join(broadband_combined, by = "postcode")



# adding visualizations



#BoXPLOT
#  Adding county column 
broadband_full <- broadband_full %>%
  mutate(county = case_when(
    district %in% c("Barnsley", "Doncaster", "Rotherham", "Sheffield") ~ "South Yorkshire",
    district %in% c("Bradford", "Calderdale", "Kirklees", "Leeds", "Wakefield") ~ "West Yorkshire",
    TRUE ~ "Other"
  ))

# Step 2: Boxplot – District vs Speed (Faceted by County)
ggplot(broadband_full, aes(x = district, y = median_speed, fill = district)) +
  geom_boxplot() +
  facet_wrap(~ county, scales = "free_x") +
  labs(
    title = "Boxplot of Download Speeds by District (Separate by County)",
    x = "District",
    y = "Download Speed (Mbps)"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "none")



#BARCHART



#  Calculating town-level averages
broadband_town_speed <- broadband_full %>%
  group_by(county, district) %>%
  summarise(avg_speed = mean(median_speed, na.rm = TRUE), .groups = "drop")

#  Separating bar charts – one for each county


#Bargraph for South Yorkshire
# South Yorkshire
ggplot(filter(broadband_town_speed, county == "South Yorkshire"),
       aes(x = reorder(district, -avg_speed), y = avg_speed, fill = district)) +
  geom_bar(stat = "identity") +
  labs(
    title = "Average Download Speed by District – South Yorkshire",
    x = "District",
    y = "Avg Speed (Mbps)"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "none")



#Bar Graph of west yorksire
# West Yorkshire
ggplot(filter(broadband_town_speed, county == "West Yorkshire"),
       aes(x = reorder(district, -avg_speed), y = avg_speed, fill = district)) +
  geom_bar(stat = "identity") +
  labs(
    title = "Average Download Speed by District – West Yorkshire",
    x = "District",
    y = "Avg Speed (Mbps)"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "none")




#download cleaned dataset
#write_csv(broadband_full, "C:/Users/user/Desktop/Sems/4th sem/Data Science/Project folder/Cleaned_datasets/broadband_full.csv")





