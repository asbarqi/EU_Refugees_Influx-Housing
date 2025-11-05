library(tidyverse)
library(stringr)
library(readr)
library(kableExtra)
library(readxl)
library(kableExtra)
library(sf)
library(AER)
library(rnaturalearth)
library(rnaturalearthdata)
library(viridis)
library(readxl)
library(fixest)
library(modelsummary)
library(stargazer)
library(lmtest)
library(multiwayvcov)
library(sandwich)
library(patchwork)
library(ggrepel)
library(knitr)
library(viridis)
library(ggplot2)
library(gganimate)
library(ggthemes)
library(ggridges)
library(plotly)
library(scales)
library(gridExtra)
library(tidyverse)
library(stringr)
library(readr)
library(kableExtra)
library(readxl)
library(kableExtra)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(viridis)
library(readxl)
library(fixest)
library(modelsummary)
library(stargazer)
library(lmtest)
library(multiwayvcov)
library(sandwich)
library(patchwork)
library(ggrepel)
library(knitr)
library(viridis)
library(ggplot2)
library(gganimate)
library(ggthemes)
library(ggridges)
library(plotly)


setwd("Raw data/")
#Loading .xlsx data downloaded from Eurostat
hpi_raw <- read_excel("HPI_EU.xlsx", sheet = "Sheet 1", skip = 8)

View(hpi_raw)

#Renaming Columns
names(hpi_raw)[1] <- "Country"

names(hpi_raw)

hpi_clean <- hpi_raw %>%
  filter(!is.na(Country)) %>%
  filter(!str_detect(Country, "GEO|Dataset|Total|Unit|TIME|Label"))


#Converting to a long form panel data for ease of analysis
hpi_long <- hpi_clean %>% 
  pivot_longer(
    cols = -Country,
    names_to = "Year",
    values_to = "HPI"
  ) %>%
  # Extract the numeric year from the column names and convert HPI to numeric
  mutate(
    Year = as.numeric(str_extract(Year, "\\d{4}")),
    HPI = as.numeric(HPI)
  ) %>%
  filter(!is.na(Year))  # Remove rows where Year could not be extracted

#Further cleaning by removing unwanted entries such as
#spurious rows

invalid_entries <- c("b", "d", "e", "p",
                     "Observation flags:", ":",
                     "Special value")

HPI_Panel_Clean_Final <- hpi_long %>% 
  filter(!Country %in% invalid_entries)



#!!!!Descriptive Stats and Descriptive Graphs's Code


#HPI_Panel_Clean_Final %>%
#  select(-post_2015, -post_2019) %>%
#  summary()

# Compute summary statistics for HPI for each country
hpi_summary <- HPI_Panel_Clean_Final %>%
  group_by(Country) %>%
  summarise(
    Mean = mean(HPI, na.rm = TRUE),
    SD = sd(HPI, na.rm = TRUE),
    Median = median(HPI, na.rm = TRUE),
    Minimum = min(HPI, na.rm = TRUE),
    Maximum = max(HPI, na.rm = TRUE))

print(hpi_summary)



#HPI Summary Table
hpi_summary %>%
  kable(caption = "Summary Statistics by Country for HPI") %>%
  kable_styling(
    bootstrap_options = c("striped", "hover", "condensed"), 
    full_width = FALSE
  )

HPI_Panel_Clean_Final <- HPI_Panel_Clean_Final %>%
  filter(Country != "Türkiye" & Country !="European Union - 28 countries (2013-2020)")

#Graph 1 - HPI Baseline by Country (2005)

hpi_2005 <- HPI_Panel_Clean_Final %>% 
  filter(Year == 2005)

p2005 <- ggplot(HPI_Panel_Clean_Final, aes(x = reorder(Country, HPI), y = HPI, fill = HPI)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  scale_fill_viridis_c(option = "plasma", direction = 1) +
  labs(
    title = "HPI by Country in 2005",
    x = "Country",
    y = "HPI"
  ) +
  theme_minimal()

print(p2005)



#Graph 2 - HPI by Country (2023)

hpi_2023 <- HPI_Panel_Clean_Final %>% 
  filter(Year == 2023)

p2023 <- ggplot(hpi_2023, aes(x = reorder(Country, HPI), y = HPI, fill = HPI)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  scale_fill_viridis_c(option = "plasma", direction = 1) +
  labs(
    title = "HPI by Country in 2023",
    x = "Country",
    y = "HPI"
  ) +
  theme_minimal()

print(p2023)



#Graph 3 - Showing Trend by Country
p2 <- ggplot(HPI_Panel_Clean_Final, aes(x = Year, y = HPI, group = Country, color = Country)) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  labs(title = "HPI Trend by Country (2005–2023)",
       x = "Year",
       y = "HPI (2015 = 100)") +
  theme_minimal() +
  theme(legend.position = "bottom")

print(p2)


#Graph 4 - Distribution of HPI Values by Country
p3 <- ggplot(HPI_Panel_Clean_Final, aes(x = reorder(Country, HPI, FUN = median), y = HPI, fill = Country)) +
  geom_boxplot() +
  coord_flip() +
  labs(title = "Distribution of HPI Values by Country (2005–2023)",
       x = "Country",
       y = "HPI (2015 = 100)") +
  theme_minimal() +
  theme(legend.position = "none")

print(p3)



# Graph 5: Scatter Plot with a Smoothing Line (Trend Across All Countries)
ggplot(HPI_Panel_Clean_Final, aes(x = Year, y = HPI)) +
  geom_point(aes(color = Country), size = 2) +
  geom_smooth(method = "loess", se = FALSE, color = "darkgreen") +
  labs(
    title = "Scatter Plot of HPI Over Time with Trend Smoothing",
    x = "Year",
    y = "HPI (2015 = 100)"
  ) +
  theme_minimal() +
  theme(legend.position = "none")


HPI_Panel_Clean_Final <- HPI_Panel_Clean_Final %>%
  mutate(HPIgrowth = (HPI - lag(HPI)) / lag(HPI) * 100) 

# Graph 6: Scatter Plot of HPI growth
ggplot(HPI_Panel_Clean_Final%>% filter(Year != 2005), aes(x = Year, y = HPIgrowth)) +
  geom_point(aes(color = Country), size = 2) +
  geom_smooth(method = "loess", se = FALSE, color = "darkgreen") +
  labs(
    title = "Scatter Plot of HPI Growth",
    x = "Year",
    y = "HPI growth(per cent)"
  ) +
  theme_minimal() +
  theme(legend.position = "none")


# Graph 7: 

hpi_data_2 <- HPI_Panel_Clean_Final %>%
  mutate(
    Year = as.numeric(Year),
    HPI = as.numeric(HPI)
  )

# Computing the increase from 2015 to 2023
hpi_increase_data2 <- hpi_data_2 %>%
  filter(Year %in% c(2015, 2023)) %>%
  pivot_wider(names_from = Year, values_from = HPI, names_prefix = "Year_") %>%
  mutate(increase = Year_2023 - Year_2015)


# Calculate the increase: HPI in 2023 minus HPI in 2015


hpi_increase_data2 <- hpi_increase_data2 %>%
  mutate(
    quartile = ntile(increase, 4),
    quartile_label = case_when(
      quartile == 1 ~ "Q1 (Lowest Increase)",
      quartile == 2 ~ "Q2",
      quartile == 3 ~ "Q3",
      quartile == 4 ~ "Q4 (Highest Increase)"
    )
  )

# Quartile increase table from 2015 to 2023

hpi_increase_data2 %>%
  drop_na() %>%  # removes all rows containing any NA values
  kable(caption = "HPI Increase Table") %>%
  kable_styling(
    bootstrap_options = c("striped", "hover", "condensed"),
    full_width = FALSE
  )

#Loading World Map
world <- ne_countries(scale = "medium", returnclass = "sf")

#Filtering Map to Show EU only
europe <- world %>% filter(continent == "Europe")

europe_data <- europe %>%
  left_join(hpi_increase_data2, by = c("name" = "Country"))


#Map's code:
p_map <- ggplot(europe_data) +
  geom_sf(aes(fill = quartile_label)) +
  scale_fill_viridis_d(option = "plasma", direction = 1, na.value = "grey80") +
  labs(
    title = "European Hot Spots based on HPI Increases (2015-2023)",
    fill = "HPI Increase Quartiles"
  ) +
  theme_minimal()

print(p_map)


########## Refugees Data
## Cleaning




#Refugees

refugee_raw <- read_excel("refugee_data.xlsx", sheet = "refugee_data", skip = 14)

str(refugee_raw, give.attr = FALSE) 

colnames(refugee_raw) <- gsub("'", "", colnames(refugee_raw)) 

refugee_raw <- refugee_raw %>% 
  mutate(Country_origin = as.factor(`Country of origin`), 
         Country_asylum = as.factor(`Country of asylum`),
         Year = as.integer(Year))

str(refugee_raw, give.attr = FALSE) 

#Renaming Columns
refugee_raw <- refugee_raw %>%
  rename(
    Refugees="Refugees under UNHCRs mandate",
    Asylum='Asylum-seekers'
  )

refugee_clean <-refugee_raw %>%
  select(Country_asylum,Year,Refugees,Asylum)

summary(refugee_raw$`Country of asylum`)

refugee_clean <- refugee_clean %>%
  group_by(Country_asylum,Year) %>%
  summarise(TotalRefugees = sum(Refugees, na.rm = TRUE),
            Totalasylum = sum(Asylum, na.rm = TRUE))



#### Refugees analysis


refugee_summary_year <- refugee_clean %>%
  group_by(Year) %>%
  summarise(across(c(TotalRefugees, Totalasylum), list(mean = ~mean(. , na.rm = TRUE),
                                                       sd = ~sd(. , na.rm = TRUE),
                                                       min = ~min(. , na.rm = TRUE),
                                                       max = ~max(. , na.rm = TRUE),
                                                       median = ~median(. , na.rm = TRUE)),
                   .names = "{.col}_{.fn}"))

# View the summary statistics
print(refugee_summary_year)


refugee_summary_year%>%
  kable(caption = "Summary Statistics by Year for Refugees") %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed"), full_width = FALSE)


refugee_clean <-refugee_clean %>%
  mutate(Totaldisplaced = TotalRefugees + Totalasylum) 
  

#Graph 8- Showing Trends by Country
refugee_plot1 <- ggplot(refugee_clean, aes(x = Year, y = Totaldisplaced, group = Country_asylum, color = Country_asylum)) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  labs(title = "Displaced Trends by Country (2007–2023)",
       x = "Year",
       y = "Number") +
  theme_minimal() +
  theme(legend.position = "bottom")

print(refugee_plot1)



#Graph9

refugee_distribution <- ggplot(refugee_clean, aes(x = reorder(Country_asylum, Totaldisplaced), y = Totaldisplaced, fill = Country_asylum)) +
  geom_boxplot() +
  coord_flip() +
  labs(title = "Distribution of Displaced by Country (2007–2023)",
       x = "Country",
       y = "iosplaced") +
  theme_minimal() +
  theme(legend.position = "none")

print(refugee_distribution)


#Graph10

ggplot(refugee_clean, aes(x = Year, y = Totaldisplaced)) +
  geom_point(aes(color = Country_asylum), size = 2) +
  geom_smooth(method = "loess", se = FALSE, color = "darkgreen") +
  labs(
    title = "Scatter Plot of Displaced Over Time with Trend Smoothing",
    x = "Year",
    y = "Total Displaced"
  ) +
  theme_minimal() +
  theme(legend.position = "none")


refugee_clean <-refugee_clean %>%
  mutate(Displacedgrowth = (Totaldisplaced - lag(Totaldisplaced)) / lag(Totaldisplaced) * 100)


# Graph 11: Scatter Plot of Displaced growth
ggplot(refugee_clean%>% filter(Year != 2007), aes(x = Year, y = Displacedgrowth)) +
  geom_point(aes(color = Country_asylum), size = 2) +
  geom_smooth(method = "loess", se = FALSE, color = "darkgreen") +
  labs(
    title = "Scatter Plot of Displaced Growth",
    x = "Year",
    y = "Displaced growth(per cent)"
  ) +
  theme_minimal() +
  theme(legend.position = "none")






######## GDP

##### Cleaning
WIR_data <- read_excel("GDP.xls", sheet = "Data")
GDP_EU <- WIR_data %>%
  filter(`Country Name` %in% HPI_Panel_Clean_Final$Country |
           `Country Name` == "United Kingdom" |
           `Country Name` == "Turkiye" ) %>%
  select(`Country Name`, `2005`:`2023`) %>%
  rename(Country = `Country Name`) %>% 
  mutate(Country = as.factor(Country) ) 
 
GDP_EU <- GDP_EU %>%
  pivot_longer(cols = -Country,  # All columns except 'Country' should be transformed
               names_to = "Year", # Create a 'Year' column
               values_to = "GDP") %>%  
  mutate(Year = as.numeric(Year) )



### GDP Growth



WIR_data <- read_excel("GDPG.xls", sheet = "Data")
GDPG_EU <- WIR_data %>% 
  filter(`Country Name` %in% HPI_Panel_Clean_Final$Country |
           `Country Name` == "United Kingdom" |
           `Country Name` == "Turkiye" ) %>%
  select(`Country Name`, `2005`:`2023`) %>%
  rename(Country = `Country Name`) %>% 
  mutate(Country = as.factor(Country) ) 
GDPG_EU  <- GDPG_EU %>%
  pivot_longer(cols = -Country,  # All columns except 'Country' should be transformed
               names_to = "Year", # Create a 'Year' column
               values_to = "GDP_Growth_Rate")  %>%  
  mutate(Year = as.numeric(Year) )



### Inflation Rate

WIR_data <- read_excel("API_FP.CPI.TOTL.ZG_DS2_en_excel_v2_26405.xls", sheet = "Data")
EU_IR <- WIR_data %>% 
  filter(`Country Name` %in% HPI_Panel_Clean_Final$Country |
           `Country Name` == "United Kingdom" |
           `Country Name` == "Turkiye" ) %>%
  select(`Country Name`, `2005`:`2023`) %>%
  rename(Country = `Country Name`) %>% 
  mutate(Country = as.factor(Country) ) 
EU_IR <- EU_IR %>%
  pivot_longer(cols = -Country,  # All columns except 'Country' should be transformed
               names_to = "Year", # Create a 'Year' column
               values_to = "Inflation_Rate") %>%  
  mutate(Year = as.numeric(Year) )


##### creating panel dataset

hpi_panel <- HPI_Panel_Clean_Final
EU_IR <- EU_IR
refugee_panel <- refugee_clean

#Clean Refugee Panel 
hpi_panel$Country[hpi_panel$Country == "Türkiye"] <- "Turkiye"
refugee_panel <- refugee_panel %>%
  mutate(Country_asylum = case_when(
    Country_asylum == "Netherlands (Kingdom of the)" ~ "Netherlands",
    Country_asylum == "T√ºrkiye" ~ "Turkiye",
    TRUE ~ Country_asylum # Keep other values as they are
  ))

refugee_panel<- refugee_panel %>% 
  rename("Country" =`Country_asylum`) %>%
  mutate(
    Country = as.factor(Country))


### Joining data

Joint_df <- full_join(hpi_panel, EU_IR, by = c("Country" = "Country", "Year" = "Year")) %>% 
  filter(Country != "European Union - 28 countries (2013-2020)" & Country != "Switzerland" ) %>% 
  filter(Country != "United Kingdom") %>% filter(Year >= 2010 & Year <= 2023) 
Joint_df <- left_join(Joint_df, GDP_EU, by = c("Country" = "Country", "Year" = "Year"))
Joint_df <- left_join(Joint_df, GDPG_EU, by = c("Country" = "Country", "Year" = "Year"))
Joint_df <- left_join(Joint_df, refugee_panel, by = c("Country" = "Country", "Year" = "Year"))
Joint_df <- Joint_df %>% 
  rename("Total_Refugees" =`TotalRefugees`) %>% 
  rename("Total_Asylum" =`Totalasylum`)

df_fin <- Joint_df

##### Adding population dataset

WIR_data <- read_excel("population.xls", sheet = "Data")
EU_Pop <- WIR_data %>% filter(`Country Name` %in% df_fin$Country) %>% 
  select(`Country Name`, `2005`:`2023`) %>% 
  rename("Country" =`Country Name`) %>%
  mutate(
    Country = as.factor(Country)
  )
EU_Pop <- EU_Pop %>%
  pivot_longer(cols = -Country,  # All columns except 'Country' should be transformed
               names_to = "Year", # Create a 'Year' column
               values_to = "Population") %>%  
  mutate(Year = as.numeric(Year) )


df_final_pop <- left_join(df_fin, EU_Pop, by = c("Country" = "Country", "Year" = "Year"))

df_final_pop <- df_final_pop %>% 
  mutate(HPI_Growth = (HPI - lag(HPI)) / lag(HPI) *100
         ,Rugees_Growth = (Total_Refugees - lag(Total_Refugees)) / lag(Total_Refugees) *100,
         Refugees_per_1000 = (Total_Refugees / Population ) * 1000 )




### Housing Supply Data 

housing_df <- read_excel("housing_supply.xlsx")

# Reshape housing data from wide to long format
housing_long <- housing_df %>%
  pivot_longer(cols = -Country, names_to = "Year", values_to = "Housing_Supply") %>%
  mutate(Year = as.numeric(Year))

# Join the reshaped housing data with the main dataframe
merged_df <- df_final_pop %>%
  left_join(housing_long, by = c("Country", "Year"))



##### Writing df to CSV

write_csv(merged_df, "Final_df_pop.csv")

library(writexl)
write_xlsx(merged_df,"Final_df_pop.xlsx")



## More analysis

EU_IR_sum <- EU_IR %>%
  group_by(Country) %>% 
  summarise(
    Mean = mean(Inflation_Rate, na.rm = TRUE),
    SD = sd(Inflation_Rate, na.rm = TRUE),
    Median = median(Inflation_Rate, na.rm = TRUE),
    Minimum = min(Inflation_Rate, na.rm = TRUE),
    Maximum = max(Inflation_Rate, na.rm = TRUE))

#Summary Table
EU_IR_sum %>%
  kable(caption = "Summary Statistics by Country for Inflation Rate") %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed"), full_width = FALSE)



# Filter the data for the three specific years (2005, 2015, and 2023)
data_filtered <- EU_IR %>% filter(Year %in% c("2005", "2015", "2023"))

# Create separate plots for each year with horizontal bars
plot_2005 <- ggplot(data_filtered %>% filter(Year == "2005"), aes(x = Inflation_Rate, y = Country, fill = Country)) +
  geom_bar(stat = "identity") +
  labs(title = "Inflation Rates in 2005", x = "Inflation Rate (%)", y = "Country") +
  theme_minimal() +
  theme(legend.position = "none") +  # Remove legend to avoid clutter
  coord_flip() +  # Flip axes to make the bars horizontal
  theme(axis.text.y = element_text(size = 10))  # Adjust y-axis text size for better readability

plot_2015 <- ggplot(data_filtered %>% filter(Year == "2015"), aes(x = Inflation_Rate, y = Country, fill = Country)) +
  geom_bar(stat = "identity") +
  labs(title = "Inflation Rates in 2015", x = "Inflation Rate (%)", y = "Country") +
  theme_minimal() +
  theme(legend.position = "none") + 
  coord_flip() +
  theme(axis.text.y = element_text(size = 10))

plot_2023 <- ggplot(data_filtered %>% filter(Year == "2023"), aes(x = Inflation_Rate, y = Country, fill = Country)) +
  geom_bar(stat = "identity") +
  labs(title = "Inflation Rates in 2023", x = "Inflation Rate (%)", y = "Country") +
  theme_minimal() +
  theme(legend.position = "none") + 
  coord_flip() +
  theme(axis.text.y = element_text(size = 10))

# Combine the plots into a single view

grid.arrange(plot_2005, plot_2015, plot_2023, ncol = 3)


#### Final report graphs

df_fin <- merged_df
## Excluding turkey
df_fin <- df_fin %>% 
  filter(Country != "Turkiye")

df_fin <- df_fin %>%
  mutate(Total_Refugees2= Total_Refugees+Total_Asylum)

df_fin <- df_fin %>%
  mutate(Refugees_per_1000= Total_Refugees2*1000/Population)


## refugees overtime
refugees_over_time <- df_fin %>%
  group_by(Year) %>%
  summarise(Total_Refugees_Aggregated = sum(Total_Refugees2, na.rm = TRUE))

plot_refugees_time <- ggplot(refugees_over_time, aes(x = Year, y = Total_Refugees_Aggregated)) +
  geom_line() +
  geom_point() +
  labs(
    title = "Total Refugee Numbers Over Time (Aggregated Across Countries)",
    x = "Year",
    y = "Total Number of Refugees"
  ) +
  theme_minimal()

print(plot_refugees_time)


# Calculate summary statistics per year
year_summary_stats <- df_fin %>%
  group_by(Year) %>%
  summarise(
    Count = n(),
    Mean = mean(Refugees_per_1000, na.rm = TRUE),
    SD = sd(Refugees_per_1000, na.rm = TRUE),
    Min = min(Refugees_per_1000, na.rm = TRUE),
    Q1 = quantile(Refugees_per_1000, 0.25, na.rm = TRUE),
    Median = median(Refugees_per_1000, na.rm = TRUE),
    Q3 = quantile(Refugees_per_1000, 0.75, na.rm = TRUE),
    Max = max(Refugees_per_1000, na.rm = TRUE)
  )

# Print summary statistics per year
kable(year_summary_stats, digits = 2, caption = "Summary Statistics for Refugees per 1000 Residents by Year")




# Data prep
latest_year <- max(df_fin$Year)
refugees_latest_year <- df_fin %>%
  filter(Year == latest_year) %>%
  arrange(desc(Refugees_per_1000))

refugees_2014 <- df_fin %>%
  filter(Year == "2014") %>%
  arrange(desc(Refugees_per_1000))

refugees_2016 <- df_fin %>%
  filter(Year == "2016") %>%
  arrange(desc(Refugees_per_1000))

top_n <- 10

refugees_latest_year_top <- refugees_latest_year %>%
  slice_max(order_by = Refugees_per_1000, n = top_n)

refugees_2016_top <- refugees_2016 %>%
  slice_max(order_by = Refugees_per_1000, n = top_n)

refugees_2014_top <- refugees_2014 %>%
  slice_max(order_by = Refugees_per_1000, n = top_n)

max_refugees <- max(refugees_latest_year$Refugees_per_1000)

plot_refugees_latest_year <- ggplot(refugees_latest_year_top, aes(x = reorder(Country, Refugees_per_1000), y = Refugees_per_1000)) +
  geom_col() +
  coord_flip() +
  labs(
    title = paste("Refugee Numbers by Country in", latest_year),
    x = "Country",
    y = "Total Number of Refugees"
  ) +
  scale_y_continuous(limits = c(0, max_refugees), labels = scales::comma) +
  theme_minimal() +
  theme(axis.text.y = element_text(size = 6))

plot_refugees_2016 <- ggplot(refugees_2016_top, aes(x = reorder(Country, Refugees_per_1000), y = Refugees_per_1000)) +
  geom_col() +
  coord_flip() +
  labs(
    title = "Refugee Numbers by Country in 2016",
    x = "Country",
    y = "Total Number of Refugees"
  ) +
  scale_y_continuous(limits = c(0, max_refugees), labels = scales::comma) +
  theme_minimal() +
  theme(axis.text.y = element_text(size = 6))

plot_refugees_2014 <- ggplot(refugees_2014_top, aes(x = reorder(Country, Refugees_per_1000), y = Refugees_per_1000)) +
  geom_col() +
  coord_flip() +
  labs(
    title = "Refugee Numbers by Country in 2014",
    x = "Country",
    y = "Total Number of Refugees"
  ) +
  scale_y_continuous(limits = c(0, max_refugees), labels = scales::comma) +
  theme_minimal() +
  theme(axis.text.y = element_text(size = 6))

# Combine plots
combined_plot <- plot_refugees_latest_year / plot_refugees_2016 / plot_refugees_2014
print(combined_plot)



data_filtered <- df_fin %>%
  filter(Year >= 2014 & Year <= 2023)

# ========== 3. Facet Line Plot ==========
# Get top 20 countries by refugee intake (2014–2023)
top_20_countries <- data_filtered %>%
  filter(Year == 2023) %>%
  group_by(Country) %>%
  summarise(total_refugees = sum(Total_Refugees, na.rm = TRUE)) %>%
  arrange(desc(total_refugees)) %>%
  slice(1:20) %>%
  pull(Country)

# Filter for top countries
top_data <- data_filtered %>%
  filter(Country %in% top_20_countries)

# Faceted Line Plot with 2015 vertical reference line
facet_plot <-ggplot(top_data, aes(x = Year, y = Refugees_per_1000, color = Country, group = Country)) +
  geom_line(size = 1.2) +
  facet_wrap(~Country, scales = "free_y") +
  scale_color_viridis_d(option = "plasma") +
  scale_y_continuous(limits = c(0, 45)) +
  theme_minimal(base_size = 13) +
  labs(
    title = "Refugee per 1000 residents trends in Top Countries (2014–2023)",
    x = "Year",
    y = "Refugees per 1000 residents"
  ) +
  theme(
    plot.title = element_text(hjust = 0.5),
    axis.text.x = element_text(angle = 90, vjust = 0.5)
  )

print(facet_plot)




df_avg <- df_fin %>%
  group_by(Country) %>%
  summarise(
    avg_refugees_per_1000 = mean(Refugees_per_1000, na.rm = TRUE)
  )

medi <- median(df_avg$avg_refugees_per_1000, na.rm = TRUE)
Q3 <- quantile(df_avg$avg_refugees_per_1000, 0.75, na.rm = TRUE)

# Step 3: Classify countries based on Q1 and Q3
df_avg <- df_avg %>%
  mutate(refugee_influx_group = case_when(
    avg_refugees_per_1000 > Q3 ~ "High",
    avg_refugees_per_1000 < medi ~ "Low",
    TRUE ~ "Medium"
  ))

# Step 4: Sort and display as a table
df_avg_sorted <- df_avg %>%
  arrange(desc(avg_refugees_per_1000))


color_vector <- case_when(
  df_avg_sorted$refugee_influx_group == "High" ~ "red",
  df_avg_sorted$refugee_influx_group == "Medium" ~ "orange",
  df_avg_sorted$refugee_influx_group == "Low" ~ "gray"
)

# Step 4: Display table with colored influx group column
df_avg_sorted %>%
  kable(
    digits = 2,
    caption = "Average Refugees per 1000 Residents (2010–2023) and Influx Classification"
  ) %>%
  kable_styling(full_width = FALSE) %>%
  column_spec(3, color = "white", background = color_vector) %>%
  row_spec(0, bold = TRUE)

