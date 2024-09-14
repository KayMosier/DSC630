library(readxl)
# open excel file and deterimine column type
Population <- read_excel("C:/Users/seled/OneDrive/Desktop/DSC630/Population.xlsx",
col_names = FALSE, col_types = c("text", "text", "numeric", "numeric","numeric","numeric"))

View(Population)

# clean the columns
df <- Population[-c(1:2), ]

colnames(df)[1:6] <- c('Rank', "Geographic_Area","April_2020", "July_2023", "Change_Number", "Change_Percent")

df <- df[-c(1:2), ]
df <- df[-c(1862:1868), ]
View(df)

# plot based on percentage change greater than 25%
library(ggplot2)
library(dplyr)

filtered_df <- df %>% filter(Change_Percent > 25)

ggplot(filtered_df, aes(x = reorder(Geographic_Area, -Change_Percent), y = Change_Percent)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  coord_flip() +
  labs(title = "Population Growth Greater than 25% (2020 to 2023)", 
       x = "Geographic Area", 
       y = "Percentage Growth") +
  theme_minimal()

#split Georgraphic_Area Column
library(tidyr)
df <- df %>%
  separate(Geographic_Area, into = c("City", "State"), sep = ", ", remove = FALSE)

# group by state
df %>%
  group_by(State) %>%
  summarize(Mean_Change = mean(Change_Percent, na.rm = TRUE)) %>%
  ggplot(aes(x = reorder(State, -Mean_Change), y = Mean_Change)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  coord_flip() +
  labs(title = "Average Population Change by State",
       x = "State",
       y = "Average Percent Change") +
  theme_minimal()

# 6 top states with highest increase of population change
states_of_interest <- c("South Carolina", "Idaho", "Montana", 
  "Arizona", "North Carolina", "Texas")

filtered_df <- df %>% filter(State %in% states_of_interest)

summarized_df <- filtered_df %>% group_by(State) %>% 
  summarize(Average_Change_Percent = mean(Change_Percent, na.rm = TRUE))

summarized_by_city_df <- filtered_df %>% group_by(City) %>% 
  summarize(Average_Change_Percent = mean(Change_Percent, na.rm = TRUE))

ggplot(summarized_df, aes(x = reorder(State, -Average_Change_Percent), 
  y = Average_Change_Percent)) + geom_bar(stat = "identity", fill = "steelblue") 
  + coord_flip() + labs(title = "Average Population Change Percent by State",
  x = "State", y = "Average Change Percent") + theme_minimal()

# Filter the data frame based on the states of interest// this code too much
ggplot(summarized_by_city_state_df, 
  aes(x = reorder(City, -Average_Change_Percent), 
  y = Average_Change_Percent, fill = State)) + geom_bar(stat = "identity") +
  facet_wrap(~ State, scales = "free_y") + coord_flip() +
  labs(title = "Population Change Percent by City within Each State",
  x = "City", y = "Average Change Percent") + theme_minimal()

# filtered by greater than 10 percent
filtered_growth_df <- summarized_by_city_state_df %>% 
  filter(Average_Change_Percent > 10)
ggplot(filtered_growth_df, aes(x = reorder(City, -Average_Change_Percent), 
  y = Average_Change_Percent, fill = State)) + geom_bar(stat = "identity") +
  facet_wrap(~ State, scales = "free_y") + coord_flip() +
  labs(title = "Population Change Percent by City within Each State", 
  x = "City", y = "Average Change Percent") + theme_minimal()

