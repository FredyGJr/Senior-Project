library(tidyverse)

crash <- read.csv("Crash_Data_2024.csv") #making my dataset

head(crash)
str(crash) #checking my data

crash <- crash %>%
  mutate(
    Total_Crashes = as.numeric(gsub(",", "", Total_Crashes)),
    Population = as.numeric(gsub(",", "", Population)),
    Fatal_Crashes = as.numeric(gsub(",", "", Fatal_Crashes)),
    Speed_Crashes = as.numeric(gsub(",", "", Speed_Crashes)),
    DUI_Crashes = as.numeric(gsub(",", "", DUI_Crashes)),
    Distracted_Crashes = as.numeric(gsub(",", "", Distracted_Crashes)),
    Serious_Crashes = as.numeric(gsub(",", "", Serious_Crashes)),
    Minor_Crashes = as.numeric(gsub(",","",Minor_Crashes))
  ) #Had to fix my mistake of having commas in my numbers from my csv

crash <- crash %>% mutate(
  Crash_Rate = (Total_Crashes / Population) * 100000,
  Fatal_Rate = (Fatal_Crashes / Population) * 100000,
  SpeedPercent = Speed_Crashes / Total_Crashes,
  DUIPercent = DUI_Crashes / Total_Crashes,
  DistractedPercent = Distracted_Crashes / Total_Crashes,
  SeverityPercent = (Fatal_Crashes + Serious_Crashes) / Total_Crashes
) #added some more Variables

summary(crash)

crash %>%
  arrange(desc(Crash_Rate)) # Ranking counties by crash rate

crash %>%
  arrange(desc(Fatal_Rate)) # Comparing Fatality rates

ggplot(crash, aes(x = reorder(County, Crash_Rate), y = Crash_Rate)) +
  geom_col() +
  coord_flip() +
  labs(title = "Crash Rate per 100,000 by County",
       x = "County",
       y = "Crash Rate") # Visualizing the Crash Rate

ggplot(crash, aes(x = SpeedPercent, y = Crash_Rate)) +
  geom_point(size = 3) +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "Speeding vs Crash Rate")

harris <- crash %>% filter(County == "Harris")

p_hat <- harris$Total_Crashes / harris$Population
n <- harris$Population

z <- 1.96
se <- sqrt((p_hat * (1 - p_hat)) / n)

lower_harris <- p_hat - z * se
upper_harris <- p_hat + z * se
 # Compaing my Confidence Intervals

montgomery <- crash %>% filter(County == "Montgomery")

p_hat <- montgomery$Total_Crashes / montgomery$Population
n <- montgomery$Population

z <- 1.96
se <- sqrt((p_hat * (1 - p_hat)) / n)

lower_montgomery <- p_hat - z * se
upper_montgomery <- p_hat + z * se


lower_harris * 100000
upper_harris * 100000

lower_montgomery * 100000
upper_montgomery * 100000

tibble(
  County = c("Harris", "Montgomery"),
  Lower_CI = c(lower_harris, lower_montgomery) * 100000,
  Upper_CI = c(upper_harris, upper_montgomery) * 100000
)

model <- lm(Crash_Rate ~ SpeedPercent + DUIPercent + DistractedPercent, data = crash)

summary(model)
