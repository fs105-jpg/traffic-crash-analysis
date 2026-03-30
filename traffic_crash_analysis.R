# Traffic Crash Data Analysis and Fault Prediction
# Felicia Selbst
#
# This project analyzes multi-source traffic crash data to examine
# relationships between driver characteristics, vehicle attributes,
# and fault outcomes. The workflow includes data cleaning, feature
# engineering, visualization, and decision tree modeling.

library(tidyverse)
library(dplyr)
library(partykit)

# -------------------- Read in Data -------------------- #

violation <- read.csv("data/violatn.csv")
accident  <- read.csv("data/accident.csv")
person    <- read.csv("data/person.csv")
vehicle   <- read.csv("data/vehicle.csv")
weather   <- read.csv("data/weather.csv")

# -------------------- Clean Violation Data -------------------- #

clean_violation <- violation %>%
  mutate(
    Fault = case_when(
      VIOLATION == 0 ~ "Not at Fault",
      VIOLATION != 0 ~ "At Fault"
    )
  ) %>%
  distinct(CASENUM, VEH_NO, .keep_all = TRUE) %>%
  select(
    CASENUM,
    Vehicle_ID = VEH_NO,
    Fault
  )

# -------------------- Clean Person Data -------------------- #

clean_person <- person %>%
  filter(SEAT_POSNAME == "Front Seat, Left Side") %>%
  filter(SEXNAME %in% c("Female", "Male")) %>%
  select(
    CASENUM,
    Vehicle_ID = VEH_NO,
    Age = AGE,
    Sex = SEXNAME
  )

# -------------------- Clean Vehicle Damage Data -------------------- #

clean_vehicle <- vehicle %>%
  mutate(
    Damage_Level = case_when(
      DEFORMED %in% c(7, 8, 9) ~ "Unknown",
      DEFORMED == 0 ~ "Undamaged",
      DEFORMED == 2 ~ "Minor",
      DEFORMED == 4 ~ "Moderate",
      DEFORMED == 6 ~ "Severe"
    )
  ) %>%
  select(
    CASENUM,
    Vehicle_ID = VEH_NO,
    Damage_Level
  )

clean_damage_numeric <- vehicle %>%
  mutate(
    Damage_Level_Num = case_when(
      DEFORMED %in% c(7, 8, 9) ~ NA_real_,
      DEFORMED == 0 ~ 1,
      DEFORMED == 2 ~ 2,
      DEFORMED == 4 ~ 3,
      DEFORMED == 6 ~ 4
    )
  ) %>%
  select(
    CASENUM,
    Vehicle_ID = VEH_NO,
    Damage_Level_Num
  )

# -------------------- Clean Accident Data -------------------- #

clean_accident <- accident %>%
  select(
    CASENUM,
    Total_Vehicles = VE_TOTAL,
    Primary_Harm_Event = HARM_EVNAME
  )

# -------------------- Merge Main Dataset -------------------- #

combined_df <- clean_person %>%
  left_join(clean_vehicle, by = c("CASENUM", "Vehicle_ID")) %>%
  left_join(clean_damage_numeric, by = c("CASENUM", "Vehicle_ID")) %>%
  left_join(clean_accident, by = "CASENUM") %>%
  left_join(clean_violation, by = c("CASENUM", "Vehicle_ID"))

combined_df$Damage_Level <- factor(
  combined_df$Damage_Level,
  levels = c("Unknown", "Undamaged", "Minor", "Moderate", "Severe")
)

# -------------------- Exploratory Plots -------------------- #

par(mar = c(5, 5, 6.5, 7.5))

damage_by_sex <- prop.table(table(combined_df$Sex, combined_df$Damage_Level), margin = 1)

barplot(
  t(damage_by_sex),
  legend = TRUE,
  args.legend = list(x = 3.2, y = 1),
  main = "Damage by Gender",
  col = c("grey", "rosybrown1", "salmon1", "tomato2", "red3"),
  ylab = "Proportion"
)

fault_by_sex <- prop.table(table(combined_df$Fault, combined_df$Sex), margin = 1)

barplot(
  t(fault_by_sex),
  legend = TRUE,
  args.legend = list(x = 3, y = 1),
  main = "Proportion of Drivers at Fault by Gender",
  ylab = "Proportion"
)

# -------------------- Build Modeling Dataset -------------------- #

accident_subset <- accident[, c("CASENUM", "REGIONNAME", "MONTH", "DAY_WEEK", "YEAR", "HOUR", "WEATHERNAME")]
person_subset   <- person[, c("CASENUM", "SEXNAME", "URBANICITYNAME", "AGE", "LOCATION")]
vehicle_subset  <- vehicle[, c("CASENUM", "MOD_YEAR", "MAKENAME", "VSPD_LIM", "VEH_ALCHNAME")]

accident_person_data <- merge(x = accident_subset, y = person_subset, by = "CASENUM")
accident_person_vehicle_data <- merge(x = accident_person_data, y = vehicle_subset, by = "CASENUM")
all_datasets <- merge(x = accident_person_vehicle_data, y = clean_violation, by = "CASENUM")

# -------------------- Feature Engineering -------------------- #

# Vehicle model country
all_datasets$MOD_COUNTRY[all_datasets$MAKENAME == "Acura"] <- "Japan"
all_datasets$MOD_COUNTRY[all_datasets$MAKENAME == "Alfa Romeo"] <- "Italy"
all_datasets$MOD_COUNTRY[all_datasets$MAKENAME == "AM General"] <- "United States"
all_datasets$MOD_COUNTRY[all_datasets$MAKENAME == "Audi"] <- "Germany"
all_datasets$MOD_COUNTRY[all_datasets$MAKENAME == "Bluebird"] <- "Japan"
all_datasets$MOD_COUNTRY[all_datasets$MAKENAME == "BMW"] <- "Germany"
all_datasets$MOD_COUNTRY[all_datasets$MAKENAME == "Buick / Opel"] <- "Germany"
all_datasets$MOD_COUNTRY[all_datasets$MAKENAME == "Cadillac"] <- "United States"
all_datasets$MOD_COUNTRY[all_datasets$MAKENAME == "Chevrolet"] <- "United States"
all_datasets$MOD_COUNTRY[all_datasets$MAKENAME == "Chrysler"] <- "United States"
all_datasets$MOD_COUNTRY[all_datasets$MAKENAME == "Dodge"] <- "United States"
all_datasets$MOD_COUNTRY[all_datasets$MAKENAME == "Ducati"] <- "Italy"
all_datasets$MOD_COUNTRY[all_datasets$MAKENAME == "Eagle"] <- "United States"
all_datasets$MOD_COUNTRY[all_datasets$MAKENAME == "Fiat"] <- "Italy"
all_datasets$MOD_COUNTRY[all_datasets$MAKENAME == "Ford"] <- "United States"
all_datasets$MOD_COUNTRY[all_datasets$MAKENAME == "Freightliner"] <- "United States"
all_datasets$MOD_COUNTRY[all_datasets$MAKENAME == "Gillig"] <- "United States"
all_datasets$MOD_COUNTRY[all_datasets$MAKENAME == "GMC"] <- "United States"
all_datasets$MOD_COUNTRY[all_datasets$MAKENAME == "Grumman"] <- "United States"
all_datasets$MOD_COUNTRY[all_datasets$MAKENAME == "Harley-Davidson"] <- "United States"
all_datasets$MOD_COUNTRY[all_datasets$MAKENAME == "Honda"] <- "Japan"
all_datasets$MOD_COUNTRY[all_datasets$MAKENAME == "Hyundai"] <- "South Korea"
all_datasets$MOD_COUNTRY[all_datasets$MAKENAME == "Infiniti"] <- "Japan"
all_datasets$MOD_COUNTRY[all_datasets$MAKENAME == "International Harvester/Navistar"] <- "United States"
all_datasets$MOD_COUNTRY[all_datasets$MAKENAME == "Isuzu"] <- "Japan"
all_datasets$MOD_COUNTRY[all_datasets$MAKENAME == "Jaguar"] <- "United Kingdom"
all_datasets$MOD_COUNTRY[all_datasets$MAKENAME == "Jeep / Kaiser-Jeep / Willys- Jeep"] <- "United States"
all_datasets$MOD_COUNTRY[all_datasets$MAKENAME == "Kawasaki"] <- "Japan"
all_datasets$MOD_COUNTRY[all_datasets$MAKENAME == "Kenworth"] <- "United States"
all_datasets$MOD_COUNTRY[all_datasets$MAKENAME == "KIA"] <- "South Korea"
all_datasets$MOD_COUNTRY[all_datasets$MAKENAME == "Land Rover"] <- "United Kingdom"
all_datasets$MOD_COUNTRY[all_datasets$MAKENAME == "Lexus"] <- "Japan"
all_datasets$MOD_COUNTRY[all_datasets$MAKENAME == "Lincoln"] <- "United States"
all_datasets$MOD_COUNTRY[all_datasets$MAKENAME == "Mack"] <- "United States"
all_datasets$MOD_COUNTRY[all_datasets$MAKENAME == "Mazda"] <- "Japan"
all_datasets$MOD_COUNTRY[all_datasets$MAKENAME == "MCI"] <- "United States"
all_datasets$MOD_COUNTRY[all_datasets$MAKENAME == "Mercedes-Benz"] <- "Germany"
all_datasets$MOD_COUNTRY[all_datasets$MAKENAME == "Mercury"] <- "United States"
all_datasets$MOD_COUNTRY[all_datasets$MAKENAME == "Mitsubishi"] <- "Japan"
all_datasets$MOD_COUNTRY[all_datasets$MAKENAME == "Nissan/Datsun"] <- "Japan"
all_datasets$MOD_COUNTRY[all_datasets$MAKENAME == "Not Reported"] <- "Other"
all_datasets$MOD_COUNTRY[all_datasets$MAKENAME == "Oldsmobile"] <- "United States"
all_datasets$MOD_COUNTRY[all_datasets$MAKENAME == "Other Domestic Manufacturers"] <- "United States"
all_datasets$MOD_COUNTRY[all_datasets$MAKENAME == "Other Import"] <- "Other"
all_datasets$MOD_COUNTRY[all_datasets$MAKENAME == "Other Make"] <- "Other"
all_datasets$MOD_COUNTRY[all_datasets$MAKENAME == "Peterbilt"] <- "United States"
all_datasets$MOD_COUNTRY[all_datasets$MAKENAME == "Plymouth"] <- "United States"
all_datasets$MOD_COUNTRY[all_datasets$MAKENAME == "Pontiac"] <- "United States"
all_datasets$MOD_COUNTRY[all_datasets$MAKENAME == "Porsche"] <- "Germany"
all_datasets$MOD_COUNTRY[all_datasets$MAKENAME == "Saab"] <- "Sweden"
all_datasets$MOD_COUNTRY[all_datasets$MAKENAME == "Saturn"] <- "United States"
all_datasets$MOD_COUNTRY[all_datasets$MAKENAME == "Scion"] <- "Japan"
all_datasets$MOD_COUNTRY[all_datasets$MAKENAME == "Smart"] <- "Germany"
all_datasets$MOD_COUNTRY[all_datasets$MAKENAME == "Subaru"] <- "Japan"
all_datasets$MOD_COUNTRY[all_datasets$MAKENAME == "Suzuki"] <- "Japan"
all_datasets$MOD_COUNTRY[all_datasets$MAKENAME == "Thomas Built"] <- "United States"
all_datasets$MOD_COUNTRY[all_datasets$MAKENAME == "Toyota"] <- "Japan"
all_datasets$MOD_COUNTRY[all_datasets$MAKENAME == "Triumph"] <- "United Kingdom"
all_datasets$MOD_COUNTRY[all_datasets$MAKENAME == "Unknown Make"] <- "Other"
all_datasets$MOD_COUNTRY[all_datasets$MAKENAME == "Victory"] <- "China"
all_datasets$MOD_COUNTRY[all_datasets$MAKENAME == "Volkswagen"] <- "Germany"
all_datasets$MOD_COUNTRY[all_datasets$MAKENAME == "Volvo"] <- "Sweden"
all_datasets$MOD_COUNTRY[all_datasets$MAKENAME == "White/Autocar White/GMC"] <- "United States"
all_datasets$MOD_COUNTRY[all_datasets$MAKENAME == "Yamaha"] <- "Japan"

all_datasets$MOD_COUNTRY[all_datasets$MOD_COUNTRY == "United States"] <- "US"
all_datasets$MOD_COUNTRY[all_datasets$MOD_COUNTRY == "United Kingdom"] <- "UK"
all_datasets$MOD_COUNTRY[all_datasets$MOD_COUNTRY == "South Korea"] <- "KOR"
all_datasets$MOD_COUNTRY[all_datasets$MOD_COUNTRY == "Germany"] <- "GER"
all_datasets$MOD_COUNTRY[all_datasets$MOD_COUNTRY == "Sweden"] <- "SWE"
all_datasets$MOD_COUNTRY[all_datasets$MOD_COUNTRY == "Japan"] <- "JPN"
all_datasets$MOD_COUNTRY[all_datasets$MOD_COUNTRY == "China"] <- "CHN"
all_datasets$MOD_COUNTRY[all_datasets$MOD_COUNTRY == "Italy"] <- "IT"
all_datasets$MOD_COUNTRY[all_datasets$MOD_COUNTRY == "Other"] <- "OtherCountry"

mod_country_factors <- as.factor(all_datasets$MOD_COUNTRY)

# Speed groups
all_datasets$SPEED_GROUP[all_datasets$VSPD_LIM %in% c(0, 5, 10, 15, 20, 25)] <- "<=25"
all_datasets$SPEED_GROUP[all_datasets$VSPD_LIM %in% c(30, 35, 40, 45, 50, 55)] <- "25<&<=55"
all_datasets$SPEED_GROUP[all_datasets$VSPD_LIM %in% c(60, 65, 70, 75, 80)] <- "55<&<=80"
all_datasets$SPEED_GROUP[all_datasets$VSPD_LIM %in% c(98, 99)] <- "SpeedNotReported"

speed_group_factors <- as.factor(all_datasets$SPEED_GROUP)

# Region labels
all_datasets$REGIONNAME[all_datasets$REGIONNAME == "Midwest (OH, IN, IL, MI, WI, MN, ND, SD, NE, IA, MO, KS)"] <- "Midwest"
all_datasets$REGIONNAME[all_datasets$REGIONNAME == "Northeast (PA, NJ, NY, NH, VT, RI, MA, ME, CT)"] <- "Northeast"
all_datasets$REGIONNAME[all_datasets$REGIONNAME == "South (MD, DE, DC, WV, VA, KY, TN, NC, SC, GA, FL, AL, MS, LA, AR, OK, TX)"] <- "South"
all_datasets$REGIONNAME[all_datasets$REGIONNAME == "West (MT, ID, WA, OR, CA, NV, NM, AZ, UT, CO, WY, AK, HI)"] <- "West"

# Other factor cleanup
all_datasets$SEXNAME[all_datasets$SEXNAME %in% c("Not Reported", "Reported as Unknown")] <- "SexNotReported"
all_datasets$WEATHERNAME[all_datasets$WEATHERNAME == "Not Reported"] <- "WeatherNotReported"
all_datasets$WEATHERNAME[all_datasets$WEATHERNAME == "Other"] <- "WeatherNotReported"
all_datasets$WEATHERNAME[all_datasets$WEATHERNAME == "Reported as Unknown"] <- "WeatherNotReported"
all_datasets$WEATHERNAME[all_datasets$WEATHERNAME == "Blowing Sand, Soil, Dirt"] <- "BlowingSand/Soil/Dirt"
all_datasets$WEATHERNAME[all_datasets$WEATHERNAME == "Fog, Smog, Smoke"] <- "Fog/Smog/Smoke"
all_datasets$WEATHERNAME[all_datasets$WEATHERNAME == "Freezing Rain or Drizzle"] <- "FreezingRain/Drizzle"
all_datasets$WEATHERNAME[all_datasets$WEATHERNAME == "Sleet or Hail"] <- "Sleet/Hail"

all_datasets$VEH_ALCHNAME[all_datasets$VEH_ALCHNAME == "No Driver Present"] <- "NoDriver"
all_datasets$VEH_ALCHNAME[all_datasets$VEH_ALCHNAME == "No Alcohol Involved"] <- "NoAlc."
all_datasets$VEH_ALCHNAME[all_datasets$VEH_ALCHNAME == "Alcohol Involved"] <- "Alc."
all_datasets$VEH_ALCHNAME[all_datasets$VEH_ALCHNAME == "Reported as Unknown"] <- "Alc.NotReported"

gender_factors <- as.factor(all_datasets$SEXNAME)
alc_factors <- as.factor(all_datasets$VEH_ALCHNAME)
fault_factors <- as.factor(all_datasets$Fault)

# -------------------- Country-Based Summaries -------------------- #

all_datasets_fault <- all_datasets %>%
  mutate(is_fault = ifelse(Fault == "At Fault", 1, 0)) %>%
  select(MOD_COUNTRY, is_fault, Fault)

country_num <- all_datasets_fault %>%
  group_by(MOD_COUNTRY) %>%
  summarise(Fault = sum(is_fault) / 1000, .groups = "drop")

all_datasets_not_fault <- all_datasets %>%
  mutate(is_not_fault = ifelse(Fault == "Not at Fault", 1, 0)) %>%
  select(MOD_COUNTRY, is_not_fault, Fault)

country_not_num <- all_datasets_not_fault %>%
  group_by(MOD_COUNTRY) %>%
  summarise(NotAtFault = sum(is_not_fault) / 1000, .groups = "drop")

total_country <- merge(x = country_num, y = country_not_num, by = "MOD_COUNTRY")
country_matrix <- as.matrix(total_country[, c("Fault", "NotAtFault")])

par(las = 1, mar = c(4, 10, 4, 4))
country_labels <- c("China", "Germany", "Italy", "Japan", "Other", "South Korea", "Sweden", "United Kingdom", "United States")

barplot(
  t(country_matrix),
  legend = TRUE,
  names.arg = country_labels,
  horiz = TRUE,
  xlab = "Total Number of Cars Involved in Accidents (Thousands)",
  main = "Number of Cars Involved in Accidents by Model Country",
  args.legend = list(x = 250, y = 2),
  xlim = c(0, 250)
)

country_prop <- all_datasets_fault %>%
  group_by(MOD_COUNTRY) %>%
  summarise(Fault = mean(is_fault), .groups = "drop")

country_not_prop <- all_datasets_not_fault %>%
  group_by(MOD_COUNTRY) %>%
  summarise(NotAtFault = mean(is_not_fault), .groups = "drop")

total_country_prop <- merge(x = country_prop, y = country_not_prop, by = "MOD_COUNTRY")
country_prop_matrix <- as.matrix(total_country_prop[, c("Fault", "NotAtFault")])

par(las = 1, mar = c(4, 10, 10, 4))
barplot(
  t(country_prop_matrix),
  legend = TRUE,
  names.arg = country_labels,
  horiz = TRUE,
  xlab = "Proportion of Cars Involved in Accidents",
  main = "Proportion of Cars Involved in Accidents by Model Country",
  args.legend = list(x = 0.6, y = 13.5)
)

# -------------------- Decision Tree Models -------------------- #

tree_country_gender <- ctree(fault_factors ~ mod_country_factors + gender_factors, data = all_datasets)
plot(tree_country_gender, gp = gpar(fontsize = 7))

tree_speed_alcohol <- ctree(fault_factors ~ alc_factors + speed_group_factors, data = all_datasets)
plot(tree_speed_alcohol, gp = gpar(fontsize = 6.5))

tree_alcohol_gender <- ctree(fault_factors ~ alc_factors + gender_factors, data = all_datasets)
plot(tree_alcohol_gender, gp = gpar(fontsize = 8))

tree_alcohol_country <- ctree(fault_factors ~ alc_factors + mod_country_factors, data = all_datasets)
plot(tree_alcohol_country, gp = gpar(fontsize = 6))

predictions <- predict(tree_country_gender, newdata = all_datasets)
table(predictions == all_datasets$Fault)
table(predictions)
table(all_datasets$Fault)

# -------------------- Train/Test Split -------------------- #

set.seed(12377)

n_train <- 366871
index_vector <- 1:524102
train_indices <- sample(index_vector, n_train, replace = FALSE)

all_datasets$Index <- 1:524102

training_set <- all_datasets %>% filter(Index %in% train_indices)
testing_set  <- all_datasets %>% filter(!(Index %in% train_indices))

# Model 1: Country + Gender
tree_country_gender_train <- ctree(fault_factors ~ mod_country_factors + gender_factors, data = training_set)
predictions <- predict(tree_country_gender_train, newdata = testing_set)
table(predictions)

# Model 2: Alcohol + Speed
tree_speed_alcohol_train <- ctree(fault_factors ~ alc_factors + speed_group_factors, data = training_set)
plot(tree_speed_alcohol_train, gp = gpar(fontsize = 8))
predictions <- predict(tree_speed_alcohol_train, newdata = testing_set)
table(predictions)

# Model 3: Gender + Alcohol
tree_gender_alcohol_train <- ctree(fault_factors ~ gender_factors + alc_factors, data = training_set)
plot(tree_gender_alcohol_train, gp = gpar(fontsize = 8))
predictions <- predict(tree_gender_alcohol_train, newdata = testing_set)
table(predictions)