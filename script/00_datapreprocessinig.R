library(tidyverse)
library(readxl)
library(naniar)

# data
data <- read_excel("data/Diabetes_ALL.xlsx")
head(data)

# Check missing values
sum(is.na(data |> select(-Gestational_Diabetes)))
miss_var_which(data)
gg_miss_var(data)


# impute with the mode
mode(data$Gender)

mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

clean_data <- data |>
  mutate(Age = ifelse(is.na(Age), mode(Age), Age)) |>
  mutate(Gender = ifelse(is.na(Gender), mode(Gender), Gender)) |>
  mutate(Marital_Status = ifelse(is.na(Marital_Status), mode(Marital_Status), Marital_Status)) |>
  mutate(Occupation = ifelse(is.na(Occupation), mode(Occupation), Occupation)) |>
  mutate(Family_Income = ifelse(is.na(Family_Income), mode(Family_Income), Family_Income)) |>
  mutate(Education = ifelse(is.na(Education), mode(Education), Education)) |>
  mutate(Residence = ifelse(is.na(Residence), mode(Residence), Residence)) |>
  mutate(Family_History = ifelse(is.na(Family_History), mode(Family_History), Family_History)) |>
  mutate(High_Blood_Pressure = ifelse(is.na(High_Blood_Pressure), mode(High_Blood_Pressure), High_Blood_Pressure)) |>
  mutate(Physically_Active = ifelse(is.na(Physically_Active), mode(Physically_Active), Physically_Active)) |>
  mutate(Height = ifelse(is.na(Height), mode(Height), Height)) |>
  mutate(Weight_Status = ifelse(is.na(Weight_Status), mode(Weight_Status), Weight_Status)) |>
  mutate(District = ifelse(is.na(District), mode(District), District))

gg_miss_var(clean_data)
miss_var_summary(clean_data)
sum(is.na(clean_data))

# Standardize Height and Weight_Status columns
clean_data <- clean_data  |>
  mutate(
    Height = str_replace_all(Height, "’", "'")  |>   # Replace curly quotes with straight quotes
      str_replace_all("’’", "\""),          # Replace double curly quotes with straight double quotes
    Weight_Status = str_trim(Weight_Status)        # Remove extra spaces
  )







# Calculate the total score
processed_data <- data |>
  mutate(
    Age_Points = ifelse(Age == "Less than 40 Years", 0, ifelse(Age == "40-49 Years", 1, ifelse(Age == "50-59 Years", 2, 3))),
    Gender_Points = ifelse(Gender == "Male", 1, 0),
    Gestational_Points = ifelse(Gestational_Diabetes == "Yes", 1, 0),
    Family_Points = ifelse(Family_History == "Yes", 1, 0),
    BP_Points = ifelse(High_Blood_Pressure == "Yes", 1, 0),
    Activity_Points = ifelse(Physically_Active == "No", 1, 0),
    Weight_Points = calculate_weight_points(Height, Weight_Status),
    Total_Score = Age_Points + Gender_Points + Gestational_Points + Family_Points + BP_Points + Activity_Points + Weight_Points,
    Diabetes_Risk = ifelse(Total_Score >= 5, "At Risk", "Not At Risk")
)

clean_data |>
  select(Height, Weight_Status) |>
  head()


unique(clean_data$Height)
unique(clean_data$Weight_Status)
