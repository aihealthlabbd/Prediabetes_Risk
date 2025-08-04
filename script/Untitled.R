# Load the tidyverse library
library(tidyverse)
library(readxl)
library(naniar)


# --- Helper function to calculate the mode ---
calculate_mode <- function(x, na.rm = TRUE) {
  if(na.rm){
    x <- x[!is.na(x)] # Remove NA values
  }
  if(length(x) == 0) {
    return(NA) # Return NA if vector is empty or all NAs
  }
  # Calculate frequency of each unique value
  ux <- unique(x)
  freq <- tabulate(match(x, ux))
  # Return the value with the maximum frequency (first one in case of tie)
  mode_value <- ux[which.max(freq)]
  return(mode_value)
}

# --- Read the data ---
df <- read_excel("data/Diabetes_ALL.xlsx")
head(df)

# --- Impute Missing Values (Mode for Categorical) ---
# Identify categorical columns used for scoring (use backticks for names with spaces)
cols_to_impute <- c("Age", "Gender", "Gestational_Diabetes ", "Family_History",
                    "High_Blood_Pressure", "Physically_Active", "Weight_Status")

# Apply mode imputation
df_imputed <- df %>%
  mutate(across(all_of(cols_to_impute),
                ~ replace_na(.x, calculate_mode(.x))
  ))

# --- Calculate scores using imputed data ---
df_scored <- df_imputed %>%
  mutate(
    Age_Score = case_when(
      Age == "Less than 40 Years" ~ 0,
      Age == "40-49 years" ~ 1,
      Age == "50-59 Years" ~ 2,
      Age == "60 years or older" ~ 3,
      TRUE ~ 0
    ),
    Gender_Score = if_else(Gender == "Male", 1, 0),

    # Use imputed `Gestational_Diabetes `
    Gestational_Diabetes_Score = case_when(
      Gender == "Female" & `Gestational_Diabetes ` == "Yes" ~ 1,
      TRUE ~ 0
    ),
    Family_History_Score = if_else(Family_History == "Yes", 1, 0),
    High_Blood_Pressure_Score = if_else(High_Blood_Pressure == "Yes", 1, 0),
    Physically_Active_Score = if_else(Physically_Active == "No", 1, 0),

    Weight_Score = case_when(
      str_detect(Weight_Status, "<189") ~ 0,
      str_detect(Weight_Status, "189-226") ~ 1,
      str_detect(Weight_Status, ">226") ~ 2,
      TRUE ~ 0
    ),

    # Calculate Total Score
    Total_Score = Age_Score + Gender_Score + Gestational_Diabetes_Score +
      Family_History_Score + High_Blood_Pressure_Score +
      Physically_Active_Score + Weight_Score
  )

# --- Print the final dataframe ---
print(df_scored)

# Optional: View in RStudio's viewer
# View(df_scored)

# Optional: Save the scored data to a new CSV file
# write_csv(df_scored, "scored_survey_data_imputed_final.csv")
