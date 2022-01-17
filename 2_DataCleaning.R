######################################
### Author: Rafael Charris
######################################

######################################
## 1. LOAD PACKAGES
######################################
library(tidyverse) 
library(readxl)
library(janitor)
library(Hmisc) # General data manipulation things
library(fastDummies)

######################################
## 2. READ THE DATA
######################################

df <- readxl::read_excel(".//data//original//EIOPIAv2.xlsx") %>%
  clean_names()

## 2.1 GENERAL DESCRIPTION 
describe(df)

######################################
## 3. DATA MANIPULATION
######################################
df_final <- df %>%
  mutate(country = case_when(
    co == 1 ~ "English",
    co == 2 ~ "Greece",
    co == 3 ~ "Portugal",
    co == 4 ~ "Estonia",
    co == 5 ~ "Bulgaria",
    co == 6 ~ "Germany",
    co == 7 ~ "NONE"
  ),
  por_grec = ifelse(co == 2 | co== 3, 1, 0),
  q1_correct = ifelse(q1 ==1, 1,0),
  q2_correct = ifelse(q2 == 2, 1, 0),
  q3_correct = case_when(
    h_module1 == 1 & q3==1 ~ 1,
    h_module1 ==2 & q3 ==2 ~ 1,
    h_module1 != 1 & h_module1 != 2 & q3 == 4 ~ 1,
    TRUE ~ 0
  ),
  q4_correct = ifelse(q4 ==3, 1,0),
  q5_correct = case_when(
    h_module2 == 3 & q5r1 == 1 ~ 1,
    q5r2 == 1 & h_module2 == 2 ~ 1,
    q5r4 == 1 & h_module1 == 2 ~ 1,
    q5r5 == 1 & h_module1 == 1 ~ 1,
    q5r6 == 1 & h_module2 == 4 ~ 1,
    q5r8 == 1 & h_module2 == 4 ~ 1,
    TRUE ~ 0
  ),#q5r3 is alwwas false because we never use a banner
  d1_correct = ifelse(d1 == 2, 1, 0),
  d2_correct = ifelse(d2 == 2, 1, 0),
  d3_correct = ifelse(d3 == 2, 1, 0),
  d4_correct = ifelse(d4 == 1, 1, 0),
  d5_correct = ifelse(d5 == 1, 1, 0),
  #Response variable
  y_allianz = ifelse(e1 == 2, 1, 0),
  # Create the group measures: risk level, digital skill, finantial literate
  below_m_age = ifelse(s1 < median(s1), 1,0),
  below_m_risk = ifelse(c1 < median(c1), 1, 0)
  ) %>%
  rename(
    "age" = "s1",
    "age_group" = "h_age",
    "when_last_buy_internet" = "b3",
    "times_buy_internet" ="b4",
    "treatment_cell" = "h_cells",
    "risk_level" = "h_module3",
    "com_practice1" = "h_module1",
    "com_practice2" ="h_module2",
    "education" = "s3",
    "marital_status" = "s4",
    "house_income" = "s5",
    "employment" = "s6",
    "risk" = "c1",
    "time_pref" = "c2"
  ) %>%
  #Reverse the values of the digital skill questions
  mutate(across(matches("b1r|b2r"), ~ abs(.x -5))) %>%
  rowwise() %>%
  mutate(
    #0 to 1
    accuracy_rate = sum(
      q1_correct,
      q2_correct,
      q3_correct,
      q4_correct,
      q5_correct
    )/5,
    # Max score is 4
    digit_skill = sum(b1r1,
                      b1r2,
                      b1r3,
                      b1r4,
                      b2r1, 
                      b2r2,
                      b2r3,
                      b2r4,
                      b2r5,
                      b2r6,
                      b2r7,
                      b2r8,
                      b2r9,
                      b2r10)/14,
    fin_lit = sum(d1_correct,d3_correct,d3_correct,d4_correct,d5_correct),
  ) %>%
  ungroup() %>%
  mutate(
    below_m_lit = ifelse(fin_lit < median(fin_lit), 1, 0),
    below_m_digit = ifelse(digit_skill < median(digit_skill), 1, 0 ),
    #Reverse the scale so the control is the intercept
    treatment_cell = max(treatment_cell) + 1 - treatment_cell,
   ) %>%
  select(-intro_1) %>%
  # To create the dummies for portugal and grece by cell y merge the columns and then transform them into dummies
  unite('grece_portugal_cell', c(treatment_cell,por_grec), sep = "XPorGrec", remove= FALSE) %>%
  unite('risk_cell', c(treatment_cell, below_m_risk), sep = "Xrisk", remove= FALSE) %>%
  unite('age_cell', c(treatment_cell, below_m_age), sep = "Xage", remove= FALSE) %>%
  unite('lit_cell', c(treatment_cell, below_m_lit), sep = "Xfinlit", remove= FALSE) %>%
  unite('digit_cell', c(treatment_cell, below_m_digit), sep = "Xdigit", remove= FALSE)
  
# Add the dummies to the data set
df_final <- bind_cols(df_final, dummy_columns(df_final$grece_portugal_cell, remove_first_dummy = TRUE, remove_selected_columns = TRUE))
df_final <- bind_cols(df_final, dummy_columns(df_final$risk_cell, remove_first_dummy = TRUE, remove_selected_columns = TRUE))
df_final <- bind_cols(df_final, dummy_columns(df_final$age_cell, remove_first_dummy = TRUE, remove_selected_columns = TRUE))
df_final <- bind_cols(df_final, dummy_columns(df_final$lit_cell, remove_first_dummy = TRUE, remove_selected_columns = TRUE))
df_final <- bind_cols(df_final, dummy_columns(df_final$digit_cell, remove_first_dummy = TRUE, remove_selected_columns = TRUE))

######################################
## 4. NOTES:
###   I need to finish the grops I need the internet one
######################################

######################################
## 5. SAVE FINAL VERSION OF THE DATA
######################################

# hacer regresiones para los dos paises,

#hacer regresiones separadas por grupos
# hacer las mismas regresiones pero por los grupos: split por mediana 
df_final %>%
  write.csv(".//data//clean//base.csv")