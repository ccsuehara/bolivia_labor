library(tidyverse)
library(randomForest)
library(caret)
library(tree)
library(rpart)
library(car)
library(factoextra)
library(Rborist)
library(pROC)
library(GGally)


source("EDA.R")

source("EDA2.R")


make_forest_importance <- function(df, name_of_file, name_df) {
  result <- randomForest(target_var ~ ., data = df, importance = T)
  importance(result, type = 1) %>% as.data.frame() %>%
    mutate(var = rownames(.)) %>%
    left_join(name_df, by = "var") %>%
    write_csv( paste0("data/", name_of_file , ".csv"))
}

convert_types <- function(df) {
  for (i in colnames(df)) {
    if (i %in% c("in_school", "primary_job")) {
      df[[i]] <- as.factor(df[[i]])
    } else if (guess_parser(df[[i]]) == "logical") {
      df[[i]] <- as.logical(df[[i]])
    } else if (guess_parser(df[[i]]) == "double") {
      df[[i]] <- as.numeric(df[[i]])
    } else if (length(unique(df[[i]])) <= 30) {
      df[[i]] <- as.factor(df[[i]])
    }
  }
  return(df)
}


adults_rf <- adults %>% select(
  area, sex, age, language_1, literate, indigenous, chronic_disease_1,disability, cellphone, internet_use, yrs_educ,  marital, emp_status)  %>%
  mutate(chronic_disease_1 = ifelse(startsWith(chronic_disease_1, "12"), F, T),
         indigenous = ifelse(indigenous == "3. No soy boliviana o boliviano", "2. No pertenece", indigenous),
         language_1 = ifelse(language_1 %in% c("CASTELLANO", "QUECHUA"), language_1, "other"),
         marital = case_when(str_detect(marital, "^1") ~ "single", str_detect(marital, "^[23]") ~ "married/cohabiting",
                             str_detect(marital, "^[456]") ~ "separated/divorced/widowed"),
         target_var = ifelse(emp_status == "Employed", "employed", "not_employed")) %>%
  select(-emp_status)




adults_rf_n <- data.frame(var = names(adults_rf),
                              name = c("area", "sex", "age", "primary language", "literacy", 
                                       "indigenous identity", "chronic disease", "disability", "cellphone  access", 
                                       "internet access", "years of education", "marital status", "employed"))
   
adults_rf <- convert_types(adults_rf)    

make_forest_importance(adults_rf, "adults_rf", adults_rf_n)


neets_rf <- ages_neet %>% select(
  area, sex, age, language_1, literate, indigenous, chronic_disease_1,disability, cellphone,
  internet_use, yrs_educ,  marital, neet_cat,
  pc_inc)  %>%
  mutate(chronic_disease_1 = ifelse(startsWith(chronic_disease_1, "12"), F, T),
         indigenous = ifelse(indigenous == "3. No soy boliviana o boliviano", "2. No pertenece", indigenous),
         language_1 = ifelse(language_1 %in% c("CASTELLANO", "QUECHUA"), language_1, "other"),
         marital = case_when(str_detect(marital, "^1") ~ "single", str_detect(marital, "^[23]") ~ "married/cohabiting",
                             str_detect(marital, "^[456]") ~ "separated/divorced/widowed"),
         target_var = ifelse(neet_cat == "NEET", "neet", "not neet")) %>%
  select(-neet_cat)




neets_rf_n <- data.frame(var = names(neets_rf),
                          name = c("area", "sex", "age", "primary language", "literacy", 
                                   "indigenous identity", "chronic disease", "disability", "cellphone  access", 
                                   "internet access", "years of education", "marital status", "income per capita", "neet"))


neets_rf <- convert_types(neets_rf)    

make_forest_importance(neets_rf, "neets_rf", neets_rf_n)



