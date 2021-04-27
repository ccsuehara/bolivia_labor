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

################################## CHILDREN #################################################
# Prepare data set ---------------------------------------
children2 <- children %>%
  select(-factor, -primary_salary, -primary_salary_freq, -work_type, -hh_lab_inc_pct, -marital, -pregnant, -num_alive_child,
         -primary_nonsalaried_income, -primary_nonsalaried_income_freq, -sec_employer_industry, -edu_status, -manual_labor,
         -sec_salary, -sec_salary_freq, -sec_nonsalaried_income, -sec_nonsalaried_income_freq, -num_literate,
         -emp_status, -folio, -nro, -sec_work_type, -indigenous_id, -edu, -any_ed, -higher_ed, -current_edu,
         -is_student, -why_not_in_school, -in_attendance, -why_absence, -internet_use_where_1, -internet_use_where_2)

children2 <- children2 %>%
  mutate(primary_job = ifelse(is.na(primary_job), F, T),
         chronic_disease_1 = ifelse(startsWith(chronic_disease_1, "12"), F, T),
         indigenous = ifelse(indigenous == "3. No soy boliviana o boliviano", "2. No pertenece", indigenous),
         language_1 = ifelse(language_1 %in% c("CASTELLANO", "QUECHUA"), language_1, "other"))

for (i in colnames(children2)) {
  if (guess_parser(children2[[i]]) == "logical") {
    children2[[i]] <- as.logical(children2[[i]])
  } else if (guess_parser(children2[[i]]) == "double") {
    children2[[i]] <- as.numeric(children2[[i]])
  } else if (length(unique(children2[[i]])) <= 30) {
    children2[[i]] <- as.factor(children2[[i]])
  }
}

children_10_ses <- children2 %>%
  filter(age > 9) %>%
  select(depto, area, sex, age, language_1, literate, indigenous, in_school, education, chronic_disease_1, disability,
         cellphone, internet_use, primary_job, union_member, hh_lab_inc, hh_sp_inc, hh_tot_inc, size) %>%
  mutate(union_member = ifelse(is.na(union_member), "2", union_member)) %>%
  mutate(primary_job = as.factor(primary_job))

# Partition data sets -------------------------
children_ind <- createDataPartition(children_10_ses$primary_job, p = 0.2, list = F)
children_10_ses_train <- children_10_ses[-children_ind,]
children_10_ses_test <- children_10_ses[children_ind,]

# Logistic regression -------------------------
children_10_ses_glm <- glm(primary_job ~ . - hh_lab_inc - hh_sp_inc - language_1 - education - literate - internet_use, data = children_10_ses_train, family = "binomial")
children_10_ses_glm_coef <- data.frame(name = names(children_10_ses_glm$coefficients), value = unname(abs(children_10_ses_glm$coefficients)))
ggplot(children_10_ses_glm_coef[-1,]) + geom_point(aes(value, reorder(name, value)), size = 3) + theme_minimal() + theme(axis.title = element_blank())
children_10_ses_glm_vars <- varImp(children_10_ses_glm) %>% as.data.frame() %>% mutate(names = rownames(.))
children_10_ses_p_hat <- predict(children_10_ses_glm, newdata = children_10_ses_test, type = "response")
children_10_ses_y_hat <- ifelse(children_10_ses_p_hat > 0.5, T, F)
children_10_ses_cm_glm <- confusionMatrix(as.factor(children_10_ses_y_hat), as.factor(children_10_ses_test$primary_job))
  # Balanced accuracy (AUC) 0.651

# Random forest -------------------------------
children_10_ses_rf <- randomForest(primary_job ~ . - hh_lab_inc - hh_sp_inc - language_1 - education - literate - internet_use, data = children_10_ses_train,
                                   importance = T)
varImpPlot(children_10_ses_rf, type = 1)
children_10_ses_y_hat_rf <- predict(children_10_ses_rf, newdata = children_10_ses_test)
children_10_ses_cm_rf <- confusionMatrix(children_10_ses_y_hat_rf, as.factor(children_10_ses_test$primary_job))
  # Balanced accuracy (AUC) 0.676



################################### YOUTH ###################################################
# Prepare data sets ---------------------------
youth2 <- youth %>%
  select(-factor, -primary_salary, -primary_salary_freq, -work_type, -hh_lab_inc_pct, -pregnant, -num_alive_child,
         -primary_nonsalaried_income, -primary_nonsalaried_income_freq, -sec_employer_industry, -edu_status, -manual_labor,
         -sec_salary, -sec_salary_freq, -sec_nonsalaried_income, -sec_nonsalaried_income_freq, -num_literate,
         -emp_status, -folio, -nro, -sec_work_type, -indigenous_id, -edu, -any_ed, -higher_ed, -current_edu,
         -is_student, -why_not_in_school, -in_attendance, -why_absence, -internet_use_where_1, -internet_use_where_2) %>%
  mutate(primary_job = ifelse(is.na(primary_job), F, T),
         in_school = ifelse(in_school == "1. Si", T, F),
         chronic_disease_1 = ifelse(startsWith(chronic_disease_1, "12"), F, T),
         language_1 = ifelse(language_1 %in% c("CASTELLANO", "QUECHUA"), language_1, "other"),
         indigenous = ifelse(indigenous == "3. No soy boliviana o boliviano", "2. No pertenece", indigenous),
         marital = case_when(str_detect(marital, "^1") ~ "single", str_detect(marital, "^[23]") ~ "married/cohabiting",
                             str_detect(marital, "^[456]") ~ "separated/divorced/widowed"),
         union_member = ifelse(is.na(union_member), "2. No", union_member))
youth2$r <- runif(nrow(youth2))

for (i in colnames(youth2)) {
  if (i %in% c("in_school", "primary_job")) {
    youth2[[i]] <- as.factor(youth2[[i]])
  } else if (guess_parser(youth2[[i]]) == "logical") {
    youth2[[i]] <- as.logical(youth2[[i]])
  } else if (guess_parser(youth2[[i]]) == "double") {
    youth2[[i]] <- as.numeric(youth2[[i]])
  } else if (length(unique(youth2[[i]])) <= 30) {
    youth2[[i]] <- as.factor(youth2[[i]])
  }
}

youth_ses <- youth2 %>%
  select(depto:primary_job, union_member, hh_tot_inc, r) %>%
  select(-nearZeroVar(.))

# Partition data sets --------------------------
youth_ind <- createDataPartition(youth2$in_school, p = 0.2, list = F)
youth_train <- youth2[-youth_ind,]
youth_test <- youth2[youth_ind,]
youth_ses_train <- youth_ses[-youth_ind,]
youth_ses_test <- youth_ses[youth_ind,]

# Variable names data frame ---------------------------
youth_ses_var_n <- data.frame(var = names(youth_ses),
                              name = c("department", "area", "sex", "age", "primary language", "marital status", "indigenous identity", "in school",
                                       "education", "cellphone access", "internet access", "employment", "monthly household income", "r"))

# Logistic regression 1 (do not use) ------------------------
# youth_ses_glm <- glm(in_school ~ ., data = youth_ses_train, family = "binomial")
# youth_ses_glm_vars <- varImp(youth_ses_glm) %>% as.data.frame()
# youth_ses_p_hat <- predict(youth_ses_glm, newdata = youth_ses_test, type = "response")
# youth_ses_y_hat <- ifelse(youth_ses_p_hat > 0.5, T, F)
# youth_ses_cm_glm <- confusionMatrix(as.factor(youth_ses_y_hat), as.factor(youth_ses_test$in_school))
  # Balanced accuracy 0.807

# Logistic regression 2: in school ------------------------
youth_ses_glm2 <- glm(in_school ~ . - primary_job - education, data = youth_ses_train, family = "binomial")
youth_ses_glm2_coef <- data.frame(name = names(youth_ses_glm2$coefficients), value = unname(abs(youth_ses_glm2$coefficients)))
ggplot(youth_ses_glm2_coef[-1,]) + geom_point(aes(value, reorder(name, value)), size = 3) + theme_minimal() + theme(axis.title = element_blank())
youth_ses_glm_vars2 <- varImp(youth_ses_glm2) %>% as.data.frame()
youth_ses_p_hat2 <- predict(youth_ses_glm2, newdata = youth_ses_test, type = "response")
youth_ses_y_hat2 <- ifelse(youth_ses_p_hat2 > 0.5, T, F)
youth_ses_cm_glm2 <- confusionMatrix(as.factor(youth_ses_y_hat2), as.factor(youth_ses_test$in_school))
  # Balanced accuracy 0.773

# Logistic regression 3: primary job -----------------------
youth_ses_glm3 <- glm(primary_job ~ ., data = youth_ses_train, family = "binomial")
youth_ses_glm3_coef <- data.frame(name = names(youth_ses_glm3$coefficients), value = unname(abs(youth_ses_glm3$coefficients)))
ggplot(youth_ses_glm3_coef[-1,]) + geom_point(aes(value, reorder(name, value)), size = 3) + theme_minimal() + theme(axis.title = element_blank())
youth_ses_glm_vars3 <- varImp(youth_ses_glm3) %>% as.data.frame()
youth_ses_p_hat3 <- predict(youth_ses_glm3, newdata = youth_ses_test, type = "response")
youth_ses_y_hat3 <- ifelse(youth_ses_p_hat3 > 0.5, T, F)
youth_ses_cm_glm3 <- confusionMatrix(as.factor(youth_ses_y_hat3), as.factor(youth_ses_test$primary_job))
  # Balanced accuracy 0.683

# Logistic regression 4: primary job with top 6 features indicated by random forest -----------------
youth_ses_glm4 <- glm(primary_job ~ in_school + sex + marital + age+ education + area, data = youth_ses_train, family = "binomial")
youth_ses_glm4_coef <- data.frame(name = names(youth_ses_glm4$coefficients), value = unname(abs(youth_ses_glm4$coefficients)))
ggplot(youth_ses_glm4_coef[-1,]) + geom_point(aes(value, reorder(name, value)), size = 3) + theme_minimal() + theme(axis.title = element_blank())
youth_ses_glm_vars3 <- varImp(youth_ses_glm3) %>% as.data.frame()
youth_ses_p_hat4 <- predict(youth_ses_glm4, newdata = youth_ses_test, type = "response")
youth_ses_y_hat4 <- ifelse(youth_ses_p_hat4 > 0.5, T, F)
youth_ses_cm_glm4 <- confusionMatrix(as.factor(youth_ses_y_hat4), as.factor(youth_ses_test$primary_job))
  # Balanced accuracy 0.729

# Random forest: in school -------------------------------
youth_ses_rf1 <- randomForest(in_school ~ . - primary_job - education - r, data = youth_ses_train, importance = T)
importance(youth_ses_rf1, type = 1) %>% as.data.frame() %>%
  mutate(var = rownames(.)) %>%
  left_join(youth_ses_var_n, by = "var") %>%
  write_csv("data/youth_ses_rf1_imp.csv")
varImpPlot(youth_ses_rf1, type = 1)
youth_ses_y_hat_rf1 <- predict(youth_ses_rf1, newdata = youth_ses_test)
youth_ses_cm_rf1 <- confusionMatrix(youth_ses_y_hat_rf1, as.factor(youth_ses_test$in_school))
  # Balanced accuracy 0.782

# acc <- sapply(1:5, function(ns) {
#   train(in_school ~ . - primary_job, method = "rf", data = youth_ses_train, tuneGrid = data.frame(mtry = 3), nodesize = ns)$results$Accuracy
# })

# Random forest: primary job -------------------------------
youth_ses_rf2 <- randomForest(primary_job ~ . - r, data = youth_ses_train, importance = T)
importance(youth_ses_rf2, type = 1) %>% as.data.frame() %>%
  mutate(var = rownames(.)) %>%
  left_join(youth_ses_var_n, by = "var") %>%
  write_csv("data/youth_ses_rf2_imp.csv")
varImpPlot(youth_ses_rf2, type = 1)
youth_ses_y_hat_rf2 <- predict(youth_ses_rf2, newdata = youth_ses_test)
youth_ses_cm_rf2 <- confusionMatrix(youth_ses_y_hat_rf2, as.factor(youth_ses_test$primary_job))
  # Balanced accuracy 0.744

# acc <- sapply(1:5, function(ns) {
#   train(primary_job ~ . - in_school, method = "rf", data = youth_ses_train, tuneGrid = data.frame(mtry = 3), nodesize = ns)$results$Accuracy
# })

# Random forest: primary job, but just for women --------------------
youth_w_ses <- youth_ses %>% filter(str_detect(sex, "^2")) %>% select(-sex)
youth_w_ind <- createDataPartition(youth_w_ses$primary_job, p = 0.2, list = F)
youth_w_ses_train <- youth_w_ses[-youth_w_ind,]
youth_w_ses_test <- youth_w_ses[youth_w_ind,]

youth_w_ses_rf <- randomForest(primary_job ~ ., data = youth_w_ses_train, importance = T)
varImpPlot(youth_w_ses_rf, type = 1)
youth_w_ses_y_hat_rf <- predict(youth_w_ses_rf, newdata = youth_w_ses_test)
youth_w_ses_cm_rf <- confusionMatrix(youth_w_ses_y_hat_rf, as.factor(youth_w_ses_test$primary_job))
  # Balanced accuracy 0.656

# Prepare data set for youth's income ------------------------
youth_job <- youth2 %>%
  filter(!is.na(lab_monthly_inc)) %>%
  mutate(paid = ifelse(lab_monthly_inc == 0, F, T)) %>%
  mutate(paid = as.factor(paid))

youth_job_ses <- youth_job %>%
  select(depto:primary_job, union_member, hh_tot_inc, r, paid, lab_monthly_inc) %>%
  mutate(hh_tot_inc2 = hh_tot_inc - lab_monthly_inc) %>%
  select(-nearZeroVar(.))

youth_job_ses_ind <- createDataPartition(youth_job_ses$paid, p = 0.2, list = F)
youth_job_ses_train <- youth_job_ses[-youth_job_ses_ind,]
youth_job_ses_test <- youth_job_ses[youth_job_ses_ind,]

# Logistic regression: paid or unpaid ---------------------
youth_job_ses_glm1 <- glm(paid ~ . - lab_monthly_inc - hh_tot_inc, data = youth_job_ses_train, family = "binomial")
youth_job_ses_glm1_coef <- data.frame(name = names(youth_job_ses_glm1$coefficients), value = unname(abs(youth_job_ses_glm1$coefficients)))
ggplot(youth_job_ses_glm1_coef[-1,]) + geom_point(aes(value, reorder(name, value)), size = 3) + theme_minimal() + theme(axis.title = element_blank())
youth_job_ses_glm_vars1 <- varImp(youth_job_ses_glm1) %>% as.data.frame()
youth_job_ses_p_hat1 <- predict(youth_job_ses_glm1, newdata = youth_job_ses_test, type = "response")
youth_job_ses_y_hat1 <- ifelse(youth_job_ses_p_hat1 > 0.5, "TRUE", "FALSE") %>% as.factor()
youth_job_ses_cm_glm1 <- confusionMatrix(youth_job_ses_y_hat1, youth_job_ses_test$paid)
  # Balanced accuracy 0.651

# Random forest: paid or unpaid ----------------------
youth_job_ses_rf1 <- randomForest(paid ~ . - lab_monthly_inc - hh_tot_inc - r, data = youth_job_ses_train, importance = T)
varImpPlot(youth_job_ses_rf1, type = 1)
youth_job_ses_y_hat_rf1 <- predict(youth_job_ses_rf1, newdata = youth_job_ses_test)
youth_job_ses_cm_rf1 <- confusionMatrix(youth_job_ses_y_hat_rf1, youth_job_ses_test$paid)
  # Balanced accuracy 0.673

# acc <- sapply(1:5, function(ns) {
#   train(paid ~ . - lab_monthly_inc, method = "rf", data = youth_job_ses_train, tuneGrid = data.frame(mtry = 3), nodesize = ns)$results$Accuracy
# })

# Logistic regression: income -------------------
youth_job_ses_glm2 <- glm(lab_monthly_inc ~ . - paid - hh_tot_inc, data = youth_job_ses_train)
youth_job_ses_glm2_coef <- data.frame(name = names(youth_job_ses_glm2$coefficients), value = unname(abs(youth_job_ses_glm2$coefficients)))
ggplot(youth_job_ses_glm2_coef[-1,]) + geom_point(aes(value, reorder(name, value)), size = 3) + theme_minimal() + theme(axis.title = element_blank())
youth_job_ses_glm_vars2 <- varImp(youth_job_ses_glm2) %>% as.data.frame()
youth_job_ses_y_hat2 <- predict(youth_job_ses_glm2, newdata = youth_job_ses_test, type = "response")
defaultSummary(data.frame(obs = youth_job_ses_test$lab_monthly_inc, pred = youth_job_ses_y_hat2))
  # R-squared 0.142

# Linear regression: income -------------------
youth_job_ses_lm <- lm(lab_monthly_inc ~ . - paid - hh_tot_inc, data = youth_job_ses_train)
youth_job_ses_lm_coef <- data.frame(name = names(youth_job_ses_lm$coefficients), value = unname(abs(youth_job_ses_lm$coefficients)))
ggplot(youth_job_ses_lm_coef[-1,]) + geom_point(aes(value, reorder(name, value)), size = 3) + theme_minimal() + theme(axis.title = element_blank())
youth_job_ses_lm_vars <- varImp(youth_job_ses_lm) %>% as.data.frame()
youth_job_ses_y_hat <- predict(youth_job_ses_lm, newdata = youth_job_ses_test, type = "response")
defaultSummary(data.frame(obs = youth_job_ses_test$lab_monthly_inc, pred = youth_job_ses_y_hat))
  # R-squared 0.142

# Random forest: income ------------------------
youth_job_ses_rf2 <- randomForest(lab_monthly_inc ~ . - paid - hh_tot_inc, data = youth_job_ses_train, importance = T)
varImpPlot(youth_job_ses_rf2, type = 1)
youth_job_ses_y_hat_rf2 <- predict(youth_job_ses_rf2, newdata = youth_job_ses_test)
defaultSummary(data.frame(obs = youth_job_ses_test$lab_monthly_inc, pred = youth_job_ses_y_hat_rf2))
  # R-squared 0.146

# acc <- sapply(1:5, function(ns) {
#   train(lab_monthly_inc ~ . - paid, method = "rf", data = youth_job_ses_train, tuneGrid = data.frame(mtry = 3), nodesize = ns)$results$Accuracy
# })

youth_job_ses_y_hat_e <- (youth_job_ses_y_hat2 + youth_job_ses_y_hat_rf2) / 2
defaultSummary(data.frame(obs = youth_job_ses_test$lab_monthly_inc, pred = youth_job_ses_y_hat_e))
  # R-squared 0.160




################################### ADULTS ##################################################
# Prepare data set -------------------
adults2 <- adults %>%
  mutate(primary_job = ifelse(is.na(primary_job), F, T),
         chronic_disease_1 = ifelse(startsWith(chronic_disease_1, "12"), F, T),
         language_1 = ifelse(language_1 %in% c("CASTELLANO", "QUECHUA"), language_1, "other"),
         indigenous = ifelse(indigenous == "3. No soy boliviana o boliviano", "2. No pertenece", indigenous),
         union_member = ifelse(is.na(union_member), "2. No", union_member))

for (i in colnames(adults2)) {
  if (guess_parser(adults2[[i]]) == "logical") {
    adults2[[i]] <- as.logical(adults2[[i]])
  } else if (guess_parser(adults2[[i]]) == "double") {
    adults2[[i]] <- as.numeric(adults2[[i]])
  } else if (length(unique(adults2[[i]])) <= 30) {
    adults2[[i]] <- as.factor(adults2[[i]])
  }
}

adults2 <- as_tibble(adults2)

adults2 <- adults2 %>%
  filter(primary_job) %>%
  select(-factor, -primary_job, -primary_work_week_hr, -primary_salary, -primary_salary_freq, -work_type, -hh_lab_inc_pct,
         -primary_nonsalaried_income, -primary_nonsalaried_income_freq, -primary_monthly_inc, -sec_employer_industry,
         -sec_work_week_hr, -sec_salary, -sec_salary_freq, -sec_nonsalaried_income, -sec_nonsalaried_income_freq, -sec_monthly_inc,
         -lab_monthly_inc, -emp_status, -folio, -nro, -sec_job, -sec_work_type, -indigenous_id, -edu, -any_ed, -higher_ed, -current_edu,
         -is_student, -why_not_in_school, -in_attendance, -why_absence, -internet_use_where_1, -internet_use_where_2, -tot_monthly_inc)
# adults2$indigenous_id <- as.factor(adults2$indigenous_id)

head(adults2) # Double check data set is correct and ready

# Divide variables into socioeconomic and income-related ---------------------
# allcols <- colnames(adults2)
# ses <- c(allcols[1:18], "union_member")
# inc <- setdiff(allcols, ses)
# 
# adults_ses <- adults2 %>% select(all_of(ses), paid)
# adults_inc <- adults2 %>% select(all_of(inc))

adults_ses <- adults2 %>% select(depto:literate, indigenous, education:internet_use, union_member, nonlab_monthly_inc, hh_nonlab_inc, size, rest_of_hh, paid)

# Partition data set into training and test sets -------------------
test_ind <- createDataPartition(adults2$paid, times = 1, p = 0.2, list = F)
adults_train <- adults2[-test_ind,]
adults_test <- adults2[test_ind,]
ses_train <- adults_ses[-test_ind,]
ses_test <- adults_ses[test_ind,]
inc_train <- adults_inc[-test_ind,]
inc_test <- adults_inc[test_ind,]

# Logistic regression on SES -------------------
ses_train <- ses_train %>%
  select(-pregnant, -num_alive_child, -literate, -num_literate)
ses_test <- ses_test %>%
  select(-pregnant, -num_alive_child, -literate, -num_literate)

glm_ses <- glm(paid ~ ., data = ses_train, family = "binomial")
glm_ses_coef <- data.frame(name = names(glm_ses$coefficients), value = unname(abs(glm_ses$coefficients)))
ggplot(glm_ses_coef[-1,]) + geom_point(aes(value, reorder(name, value)), size = 3) + theme_minimal() + theme(axis.title = element_blank())
glm_ses_vars <- varImp(glm_ses)
glm_ses_p_hat <- predict(glm_ses, newdata = ses_test, type = "response")
glm_ses_y_hat <- ifelse(glm_ses_p_hat > 0.5, "unpaid", "paid")
ses_cm_logit <- confusionMatrix(as.factor(glm_ses_y_hat), as.factor(ses_test$paid))
  # Balanced accuracy (AUC) 0.610

# Logistic regression on SES with women only, checking effects of pregnancy and number of children ---------------
women <- adults2 %>%
  filter(str_detect(sex, "^2")) %>%
  select(pregnant, num_alive_child, paid) %>%
  mutate(pregnant = ifelse(is.na(pregnant), "3", pregnant), num_alive_child = ifelse(is.na(num_alive_child), 0, num_alive_child)) %>%
  mutate(pregnant = as.factor(pregnant))
test_ind_women <- createDataPartition(women$paid, times = 1, p = 0.2, list = F)
women_train <- women[-test_ind_women,]
women_test <- women[test_ind_women,]

glm_women <- glm(paid ~ ., data = women_train, family = "binomial")
glm_women_vars <- glm_women$coefficients %>% as.data.frame()
p_hat_women <- predict.glm(glm_women, newdata = women_test, type = "response")
y_hat_women <- ifelse(p_hat_women > 0.5, "unpaid", "paid")
cm_women <- confusionMatrix(as.factor(y_hat_women), women_test$paid)
  # Balanced accuracy only 0.503. Do not use this.

# Logistic regression on SES with numeric predictors -------------------------
# Prepare data set
adults_num <- adults2

for (i in colnames(adults_num)) {
  if (class(adults2[[i]]) == "factor") {
    adults_num[[i]] <- as.numeric(adults2[[i]])
  }
}

adults_num <- adults_num %>% mutate_all(~replace(., is.na(.), 0))

adults_num$paid <- adults_num$paid - 1

adults_ses_num <- adults_num %>% select(all_of(ses), paid)
adults_inc_num <- adults_num %>% select(all_of(inc))
adults_train_num <- adults_num[-test_ind,]
adults_test_num <- adults_num[test_ind,]
ses_train_num <- adults_ses_num[-test_ind,]
ses_test_num <- adults_ses_num[test_ind,]
inc_train_num <- adults_inc_num[-test_ind,]
inc_test_num <- adults_inc_num[test_ind,]

# Run logistic regression
glm_ses_num <- glm(paid ~ ., data = ses_train_num, family = "binomial")
glm_ses_num_vars <- glm_ses_num$coefficients %>% as.data.frame()
p_hat_logit <- predict.glm(glm_ses_num, newdata = ses_test_num, type = "response")
y_hat_logit <- ifelse(p_hat_logit > 0.5, 1, 0)
ses_cm_logit1 <- confusionMatrix(as.factor(y_hat_logit), as.factor(ses_test_num$paid))
  # Balanced accuracy (AUC) 0.589

# Detect variables with high VIF
glm_ses_vif <- vif(glm_ses_num) %>% as.data.frame()

# Third logistic regression on SES, with confounding variables removed -------------------------------------
glm_ses_num2 <- glm(paid ~ . - num_alive_child - pregnant - literate - num_literate, data = ses_train_num, family = "binomial")
glm_ses_num_vars2 <- glm_ses_num2$coefficients %>% as.data.frame()
ses_p_hat_logit2 <- predict.glm(glm_ses_num2, newdata = ses_test_num, type = "response")
ses_y_hat_logit2 <- ifelse(ses_p_hat_logit2 > 0.5, 1, 0)
ses_cm_logit2 <- confusionMatrix(as.factor(ses_y_hat_logit2), as.factor(ses_test_num$paid))
  # Balanced accuracy (AUC) 0.605

# ses_roc_logit2 <- roc(response = as.factor(ses_test_num$paid), predictor = y_hat_logit2)

# Logistic regression on INC -----------------------
adults_inc_filled <- adults_inc

for (i in colnames(adults_inc_filled)) {
  if (class(adults_inc_filled[[i]]) == "numeric") {
    adults_inc_filled[[i]] <- replace_na(adults_inc_filled[[i]], 0)
  } else if (class(adults_inc_filled[[i]]) == "factor") {
    adults_inc_filled[[i]] <- as.numeric(adults_inc_filled[[i]])
  }
}

adults_inc_filled$paid <- as.numeric(adults_inc_filled$paid) - 1

inc_train_filled <- adults_inc_filled[-test_ind,]
inc_test_filled <- adults_inc_filled[test_ind,]

glm_inc_filled <- glm(paid ~ ., data = inc_train_filled)
glm_inc_filled_vars <- glm_inc_filled$coefficients %>% as.data.frame()
inc_p_hat_logit <- predict.glm(glm_inc_filled, newdata = inc_test_filled, type = "response")
inc_y_hat_logit <- ifelse(inc_p_hat_logit > 0.5, 1, 0)
inc_cm_logit <- confusionMatrix(as.factor(inc_y_hat_logit), as.factor(inc_test_filled$paid))
  # Balanced accuracy (AUC) 0.499







# Clustering ----------------------
adults_select <- adults_num %>%
  select(sex, area, union_member, cellphone, marital, internet_use)

# fviz_nbclust(adults_select, kmeans, method = "wss")
cluster <- kmeans(adults_select, 4, nstart = 50)$cluster
cluster1 <- adults_select[cluster == 1,]
cluster2 <- adults_select[cluster == 2,]
cluster3 <- adults_select[cluster == 3,]
cluster4 <- adults_select[cluster == 4,]

cm1 <- colMeans(cluster1) %>% as.data.frame()
cm2 <- colMeans(cluster2) %>% as.data.frame()
cm3 <- colMeans(cluster3) %>% as.data.frame()
cm4 <- colMeans(cluster4) %>% as.data.frame()
cm <- cbind(cm1, cm2, cm3, cm4)
  # Don't seem very useful right now


# Tree based methods --------------------------------
# Test run a single decision tree -------------------
single_tree_ses <- rpart(paid ~ ., data = ses_train, control = rpart.control(cp = 0.02))
plot(single_tree_ses)
text(single_tree_ses)

single_tree_inc <- rpart(paid ~ ., data = inc_train)
plot(single_tree_inc)
text(single_tree_inc)

# Tune hyperparameter cp ----------------------
# ses_tune <-  train(paid ~ ., data = ses_train, method = "rpart",
#                    tuneGrid = data.frame(cp = seq(0, 0.1, len = 50)), na.action = na.omit) # cp value with highest accuracy: 0.02
# inc_tune <-  train(paid ~ ., data = inc_train, method = "rpart",
                   # tuneGrid = data.frame(cp = seq(0, 0.1, len = 50)), na.action = na.omit) # cp value with highest accuracy: 0.1

# Random forest ------------------------
rf_ses <- randomForest(paid ~ ., data = ses_train %>% select(-pregnant, -num_alive_child, -nonlab_monthly_inc), importance = T)
plot(rf_ses)
varImp(rf_ses)
varImpPlot(rf_ses, type = 1)
y_hat_rf_ses <- predict(rf_ses, newdata = ses_test)
cm_rf_ses <- confusionMatrix(data = y_hat_rf_ses, reference = ses_test$paid)
  # Balanced accuracy (AUC) 0.696

## For some reason, rf_inc (and as a corollary, rf_adults) does not work well. There seems to be an error
# rf_inc <- randomForest(paid ~ ., data = inc_train, na.action = na.omit)
# plot(rf_inc)
# varImp(rf_inc)
# varImpPlot(rf_inc)
# 
# rf_adults <- randomForest(paid ~ ., data = adults_train, na.action = na.omit)
# plot(rf_adults)
# varImp(rf_adults)
# varImpPlot(rf_adults)

# Redo random forest with factor vectors as numeric ------------------------
# Step 1: Turn factor vectors into numeric
adults3 <- adults2

for (i in colnames(adults3)) {
  if (class(adults2[[i]]) == "factor" & i != "paid") {
    adults3[[i]] <- as.numeric(adults2[[i]])
  }
}

# Step 2: Prepare data frames
adults_ses2 <- adults3 %>% select(all_of(ses), paid)
adults_inc2 <- adults3 %>% select(all_of(inc))
adults_train2 <- adults3[-test_ind,]
adults_test2 <- adults3[test_ind,]
ses_train2 <- adults_ses2[-test_ind,]
ses_test2 <- adults_ses2[test_ind,]
inc_train2 <- adults_inc2[-test_ind,]
inc_test2 <- adults_inc2[test_ind,]

# Step 3: Redo random forest
rf_ses2 <- randomForest(paid ~ . - num_alive_child - pregnant - literate - num_literate, data = ses_train2, na.action = na.omit)
plot(rf_ses2)
varImp(rf_ses2)
varImpPlot(rf_ses2)
# p_hat_rf <- predict(rf_ses2, newdata = ses_test2, type = "response")  # cannot test on test set yet, because of missing values

# Random forest with NA values filled in -----------------------
ses_train_filled <- lapply(ses_train_num, as.factor) %>% as_tibble()
ses_train_filled$age <- as.numeric(ses_train_filled$age)
ses_train_filled$education <- as.numeric(ses_train_filled$education)

ses_test_filled <- lapply(ses_test_num, as.factor) %>% as_tibble()
ses_test_filled$age <- as.numeric(ses_test_filled$age)
ses_test_filled$education <- as.numeric(ses_test_filled$education)


rf_ses3 <- randomForest(paid ~ . - num_alive_child - pregnant - literate - num_literate, data = ses_train_filled)
plot(rf_ses3)
varImp(rf_ses3)
varImpPlot(rf_ses3)



# temp <- adults_num %>%
#   group_by(depto) %>%
#   summarize(mean_urb = mean(area == 2), mean_unpaid = mean(paid == 1))
# plot(temp$mean_urb, temp$mean_unpaid) # Strong correlation between rural area and unpaid labor


################################ OLDER ADULTS ###############################################
# Prepare data sets --------------------------
older2 <- older %>%
  select(-primary_salary, -primary_salary_freq, -work_type, -hh_lab_inc_pct, -pregnant, -num_alive_child,
         -primary_nonsalaried_income, -primary_nonsalaried_income_freq, -sec_employer_industry, -manual_labor,
         -sec_salary, -sec_salary_freq, -sec_nonsalaried_income, -sec_nonsalaried_income_freq, -num_literate,
         -emp_status, -folio, -nro, -sec_work_type, -indigenous_id, -edu, -any_ed, -higher_ed, -current_edu,
         -is_student, -why_not_in_school, -in_attendance, -why_absence, -internet_use_where_1, -internet_use_where_2) %>%
  mutate(primary_job = ifelse(is.na(primary_job), F, T),
         in_school = ifelse(in_school == "1. Si", T, F),
         chronic_disease_1 = ifelse(startsWith(chronic_disease_1, "12"), F, T),
         language_1 = ifelse(language_1 %in% c("CASTELLANO", "QUECHUA"), language_1, "other"),
         indigenous = ifelse(indigenous == "3. No soy boliviana o boliviano", "2. No pertenece", indigenous),
         marital = case_when(str_detect(marital, "^1") ~ "single", str_detect(marital, "^[23]") ~ "married/cohabiting",
                             str_detect(marital, "^[456]") ~ "separated/divorced/widowed"),
         union_member = ifelse(is.na(union_member), "2. No", union_member))
older2$r <- runif(nrow(older2))

for (i in colnames(older2)) {
  if (i %in% c("in_school", "primary_job")) {
    older2[[i]] <- as.factor(older2[[i]])
  } else if (guess_parser(older2[[i]]) == "logical") {
    older2[[i]] <- as.logical(older2[[i]])
  } else if (guess_parser(older2[[i]]) == "double") {
    older2[[i]] <- as.numeric(older2[[i]])
  } else if (length(unique(older2[[i]])) <= 30) {
    older2[[i]] <- as.factor(older2[[i]])
  }
}

older_ses <- older2 %>%
  select(depto:indigenous, education:primary_job, lab_monthly_inc, union_member, sp_monthly_inc:tot_monthly_inc, hh_sp_inc, hh_nonlab_inc, hh_tot_inc, pc_inc:paid) %>%
  select(-nearZeroVar(.)) %>%
  mutate(lab_monthly_inc = replace_na(lab_monthly_inc, 0))
  
# Partition data sets ----------------------------
older_ind <- createDataPartition(older2$r, p = 0.2, list = F)
older_train <- older2[-older_ind,]
older_test <- older2[older_ind,]
older_ses_train <- older_ses[-older_ind,]
older_ses_test <- older_ses[older_ind,]

# Variable names data frame ----------------------
older_ses_var_n <- data.frame(var = names(older_ses),
                              name = c("department", "area", "sex", "age", "primary language", "marital status", "literacy",
                                       "indigenous identity", "education", "chronic disease", "disability", "cellphone access", "internet access", "employment",
                                       "monthly labor income", "union membership", "monthly social protection income", "monthly non-labor income", "monthly total income",
                                       "household monthly social protection income", "household monthly non-labor income", "household monthly total income",
                                       "household monthly per capita income", "household size", "monthly per capita income\nfor rest of household", "paid"))

# Random forest: primary job ------------------------------
older_ses_rf1 <- randomForest(primary_job ~ ., data = older_ses_train %>% select(-lab_monthly_inc, -sp_monthly_inc, -tot_monthly_inc, -hh_sp_inc, -hh_nonlab_inc, -hh_tot_inc, -pc_inc, -paid), importance = T)
# older_ses_rf1 <- train(primary_job ~ ., data = older_ses_train %>% select(-lab_monthly_inc, -sp_monthly_inc, -tot_monthly_inc, -hh_sp_inc, -hh_nonlab_inc, -hh_tot_inc, -pc_inc, -paid), importance = T)
plot(older_ses_rf1)
importance(older_ses_rf1, type = 1) %>% as.data.frame() %>%
  mutate(var = rownames(.)) %>%
  left_join(older_ses_var_n, by = "var") %>%
  write_csv("data/older_ses_rf1_imp.csv")
varImpPlot(older_ses_rf1, type = 1)
# most important features: nonlab_monthly_inc, depto, age, size, sex, rest_of_hh, area
older_ses_rf_y_hat1 <- predict(older_ses_rf1, newdata = older_ses_test)
older_ses_rf_cm1 <- confusionMatrix(older_ses_rf_y_hat1, older_ses_test$primary_job)
  # Balanced accuracy 0.781

# Logistic regression: primary job ------------------------
older_ses_glm1 <- glm(primary_job ~ nonlab_monthly_inc + area + age + sex + pc_inc, data = older_ses_train, family = "binomial")
older_ses_glm1_coef <- data.frame(name = names(older_ses_glm1$coefficients), value = unname(abs(older_ses_glm1$coefficients)))
older_ses_glm_p_hat1 <- predict(older_ses_glm1, newdata = older_ses_test)
older_ses_glm_y_hat1 <- ifelse(older_ses_glm_p_hat1 > 0.5, "TRUE", "FALSE") %>% as.factor()
older_ses_glm_cm1 <- confusionMatrix(older_ses_glm_y_hat1, older_ses_test$primary_job)
  # Balanced accuracy 0.738

# Random forest: paid or unpaid ----------------------------
older_job <- older_ses %>%
  filter(primary_job == "TRUE") %>%
  select(-primary_job)

older_job_ind <- createDataPartition(older_job$paid, p = 0.2, list = F)
older_job_train <- older_job[-older_job_ind,]
older_job_test <- older_job[older_job_ind,]

older_ses_rf2 <- randomForest(paid ~ ., data = older_job_train %>% select(-lab_monthly_inc, -sp_monthly_inc, -tot_monthly_inc, -hh_sp_inc, -hh_nonlab_inc, -hh_tot_inc, -pc_inc), importance = T)
plot(older_ses_rf2)
importance(older_ses_rf2, type = 1) %>% as.data.frame() %>%
  mutate(var = rownames(.)) %>%
  left_join(older_ses_var_n, by = "var") %>%
  write_csv("data/older_ses_rf2_imp.csv")
varImpPlot(older_ses_rf2, type = 1)
# most important features: sex, depto, size, age, nonlab_monthly_inc, rest_of_hh
older_ses_rf_y_hat2 <- predict(older_ses_rf2, newdata = older_job_test)
older_ses_rf_cm2 <- confusionMatrix(older_ses_rf_y_hat2, older_job_test$paid)
  # Balanced accuracy 0.805

# Random forest: labor income ------------------------------
older_pay <- older_job %>%
  filter(paid == "paid") %>%
  select(-paid)

older_pay_ind <- createDataPartition(older_pay$lab_monthly_inc, p = 0.2, list = F)
older_pay_train <- older_pay[-older_pay_ind,]
older_pay_test <- older_pay[older_pay_ind,]

older_ses_rf3 <- randomForest(lab_monthly_inc ~ ., data = older_pay_train %>%
                                select(-sp_monthly_inc, -tot_monthly_inc, -hh_sp_inc, -hh_nonlab_inc, -hh_tot_inc, -pc_inc), importance = T)
plot(older_ses_rf3)
varImpPlot(older_ses_rf3, type = 1)
# most important features: rest_of_hh, education, age, nonlab_monthly_inc, area, size
older_ses_rf_y_hat3 <- predict(older_ses_rf3, newdata = older_pay_test)
defaultSummary(data.frame(obs = older_pay_test$lab_monthly_inc, pred = older_ses_rf_y_hat3))
  # R-squared 0.082

# Logistic regression: labor income -------------------------
older_ses_glm2 <- glm(lab_monthly_inc ~ sex + rest_of_hh + area + marital + size + depto, data = older_pay_train %>%
                        select(-sp_monthly_inc, -tot_monthly_inc, -hh_sp_inc, -hh_nonlab_inc, -hh_tot_inc, -pc_inc))
older_ses_glm2_coef <- data.frame(name = names(older_ses_glm2$coefficients), value = unname(abs(older_ses_glm2$coefficients)))
older_ses_glm_y_hat2 <- predict(older_ses_glm2, newdata = older_pay_test)
defaultSummary(data.frame(obs = older_pay_test$lab_monthly_inc, pred = older_ses_glm_y_hat2))
  # R-squared 0.177
