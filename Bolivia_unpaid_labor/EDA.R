library(readxl)
library(tidyverse)

personas <- suppressWarnings(read_excel("/Users/csolisu/Documents/Carla/chamba/shared_Bolivia/data/EH_2018/EH2018_Personas2.xlsx") %>%
  mutate(higher_ed = ifelse(startsWith(edu, "7") | startsWith(edu, "8"), T, F),
         any_ed = ifelse(startsWith(edu, "11"), F, T)))

# Select people with at least 1 job
with_job <- personas %>%
  filter(!is.na(primary_job) | !is.na(sec_job)) %>%
  select(folio, nro, area, sex, age, marital, literate, num_literate, indigenous, indigenous_id,
         edu, any_ed, higher_ed, in_school,
         chronic_disease_1, disability_1, pregnant, num_alive_child,
         manual_labor,
         cellphone, internet_use, internet_use_where_1, internet_use_where_2,
         primary_job, work_type, primary_job_salary, primary_job_salary_freq, primary_job_nonsalaried_income, primary_job_nonsalaried_income_freq,
         sec_job, sec_employer_industry, sec_work_type, sec_salary, sec_salary_freq, sec_income, sec_income_freq,
         want_work_more, avail_work_more, union_member)

# Select people with at least 1 unpaid job
unpaid_job <- with_job %>%
  filter(startsWith(work_type, "7") | startsWith(work_type, "8") | startsWith(sec_work_type, "7") | startsWith(sec_work_type, "8"))

# Select people with only 1 job, which is unpaid
unpaid_job1 <- unpaid_job %>%
  filter(sec_job == "2. No")

# Select people whose primary job is paid but secondary job is unpaid
unpaid_sec_job <- with_job %>%
  filter(!startsWith(work_type, "7") & !startsWith(work_type, "8") & (startsWith(sec_work_type, "7") | startsWith(sec_work_type, "8")))

# Select people whose primary job is unpaid but secondary job is paid
unpaid_pri_job <- with_job %>%
  filter((startsWith(work_type, "7") | startsWith(work_type, "8")) & !startsWith(sec_work_type, "7") & !startsWith(sec_work_type, "8"))

# Select child workers under 15
child_worker <- with_job %>%
  filter(age < 15) %>%
  filter(startsWith(work_type, "7"))

child <- personas %>%
  filter(age < 15) %>%
  filter(in_school == "2. No" & is.na(work_type))


educ_list <- sort(unique(c(personas$edu)))
educ_eq <- c("Less than Primary", "Less than Primary", #11, 12
             "Less than Primary", #13
             "Primary","Secondary", #21 22 
             "Secondary", "Primary", "Secondary", #23, 31, 32, 
             "Primary", "Secondary",  #41, 42
             "Primary", "Secondary",  #51, 52
             "Secondary","Primary",  #61, 62
             "Secondary","Primary",  #63, 64
             "Secondary", #65
             "Tertiary", "Tertiary", "Tertiary", #71, 72, 73
             "Tertiary", "Tertiary", "Tertiary", #74, 75, 76
             "Tertiary", "Tertiary", "Tertiary", # 77, 79, 80
             "Tertiary" #81
             )

personas <- personas %>% mutate(education = plyr::mapvalues(edu,educ_list, educ_eq))

personas <- personas %>% mutate(emp_status = case_when(
  work_last_week_1 == '1. Si' ~ "Employed", 
  work_last_week_2 != "8.NINGUNA ACTIVIDAD" & is.na(work_last_week_2) == FALSE ~ "Employed",
  work_last_week_3 == "1.Vacaciones o permisos?"| 
    work_last_week_3 == "2.Licencia de maternidad?"|
    work_last_week_3 == "8.Estar suspendido?" ~ "Employed",
  work_last_week_3 == "5.Temporada baja?"| 
  work_last_week_3 == "9.Problemas personales o familiares?"| 
  work_last_week_3 == "4.Falta de materiales o insumos?"|
  work_last_week_3 == "3.Enfermedad o accidente?" ~ "Unemployed",
  unemployed =="1. Si" ~ "Unemployed",
  looked_for_work  =="1. Si" ~ "Unemployed",
  looked_for_work  =="2. No" ~ "Inactive")
)

personas <- personas %>% mutate(is_student = case_when( in_school == "1. Si" ~ "Yes",
                                                        in_school == "2. No" ~ "No"))

employed <- personas %>% filter(emp_status == "Employed") %>%
  select(folio, nro, area, sex, age, marital, literate, num_literate, indigenous, indigenous_id,
         edu, any_ed, higher_ed, in_school,
         chronic_disease_1, disability_1, pregnant, num_alive_child,
         manual_labor,
         cellphone, internet_use, internet_use_where_1, internet_use_where_2,
         primary_job, work_type, primary_job_salary, primary_job_salary_freq, primary_job_nonsalaried_income, primary_job_nonsalaried_income_freq,
         sec_job, sec_employer_industry, sec_work_type, sec_salary, sec_salary_freq, sec_income, sec_income_freq,
         want_work_more, avail_work_more, union_member) %>% filter(age %in% 25:90)

unemployed <-  personas %>% filter(emp_status == "Unemployed") %>% 
  select(folio, nro, area, sex, age, marital, literate, num_literate, indigenous, indigenous_id,
         edu, any_ed, higher_ed, in_school,
         chronic_disease_1, disability_1, pregnant, num_alive_child,
         manual_labor,
         cellphone, internet_use, internet_use_where_1, internet_use_where_2,
         primary_job, work_type, primary_job_salary, primary_job_salary_freq, primary_job_nonsalaried_income, primary_job_nonsalaried_income_freq,
         sec_job, sec_employer_industry, sec_work_type, sec_salary, sec_salary_freq, sec_income, sec_income_freq,
         want_work_more, avail_work_more, union_member) %>% filter(age %in% 25:90)

inactive <- personas %>% filter(emp_status == "Inactive") %>% 
  select(folio, nro, area, sex, age, marital, literate, num_literate, indigenous, indigenous_id,
         edu, any_ed, higher_ed, in_school,
         chronic_disease_1, disability_1, pregnant, num_alive_child,
         manual_labor,
         cellphone, internet_use, internet_use_where_1, internet_use_where_2,
         primary_job, work_type, primary_job_salary, primary_job_salary_freq, primary_job_nonsalaried_income, primary_job_nonsalaried_income_freq,
         sec_job, sec_employer_industry, sec_work_type, sec_salary, sec_salary_freq, sec_income, sec_income_freq,
         want_work_more, avail_work_more, union_member) %>% filter(age %in% 25:90)

neet  <- personas %>% filter(emp_status == "Inactive" & is_student == "No") %>%
  select(folio, nro, area, sex, age, marital, literate, num_literate, indigenous, indigenous_id,
         edu, any_ed, higher_ed, in_school,
         chronic_disease_1, disability_1, pregnant, num_alive_child,
         manual_labor,
         cellphone, internet_use, internet_use_where_1, internet_use_where_2,
         primary_job, work_type, primary_job_salary, primary_job_salary_freq, primary_job_nonsalaried_income, primary_job_nonsalaried_income_freq,
         sec_job, sec_employer_industry, sec_work_type, sec_salary, sec_salary_freq, sec_income, sec_income_freq,
         want_work_more, avail_work_more, union_member)  %>% filter(age %in% 15:45)
  

# ggplot(child_worker) +
#   geom_bar(aes(x = age, fill = work_type), position = "dodge", width = 0.5) +
#   theme_minimal() +
#   theme(legend.position = "bottom")
# 
# 
# ggplot(unemployed) +
#   geom_density(aes(x = age, fill = sex), position = "dodge", width = 0.5, alpha = 0.5) +
#   theme_minimal() +
#   theme(legend.position = "bottom")


# General population characteristics

# Age and sex
# ggplot(personas) +
#   geom_bar(aes(x = age, fill = sex), position = "dodge") +
#   theme_minimal() +
#   theme(legend.position = "bottom") +
#   scale_x_continuous(breaks = c(0, 7, 18, 90))
# 
# ggplot(personas %>% filter(age > 3)) +
#   geom_bar(aes(y = higher_ed, fill = sex), position = "fill") +
#   theme_minimal() +
#   theme(legend.position = "bottom")
# 
# # Job by age and sex
# ggplot(with_job) +
#   geom_bar(aes(x = age, fill = sex), position = "dodge") +
#   theme_minimal() +
#   theme(legend.position = "bottom") +
#   scale_x_continuous(breaks = c(7, 18, 90))
# # ggsave("job_by_age_and_sex.png")
# 
# # People with any unpaid job, by sex and edu
# ggplot(unpaid_job) +
#   geom_bar(aes(x = sex, fill = higher_ed), position = "fill") +
#   theme_minimal() +
#   theme(legend.position = "bottom")
# 
# ggplot(unpaid_job1) +
#   geom_bar(aes(x = sex, fill = higher_ed), position = "fill") +
#   theme_minimal() +
#   theme(legend.position = "bottom")
# 
# ggplot(unpaid_sec_job) +
#   geom_bar(aes(x = sex, fill = higher_ed), position = "fill") +
#   theme_minimal() +
#   theme(legend.position = "bottom")
# 
# ggplot(unpaid_pri_job) +
#   geom_bar(aes(x = sex, fill = higher_ed), position = "dodge") +
#   theme_minimal() +
#   theme(legend.position = "bottom")
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# ggplot(personas1) +
#   geom_bar(aes(x = age, fill = indigenous), position = "fill") +
#   theme_minimal() +
#   scale_x_continuous(breaks = c(7, 18, 90))
# 
# ggplot(personas) +
#   geom_bar(aes(x = age, fill = indigenous), position = "fill") +
#   theme_minimal() +
#   scale_x_continuous(breaks = c(7, 18, 90))
# 
# 
# 
# # Processed
# processed <- read_excel("../EH_2018/EH_2018_processed.xlsx")
# processed1 <- processed %>%
#   filter(is_employed & pinc_total == 0)
# 
# ggplot(processed) +
#   geom_bar(aes(INE_sector, fill = sex), position = "dodge") +
#   theme_minimal() +
#   coord_flip()
# 
# ggplot(processed1) +
#   geom_bar(aes(x = INE_sector, fill = sex), position = "dodge") +
#   theme_minimal() +
#   coord_flip()
