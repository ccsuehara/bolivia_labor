library(tidyverse)
library(readxl)
library(ggridges)

color1 <- "#DDCC77"
color2 <- "#88CCEE"

income_freq <- function(var) {
  case_when(
    startsWith(var, "1") ~ 30,
    startsWith(var, "2") ~ 30/7,
    startsWith(var, "3") ~ 2,
    startsWith(var, "4") ~ 1,
    startsWith(var, "5") ~ 1/2,
    startsWith(var, "6") ~ 1/3,
    startsWith(var, "7") ~ 1/6,
    startsWith(var, "8") ~ 1/12
  )
}

personas <- suppressWarnings(read_excel("../data/EH_2018/EH2018_Personas2.xlsx") %>%
  mutate(higher_ed = ifelse(startsWith(edu, "7") | startsWith(edu, "8"), T, F),
         any_ed = ifelse(startsWith(edu, "11"), F, T),
         primary_work_week_hr = (work_day_hr + work_day_min/60) * work_days_week,
         sec_work_week_hr = (sec_work_hr + sec_work_min/60) * sec_work_days_week,
         tot_work_week_hr = ifelse(is.na(sec_work_week_hr), primary_work_week_hr, primary_work_week_hr + sec_work_week_hr),
         primary_salary_freq = income_freq(primary_salary_freq),
         primary_nonsalaried_income_freq = income_freq(primary_nonsalaried_income_freq),
         primary_monthly_inc = ifelse(is.na(primary_job), NA,
                                      case_when(is.na(primary_salary) & is.na(primary_nonsalaried_income) ~ 0,
                                                is.na(primary_salary) ~ primary_nonsalaried_income * primary_nonsalaried_income_freq,
                                                !is.na(primary_salary) ~ primary_salary * primary_salary_freq)),
         sec_salary_freq = income_freq(sec_salary_freq),
         sec_nonsalaried_income_freq = income_freq(sec_nonsalaried_income_freq),
         sec_monthly_inc = ifelse(startsWith(sec_job, "2"), NA,
                                  case_when(is.na(sec_salary) & is.na(sec_nonsalaried_income) ~ 0,
                                            is.na(sec_salary) ~ sec_nonsalaried_income * sec_nonsalaried_income_freq,
                                            !is.na(sec_salary) ~ sec_salary * sec_salary_freq)),
         tot_monthly_inc = ifelse(is.na(primary_monthly_inc), NA,
                                  case_when(is.na(sec_monthly_inc) ~ primary_monthly_inc,
                                            !is.na(sec_monthly_inc) ~ primary_monthly_inc + sec_monthly_inc))))
hh_inc_df <- personas %>%
  group_by(folio) %>%
  summarize(hh_inc = sum(tot_monthly_inc, na.rm = T),
            hh_hr = sum(tot_work_week_hr, na.rm = T))
personas <- personas %>%
  left_join(hh_inc_df, by = "folio") %>%
  mutate(hh_inc_pct = tot_monthly_inc / hh_inc * 100,
         hh_hr_pct = tot_work_week_hr / hh_hr * 100)
# Remaining question: do we want to replace NA values (including those who do not work, not including those who work withotu pay) with 0?
personas$hh_inc_pct[is.na(personas$hh_inc_pct)] <- 0
personas$hh_hr_pct[is.na(personas$hh_hr_pct)] <- 0

# Select age groups
# Children
children <- personas %>%
  filter(age < 18) %>%
  select(folio, nro, area, sex, age, marital, literate, num_literate, indigenous, indigenous_id,
         edu, any_ed, higher_ed, in_school, why_not_in_school, current_edu, in_attendance, why_absence,
         chronic_disease_1, disability_1, pregnant, num_alive_child,
         manual_labor,
         cellphone, internet_use, internet_use_where_1, internet_use_where_2,
         primary_job, work_type, primary_work_week_hr,
         primary_salary, primary_salary_freq, primary_nonsalaried_income, primary_nonsalaried_income_freq, primary_monthly_inc,
         sec_job, sec_employer_industry, sec_work_type, sec_work_week_hr,
         sec_salary, sec_salary_freq, sec_nonsalaried_income, sec_nonsalaried_income_freq, sec_monthly_inc,
         tot_monthly_inc, tot_work_week_hr, hh_inc, hh_inc_pct, hh_hr, hh_hr_pct, want_work_more, avail_work_more, union_member) %>%
  mutate(edu_status = case_when(  # children 0-3 are all NA
    startsWith(in_school, "1") & startsWith(in_attendance, "1") ~ "enrolled,\nattending",
    startsWith(in_school, "1") & startsWith(in_attendance, "2") ~ "enrolled,\nnot attending",
    startsWith(in_school, "2") ~ "not\nenrolled"
  ))

ggplot(children %>% filter(!is.na(primary_job))) +
  geom_jitter(aes(x = age, y = hh_inc_pct, color = sex, size = tot_monthly_inc), alpha = 0.15) +
  scale_size(range = c(0.1, 30)) +
  theme_minimal()

df1 <- children %>% filter(!is.na(primary_job) & startsWith(sex, "2"))
mean(df1$hh_inc_pct)

ggplot(children %>% filter(!is.na(tot_monthly_inc))) +
  geom_jitter(aes(x = age, y = hh_inc_pct, size = tot_work_week_hr, color = sex), alpha = 0.15) +
  geom_line(data = children %>% filter(!is.na(tot_monthly_inc)) %>% group_by(age, sex) %>% summarize(mean_hr = mean(tot_work_week_hr), mean_inc = mean(tot_monthly_inc), mean_pct = mean(hh_inc_pct)),
            aes(y = mean_pct, x = age, color = sex), size = 1) +
  geom_point(data = children %>% filter(!is.na(tot_monthly_inc)) %>% group_by(age, sex) %>% summarize(mean_hr = mean(tot_work_week_hr), mean_inc = mean(tot_monthly_inc), mean_pct = mean(hh_inc_pct)),
             aes(y = mean_pct, x = age, size = mean_hr, color = sex)) +
  theme_minimal() +
  theme(legend.position = "bottom", panel.grid.major.y = element_blank(), panel.grid.minor = element_blank()) +
  scale_x_continuous(limits = c(7, 17.8)) +
  scale_color_manual(values = c(color1, color2), labels = c("boys", "girls")) +
  scale_size(range = c(0.1, 10)) +
  labs(y = "contribution to household income (%)", size = "weekly work hours", color = "average value")

hh_hr_area <- ggplot(children %>% filter(!is.na(tot_monthly_inc))) +
  geom_jitter(aes(x = age, y = hh_hr_pct, size = tot_monthly_inc, color = area), alpha = 0.15) +
  geom_line(data = children %>% filter(!is.na(tot_monthly_inc)) %>% group_by(age, area) %>%
              summarize(mean_hr = mean(tot_work_week_hr), mean_inc = mean(tot_monthly_inc), mean_pct = mean(hh_hr_pct)),
            aes(y = mean_pct, x = age, color = area), size = 1) +
  geom_point(data = children %>% filter(!is.na(tot_monthly_inc)) %>% group_by(age, area) %>%
               summarize(mean_hr = mean(tot_work_week_hr), mean_inc = mean(tot_monthly_inc), mean_pct = mean(hh_hr_pct)),
             aes(y = mean_pct, x = age, size = mean_inc, color = area)) +
  theme_minimal() +
  theme(legend.position = "bottom", panel.grid.minor = element_blank()) +
  scale_x_continuous(limits = c(7, 17.8)) +
  scale_color_manual(values = c(color1, color2), labels = c("rural", "urban")) +
  scale_size(range = c(0.1, 30)) +
  labs(y = "share of household work hours (%)", size = "monthly income", color = "average")

hh_hr_sex <- ggplot(children %>% filter(!is.na(tot_monthly_inc))) +
  geom_jitter(aes(x = age, y = hh_hr_pct, size = tot_monthly_inc, color = sex), alpha = 0.15) +
  geom_line(data = children %>% filter(!is.na(tot_monthly_inc)) %>% group_by(age, sex) %>% 
              summarize(mean_hr = mean(tot_work_week_hr), mean_inc = mean(tot_monthly_inc), mean_pct = mean(hh_hr_pct)),
            aes(y = mean_pct, x = age, color = sex), size = 1) +
  geom_point(data = children %>% filter(!is.na(tot_monthly_inc)) %>% group_by(age, sex) %>% 
               summarize(mean_hr = mean(tot_work_week_hr), mean_inc = mean(tot_monthly_inc), mean_pct = mean(hh_hr_pct)),
             aes(y = mean_pct, x = age, size = mean_inc, color = sex)) +
  theme_minimal() +
  theme(legend.position = "bottom", panel.grid.minor = element_blank()) +
  scale_x_continuous(limits = c(7, 17.8)) +
  scale_color_manual(values = c(color1, color2), labels = c("boys", "girls")) +
  scale_size(range = c(0.1, 30)) +
  labs(y = "share of household work hours (%)", size = "monthly income", color = "average")

hh_inc_area <- ggplot(children %>% filter(!is.na(tot_monthly_inc))) +
  geom_jitter(aes(x = age, y = hh_inc_pct, size = tot_work_week_hr, color = area), alpha = 0.15) +
  geom_line(data = children %>% filter(!is.na(tot_monthly_inc)) %>% group_by(age, area) %>% 
              summarize(mean_hr = mean(tot_work_week_hr), mean_inc = mean(tot_monthly_inc), mean_pct = mean(hh_inc_pct)),
            aes(y = mean_pct, x = age, color = area), size = 1) +
  geom_point(data = children %>% filter(!is.na(tot_monthly_inc)) %>% group_by(age, area) %>% 
               summarize(mean_hr = mean(tot_work_week_hr), mean_inc = mean(tot_monthly_inc), mean_pct = mean(hh_inc_pct)),
             aes(y = mean_pct, x = age, size = mean_hr, color = area)) +
  theme_minimal() +
  theme(legend.position = "bottom", panel.grid.minor = element_blank()) +
  scale_x_continuous(limits = c(7, 17.8)) +
  scale_color_manual(values = c(color1, color2), labels = c("rural", "urban")) +
  scale_size(range = c(0.1, 10)) +
  labs(y = "contribution to household income (%)", size = "weekly work hours", color = "average")

hh_inc_sex <- ggplot(children %>% filter(!is.na(tot_monthly_inc))) +
  geom_jitter(aes(x = age, y = hh_inc_pct, size = tot_work_week_hr, color = sex), alpha = 0.15) +
  geom_line(data = children %>% filter(!is.na(tot_monthly_inc)) %>% group_by(age, sex) %>% 
              summarize(mean_hr = mean(tot_work_week_hr), mean_inc = mean(tot_monthly_inc), mean_pct = mean(hh_inc_pct)),
            aes(y = mean_pct, x = age, color = sex), size = 1) +
  geom_point(data = children %>% filter(!is.na(tot_monthly_inc)) %>% group_by(age, sex) %>% 
               summarize(mean_hr = mean(tot_work_week_hr), mean_inc = mean(tot_monthly_inc), mean_pct = mean(hh_inc_pct)),
             aes(y = mean_pct, x = age, size = mean_hr, color = sex)) +
  theme_minimal() +
  theme(legend.position = "bottom", panel.grid.minor = element_blank()) +
  scale_x_continuous(limits = c(7, 17.8)) +
  scale_color_manual(values = c(color1, color2), labels = c("boys", "girls")) +
  scale_size(range = c(0.1, 10)) +
  labs(y = "contribution to household income (%)", size = "weekly work hours", color = "average")

# Youth
# Adults
# Older adults

# Select people with at least 1 job--------------
with_job <- personas %>%
  filter(!is.na(primary_job)) %>%
  select(folio, nro, depto, area, sex, age, language_1, marital, literate, num_literate, indigenous, indigenous_id,
         edu, any_ed, higher_ed, in_school, why_not_in_school, current_edu, in_attendance, why_absence,
         chronic_disease_1, disability_1, pregnant, num_alive_child,
         manual_labor,
         cellphone, internet_use, internet_use_where_1, internet_use_where_2,
         primary_job, work_type, primary_work_week_hr,
         primary_salary, primary_salary_freq, primary_nonsalaried_income, primary_nonsalaried_income_freq, primary_monthly_inc,
         sec_job, sec_employer_industry, sec_work_type, sec_work_week_hr,
         sec_salary, sec_salary_freq, sec_nonsalaried_income, sec_nonsalaried_income_freq, sec_monthly_inc,
         tot_monthly_inc, tot_work_week_hr, want_work_more, avail_work_more, union_member)

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
  filter(age < 15)


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
