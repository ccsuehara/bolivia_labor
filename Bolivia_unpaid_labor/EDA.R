library(tidyverse)
library(readxl)
library(ggridges)
library(quantmod)

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

cur_conv <- function(var) {
  bob <- "BOB=X"
  case_when(startsWith(var, "1") ~ 1,
            startsWith(var, "2") ~ getQuote(paste0("EUR", bob))$Last,
            startsWith(var, "3") ~ getQuote(paste0("USD", bob))$Last,
            startsWith(var, "4") ~ getQuote(paste0("ARS", bob))$Last,
            startsWith(var, "5") ~ getQuote(paste0("BRL", bob))$Last,
            startsWith(var, "6") ~ getQuote(paste0("CLP", bob))$Last,
            startsWith(var, "PESOS") ~ getQuote(paste0("MXN", bob))$Last,
            startsWith(var, "SOL") ~ getQuote(paste0("PEN", bob))$Last)
}

personas0 <- read_excel("../data/EH_2018/EH2018_Personas2.xlsx", guess_max = 37517) %>%
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
         lab_monthly_inc = ifelse(is.na(primary_monthly_inc), NA,
                                  case_when(is.na(sec_monthly_inc) ~ primary_monthly_inc,
                                            !is.na(sec_monthly_inc) ~ primary_monthly_inc + sec_monthly_inc)),
         sp_monthly_inc = ifelse(is.na(renta_dig_inc), pension_inc + bene_inc + disability_inc + wid_orph_inc,
                                 pension_inc + bene_inc + disability_inc + wid_orph_inc + renta_dig_inc),
         extra_monthly_inc = interest_inc + rent_inc + other_monthly_inc +
                             (ag_prop_rent_inc + biz_prof_inc + equip_rent_inc +
                              severance_inc + insurance_inc + antichretic_inc + other_extra_inc)/12,
         mutate(across(ends_with("inc_freq"), income_freq)),
         intl_remit_freq = income_freq(intl_remit_freq),
         intl_remit_cur = ifelse(startsWith(intl_remit_cur, "7"), s07c_08e, intl_remit_cur),
         dom_trans_monthly_inc = ifelse(is.na(fam_trans_inc_freq), 0, fam_trans_inc * fam_trans_inc_freq) +
                                 ifelse(is.na(dom_remit_inc_freq), 0, dom_remit_inc * dom_remit_inc_freq) +
                                 ifelse(is.na(gift_inc_freq), 0, gift_inc * gift_inc_freq) +
                                 ifelse(is.na(other_cash_trans_inc_freq), 0, other_cash_trans_inc * other_cash_trans_inc_freq) +
                                 ifelse(is.na(other_in_kind_trans_inc_freq), 0, other_in_kind_trans_inc * other_in_kind_trans_inc_freq),
         intl_remit_cur = cur_conv(intl_remit_cur),
         intl_remit_inc = intl_remit_amt * intl_remit_cur,
         intl_remit_monthly_inc = intl_remit_inc * intl_remit_freq,
         nonlab_monthly_inc = ifelse(is.na(sp_monthly_inc), 0, sp_monthly_inc) + ifelse(is.na(extra_monthly_inc), 0, extra_monthly_inc) +
           ifelse(is.na(dom_trans_monthly_inc), 0, dom_trans_monthly_inc) + ifelse(is.na(intl_remit_monthly_inc), 0, intl_remit_monthly_inc),
         tot_monthly_inc = ifelse(is.na(lab_monthly_inc), 0, lab_monthly_inc) + nonlab_monthly_inc)

# Have ever had children
# moms <- personas %>%
#   filter(!is.na(num_child) & num_child != 0)
# 
# moms_sum <- moms %>%
#   mutate(num_alive_child = ifelse(num_alive_child > 7, "8+", num_alive_child)) %>%
#   group_by(num_alive_child) %>%
#   summarize(mean_inc = mean(hh_lab_inc), count = n())
# 
# ggplot(moms_sum) +
#   geom_col(aes(x = num_alive_child, y = mean_inc))
# 
# # Moms with at least 1 dead child
# moms1 <- personas %>%
#   filter(!is.na(num_child) & num_child != 0) %>%
#   mutate(dead_child = num_child - num_alive_child) %>%
#   filter(dead_child > 0)
# 
# ggplot(moms) +
#   geom_jitter(aes(x = num_alive_child, y = hh_lab_inc), alpha = 0.1) +
#   theme_minimal()
# 
# ggplot(moms1) +
#   geom_jitter(aes(x = num_alive_child, y = hh_lab_inc), alpha = 0.1) +
#   theme_minimal()

hh_inc_df <- personas0 %>%
  group_by(folio) %>%
  summarize(hh_lab_inc = sum(lab_monthly_inc, na.rm = T),
            hh_hr = sum(tot_work_week_hr, na.rm = T),
            hh_sp_inc = sum(sp_monthly_inc, na.rm = T),
            hh_nonlab_inc = sum(nonlab_monthly_inc, na.rm = T),
            hh_tot_inc = sum(tot_monthly_inc, na.rm = T))
personas <- personas0 %>%
  left_join(hh_inc_df, by = "folio") %>%
  mutate(hh_lab_inc_pct = lab_monthly_inc / hh_lab_inc * 100,
         hh_hr_pct = tot_work_week_hr / hh_hr * 100,
         hh_sp_inc_pct = sp_monthly_inc / hh_sp_inc * 100,
         hh_nonlab_inc_pct = nonlab_monthly_inc / hh_nonlab_inc * 100,
         hh_tot_inc_pct = tot_monthly_inc / hh_tot_inc * 100) %>%
  replace_na(list(hh_lab_inc_pct = 0, hh_hr_pct = 0, hh_sp_inc_pct = 0, hh_nonlab_inc_pct = 0, hh_tot_inc_pct = 0))

# Select age groups
# Children
children <- personas %>%
  filter(age < 18) %>%
  select(folio, nro, depto, area, sex, age, language_1, marital, literate, num_literate, indigenous, indigenous_id,
         edu, any_ed, higher_ed, in_school, why_not_in_school, current_edu, in_attendance, why_absence,
         chronic_disease_1, disability_1, pregnant, num_alive_child,
         manual_labor,
         cellphone, internet_use, internet_use_where_1, internet_use_where_2,
         primary_job, work_type, primary_work_week_hr,
         primary_salary, primary_salary_freq, primary_nonsalaried_income, primary_nonsalaried_income_freq, primary_monthly_inc,
         sec_job, sec_employer_industry, sec_work_type, sec_work_week_hr,
         sec_salary, sec_salary_freq, sec_nonsalaried_income, sec_nonsalaried_income_freq, sec_monthly_inc,
         lab_monthly_inc, tot_work_week_hr, hh_lab_inc, hh_lab_inc_pct, hh_hr, hh_hr_pct, want_work_more, avail_work_more, union_member,
         sp_monthly_inc, extra_monthly_inc, dom_trans_monthly_inc, intl_remit_monthly_inc, nonlab_monthly_inc, tot_monthly_inc,
         hh_sp_inc, hh_sp_inc_pct, hh_nonlab_inc, hh_nonlab_inc_pct, hh_tot_inc, hh_tot_inc_pct) %>%
  mutate(edu_status = case_when(  # children 0-3 are all NA
    startsWith(in_school, "1") & startsWith(in_attendance, "1") ~ "enrolled,\nattending",
    startsWith(in_school, "1") & startsWith(in_attendance, "2") ~ "enrolled,\nnot attending",
    startsWith(in_school, "2") ~ "not\nenrolled"
  ))

hh_hr_area <- ggplot(children %>% filter(!is.na(lab_monthly_inc))) +
  geom_jitter(aes(x = age, y = hh_hr_pct, size = lab_monthly_inc, color = area), alpha = 0.15) +
  geom_line(data = children %>% filter(!is.na(lab_monthly_inc)) %>% group_by(age, area) %>%
              summarize(mean_hr = mean(tot_work_week_hr), mean_inc = mean(lab_monthly_inc), mean_pct = mean(hh_hr_pct)),
            aes(y = mean_pct, x = age, color = area), size = 1) +
  geom_point(data = children %>% filter(!is.na(lab_monthly_inc)) %>% group_by(age, area) %>%
               summarize(mean_hr = mean(tot_work_week_hr), mean_inc = mean(lab_monthly_inc), mean_pct = mean(hh_hr_pct)),
             aes(y = mean_pct, x = age, size = mean_inc, color = area)) +
  theme_minimal() +
  theme(legend.position = "bottom", panel.grid.minor = element_blank()) +
  scale_x_continuous(limits = c(7, 17.8)) +
  scale_color_manual(values = c(color1, color2), labels = c("rural", "urban")) +
  scale_size(range = c(0.1, 30)) +
  labs(y = "share of household work hours (%)", size = "monthly income (BOB)", color = "average")

hh_hr_sex <- ggplot(children %>% filter(!is.na(lab_monthly_inc))) +
  geom_jitter(aes(x = age, y = hh_hr_pct, size = lab_monthly_inc, color = sex), alpha = 0.15) +
  geom_line(data = children %>% filter(!is.na(lab_monthly_inc)) %>% group_by(age, sex) %>% 
              summarize(mean_hr = mean(tot_work_week_hr), mean_inc = mean(lab_monthly_inc), mean_pct = mean(hh_hr_pct)),
            aes(y = mean_pct, x = age, color = sex), size = 1) +
  geom_point(data = children %>% filter(!is.na(lab_monthly_inc)) %>% group_by(age, sex) %>% 
               summarize(mean_hr = mean(tot_work_week_hr), mean_inc = mean(lab_monthly_inc), mean_pct = mean(hh_hr_pct)),
             aes(y = mean_pct, x = age, size = mean_inc, color = sex)) +
  theme_minimal() +
  theme(legend.position = "bottom", panel.grid.minor = element_blank()) +
  scale_x_continuous(limits = c(7, 17.8)) +
  scale_color_manual(values = c(color1, color2), labels = c("boys", "girls")) +
  scale_size(range = c(0.1, 30)) +
  labs(y = "share of household work hours (%)", size = "monthly income (BOB)", color = "average")

hh_lab_inc_area <- ggplot(children %>% filter(!is.na(lab_monthly_inc))) +
  geom_jitter(aes(x = age, y = hh_lab_inc_pct, size = tot_work_week_hr, color = area), alpha = 0.15) +
  geom_line(data = children %>% filter(!is.na(lab_monthly_inc)) %>% group_by(age, area) %>% 
              summarize(mean_hr = mean(tot_work_week_hr), mean_inc = mean(lab_monthly_inc), mean_pct = mean(hh_lab_inc_pct)),
            aes(y = mean_pct, x = age, color = area), size = 1) +
  geom_point(data = children %>% filter(!is.na(lab_monthly_inc)) %>% group_by(age, area) %>% 
               summarize(mean_hr = mean(tot_work_week_hr), mean_inc = mean(lab_monthly_inc), mean_pct = mean(hh_lab_inc_pct)),
             aes(y = mean_pct, x = age, size = mean_hr, color = area)) +
  theme_minimal() +
  theme(legend.position = "bottom", panel.grid.minor = element_blank()) +
  scale_x_continuous(limits = c(7, 17.8)) +
  scale_color_manual(values = c(color1, color2), labels = c("rural", "urban")) +
  scale_size(range = c(0.1, 10)) +
  labs(y = "contribution to household labor income (%)", size = "weekly work hours", color = "average")

hh_lab_inc_sex <- ggplot(children %>% filter(!is.na(lab_monthly_inc))) +
  geom_jitter(aes(x = age, y = hh_lab_inc_pct, size = tot_work_week_hr, color = sex), alpha = 0.15) +
  geom_line(data = children %>% filter(!is.na(lab_monthly_inc)) %>% group_by(age, sex) %>% 
              summarize(mean_hr = mean(tot_work_week_hr), mean_inc = mean(lab_monthly_inc), mean_pct = mean(hh_lab_inc_pct)),
            aes(y = mean_pct, x = age, color = sex), size = 1) +
  geom_point(data = children %>% filter(!is.na(lab_monthly_inc)) %>% group_by(age, sex) %>% 
               summarize(mean_hr = mean(tot_work_week_hr), mean_inc = mean(lab_monthly_inc), mean_pct = mean(hh_lab_inc_pct)),
             aes(y = mean_pct, x = age, size = mean_hr, color = sex)) +
  theme_minimal() +
  theme(legend.position = "bottom", panel.grid.minor = element_blank()) +
  scale_x_continuous(limits = c(7, 17.8)) +
  scale_color_manual(values = c(color1, color2), labels = c("boys", "girls")) +
  scale_size(range = c(0.1, 10)) +
  labs(y = "contribution to household labor income (%)", size = "weekly work hours", color = "average")

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
         lab_monthly_inc, tot_work_week_hr, hh_lab_inc, hh_lab_inc_pct, hh_hr, hh_hr_pct, want_work_more, avail_work_more, union_member,
         sp_monthly_inc, extra_monthly_inc, dom_trans_monthly_inc, intl_remit_monthly_inc, nonlab_monthly_inc, tot_monthly_inc,
         hh_sp_inc, hh_sp_inc_pct, hh_nonlab_inc, hh_nonlab_inc_pct, hh_tot_inc, hh_tot_inc_pct)

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
# ggplot(personas1) +
#   geom_bar(aes(x = age, fill = indigenous), position = "fill") +
#   theme_minimal() +
#   scale_x_continuous(breaks = c(7, 18, 90))
# 
# ggplot(personas) +
#   geom_bar(aes(x = age, fill = indigenous), position = "fill") +
#   theme_minimal() +
#   scale_x_continuous(breaks = c(7, 18, 90))
