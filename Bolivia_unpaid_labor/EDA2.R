employed <- adults %>% filter(emp_status == "Employed") %>%
  select(folio, nro, area, sex, age, marital, literate, num_literate, indigenous, indigenous_id,
         edu, any_ed, higher_ed, in_school,
         chronic_disease_1, disability, pregnant, num_alive_child,
         manual_labor,
         cellphone, internet_use, internet_use_where_1, internet_use_where_2,
         primary_job, work_type, primary_salary, primary_salary_freq, primary_nonsalaried_income, primary_nonsalaried_income_freq,
         sec_job, sec_employer_industry, sec_work_type, sec_salary, sec_salary_freq, sec_nonsalaried_income, sec_nonsalaried_income_freq,
         want_work_more, avail_work_more, union_member, factor, education)

unemployed <-  adults %>% filter(emp_status == "Unemployed") %>% 
  select(folio, nro, area, sex, age, marital, literate, num_literate, indigenous, indigenous_id,
         edu, any_ed, higher_ed, in_school,
         chronic_disease_1, disability, pregnant, num_alive_child,
         manual_labor,
         cellphone, internet_use, internet_use_where_1, internet_use_where_2,
         primary_job, work_type, primary_salary, primary_salary_freq, primary_nonsalaried_income, primary_nonsalaried_income_freq,
         sec_job, sec_employer_industry, sec_work_type, sec_salary, sec_salary_freq, sec_nonsalaried_income, sec_nonsalaried_income_freq,
         want_work_more, avail_work_more, union_member, factor, education)

inactive <- adults %>% filter(emp_status == "Inactive") %>% 
  select(folio, nro, area, sex, age, marital, literate, num_literate, indigenous, indigenous_id,
         edu, any_ed, higher_ed, in_school,
         chronic_disease_1, disability, pregnant, num_alive_child,
         manual_labor,
         cellphone, internet_use, internet_use_where_1, internet_use_where_2,
         primary_job, work_type, primary_salary, primary_salary_freq, primary_nonsalaried_income, primary_nonsalaried_income_freq,
         sec_job, sec_employer_industry, sec_work_type, sec_salary, sec_salary_freq, sec_nonsalaried_income, sec_nonsalaried_income_freq,
         want_work_more, avail_work_more, union_member, factor, education)

neet <- personas %>% filter(emp_status == "Inactive" & is_student == "No") %>%
  select(folio, nro, area, sex, age, marital, literate, num_literate, indigenous, indigenous_id,
         edu, any_ed, higher_ed, in_school,
         chronic_disease_1, disability, pregnant, num_alive_child,
         manual_labor,
         cellphone, internet_use, internet_use_where_1, internet_use_where_2,
         primary_job, work_type, primary_salary, primary_salary_freq, primary_nonsalaried_income, primary_nonsalaried_income_freq,
         sec_job, sec_employer_industry, sec_work_type, sec_salary, sec_salary_freq, sec_nonsalaried_income, sec_nonsalaried_income_freq,
         want_work_more, avail_work_more, union_member)  %>% filter(age %in% 18:45)


employed_gender <- employed %>%
  group_by(sex) %>%
  summarise(total_pop = sum(factor))

inactive_gender <- inactive %>%
  group_by(sex) %>%
  summarise(total_pop = sum(factor))

unemp_gender <- unemployed %>%
  group_by(sex) %>%
  summarise(total_pop = sum(factor)) 


wfl_plot <- function(df,people) {
  waffle(df$total_pop/people, rows=4, size=0.5, 
         colors=c("#DDCC77", "#88CCEE"),
         xlab=paste0("1 sq. = ", as.integer(people/1000), " k people"))
}

ridge_educ <- function(df) {
df %>%
  mutate(educFct = fct_rev(as.factor(education))) %>%
  ggplot(aes(y = educFct)) +
  geom_density_ridges(
    aes(x = age, fill = paste(educFct, sex)), 
    alpha = .8, color = "white", from = 25, to = 60
  ) +
  scale_y_discrete(expand = c(0, 0)) +
  scale_x_continuous(expand = c(0, 0)) +
  scale_fill_cyclical(
    breaks = c("Less than Primary 1.Hombre", "Less than Primary 2.Mujer"),
    labels = c(`Less than Primary 1.Hombre` = "male", `Less than Primary 2.Mujer` = "female"),
    values = c("#DDCC77", "#88CCEE")
  ) +
  coord_cartesian(clip = "off") +
  theme_ridges(grid = TRUE) +
  theme(axis.text.y = element_text(angle = 70, hjust = 1, vjust = 0.5))
}
  

ridge_indigen <- function(df) {
  df %>% filter(indigenous!= "3. No soy boliviana o boliviano") %>%
    mutate(educFct = fct_rev(as.factor(indigenous))) %>%
    ggplot(aes(y = educFct)) +
    geom_density_ridges(
      aes(x = age, fill = paste(educFct, sex)), 
      alpha = .8, color = "white", from = 25, to = 60
    ) +
    scale_y_discrete(expand = c(0, 0)) +
    scale_x_continuous(expand = c(0, 0)) +
    scale_fill_cyclical(
      breaks = c("Pertenece 1.Hombre", "Pertenece 2.Mujer"),
      labels = c(`Pertenece 1.Hombre` = "male", `Pertenece 2.Mujer` = "female"),
      values = c("#DDCC77", "#88CCEE"),
      name = "Gender", guide = "legend"
    ) +
    coord_cartesian(clip = "off") +
    theme_ridges(grid = TRUE)
}


ridge_urban <- function(df) {
  df %>%
    mutate(educFct = fct_rev(as.factor(area))) %>%
    ggplot(aes(y = educFct)) +
    geom_density_ridges(
      aes(x = age, fill = paste(educFct, sex)), 
      alpha = .8, color = "white", from = 25, to = 60
    ) +
    scale_y_discrete(expand = c(0, 0)) +
    scale_x_continuous(expand = c(0, 0)) +
    scale_fill_cyclical(
      breaks = c("Urbana 1.Hombre", "Urbana 2.Mujer"),
      labels = c(`Urbana 1.Hombre` = "male", `Urbana 2.Mujer` = "female"),
      values = c("#DDCC77", "#88CCEE")
    ) +
    coord_cartesian(clip = "off") +
    theme_ridges(grid = TRUE)
}


age_lfp <- function(df) {
  ggplot(df) +
    geom_density(aes(x = age, fill = sex), position = "dodge", width = 0.5, alpha = 0.5) +
    theme_minimal() +
    theme(legend.position = "bottom") +
    scale_fill_manual(values=c("#DDCC77", "#88CCEE"))
}