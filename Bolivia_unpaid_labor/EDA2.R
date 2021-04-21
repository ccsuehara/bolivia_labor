
##Employment status graphs##
color1 <- "#DDCC77"
color2 <- "#88CCEE"
color3 <- "#44AA99"
color4 <- "#117733"
color5 <- "#332288"
color6 <- "#CC6677"
color7 <- "#AA4499"
color8 <- "#882255"
color9 <- "#e6e6e6" # grey10
color_pal <- c(color1, color2, color3, color4, color5, color6, color7, color8, color9)

##Making dataframes for working well with each one


educ_yrs <- c(0, 0, #11, 12
             0, #13
             6,11, #21 22
             11, 6, 11, #23, 31, 32,
             6, 11,  #41, 42
             6, 11,  #51, 52
             11,6,  #61, 62
             11,6,  #63, 64
             11, #65
             16, 16, 16, #71, 72, 73
             16, 16, 16, #74, 75, 76
             16, 16, 16, # 77, 79, 80
             16) #81

educ_list <- sort(unique(personas$edu))


personas <- personas %>% mutate(yrs_educ = plyr::mapvalues(edu, educ_list, educ_yrs)) %>% 
  mutate(why_not_work = s06a_10)
  

adults <- adults %>% mutate(yrs_educ = plyr::mapvalues(edu, educ_list, educ_yrs)) %>% 
  mutate(why_not_work = s06a_10)


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
         want_work_more, avail_work_more, union_member, why_not_in_school, why_not_work, factor)  %>% filter(age %in% 14:30)


ages_neet <- personas %>% filter(age %in% 14:30) %>%
  mutate(neet_cat = case_when(emp_status == "Inactive" & is_student == "No" ~ "NEET",
                                 emp_status == "Inactive" & is_student == "Yes" ~ "Only studies",
                                 emp_status != "Inactive" & is_student == "Yes" ~ "Works and studies",
                                 emp_status != "Inactive" & is_student == "No" ~ "Only works",
                                 ))


employed_gender <- employed %>%
  group_by(sex) %>%
  summarise(total_pop = sum(factor))

inactive_gender <- inactive %>%
  group_by(sex) %>%
  summarise(total_pop = sum(factor))

unemp_gender <- unemployed %>%
  group_by(sex) %>%
  summarise(total_pop = sum(factor)) 


emp_per <- adults %>% filter(!is.na(emp_status)) %>% group_by(emp_status, sex) %>% summarize(sum_peep = sum(factor, na.rm = T)) %>%
  mutate(sum_peep = round(sum_peep/ 20000,0))

#Defining functions here!!! -------------

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


area_chart_sex <- function(df) {
ggplot(df) +
  geom_bar(aes(age, fill = education), position = position_fill(reverse = TRUE)) +
  scale_fill_manual(values = c(color1, color2, color3, color4)) +
  facet_wrap(vars(sex), labeller = labeller(sex = c("1.Hombre" = "men", "2.Mujer" = "women"))) +
  theme_minimal() +
  theme(legend.position = "bottom", legend.title = element_blank()) +
  ylab("proportion")
}


emp_per <- adults %>% filter(!is.na(emp_status)) %>% group_by(emp_status, sex) %>% summarize(sum_peep = sum(factor, na.rm = T)) %>%
  mutate(sum_peep = round(sum_peep/ 20000,0))


waffl_work <- function(df) {
  ggplot(df, aes(fill = emp_status, values = sum_peep)) +
    geom_waffle(color = "white", size = .25, n_rows = 10, flip = TRUE) +
    facet_wrap(~sex, nrow = 1, strip.position = "bottom") +
    scale_x_discrete() + 
    scale_y_continuous(labels = function(x) x * 10, # make this multiplyer the same as n_rows
                       expand = c(0,0)) +
    scale_fill_manual(
      name = NULL,
      values = c(color3, color7, color4)
    ) +
    coord_equal() +
    labs(
      x = "Gender",
      y = "Adults aged 25-60 (1 tile = 20k)"
    ) +
    theme_minimal() +
    theme(panel.grid = element_blank(), axis.ticks.y = element_line()) +
    guides(fill = guide_legend(reverse = FALSE)) 
}


######## Working population statistics ########

## graph for hours of work

hours_worked_graph <- function(df) {
ggplot(df %>% filter(emp_status == "Employed")) +
  geom_jitter(aes(x = age, y = tot_work_week_hr, color = sex), alpha = 0.05) +
  geom_line(data = df %>% filter(emp_status == "Employed") %>% group_by(age, sex) %>% summarize(mean = mean(tot_work_week_hr, na.rm = T)),
            aes(x = age, y = mean, color = sex), size = 1) +
  geom_point(data = df %>% filter(emp_status == "Employed") %>% group_by(age, sex) %>% summarize(mean = mean(tot_work_week_hr, na.rm = T)),
             aes(x = age, y = mean, color = sex), size = 2.5) +
  theme_minimal() +
  theme(legend.position = "bottom", legend.title = element_blank(), panel.grid.minor = element_blank()) +
  scale_color_manual(values = c(color1, color2), labels = c("men", "women")) +
  ylab("average hours worked") +
  ylim(5, 60)
}



###### NEET POPULATION STATISTICS ########

why_neet_no_study <- neet %>%
  filter(!is.na(why_not_in_school)) %>%
  mutate(why_not_in_school = case_when(startsWith(why_not_in_school, "14") ~ "reasons not\nlisted in survey",
                                       startsWith(why_not_in_school, "11") ~ "work",
                                       startsWith(why_not_in_school, "2") ~ "illness,\naccident,\ndisability",
                                       startsWith(why_not_in_school, "3") ~ "pregnancy",
                                       startsWith(why_not_in_school, "4") ~ "lack of money",
                                       startsWith(why_not_in_school, "5") ~ "school is\ntoo far",
                                       startsWith(why_not_in_school, "8") ~ "lack of\ninterest",
                                       startsWith(why_not_in_school, "9") ~ "household chores/\nchildcare",
                                       !is.na(why_not_in_school) ~ "everything\nelse"),
         sex = case_when(startsWith(sex, "1") ~ "Men",
                         startsWith(sex, "2") ~ "Women")) %>%
  group_by(why_not_in_school, sex) %>%
  summarize(sum = n())


why_neet_no_work <- neet %>%
  filter(!is.na(why_not_work)) %>%
  mutate(why_not_work = case_when(startsWith(why_not_work, "10") ~ "doesn't neet to work",
                                       startsWith(why_not_work, "11") ~ "household chores/\nchildcare",
                                       startsWith(why_not_work, "9") ~ "illness,\naccident,\ndisability",
                                       startsWith(why_not_work, "13") ~ "reasons not\nlisted in survey",
                                       !is.na(why_not_work) ~ "everything\nelse"),
         sex = case_when(startsWith(sex, "1") ~ "Men",
                         startsWith(sex, "2") ~ "Women")) %>%
  group_by(why_not_work, sex) %>%
  summarize(sum = n())


plot_bars_neet <- function(df) {
ggplot(df) +
  geom_col(aes(x = sum, y = why_not_work, fill = sex), position = "dodge", width = 0.5) +
  theme_minimal() +
  theme(legend.position = "bottom", legend.title = element_blank(), panel.grid.minor = element_blank()) +
  scale_fill_manual(values = c(color1, color2), labels = c("Men", "Women")) +
  ylab("") + xlab("population")

}

#plot_bars_neet(why_neet_no_work)


plot_bars_neet_study <- function(df) {
  ggplot(df) +
    geom_col(aes(x = sum, y = why_not_in_school, fill = sex), position = "dodge", width = 0.5) +
    theme_minimal() +
    theme(legend.position = "bottom", legend.title = element_blank(), panel.grid.minor = element_blank()) +
    scale_fill_manual(values = c(color1, color2), labels = c("Men", "Women")) +
    ylab("") + xlab("population")
  
}

#plot_bars_neet_study(why_neet_no_study)

#together_g <- ggarrange(plot_bars_neet(why_neet_no_work), plot_bars_neet_study(why_neet_no_study))

area_neet_cat_sex <- function(df) {
  ggplot(df) +
    geom_bar(aes(age, fill = neet_cat), position = position_fill(reverse = TRUE)) +
    scale_fill_manual(values = c(color1, color2, color3, color4)) +
    facet_wrap(vars(sex), labeller = labeller(sex = c("1.Hombre" = "men", "2.Mujer" = "women"))) +
    theme_minimal() +
    theme(legend.position = "bottom", legend.title = element_blank()) +
    ylab("proportion")
}


#area_neet_cat_sex(ages_neet)



neets_rfplot <- read_csv("data/neets_rf.csv") %>%
  ggplot() +
  geom_segment(aes(x = 0, y = reorder(name, MeanDecreaseAccuracy), xend = MeanDecreaseAccuracy, yend = reorder(name, MeanDecreaseAccuracy)), color = "grey") +
  geom_point(aes(MeanDecreaseAccuracy, name), color = color1, size = 3) +
  theme_minimal() +
  theme(panel.grid.major.y = element_blank(), panel.grid.minor = element_blank(), axis.text.x = element_blank()) +
  scale_x_continuous(position = "top", breaks = c(0)) +
  xlab("variable importance") + ylab("")

emp_rfplot <- read_csv("data/adults_rf.csv") %>%
  ggplot() +
  geom_segment(aes(x = 0, y = reorder(name, MeanDecreaseAccuracy), xend = MeanDecreaseAccuracy, yend = reorder(name, MeanDecreaseAccuracy)), color = "grey") +
  geom_point(aes(MeanDecreaseAccuracy, name), color = color1, size = 3) +
  theme_minimal() +
  theme(panel.grid.major.y = element_blank(), panel.grid.minor = element_blank(), axis.text.x = element_blank()) +
  scale_x_continuous(position = "top", breaks = c(0)) +
  xlab("variable importance") + ylab("")


neets_waff <- neet  %>% group_by(sex) %>% summarize(sum_peep = sum(factor, na.rm = T)) %>%
  mutate(sum_peep = round(sum_peep/ 1000,0))



waffl_neet <- function(df) {
  ggplot(df, aes(fill = sex, values = sum_peep)) +
    geom_waffle(color = "white", size = .25, n_rows = 20) +
    scale_x_discrete() + 
    scale_y_continuous(labels = function(x) x * 20, # make this multiplyer the same as n_rows
                       expand = c(0,0)) +
    scale_fill_manual(values = c(color1, color2), labels = c("Men", "Women")) +
    #coord_equal() +
    labs(
      y = "Aged 14-30 (1 tile = 1k)"
    ) +
    theme_minimal() 
  # +
  #   theme(panel.grid = element_blank(), axis.ticks.y = element_line()) +
  #   guides(fill = guide_legend(reverse = TRUE)) 
}

#waffl_neet(neets_waff)




