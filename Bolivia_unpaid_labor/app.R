library(shiny)
library(shinydashboard)
library(shinythemes)
library(tidyverse)
library(shinyWidgets)
library(plotly)
library(waffle)
library(ggridges)
library(treemap)
library(d3treeR)
library(wordcloud2)
library(scales)
library(StatMeasures)

# change folder path
# setwd("/Users/csolisu/Documents/Carla/chamba/shared_Bolivia/Bolivia_unpaid_labor")

source("EDA.R")
source("EDA2.R")

c1 <- "General population (2018)"
c2 <- "People with at least 1 job, paid or unpaid"
c3 <- "People with at least 1 unpaid job"
c4 <- "People with a paid primary job and an unpaid secondary job"

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
filler <- "Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Eget felis eget nunc lobortis. Viverra nam libero justo laoreet sit amet cursus sit amet. Gravida quis blandit turpis cursus in hac habitasse platea. Id interdum velit laoreet id donec ultrices tincidunt. Diam ut venenatis tellus in metus. Donec adipiscing tristique risus nec feugiat in fermentum posuere urna. Penatibus et magnis dis parturient montes nascetur. Orci a scelerisque purus semper. Nisi vitae suscipit tellus mauris a diam maecenas sed enim. Leo vel fringilla est ullamcorper eget nulla facilisi etiam. Sed arcu non odio euismod. Turpis egestas maecenas pharetra convallis posuere morbi. At volutpat diam ut venenatis tellus in metus vulputate. Morbi tincidunt ornare massa eget. Enim sit amet venenatis urna cursus eget nunc."
names_m <- c("Juan", "Jose", "Luis", "Carlos", "Mario", "Jorge", "Victor", "Miguel", "Pedro", "Antonio", "Fernando", "Roberto", "Felix", "Julio")
names_w <- c("Maria", "Juana", "Ana", "Martha", "Carmen", "Rosa", "Julia", "Elizabeth", "Cristina", "Lidia", "Patricia", "Sonia", "Isabel", "Victoria")

lf1 <- "Employed Population"
lf2 <- "Unemployed Population"
lf3 <- "Inactive Population"

wgt <- 258.651824951171
# wgt <- 1

# UI -------------------------------
ui <- fluidPage(
  theme = shinytheme("sandstone"),
  tags$style(type = "text/css",
             "h1, h2, h3, h4 { text-align: center; }",
             "p { text-align: center; color: grey; }",
             "hr { margin-top: 2em; margin-bottom: 2em; }",
             "#children_madlib { color: white; }",
             "#landing { align-self: center; }"),
  
  navbarPage("Working while female in Bolivia",
             id = "main",
             collapsible = T,
             
             # Tab panel: home -----------------
             tabPanel("Home",
                      fluidRow(width = 12,
                               imageOutput("landing",
                                           width = "99%",
                                           click = "landing_cl"))),
             
             # Tab panel: children under 18 --------------------
             tabPanel("Children under 18",
                      value = "children",
                      
                      fluidRow(style = 'background-image: url("children.jpg"); background-size: cover;',
                               column(12, style = "padding: 80px 50px;",
                                      fluidRow(
                                        column(8,
                                               offset = 2,
                                               h3(textOutput("children_madlib")))))),
                      hr(),
                      fluidRow(
                        column(6,
                               offset = 3,
                               textOutput("children_intro"),
                               h3("Overview: children in school and at work"),
                               plotOutput("children_1"),
                               
                               # h3("Children population overview"),
                               # fluidRow(
                               #   column(6,
                               #          plotOutput("children_overview1")),
                               #   column(6,
                               #          plotOutput("children_overview2"))
                               # ),
                               
                               # hr(),
                               # textOutput("children_t1"),
                               # h3("School enrollment and attendance"),
                               # fluidRow(
                               #   column(6,
                               #          plotOutput("children_edu1")),
                               #   column(6,
                               #          plotOutput("children_edu2"))
                               # ),
                               
                               hr(),
                               textOutput("children_t2"),
                               h3("Reasons for not enrolling in school"),
                               d3tree2Output("children_edu3"),
                               plotOutput("children_edu4"),
                               hr(),
                               
                               # textOutput("children_t3"),
                               # h3("Educational attainment"),
                               # fluidRow(
                               #   column(6,
                               #          plotOutput("children_edu5")),
                               #   column(6,
                               #          plotOutput("children_edu6"))
                               # ),
                               # hr(),
                               
                               textOutput("children_t5"),
                               p(""),
                               textOutput("children_t8"),
                               p(""),
                               textOutput("children_t9"),
                               p(""),
                               textOutput("children_t10"),
                               p(""),
                               textOutput("children_t11"),
                               h3("Socioeconomic determinants of children's work in relation to the household"),
                               selectInput("ses",
                                           label = "",
                                           choices = c("Sex" = "sex",
                                                       "Area (rural/urban)" = "area",
                                                       "Department" = "depto",
                                                       "Indigenous identity" = "indi",
                                                       "Household income" = "inc")),
                               plotOutput("children_ses1"),
                               plotOutput("children_ses2"),
                               hr(),
                               
                               textOutput("children_t4"),
                               h3("Average work hours and income"),
                               plotOutput("children_lfp1"),
                               plotOutput("children_lfp2")
                               # hr(),
                               # textOutput("children_t5"),
                               # h3("Work hours and income in relation to the household"),
                               # plotOutput("children_lfp3"),
                               # plotOutput("children_lfp4"),
                               # hr(),
                               # textOutput("children_t6"),
                               # plotOutput("children_lfp5"),
                               # plotOutput("children_lfp6"),
                               # hr(),
                               # textOutput("children_t7"),
                               # plotOutput("children_lfp7"),
                               # plotOutput("children_lfp8")
                               
                               # hr(),
                               # textOutput("children_t9"),
                               # h3("Children and indigeneity"),
                               # fluidRow(
                               #   column(6,
                               #          plotOutput("children_indi1")),
                               #   column(6,
                               #          plotOutput("children_indi2"))
                               # ),
                               # hr(),
                               # fluidRow(
                               #   column(6,
                               #          plotOutput("children_indi3")),
                               #   column(6,
                               #          plotOutput("children_indi4"))
                               # )
                               ),
                        column(3,
                               fixedPanel(
                                 actionButton("to_youth", label = "youth >"),
                                 right = 10, bottom = 10
                               ))
                      )),
             
             # Tab panel: youth 18-25 --------------------
             tabPanel("Youth 18-25",
                      value = "youth",
                      
                      fluidRow(
                        column(3,
                               fixedPanel(
                                 actionButton("to_children", label = "< children"),
                                 left = 10, bottom = 10
                               )),
                        
                        column(6,
                               
                               # Youth: intro ----------------------
                               textOutput("youth_intro"),
                               h3("Youth overview"),
                               plotOutput("youth_overview"),
                               hr(),
                               
                               # Youth: education ------------------
                               fluidRow(style = 'background-image: url("education_bw.jpg"); background-size: cover;',
                                        column(12, style = "padding: 80px 50px;",
                                               h3("EDUCATION", style = "color: white;"))),
                               hr(),
                               textOutput("youth_edu_t1"),
                               p(""),
                               h3("Which socioeconomic characteristics affect youths' education the most?"),
                               plotOutput("youth_edu_imp"),
                               hr(),
                               textOutput("youth_edu_marital"),
                               p(""),
                               textOutput("youth_edu_internet"),
                               p(""),
                               textOutput("youth_edu_area"),
                               p(""),
                               textOutput("youth_edu_depto"),
                               p(""),
                               textOutput("youth_edu_indi"),
                               p(""),
                               textOutput("youth_edu_lang"),
                               p(""),
                               selectInput("youth_edu_ses",
                                           label = "What affects youths' education?",
                                           choices = c("Marital status" = "marital",
                                                       "Internet access" = "internet",
                                                       "Area (rural/urban)" = "area",
                                                       "Department" = "depto",
                                                       "Indigenous identity" = "indi",
                                                       "Primary language" = "lang")),
                               plotOutput("youth_edu1"),
                               hr(),
                               textOutput("youth_edu_t2"),
                               h3("Educational attainment"),
                               plotOutput("youth_edu2"),
                               hr(),
                               
                               # Youth: employment -------------------------
                               fluidRow(style = 'background-image: url("employment_bw.jpg"); background-size: cover;',
                                        column(12, style = "padding: 80px 50px;",
                                               h3("EMPLOYMENT", style = "color: white;"))),
                               hr(),
                               textOutput("youth_emp_overview"),
                               fluidRow(
                                 column(6,
                                        h4("Gender ratio of youths who are students"),
                                        plotOutput("youth_edu_sex")),
                                 column(6,
                                        h4("Gender ratio of youths doing paid or unpaid work"),
                                        plotOutput("youth_emp_sex"))
                               ),
                               hr(),
                               
                               textOutput("youth_emp_t1"),
                               p(""),
                               h3("Which socioeconomic characteristics affect youths' employment the most?"),
                               plotOutput("youth_emp_imp"),
                               hr(),
                               textOutput("youth_emp_stu"),
                               p(""),
                               textOutput("youth_emp_edu"),
                               p(""),
                               textOutput("youth_emp_marital"),
                               p(""),
                               textOutput("youth_emp_area"),
                               p(""),
                               selectInput("youth_emp_ses",
                                           label = "What affects youths' employment?",
                                           choices = c("Student status" = "stu",
                                                       "Education" = "edu",
                                                       "Marital status" = "marital",
                                                       "Area (rural/urban)" = "area")),
                               plotOutput("youth_emp1"),
                               hr(),
                               
                               # Youth: income -------------------------------
                               fluidRow(style = 'background-image: url("income_bw.jpg"); background-size: cover;',
                                        column(12, style = "padding: 80px 50px;",
                                               h3("INCOME", style = "color: white; text-shadow: 0px 0px 6px black;"))),
                               hr(),
                               
                               textOutput("youth_inc_t1"),
                               h3("Proportion of unpaid workers among all workers"),
                               plotOutput("youth_inc_paid"),
                               hr(),
                               
                               textOutput("youth_inc_t2"),
                               h3("Average monthly labor income by sex and age"),
                               plotOutput("youth_inc_inc"),
                               p("")),
                        
                        column(3,
                               fixedPanel(
                                 actionButton("to_employment", label = "adults - entering the job market >"),
                                 right = 10, bottom = 10
                               ))
                      )),
             
             # Tab panel: adults 25-60 --------------------
             navbarMenu("Adults 25-60",
                        
                        # Entering the job market -------------------------
                        tabPanel("Entering the job market",
                                 value = "employment",
                                 box(h2("Column overview"),
                                     fluidRow(
                                       column(4,
                                              p(lf1),
                                              h1(textOutput("empl")),
                                              p("people (est.)")),
                                       column(4,
                                              p(lf2),
                                              h1(textOutput("unempl")),
                                              p("people (est.)")),
                                       column(4,
                                              p(lf3),
                                              h1(textOutput("inactiv")),
                                              p("people (est.)"))
                                     ),
                                     width = 12,
                                     collapsible = T),
                                 box(hr(), width = 12),
                                 
                                 box(h2("Age"),
                                     h4(textOutput("age_t_lfp")),
                                     fluidRow(
                                       column(4,
                                              p(lf1),
                                              plotOutput("pop_emp")),
                                       column(4,
                                              p(lf2),
                                              plotOutput("pop_unemp")),
                                       column(4,
                                              p(lf3),
                                              plotOutput("pop_inac"))
                                     ),
                                     width = 12,
                                     collapsible = T),
                                 box(hr(), width = 12),
                                 
                                 box(h2("Proportions"),
                                     h4(textOutput("proportion_exp")),
                                     fluidRow(
                                       column(4,
                                              p(lf1),
                                              plotOutput("waf_emp")),
                                       column(4,
                                              p(lf2),
                                              plotOutput("waf_unemp")),
                                       column(4,
                                              p(lf3),
                                              plotOutput("waf_inac"))
                                     ),
                                     width = 12,
                                     collapsible = T),
                                 box(hr(), width = 12),
                                 box(h2("Rural/ Urban population"),
                                     h4(textOutput("ar_lf")),
                                     fluidRow(
                                       column(4,
                                              p(lf1),
                                              plotOutput("ar_emp")),
                                       column(4,
                                              p(lf2),
                                              plotOutput("ar_unemp")),
                                       column(4,
                                              p(lf3),
                                              plotOutput("ar_inac"))
                                     ),
                                     width = 12,
                                     collapsible = T),
                                 box(hr(), width = 12),
                                 box(h2("School attendance"),
                                     h4(textOutput("sch_lf")),
                                     fluidRow(
                                       column(4,
                                              p(lf1),
                                              plotOutput("ed_emp")),
                                       column(4,
                                              p(lf2),
                                              plotOutput("ed_unemp")),
                                       column(4,
                                              p(lf3),
                                              plotOutput("ed_inac"))
                                     ),
                                     width = 12,
                                     collapsible = T),
                                 box(hr(), width = 12),
                                 box(h2("Indigenous Belonging"),
                                     h4(textOutput("ind_lf")),
                                     fluidRow(
                                       column(4,
                                              p(lf1),
                                              plotOutput("ind_emp")),
                                       column(4,
                                              p(lf2),
                                              plotOutput("ind_unemp")),
                                       column(4,
                                              p(lf3),
                                              plotOutput("ind_inac"))
                                     ),
                                     width = 12,
                                     collapsible = T),
                                 box(hr(), width = 12)),
                        
                        # Paid and unpaid labor ------------------------------
                        tabPanel("Paid and unpaid labor",
                                 value = "pay",
                                 
                                 fluidRow(
                                   column(3,
                                          fixedPanel(
                                            actionButton("to_employment2", label = "< adults - entering the job market"),
                                            left = 10, bottom = 10
                                          )),
                                   
                                   column(6,
                                          
                                          textOutput("pay_intro"),
                                          h3("Paid and unpaid labor throughout the lifetime"),
                                          plotOutput("pay_age"),
                                          hr(),
                                          textOutput("pay_t1"),
                                          h3("Average income by age and sex"),
                                          plotOutput("pay_lab_inc1"),
                                          h3("Average income among paid workers by age and sex"),
                                          plotOutput("pay_lab_inc2"),
                                          hr(),
                                          textOutput("pay_t2"),
                                          h3("Economic contribution of unpaid labor on household level"),
                                          plotOutput("pay_worth"),
                                          hr(),
                                          textOutput("pay_t3"),
                                          h3("Aggregate contribution of unpaid labor per month"),
                                          fluidRow(
                                            column(6,
                                                   p("men"),
                                                   h2(textOutput("pay_worth_tot1")),
                                                   p("bolivianos")),
                                            column(6,
                                                   p("women"),
                                                   h2(textOutput("pay_worth_tot2")),
                                                   p("bolivianos"))
                                          ),
                                          hr(),
                                          textOutput("pay_t4"),
                                          h3("Presence of unpaid workers by household labor income"),
                                          plotOutput("pay_ru1"),
                                          h3("Aggregate contribution of unpaid labor"),
                                          fluidRow(
                                            column(6,
                                                   h2(textOutput("pay_ru2")),
                                                   p("of total rural household productivity")),
                                            column(6,
                                                   h2(textOutput("pay_ru3")),
                                                   p("of total urban household productivity"))
                                          ),
                                          hr(),
                                          textOutput("pay_t5"),
                                          h3("Unpaid workers by department and rural/urban area"),
                                          plotOutput("pay_depto1"),
                                          hr(),
                                          textOutput("pay_t6"),
                                          h3("Unpaid workers by marital status and sex"),
                                          plotOutput("pay_marital1"),
                                          hr(),
                                          textOutput("pay_t7"),
                                          h3("Unpaid labor by cellphone access and household income"),
                                          plotOutput("pay_cell1")
                                   ),
                                   column(3,
                                          fixedPanel(
                                            actionButton("to_neet", label = "adults - neet population >"),
                                            right = 10, bottom = 10
                                          )))),
                        
                        # NEET -------------------------
                        tabPanel("NEET population",
                                 value = "neet",
                                 
                                 fluidRow(
                                   column(3,
                                          fixedPanel(
                                            actionButton("to_pay", label = "< adults - paid and unpaid labor"),
                                            left = 10, bottom = 10
                                          )),
                                   column(6),
                                   column(3,
                                          fixedPanel(
                                            actionButton("to_older", label = "older adults >"),
                                            right = 10, bottom = 10
                                          ))
                                 ))),
             
             # Tab panel: older adults 60+ --------------------
             tabPanel("Older adults 60+",
                      value = "older",
                      
                      fluidRow(
                        column(3,
                               fixedPanel(
                                 actionButton("to_neet2", label = "< adults - neet population"),
                                 left = 10, bottom = 10
                               )),
                        column(6,
                               textOutput("older_t1"),
                               h3("Social protection income among older adults"),
                               plotOutput("older_p1"),
                               hr(),
                               
                               textOutput("older_t2"),
                               h3("Which socioeconomic characteristics affect older adults' work status the most?"),
                               plotOutput("older_p2"),
                               hr(),
                               h2("The impact of select socioeconomic characteristics on older adults' work status"),
                               selectInput("older_job",
                                           label = "",
                                           choices = c("Non-labor income" = "nonlab",
                                                       "Family members' income" = "rest_of_hh",
                                                       "Area" = "area",
                                                       "Sex" = "sex")),
                               plotOutput("older_job_p"),
                               hr(),
                               
                               textOutput("older_t3"),
                               h3("Which socioeconomic characteristics affect whether one does paid or unpaid work?"),
                               plotOutput("older_p3"),
                               hr(),
                               h2("The impact of select socioeconomic characteristics on the nature of one's work"),
                               selectInput("older_pay",
                                           label = "",
                                           choices = c("Sex" = "sex",
                                                       "Family members' income" = "rest_of_hh",
                                                       "Area" = "area",
                                                       "Marital status" = "marital",
                                                       "Household size" = "size")),
                               plotOutput("older_pay_p"),
                               hr(),
                               
                               textOutput("older_t4"),
                               h3("Income distribution among working older adults"),
                               plotOutput("older_p4"),
                               p(""))
                      ))
  )
)

# Server -----------------------------
server <- function(input, output, session) {
  # Tab panel: home --------------------------
  output$landing <- renderImage(list(src = "www/landing_page1.png", width = "100%"), deleteFile = F)
  observeEvent(input$landing_cl, {
    updateNavbarPage(session, "main", selected = {
      if (between(input$landing_cl$x, 161, 421) & between(input$landing_cl$y, 19, 277)) {"children"}
      else if (between(input$landing_cl$x, 380, 647) & between(input$landing_cl$y, 214, 490)) {"youth"}
      else if (between(input$landing_cl$x, 603, 870) & between(input$landing_cl$y, 14, 271)) {"pay"}
      else if (between(input$landing_cl$x, 843, 1118) & between(input$landing_cl$y, 200, 472)) {"older"}
    })
  })
  

  output$pop <- renderText(round(nrow(personas) * wgt, 0))
  output$with_job <- renderText(round(nrow(with_job) * wgt, 0))
  output$unpaid <- renderText(round(nrow(unpaid_job) * wgt, 0))
  output$unpaid_sec <- renderText(round(nrow(unpaid_sec_job) *wgt, 0))

  output$empl <- renderText(round(nrow(employed) * wgt, 0))
  output$unempl <- renderText(round(nrow(unemployed) * wgt, 0))
  output$inactiv <- renderText(round(nrow(inactive) * wgt, 0))


  # Tab panel: children --------------------------
  output$children_madlib <- renderText({
    invalidateLater(10000)
    
    madlib_df <- child_worker[sample(nrow(child_worker), 1), ]
    sex_test <- startsWith(madlib_df$sex, "1")
    madlib_name <- ifelse(sex_test, sample(names_m, 1), sample(names_w, 1))
    madlib_pron1 <- ifelse(sex_test, "He", "She")
    madlib_pron2 <- ifelse(sex_test, "he", "she")
    madlib_pron3 <- ifelse(sex_test, "his", "her")
    madlib_lang <- paste0(substring(madlib_df$language_1, 1, 1), tolower(substring(madlib_df$language_1, 2, nchar(madlib_df$language_1))))
    
    paste(
      madlib_name, "is", round(madlib_df$age, 0), "years old.",
      madlib_pron1, "lives in", ifelse(madlib_df$area == "Rural", "rural", "urban"), madlib_df$depto, "and primarily speaks", paste0(madlib_lang, "."),
      case_when(startsWith(madlib_df$in_school, "1") & startsWith(madlib_df$in_attendance, "1") ~ paste(madlib_pron1, "goes to school every day."),
                      startsWith(madlib_df$in_school, "1") & startsWith(madlib_df$in_attendance, "2") ~ paste(madlib_pron1, "is enrolled in school but is not always able to attend it."),
                      startsWith(madlib_df$in_school, "2") ~ paste(madlib_pron1, "is not going to school.")),
      case_when(!is.na(madlib_df$primary_job) & startsWith(madlib_df$sec_job, "2") ~ paste(madlib_pron1, "works as a", tolower(madlib_df$primary_job), "for", madlib_df$tot_work_week_hr, "hours per week."),
                      !is.na(madlib_df$primary_job) & startsWith(madlib_df$sec_job, "1") ~ paste(madlib_pron1, "mainly works as a", madlib_df$primary_job, "for", madlib_df$primary_work_week_hr, "hours per week, but", madlib_pron2, "also has a second job for another", madlib_df$sec_work_week_hr, "weekly hours."),
                      is.na(madlib_df$primary_job) ~ paste(madlib_pron1, "does not have a job.")),
      ifelse(round(madlib_df$lab_monthly_inc, 0) == 0,
                   paste(madlib_pron1, "does not earn any income from", madlib_pron3, "work."),
                   paste("In total,", madlib_pron2, "makes", round(madlib_df$lab_monthly_inc, 0), "Bolivianos every month."))
    )
  })
  
  output$children_intro <- renderText("In this household survey, children make up about 35% of the sampled population, indicating that children are an important demographic in Bolivia.
                                      It is also worth highlighting that Bolivia has the lowest legal minimum working age, ten, instead of the international consensus of fourteen. This labor law remains controversial today, with some arguing that it destigmatizes child workers' struggle for fair labor, while others casting doubt on the legislation's efficacy and its implications for children's long-term well-being.
                                      Given this reality, it is even more crucial to understand the conditions in which children live and work, as well as the persistent impact of social disparities.")
  # output$children_t1 <- renderText('Bolivia enjoys a high level of school enrollment among children.
  #                                  Even among those in the category of "enrolled, not attending", the vast majority are simply due to school break or recess (i.e. the timing of the survey), not structural barriers.
  #                                  There is no evidence for gender disparity in receiving education.')
  output$children_t2 <- renderText('Bolivia enjoys a high level of school enrollment among children.
                                   However, there is still a notable minority of children who could not go to school, and the survey largely fails to capture the complex reasons.
                                   From the available data, poverty and inaccessibility appear to remain an obstacle for many children.')
  # output$children_t3 <- renderText("TBD")
  output$children_t4 <- renderText("When we look at children individually, there is not a significant difference between boys' and girls' work hours and incomes,
                                   except that boys start earning more than girls on a monthly basis as they approach 18.
                                   Interestingly, their hourly incomes do not differ, which means that teenage boys probably work more hours than girls.
                                   Overall, a large number of boys and girls merely help out with their families' work, rather than seeking outside employment.")
  output$children_t5 <- renderText("More intriguing patterns emerge, however, when we examine children's work as contributions to their households.
                                   Clearly, starting from age 16, boys begin to make a larger contribution to their family's income than girls, but their share of total work hours among all family members remains equal.
                                   This likely means that girls start doing unpaid or family-based labor during adolescence.
                                   Puberty -> devaluation of female labor. Sexualization and devaluation go hand in hand.
                                   In terms of DRM, the data show that men/boys are more likely to have income sources outside their homes and access an additional channel of financial stability.
                                   Yet, in some scenarios, teenage boys are actually the main breadwinners in their families, which denotes heightened vulnerability to social and economic shocks.")
  output$children_t6 <- renderText("Although working children only make up about 3% of the total children population, their characteristics and vulnerabilities should not be overlooked.
                                   The vast majority of them come from lower-income households, many of whom heavily depend on the children as their source of income.")
  output$children_t7 <- renderText("Relationship between children's contribution to household income and household total income (labor and non-labor).")
  output$children_t8 <- renderText("An even more stark contrast exists between children in rural and urban areas.
                                   Children in cities start contributing to their family finances at an earlier age, work more hours, and bring home more money.
                                   Additionally, while rural children work a similar amount of hours throughout their childhood and adolescence, city kids pick up more work as they grow older, especially from age 12.")
  output$children_t9 <- renderText("There are tremendous variations within areas that fall under the rural or urban categories.
                                   While rural Potosi and La Paz have the largest shares of children who are working, it is in rural Tarija where children's school attendance rate severely lags behind the nation.
                                   The impact of gender is by no means uniform, either.
                                   For instance, in places like Pando and Potosi, girls' education is more negatively affected than boys', even though boys are more likely to be working.
                                   Such differences also exist in cities, but the variation tends to be much smaller, since city kids are much less likely to work and more likely to stay in school compared to their rural counterparts.")
  output$children_t10 <- renderText("Between indigenous and non-indigenous children, their work conditions on average do not differ very much, though non-indigenous teenagers tend to earn more than indigenous children of the same age.")
  output$children_t11 <- renderText("Perhaps most surprisingly, household income does not seem to have an outright impact on how many hours children work.
                                    Yet, this does not mean household economic conditions do not matter.
                                    Among households of the lowest income decile, there are many more children who have to work to support their family.
                                    In fact, some children from poorer families have even become the sole breadwinner, whereas such burden does not fall on children from better-off households.")
  
  
  children_1_df <- children %>%
    filter(age > 6) %>%
    group_by(age, sex) %>%
    summarize(tot = n(), in_school = sum(in_school == "1. Si") / tot * 100, job = sum(!is.na(primary_job)) / tot * 100)
  
  output$children_1 <- renderPlot(
    ggplot(children_1_df) +
      geom_line(aes(age, in_school), color = color1, size = 2) +
      geom_text(aes(12, 85, label = "% of surveyed children who are in school"), color = color1) +
      geom_line(aes(age, job), color = color2, size = 2) +
      geom_text(aes(12, 25, label = "% of surveyed children\nwho are working"), color = color2) +
      facet_wrap(vars(sex), labeller = labeller(sex = c("1.Hombre" = "boys", "2.Mujer" = "girls"))) +
      theme_minimal() +
      theme(panel.grid.minor = element_blank()) +
      scale_x_continuous(breaks = seq(7, 17, 2)) +
      ylab("")
  )
  
  why_not_in_school_df <- children %>%
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
           sex = case_when(startsWith(sex, "1") ~ "boys",
                           startsWith(sex, "2") ~ "girls")) %>%
    group_by(why_not_in_school, sex) %>%
    summarize(sum = n())
  
  output$children_edu3 <- renderD3tree2(d3tree2(treemap(why_not_in_school_df,
                                                        index = c("why_not_in_school", "sex"),
                                                        vSize = "sum",
                                                        type = "index",
                                                        palette = color_pal,
                                                        title = "",
                                                        fontsize.labels = c(13, 10),
                                                        fontcolor.labels = c("white"),
                                                        fontface.labels = c(2, 3),
                                                        bg.labels = c("transparent"),
                                                        align.labels = list(c("center", "center"),
                                                                            c("left", "bottom")),
                                                        overlap.labels = 0.5,
                                                        border.col = c("white")),
                                                rootname = "Reasons for not enrolling in school: "))
  
  output$children_edu4 <- renderPlot(
    ggplot(why_not_in_school_df) +
      geom_col(aes(x = sum, y = why_not_in_school, fill = sex), position = "dodge", width = 0.5) +
      theme_minimal() +
      theme(legend.position = "bottom", legend.title = element_blank(), panel.grid.minor = element_blank()) +
      scale_fill_manual(values = c(color1, color2), labels = c("boys", "girls")) +
      ylab("") + xlab("population")
  )
  
  children_var <- reactive(input$ses)
  
  ses1_p <- function(df, var, labels) {
    df2 <- df %>%
      filter(!is.na(lab_monthly_inc)) %>%
      group_by(age, .data[[var]]) %>%
      summarize(mean_hr = mean(tot_work_week_hr), mean_inc = mean(lab_monthly_inc), mean_pct = mean(hh_lab_inc_pct))
    
    ggplot(df2) +
      geom_jitter(data = df %>% filter(!is.na(lab_monthly_inc)),
                  aes(x = age, y = hh_lab_inc_pct, size = tot_work_week_hr, color = .data[[var]]), alpha = 0.15) +
      geom_line(aes(y = mean_pct, x = age, color = .data[[var]]), size = 1) +
      geom_point(aes(y = mean_pct, x = age, size = mean_hr, color = .data[[var]])) +
      geom_text(aes(7, 85, label = "circle size =\nweekly work hours"), color = "grey", hjust = "left") +
      theme_minimal() +
      theme(legend.position = "bottom", panel.grid.minor = element_blank()) +
      scale_x_continuous(limits = c(7, 17.8)) +
      scale_color_manual(values = c(color1, color2), labels = labels) +
      scale_size(range = c(0.1, 10), guide = F) +
      labs(y = "contribution to household labor income (%)", color = "average")
  }
  
  ses2_p <- function(df, var, labels) {
    df2 <- df %>%
      filter(!is.na(lab_monthly_inc)) %>%
      group_by(age, .data[[var]]) %>%
      summarize(mean_hr = mean(tot_work_week_hr), mean_inc = mean(lab_monthly_inc), mean_pct = mean(hh_hr_pct))
    
    ggplot(df2) +
      geom_jitter(data = df %>% filter(!is.na(lab_monthly_inc)),
                  aes(x = age, y = hh_hr_pct, size = lab_monthly_inc, color = .data[[var]]), alpha = 0.15) +
      geom_line(aes(y = mean_pct, x = age, color = .data[[var]]), size = 1) +
      geom_point(aes(y = mean_pct, x = age, size = mean_inc, color = .data[[var]])) +
      geom_text(aes(7, 85, label = "circle size =\nmonthly income (BOB)"), color = "grey", hjust = "left") +
      theme_minimal() +
      theme(legend.position = "bottom", panel.grid.minor = element_blank()) +
      scale_x_continuous(limits = c(7, 17.8)) +
      scale_color_manual(values = c(color1, color2), labels = labels) +
      scale_size(range = c(0.1, 30), guide = F) +
      labs(y = "share of household work hours (%)", color = "average")
  }
  
  children_depto <- children %>%
    filter(age > 6) %>%
    group_by(depto, area, sex) %>%
    summarize(cw = mean(!is.na(lab_monthly_inc)) * 100, tot = n(), school = mean(in_school == "2. No") * 100)
  
  # children_decile <- children %>%
  #   group_by(folio) %>%
  #   summarize(hh_tot_inc = first(hh_tot_inc)) %>%
  #   mutate(decile = StatMeasures::decile(hh_tot_inc)) %>%
  #   right_join(children)
  
  output$children_ses1 <- renderPlot(
    if (children_var() == "sex") {
      ses1_p(children, "sex", c("boys", "girls"))
    } else if (children_var() == "area") {
      ses1_p(children, "area", c("rural", "urban"))
    } else if (children_var() == "indi") {
      ses1_p(children %>% filter(!startsWith(indigenous, "3")), "indigenous", c("indigenous", "not indigenous"))
    } else if (children_var() == "depto") {
      ggplot(children_depto) +
        geom_col(aes(cw, depto, fill = sex), position = "dodge", width = 0.5) +
        facet_wrap(vars(area), labeller = labeller(area = c("Rural" = "rural", "Urbana" = "urban"))) +
        theme_minimal() +
        theme(legend.position = "bottom", panel.grid.minor = element_blank()) +
        scale_fill_manual(values = c(color1, color2), labels = c("boys", "girls")) +
        labs(x = "% children with a job", y = "", fill = "")
    } else if (children_var() == "inc") {
      # children_decile2 <- children_decile %>%
      #   filter(!is.na(lab_monthly_inc)) %>%
      #   group_by(decile, sex) %>%
      #   summarize(mean_hr = mean(tot_work_week_hr), mean_inc = mean(lab_monthly_inc), mean_pct = mean(hh_lab_inc_pct))
      
      ggplot(children %>% filter(!is.na(lab_monthly_inc) & hh_tot_inc != 0)) +
        geom_point(aes(hh_tot_inc + 1, y = hh_lab_inc_pct, size = tot_work_week_hr, color = sex), alpha = 0.15) +
        geom_smooth(aes(hh_tot_inc + 1, hh_lab_inc_pct, color = sex), alpha = 0.1) +
        # geom_line(aes(y = mean_pct, x = decile, color = sex), size = 1) +
        # geom_point(aes(y = mean_pct, x = decile, size = mean_hr, color = sex)) +
        geom_text(aes(120, 75, label = "circle size =\nweekly work hours"), color = "grey", hjust = "left") +
        theme_minimal() +
        theme(legend.position = "bottom", panel.grid.minor = element_blank()) +
        scale_x_continuous(trans = "log10") +
        ylim(-15, 100) +
        scale_color_manual(values = c(color1, color2), labels = c("boys", "girls")) +
        scale_size(range = c(0.1, 10), guide = F) +
        labs(x = "household monthly income (BOB)", y = "contribution to household labor income (%)", color = "average")
    }
  )
  
  output$children_ses2 <- renderPlot(
    if (children_var() == "sex") {
      ses2_p(children, "sex", c("boys", "girls"))
    } else if (children_var() == "area") {
      ses2_p(children, "area", c("rural", "urban"))
    } else if (children_var() == "indi") {
      ses2_p(children %>% filter(!startsWith(indigenous, "3")), "indigenous", c("indigenous", "not indigenous"))
    } else if (children_var() == "depto") {
      ggplot(children_depto) +
        geom_col(aes(school, depto, fill = sex), position = "dodge", width = 0.5) +
        facet_wrap(vars(area), labeller = labeller(area = c("Rural" = "rural", "Urbana" = "urban"))) +
        theme_minimal() +
        theme(legend.position = "bottom", panel.grid.minor = element_blank()) +
        scale_fill_manual(values = c(color1, color2), labels = c("boys", "girls")) +
        labs(x = "% children not in school", y = "", fill = "")
    } else if (children_var() == "inc") {
      # children_decile3 <- children_decile %>%
      #   filter(!is.na(lab_monthly_inc)) %>%
      #   group_by(decile, sex) %>%
      #   summarize(mean_hr = mean(tot_work_week_hr), mean_inc = mean(lab_monthly_inc), mean_pct = mean(hh_hr_pct))
      
      ggplot(children %>% filter(!is.na(lab_monthly_inc) & hh_tot_inc != 0)) +
        geom_jitter(aes(hh_tot_inc + 1, y = hh_hr_pct, size = lab_monthly_inc, color = sex), alpha = 0.15) +
        geom_smooth(aes(hh_tot_inc + 1, hh_hr_pct, color = sex), alpha = 0.1) +
        # geom_line(aes(y = mean_pct, x = decile, color = sex), size = 1) +
        # geom_point(aes(y = mean_pct, x = decile, size = mean_inc, color = sex)) +
        geom_text(aes(120, 85, label = "circle size =\nmonthly income (BOB)"), color = "grey", hjust = "left") +
        theme_minimal() +
        theme(legend.position = "bottom", panel.grid.minor = element_blank()) +
        scale_x_continuous(trans = "log10") +
        scale_color_manual(values = c(color1, color2), labels = c("boys", "girls")) +
        scale_size(range = c(0.1, 30), guide = F) +
        labs(x = "household monthly income (BOB)", y = "share of household work hours (%)", color = "average")
    }
  )
  
  children %>%
    ggplot() +
    geom_smooth(aes(hh_tot_inc, hh_hr_pct, color = sex)) +
    theme_minimal() +
    scale_x_continuous(trans = "log10")
  
  output$children_lfp1 <- renderPlot(
    ggplot(children %>% filter(!is.na(lab_monthly_inc))) +
      geom_jitter(aes(x = age, y = tot_work_week_hr, size = lab_monthly_inc, color = sex), alpha = 0.15) +
      geom_line(data = . %>% group_by(age, sex) %>% summarize(mean_hr = mean(tot_work_week_hr), mean_inc = mean(lab_monthly_inc)),
                aes(y = mean_hr, x = age, color = sex), size = 1) +
      geom_point(data = . %>% group_by(age, sex) %>% summarize(mean_hr = mean(tot_work_week_hr), mean_inc = mean(lab_monthly_inc)),
                 aes(y = mean_hr, x = age, size = mean_inc, color = sex)) +
      geom_text(aes(7, 85, label = "circle size =\nmonthly income (BOB)"), color = "grey", hjust = "left") +
      theme_minimal() +
      theme(legend.position = "bottom", panel.grid.minor = element_blank()) +
      scale_x_continuous(limits = c(7, 17.8)) +
      scale_color_manual(values = c(color1, color2), labels = c("boys", "girls")) +
      scale_size(range = c(0.1, 30), guide = F) +
      labs(y = "weekly work hours", color = "")
  )
  output$children_lfp2 <- renderPlot(
    ggplot(children %>% filter(!is.na(lab_monthly_inc))) +
      geom_jitter(aes(x = age, y = tot_work_week_hr, size = lab_monthly_inc / tot_work_week_hr / 4.33, color = sex), alpha = 0.15) +
      geom_line(data = . %>% group_by(age, sex) %>% summarize(mean_hr = mean(tot_work_week_hr), mean_inc = mean(lab_monthly_inc / tot_work_week_hr / 4.33)),
                aes(y = mean_hr, x = age, color = sex), size = 1) +
      geom_point(data = . %>% group_by(age, sex) %>% summarize(mean_hr = mean(tot_work_week_hr), mean_inc = mean(lab_monthly_inc / tot_work_week_hr / 4.33)),
                 aes(y = mean_hr, x = age, size = mean_inc, color = sex)) +
      geom_text(aes(7, 85, label = "circle size =\nhourly income (BOB)"), color = "grey", hjust = "left") +
      theme_minimal() +
      theme(legend.position = "bottom", panel.grid.minor = element_blank()) +
      scale_x_continuous(limits = c(7, 17.8)) +
      scale_color_manual(values = c(color1, color2), labels = c("boys", "girls")) +
      scale_size(range = c(0.1, 30), guide = F) +
      labs(y = "weekly work hours", color = "")
  )
  
  
  # Tab panel: youth ------------------------------------------------
    # Youth: intro ----------------------------------
  output$youth_intro <- renderText("The youth period represents a pivotal moment in one's life.
                                   Faced with life-changing decisions on education and employment, and situated within diverse familial and societal contexts, youths not only shape their own life trajectories in important ways, but also determine a society's development for decades to come.
                                   Therefore, the well-being and opportunities of youths merit special consideration.
                                   The analysis in this section revolves around three aspects: educational attainment, employment, and income.
                                   We seek to highlight the socioeconomic factors that impact different youths' access to (higher) education, job opportunities, and fair pay, all through the lens of gender.
                                   As we can see in the graph here, there is already a gap in the level of employment between women and men.
                                   In the subsections below, we will delve further into the reasons and detailed manifestations of this disparity.")
  
  youth1 <- youth %>%
    group_by(age, sex) %>%
    summarize(tot = n(), in_school = sum(in_school == "1. Si") / tot * 100, job = sum(!is.na(primary_job)) / tot * 100)
  
  output$youth_overview <- renderPlot(
    ggplot(youth1) +
      geom_line(aes(age, in_school), color = color1, size = 1) +
      geom_text(aes(20.5, 70, label = "% of surveyed youth\nwho are in school"), color = color1) +
      geom_line(aes(age, job), color = color2, size = 1) +
      geom_text(aes(19, 30, label = "% of surveyed youth\nwho are working"), color = color2) +
      facet_wrap(vars(sex), labeller = labeller(sex = c("1.Hombre" = "men", "2.Mujer" = "women"))) +
      theme_minimal() +
      theme(panel.grid.minor = element_blank()) +
      ylim(0, 100) +
      ylab("")
  )
  
    # Youth: education -------------------------------------
  output$youth_edu_t1 <- renderText("Further education is often the gateway to more desirable employment opportunities and better life outcomes, but access to education, especially beyond the 12 years of compulsory education, is by no means equal.")
  output$youth_edu_marital <- renderText("One of the most crucial factors in determining educational attainment is one's marital status.
                                         Once a young adult enters into a marriage or cohabiting relationship, their likelihood of remaining students drops precipitously.
                                         This happens regardless of the young adult's gender.
                                         In fact, it is observed that single women in this age group are slightly more likely to continue their education than single men.")
  output$youth_edu_internet <- renderText("Another critical element is Internet access.
                                          Not only do people with Internet obtain education at a much higher rate than those without, but such differential impact is also stronger among women than among men.
                                          Of course, no simplistic causal relationship can be directed inferred from the data, and there could be other confounding factors.
                                          But we have found that the effect of Internet access still holds after controlling for per capita household income decile.
                                          That means even for the poorest 10% of households earning no more than 527 bolivianos per capita per month, having Internet access can dramatically change the youths' educational outcome.")
  output$youth_edu_area <- renderText("Similar to the effect of Internet access, the rural/urban disparity also plays out differently for men and women.
                                      In urban areas, young women seem to continue education at a slightly higher rate than their male counterparts, while in rural Bolivia, women are consistently less likely to remain students than men.")
  output$youth_edu_depto <- renderText("There are also variations within rural and urban areas, with departments like Oruro exhibiting some of the biggest gender gaps in education among youths.")
  output$youth_edu_indi <- renderText("Another critical social identity is indigeneity.
                                      For both genders, non-indigenous young adults enjoy a small but noticeable advantage in educational access compared to indigenous ones.")
  output$youth_edu_lang <- renderText("Finally, it is important to note that indigenous people are by no means a monolith.
                                      One's primary language plays a sizeable role in their education as well.
                                      Women and men who speak Castellano as their primary language obtain education at a much higher rate than those speaking Quechua or other languages, and this phenomenon is more pronounced among women than men.")
  output$youth_edu_t2 <- renderText("The overall education landscape in Bolivia shows that more than half of youths finish tertiary education by the age of 22, whether it is a university or technical degree.
                                    It also demonstrates little difference between the two genders.
                                    However, it is crucial to interrogate: Who has the ability to be among the more educated half? And how does young adults' education translate into and interact with their employment conditions?")
  
  output$youth_edu_imp <- renderPlot(
    read_csv("data/youth_ses_rf1_imp.csv") %>%
      ggplot() +
      geom_segment(aes(x = 0, y = reorder(name, MeanDecreaseAccuracy), xend = MeanDecreaseAccuracy, yend = reorder(name, MeanDecreaseAccuracy)), color = "grey") +
      geom_point(aes(MeanDecreaseAccuracy, name), color = color1, size = 3) +
      theme_minimal() +
      theme(panel.grid.major.y = element_blank(), panel.grid.minor = element_blank(), axis.text.x = element_blank()) +
      scale_x_continuous(position = "top", breaks = c(0)) +
      xlab("variable importance") + ylab("")
  )
  
  youth_edu1_p <- function(df, var, labels) {
    df2 <- df %>%
      group_by(age, sex, .data[[var]]) %>%
      summarize(mean = mean(in_school == "1. Si") * 100, count = n()) %>%
      filter(count > 9)
    
    n <- length(unique(df2[[var]]))
    
    ggplot(df2) +
      geom_line(aes(age, mean, color = .data[[var]]), size = 1) +
      facet_wrap(vars(sex), labeller = labeller(sex = c("1.Hombre" = "men", "2.Mujer" = "women"))) +
      theme_minimal() +
      theme(legend.position = "bottom", panel.grid.minor = element_blank()) +
      scale_color_manual(values = color_pal[1:n], labels = labels) +
      labs(y = "% student", color = "") + ylim(0, 100)
  }
  
  youth_edu1_var <- reactive(input$youth_edu_ses)
  
  output$youth_edu1 <- renderPlot(
    if (youth_edu1_var() == "internet") {
      youth %>% mutate(decile = cut(pc_inc, 
                                    breaks = unique(quantile(pc_inc, probs = seq.int(0, 1, by = 0.1))), 
                                    include.lowest = T)) %>%
        group_by(sex, decile, internet_use) %>%
        summarize(mean = mean(in_school == "1. Si") * 100, count = n()) %>%
        mutate(decile = as.numeric(decile)) %>%
        ggplot() +
        geom_line(aes(decile, mean, color = internet_use), size = 1) +
        facet_wrap(vars(sex), labeller = labeller(sex = c("1.Hombre" = "men", "2.Mujer" = "women"))) +
        theme_minimal() +
        theme(legend.position = "bottom", panel.grid.minor = element_blank()) +
        scale_color_manual(values = color_pal[1:2], labels = c("with Internet", "without Internet")) +
        scale_x_continuous(breaks = 1:10) +
        labs(x = "per capita household income decile", y = "% student", color = "") + ylim(0, 100)
    } else if (youth_edu1_var() == "area") {
      youth_edu1_p(youth, "area", c("rural", "urban"))
    } else if (youth_edu1_var() == "indi") {
      youth_edu1_p(youth %>% mutate(indigenous = ifelse(startsWith(indigenous, "3"), "2. No pertenece", indigenous)),
                   "indigenous", c("indigenous", "not indigenous"))
    } else if (youth_edu1_var() == "depto") {
      youth_depto <- youth %>%
        group_by(depto, area, sex) %>%
        summarize(mean = mean(in_school == "1. Si") * 100)
      
      ggplot(youth_depto) +
        geom_col(aes(mean, depto, fill = sex), position = "dodge", width = 0.5) +
        facet_wrap(vars(area), labeller = labeller(area = c("Rural" = "rural", "Urbana" = "urban"))) +
        theme_minimal() +
        theme(legend.position = "bottom", panel.grid.minor = element_blank()) +
        scale_fill_manual(values = c(color1, color2), labels = c("men", "women")) +
        labs(x = "% student", y = "", fill = "")
    } else if (youth_edu1_var() == "lang") {
      youth_lang <- youth %>%
        mutate(language_1 = ifelse(language_1 %in% c("QUECHUA", "CASTELLANO"), language_1, "OTHER"),
               indigenous = ifelse(startsWith(indigenous, "3"), "2. No pertenece", indigenous)) %>%
        group_by(language_1, sex, indigenous) %>%
        summarize(mean = mean(in_school == "1. Si") * 100)
      
      ggplot(youth_lang) +
        geom_col(aes(mean, language_1, fill = sex), position = "dodge", width = 0.5) +
        facet_wrap(vars(indigenous), labeller = labeller(indigenous = c("1. Pertenece" = "indigenous", "2. No pertenece" = "not indigenous"))) +
        theme_minimal() +
        theme(legend.position = "bottom", panel.grid.minor = element_blank()) +
        scale_fill_manual(values = c(color1, color2), labels = c("men", "women")) +
        labs(x = "% student", y = "", fill = "")
    } else if (youth_edu1_var() == "marital") {
      youth_edu1_p(youth %>% filter(str_detect(marital, "^[1-3]")), "marital", c("single", "married", "cohabiting"))
    }
  )

  output$youth_edu2 <- renderPlot(
    ggplot(youth) +
      geom_bar(aes(age, fill = education), position = "fill") +
      scale_fill_manual(values = c(color1, color2, color3, color4)) +
      facet_wrap(vars(sex), labeller = labeller(sex = c("1.Hombre" = "men", "2.Mujer" = "women"))) +
      theme_minimal() +
      theme(legend.position = "bottom", legend.title = element_blank()) +
      ylab("proportion")
  )
  
    # Youth: employment --------------------
  
  output$youth_emp_overview <- renderText("Although gender undoubtedly influences youths' education in intricate ways, young women and men actually attend school at similar rates overall.
                                          Where the differentiation truly manifests is in young adults' employment.
                                          There are almost twice as many men between the ages of 18 and 24 doing paid or unpaid work than women, a curious phenomenon that provokes further inquiries:
                                          Why does this happen? What do we know about the working youths? What is the nature of their work?")
  # output$youth_emp_t1 <- renderText("Below is the relative importance of various socioeconomic factors that affect youths' employment.")
  output$youth_emp_stu <- renderText("Clearly, whether one is in school has a significant impact on their likelihood to work, but such effect is severely gendered.
                                     For men, non-students are about three times as likely than students to be working, with their labor force participation (LFP) rate going up as they age.
                                     For women, however, even those who are not in school have only a roughly 50% chance to be working, paid or unpaid.")
  output$youth_emp_edu <- renderText("Along the same line, the data on educational attainment and LFP corroborates the above evidence.
                                     Women and men with tertiary degrees tend not to work between the ages of 18 and 24, most likely because they are still finishing their education during this time.
                                     However, for people who terminate their education after obtaining a primary or secondary degree, the story is drastically different.
                                     Men falling into this category are almost guaranteed to have a job, with their LFP nearing or reaching 100% by the age of 24;
                                     whereas for women, even after finishing their education, only around half of them end up working.
                                     What are the other half's primary activity as they begin their adulthood, if they are neither in school nor pursuing employment?")
  output$youth_emp_marital <- renderText("Statistics on marital status in Bolivia provides a potential answer to this question.
                                         As the household survey shows, nearly all men in a marriage or cohabiting relationship work--even if some of them are as young as 18.
                                         Single men, on the other hand, are much less likely to do so.
                                         Meanwhile, for women, their marital status does not have a visible influence on their decision to work.
                                         The average LFP hovers just below 50% for women across different marital statuses.
                                         This shows that the traditional male breadwinner/female homemaker family model is dominant among young adults in Bolivia and underlies their employment decisions.
                                         Together with the previous observations, we can infer that women who do not pursue higher education will probably depend on their husbands for income, which render their livelihoods potentially precarious.")
  output$youth_emp_area <- renderText("Lastly, the rural/urban divide shows that rural residents, regardless of gender, are actually a lot more likely than their urban peers at the same age to work.
                                      An important contributing factor is probably the fact that young adults in cities often remain students for a longer period of time before commencing their career.
                                      This sheds light on the diverging life trajectories for rural and urban dwellers, as well as the different skills and qualifications desired by rural and urban labor markets.")
  
  output$youth_edu_sex <- renderPlot(
    ggplot(youth %>% filter(in_school == "1. Si")) +
      geom_bar(aes(age, fill = sex), width = 0.5, position = "fill") +
      theme_minimal() +
      theme(legend.position = "bottom", legend.title = element_blank(), panel.grid.minor = element_blank()) +
      scale_fill_manual(values = c(color1, color2), labels = c("men", "women")) +
      ylab("proportion")
  )
  
  output$youth_emp_sex <- renderPlot(
    ggplot(youth %>% filter(!is.na(primary_job))) +
      geom_bar(aes(age, fill = sex), width = 0.5, position = "fill") +
      theme_minimal() +
      theme(legend.position = "bottom", legend.title = element_blank(), panel.grid.minor = element_blank()) +
      scale_fill_manual(values = c(color1, color2), labels = c("men", "women")) +
      ylab("proportion")
  )
  
  output$youth_emp_imp <- renderPlot(
    read_csv("data/youth_ses_rf2_imp.csv") %>%
      ggplot() +
      geom_segment(aes(x = 0, y = reorder(name, MeanDecreaseAccuracy), xend = MeanDecreaseAccuracy, yend = reorder(name, MeanDecreaseAccuracy)), color = "grey") +
      geom_point(aes(MeanDecreaseAccuracy, name), color = color1, size = 3) +
      theme_minimal() +
      theme(panel.grid.major.y = element_blank(), panel.grid.minor = element_blank(), axis.text.x = element_blank()) +
      scale_x_continuous(position = "top", breaks = c(0)) +
      xlab("variable importance") + ylab("")
  )
  
  youth_emp1_p <- function(df, var, labels) {
    df2 <- df %>%
      group_by(age, sex, .data[[var]]) %>%
      summarize(mean = mean(!is.na(primary_job)) * 100, count = n()) %>%
      filter(count > 9)
    
    n <- length(unique(df2[[var]]))
    
    ggplot(df2) +
      geom_line(aes(age, mean, color = .data[[var]]), size = 1) +
      facet_wrap(vars(sex), labeller = labeller(sex = c("1.Hombre" = "men", "2.Mujer" = "women"))) +
      theme_minimal() +
      theme(legend.position = "bottom", panel.grid.minor = element_blank()) +
      scale_color_manual(values = color_pal[1:n], labels = labels) +
      labs(y = "% working", color = "") + ylim(0, 100)
  }
  
  youth_emp1_var <- reactive(input$youth_emp_ses)
  
  output$youth_emp1 <- renderPlot(
    if (youth_emp1_var() == "marital") {
      youth_emp1_p(youth, "marital", c("single", "married", "cohabiting"))
    } else if (youth_emp1_var() == "edu") {
      youth_emp1_p(youth, "education", c("primary", "secondary", "tertiary"))
    } else if (youth_emp1_var() == "area") {
      youth_emp1_p(youth, "area", c("rural", "urban"))
    } else if (youth_emp1_var() == "stu") {
      youth_emp1_p(youth, "in_school", c("in school", "not in school"))
    }
  )
  
    # Youth: income ------------------------
  output$youth_inc_t1 <- renderText("Gender disparity does not only manifest in the LFP rates, but also in pay gaps between women and men, compounding women's economic vulnerability.
                                    Women consistently perform unpaid labor at a higher rate than men of the same age.
                                    At the same time, we also see that the percentage of unpaid workers goes down as one ages, signaling that young adulthood marks the transition from unpaid labor, mostly family-based, to more formal, paid work.")
  output$youth_inc_t2 <- renderText("Another form of gender pay gap is the different average income received by women and men.
                                    From age 19, women's and men's incomes start to diverge, even though their average work hours remain approximately the same.
                                    This means that women receive less compensation for their labor, even though they obtain similar levels of education compared to men.
                                    Thus, for women, the return on their education is undercut by gender discrimination, which potentially limits their potential in the workplace.")
  
  output$youth_inc_paid <- renderPlot(
    youth %>%
      filter(!is.na(primary_job)) %>%
      mutate(paid = ifelse(str_detect(work_type, "^[78]"), "unpaid", "paid")) %>%
      group_by(age, sex) %>%
      summarize(mean = mean(paid == "unpaid") * 100) %>%
      ggplot() +
      geom_line(aes(age, mean, color = sex), size = 1) +
      theme_minimal() +
      theme(panel.grid.minor = element_blank(), legend.position = "bottom", legend.title = element_blank()) +
      scale_color_manual(values = color_pal[1:2], labels = c("men", "women")) +
      ylab("% unpaid worker") + ylim(0, 60)
  )
  
  output$youth_inc_inc <- renderPlot(
    youth %>%
      filter(!is.na(lab_monthly_inc) & lab_monthly_inc != 0) %>%
      group_by(age, sex) %>%
      summarize(mean = mean(lab_monthly_inc), mean_hr = mean(tot_work_week_hr)) %>%
      ggplot() +
      geom_jitter(data = youth %>% filter(!is.na(lab_monthly_inc) & lab_monthly_inc != 0),
                  aes(age, lab_monthly_inc, color = sex, size = tot_work_week_hr), alpha = 0.08, width = 0.5) +
      geom_line(aes(age, mean, color = sex), size = 1) +
      geom_point(aes(age, mean, color = sex, size = mean_hr)) +
      geom_text(aes(18, 8500, label = "circle size =\nweekly work hours"), color = "grey") +
      theme_minimal() +
      theme(panel.grid.minor.y = element_blank(), legend.position = "bottom") +
      scale_color_manual(values = color_pal[1:2], labels = c("men", "women")) +
      scale_size_continuous(range = c(0.1, 8), guide = F) +
      ylim(0, 10000) +
      labs(y = "Monthly labor income (BOB)", color = "average")
  )
  
  # Tab panel: entering the job market --------------------------
  output$age_t_lfp <- renderText('There are different trends in Labor Force Participation for each segment')
  output$pop_emp <- renderPlot(age_lfp(employed))
  output$pop_unemp <- renderPlot(age_lfp(unemployed))
  output$pop_inac <- renderPlot(age_lfp(inactive))
  
  
  output$proportion_exp <- renderText("Let's look at the proportions of people in each category")
  output$waf_emp <- renderPlot(wfl_plot(employed_gender,50000))
  output$waf_unemp <- renderPlot(wfl_plot(unemp_gender,50000))
  output$waf_inac <- renderPlot(wfl_plot(inactive_gender,50000))
  
  output$sch_lf <- renderText("let's look at the distributions by education, gender, age and participation in the labor force")
  output$ed_emp <- renderPlot(ridge_educ(employed))
  output$ed_unemp <- renderPlot(ridge_educ(unemployed))
  output$ed_inac <- renderPlot(ridge_educ(inactive))
  
  output$ar_lf <- renderText("Population Distributions by gender, area, age and participation in the labor force")
  output$ar_emp <- renderPlot(ridge_urban(employed))
  output$ar_unemp <- renderPlot(ridge_urban(unemployed))
  output$ar_inac <- renderPlot(ridge_urban(inactive))
  
  output$ind_lf <- renderText("Population Distributions by gender, indigenous belonging, age and participation in the labor force")
  output$ind_emp <- renderPlot(ridge_indigen(employed))
  output$ind_unemp <- renderPlot(ridge_indigen(unemployed))
  output$ind_inac <- renderPlot(ridge_indigen(inactive))
  
  # Tab panel: paid and unpaid labor --------------------------------
  output$pay_intro <- renderText("The unpaid labor force is dominated by women, while men make up almost two thirds of the paid labor market.")
  output$pay_t1 <- renderText("As a result, on average, women earn much less labor income than men, which contributes significantly to the gender income gap. However, unpaid labor is not the only factor at play. Even among paid workers, there is still a notable difference between women's and men's earnings.")
  output$pay_t2 <- renderText("How much is the unpaid labor really worth? To calculate this, we assume that all workers within a household, paid or unpaid, contribute the same value to household labor income with each unit of time they spend working. Therefore, for a 2-person household where one paid and one unpaid worker each reports 50 weekly work hours, we would evenly distribute the monthly labor income across the two, for example.")
  output$pay_t3 <- renderText("As expected, unpaid women workers contribute over 4 times the economic value to their families than unpaid men, largely due to women's overwhelming representation in unpaid work and their long work hours.")
  output$pay_t4 <- renderText("Rural area households, in particular, rely on unpaid labor. In sectors like agriculture and mining, informal unpaid labor is crucial to family subsistence and to the overall productivity of these industries. Additionally, similar patterns emerge across rural and urban areas: in both cases, lower-income families see more incidents of unpaid labor, and women tend to bear most of the burden. In aggregate, unpaid labor constitute an important component of the economy, making up 7.9% and 3.5% of total household productivity in rural and urban areas, respectively.")
  output$pay_t5 <- renderText("Yet, not all rural areas are equal. In departments like Potosi, a historic mining center, more than three in ten workers receive no pay, whereas the proportion in Pando or Santa Cruz is much lower: less than 10%. In contrast, urban areas across various departments do not have such variations in unpaid labor.")
  output$pay_t6 <- renderText("Another factor influencing people's, especially women's, pay status is marriage. As single women enter into marriage or cohabiting relationships, their rate of doing unpaid labor triples, while the corresponding rate for men is cut in third. Hence, (heterosexual) marriage enhances existing gender inequalities, and such effects persist even after the end of marital relationships, as is evident in the noticeable gap between female and male divorcees and widow(er)s.")
  output$pay_t7 <- renderText("Finally, cellphone usage emerges as a surprising factor with a significant correlation with unpaid labor, and this is true even after accounting for household labor income. Although, as expected, members of lower-income households are less likely to have access to cellphones, women without cellphones are more likely to be doing unpaid labor than those with cellphones from households of similar levels of income.")

  output$pay_age <- renderPlot(
    ggplot(adults %>% filter(!is.na(paid))) +
      geom_bar(aes(age, fill = sex), position = "fill", width = 0.9) +
      geom_hline(yintercept = 0.5, alpha = 0.2, linetype = "dashed") +
      facet_wrap(vars(paid)) +
      theme_minimal() +
      theme(legend.position = "bottom", legend.title = element_blank(), panel.grid.minor = element_blank()) +
      scale_fill_manual(values = c(color1, color2), labels = c("men", "women")) +
      ylab("proportion")
  )
  
  output$pay_lab_inc1 <- renderPlot(
    ggplot(adults %>% filter(!is.na(paid))) +
      geom_jitter(aes(x = age, y = lab_monthly_inc, color = sex), alpha = 0.05) +
      geom_line(data = adults %>% filter(!is.na(paid)) %>% group_by(age, sex) %>% summarize(mean = mean(lab_monthly_inc, na.rm = T)),
                aes(x = age, y = mean, color = sex), size = 1) +
      geom_point(data = adults %>% filter(!is.na(paid)) %>% group_by(age, sex) %>% summarize(mean = mean(lab_monthly_inc, na.rm = T)),
                 aes(x = age, y = mean, color = sex), size = 2.5) +
      theme_minimal() +
      theme(legend.position = "bottom", legend.title = element_blank(), panel.grid.minor = element_blank()) +
      scale_color_manual(values = c(color1, color2), labels = c("men", "women")) +
      ylab("monthly labor income (BOB)") +
      ylim(0, 20000)
  )
  
  output$pay_lab_inc2 <- renderPlot(
    ggplot(adults %>% filter(paid == "paid")) +
      geom_jitter(aes(x = age, y = lab_monthly_inc, color = sex), alpha = 0.05) +
      geom_line(data = adults %>% filter(paid == "paid") %>% group_by(age, sex) %>% summarize(mean = mean(lab_monthly_inc, na.rm = T)),
                aes(x = age, y = mean, color = sex), size = 1) +
      geom_point(data = adults %>% filter(paid == "paid") %>% group_by(age, sex) %>% summarize(mean = mean(lab_monthly_inc, na.rm = T)),
                 aes(x = age, y = mean, color = sex), size = 2.5) +
      theme_minimal() +
      theme(legend.position = "bottom", legend.title = element_blank(), panel.grid.minor = element_blank()) +
      scale_color_manual(values = c(color1, color2), labels = c("men", "women")) +
      ylab("monthly labor income (BOB)") +
      ylim(0, 20000)
  )
  
  output$pay_worth <- renderPlot(
    ggplot(adults %>% filter(paid == "unpaid") %>% mutate(unpaid_worth = hh_lab_inc * hh_hr_pct)) +
      geom_density(aes(unpaid_worth, fill = sex), alpha = 0.35, color = "grey") +
      theme_minimal() +
      theme(legend.position = "bottom", legend.title = element_blank(), panel.grid.minor = element_blank(), axis.text.y = element_blank()) +
      scale_fill_manual(values = c(color1, color2), labels = c("men", "women")) +
      xlim(0, 1700000) + xlab("Contribution to monthly household labor income (BOB)")
  )
  
  adults_unpaid_worth <- adults %>%
    filter(paid == "unpaid") %>%
    mutate(unpaid_worth = hh_lab_inc * hh_hr_pct) %>%
    group_by(sex) %>%
    summarize(sum = sum(unpaid_worth))

  output$pay_worth_tot1 <- renderText(comma(adults_unpaid_worth$sum[1]))
  output$pay_worth_tot2 <- renderText(comma(adults_unpaid_worth$sum[2]))
  
  output$pay_ru1 <- renderPlot(
    ggplot(adults %>% filter(paid == "unpaid")) +
      geom_density(aes(hh_lab_inc, fill = sex), alpha = 0.35, color = "grey") +
      facet_wrap(vars(area), labeller = labeller(area = c("Rural" = "rural", "Urbana" = "urban"))) +
      theme_minimal() +
      theme(legend.position = "bottom", legend.title = element_blank(), panel.grid.minor = element_blank(), axis.text.y = element_blank()) +
      scale_fill_manual(values = c(color1, color2), labels = c("men", "women")) +
      xlim(0, 40000) + xlab("Household monthly labor income (BOB)")
  )
  
  adults_unpaid_worth_ru <- adults %>%
    mutate(paid = paid == "unpaid", unpaid_hr = tot_work_week_hr * paid) %>%
    group_by(folio) %>%
    summarize(area = first(area), hr = first(hh_hr), inc = first(hh_lab_inc), unpaid_hr = sum(unpaid_hr, na.rm = T),
              unpaid_inc = ifelse(hr == 0, 0, inc * unpaid_hr / hr)) %>%
    group_by(area) %>%
    summarize(tot_inc = sum(inc), tot_unpaid_inc = sum(unpaid_inc), unpaid_pct = round(tot_unpaid_inc / tot_inc * 100, 1))
  
  output$pay_ru2 <- renderText(paste0(adults_unpaid_worth_ru$unpaid_pct[1], "%"))
  output$pay_ru3 <- renderText(paste0(adults_unpaid_worth_ru$unpaid_pct[2], "%"))
  
  adults_pay_depto <- adults %>%
    filter(!is.na(paid)) %>%
    group_by(depto, area) %>%
    summarize(mean_unpaid = mean(paid == "unpaid") * 100) %>%
    pivot_wider(names_from = area, values_from = mean_unpaid) %>%
    arrange(Rural)
  
  depto_ru <- adults_pay_depto$depto
    
  output$pay_depto1 <- renderPlot(
    ggplot(adults_pay_depto) +
      geom_segment(aes(x = Urbana, xend = Rural, y = factor(depto, levels = depto_ru), yend = depto), color = "grey") +
      geom_point(aes(y = depto, x = Rural), color = color1, size = 6) +
      geom_point(aes(y = depto, x = Urbana), color = color2, size = 6) +
      geom_point(aes(25, 2.2), color = color1, size = 6) +
      geom_text(aes(27, 2.2, label = "rural"), hjust = "left") +
      geom_point(aes(25, 1.5), color = color2, size = 6) +
      geom_text(aes(27, 1.5, label = "urban"), hjust = "left") +
      theme_minimal() +
      theme(legend.position = "bottom", legend.title = element_blank(), panel.grid.minor = element_blank(),
            panel.grid.major.y = element_blank()) +
      xlab("% unpaid workers") + ylab("")
  )
  
  adults_marital <- adults %>%
    filter(!is.na(paid)) %>%
    mutate(marital = as.numeric(substr(marital, 1, 1))) %>%
    mutate(marital = case_when(marital == 1 ~ "single", marital == 2 ~ "married", marital == 3 ~ "cohabiting",
                               marital == 4 ~ "separated", marital == 5 ~ "divorced", marital == 6 ~ "widowed")) %>%
    group_by(marital, sex) %>%
    summarize(mean_unpaid = mean(paid == "unpaid") * 100) %>%
    separate(sex, into = c(NA, "sex"), sep = 2, remove = T) %>%
    pivot_wider(names_from = sex, values_from = mean_unpaid, names_sep = "_") %>%
    arrange(Mujer)
  
  marital_sex <- adults_marital$marital
  
  output$pay_marital1 <- renderPlot(
    ggplot(adults_marital) +
      geom_segment(aes(x = Hombre, xend = Mujer, y = factor(marital, levels = marital_sex), yend = marital), color = "grey") +
      geom_point(aes(y = marital, x = Hombre), color = color1, size = 6) +
      geom_point(aes(y = marital, x = Mujer), color = color2, size = 6) +
      geom_point(aes(16, 2), color = color1, size = 6) +
      geom_text(aes(17, 2, label = "men"), hjust = "left") +
      geom_point(aes(16, 1.5), color = color2, size = 6) +
      geom_text(aes(17, 1.5, label = "women"), hjust = "left") +
      theme_minimal() +
      theme(legend.position = "bottom", legend.title = element_blank(), panel.grid.minor = element_blank(),
            panel.grid.major.y = element_blank()) +
      xlab("% of unpaid workers") + ylab("")
  )
  
  output$pay_cell1 <- renderPlot(
    ggplot(adults %>% filter(paid == "unpaid")) +
      geom_density(aes(hh_lab_inc, fill = sex), alpha = 0.35, color = "grey") +
      facet_wrap(vars(cellphone), labeller = labeller(cellphone = c("1. Si" = "with cellphone", "2. No" = "without cellphone"))) +
      theme_minimal() +
      theme(legend.position = "bottom", legend.title = element_blank(), panel.grid.minor = element_blank(), axis.text.y = element_blank()) +
      scale_fill_manual(values = c(color1, color2), labels = c("men", "women")) +
      xlim(0, 40000) + xlab("Household monthly labor income (BOB)")
  )

  
  # Tab panel: older adults -----------------------------------------
  output$older_t1 <- renderText("In the 2000s, Bolivia instituted the Renta Dignidad program, a non-contributory cash transfer program that guarantees a basic income for everyone over 60.
                                Unlike pensions, Renta Dignidad is not contingent on one's prior participation in the formal labor market, which entails enormous and tangible benefits for the large part of Bolivian labor force who work informally.
                                While this program has achieved notable successes in terms of improving older adults' quality of life and reducing child labor rates, many questions remain regarding older adults' well-being.
                                To begin with, the amount of benefits Renta Dignidad provides is low: only 250-300 bolivianos per person per month, regardless of one's location or needs.
                                Therefore, it is not always sufficient to cover one's cost of living.
                                Furthermore, older adults rely on not only a universal basic income, but also a strong support network.
                                How well their family members are faring often has a direct influence on whether the older adults can maintain a stable livelihood as well.
                                With these considerations in mind, we seek to disentangle older adults' well-being by delving into three questions concerning their social and economic lives.")
  output$older_t2 <- renderText("When examining older adults' well-being, the first question we look at is whether they continue to work after the official retirement age of 60.
                                Although retirement is undoubtedly a personal choice, here we use retirement status as a proxy to measure whether one has the ability to retire if so desired.
                                In other words, which of the older adults have no choice but to continue working in order to sustain themselves financially?
                                The random forest model indicates that one's non-labor income, which includes cash transfers, pensions, remittances, etc., has a great impact on one's likelihood to work after 60.
                                Yet, intriguingly, the data show that people who work actually receive higher non-labor income on average than those who do not.
                                Another important income-related factor is the household economic condition--namely, how the other household members are doing economically.
                                Here, this is measured by what the per capita household income would have been if the older adult in question did not bring home any income, labor or non-labor.
                                In addition, other variables to be considered are department, age, household size, and sex.")
  output$older_t3 <- renderText("Next, we zoom into the working population and ask, do they get compensated for their labor? What is the nature of their work?
                                Similar to other age groups, older adults also face a considerable gender gap when it comes to unpaid labor.
                                While other variables, such as age, household size, and non-labor income, remain salient, it is gender that most markedly sets paid and unpaid workers apart.")
  # TODO: change the text below
  output$older_t4 <- renderText("The final question we interrogate is the labor income among the paid workers.
                                What determines their earnings?
                                It turns out that family background matters, and it matters a lot.
                                Older adults from better-off families are more likely to be holding a well-paid job compared to those from poorer households.
                                This correlation probably works through the hierarchies of social classes, education levels, professional skills, and other entrenched power structures that were already at play pre-retirement and continue to solidify one's standing in the labor market post-retirement age.
                                Other factors we will dive into include educational attainment, age, non-labor income, area, department, and household size.")

  output$older_p1 <- renderPlot(
    older %>%
      # mutate(primary_job = ifelse(is.na(primary_job), F, T)) %>%
      ggplot() +
      geom_jitter(aes(age, sp_monthly_inc, color = sex), width = 1, alpha = 0.25) +
      geom_smooth(aes(age, sp_monthly_inc, color = sex), method = "loess", se = F, span = 0.1, method.args = list(degree = 1)) +
      theme_minimal() +
      theme(panel.grid.minor = element_blank(), legend.position = "bottom", legend.title = element_blank()) +
      scale_color_manual(values = color_pal[1:2], labels = c("men", "women")) +
      ylab("monthly social protection income (BOB)") + ylim(-1, 7000)
  )
  
  output$older_p2 <- renderPlot(
    read_csv("data/older_ses_rf1_imp.csv") %>%
      ggplot() +
      geom_segment(aes(x = 0, y = reorder(name, MeanDecreaseAccuracy), xend = MeanDecreaseAccuracy, yend = reorder(name, MeanDecreaseAccuracy)), color = "grey") +
      geom_point(aes(MeanDecreaseAccuracy, name), color = color1, size = 3) +
      theme_minimal() +
      theme(panel.grid.major.y = element_blank(), panel.grid.minor = element_blank(), axis.text.x = element_blank()) +
      scale_x_continuous(position = "top", breaks = c(0)) +
      xlab("variable importance") + ylab("")
  )
  
  older_job_var <- reactive(input$older_job)
  
  older_f1 <- function(var, ylab) {
    fit1 <- loess(older %>% filter(is.na(primary_job) & startsWith(sex, "1")) %>% pull(.data[[var]]) ~ age, degree = 1, span = 0.1,
                  data = older %>% filter(is.na(primary_job) & startsWith(sex, "1")))
    fit2 <- loess(older %>% filter(is.na(primary_job) & startsWith(sex, "2")) %>% pull(.data[[var]]) ~ age, degree = 1, span = 0.1,
                  data = older %>% filter(is.na(primary_job) & startsWith(sex, "2")))
    fit3 <- loess(older %>% filter(!is.na(primary_job) & startsWith(sex, "1")) %>% pull(.data[[var]]) ~ age, degree = 1, span = 0.1,
                  data = older %>% filter(!is.na(primary_job) & startsWith(sex, "1")))
    fit4 <- loess(older %>% filter(!is.na(primary_job) & startsWith(sex, "2")) %>% pull(.data[[var]]) ~ age, degree = 1, span = 0.1,
                  data = older %>% filter(!is.na(primary_job) & startsWith(sex, "2")))
    
    older %>%
      mutate(primary_job = !is.na(primary_job)) %>%
      arrange(primary_job, sex) %>%
      mutate(smooth = c(fit1$fitted, fit2$fitted, fit3$fitted, fit4$fitted)) %>%
      ggplot() +
      geom_jitter(aes(age, .data[[var]], color = primary_job), width = 1, alpha = 0.1) +
      geom_line(aes(age, smooth, color = primary_job), size = 1) +
      facet_wrap(vars(sex)) +
      theme_minimal() +
      theme(panel.grid.minor = element_blank(), legend.position = "bottom") +
      scale_color_manual(values = color_pal[1:2], labels = c("working", "not working"), name = "") +
      ylab(ylab) + ylim(-1, 5000)
  }
  
  older_f2 <- function(var, labels) {
    df <- older %>%
      mutate(primary_job = !is.na(primary_job)) %>%
      group_by(age, .data[[var]]) %>%
      summarize(n = n(), mean = mean(primary_job) * 100) %>%
      filter(n > 9)
    
    ggplot(df) +
      geom_line(aes(age, mean, color = .data[[var]]), size = 1) +
      theme_minimal() +
      theme(panel.grid.minor = element_blank(), legend.position = "bottom") +
      scale_color_manual(values = color_pal[1:2], labels = labels, name = "average") +
      ylab("% working")
  }
  
  output$older_job_p <- renderPlot(
    if (older_job_var() == "nonlab") {
      older_f1("nonlab_monthly_inc", "monthly non-labor income (BOB)")
    } else if (older_job_var() == "rest_of_hh") {
      older_f1("rest_of_hh", "monthly per capita income for rest of household (BOB)")
    } else if (older_job_var() == "area") {
      older_f2("area", c("rural", "urban"))
    } else if (older_job_var() == "sex") {
      older_f2("sex", c("men", "women"))
    }
  )
  
  output$older_p3 <- renderPlot(
    read_csv("data/older_ses_rf2_imp.csv") %>%
      ggplot() +
      geom_segment(aes(x = 0, y = reorder(name, MeanDecreaseAccuracy), xend = MeanDecreaseAccuracy, yend = reorder(name, MeanDecreaseAccuracy)), color = "grey") +
      geom_point(aes(MeanDecreaseAccuracy, name), color = color1, size = 3) +
      theme_minimal() +
      theme(panel.grid.major.y = element_blank(), panel.grid.minor = element_blank(), axis.text.x = element_blank()) +
      scale_x_continuous(position = "top", breaks = c(0)) +
      xlab("variable importance") + ylab("")
  )
  
  older_pay_var <- reactive(input$older_pay)
  
  older_f3 <- function(var, labels) {
    older %>%
      filter(!is.na(paid)) %>%
      mutate(marital = case_when(str_detect(marital, "^[23]") ~ "married/cohabiting",
                                 str_detect(marital, "^[1,456]") ~ "single/separated/divorced/widowed")) %>%
      group_by(age, sex, .data[[var]]) %>%
      summarize(n = n(), mean = mean(paid == "paid") * 100) %>%
      filter(n > 5) %>%
      ggplot() +
      geom_line(aes(age, mean, color = .data[[var]]), size = 1) +
      facet_wrap(vars(sex), labeller = labeller(sex = c("1.Hombre" = "men", "2.Mujer" = "women"))) +
      theme_minimal() +
      theme(panel.grid.minor = element_blank(), legend.position = "bottom") +
      scale_color_manual(values = color_pal[1:2], labels = labels, name = "average") +
      ylab("% workers who are paid")
  }
  
  output$older_pay_p <- renderPlot(
    if (older_pay_var() == "sex") {
      older %>%
        filter(!is.na(paid)) %>%
        mutate(marital = case_when(str_detect(marital, "^[23]") ~ "married/cohabiting",
                                   str_detect(marital, "^[1,456]") ~ "single/separated/divorced/widowed")) %>%
        group_by(age, sex) %>%
        summarize(n = n(), mean = mean(paid == "paid") * 100) %>%
        filter(n > 9) %>%
        ggplot() +
        geom_line(aes(age, mean, color = sex), size = 1) +
        theme_minimal() +
        theme(panel.grid.minor = element_blank(), legend.position = "bottom") +
        scale_color_manual(values = color_pal[1:2], labels = c("men", "women"), name = "average") +
        ylab("% workers who are paid")
    } else if (older_pay_var() == "area") {
      older_f3("area", c("rural", "urban"))
    } else if (older_pay_var() == "marital") {
      older_f3("marital", c("married/cohabiting", "single/separated/divorced/widowed"))
    } else if (older_pay_var() == "rest_of_hh") {
      older %>%
        filter(!is.na(paid)) %>%
        ggplot() +
        geom_density(aes(rest_of_hh, fill = paid), color = "grey", alpha = 0.3) +
        theme_minimal() +
        theme(panel.grid.minor = element_blank(), legend.position = "bottom", axis.text.y = element_blank()) +
        scale_fill_manual(values = color_pal[1:2], labels = c("paid", "unpaid"), name = "") +
        xlab("monthly per capita income for rest of household (BOB)") + xlim(0, 4000) + ylab("density")
    } else if (older_pay_var() == "size") {
      older %>%
        filter(!is.na(paid)) %>%
        group_by(size, sex) %>%
        summarize(n = n(), mean = mean(paid == "paid") * 100) %>%
        filter(n > 5) %>%
        ggplot() +
        geom_line(aes(size, mean, color = sex), size = 1) +
        theme_minimal() +
        theme(panel.grid.minor = element_blank(), legend.position = "bottom") +
        scale_color_manual(values = color_pal[1:2], labels = c("men", "women"), name = "") +
        scale_x_continuous(breaks = seq(1, 9, 2)) +
        ylab("% works who are paid") + ylim(0, 100)
    }
  )
  
  output$older_p4 <- renderPlot(
    personas %>%
      filter(paid == "paid") %>%
      mutate(age_group = cut(age, breaks = c(7, seq(20, 100, 10)), include.lowest = T)) %>%
      mutate(older = age > 60) %>%
      ggplot() +
      geom_jitter(aes(lab_monthly_inc+1, age_group, color = older), alpha = 0.1) +
      geom_boxplot(aes(lab_monthly_inc+1, age_group, color = older), fill = "white", alpha = 0.5, outlier.shape = NA) +
      # geom_density_ridges(aes(lab_monthly_inc+1, age_group, fill = older), alpha = 0.5, color = "grey") +
      theme_minimal() +
      theme(panel.grid.minor = element_blank(), legend.position = "none") +
      scale_x_continuous(trans = 'log10') +
      xlab("monthly labor income (BOB)") + ylab("age group") +
      scale_color_manual(values = c("#E5E5E5", color1))
  )
  
  
  # Page navigation ---------------------------------
  # Children
  observeEvent(input$to_youth, {
    updateNavbarPage(session, "main", selected = "youth")
  })
  
  # Youth
  observeEvent(input$to_children, {
    updateNavbarPage(session, "main", selected = "children")
  })
  observeEvent(input$to_employment, {
    updateNavbarPage(session, "main", selected = "employment")
  })
  
  # Adults - paid and unpaid labor
  observeEvent(input$to_employment2, {
    updateNavbarPage(session, "main", selected = "employment")
  })
  observeEvent(input$to_neet, {
    updateNavbarPage(session, "main", selected = "neet")
  })
  
  # Adults - neet
  observeEvent(input$to_pay, {
    updateNavbarPage(session, "main", selected = "pay")
  })
  observeEvent(input$to_older, {
    updateNavbarPage(session, "main", selected = "older")
  })
  
  # Older adults
  observeEvent(input$to_neet2, {
    updateNavbarPage(session, "main", selected = "neet")
  })
}

shinyApp(ui = ui, server = server)
