library(shiny)
library(shinydashboard)
library(shinythemes)
library(tidyverse)
library(shinyWidgets)
library(plotly)
# library(waffle)
library(ggridges)
library(treemap)
# library(d3treeR)
library(wordcloud2)
library(scales)
library(StatMeasures)
library(ggpubr)
library(pkgload)
pkgload::load_all("package/waffle")

# change folder path
#setwd("/Users/csolisu/Documents/Carla/chamba/shared_Bolivia/Bolivia_unpaid_labor")

source("EDA.R")
source("texts.R")
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


# UI -------------------------------
ui <- fluidPage(
  theme = shinytheme("sandstone"),
  tags$style(type = "text/css",
             "h1, h2, h3, h4 { text-align: center; }",
             "p { text-align: center; color: grey; }",
             "hr { margin-top: 2em; margin-bottom: 2em; }",
             "#children_madlib { color: white; }"),
  
  navbarPage("Living a life of labor in Bolivia",
             id = "main",
             collapsible = T, position = "fixed-top",
             
             # Tab panel: home -----------------
             tabPanel("Home",
                      fluidRow(width = 12, hr(), hr(),
                               imageOutput("landing",
                                           width = "99%",
                                           height = "90%",
                                           click = "landing_cl"))),
             
             # Tab panel: children under 18 --------------------
             tabPanel("Children under 18",
                      value = "children", hr(), hr(),
                      
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
                               
                               hr(),
                               textOutput("children_t2"),
                               h3("Reasons for not enrolling in school"),
                               # d3tree2Output("children_edu3"),
                               plotOutput("children_edu4"),
                               hr(),
                               
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
                        ),
                        column(3,
                               fixedPanel(
                                 tags$a(img(src = "GitHub-Mark.png", style = "width: 32px; "), href = "https://github.com/ccsuehara/bolivia_labor"),
                                 tags$a(img(src = "cloud-computing.png", style = "width: 32px; "),
                                        href = "https://www.ine.gob.bo/index.php/censos-y-banco-de-datos/censos/bases-de-datos-encuestas-sociales/"),
                                 right = 25, top = 85
                               ),
                               fixedPanel(
                                 actionButton("to_youth", label = "youth >"),
                                 right = 10, bottom = 10
                               ))
                      )),
             
             # Tab panel: youth 18-24 --------------------
             tabPanel("Youth 18-24",
                      value = "youth",hr(),hr(),
                      
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
                                 tags$a(img(src = "GitHub-Mark.png", style = "width: 32px; "), href = "https://github.com/ccsuehara/bolivia_labor"),
                                 tags$a(img(src = "cloud-computing.png", style = "width: 32px; "),
                                        href = "https://www.ine.gob.bo/index.php/censos-y-banco-de-datos/censos/bases-de-datos-encuestas-sociales/"),
                                 right = 25, top = 85
                               ),
                               fixedPanel(
                                 actionButton("to_employment", label = "adults - entering the job market >"),
                                 right = 10, bottom = 10
                               ))
                      )),
             
             # Tab panel: adults 25-60 --------------------
             navbarMenu("Adults 25-60",
                        
                        # Entering the job market -------------------------
                        tabPanel("Entering the job market",
                                 value = "employment",hr(),hr(),
                                 
                                 fluidRow(
                                   column(3,
                                          fixedPanel(
                                            actionButton("to_youth2", label = "< youth"),
                                            left = 10, bottom = 10
                                          )),
                                   column(6,
                                          textOutput("lmarket_intro"),
                                          h3("Education overview of adult population"),
                                          textOutput("labor_t_1"),
                                          plotOutput("adult_educ"),
                                          hr(),
                                          
                                          fluidRow(style = 'background-image: url("street_food_1.jpeg"); background-size: cover;',
                                                   column(12, style = "padding: 80px 50px;",
                                                          h3("PARTICIPATION", style = "color: white;"))),
                                          hr(),
                                          h3("Adult population by employment status"),
                                          textOutput("labor_t_2"),
                                          plotOutput("wfl_labor"),
                                          hr(),
                                          
                                          textOutput("labor_t_4"),
                                          plotOutput("labor_hrs"),
                                          hr(),
                                          
                                          textOutput("labor_t_3"),
                                          plotOutput("labor_rf")
                                   ),
                                   column(3,
                                          fixedPanel(
                                            tags$a(img(src = "GitHub-Mark.png", style = "width: 32px; "), href = "https://github.com/ccsuehara/bolivia_labor"),
                                            tags$a(img(src = "cloud-computing.png", style = "width: 32px; "),
                                                   href = "https://www.ine.gob.bo/index.php/censos-y-banco-de-datos/censos/bases-de-datos-encuestas-sociales/"),
                                            right = 25, top = 85
                                          ),
                                          fixedPanel(
                                            actionButton("to_pay", label = "paid/unpaid labor >"),
                                            right = 10, bottom = 10
                                          ))
                                 )),
                        
                        # Paid and unpaid labor ------------------------------
                        tabPanel("Paid and unpaid labor",
                                 value = "pay", hr(),hr(),
                                 
                                 fluidRow(
                                   column(3,
                                          fixedPanel(
                                            actionButton("to_employment2", label = "< entering the job market"),
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
                                            tags$a(img(src = "GitHub-Mark.png", style = "width: 32px; "), href = "https://github.com/ccsuehara/bolivia_labor"),
                                            tags$a(img(src = "cloud-computing.png", style = "width: 32px; "),
                                                   href = "https://www.ine.gob.bo/index.php/censos-y-banco-de-datos/censos/bases-de-datos-encuestas-sociales/"),
                                            right = 25, top = 85
                                          ),
                                          fixedPanel(
                                            actionButton("to_neet", label = "neet population >"),
                                            right = 10, bottom = 10
                                          )))),
                        
                        # NEET -------------------------
                        tabPanel("NEET population",
                                 value = "neet",hr(),hr(),
                                 
                                 fluidRow(
                                   column(3,
                                          fixedPanel(
                                            actionButton("to_pay2", label = "< paid/unpaid labor"),
                                            left = 10, bottom = 10
                                          )),
                                   column(6,
                                          textOutput("neet_t1"),
                                          h3("Distribution of study-labor activities by age and gender"),
                                          plotOutput("neet_p1"),
                                          hr(),
                                          textOutput("neet_t2"),
                                          h3("Neet population aged 14-30, by gender"),
                                          plotOutput("neet_p2"),
                                          hr(),
                                          textOutput("neet_t3"),
                                          h3("Why the NEETs won't work"),
                                          plotOutput("neet_p3"),
                                          hr(),
                                          textOutput("neet_t4"),
                                          h3("Why the NEETs won't study"),
                                          plotOutput("neet_p4"),
                                          
                                          textOutput("neet_t5"),
                                          h3("Which socioeconomic characteristics are related to NEET?"),
                                          plotOutput("neet_p5"),
                                          
                                          selectInput("neet_vars",
                                                      label = "What affects the chances of NEET?",
                                                      choices = c("Marital status" = "marital",
                                                                  "Internet access" = "internet",
                                                                  "Area (rural/urban)" = "area",
                                                                  "Department" = "depto",
                                                                  "Indigenous identity" = "indi",
                                                                  "Primary language" = "lang")),
                                          plotOutput("neets_dec_graph")
                                          
                                   ),
                                   column(3,
                                          fixedPanel(
                                            tags$a(img(src = "GitHub-Mark.png", style = "width: 32px; "), href = "https://github.com/ccsuehara/bolivia_labor"),
                                            tags$a(img(src = "cloud-computing.png", style = "width: 32px; "),
                                                   href = "https://www.ine.gob.bo/index.php/censos-y-banco-de-datos/censos/bases-de-datos-encuestas-sociales/"),
                                            right = 25, top = 85
                                          ),
                                          fixedPanel(
                                            actionButton("to_older", label = "older adults >"),
                                            right = 10, bottom = 10
                                          ))
                                 ))),
             
             # Tab panel: older adults 60+ --------------------
             tabPanel("Older adults 60+",
                      value = "older", hr(), hr(),
                      
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
                               h3("The impact of select socioeconomic characteristics on older adults' work status"),
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
                               h3("The impact of select socioeconomic characteristics on the nature of one's work"),
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
                               p("")),
                        
                        column(3,
                               fixedPanel(
                                 tags$a(img(src = "GitHub-Mark.png", style = "width: 32px; "), href = "https://github.com/ccsuehara/bolivia_labor"),
                                 tags$a(img(src = "cloud-computing.png", style = "width: 32px; "),
                                        href = "https://www.ine.gob.bo/index.php/censos-y-banco-de-datos/censos/bases-de-datos-encuestas-sociales/"),
                                 right = 25, top = 85
                               ),
                               fixedPanel(
                                 actionButton("to_sum", label = "summary >"),
                                 right = 10, bottom = 10
                               ))
                      )),
             
             # Tab panel: summary ----------------------
             tabPanel("Summary",
                      value = "sum", hr(), hr(),
                      
                      fluidRow(
                        column(2,
                               fixedPanel(
                                 actionButton("to_older2", label = "< older adults"),
                                 left = 10, bottom = 10
                               )),
                        
                        column(8,
                               textOutput("sum_t1"),
                               hr(),
                               
                               fluidRow(
                                 column(2,
                                        p("pillars of oppression")),
                                 column(10,
                                        column(3, style = "text-align: center;",
                                               strong("gender")),
                                        column(3, style = "text-align: center;",
                                               strong("geography")),
                                        column(3, style = "text-align: center;",
                                               strong("social class")),
                                        column(3, style = "text-align: center;",
                                               strong("marriage and family norms")))
                               ),
                               
                               fluidRow(
                                 column(10, offset = 2,
                                        column(3, style = "padding-top: 2.5em;", 
                                               p("Compared to a boy, a girl born in Bolivia has a",
                                                 style = "font-size: 0.8em; ")),
                                        column(3, style = "padding-top: 1.5em;", 
                                               p("Compared to a person born in urban La Paz, a person born in rural Potosi has a",
                                                 style = "font-size: 0.8em; ")),
                                        column(3,
                                               p("Compared to a person born in a family in the highest income decile, a person born into the lowest income decile has a",
                                                 style = "font-size: 0.8em; ")),
                                        column(3, style = "padding-top: 2em;", 
                                               p("Compared to a single woman, a married woman has a",
                                                 style = "font-size: 0.8em; ")))
                               ),
                               hr(style = "margin: 0.5em 0em;"),
                               
                               fluidRow(
                                 column(2, style = "padding-top: 1.5em;",
                                        p("likelihood to be working at the age of 17",
                                          style = "font-size: 0.8em; ")),
                                 column(10, style = "height: 7em; ",
                                        column(3,
                                               plotOutput("sum_m11", height = "7em")),
                                        column(3,
                                               plotOutput("sum_m12", height = "7em")),
                                        column(3,
                                               plotOutput("sum_m13", height = "7em")),
                                        column(3))
                               ),
                               hr(style = "margin: 0em;"),
                               
                               fluidRow(
                                 column(2, style = "padding-top: 1.5em;",
                                        p("likelihood to be in school at the age of 23",
                                          style = "font-size: 0.8em; ")),
                                 column(10,
                                        column(3,
                                               plotOutput("sum_m21", height = "7em")),
                                        column(3,
                                               plotOutput("sum_m22", height = "7em")),
                                        column(3,
                                               plotOutput("sum_m23", height = "7em")),
                                        column(3,
                                               plotOutput("sum_m24", height = "7em")))
                               ),
                               hr(style = "margin: 0em;"),
                               
                               fluidRow(
                                 column(2, style = "padding-top: 1.5em;",
                                        p("likelihood to be doing paid work at the age of 35",
                                          style = "font-size: 0.8em; ")),
                                 column(10,
                                        column(3,
                                               plotOutput("sum_m31", height = "7em")),
                                        column(3,
                                               plotOutput("sum_m32", height = "7em")),
                                        column(3,
                                               plotOutput("sum_m33", height = "7em")),
                                        column(3,
                                               plotOutput("sum_m34", height = "7em")))
                               ),
                               hr(style = "margin: 0em;"),
                               
                               fluidRow(
                                 column(2,
                                        p("likelihood to be making 3,000+ bolivianos (~PPP$37/day) per month at the age of 45",
                                          style = "font-size: 0.8em; ")),
                                 column(10,
                                        column(3,
                                               plotOutput("sum_m41", height = "7em")),
                                        column(3,
                                               plotOutput("sum_m42", height = "7em")),
                                        column(3,
                                               plotOutput("sum_m43", height = "7em")),
                                        column(3,
                                               plotOutput("sum_m44", height = "7em")))
                               ),
                               hr(style = "margin: 0em;"),
                               
                               fluidRow(
                                 column(2, style = "padding-top: 1.5em;",
                                        p("likelihood to be retired at the age of 65",
                                          style = "font-size: 0.8em; ")),
                                 column(10,
                                        column(3,
                                               plotOutput("sum_m51", height = "7em")),
                                        column(3,
                                               plotOutput("sum_m52", height = "7em")),
                                        column(3,
                                               plotOutput("sum_m53", height = "7em")),
                                        column(3,
                                               plotOutput("sum_m54", height = "7em")))
                               ),
                               hr(style = "margin: 0em;"),
                               
                               p("unit: percentage point", style = "font-style: italic; font-size: 0.8em; text-align: right; padding-bottom: 3em; ")
                        ),
                        column(2,
                               fixedPanel(
                                 tags$a(img(src = "GitHub-Mark.png", style = "width: 32px; "), href = "https://github.com/ccsuehara/bolivia_labor"),
                                 tags$a(img(src = "cloud-computing.png", style = "width: 32px; "),
                                        href = "https://www.ine.gob.bo/index.php/censos-y-banco-de-datos/censos/bases-de-datos-encuestas-sociales/"),
                                 right = 25, top = 85
                               )
                        )))
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
                !is.na(madlib_df$primary_job) & startsWith(madlib_df$sec_job, "1") ~ paste(madlib_pron1, "mainly works as a", tolower(madlib_df$primary_job), "for", madlib_df$primary_work_week_hr, "hours per week, but", madlib_pron2, "also has a second job for another", madlib_df$sec_work_week_hr, "weekly hours."),
                is.na(madlib_df$primary_job) ~ paste(madlib_pron1, "does not have a job.")),
      ifelse(round(madlib_df$lab_monthly_inc, 0) == 0,
             paste(madlib_pron1, "does not earn any income from", madlib_pron3, "work."),
             paste("In total,", madlib_pron2, "makes", round(madlib_df$lab_monthly_inc, 0), "Bolivianos every month."))
    )
  })
  
  output$children_intro <- renderText(children_intro1)
  output$children_t2 <- renderText(children_t21)
  
  output$children_t4 <- renderText(children_t41)
  output$children_t5 <- renderText(children_t51)
  
  output$children_t6 <- renderText(children_t61)
  output$children_t8 <- renderText(children_t81)
  output$children_t9 <- renderText(children_t91)
  output$children_t10 <- renderText(children_t101)
  output$children_t11 <- renderText(children_t111)
  
  
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
  
  # output$children_edu3 <- renderD3tree2(d3tree2(treemap(why_not_in_school_df,
  #                                                       index = c("why_not_in_school", "sex"),
  #                                                       vSize = "sum",
  #                                                       type = "index",
  #                                                       palette = color_pal,
  #                                                       title = "",
  #                                                       fontsize.labels = c(13, 10),
  #                                                       fontcolor.labels = c("white"),
  #                                                       fontface.labels = c(2, 3),
  #                                                       bg.labels = c("transparent"),
  #                                                       align.labels = list(c("center", "center"),
  #                                                                           c("left", "bottom")),
  #                                                       overlap.labels = 0.5,
  #                                                       border.col = c("white")),
  #                                               rootname = "Reasons for not enrolling in school: "))
  
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
      ggplot(children %>% filter(!is.na(lab_monthly_inc) & hh_tot_inc != 0)) +
        geom_point(aes(hh_tot_inc + 1, y = hh_lab_inc_pct, size = tot_work_week_hr, color = sex), alpha = 0.15) +
        geom_smooth(aes(hh_tot_inc + 1, hh_lab_inc_pct, color = sex), alpha = 0.1) +
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
      ggplot(children %>% filter(!is.na(lab_monthly_inc) & hh_tot_inc != 0)) +
        geom_jitter(aes(hh_tot_inc + 1, y = hh_hr_pct, size = lab_monthly_inc, color = sex), alpha = 0.15) +
        geom_smooth(aes(hh_tot_inc + 1, hh_hr_pct, color = sex), alpha = 0.1) +
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
  output$youth_intro <- renderText(youth_intro1)
  
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
  output$youth_edu_t1 <- renderText(youth_edu_t11)
  output$youth_edu_marital <- renderText(youth_edu_marital1)
  output$youth_edu_internet <- renderText(youth_edu_internet1)
  output$youth_edu_area <- renderText(youth_edu_area1)
  output$youth_edu_indi <- renderText(youth_edu_indi1)
  output$youth_edu_depto <- renderText(youth_edu_depto1)
  output$youth_edu_lang <- renderText(youth_edu_lang1)
  output$youth_edu_t2 <- renderText(youth_edu_t21)
  
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
  
  output$youth_emp_overview <- renderText(youth_emp_ov)
  output$youth_emp_stu <- renderText(youth_emp_stu1)
  output$youth_emp_edu <- renderText(youth_emp_edu1)
  output$youth_emp_marital <- renderText(youth_emp_marital1)
  output$youth_emp_area <- renderText(youth_emp_area1)
  
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
  output$youth_inc_t1 <- renderText(youth_inc_t11)
  output$youth_inc_t2 <- renderText(youth_inc_t21)
  
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
  
  output$lmarket_intro <- renderText(labor_intro)
  
  output$adult_educ <- renderPlot(area_chart_sex(adults))
  output$wfl_labor <-  renderPlot(waffl_work(emp_per))
  
  output$labor_rf <-  renderPlot(emp_rfplot)
  
  output$labor_hrs <-  renderPlot(hours_worked_graph(adults))
  
  output$labor_t_1 <- renderText(labor_txt_1)
  output$labor_t_2 <- renderText(labor_txt_2)
  output$labor_t_3 <- renderText(labor_txt_3)
  output$labor_t_4 <- renderText(labor_txt_4)

  # Tab panel: neets  --------------------------------
  
  output$neet_t1 <- renderText(neet_txt_1)
  output$neet_t2 <- renderText(neet_txt_2)
  output$neet_t3 <- renderText(neet_txt_3)
  output$neet_t4 <- renderText(neet_txt_4)
  output$neet_t5 <- renderText(neet_txt_5)
  
  
  output$neet_p1 <-  renderPlot(area_neet_cat_sex(ages_neet))
  output$neet_p2 <-  renderPlot(waffl_neet(neets_waff))
  output$neet_p3 <-  renderPlot(plot_bars_neet(why_neet_no_work))
  output$neet_p4 <-  renderPlot(plot_bars_neet_study(why_neet_no_study))
  
  output$neet_p5 <-  renderPlot(neets_rfplot)
  
  
  neet_decision_var <- reactive(input$neet_vars)
  
  output$neets_dec_graph <- renderPlot(
    if (neet_decision_var() == "internet") {
      ages_neet %>% mutate(decile = cut(pc_inc, 
                                        breaks = unique(quantile(pc_inc, probs = seq.int(0, 1, by = 0.1))), 
                                        include.lowest = T)) %>%
        group_by(sex, decile, internet_use) %>%
        summarize(mean = mean(neet_cat == "NEET") * 100, count = n()) %>%
        mutate(decile = as.numeric(decile)) %>%
        ggplot() +
        geom_line(aes(decile, mean, color = internet_use), size = 1) +
        facet_wrap(vars(sex), labeller = labeller(sex = c("1.Hombre" = "men", "2.Mujer" = "women"))) +
        theme_minimal() +
        theme(legend.position = "bottom", panel.grid.minor = element_blank()) +
        scale_color_manual(values = color_pal[1:2], labels = c("with Internet", "without Internet")) +
        scale_x_continuous(breaks = 1:10) +
        labs(x = "per capita household income decile", y = "% NEET", color = "") + ylim(0, 100)
    } else if (neet_decision_var() == "area") {
      neet_pop_p(ages_neet, "area", c("rural", "urban"))
    } else if (neet_decision_var() == "indi") {
      neet_pop_p(ages_neet %>% mutate(indigenous = ifelse(startsWith(indigenous, "3"), "2. No pertenece", indigenous)),
                 "indigenous", c("indigenous", "not indigenous"))
    } else if (neet_decision_var() == "depto") {
      youth_depto <- ages_neet %>%
        group_by(depto, area, sex) %>%
        summarize(mean = mean(neet_cat == "NEET") * 100)
      
      ggplot(youth_depto) +
        geom_col(aes(mean, depto, fill = sex), position = "dodge", width = 0.5) +
        facet_wrap(vars(area), labeller = labeller(area = c("Rural" = "rural", "Urbana" = "urban"))) +
        theme_minimal() +
        theme(legend.position = "bottom", panel.grid.minor = element_blank()) +
        scale_fill_manual(values = c(color1, color2), labels = c("men", "women")) +
        labs(x = "% NEET", y = "", fill = "")
    } else if (neet_decision_var() == "lang") {
      youth_lang <- ages_neet %>%
        mutate(language_1 = ifelse(language_1 %in% c("QUECHUA", "CASTELLANO"), language_1, "OTHER"),
               indigenous = ifelse(startsWith(indigenous, "3"), "2. No pertenece", indigenous)) %>%
        group_by(language_1, sex, indigenous) %>%
        summarize(mean = mean(neet_cat == "NEET") * 100)
      
      ggplot(youth_lang) +
        geom_col(aes(mean, language_1, fill = sex), position = "dodge", width = 0.5) +
        facet_wrap(vars(indigenous), labeller = labeller(indigenous = c("1. Pertenece" = "indigenous", "2. No pertenece" = "not indigenous"))) +
        theme_minimal() +
        theme(legend.position = "bottom", panel.grid.minor = element_blank()) +
        scale_fill_manual(values = c(color1, color2), labels = c("men", "women")) +
        labs(x = "% NEET", y = "", fill = "")
    } else if (neet_decision_var() == "marital") {
      neet_pop_p(ages_neet %>% filter(str_detect(marital, "^[1-3]")), "marital", c("single", "married", "cohabiting"))
    }
  )
  
  # Tab panel: paid and unpaid labor --------------------------------
  output$pay_intro <- renderText(pay_intro1)
  output$pay_t1 <- renderText(pay_t11)
  output$pay_t2 <- renderText(pay_t21)
  output$pay_t3 <- renderText(pay_t31)
  output$pay_t4 <- renderText(pay_t41)
  output$pay_t5 <- renderText(pay_t51)
  output$pay_t6 <- renderText(pay_t61)
  output$pay_t7 <- renderText(pay_t71)
  
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
      xlab("% unpaid workers") + ylab("")
  )
  
  output$pay_cell1 <- renderPlot(
    ggplot(adults %>% filter(paid == "unpaid")) +
      geom_density(aes(hh_lab_inc, fill = sex), alpha = 0.35, color = "grey") +
      facet_wrap(vars(cellphone), labeller = labeller(cellphone = c("1. Si" = "with cellphone", "2. No" = "without cellphone"))) +
      theme_minimal() +
      theme(legend.position = "bottom", legend.title = element_blank(), panel.grid.minor = element_blank(), axis.text.y = element_blank()) +
      scale_fill_manual(values = c(color1, color2), labels = c("men", "women")) +
      xlim(0, 40000) + xlab("household monthly labor income (BOB)")
  )
  
  
  # Tab panel: older adults -----------------------------------------
  output$older_t1 <- renderText(older_t11)
  output$older_t2 <- renderText(older_t21)
  output$older_t3 <- renderText(older_t31)
  output$older_t4 <- renderText(older_t41)
  
  output$older_p1 <- renderPlot(
    older %>%
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
      mutate(primary_job = ifelse(!is.na(primary_job), "1 with job", "2 without job")) %>%
      arrange(primary_job, sex) %>%
      mutate(smooth = c(fit3$fitted, fit4$fitted, fit1$fitted, fit2$fitted)) %>%
      ggplot() +
      geom_jitter(aes(age, .data[[var]], color = primary_job), width = 1, alpha = 0.1) +
      geom_line(aes(age, smooth, color = primary_job), size = 1) +
      facet_wrap(vars(sex), labeller = labeller(sex = c("1.Hombre" = "men", "2.Mujer" = "women"))) +
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
      theme_minimal() +
      theme(panel.grid.minor = element_blank(), legend.position = "none") +
      scale_x_continuous(trans = 'log10') +
      xlab("monthly labor income (BOB)") + ylab("age group") +
      scale_color_manual(values = c("#E5E5E5", color1))
  )
  
  
  # Tab panel: summary -----------------------
  output$sum_t1 <- renderText(summ1)
  
  # representative ages
  a1 <- 16.5  # working
  a2 <- 23  # in school
  a3 <- 35  # paid work
  a4 <- 45  # earning X bolivianos per month
  a5 <- 65  # retired
  
  m1 <- function(rep_age, range, opp) {
    df <- personas %>%
      filter(between(age, rep_age - range, rep_age + range))
    
    df <- if (opp == 1) {
      df %>%
        group_by(sex)
    } else if (opp == 2) {
      df %>%
        filter((area == "Urbana" & depto == "La Paz") | (area == "Rural" & depto == "Potosi")) %>%
        group_by(depto)
    } else if (opp == 3) {
      df %>%
        mutate(decile = cut(pc_inc, 
                            breaks = unique(quantile(pc_inc, probs = seq.int(0, 1, by = 0.1))), 
                            include.lowest = T)) %>%
        mutate(decile = as.numeric(decile)) %>%
        filter(decile %in% c(1, 10)) %>%
        mutate(decile = ifelse(decile == 1, "low", "high")) %>%
        group_by(decile)
    } else if (opp == 4) {
      df %>%
        filter(!is.na(marital)) %>%
        mutate(mar = ifelse(str_detect(marital, "^[1456]"), "1single", "2married")) %>%
        group_by(mar)
    }
    
    df <- if (rep_age == a1) {
      df %>% summarize(mean = mean(!is.na(primary_job)) * 100) %>% pull(mean)
    } else if (rep_age == a2) {
      df %>% summarize(mean = mean(in_school == "1. Si") * 100) %>% pull(mean)
    } else if (rep_age == a3) {
      df %>% mutate(paid = ifelse(is.na(paid), "unpaid", paid)) %>% summarize(mean = mean(paid == "paid") * 100) %>% pull(mean)
    } else if (rep_age == a4) {
      df %>% summarize(mean = mean(tot_monthly_inc >= 3000) * 100) %>% pull(mean)
    } else if (rep_age == a5) {
      df %>% summarize(mean = mean(is.na(primary_job)) * 100) %>% pull(mean)
    }
    
    data.frame(x = df[2] - df[1], y = 1)
  }
  
  m_plot <- function(df) {
    neg <- ifelse(df$x < 0, T, F)
    
    p <- df %>%
      ggplot() +
      geom_col(aes(x, y, fill = x > 0), width = 0.3, orientation = "y") +
      theme_minimal() +
      theme(panel.grid.minor = element_blank(), panel.grid.major.y = element_blank(),
            axis.text = element_blank(), legend.position = "none", axis.title = element_blank()) +
      scale_fill_manual(values = setNames(c(color2, color1), c(T, F))) +
      ylim(0.5, 1.5) +
      scale_x_continuous(breaks = c(0), limits = c(-110, 110)) + coord_fixed(ratio = 140)
    
    if (neg) {
      p + geom_text(aes(x = x, y = 1, label = round(x)), hjust = "right", nudge_x = -5)
    } else {
      p + geom_text(aes(x = x, y = 1, label = round(x)), hjust = "left", nudge_x = 5)
    }
  }
  
  output$sum_m11 <- renderPlot(m_plot(m1(a1, 0.5, 1)))
  output$sum_m12 <- renderPlot(m_plot(m1(a1, 0.5, 2)))
  output$sum_m13 <- renderPlot(m_plot(m1(a1, 0.5, 3)))
  # output$sum_m14 <- renderText("-")
  
  output$sum_m21 <- renderPlot(m_plot(m1(a2, 1, 1)))
  output$sum_m22 <- renderPlot(m_plot(m1(a2, 1, 2)))
  output$sum_m23 <- renderPlot(m_plot(m1(a2, 1, 3)))
  output$sum_m24 <- renderPlot(m_plot(m1(a2, 1, 4)))
  
  output$sum_m31 <- renderPlot(m_plot(m1(a3, 3, 1)))
  output$sum_m32 <- renderPlot(m_plot(m1(a3, 3, 2)))
  output$sum_m33 <- renderPlot(m_plot(m1(a3, 3, 3)))
  output$sum_m34 <- renderPlot(m_plot(m1(a3, 3, 4)))
  
  output$sum_m41 <- renderPlot(m_plot(m1(a4, 3, 1)))
  output$sum_m42 <- renderPlot(m_plot(m1(a4, 3, 2)))
  output$sum_m43 <- renderPlot(m_plot(m1(a4, 3, 3)))
  output$sum_m44 <- renderPlot(m_plot(m1(a4, 3, 4)))
  
  output$sum_m51 <- renderPlot(m_plot(m1(a5, 3, 1)))
  output$sum_m52 <- renderPlot(m_plot(m1(a5, 3, 2)))
  output$sum_m53 <- renderPlot(m_plot(m1(a5, 3, 3)))
  output$sum_m54 <- renderPlot(m_plot(m1(a5, 3, 4)))
  
  
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
  
  # Adults - entering the job market
  observeEvent(input$to_youth2, {
    updateNavbarPage(session, "main", selected = "youth")
  })
  observeEvent(input$to_pay, {
    updateNavbarPage(session, "main", selected = "pay")
  })
  
  # Adults - paid and unpaid labor
  observeEvent(input$to_employment2, {
    updateNavbarPage(session, "main", selected = "employment")
  })
  observeEvent(input$to_neet, {
    updateNavbarPage(session, "main", selected = "neet")
  })
  
  # Adults - neet
  observeEvent(input$to_pay2, {
    updateNavbarPage(session, "main", selected = "pay")
  })
  observeEvent(input$to_older, {
    updateNavbarPage(session, "main", selected = "older")
  })
  
  # Older adults
  observeEvent(input$to_neet2, {
    updateNavbarPage(session, "main", selected = "neet")
  })
  observeEvent(input$to_sum, {
    updateNavbarPage(session, "main", selected = "sum")
  })
  
  # Summary
  observeEvent(input$to_older2, {
    updateNavbarPage(session, "main", selected = "older")
  })
}

shinyApp(ui = ui, server = server)
