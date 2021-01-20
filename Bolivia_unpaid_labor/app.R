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
                                           width = "1130px",
                                           click = clickOpts(id = "landing_cl")))),
             
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
                               
                               textOutput("youth_intro"),
                               h3("Youth overview"),
                               plotOutput("youth_overview"),
                               hr(),
                               
                               fluidRow(style = 'background-image: url("education_bw.jpg"); background-size: cover;',
                                        column(12, style = "padding: 80px 50px;",
                                               h3("EDUCATION", style = "color: white;"))),
                               hr(),
                               textOutput("youth_edu_t1"),
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
                               selectInput("youth_edu_ses",
                                           label = "",
                                           choices = c("Internet access" = "internet",
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
                               
                               fluidRow(style = 'background-image: url("employment_bw.jpg"); background-size: cover;',
                                        column(12, style = "padding: 80px 50px;",
                                               h3("EMPLOYMENT", style = "color: white;"))),
                               hr(),
                               p("Under development. Check back later for updates :)"),
                               hr(),
                               
                               fluidRow(style = 'background-image: url("income_bw.jpg"); background-size: cover;',
                                        column(12, style = "padding: 80px 50px;",
                                               h3("INCOME", style = "color: white; text-shadow: 2px 2px 2px black;"))),
                               hr(),
                               p("Under development. Check back later for updates :)")),
                        
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
                                            actionButton("to_employment", label = "< adults - entering the job market"),
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
                        
                        tabPanel("NEET population",
                                 value = "neet",
                                 
                                 p("Under development. Check back later for updates :)"))),
             
             # Tab panel: older adults 60+ --------------------
             tabPanel("Older adults 60+",
                      value = "older",
                      
                      p("Under development. Check back later for updates :)"))
             
             # Tab panel: Indigenous --------------------
             # tabPanel("Indigenous identity",
             #          value = "indigenous",
             #          
             #          h2("Overall indigenous population"),
             #          fluidRow(column(10, offset = 1,
             #                          h4(textOutput("indi_all_t")))),
             #          fluidRow(
             #            column(4,
             #                   p("All"),
             #                   plotOutput("indi_all")),
             #            column(4,
             #                   p("Women"),
             #                   plotOutput("indi_w")),
             #            column(4,
             #                   p("Men"),
             #                   plotOutput("indi_m"))
             #          ),
             #          hr(),
             #          
             #          h2("Indigenous people and labor"),
             #          fluidRow(column(10, offset = 1,
             #                          h4(textOutput("indi_work_t")))),
             #          fluidRow(
             #            column(4,
             #                   p(c1),
             #                   plotOutput("pop_indi_work")),
             #            column(4,
             #                   p(c2),
             #                   plotOutput("with_job_indi_work")),
             #            column(4,
             #                   p(c3),
             #                   plotOutput("unpaid_indi_work"))
             #          ),
             #          hr(),
             #          
             #          h2("Indigenous people in rural areas"),
             #          fluidRow(column(10, offset = 1,
             #                          h4(textOutput("indi_r_t1")))),
             #          fluidRow(
             #            column(4,
             #                   p("All"),
             #                   plotOutput("indi_r_all")),
             #            column(4,
             #                   p("Women"),
             #                   plotOutput("indi_r_w")),
             #            column(4,
             #                   p("Men"),
             #                   plotOutput("indi_r_m"))
             #          ),
             #          hr(),
             #          fluidRow(column(10, offset = 1,
             #                          h4(textOutput("indi_r_t2")))),
             #          fluidRow(
             #            column(4,
             #                   p(c1),
             #                   plotOutput("pop_indi_r")),
             #            column(4,
             #                   p(c2),
             #                   plotOutput("with_job_indi_r")),
             #            column(4,
             #                   p(c3),
             #                   plotOutput("unpaid_indi_r"))
             #          ),
             #          hr(),
             #          
             #          h2("Indigenous people in urban areas"),
             #          fluidRow(column(10, offset = 1,
             #                          h4(textOutput("indi_u_t1")))),
             #          fluidRow(
             #            column(4,
             #                   p("All"),
             #                   plotOutput("indi_u_all")),
             #            column(4,
             #                   p("Women"),
             #                   plotOutput("indi_u_w")),
             #            column(4,
             #                   p("Men"),
             #                   plotOutput("indi_u_m"))
             #          ),
             #          hr(),
             #          fluidRow(column(10, offset = 1,
             #                          h4(textOutput("indi_u_t2")))),
             #          fluidRow(
             #            column(4,
             #                   p(c1),
             #                   plotOutput("pop_indi_u")),
             #            column(4,
             #                   p(c2),
             #                   plotOutput("with_job_indi_u")),
             #            column(4,
             #                   p(c3),
             #                   plotOutput("unpaid_indi_u"))
             #          )),

             # Tab panel: Figures --------------------
             # tabPanel("Figures",
             #          box(h2("Column overview"),
             #              fluidRow(
             #                column(3,
             #                       p(paste(c1, "(2018)")),
             #                       h1(textOutput("pop")),
             #                       p("people (est.)")),
             #                column(3,
             #                       p(c2),
             #                       h1(textOutput("with_job")),
             #                       p("people (est.)")),
             #                column(3,
             #                       p(c3),
             #                       h1(textOutput("unpaid")),
             #                       p("people (est.)")),
             #                column(3,
             #                       p(c4),
             #                       h1(textOutput("unpaid_sec")),
             #                       p("people (est.)"))
             #              ),
             #              width = 12,
             #              collapsible = T),
             #          box(hr(), width = 12),
             #          
             #          box(h2("Age"),
             #              h4(textOutput("age_t")),
             #              fluidRow(
             #                column(3,
             #                       p(c1),
             #                       plotOutput("pop_age")),
             #                column(3,
             #                       p(c2),
             #                       plotOutput("with_job_age")),
             #                column(3,
             #                       p(c3),
             #                       plotOutput("unpaid_age")),
             #                column(3,
             #                       p(c4),
             #                       plotOutput("unpaid_sec_age"))
             #              ),
             #              width = 12,
             #              collapsible = T),
             #          box(hr(), width = 12),
             #          
             #          box(h2("School attendance"),
             #              h4(textOutput("sch_t")),
             #              fluidRow(
             #                column(3,
             #                       p(c1),
             #                       plotOutput("pop_sch")),
             #                column(3,
             #                       p(c2),
             #                       plotOutput("with_job_sch")),
             #                column(3,
             #                       p(c3),
             #                       plotOutput("unpaid_sch")),
             #                column(3,
             #                       p(c4),
             #                       plotOutput("unpaid_sec_sch"))
             #              ),
             #              width = 12,
             #              collapsible = T),
             #          box(hr(), width = 12),
             #          
             #          box(h2("Urban/rural area"),
             #              h4(textOutput("ur_t")),
             #              fluidRow(
             #                column(3,
             #                       p(c1),
             #                       plotOutput("pop_ur")),
             #                column(3,
             #                       p(c2),
             #                       plotOutput("with_job_ur")),
             #                column(3,
             #                       p(c3),
             #                       plotOutput("unpaid_ur")),
             #                column(3,
             #                       p(c4),
             #                       plotOutput("unpaid_sec_ur"))
             #              ),
             #              width = 12,
             #              collapsible = T),
             #          box(hr(), width = 12),
             #          
             #          box(h2("Indigenous identity"),
             #              fluidRow(
             #                column(3,
             #                       p(c1),
             #                       plotOutput("pop_indi"),
             #                       plotOutput("pop_indi_p"),
             #                       plotOutput("pop_indi_id")),
             #                column(3,
             #                       p(c2),
             #                       plotOutput("with_job_indi"),
             #                       plotOutput("with_job_indi_p"),
             #                       plotOutput("with_job_indi_id")),
             #                column(3,
             #                       p(c3),
             #                       plotOutput("unpaid_indi"),
             #                       plotOutput("unpaid_indi_p"),
             #                       plotOutput("unpaid_indi_id")),
             #                column(3,
             #                       p(c4),
             #                       plotOutput("unpaid_sec_indi"),
             #                       plotOutput("unpaid_sec_indi_p"),
             #                       plotOutput("unpaid_sec_indi_id"))
             #              ),
             #              width = 12,
             #              collapsible = T),
             #          box(hr(), width = 12),
             #          
             #          box(h2("Marital status"),
             #              h4(textOutput("mari_t")),
             #              fluidRow(
             #                column(3,
             #                       p(c1),
             #                       plotOutput("pop_mari")),
             #                column(3,
             #                       p(c2),
             #                       plotOutput("with_job_mari")),
             #                column(3,
             #                       p(c3),
             #                       plotOutput("unpaid_mari")),
             #                column(3,
             #                       p(c4),
             #                       plotOutput("unpaid_sec_mari"))
             #              ),
             #              fluidRow(
             #                column(3,
             #                       p(c1),
             #                       plotOutput("pop_mari_age")),
             #                column(3,
             #                       p(c2),
             #                       plotOutput("with_job_mari_age")),
             #                column(3,
             #                       p(c3),
             #                       plotOutput("unpaid_mari_age")),
             #                column(3,
             #                       p(c4),
             #                       plotOutput("unpaid_sec_mari_age"))
             #              ),
             #              width = 12,
             #              collapsible = T),
             #          box(hr(), width = 12),
             #          
             #          box(h2("Number of children"),
             #              h4(textOutput("kid_t")),
             #              fluidRow(
             #                column(3,
             #                       p(c1),
             #                       plotOutput("pop_kid")),
             #                column(3,
             #                       p(c2),
             #                       plotOutput("with_job_kid")),
             #                column(3,
             #                       p(c3),
             #                       plotOutput("unpaid_kid")),
             #                column(3,
             #                       p(c4),
             #                       plotOutput("unpaid_sec_kid"))
             #              ),
             #              width = 12,
             #              collapsible = T),
             #          box(hr(), width = 12),
             #          
             #          box(h2("Literacy"),
             #              h4(textOutput("lit_t")),
             #              fluidRow(
             #                column(3,
             #                       p(c1),
             #                       plotOutput("pop_lit")),
             #                column(3,
             #                       p(c2),
             #                       plotOutput("with_job_lit")),
             #                column(3,
             #                       p(c3),
             #                       plotOutput("unpaid_lit")),
             #                column(3,
             #                       p(c4),
             #                       plotOutput("unpaid_sec_lit"))
             #              ),
             #              width = 12,
             #              collapsible = T),
             #          box(hr(), width = 12),
             #          
             #          box(h2("Education"),
             #              h4(textOutput("edu_t")),
             #              fluidRow(
             #                column(3,
             #                       p(c1),
             #                       plotOutput("pop_edu")),
             #                column(3,
             #                       p(c2),
             #                       plotOutput("with_job_edu")),
             #                column(3,
             #                       p(c3),
             #                       plotOutput("unpaid_edu")),
             #                column(3,
             #                       p(c4),
             #                       plotOutput("unpaid_sec_edu"))
             #              ),
             #              h4(textOutput("any_ed_t")),
             #              fluidRow(
             #                column(3,
             #                       p(c1),
             #                       plotOutput("pop_any_ed")),
             #                column(3,
             #                       p(c2),
             #                       plotOutput("with_job_any_ed")),
             #                column(3,
             #                       p(c3),
             #                       plotOutput("unpaid_any_ed")),
             #                column(3,
             #                       p(c4),
             #                       plotOutput("unpaid_sec_any_ed"))
             #              ),
             #              fluidRow(
             #                column(3,
             #                       p(c1),
             #                       plotOutput("pop_higher_ed")),
             #                column(3,
             #                       p(c2),
             #                       plotOutput("with_job_higher_ed")),
             #                column(3,
             #                       p(c3),
             #                       plotOutput("unpaid_higher_ed")),
             #                column(3,
             #                       p(c4),
             #                       plotOutput("unpaid_sec_higher_ed"))
             #              ),
             #              h4(textOutput("any_ed2_t")),
             #              fluidRow(
             #                column(3,
             #                       p(c1),
             #                       plotOutput("pop_any_ed2")),
             #                column(3,
             #                       p(c2),
             #                       plotOutput("with_job_any_ed2")),
             #                column(3,
             #                       p(c3),
             #                       plotOutput("unpaid_any_ed2")),
             #                column(3,
             #                       p(c4),
             #                       plotOutput("unpaid_sec_any_ed2"))
             #              ),
             #              fluidRow(
             #                column(3,
             #                       p(c1),
             #                       plotOutput("pop_higher_ed2")),
             #                column(3,
             #                       p(c2),
             #                       plotOutput("with_job_higher_ed2")),
             #                column(3,
             #                       p(c3),
             #                       plotOutput("unpaid_higher_ed2")),
             #                column(3,
             #                       p(c4),
             #                       plotOutput("unpaid_sec_higher_ed2"))
             #              ),
             #              width = 12,
             #              collapsible = T),
             #          box(hr(), width = 12),
             #          
             #          box(h2("Chronic disease"),
             #              fluidRow(
             #                column(3,
             #                       p(c1),
             #                       plotOutput("pop_disease")),
             #                column(3,
             #                       p(c2),
             #                       plotOutput("with_job_disease")),
             #                column(3,
             #                       p(c3),
             #                       plotOutput("unpaid_disease")),
             #                column(3,
             #                       p(c4),
             #                       plotOutput("unpaid_sec_disease"))
             #              ),
             #              width = 12,
             #              collapsible = T),
             #          box(hr(), width = 12),
             #          
             #          box(h2("Technology"),
             #              h4(textOutput("tech1")),
             #              fluidRow(
             #                column(3,
             #                       p(c1),
             #                       plotOutput("pop_cell1")),
             #                column(3,
             #                       p(c2),
             #                       plotOutput("with_job_cell1")),
             #                column(3,
             #                       p(c3),
             #                       plotOutput("unpaid_cell1")),
             #                column(3,
             #                       p(c4),
             #                       plotOutput("unpaid_sec_cell1"))
             #              ),
             #              fluidRow(
             #                column(3,
             #                       p(c1),
             #                       plotOutput("pop_cell2")),
             #                column(3,
             #                       p(c2),
             #                       plotOutput("with_job_cell2")),
             #                column(3,
             #                       p(c3),
             #                       plotOutput("unpaid_cell2")),
             #                column(3,
             #                       p(c4),
             #                       plotOutput("unpaid_sec_cell2"))
             #              ),
             #              h4(textOutput("tech2")),
             #              fluidRow(
             #                column(3,
             #                       p(c1),
             #                       plotOutput("pop_internet1")),
             #                column(3,
             #                       p(c2),
             #                       plotOutput("with_job_internet1")),
             #                column(3,
             #                       p(c3),
             #                       plotOutput("unpaid_internet1")),
             #                column(3,
             #                       p(c4),
             #                       plotOutput("unpaid_sec_internet1"))
             #              ),
             #              fluidRow(
             #                column(3,
             #                       p(c1),
             #                       plotOutput("pop_internet2")),
             #                column(3,
             #                       p(c2),
             #                       plotOutput("with_job_internet2")),
             #                column(3,
             #                       p(c3),
             #                       plotOutput("unpaid_internet2")),
             #                column(3,
             #                       p(c4),
             #                       plotOutput("unpaid_sec_internet2"))
             #              ),
             #              h4(textOutput("tech3")),
             #              fluidRow(
             #                column(3,
             #                       p(c1),
             #                       plotOutput("pop_inter_home1")),
             #                column(3,
             #                       p(c2),
             #                       plotOutput("with_job_inter_home1")),
             #                column(3,
             #                       p(c3),
             #                       plotOutput("unpaid_inter_home1")),
             #                column(3,
             #                       p(c4),
             #                       plotOutput("unpaid_sec_inter_home1"))
             #              ),
             #              fluidRow(
             #                column(3,
             #                       p(c1),
             #                       plotOutput("pop_inter_home2")),
             #                column(3,
             #                       p(c2),
             #                       plotOutput("with_job_inter_home2")),
             #                column(3,
             #                       p(c3),
             #                       plotOutput("unpaid_inter_home2")),
             #                column(3,
             #                       p(c4),
             #                       plotOutput("unpaid_sec_inter_home2"))
             #              ),
             #              width = 12,
             #              collapsible = T),
             #          box(hr(), width = 12),
             #          
             #          box(h2("Union member"),
             #              fluidRow(
             #                column(3,
             #                       p(c1),
             #                       plotOutput("pop_union1")),
             #                column(3,
             #                       p(c2),
             #                       plotOutput("with_job_union1")),
             #                column(3,
             #                       p(c3),
             #                       plotOutput("unpaid_union1")),
             #                column(3,
             #                       p(c4),
             #                       plotOutput("unpaid_sec_union1"))
             #              ),
             #              fluidRow(
             #                column(3,
             #                       p(c1),
             #                       plotOutput("pop_union2")),
             #                column(3,
             #                       p(c2),
             #                       plotOutput("with_job_union2")),
             #                column(3,
             #                       p(c3),
             #                       plotOutput("unpaid_union2")),
             #                column(3,
             #                       p(c4),
             #                       plotOutput("unpaid_sec_union2"))
             #              ),
             #              width = 12,
             #              collapsible = T),
             #          box(hr(), width = 12),
             #          
             #          box(h2("Income"),
             #              h4(textOutput("salary_t")),
             #              fluidRow(
             #                column(3,
             #                       p(c1),
             #                       plotOutput("pop_salary")),
             #                column(3,
             #                       p(c2),
             #                       plotOutput("with_job_salary")),
             #                column(3,
             #                       p(c3),
             #                       plotOutput("unpaid_salary")),
             #                column(3,
             #                       p(c4),
             #                       plotOutput("unpaid_sec_salary"))
             #              ),
             #              h4(textOutput("irr_t")),
             #              fluidRow(
             #                column(3,
             #                       p(c1),
             #                       plotOutput("pop_irr")),
             #                column(3,
             #                       p(c2),
             #                       plotOutput("with_job_irr")),
             #                column(3,
             #                       p(c3),
             #                       plotOutput("unpaid_irr")),
             #                column(3,
             #                       p(c4),
             #                       plotOutput("unpaid_sec_irr"))
             #              ),
             #              width = 12,
             #              collapsible = T),
             #          box(hr(), width = 12),
             #          
             #          box(h2("Want to work more?"),
             #              h4(textOutput("want1_t")),
             #              fluidRow(
             #                column(3,
             #                       p(c1),
             #                       plotOutput("pop_want1")),
             #                column(3,
             #                       p(c2),
             #                       plotOutput("with_job_want1")),
             #                column(3,
             #                       p(c3),
             #                       plotOutput("unpaid_want1")),
             #                column(3,
             #                       p(c4),
             #                       plotOutput("unpaid_sec_want1"))
             #              ),
             #              fluidRow(
             #                column(3,
             #                       p(c1),
             #                       plotOutput("pop_want2")),
             #                column(3,
             #                       p(c2),
             #                       plotOutput("with_job_want2")),
             #                column(3,
             #                       p(c3),
             #                       plotOutput("unpaid_want2")),
             #                column(3,
             #                       p(c4),
             #                       plotOutput("unpaid_sec_want2"))
             #              ),
             #              width = 12,
             #              collapsible = T)),
             
             # Tab panel: stories -----------------------------
             # tabPanel("Stories",
             #          h3(textOutput("t1")),
             #          actionButton("b1", label = "Tell me a story", icon = icon("redo")),
             #          tableOutput("c1"),
             #          hr(),
             #          h3(textOutput("t2")),
             #          actionButton("b2", label = "Tell me a story", icon = icon("redo")),
             #          tableOutput("c2"),
             #          hr(),
             #          h3(textOutput("t3")),
             #          actionButton("b3", label = "Tell me a story", icon = icon("redo")),
             #          tableOutput("c3"),
             #          hr(),
             #          h3(textOutput("t4")),
             #          actionButton("b4", label = "Tell me a story", icon = icon("redo")),
             #          tableOutput("c4"),
             #          hr(),
             #          h3(textOutput("t5")),
             #          actionButton("b5", label = "Tell me a story", icon = icon("redo")),
             #          tableOutput("c5"))
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
  
  # unused --------------
  # output$children_overview1 <- renderPlot(
  #   ggplot(children) +
  #     geom_bar(aes(x = age, fill = sex), position = "stack", width = 1) +
  #     theme_minimal() +
  #     theme(legend.position = "bottom", legend.title = element_blank(), panel.grid.minor = element_blank()) +
  #     scale_x_continuous(breaks = c(7, 10, 14, 17)) +
  #     scale_fill_manual(values = c(color1, color2), labels = c("boys", "girls")) +
  #     ylab("population")
  # )
  # output$children_overview2 <- renderPlot(
  #   ggplot(children) +
  #     geom_bar(aes(x = age, fill = sex), position = "fill", width = 1) +
  #     geom_segment(aes(x = min(age) - 0.5, xend = max(age) + 0.5, y = 0.5, yend = 0.5), linetype = "dashed", color = "grey") +
  #     theme_minimal() +
  #     theme(legend.position = "bottom", legend.title = element_blank(), panel.grid.minor = element_blank()) +
  #     scale_x_continuous(breaks = c(7, 10, 14, 17)) +
  #     scale_fill_manual(values = c(color1, color2), labels = c("boys", "girls")) +
  #     ylab("proportion")
  # )
  
  # output$children_ru1 <- renderPlot(
  #   ggplot(children) +
  #     geom_bar(aes(x = area, fill = sex), position = "stack", width = 0.5) +
  #     theme_minimal() +
  #     theme(legend.position = "bottom", legend.title = element_blank(), panel.grid.minor = element_blank()) +
  #     scale_x_discrete(labels = c("Rural" = "rural", "Urbana" = "urban")) +
  #     scale_fill_manual(values = c(color1, color2), labels = c("boys", "girls")) +
  #     xlab("") + ylab("population")
  # )
  # output$children_ru2 <- renderPlot(
  #   ggplot(children) +
  #     geom_bar(aes(x = area, fill = sex), position = "fill", width = 0.5) +
  #     theme_minimal() +
  #     theme(legend.position = "bottom", legend.title = element_blank(), panel.grid.minor = element_blank()) +
  #     scale_x_discrete(labels = c("Rural" = "rural", "Urbana" = "urban")) +
  #     scale_fill_manual(values = c(color1, color2), labels = c("boys", "girls")) +
  #     xlab("") + ylab("proportion")
  # )
  
  # output$children_indi1 <- renderPlot(
  #   ggplot(children) +
  #     geom_bar(aes(x = indigenous, fill = sex), position = "stack", width = 0.5) +
  #     theme_minimal() +
  #     theme(legend.position = "bottom", legend.title = element_blank(), panel.grid.minor = element_blank()) +
  #     scale_x_discrete(labels = c("1. Pertenece" = "indigenous", "2. No pertenece" = "not\nindigenous",
  #                                 "3. No soy boliviana o boliviano" = "not\nBolivian")) +
  #     scale_fill_manual(values = c(color1, color2), labels = c("boys", "girls")) +
  #     xlab("") + ylab("population")
  # )
  # output$children_indi2 <- renderPlot(
  #   ggplot(children) +
  #     geom_bar(aes(x = indigenous, fill = sex), position = "fill", width = 0.5) +
  #     theme_minimal() +
  #     theme(legend.position = "bottom", legend.title = element_blank(), panel.grid.minor = element_blank()) +
  #     scale_x_discrete(labels = c("1. Pertenece" = "indigenous", "2. No pertenece" = "not\nindigenous",
  #                                 "3. No soy boliviana o boliviano" = "not\nBolivian")) +
  #     scale_fill_manual(values = c(color1, color2), labels = c("boys", "girls")) +
  #     xlab("") + ylab("proportion")
  # )
  # output$children_indi3 <- renderPlot(
  #   ggplot(children) +
  #     geom_bar(aes(x = age, fill = indigenous), position = "stack", width = 1) +
  #     theme_minimal() +
  #     theme(legend.position = "bottom", legend.title = element_blank(), panel.grid.minor = element_blank()) +
  #     scale_fill_manual(values = c(color1, color2, color3), labels = c("indigenous", "not\nindigenous", "not\nBolivian")) +
  #     ylab("population")
  # )
  # output$children_indi4 <- renderPlot(
  #   ggplot(children) +
  #     geom_bar(aes(x = age, fill = indigenous), position = "fill", width = 1) +
  #     theme_minimal() +
  #     theme(legend.position = "bottom", legend.title = element_blank(), panel.grid.minor = element_blank()) +
  #     scale_fill_manual(values = c(color1, color2, color3), labels = c("indigenous", "not\nindigenous", "not\nBolivian")) +
  #     ylab("proportion")
  # )
  
  # output$children_edu1 <- renderPlot(
  #   ggplot(children) +
  #     geom_bar(aes(x = edu_status, fill = sex), position = "stack", width = 0.5) +
  #     theme_minimal() +
  #     theme(legend.position = "bottom", legend.title = element_blank(), panel.grid.minor = element_blank()) +
  #     scale_fill_manual(values = c(color1, color2), labels = c("boys", "girls")) +
  #     xlab("") + ylab("population")
  # )
  # output$children_edu2 <- renderPlot(
  #   ggplot(children) +
  #     geom_bar(aes(x = edu_status, fill = sex), position = "fill", width = 0.5) +
  #     theme_minimal() +
  #     theme(legend.position = "bottom", legend.title = element_blank(), panel.grid.minor = element_blank()) +
  #     scale_fill_manual(values = c(color1, color2), labels = c("boys", "girls")) +
  #     xlab("") + ylab("proportion")
  # )
  # ----------------------------
  
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
  
  # output$children_lfp3 <- renderPlot(
  #   ggplot(children %>% filter(!is.na(lab_monthly_inc)) %>% group_by(age, sex)) +
  #     geom_jitter(aes(x = age, y = hh_lab_inc_pct, size = tot_work_week_hr, color = sex), alpha = 0.15) +
  #     geom_line(data = . %>% summarize(mean_hr = mean(tot_work_week_hr), mean_inc = mean(lab_monthly_inc), mean_pct = mean(hh_lab_inc_pct)),
  #               aes(y = mean_pct, x = age, color = sex), size = 1) +
  #     geom_point(data = . %>% summarize(mean_hr = mean(tot_work_week_hr), mean_inc = mean(lab_monthly_inc), mean_pct = mean(hh_lab_inc_pct)),
  #                aes(y = mean_pct, x = age, size = mean_hr, color = sex)) +
  #     geom_text(aes(7, 85, label = "circle size =\nweekly work hours"), color = "grey", hjust = "left") +
  #     theme_minimal() +
  #     theme(legend.position = "bottom", panel.grid.minor = element_blank()) +
  #     scale_x_continuous(limits = c(7, 17.8)) +
  #     scale_color_manual(values = c(color1, color2), labels = c("boys", "girls")) +
  #     scale_size(range = c(0.1, 10), guide = F) +
  #     labs(y = "contribution to household labor income (%)", color = "average")
  # )
  # output$children_lfp4 <- renderPlot(
  #   ggplot(children %>% filter(!is.na(lab_monthly_inc)) %>% group_by(age, sex)) +
  #     geom_jitter(aes(x = age, y = hh_hr_pct, size = lab_monthly_inc, color = sex), alpha = 0.15) +
  #     geom_line(data = . %>% summarize(mean_hr = mean(tot_work_week_hr), mean_inc = mean(lab_monthly_inc), mean_pct = mean(hh_hr_pct)),
  #               aes(y = mean_pct, x = age, color = sex), size = 1) +
  #     geom_point(data = . %>% summarize(mean_hr = mean(tot_work_week_hr), mean_inc = mean(lab_monthly_inc), mean_pct = mean(hh_hr_pct)),
  #                aes(y = mean_pct, x = age, size = mean_inc, color = sex)) +
  #     geom_text(aes(7, 85, label = "circle size =\nmonthly income (BOB)"), color = "grey", hjust = "left") +
  #     theme_minimal() +
  #     theme(legend.position = "bottom", panel.grid.minor = element_blank()) +
  #     scale_x_continuous(limits = c(7, 17.8)) +
  #     scale_color_manual(values = c(color1, color2), labels = c("boys", "girls")) +
  #     scale_size(range = c(0.1, 30), guide = F) +
  #     labs(y = "share of household work hours (%)", color = "average")
  # )
  # 
  # output$children_lfp5 <- renderPlot(
  #   ggplot(children %>% filter(!is.na(lab_monthly_inc))) +
  #     geom_point(aes(x = hh_lab_inc, y = hh_lab_inc_pct, size = tot_work_week_hr, color = sex), alpha = 0.3) +
  #     facet_wrap(vars(sex), labeller = labeller(sex = c("1.Hombre" = "boys", "2.Mujer" = "girls"))) +
  #     theme_minimal() +
  #     theme(legend.position = "bottom", panel.grid.minor = element_blank()) +
  #     scale_color_manual(values = c(color1, color2), labels = c("boys", "girls")) +
  #     scale_size(range = c(0.1, 10)) +
  #     labs(x = "monthly household labor income (BOB)", y = "contribution to household labor income (%)", size = "weekly work hours")
  # )
  # output$children_lfp6 <- renderPlot(
  #   ggplot(children %>% filter(!is.na(lab_monthly_inc))) +
  #     geom_point(aes(x = hh_lab_inc, y = hh_hr_pct, size = lab_monthly_inc, color = sex), alpha = 0.3) +
  #     facet_wrap(vars(sex), labeller = labeller(sex = c("1.Hombre" = "boys", "2.Mujer" = "girls"))) +
  #     theme_minimal() +
  #     theme(legend.position = "bottom", panel.grid.minor = element_blank()) +
  #     scale_color_manual(values = c(color1, color2), labels = c("boys", "girls")) +
  #     scale_size(range = c(0.1, 15)) +
  #     labs(x = "monthly household labor income (BOB)", y = "share of household work hours (%)", size = "monthly income (BOB)")
  # )
  # 
  # output$children_lfp7 <- renderPlot(
  #   ggplot(children %>% filter(!is.na(lab_monthly_inc))) +
  #     geom_point(aes(x = hh_tot_inc, y = hh_lab_inc_pct, size = tot_work_week_hr, color = sex), alpha = 0.3) +
  #     facet_wrap(vars(sex), labeller = labeller(sex = c("1.Hombre" = "boys", "2.Mujer" = "girls"))) +
  #     theme_minimal() +
  #     theme(legend.position = "bottom", panel.grid.minor = element_blank()) +
  #     scale_color_manual(values = c(color1, color2), labels = c("boys", "girls")) +
  #     scale_size(range = c(0.1, 10)) +
  #     labs(x = "monthly household income (labor & non-labor) (BOB)", y = "contribution to household labor income (%)", size = "weekly work hours")
  # )
  # output$children_lfp8 <- renderPlot(
  #   ggplot(children %>% filter(!is.na(lab_monthly_inc))) +
  #     geom_point(aes(x = hh_tot_inc, y = hh_hr_pct, size = lab_monthly_inc, color = sex), alpha = 0.3) +
  #     facet_wrap(vars(sex), labeller = labeller(sex = c("1.Hombre" = "boys", "2.Mujer" = "girls"))) +
  #     theme_minimal() +
  #     theme(legend.position = "bottom", panel.grid.minor = element_blank()) +
  #     scale_color_manual(values = c(color1, color2), labels = c("boys", "girls")) +
  #     scale_size(range = c(0.1, 15)) +
  #     labs(x = "monthly household income (labor & non-labor) (BOB)", y = "share of household work hours (%)", size = "monthly income (BOB)")
  # )
  
  
  # Tab panel: youth ------------------------------------------------
  output$youth_intro <- renderText("The youth period represents a pivotal moment in one's life.
                                   Faced with life-changing decisions on education and employment, and situated within diverse familial and societal contexts, youths not only shape their own life trajectories in important ways, but also determine a society's development for decades to come.
                                   Therefore, the well-being and opportunities of youths merit special consideration.
                                   The analysis in this section revolves around three aspects: educational attainment, employment, and income.
                                   We seek to highlight the socioeconomic factors that impact different youths' access to (higher) education, job opportunities, and fair pay, all through the lens of gender.
                                   As we can see in the graph here, there is already a gap in the level of employment between women and men.
                                   In the subsections below, we will delve further into the reasons and detailed manifestations of this disparity.")
  output$youth_edu_t1 <- renderText("Further education is often the gateway to more desirable employment opportunities and better life outcomes, but access to education, especially beyond the 12 years of compulsory education, is by no means equal.")
  output$youth_edu_internet <- renderText("One of the most crucial factors in determining educational attainment is Internet access.
                                          Not only do people with Internet obtain education at a much higher rate than those without, but such differential impact is also much stronger among women than among men.
                                          Of course, no simplistic causal relationship can be directed deducted from the data, and there could be other confounding factors.
                                          But we have found that the effect of Internet access still holds after controlling for household income.")
  output$youth_edu_area <- renderText("Similar to the effect of Internet access, the rural/urban disparity also plays out differently for men and women.
                                      In urban areas, young women seem to continue education at a slightly higher rate than their male counterparts, while in rural Bolivia, women are consistently less likely to remain students than men.")
  output$youth_edu_depto <- renderText("There are also variations within rural and urban areas, with departments like Oruro exhibiting some of the biggest gender gaps in education among youths.")
  output$youth_edu_indi <- renderText("Another critical social identity is indigeneity.
                                      On average, indigenous women are less educated than both indigenous men and non-indigenous women, which compromise their employment prospects and income potentials.")
  output$youth_edu_lang <- renderText("Finally, it is important to note that indigenous people are by no means a monolith.
                                      One's primary language plays a sizeable role in their education as well.
                                      Women and men who speak Castellano as their primary language obtain education at a much higher rate than those speaking Quechua or other languages, and this phenomenon is more pronounced among women than men.")
  output$youth_edu_t2 <- renderText("Overall, the education landscape for youths in Bolivia is highly uneven.
                                    More than half of youths at the age of 22 have finished tertiary education, whether it is a university or technical degree.
                                    However, it is crucial to interrogate: Who has the ability to be among the more educated half? And what happens when youths have such differential access to education?")
  
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
  
  youth_edu1_p <- function(df, var, labels) {
    df2 <- df %>%
      group_by(age, sex, .data[[var]]) %>%
      summarize(mean = mean(in_school == "1. Si") * 100)
    
    ggplot(df2) +
      geom_line(aes(age, mean, color = .data[[var]]), size = 1) +
      facet_wrap(vars(sex), labeller = labeller(sex = c("1.Hombre" = "men", "2.Mujer" = "women"))) +
      theme_minimal() +
      theme(legend.position = "bottom", panel.grid.minor = element_blank()) +
      scale_color_manual(values = c(color1, color2), labels = labels) +
      labs(y = "% student", color = "") + ylim(0, 100)
  }
  
  youth_edu1_var <- reactive(input$youth_edu_ses)
  
  output$youth_edu1 <- renderPlot(
    if (youth_edu1_var() == "internet") {
      youth_edu1_p(youth, "internet_use", c("with Internet", "without Internet"))
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
    }
  )

  output$youth_edu2 <- renderPlot(
    ggplot(youth) +
      geom_bar(aes(age, fill = education), position = "fill") +
      scale_fill_manual(values = c(color1, color2, color3, color4)) +
      theme_minimal() +
      theme(legend.position = "bottom", legend.title = element_blank()) +
      ylab("proportion")
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

  # Tab panel: age ------------------------------
  # age_all <- function(df) {
  #   ggplot(df) +
  #     geom_bar(aes(x = age, fill = sex), position = "fill") +
  #     geom_segment(aes(x = min(age) - 0.5, xend = max(age) + 0.5, y = 0.5, yend = 0.5), linetype = "dashed", color = "grey") +
  #     geom_segment(aes(x = 10, xend = 10, y = 0, yend = 1), linetype = "dotted", color = "grey") +
  #     geom_segment(aes(x = 18, xend = 18, y = 0, yend = 1), linetype = "dotted", color = "grey") +
  #     geom_segment(aes(x = 60, xend = 60, y = 0, yend = 1), linetype = "dotted", color = "grey") +
  #     theme_minimal() +
  #     theme(legend.position = "bottom", legend.title = element_blank(), panel.grid.minor = element_blank()) +
  #     scale_x_continuous(breaks = c(10, 18, 60)) +
  #     scale_fill_manual(values = c(color1, color2), labels = c("men", "women")) +
  #     ylab("proportion")
  # }
  # 
  # output$age_all_t <- renderText("Although men make up a larger proportion of the labor force,
  #                                 women are overwhelmingly overrepresented in unpaid labor.
  #                                 Moreover, men are slightly more likely to do unpaid work only during their teenage years,
  #                                 while women tend to work without pay throughout their lifetime and well into old age.")
  # output$pop_age_all <- renderPlot(age_all(personas))
  # output$with_job_age_all <- renderPlot(age_all(with_job))
  # output$unpaid_age_all <- renderPlot(age_all(unpaid_job))
  # 
  # age_18 <- function(df) {
  #   df <- df %>%
  #     filter(age < 18)
  #   ggplot(df) +
  #     geom_bar(aes(x = age, fill = sex), position = "fill") +
  #     geom_segment(aes(x = min(age) - 0.5, xend = max(age) + 0.5, y = 0.5, yend = 0.5), linetype = "dashed", color = "grey") +
  #     theme_minimal() +
  #     theme(legend.position = "bottom", legend.title = element_blank(), panel.grid.minor = element_blank()) +
  #     scale_x_continuous(breaks = c(7, 10, 14, 17)) +
  #     scale_fill_manual(values = c(color1, color2), labels = c("boys", "girls")) +
  #     ylab("proportion")
  # }
  # 
  # output$age_18_t <- renderText("While the international consensus for minimum working age is 14,
  #                               Bolivia reduced it to 10 in 2014, lowest in the world.
  #                               In the survey questionnaire, individuals 7 years and above are asked to report work status.")
  # output$pop_age_18 <- renderPlot(age_18(personas))
  # output$with_job_age_18 <- renderPlot(age_18(with_job))
  # output$unpaid_age_18 <- renderPlot(age_18(unpaid_job))
  # 
  # age_18_60 <- function(df) {
  #   df <- df %>%
  #     filter(age > 17 & age < 61)
  #   ggplot(df) +
  #     geom_bar(aes(x = age, fill = sex), position = "fill") +
  #     geom_segment(aes(x = min(age) - 0.5, xend = max(age) + 0.5, y = 0.5, yend = 0.5), linetype = "dashed", color = "grey") +
  #     theme_minimal() +
  #     theme(legend.position = "bottom", legend.title = element_blank(), panel.grid.minor = element_blank()) +
  #     scale_fill_manual(values = c(color1, color2), labels = c("men", "women")) +
  #     ylab("proportion")
  # }
  # 
  # output$pop_age_18_60 <- renderPlot(age_18_60(personas))
  # output$with_job_age_18_60 <- renderPlot(age_18_60(with_job))
  # output$unpaid_age_18_60 <- renderPlot(age_18_60(unpaid_job))
  # 
  # age_60 <- function(df) {
  #   df <- df %>%
  #     filter(age > 60)
  #   ggplot(df) +
  #     geom_bar(aes(x = age, fill = sex), position = "fill") +
  #     geom_segment(aes(x = min(age) - 0.5, xend = max(age) + 0.5, y = 0.5, yend = 0.5), linetype = "dashed", color = "grey") +
  #     theme_minimal() +
  #     theme(legend.position = "bottom", legend.title = element_blank(), panel.grid.minor = element_blank()) +
  #     scale_fill_manual(values = c(color1, color2), labels = c("men", "women")) +
  #     ylab("proportion")
  # }
  # 
  # output$pop_age_60 <- renderPlot(age_60(personas))
  # output$with_job_age_60 <- renderPlot(age_60(with_job))
  # output$unpaid_age_60 <- renderPlot(age_60(unpaid_job))
  
  # Tab panel: indigenous ---------------------------
  # output$indi_all <- renderPlot({
  #   ggplot(personas) +
  #     geom_bar(aes(x = age, fill = indigenous), position = "fill") +
  #     theme_minimal() +
  #     theme(legend.position = "bottom", legend.title = element_blank(), panel.grid.minor = element_blank()) +
  #     scale_fill_manual(values = c(color1, color2, color3), labels = c("indigeous", "not indigenous", "not Bolivian")) +
  #     ylab("proportion")
  # })
  # 
  # output$indi_w <- renderPlot({
  #   ggplot(personas %>% filter(sex == "2.Mujer")) +
  #     geom_bar(aes(x = age, fill = indigenous), position = "fill") +
  #     theme_minimal() +
  #     theme(legend.position = "bottom", legend.title = element_blank(), panel.grid.minor = element_blank()) +
  #     scale_fill_manual(values = c(color1, color2, color3), labels = c("indigeous", "not indigenous", "not Bolivian")) +
  #     ylab("proportion")
  # })
  # 
  # output$indi_m <- renderPlot({
  #   ggplot(personas %>% filter(sex == "1.Hombre")) +
  #     geom_bar(aes(x = age, fill = indigenous), position = "fill") +
  #     theme_minimal() +
  #     theme(legend.position = "bottom", legend.title = element_blank(), panel.grid.minor = element_blank()) +
  #     scale_fill_manual(values = c(color1, color2, color3), labels = c("indigeous", "not indigenous", "not Bolivian")) +
  #     ylab("proportion")
  # })
  # 
  # indi_work <- function(df) {
  #   ggplot(df) +
  #     geom_bar(aes(x = indigenous, fill = sex), position = "fill", width = 0.5) +
  #     theme_minimal() +
  #     theme(legend.position = "bottom", legend.title = element_blank(), panel.grid.minor = element_blank()) +
  #     scale_fill_manual(values = c(color1, color2), labels = c("men", "women")) +
  #     ylab("proportion")
  # }
  # 
  # output$pop_indi_work <- renderPlot(indi_work(personas))
  # output$with_job_indi_work <- renderPlot(indi_work(with_job))
  # output$unpaid_indi_work <- renderPlot(indi_work(unpaid_job))
  # 
  # indi_ru <- function(df) {
  #   ggplot(df) +
  #     geom_bar(aes(x = age, fill = indigenous), position = "fill") +
  #     theme_minimal() +
  #     theme(legend.position = "bottom", legend.title = element_blank(), panel.grid.minor = element_blank()) +
  #     scale_fill_manual(values = c(color1, color2, color3), labels = c("indigeous", "not indigenous", "not Bolivian")) +
  #     ylab("proportion")
  # }
  # 
  # output$indi_r_all <- renderPlot(indi_ru(personas %>% filter(area == "Rural")))
  # output$indi_r_w <- renderPlot(indi_ru(personas %>% filter(area == "Rural" & sex == "2.Mujer")))
  # output$indi_r_m <- renderPlot(indi_ru(personas %>% filter(area == "Rural" & sex == "1.Hombre")))
  # 
  # output$pop_indi_r <- renderPlot(indi_work(personas %>% filter(area == "Rural")))
  # output$with_job_indi_r <- renderPlot(indi_work(with_job %>% filter(area == "Rural")))
  # output$unpaid_indi_r <- renderPlot(indi_work(unpaid_job %>% filter(area == "Rural")))
  # 
  # output$indi_u_all <- renderPlot(indi_ru(personas %>% filter(area == "Urbana")))
  # output$indi_u_w <- renderPlot(indi_ru(personas %>% filter(area == "Urbana" & sex == "2.Mujer")))
  # output$indi_u_m <- renderPlot(indi_ru(personas %>% filter(area == "Urbana" & sex == "1.Hombre")))
  # 
  # output$pop_indi_u <- renderPlot(indi_work(personas %>% filter(area == "Urbana")))
  # output$with_job_indi_u <- renderPlot(indi_work(with_job %>% filter(area == "Urbana")))
  # output$unpaid_indi_u <- renderPlot(indi_work(unpaid_job %>% filter(area == "Urbana")))
  
  
  # Tab panel: figures -------------------------------
  # output$pop <- renderText(round(nrow(personas), 0))
  # output$with_job <- renderText(round(nrow(with_job), 0))
  # output$unpaid <- renderText(round(nrow(unpaid_job), 0))
  # output$unpaid_sec <- renderText(round(nrow(unpaid_sec_job), 0))
  # 
  # age <- function(df) {
  #   ggplot(df) +
  #     geom_bar(aes(x = age, fill = sex), position = "dodge") +
  #     theme_minimal() +
  #     theme(legend.position = "bottom", legend.title = element_blank()) +
  #     scale_x_continuous(breaks = c(7, 18, 90))
  # }
  # 
  # output$age_t <- renderText('Men are more likely to do unpaid labor during their teenage years,
  #                            while women tend to work without pay throughout their lifetime.')
  # output$pop_age <- renderPlot(age(personas))
  # output$with_job_age <- renderPlot(age(with_job))
  # output$unpaid_age <- renderPlot(age(unpaid_job))
  # output$unpaid_sec_age <- renderPlot(age(unpaid_sec_job))
  # 
  # sch <- function(df) {
  #   ggplot(df) +
  #     geom_bar(aes(x = age, fill = in_school), position = "dodge") +
  #     theme_minimal() +
  #     theme(legend.position = "bottom", legend.title = element_blank()) +
  #     scale_x_continuous(breaks = c(7, 18, 90))
  # }
  # 
  # output$sch_t <- renderText("")
  # output$pop_sch <- renderPlot(sch(personas))
  # output$with_job_sch <- renderPlot(sch(with_job))
  # output$unpaid_sch <- renderPlot(sch(unpaid_job))
  # output$unpaid_sec_sch <- renderPlot(sch(unpaid_sec_job))
  # 
  # ur <- function(df) {
  #   ggplot(df) +
  #     geom_bar(aes(x = area, fill = sex), width = 0.3) +
  #     theme_minimal() +
  #     theme(legend.position = "bottom", legend.title = element_blank())
  # }
  # 
  # output$ur_t <- renderText('Most of the unpaid labor is done by rural residents, especially rural women,
  #                           even though they make up a small percentage of the national population.
  #                           However, when it comes to juggling an unpaid "side hustle" on top of a paid job,
  #                           men are more likely than women to do so.')
  # output$pop_ur <- renderPlot(ur(personas))
  # output$with_job_ur <- renderPlot(ur(with_job))
  # output$unpaid_ur <- renderPlot(ur(unpaid_job))
  # output$unpaid_sec_ur <- renderPlot(ur(unpaid_sec_job))
  # 
  # indi <- function(df) {
  #   ggplot(df) +
  #     geom_bar(aes(x = indigenous, fill = sex), width = 0.4) +
  #     theme_minimal() +
  #     theme(legend.position = "bottom", legend.title = element_blank())
  # }
  # 
  # output$pop_indi <- renderPlot(indi(personas))
  # output$with_job_indi <- renderPlot(indi(with_job))
  # output$unpaid_indi <- renderPlot(indi(unpaid_job))
  # output$unpaid_sec_indi <- renderPlot(indi(unpaid_sec_job))
  # 
  # indi_p <- function(df) {
  #   ggplot(df) +
  #     geom_bar(aes(x = indigenous, fill = sex), width = 0.4, position = "fill") +
  #     theme_minimal() +
  #     theme(legend.position = "bottom", legend.title = element_blank(), axis.title.y = element_blank())
  # }
  # 
  # output$pop_indi_p <- renderPlot(indi_p(personas))
  # output$with_job_indi_p <- renderPlot(indi_p(with_job))
  # output$unpaid_indi_p <- renderPlot(indi_p(unpaid_job))
  # output$unpaid_sec_indi_p <- renderPlot(indi_p(unpaid_sec_job))
  # 
  # indi_id <- function(df) {
  #   df$indigenous_id <- replace(df$indigenous_id, df$indigenous_id == "Indígena u originario no especificado", "No especificado")
  #   
  #   ggplot(df %>% filter(indigenous == "1. Pertenece")) +
  #     geom_bar(aes(y = indigenous_id, fill = sex), width = 0.6) +
  #     theme_minimal() +
  #     theme(legend.position = "bottom", legend.title = element_blank())
  # }
  # 
  # output$pop_indi_id <- renderPlot(indi_id(personas))
  # output$with_job_indi_id <- renderPlot(indi_id(with_job))
  # output$unpaid_indi_id <- renderPlot(indi_id(unpaid_job))
  # output$unpaid_sec_indi_id <- renderPlot(indi_id(unpaid_sec_job))
  # 
  # mari <- function(df) {
  #   ggplot(df) +
  #     geom_bar(aes(y = marital, fill = sex), width = 0.4) +
  #     theme_minimal() +
  #     theme(legend.position = "bottom", legend.title = element_blank())
  # }
  # 
  # output$mari_t <- renderText("Married women are much more likely than men to do unpaid labor,
  #                             but this gender dynamic is reversed among single people.")
  # output$pop_mari <- renderPlot(mari(personas))
  # output$with_job_mari <- renderPlot(mari(with_job))
  # output$unpaid_mari <- renderPlot(mari(unpaid_job))
  # output$unpaid_sec_mari <- renderPlot(mari(unpaid_sec_job))
  # 
  # mari_age <- function(df) {
  #   ref <- data.frame(
  #     marital_code = c("1", "2", "3", "4", "5", "6", NA),
  #     sp = c(F, T, T, F, F, F, NA)
  #   )
  #   
  #   df <- df %>%
  #     mutate(marital_code = ifelse(is.na(marital), NA, substr(marital, 1, 1))) %>%
  #     left_join(ref, by = "marital_code")
  #   
  #   ggplot(df) +
  #     geom_bar(aes(x = age, fill = sp), width = 0.4, position = "fill") +
  #     theme_minimal() +
  #     theme(legend.position = "bottom") +
  #     labs(fill = "with spouse or partner", y = "") +
  #     scale_x_continuous(breaks = c(7, 18, 90))
  # }
  # 
  # output$mari_age_t <- renderText("Married women are much more likely than men to do unpaid labor,
  #                             but this gender dynamic is reversed among single people.")
  # output$pop_mari_age <- renderPlot(mari_age(personas))
  # output$with_job_mari_age <- renderPlot(mari_age(with_job))
  # output$unpaid_mari_age <- renderPlot(mari_age(unpaid_job))
  # output$unpaid_sec_mari_age <- renderPlot(mari_age(unpaid_sec_job))
  # 
  # kid <- function(df) {
  #   df$num_alive_child <- replace(df$num_alive_child, is.na(df$num_alive_child), 0)
  #   ggplot(df) +
  #     geom_bar(aes(x = num_alive_child, fill = sex), width = 0.4) +
  #     theme_minimal() +
  #     theme(legend.position = "bottom", legend.title = element_blank())
  # }
  # 
  # output$kid_t <- renderText("Need some more data wrangling, because the survey logs children under women only.
  #                            Where are the single dads?!")
  # output$pop_kid <- renderPlot(kid(personas))
  # output$with_job_kid <- renderPlot(kid(with_job))
  # output$unpaid_kid <- renderPlot(kid(unpaid_job))
  # output$unpaid_sec_kid <- renderPlot(kid(unpaid_sec_job))
  # 
  # lit <- function(df) {
  #   ggplot(df) +
  #     geom_bar(aes(x = literate, fill = sex), width = 0.3) +
  #     theme_minimal() +
  #     theme(legend.position = "bottom", legend.title = element_blank())
  # }
  # 
  # output$lit_t <- renderText("Compared to the total working population,
  #                            people who are illiterate are more likely to work unpaid jobs.
  #                            However, even for literate women, they are still more likely to do unpaid labor than their male counterparts.")
  # output$pop_lit <- renderPlot(lit(personas))
  # output$with_job_lit <- renderPlot(lit(with_job))
  # output$unpaid_lit <- renderPlot(lit(unpaid_job))
  # output$unpaid_sec_lit <- renderPlot(lit(unpaid_sec_job))
  # 
  # edu <- function(df) {
  #   ggplot(df) +
  #     geom_bar(aes(y = edu, fill = sex), width = 0.4) +
  #     theme_minimal() +
  #     theme(legend.position = "bottom", legend.title = element_blank())
  # }
  # 
  # output$edu_t <- renderText("These plots are hard to read,
  #                             so I broke down the education variable into 2 binary variables:
  #                             1) whether an individual has received any education and
  #                             2) higher education attainment.")
  # output$pop_edu <- renderPlot(edu(personas))
  # output$with_job_edu <- renderPlot(edu(with_job))
  # output$unpaid_edu <- renderPlot(edu(unpaid_job))
  # output$unpaid_sec_edu <- renderPlot(edu(unpaid_sec_job))
  # 
  # any_ed <- function(df) {
  #   ggplot(df) +
  #     geom_bar(aes(x = any_ed, fill = sex), width = 0.3) +
  #     theme_minimal() +
  #     theme(legend.position = "bottom", legend.title = element_blank()) +
  #     xlab("received any education")
  # }
  # 
  # output$any_ed_t <- renderText("The charts below represent people who have received any formal education (first row)
  #                               and people who have received higher education (second row).
  #                               As expected, people with more education tend to hold paid jobs.")
  # output$pop_any_ed <- renderPlot(any_ed(personas))
  # output$with_job_any_ed <- renderPlot(any_ed(with_job))
  # output$unpaid_any_ed <- renderPlot(any_ed(unpaid_job))
  # output$unpaid_sec_any_ed <- renderPlot(any_ed(unpaid_sec_job))
  # 
  # higher_ed <- function(df) {
  #   ggplot(df) +
  #     geom_bar(aes(x = higher_ed, fill = sex), width = 0.3) +
  #     theme_minimal() +
  #     theme(legend.position = "bottom", legend.title = element_blank()) +
  #     xlab("received higher education")
  # }
  # 
  # output$pop_higher_ed <- renderPlot(higher_ed(personas))
  # output$with_job_higher_ed <- renderPlot(higher_ed(with_job))
  # output$unpaid_higher_ed <- renderPlot(higher_ed(unpaid_job))
  # output$unpaid_sec_higher_ed <- renderPlot(higher_ed(unpaid_sec_job))
  # 
  # any_ed2 <- function(df) {
  #   ggplot(df) +
  #     geom_bar(aes(x = sex, fill = any_ed), width = 0.3) +
  #     theme_minimal() +
  #     theme(legend.position = "bottom") +
  #     labs(fill = "received any education")
  # }
  # 
  # 
  # output$any_ed2_t <- renderText("The following two rows are the same data as the previous two rows,
  #                                but coded differently to better show the gender dynamics.")
  # output$pop_any_ed2 <- renderPlot(any_ed2(personas))
  # output$with_job_any_ed2 <- renderPlot(any_ed2(with_job))
  # output$unpaid_any_ed2 <- renderPlot(any_ed2(unpaid_job))
  # output$unpaid_sec_any_ed2 <- renderPlot(any_ed2(unpaid_sec_job))
  # 
  # higher_ed2 <- function(df) {
  #   ggplot(df) +
  #     geom_bar(aes(x = sex, fill = higher_ed), width = 0.3) +
  #     theme_minimal() +
  #     theme(legend.position = "bottom") +
  #     labs(fill = "received higher education")
  # }
  # 
  # output$pop_higher_ed2 <- renderPlot(higher_ed2(personas))
  # output$with_job_higher_ed2 <- renderPlot(higher_ed2(with_job))
  # output$unpaid_higher_ed2 <- renderPlot(higher_ed2(unpaid_job))
  # output$unpaid_sec_higher_ed2 <- renderPlot(higher_ed2(unpaid_sec_job))
  # 
  # disease <- function(df) {
  #   df <- df %>% mutate(disease = chronic_disease_1 != "12. Ninguna?")
  #   
  #   ggplot(df) +
  #     geom_bar(aes(x = sex, fill = disease), width = 0.3) +
  #     theme_minimal() +
  #     theme(legend.position = "bottom")
  # }
  # 
  # output$pop_disease <- renderPlot(disease(personas))
  # output$with_job_disease <- renderPlot(disease(with_job))
  # output$unpaid_disease <- renderPlot(disease(unpaid_job))
  # output$unpaid_sec_disease <- renderPlot(disease(unpaid_sec_job))
  # 
  # cell1 <- function(df) {
  #   ggplot(df) +
  #     geom_bar(aes(x = cellphone, fill = sex), width = 0.3) +
  #     theme_minimal() +
  #     theme(legend.position = "bottom", legend.title = element_blank())
  # }
  # cell2 <- function(df) {
  #   ggplot(df) +
  #     geom_bar(aes(x = sex, fill = cellphone), width = 0.3) +
  #     theme_minimal() +
  #     theme(legend.position = "bottom")
  # }
  # 
  # output$tech1 <- renderText("Have a cellphone:\n
  #                            Women who have unpaid jobs are much less likely to have cellphones,
  #                            though the causal relationship is unclear.")
  # output$pop_cell1 <- renderPlot(cell1(personas))
  # output$with_job_cell1 <- renderPlot(cell1(with_job))
  # output$unpaid_cell1 <- renderPlot(cell1(unpaid_job))
  # output$unpaid_sec_cell1 <- renderPlot(cell1(unpaid_sec_job))
  # output$pop_cell2 <- renderPlot(cell2(personas))
  # output$with_job_cell2 <- renderPlot(cell2(with_job))
  # output$unpaid_cell2 <- renderPlot(cell2(unpaid_job))
  # output$unpaid_sec_cell2 <- renderPlot(cell2(unpaid_sec_job))
  # 
  # internet1 <- function(df) {
  #   ggplot(df) +
  #     geom_bar(aes(x = internet_use, fill = sex), width = 0.3) +
  #     theme_minimal() +
  #     theme(legend.position = "bottom", legend.title = element_blank())
  # }
  # internet2 <- function(df) {
  #   ggplot(df) +
  #     geom_bar(aes(x = sex, fill = internet_use), width = 0.3) +
  #     theme_minimal() +
  #     theme(legend.position = "bottom")
  # }
  # 
  # output$tech2 <- renderText("Internet access:")
  # output$pop_internet1 <- renderPlot(internet1(personas))
  # output$with_job_internet1 <- renderPlot(internet1(with_job))
  # output$unpaid_internet1 <- renderPlot(internet1(unpaid_job))
  # output$unpaid_sec_internet1 <- renderPlot(internet1(unpaid_sec_job))
  # output$pop_internet2 <- renderPlot(internet2(personas))
  # output$with_job_internet2 <- renderPlot(internet2(with_job))
  # output$unpaid_internet2 <- renderPlot(internet2(unpaid_job))
  # output$unpaid_sec_internet2 <- renderPlot(internet2(unpaid_sec_job))
  # 
  # inter_home1 <- function(df) {
  #   df <- df %>% mutate(inter_home = (internet_use_where_1 == "1. En el Hogar?" | internet_use_where_2 == "1. En el Hogar?"))
  #   df$inter_home <- replace(df$inter_home, is.na(df$inter_home), F)
  #   
  #   ggplot(df) +
  #     geom_bar(aes(x = inter_home, fill = sex), width = 0.3) +
  #     theme_minimal() +
  #     theme(legend.position = "bottom", legend.title = element_blank())
  # }
  # inter_home2 <- function(df) {
  #   df <- df %>% mutate(inter_home = (internet_use_where_1 == "1. En el Hogar?" | internet_use_where_2 == "1. En el Hogar?"))
  #   df$inter_home <- replace(df$inter_home, is.na(df$inter_home), F)
  #   
  #   ggplot(df) +
  #     geom_bar(aes(x = sex, fill = inter_home), width = 0.3) +
  #     theme_minimal() +
  #     theme(legend.position = "bottom") +
  #     labs(fill = "Internet at home")
  # }
  # 
  # output$tech3 <- renderText("More specifically, Internet access at home:")
  # output$pop_inter_home1 <- renderPlot(inter_home1(personas))
  # output$with_job_inter_home1 <- renderPlot(inter_home1(with_job))
  # output$unpaid_inter_home1 <- renderPlot(inter_home1(unpaid_job))
  # output$unpaid_sec_inter_home1 <- renderPlot(inter_home1(unpaid_sec_job))
  # output$pop_inter_home2 <- renderPlot(inter_home2(personas))
  # output$with_job_inter_home2 <- renderPlot(inter_home2(with_job))
  # output$unpaid_inter_home2 <- renderPlot(inter_home2(unpaid_job))
  # output$unpaid_sec_inter_home2 <- renderPlot(inter_home2(unpaid_sec_job))
  # 
  # union1 <- function(df) {
  #   ggplot(df) +
  #     geom_bar(aes(x = union_member, fill = sex), width = 0.3) +
  #     theme_minimal() +
  #     theme(legend.position = "bottom", legend.title = element_blank())
  # }
  # union2 <- function(df) {
  #   ggplot(df) +
  #     geom_bar(aes(x = sex, fill = union_member), width = 0.3) +
  #     theme_minimal() +
  #     theme(legend.position = "bottom", legend.title = element_blank())
  # }
  # 
  # output$pop_union1 <- renderPlot(union1(personas))
  # output$with_job_union1 <- renderPlot(union1(with_job))
  # output$unpaid_union1 <- renderPlot(union1(unpaid_job))
  # output$unpaid_sec_union1 <- renderPlot(union1(unpaid_sec_job))
  # output$pop_union2 <- renderPlot(union2(personas))
  # output$with_job_union2 <- renderPlot(union2(with_job))
  # output$unpaid_union2 <- renderPlot(union2(unpaid_job))
  # output$unpaid_sec_union2 <- renderPlot(union2(unpaid_sec_job))
  # 
  # salary <- function(df) {
  #   df$primary_salary <- replace(df$primary_salary, is.na(df$primary_salary), 0)
  #   
  #   ggplot(df) +
  #     geom_violin(aes(x = sex, y = primary_salary, fill = sex), color = NA, alpha = 0.75, width = 1) +
  #     geom_boxplot(aes(x = sex, y = primary_salary), width = 0.3, color = "black", fill = NA) +
  #     theme_minimal() +
  #     theme(legend.position = "bottom", legend.title = element_blank())
  # }
  # 
  # output$salary_t <- renderText("Wage income:")
  # output$pop_salary <- renderPlot(salary(personas))
  # output$with_job_salary <- renderPlot(salary(with_job))
  # output$unpaid_salary <- renderPlot(salary(unpaid_job))
  # output$unpaid_sec_salary <- renderPlot(salary(unpaid_sec_job))
  # 
  # irr <- function(df) {
  #   df$primary_nonsalaried_income <- replace(df$primary_nonsalaried_income, is.na(df$primary_nonsalaried_income), 0)
  #   
  #   ggplot(df) +
  #     geom_violin(aes(x = sex, y = primary_nonsalaried_income, fill = sex), color = NA, alpha = 0.75, width = 1) +
  #     geom_boxplot(aes(x = sex, y = primary_nonsalaried_income), width = 0.3, color = "black", fill = NA) +
  #     theme_minimal() +
  #     theme(legend.position = "bottom", legend.title = element_blank())
  # }
  # 
  # output$irr_t <- renderText("Irregular income:")
  # output$pop_irr <- renderPlot(irr(personas))
  # output$with_job_irr <- renderPlot(irr(with_job))
  # output$unpaid_irr <- renderPlot(irr(unpaid_job))
  # output$unpaid_sec_irr <- renderPlot(irr(unpaid_sec_job))
  # 
  # want1 <- function(df) {
  #   ref <- data.frame(
  #     want_work_more = c("1. Si", "1. Si", "2. No", "2. No"),
  #     avail_work_more = c("1. Si", "2. No", "1. Si", "2. No"),
  #     work_more = c("Want to and can work more", "Want to work more but can't", "Can work more but don't want to", "Neither")
  #   )
  #   
  #   df <- df %>% left_join(ref, by = c("want_work_more", "avail_work_more"))
  #   
  #   ggplot(df) +
  #     geom_bar(aes(x = work_more, fill = sex), width = 0.3) +
  #     theme_minimal() +
  #     theme(legend.position = "bottom", legend.title = element_blank())
  # }
  # want2 <- function(df) {
  #   ref <- data.frame(
  #     want_work_more = c("1. Si", "1. Si", "2. No", "2. No"),
  #     avail_work_more = c("1. Si", "2. No", "1. Si", "2. No"),
  #     work_more = c("Want to and can work more", "Want to work more but can't", "Can work more but don't want to", "Neither")
  #   )
  #   
  #   df <- df %>% left_join(ref, by = c("want_work_more", "avail_work_more"))
  #   
  #   ggplot(df) +
  #     geom_bar(aes(x = sex, fill = work_more), width = 0.3) +
  #     theme_minimal() +
  #     theme(legend.position = "bottom", legend.title = element_blank())
  # }
  # 
  # output$want_t <- renderText("Want to work more vs. can work more")
  # output$pop_want1 <- renderPlot(want1(personas))
  # output$with_job_want1 <- renderPlot(want1(with_job))
  # output$unpaid_want1 <- renderPlot(want1(unpaid_job))
  # output$unpaid_sec_want1 <- renderPlot(want1(unpaid_sec_job))
  # output$pop_want2 <- renderPlot(want2(personas))
  # output$with_job_want2 <- renderPlot(want2(with_job))
  # output$unpaid_want2 <- renderPlot(want2(unpaid_job))
  # output$unpaid_sec_want2 <- renderPlot(want2(unpaid_sec_job))
  
  # Tab panel: stories -----------------------------------
  
  # c1_df <- unpaid_job1 %>%
  #   filter(sex == "2.Mujer", higher_ed == T) %>%
  #   select(area, age, marital, indigenous, edu, in_school, chronic_disease_1, num_alive_child,
  #          primary_job, work_type, want_work_more, avail_work_more)
  # c1_df2 <- eventReactive(input$b1, {
  #   c1_df[sample(nrow(c1_df), 1), ]
  # })
  # 
  # output$t1 <- renderText(paste("Women with higher education working an unpaid job:", nrow(c1_df)))
  # observeEvent(input$b1, {
  #   updateActionButton(session, "b1", label = " Tell me another story", icon = icon("redo"))
  # })
  # output$c1 <- renderTable({ c1_df2() })
  # 
  # c2_df <- unpaid_sec_job %>%
  #   select(area, sex, age, marital, indigenous, edu, in_school, chronic_disease_1, num_alive_child,
  #          primary_job, work_type, primary_salary, primary_salary_freq, primary_nonsalaried_income, primary_nonsalaried_income_freq,
  #          sec_job, sec_employer_industry, sec_work_type,
  #          want_work_more, avail_work_more)
  # c2_df2 <- eventReactive(input$b2, {
  #   c2_df[sample(nrow(c2_df), 1), ]
  # })
  # 
  # output$t2 <- renderText(paste("People with a paid primary job and an unpaid secondary job:", nrow(c2_df)))
  # observeEvent(input$b2, {
  #   updateActionButton(session, "b2", label = " Tell me another story", icon = icon("redo"))
  # })
  # output$c2 <- renderTable({ c2_df2() })
  # 
  # c3_df <- unpaid_pri_job %>%
  #   select(area, sex, age, marital, indigenous, edu, in_school, chronic_disease_1, num_alive_child,
  #          primary_job, work_type, sec_job, sec_employer_industry, sec_work_type, sec_salary, sec_salary_freq, sec_nonsalaried_income, sec_nonsalaried_income_freq,
  #          want_work_more, avail_work_more)
  # c3_df2 <- eventReactive(input$b3, {
  #   c3_df[sample(nrow(c3_df), 1), ]
  # })
  # 
  # output$t3 <- renderText(paste("People with an unpaid primary job and a paid secondary job:", nrow(c3_df)))
  # observeEvent(input$b3, {
  #   updateActionButton(session, "b3", label = " Tell me another story", icon = icon("redo"))
  # })
  # output$c3 <- renderTable({ c3_df2() })
  # 
  # c4_df <- unpaid_job %>%
  #   filter(union_member == "1. Si") %>%
  #   select(area, sex, age, marital, indigenous, edu, in_school, literate, chronic_disease_1, num_alive_child,
  #          primary_job, work_type, sec_job, sec_employer_industry, sec_work_type, sec_salary, sec_salary_freq, sec_nonsalaried_income, sec_nonsalaried_income_freq,
  #          want_work_more, avail_work_more)
  # c4_df2 <- eventReactive(input$b4, {
  #   c4_df[sample(nrow(c4_df), 1), ]
  # })
  # 
  # output$t4 <- renderText(paste("Union members with unpaid jobs:", nrow(c4_df)))
  # observeEvent(input$b4, {
  #   updateActionButton(session, "b4", label = " Tell me another story", icon = icon("redo"))
  # })
  # output$c4 <- renderTable({ c4_df2() })
  # 
  # c5_df <- with_job %>%
  #   filter(age < 10) %>%
  #   select(area, sex, age, indigenous, edu, in_school,
  #          primary_job, work_type, primary_salary, primary_salary_freq, primary_nonsalaried_income, primary_nonsalaried_income_freq,
  #          sec_job, sec_employer_industry, sec_work_type, union_member)
  # c5_df2 <- eventReactive(input$b5, {
  #   c5_df[sample(nrow(c5_df), 1), ]
  # })
  # 
  # output$t5 <- renderText(paste("Child laborers under ten:", nrow(c5_df)))
  # observeEvent(input$b5, {
  #   updateActionButton(session, "b5", label = " Tell me another story", icon = icon("redo"))
  # })
  # output$c5 <- renderTable({ c5_df2() })
  
  # Page navigation ---------------------------------
  observeEvent(input$to_youth, {
    updateNavbarPage(session, "main", selected = "youth")
  })
  observeEvent(input$to_employment, {
    updateNavbarPage(session, "main", selected = "employment")
  })
  observeEvent(input$to_neet, {
    updateNavbarPage(session, "main", selected = "neet")
  })
}

shinyApp(ui = ui, server = server)
