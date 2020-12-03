packages.list <- c("shiny", "shinydashboard","shinythemes",
                   "tidyverse", "shinyWidgets", "plotly", "waffle", "ggridges")
for (p in packages.list) {
  if (!p %in% installed.packages()[, "Package"]) install.packages(p)
  library(p, character.only = TRUE)
}

library(treemap)
library(d3treeR)

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
color9 <- "#e6e6e6" # grey 10
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
                      
                      fluidRow(column(12, style = "background-color: #3e3f3a; padding: 80px 50px;",
                                      fluidRow(
                                        column(8,
                                               offset = 2,
                                               h3(textOutput("children_madlib")))))),
                      hr(),
                      fluidRow(
                        column(6,
                               offset = 3,
                               textOutput("children_intro"),
                               h3("Children population overview"),
                               fluidRow(
                                 column(6,
                                        plotOutput("children_overview1")),
                                 column(6,
                                        plotOutput("children_overview2"))
                               ),
                               hr(),
                               textOutput("children_t1"),
                               h3("School enrollment and attendance"),
                               fluidRow(
                                 column(6,
                                        plotOutput("children_edu1")),
                                 column(6,
                                        plotOutput("children_edu2"))
                               ),
                               hr(),
                               textOutput("children_t2"),
                               h3("Reasons for not enrolling in school"),
                               d3tree2Output("children_edu3"),
                               plotOutput("children_edu4"),
                               hr(),
                               textOutput("children_t3"),
                               h3("Educational attainment"),
                               fluidRow(
                                 column(6,
                                        plotOutput("children_edu5")),
                                 column(6,
                                        plotOutput("children_edu6"))
                               ),
                               hr(),
                               textOutput("children_t4"),
                               h3("Work hours and income"),
                               plotOutput("children_lfp1"),
                               plotOutput("children_lfp2"),
                               hr(),
                               textOutput("children_t5"),
                               h3("Work hours and income in relation to the household"),
                               plotOutput("children_lfp3"),
                               plotOutput("children_lfp4"),
                               hr(),
                               textOutput("children_t6"),
                               h3("Children in rural and urban areas"),
                               plotOutput("children_ru1"),
                               plotOutput("children_ru2"),
                               hr(),
                               textOutput("children_t7"),
                               h3("Children and indigeneity"),
                               fluidRow(
                                 column(6,
                                        plotOutput("children_indi1")),
                                 column(6,
                                        plotOutput("children_indi2"))
                               ),
                               hr(),
                               fluidRow(
                                 column(6,
                                        plotOutput("children_indi3")),
                                 column(6,
                                        plotOutput("children_indi4"))
                               ))
                      )),
             
             # Tab panel: youth 18-25 --------------------
             tabPanel("Youth 18-25",
                      value = "youth"),
             
             # Tab panel: adults 25-60 --------------------
             navbarMenu("Adults 25-60",
                        tabPanel("Entering the job market",
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
                        tabPanel("Paid and unpaid labor"),
                        tabPanel("NEET population")),
             
             # Tab panel: older adults 60+ --------------------
             tabPanel("Older adults 60+",
                      value = "older"),
             
             # Tab panel: Indigenous --------------------
             tabPanel("Indigenous identity",
                      value = "indigenous",
                      
                      h2("Overall indigenous population"),
                      fluidRow(column(10, offset = 1,
                                      h4(textOutput("indi_all_t")))),
                      fluidRow(
                        column(4,
                               p("All"),
                               plotOutput("indi_all")),
                        column(4,
                               p("Women"),
                               plotOutput("indi_w")),
                        column(4,
                               p("Men"),
                               plotOutput("indi_m"))
                      ),
                      hr(),
                      
                      h2("Indigenous people and labor"),
                      fluidRow(column(10, offset = 1,
                                      h4(textOutput("indi_work_t")))),
                      fluidRow(
                        column(4,
                               p(c1),
                               plotOutput("pop_indi_work")),
                        column(4,
                               p(c2),
                               plotOutput("with_job_indi_work")),
                        column(4,
                               p(c3),
                               plotOutput("unpaid_indi_work"))
                      ),
                      hr(),
                      
                      h2("Indigenous people in rural areas"),
                      fluidRow(column(10, offset = 1,
                                      h4(textOutput("indi_r_t1")))),
                      fluidRow(
                        column(4,
                               p("All"),
                               plotOutput("indi_r_all")),
                        column(4,
                               p("Women"),
                               plotOutput("indi_r_w")),
                        column(4,
                               p("Men"),
                               plotOutput("indi_r_m"))
                      ),
                      hr(),
                      fluidRow(column(10, offset = 1,
                                      h4(textOutput("indi_r_t2")))),
                      fluidRow(
                        column(4,
                               p(c1),
                               plotOutput("pop_indi_r")),
                        column(4,
                               p(c2),
                               plotOutput("with_job_indi_r")),
                        column(4,
                               p(c3),
                               plotOutput("unpaid_indi_r"))
                      ),
                      hr(),
                      
                      h2("Indigenous people in urban areas"),
                      fluidRow(column(10, offset = 1,
                                      h4(textOutput("indi_u_t1")))),
                      fluidRow(
                        column(4,
                               p("All"),
                               plotOutput("indi_u_all")),
                        column(4,
                               p("Women"),
                               plotOutput("indi_u_w")),
                        column(4,
                               p("Men"),
                               plotOutput("indi_u_m"))
                      ),
                      hr(),
                      fluidRow(column(10, offset = 1,
                                      h4(textOutput("indi_u_t2")))),
                      fluidRow(
                        column(4,
                               p(c1),
                               plotOutput("pop_indi_u")),
                        column(4,
                               p(c2),
                               plotOutput("with_job_indi_u")),
                        column(4,
                               p(c3),
                               plotOutput("unpaid_indi_u"))
                      )),

             # Tab panel: Figures --------------------
             tabPanel("Figures",
                      box(h2("Column overview"),
                          fluidRow(
                            column(3,
                                   p(paste(c1, "(2018)")),
                                   h1(textOutput("pop")),
                                   p("people (est.)")),
                            column(3,
                                   p(c2),
                                   h1(textOutput("with_job")),
                                   p("people (est.)")),
                            column(3,
                                   p(c3),
                                   h1(textOutput("unpaid")),
                                   p("people (est.)")),
                            column(3,
                                   p(c4),
                                   h1(textOutput("unpaid_sec")),
                                   p("people (est.)"))
                          ),
                          width = 12,
                          collapsible = T),
                      box(hr(), width = 12),
                      
                      box(h2("Age"),
                          h4(textOutput("age_t")),
                          fluidRow(
                            column(3,
                                   p(c1),
                                   plotOutput("pop_age")),
                            column(3,
                                   p(c2),
                                   plotOutput("with_job_age")),
                            column(3,
                                   p(c3),
                                   plotOutput("unpaid_age")),
                            column(3,
                                   p(c4),
                                   plotOutput("unpaid_sec_age"))
                          ),
                          width = 12,
                          collapsible = T),
                      box(hr(), width = 12),
                      
                      box(h2("School attendance"),
                          h4(textOutput("sch_t")),
                          fluidRow(
                            column(3,
                                   p(c1),
                                   plotOutput("pop_sch")),
                            column(3,
                                   p(c2),
                                   plotOutput("with_job_sch")),
                            column(3,
                                   p(c3),
                                   plotOutput("unpaid_sch")),
                            column(3,
                                   p(c4),
                                   plotOutput("unpaid_sec_sch"))
                          ),
                          width = 12,
                          collapsible = T),
                      box(hr(), width = 12),
                      
                      box(h2("Urban/rural area"),
                          h4(textOutput("ur_t")),
                          fluidRow(
                            column(3,
                                   p(c1),
                                   plotOutput("pop_ur")),
                            column(3,
                                   p(c2),
                                   plotOutput("with_job_ur")),
                            column(3,
                                   p(c3),
                                   plotOutput("unpaid_ur")),
                            column(3,
                                   p(c4),
                                   plotOutput("unpaid_sec_ur"))
                          ),
                          width = 12,
                          collapsible = T),
                      box(hr(), width = 12),
                      
                      box(h2("Indigenous identity"),
                          fluidRow(
                            column(3,
                                   p(c1),
                                   plotOutput("pop_indi"),
                                   plotOutput("pop_indi_p"),
                                   plotOutput("pop_indi_id")),
                            column(3,
                                   p(c2),
                                   plotOutput("with_job_indi"),
                                   plotOutput("with_job_indi_p"),
                                   plotOutput("with_job_indi_id")),
                            column(3,
                                   p(c3),
                                   plotOutput("unpaid_indi"),
                                   plotOutput("unpaid_indi_p"),
                                   plotOutput("unpaid_indi_id")),
                            column(3,
                                   p(c4),
                                   plotOutput("unpaid_sec_indi"),
                                   plotOutput("unpaid_sec_indi_p"),
                                   plotOutput("unpaid_sec_indi_id"))
                          ),
                          width = 12,
                          collapsible = T),
                      box(hr(), width = 12),
                      
                      box(h2("Marital status"),
                          h4(textOutput("mari_t")),
                          fluidRow(
                            column(3,
                                   p(c1),
                                   plotOutput("pop_mari")),
                            column(3,
                                   p(c2),
                                   plotOutput("with_job_mari")),
                            column(3,
                                   p(c3),
                                   plotOutput("unpaid_mari")),
                            column(3,
                                   p(c4),
                                   plotOutput("unpaid_sec_mari"))
                          ),
                          fluidRow(
                            column(3,
                                   p(c1),
                                   plotOutput("pop_mari_age")),
                            column(3,
                                   p(c2),
                                   plotOutput("with_job_mari_age")),
                            column(3,
                                   p(c3),
                                   plotOutput("unpaid_mari_age")),
                            column(3,
                                   p(c4),
                                   plotOutput("unpaid_sec_mari_age"))
                          ),
                          width = 12,
                          collapsible = T),
                      box(hr(), width = 12),
                      
                      box(h2("Number of children"),
                          h4(textOutput("kid_t")),
                          fluidRow(
                            column(3,
                                   p(c1),
                                   plotOutput("pop_kid")),
                            column(3,
                                   p(c2),
                                   plotOutput("with_job_kid")),
                            column(3,
                                   p(c3),
                                   plotOutput("unpaid_kid")),
                            column(3,
                                   p(c4),
                                   plotOutput("unpaid_sec_kid"))
                          ),
                          width = 12,
                          collapsible = T),
                      box(hr(), width = 12),
                      
                      box(h2("Literacy"),
                          h4(textOutput("lit_t")),
                          fluidRow(
                            column(3,
                                   p(c1),
                                   plotOutput("pop_lit")),
                            column(3,
                                   p(c2),
                                   plotOutput("with_job_lit")),
                            column(3,
                                   p(c3),
                                   plotOutput("unpaid_lit")),
                            column(3,
                                   p(c4),
                                   plotOutput("unpaid_sec_lit"))
                          ),
                          width = 12,
                          collapsible = T),
                      box(hr(), width = 12),
                      
                      box(h2("Education"),
                          h4(textOutput("edu_t")),
                          fluidRow(
                            column(3,
                                   p(c1),
                                   plotOutput("pop_edu")),
                            column(3,
                                   p(c2),
                                   plotOutput("with_job_edu")),
                            column(3,
                                   p(c3),
                                   plotOutput("unpaid_edu")),
                            column(3,
                                   p(c4),
                                   plotOutput("unpaid_sec_edu"))
                          ),
                          h4(textOutput("any_ed_t")),
                          fluidRow(
                            column(3,
                                   p(c1),
                                   plotOutput("pop_any_ed")),
                            column(3,
                                   p(c2),
                                   plotOutput("with_job_any_ed")),
                            column(3,
                                   p(c3),
                                   plotOutput("unpaid_any_ed")),
                            column(3,
                                   p(c4),
                                   plotOutput("unpaid_sec_any_ed"))
                          ),
                          fluidRow(
                            column(3,
                                   p(c1),
                                   plotOutput("pop_higher_ed")),
                            column(3,
                                   p(c2),
                                   plotOutput("with_job_higher_ed")),
                            column(3,
                                   p(c3),
                                   plotOutput("unpaid_higher_ed")),
                            column(3,
                                   p(c4),
                                   plotOutput("unpaid_sec_higher_ed"))
                          ),
                          h4(textOutput("any_ed2_t")),
                          fluidRow(
                            column(3,
                                   p(c1),
                                   plotOutput("pop_any_ed2")),
                            column(3,
                                   p(c2),
                                   plotOutput("with_job_any_ed2")),
                            column(3,
                                   p(c3),
                                   plotOutput("unpaid_any_ed2")),
                            column(3,
                                   p(c4),
                                   plotOutput("unpaid_sec_any_ed2"))
                          ),
                          fluidRow(
                            column(3,
                                   p(c1),
                                   plotOutput("pop_higher_ed2")),
                            column(3,
                                   p(c2),
                                   plotOutput("with_job_higher_ed2")),
                            column(3,
                                   p(c3),
                                   plotOutput("unpaid_higher_ed2")),
                            column(3,
                                   p(c4),
                                   plotOutput("unpaid_sec_higher_ed2"))
                          ),
                          width = 12,
                          collapsible = T),
                      box(hr(), width = 12),
                      
                      box(h2("Chronic disease"),
                          fluidRow(
                            column(3,
                                   p(c1),
                                   plotOutput("pop_disease")),
                            column(3,
                                   p(c2),
                                   plotOutput("with_job_disease")),
                            column(3,
                                   p(c3),
                                   plotOutput("unpaid_disease")),
                            column(3,
                                   p(c4),
                                   plotOutput("unpaid_sec_disease"))
                          ),
                          width = 12,
                          collapsible = T),
                      box(hr(), width = 12),
                      
                      box(h2("Technology"),
                          h4(textOutput("tech1")),
                          fluidRow(
                            column(3,
                                   p(c1),
                                   plotOutput("pop_cell1")),
                            column(3,
                                   p(c2),
                                   plotOutput("with_job_cell1")),
                            column(3,
                                   p(c3),
                                   plotOutput("unpaid_cell1")),
                            column(3,
                                   p(c4),
                                   plotOutput("unpaid_sec_cell1"))
                          ),
                          fluidRow(
                            column(3,
                                   p(c1),
                                   plotOutput("pop_cell2")),
                            column(3,
                                   p(c2),
                                   plotOutput("with_job_cell2")),
                            column(3,
                                   p(c3),
                                   plotOutput("unpaid_cell2")),
                            column(3,
                                   p(c4),
                                   plotOutput("unpaid_sec_cell2"))
                          ),
                          h4(textOutput("tech2")),
                          fluidRow(
                            column(3,
                                   p(c1),
                                   plotOutput("pop_internet1")),
                            column(3,
                                   p(c2),
                                   plotOutput("with_job_internet1")),
                            column(3,
                                   p(c3),
                                   plotOutput("unpaid_internet1")),
                            column(3,
                                   p(c4),
                                   plotOutput("unpaid_sec_internet1"))
                          ),
                          fluidRow(
                            column(3,
                                   p(c1),
                                   plotOutput("pop_internet2")),
                            column(3,
                                   p(c2),
                                   plotOutput("with_job_internet2")),
                            column(3,
                                   p(c3),
                                   plotOutput("unpaid_internet2")),
                            column(3,
                                   p(c4),
                                   plotOutput("unpaid_sec_internet2"))
                          ),
                          h4(textOutput("tech3")),
                          fluidRow(
                            column(3,
                                   p(c1),
                                   plotOutput("pop_inter_home1")),
                            column(3,
                                   p(c2),
                                   plotOutput("with_job_inter_home1")),
                            column(3,
                                   p(c3),
                                   plotOutput("unpaid_inter_home1")),
                            column(3,
                                   p(c4),
                                   plotOutput("unpaid_sec_inter_home1"))
                          ),
                          fluidRow(
                            column(3,
                                   p(c1),
                                   plotOutput("pop_inter_home2")),
                            column(3,
                                   p(c2),
                                   plotOutput("with_job_inter_home2")),
                            column(3,
                                   p(c3),
                                   plotOutput("unpaid_inter_home2")),
                            column(3,
                                   p(c4),
                                   plotOutput("unpaid_sec_inter_home2"))
                          ),
                          width = 12,
                          collapsible = T),
                      box(hr(), width = 12),
                      
                      box(h2("Union member"),
                          fluidRow(
                            column(3,
                                   p(c1),
                                   plotOutput("pop_union1")),
                            column(3,
                                   p(c2),
                                   plotOutput("with_job_union1")),
                            column(3,
                                   p(c3),
                                   plotOutput("unpaid_union1")),
                            column(3,
                                   p(c4),
                                   plotOutput("unpaid_sec_union1"))
                          ),
                          fluidRow(
                            column(3,
                                   p(c1),
                                   plotOutput("pop_union2")),
                            column(3,
                                   p(c2),
                                   plotOutput("with_job_union2")),
                            column(3,
                                   p(c3),
                                   plotOutput("unpaid_union2")),
                            column(3,
                                   p(c4),
                                   plotOutput("unpaid_sec_union2"))
                          ),
                          width = 12,
                          collapsible = T),
                      box(hr(), width = 12),
                      
                      box(h2("Income"),
                          h4(textOutput("salary_t")),
                          fluidRow(
                            column(3,
                                   p(c1),
                                   plotOutput("pop_salary")),
                            column(3,
                                   p(c2),
                                   plotOutput("with_job_salary")),
                            column(3,
                                   p(c3),
                                   plotOutput("unpaid_salary")),
                            column(3,
                                   p(c4),
                                   plotOutput("unpaid_sec_salary"))
                          ),
                          h4(textOutput("irr_t")),
                          fluidRow(
                            column(3,
                                   p(c1),
                                   plotOutput("pop_irr")),
                            column(3,
                                   p(c2),
                                   plotOutput("with_job_irr")),
                            column(3,
                                   p(c3),
                                   plotOutput("unpaid_irr")),
                            column(3,
                                   p(c4),
                                   plotOutput("unpaid_sec_irr"))
                          ),
                          width = 12,
                          collapsible = T),
                      box(hr(), width = 12),
                      
                      box(h2("Want to work more?"),
                          h4(textOutput("want1_t")),
                          fluidRow(
                            column(3,
                                   p(c1),
                                   plotOutput("pop_want1")),
                            column(3,
                                   p(c2),
                                   plotOutput("with_job_want1")),
                            column(3,
                                   p(c3),
                                   plotOutput("unpaid_want1")),
                            column(3,
                                   p(c4),
                                   plotOutput("unpaid_sec_want1"))
                          ),
                          fluidRow(
                            column(3,
                                   p(c1),
                                   plotOutput("pop_want2")),
                            column(3,
                                   p(c2),
                                   plotOutput("with_job_want2")),
                            column(3,
                                   p(c3),
                                   plotOutput("unpaid_want2")),
                            column(3,
                                   p(c4),
                                   plotOutput("unpaid_sec_want2"))
                          ),
                          width = 12,
                          collapsible = T)),
             
             # Tab panel: stories -----------------------------
             tabPanel("Stories",
                      h3(textOutput("t1")),
                      actionButton("b1", label = "Tell me a story", icon = icon("redo")),
                      tableOutput("c1"),
                      hr(),
                      h3(textOutput("t2")),
                      actionButton("b2", label = "Tell me a story", icon = icon("redo")),
                      tableOutput("c2"),
                      hr(),
                      h3(textOutput("t3")),
                      actionButton("b3", label = "Tell me a story", icon = icon("redo")),
                      tableOutput("c3"),
                      hr(),
                      h3(textOutput("t4")),
                      actionButton("b4", label = "Tell me a story", icon = icon("redo")),
                      tableOutput("c4"),
                      hr(),
                      h3(textOutput("t5")),
                      actionButton("b5", label = "Tell me a story", icon = icon("redo")),
                      tableOutput("c5"))
  )
)

# Server -----------------------------
server <- function(input, output, session) {
  # Tab panel: home --------------------------
  output$landing <- renderImage(list(src = "www/landing_page.png", width = "100%"), deleteFile = F)
  observeEvent(input$landing_cl, {
    updateNavbarPage(session, "main", selected = {
      if (between(input$landing_cl$x, 95, 280) & between(input$landing_cl$y, 19, 129)) {"age"}
      else if (between(input$landing_cl$x, 23, 233) & between(input$landing_cl$y, 189, 311)) {"lfp"}
      else if (between(input$landing_cl$x, 877, 1091) & between(input$landing_cl$y, 77, 209)) {"edu"}
      else if (between(input$landing_cl$x, 347, 507) & between(input$landing_cl$y, 324, 495)) {"ru"}
      else if (between(input$landing_cl$x, 890, 1053) & between(input$landing_cl$y, 266, 445)) {"indigenous"}
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
      ifelse(round(madlib_df$tot_monthly_inc, 0) == 0,
                   paste(madlib_pron1, "does not earn any income from", madlib_pron3, "work."),
                   paste("In total,", madlib_pron2, "makes", round(madlib_df$tot_monthly_inc, 0), "Bolivianos every month."))
    )
  })
  
  output$children_intro <- renderText("In this household survey, children make up about 35% of the sampled population, indicating that children are an important demographic in Bolivia.
                                      It is also worth highlighting that Bolivia has the lowest legal minimum working age, ten, instead of the international consensus of fourteen. This labor law remains controversial today, with some arguing that it destigmatizes child workers' struggle for fair labor, while others casting doubt on the legislation's efficacy and its implications for children's long-term well-being.
                                      Given this reality, it is even more crucial to understand the conditions in which children live and work, as well as the persistent impact of social disparities.")
  output$children_t1 <- renderText('Bolivia enjoys a high level of school enrollment among children.
                                   Even among those in the category of "enrolled, not attending", the vast majority are simply due to school break or recess (i.e. the timing of the survey), not structural barriers.
                                   There is no evidence for gender disparity in receiving education.')
  output$children_t2 <- renderText('However, there is still a notable minority of children who could not go to school, and the survey largely fails to capture the complex reasons.
                                   From the available data, poverty and inaccessibility appear to remain an obstacle for many children.')
  output$children_t3 <- renderText("TBD")
  output$children_t4 <- renderText("When we look at children individually, there is not a significant difference between boys' and girls' work hours and incomes,
                                   except that boys start earning more than girls on a monthly basis as they approach 18.
                                   Interestingly, their hourly incomes do not differ, which means that teenage boys probably work more hours than girls.
                                   Overall, a large number of boys and girls merely help out with their families' work, rather than seeking outside employment.")
  output$children_t5 <- renderText("More intriguing patterns emerge, however, when we examine children's work as contributions to their households.
                                   Clearly, starting from age 16, boys begin to make a larger contribution to their family's income than girls, but their share of total work hours among all family members remains equal.
                                   This likely means that girls start doing unpaid or family-based labor during adolescence.
                                   Puberty -> devaluation of female labor. Sexualization and devaluation go hand in hand.
                                   In terms of DRM, the data show that men/boys are more likely to have income sources outside their homes and access an additional channel of financial stability.
                                   Yet, in some scenarios, teenage boys are actual the main breadwinners in their families, which denotes heightened vulnerability to social and economic shocks.")
  output$children_t6 <- renderText("An even more stark contrast exists between children in rural and urban areas.
                                   Children in cities start contributing to their family finances at an earlier age, work more hours, and bring home more money.
                                   Additionally, while rural children work a similar amount of hours throughout their childhood and adolescence, city kids pick up more work as they grow older, especially from age 12.")
  output$children_t7 <- renderText("Not much to note here yet, except that the indigenous population seems to be in decline (the trend is more obvious when all age groups are mapped together).
                                   The exact reason remains to be investigated.")
  
  output$children_overview1 <- renderPlot(
    ggplot(children) +
      geom_bar(aes(x = age, fill = sex), position = "stack", width = 1) +
      theme_minimal() +
      theme(legend.position = "bottom", legend.title = element_blank(), panel.grid.minor = element_blank()) +
      scale_x_continuous(breaks = c(7, 10, 14, 17)) +
      scale_fill_manual(values = c(color1, color2), labels = c("boys", "girls")) +
      ylab("population")
  )
  output$children_overview2 <- renderPlot(
    ggplot(children) +
      geom_bar(aes(x = age, fill = sex), position = "fill", width = 1) +
      geom_segment(aes(x = min(age) - 0.5, xend = max(age) + 0.5, y = 0.5, yend = 0.5), linetype = "dashed", color = "grey") +
      theme_minimal() +
      theme(legend.position = "bottom", legend.title = element_blank(), panel.grid.minor = element_blank()) +
      scale_x_continuous(breaks = c(7, 10, 14, 17)) +
      scale_fill_manual(values = c(color1, color2), labels = c("boys", "girls")) +
      ylab("proportion")
  )
  
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
  
  output$children_indi1 <- renderPlot(
    ggplot(children) +
      geom_bar(aes(x = indigenous, fill = sex), position = "stack", width = 0.5) +
      theme_minimal() +
      theme(legend.position = "bottom", legend.title = element_blank(), panel.grid.minor = element_blank()) +
      scale_x_discrete(labels = c("1. Pertenece" = "indigenous", "2. No pertenece" = "not\nindigenous",
                                  "3. No soy boliviana o boliviano" = "not\nBolivian")) +
      scale_fill_manual(values = c(color1, color2), labels = c("boys", "girls")) +
      xlab("") + ylab("population")
  )
  output$children_indi2 <- renderPlot(
    ggplot(children) +
      geom_bar(aes(x = indigenous, fill = sex), position = "fill", width = 0.5) +
      theme_minimal() +
      theme(legend.position = "bottom", legend.title = element_blank(), panel.grid.minor = element_blank()) +
      scale_x_discrete(labels = c("1. Pertenece" = "indigenous", "2. No pertenece" = "not\nindigenous",
                                  "3. No soy boliviana o boliviano" = "not\nBolivian")) +
      scale_fill_manual(values = c(color1, color2), labels = c("boys", "girls")) +
      xlab("") + ylab("proportion")
  )
  output$children_indi3 <- renderPlot(
    ggplot(children) +
      geom_bar(aes(x = age, fill = indigenous), position = "stack", width = 1) +
      theme_minimal() +
      theme(legend.position = "bottom", legend.title = element_blank(), panel.grid.minor = element_blank()) +
      scale_fill_manual(values = c(color1, color2, color3), labels = c("indigenous", "not\nindigenous", "not\nBolivian")) +
      ylab("population")
  )
  output$children_indi4 <- renderPlot(
    ggplot(children) +
      geom_bar(aes(x = age, fill = indigenous), position = "fill", width = 1) +
      theme_minimal() +
      theme(legend.position = "bottom", legend.title = element_blank(), panel.grid.minor = element_blank()) +
      scale_fill_manual(values = c(color1, color2, color3), labels = c("indigenous", "not\nindigenous", "not\nBolivian")) +
      ylab("proportion")
  )
  
  output$children_edu1 <- renderPlot(
    ggplot(children) +
      geom_bar(aes(x = edu_status, fill = sex), position = "stack", width = 0.5) +
      theme_minimal() +
      theme(legend.position = "bottom", legend.title = element_blank(), panel.grid.minor = element_blank()) +
      scale_fill_manual(values = c(color1, color2), labels = c("boys", "girls")) +
      xlab("") + ylab("population")
  )
  output$children_edu2 <- renderPlot(
    ggplot(children) +
      geom_bar(aes(x = edu_status, fill = sex), position = "fill", width = 0.5) +
      theme_minimal() +
      theme(legend.position = "bottom", legend.title = element_blank(), panel.grid.minor = element_blank()) +
      scale_fill_manual(values = c(color1, color2), labels = c("boys", "girls")) +
      xlab("") + ylab("proportion")
  )
  
  why_not_in_school_df <- children %>%
    filter(!is.na(why_not_in_school)) %>%
    mutate(why_not_in_school = case_when(startsWith(why_not_in_school, "14") ~ "reasons not\nlisted in survey",
                                         startsWith(why_not_in_school, "11") ~ "work",
                                         startsWith(why_not_in_school, "2") ~ "illness,\naccident,\ndisability",
                                         startsWith(why_not_in_school, "3") ~ "pregnancy",
                                         startsWith(why_not_in_school, "4") ~ "lack of money\nfor school supplies",
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
  
  output$children_lfp1 <- renderPlot(
    ggplot(children %>% filter(!is.na(tot_monthly_inc))) +
      geom_jitter(aes(x = age, y = tot_work_week_hr, size = tot_monthly_inc, color = sex), alpha = 0.15) +
      geom_line(data = children %>% filter(!is.na(tot_monthly_inc)) %>% group_by(age, sex) %>% summarize(mean_hr = mean(tot_work_week_hr), mean_inc = mean(tot_monthly_inc)),
                aes(y = mean_hr, x = age, color = sex), size = 1) +
      geom_point(data = children %>% filter(!is.na(tot_monthly_inc)) %>% group_by(age, sex) %>% summarize(mean_hr = mean(tot_work_week_hr), mean_inc = mean(tot_monthly_inc)),
                 aes(y = mean_hr, x = age, size = mean_inc, color = sex)) +
      theme_minimal() +
      theme(legend.position = "bottom", panel.grid.major.y = element_blank(), panel.grid.minor = element_blank()) +
      scale_x_continuous(limits = c(7, 17.8)) +
      scale_color_manual(values = c(color1, color2), labels = c("boys", "girls")) +
      scale_size(range = c(0.1, 30)) +
      labs(y = "weekly work hours", size = "monthly income (BOB)", color = "")
  )
  output$children_lfp2 <- renderPlot(
    ggplot(children %>% filter(!is.na(tot_monthly_inc))) +
      geom_jitter(aes(x = age, y = tot_work_week_hr, size = tot_monthly_inc / tot_work_week_hr / 4.33, color = sex), alpha = 0.15) +
      geom_line(data = children %>% filter(!is.na(tot_monthly_inc)) %>% group_by(age, sex) %>% summarize(mean_hr = mean(tot_work_week_hr), mean_inc = mean(tot_monthly_inc / tot_work_week_hr / 4.33)),
                aes(y = mean_hr, x = age, color = sex), size = 1) +
      geom_point(data = children %>% filter(!is.na(tot_monthly_inc)) %>% group_by(age, sex) %>% summarize(mean_hr = mean(tot_work_week_hr), mean_inc = mean(tot_monthly_inc / tot_work_week_hr / 4.33)),
                 aes(y = mean_hr, x = age, size = mean_inc, color = sex)) +
      theme_minimal() +
      theme(legend.position = "bottom", panel.grid.major.y = element_blank(), panel.grid.minor = element_blank()) +
      scale_x_continuous(limits = c(7, 17.8)) +
      scale_color_manual(values = c(color1, color2), labels = c("boys", "girls")) +
      scale_size(range = c(0.1, 30)) +
      labs(y = "weekly work hours", size = "hourly income (BOB)", color = "")
  )
  
  output$children_lfp3 <- renderPlot(hh_inc_sex)
  output$children_lfp4 <- renderPlot(hh_hr_sex)
  
  output$children_ru1 <- renderPlot(hh_inc_area)
  output$children_ru2 <- renderPlot(hh_hr_area)
  
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
  output$indi_all <- renderPlot({
    ggplot(personas) +
      geom_bar(aes(x = age, fill = indigenous), position = "fill") +
      theme_minimal() +
      theme(legend.position = "bottom", legend.title = element_blank(), panel.grid.minor = element_blank()) +
      scale_fill_manual(values = c(color1, color2, color3), labels = c("indigeous", "not indigenous", "not Bolivian")) +
      ylab("proportion")
  })
  
  output$indi_w <- renderPlot({
    ggplot(personas %>% filter(sex == "2.Mujer")) +
      geom_bar(aes(x = age, fill = indigenous), position = "fill") +
      theme_minimal() +
      theme(legend.position = "bottom", legend.title = element_blank(), panel.grid.minor = element_blank()) +
      scale_fill_manual(values = c(color1, color2, color3), labels = c("indigeous", "not indigenous", "not Bolivian")) +
      ylab("proportion")
  })
  
  output$indi_m <- renderPlot({
    ggplot(personas %>% filter(sex == "1.Hombre")) +
      geom_bar(aes(x = age, fill = indigenous), position = "fill") +
      theme_minimal() +
      theme(legend.position = "bottom", legend.title = element_blank(), panel.grid.minor = element_blank()) +
      scale_fill_manual(values = c(color1, color2, color3), labels = c("indigeous", "not indigenous", "not Bolivian")) +
      ylab("proportion")
  })
  
  indi_work <- function(df) {
    ggplot(df) +
      geom_bar(aes(x = indigenous, fill = sex), position = "fill", width = 0.5) +
      theme_minimal() +
      theme(legend.position = "bottom", legend.title = element_blank(), panel.grid.minor = element_blank()) +
      scale_fill_manual(values = c(color1, color2), labels = c("men", "women")) +
      ylab("proportion")
  }
  
  output$pop_indi_work <- renderPlot(indi_work(personas))
  output$with_job_indi_work <- renderPlot(indi_work(with_job))
  output$unpaid_indi_work <- renderPlot(indi_work(unpaid_job))
  
  indi_ru <- function(df) {
    ggplot(df) +
      geom_bar(aes(x = age, fill = indigenous), position = "fill") +
      theme_minimal() +
      theme(legend.position = "bottom", legend.title = element_blank(), panel.grid.minor = element_blank()) +
      scale_fill_manual(values = c(color1, color2, color3), labels = c("indigeous", "not indigenous", "not Bolivian")) +
      ylab("proportion")
  }
  
  output$indi_r_all <- renderPlot(indi_ru(personas %>% filter(area == "Rural")))
  output$indi_r_w <- renderPlot(indi_ru(personas %>% filter(area == "Rural" & sex == "2.Mujer")))
  output$indi_r_m <- renderPlot(indi_ru(personas %>% filter(area == "Rural" & sex == "1.Hombre")))
  
  output$pop_indi_r <- renderPlot(indi_work(personas %>% filter(area == "Rural")))
  output$with_job_indi_r <- renderPlot(indi_work(with_job %>% filter(area == "Rural")))
  output$unpaid_indi_r <- renderPlot(indi_work(unpaid_job %>% filter(area == "Rural")))
  
  output$indi_u_all <- renderPlot(indi_ru(personas %>% filter(area == "Urbana")))
  output$indi_u_w <- renderPlot(indi_ru(personas %>% filter(area == "Urbana" & sex == "2.Mujer")))
  output$indi_u_m <- renderPlot(indi_ru(personas %>% filter(area == "Urbana" & sex == "1.Hombre")))
  
  output$pop_indi_u <- renderPlot(indi_work(personas %>% filter(area == "Urbana")))
  output$with_job_indi_u <- renderPlot(indi_work(with_job %>% filter(area == "Urbana")))
  output$unpaid_indi_u <- renderPlot(indi_work(unpaid_job %>% filter(area == "Urbana")))
  
  
  # Tab panel: figures -------------------------------
  output$pop <- renderText(round(nrow(personas), 0))
  output$with_job <- renderText(round(nrow(with_job), 0))
  output$unpaid <- renderText(round(nrow(unpaid_job), 0))
  output$unpaid_sec <- renderText(round(nrow(unpaid_sec_job), 0))
  
  age <- function(df) {
    ggplot(df) +
      geom_bar(aes(x = age, fill = sex), position = "dodge") +
      theme_minimal() +
      theme(legend.position = "bottom", legend.title = element_blank()) +
      scale_x_continuous(breaks = c(7, 18, 90))
  }
  
  output$age_t <- renderText('Men are more likely to do unpaid labor during their teenage years,
                             while women tend to work without pay throughout their lifetime.')
  output$pop_age <- renderPlot(age(personas))
  output$with_job_age <- renderPlot(age(with_job))
  output$unpaid_age <- renderPlot(age(unpaid_job))
  output$unpaid_sec_age <- renderPlot(age(unpaid_sec_job))

  sch <- function(df) {
    ggplot(df) +
      geom_bar(aes(x = age, fill = in_school), position = "dodge") +
      theme_minimal() +
      theme(legend.position = "bottom", legend.title = element_blank()) +
      scale_x_continuous(breaks = c(7, 18, 90))
  }
  
  output$sch_t <- renderText("")
  output$pop_sch <- renderPlot(sch(personas))
  output$with_job_sch <- renderPlot(sch(with_job))
  output$unpaid_sch <- renderPlot(sch(unpaid_job))
  output$unpaid_sec_sch <- renderPlot(sch(unpaid_sec_job))
  
  ur <- function(df) {
    ggplot(df) +
      geom_bar(aes(x = area, fill = sex), width = 0.3) +
      theme_minimal() +
      theme(legend.position = "bottom", legend.title = element_blank())
  }
  
  output$ur_t <- renderText('Most of the unpaid labor is done by rural residents, especially rural women,
                            even though they make up a small percentage of the national population.
                            However, when it comes to juggling an unpaid "side hustle" on top of a paid job,
                            men are more likely than women to do so.')
  output$pop_ur <- renderPlot(ur(personas))
  output$with_job_ur <- renderPlot(ur(with_job))
  output$unpaid_ur <- renderPlot(ur(unpaid_job))
  output$unpaid_sec_ur <- renderPlot(ur(unpaid_sec_job))
  
  indi <- function(df) {
    ggplot(df) +
      geom_bar(aes(x = indigenous, fill = sex), width = 0.4) +
      theme_minimal() +
      theme(legend.position = "bottom", legend.title = element_blank())
  }
  
  output$pop_indi <- renderPlot(indi(personas))
  output$with_job_indi <- renderPlot(indi(with_job))
  output$unpaid_indi <- renderPlot(indi(unpaid_job))
  output$unpaid_sec_indi <- renderPlot(indi(unpaid_sec_job))
  
  indi_p <- function(df) {
    ggplot(df) +
      geom_bar(aes(x = indigenous, fill = sex), width = 0.4, position = "fill") +
      theme_minimal() +
      theme(legend.position = "bottom", legend.title = element_blank(), axis.title.y = element_blank())
  }
  
  output$pop_indi_p <- renderPlot(indi_p(personas))
  output$with_job_indi_p <- renderPlot(indi_p(with_job))
  output$unpaid_indi_p <- renderPlot(indi_p(unpaid_job))
  output$unpaid_sec_indi_p <- renderPlot(indi_p(unpaid_sec_job))
  
  indi_id <- function(df) {
    df$indigenous_id <- replace(df$indigenous_id, df$indigenous_id == "Indígena u originario no especificado", "No especificado")
    
    ggplot(df %>% filter(indigenous == "1. Pertenece")) +
      geom_bar(aes(y = indigenous_id, fill = sex), width = 0.6) +
      theme_minimal() +
      theme(legend.position = "bottom", legend.title = element_blank())
  }
  
  output$pop_indi_id <- renderPlot(indi_id(personas))
  output$with_job_indi_id <- renderPlot(indi_id(with_job))
  output$unpaid_indi_id <- renderPlot(indi_id(unpaid_job))
  output$unpaid_sec_indi_id <- renderPlot(indi_id(unpaid_sec_job))
  
  mari <- function(df) {
    ggplot(df) +
      geom_bar(aes(y = marital, fill = sex), width = 0.4) +
      theme_minimal() +
      theme(legend.position = "bottom", legend.title = element_blank())
  }
  
  output$mari_t <- renderText("Married women are much more likely than men to do unpaid labor,
                              but this gender dynamic is reversed among single people.")
  output$pop_mari <- renderPlot(mari(personas))
  output$with_job_mari <- renderPlot(mari(with_job))
  output$unpaid_mari <- renderPlot(mari(unpaid_job))
  output$unpaid_sec_mari <- renderPlot(mari(unpaid_sec_job))
  
  mari_age <- function(df) {
    ref <- data.frame(
      marital_code = c("1", "2", "3", "4", "5", "6", NA),
      sp = c(F, T, T, F, F, F, NA)
    )
    
    df <- df %>%
      mutate(marital_code = ifelse(is.na(marital), NA, substr(marital, 1, 1))) %>%
      left_join(ref, by = "marital_code")
    
    ggplot(df) +
      geom_bar(aes(x = age, fill = sp), width = 0.4, position = "fill") +
      theme_minimal() +
      theme(legend.position = "bottom") +
      labs(fill = "with spouse or partner", y = "") +
      scale_x_continuous(breaks = c(7, 18, 90))
  }
  
  output$mari_age_t <- renderText("Married women are much more likely than men to do unpaid labor,
                              but this gender dynamic is reversed among single people.")
  output$pop_mari_age <- renderPlot(mari_age(personas))
  output$with_job_mari_age <- renderPlot(mari_age(with_job))
  output$unpaid_mari_age <- renderPlot(mari_age(unpaid_job))
  output$unpaid_sec_mari_age <- renderPlot(mari_age(unpaid_sec_job))
  
  kid <- function(df) {
    df$num_alive_child <- replace(df$num_alive_child, is.na(df$num_alive_child), 0)
    ggplot(df) +
      geom_bar(aes(x = num_alive_child, fill = sex), width = 0.4) +
      theme_minimal() +
      theme(legend.position = "bottom", legend.title = element_blank())
  }
  
  output$kid_t <- renderText("Need some more data wrangling, because the survey logs children under women only.
                             Where are the single dads?!")
  output$pop_kid <- renderPlot(kid(personas))
  output$with_job_kid <- renderPlot(kid(with_job))
  output$unpaid_kid <- renderPlot(kid(unpaid_job))
  output$unpaid_sec_kid <- renderPlot(kid(unpaid_sec_job))
  
  lit <- function(df) {
    ggplot(df) +
      geom_bar(aes(x = literate, fill = sex), width = 0.3) +
      theme_minimal() +
      theme(legend.position = "bottom", legend.title = element_blank())
  }
  
  output$lit_t <- renderText("Compared to the total working population,
                             people who are illiterate are more likely to work unpaid jobs.
                             However, even for literate women, they are still more likely to do unpaid labor than their male counterparts.")
  output$pop_lit <- renderPlot(lit(personas))
  output$with_job_lit <- renderPlot(lit(with_job))
  output$unpaid_lit <- renderPlot(lit(unpaid_job))
  output$unpaid_sec_lit <- renderPlot(lit(unpaid_sec_job))
  
  edu <- function(df) {
    ggplot(df) +
      geom_bar(aes(y = edu, fill = sex), width = 0.4) +
      theme_minimal() +
      theme(legend.position = "bottom", legend.title = element_blank())
  }
  
  output$edu_t <- renderText("These plots are hard to read,
                              so I broke down the education variable into 2 binary variables:
                              1) whether an individual has received any education and
                              2) higher education attainment.")
  output$pop_edu <- renderPlot(edu(personas))
  output$with_job_edu <- renderPlot(edu(with_job))
  output$unpaid_edu <- renderPlot(edu(unpaid_job))
  output$unpaid_sec_edu <- renderPlot(edu(unpaid_sec_job))
  
  any_ed <- function(df) {
    ggplot(df) +
      geom_bar(aes(x = any_ed, fill = sex), width = 0.3) +
      theme_minimal() +
      theme(legend.position = "bottom", legend.title = element_blank()) +
      xlab("received any education")
  }
  
  output$any_ed_t <- renderText("The charts below represent people who have received any formal education (first row)
                                and people who have received higher education (second row).
                                As expected, people with more education tend to hold paid jobs.")
  output$pop_any_ed <- renderPlot(any_ed(personas))
  output$with_job_any_ed <- renderPlot(any_ed(with_job))
  output$unpaid_any_ed <- renderPlot(any_ed(unpaid_job))
  output$unpaid_sec_any_ed <- renderPlot(any_ed(unpaid_sec_job))
  
  higher_ed <- function(df) {
    ggplot(df) +
      geom_bar(aes(x = higher_ed, fill = sex), width = 0.3) +
      theme_minimal() +
      theme(legend.position = "bottom", legend.title = element_blank()) +
      xlab("received higher education")
  }
  
  output$pop_higher_ed <- renderPlot(higher_ed(personas))
  output$with_job_higher_ed <- renderPlot(higher_ed(with_job))
  output$unpaid_higher_ed <- renderPlot(higher_ed(unpaid_job))
  output$unpaid_sec_higher_ed <- renderPlot(higher_ed(unpaid_sec_job))
  
  any_ed2 <- function(df) {
    ggplot(df) +
      geom_bar(aes(x = sex, fill = any_ed), width = 0.3) +
      theme_minimal() +
      theme(legend.position = "bottom") +
      labs(fill = "received any education")
  }
  
  
  output$any_ed2_t <- renderText("The following two rows are the same data as the previous two rows,
                                 but coded differently to better show the gender dynamics.")
  output$pop_any_ed2 <- renderPlot(any_ed2(personas))
  output$with_job_any_ed2 <- renderPlot(any_ed2(with_job))
  output$unpaid_any_ed2 <- renderPlot(any_ed2(unpaid_job))
  output$unpaid_sec_any_ed2 <- renderPlot(any_ed2(unpaid_sec_job))
  
  higher_ed2 <- function(df) {
    ggplot(df) +
      geom_bar(aes(x = sex, fill = higher_ed), width = 0.3) +
      theme_minimal() +
      theme(legend.position = "bottom") +
      labs(fill = "received higher education")
  }
  
  output$pop_higher_ed2 <- renderPlot(higher_ed2(personas))
  output$with_job_higher_ed2 <- renderPlot(higher_ed2(with_job))
  output$unpaid_higher_ed2 <- renderPlot(higher_ed2(unpaid_job))
  output$unpaid_sec_higher_ed2 <- renderPlot(higher_ed2(unpaid_sec_job))
  
  disease <- function(df) {
    df <- df %>% mutate(disease = chronic_disease_1 != "12. Ninguna?")
    
    ggplot(df) +
      geom_bar(aes(x = sex, fill = disease), width = 0.3) +
      theme_minimal() +
      theme(legend.position = "bottom")
  }
  
  output$pop_disease <- renderPlot(disease(personas))
  output$with_job_disease <- renderPlot(disease(with_job))
  output$unpaid_disease <- renderPlot(disease(unpaid_job))
  output$unpaid_sec_disease <- renderPlot(disease(unpaid_sec_job))
  
  cell1 <- function(df) {
    ggplot(df) +
      geom_bar(aes(x = cellphone, fill = sex), width = 0.3) +
      theme_minimal() +
      theme(legend.position = "bottom", legend.title = element_blank())
  }
  cell2 <- function(df) {
    ggplot(df) +
      geom_bar(aes(x = sex, fill = cellphone), width = 0.3) +
      theme_minimal() +
      theme(legend.position = "bottom")
  }
  
  output$tech1 <- renderText("Have a cellphone:\n
                             Women who have unpaid jobs are much less likely to have cellphones,
                             though the causal relationship is unclear.")
  output$pop_cell1 <- renderPlot(cell1(personas))
  output$with_job_cell1 <- renderPlot(cell1(with_job))
  output$unpaid_cell1 <- renderPlot(cell1(unpaid_job))
  output$unpaid_sec_cell1 <- renderPlot(cell1(unpaid_sec_job))
  output$pop_cell2 <- renderPlot(cell2(personas))
  output$with_job_cell2 <- renderPlot(cell2(with_job))
  output$unpaid_cell2 <- renderPlot(cell2(unpaid_job))
  output$unpaid_sec_cell2 <- renderPlot(cell2(unpaid_sec_job))
  
  internet1 <- function(df) {
    ggplot(df) +
      geom_bar(aes(x = internet_use, fill = sex), width = 0.3) +
      theme_minimal() +
      theme(legend.position = "bottom", legend.title = element_blank())
  }
  internet2 <- function(df) {
    ggplot(df) +
      geom_bar(aes(x = sex, fill = internet_use), width = 0.3) +
      theme_minimal() +
      theme(legend.position = "bottom")
  }
  
  output$tech2 <- renderText("Internet access:")
  output$pop_internet1 <- renderPlot(internet1(personas))
  output$with_job_internet1 <- renderPlot(internet1(with_job))
  output$unpaid_internet1 <- renderPlot(internet1(unpaid_job))
  output$unpaid_sec_internet1 <- renderPlot(internet1(unpaid_sec_job))
  output$pop_internet2 <- renderPlot(internet2(personas))
  output$with_job_internet2 <- renderPlot(internet2(with_job))
  output$unpaid_internet2 <- renderPlot(internet2(unpaid_job))
  output$unpaid_sec_internet2 <- renderPlot(internet2(unpaid_sec_job))
  
  inter_home1 <- function(df) {
    df <- df %>% mutate(inter_home = (internet_use_where_1 == "1. En el Hogar?" | internet_use_where_2 == "1. En el Hogar?"))
    df$inter_home <- replace(df$inter_home, is.na(df$inter_home), F)
    
    ggplot(df) +
      geom_bar(aes(x = inter_home, fill = sex), width = 0.3) +
      theme_minimal() +
      theme(legend.position = "bottom", legend.title = element_blank())
  }
  inter_home2 <- function(df) {
    df <- df %>% mutate(inter_home = (internet_use_where_1 == "1. En el Hogar?" | internet_use_where_2 == "1. En el Hogar?"))
    df$inter_home <- replace(df$inter_home, is.na(df$inter_home), F)
    
    ggplot(df) +
      geom_bar(aes(x = sex, fill = inter_home), width = 0.3) +
      theme_minimal() +
      theme(legend.position = "bottom") +
      labs(fill = "Internet at home")
  }
  
  output$tech3 <- renderText("More specifically, Internet access at home:")
  output$pop_inter_home1 <- renderPlot(inter_home1(personas))
  output$with_job_inter_home1 <- renderPlot(inter_home1(with_job))
  output$unpaid_inter_home1 <- renderPlot(inter_home1(unpaid_job))
  output$unpaid_sec_inter_home1 <- renderPlot(inter_home1(unpaid_sec_job))
  output$pop_inter_home2 <- renderPlot(inter_home2(personas))
  output$with_job_inter_home2 <- renderPlot(inter_home2(with_job))
  output$unpaid_inter_home2 <- renderPlot(inter_home2(unpaid_job))
  output$unpaid_sec_inter_home2 <- renderPlot(inter_home2(unpaid_sec_job))
  
  union1 <- function(df) {
    ggplot(df) +
      geom_bar(aes(x = union_member, fill = sex), width = 0.3) +
      theme_minimal() +
      theme(legend.position = "bottom", legend.title = element_blank())
  }
  union2 <- function(df) {
    ggplot(df) +
      geom_bar(aes(x = sex, fill = union_member), width = 0.3) +
      theme_minimal() +
      theme(legend.position = "bottom", legend.title = element_blank())
  }
  
  output$pop_union1 <- renderPlot(union1(personas))
  output$with_job_union1 <- renderPlot(union1(with_job))
  output$unpaid_union1 <- renderPlot(union1(unpaid_job))
  output$unpaid_sec_union1 <- renderPlot(union1(unpaid_sec_job))
  output$pop_union2 <- renderPlot(union2(personas))
  output$with_job_union2 <- renderPlot(union2(with_job))
  output$unpaid_union2 <- renderPlot(union2(unpaid_job))
  output$unpaid_sec_union2 <- renderPlot(union2(unpaid_sec_job))
  
  salary <- function(df) {
    df$primary_salary <- replace(df$primary_salary, is.na(df$primary_salary), 0)
    
    ggplot(df) +
      geom_violin(aes(x = sex, y = primary_salary, fill = sex), color = NA, alpha = 0.75, width = 1) +
      geom_boxplot(aes(x = sex, y = primary_salary), width = 0.3, color = "black", fill = NA) +
      theme_minimal() +
      theme(legend.position = "bottom", legend.title = element_blank())
  }
  
  output$salary_t <- renderText("Wage income:")
  output$pop_salary <- renderPlot(salary(personas))
  output$with_job_salary <- renderPlot(salary(with_job))
  output$unpaid_salary <- renderPlot(salary(unpaid_job))
  output$unpaid_sec_salary <- renderPlot(salary(unpaid_sec_job))
  
  irr <- function(df) {
    df$primary_nonsalaried_income <- replace(df$primary_nonsalaried_income, is.na(df$primary_nonsalaried_income), 0)
    
    ggplot(df) +
      geom_violin(aes(x = sex, y = primary_nonsalaried_income, fill = sex), color = NA, alpha = 0.75, width = 1) +
      geom_boxplot(aes(x = sex, y = primary_nonsalaried_income), width = 0.3, color = "black", fill = NA) +
      theme_minimal() +
      theme(legend.position = "bottom", legend.title = element_blank())
  }
  
  output$irr_t <- renderText("Irregular income:")
  output$pop_irr <- renderPlot(irr(personas))
  output$with_job_irr <- renderPlot(irr(with_job))
  output$unpaid_irr <- renderPlot(irr(unpaid_job))
  output$unpaid_sec_irr <- renderPlot(irr(unpaid_sec_job))
  
  want1 <- function(df) {
    ref <- data.frame(
      want_work_more = c("1. Si", "1. Si", "2. No", "2. No"),
      avail_work_more = c("1. Si", "2. No", "1. Si", "2. No"),
      work_more = c("Want to and can work more", "Want to work more but can't", "Can work more but don't want to", "Neither")
    )
    
    df <- df %>% left_join(ref, by = c("want_work_more", "avail_work_more"))
    
    ggplot(df) +
      geom_bar(aes(x = work_more, fill = sex), width = 0.3) +
      theme_minimal() +
      theme(legend.position = "bottom", legend.title = element_blank())
  }
  want2 <- function(df) {
    ref <- data.frame(
      want_work_more = c("1. Si", "1. Si", "2. No", "2. No"),
      avail_work_more = c("1. Si", "2. No", "1. Si", "2. No"),
      work_more = c("Want to and can work more", "Want to work more but can't", "Can work more but don't want to", "Neither")
    )
    
    df <- df %>% left_join(ref, by = c("want_work_more", "avail_work_more"))
    
    ggplot(df) +
      geom_bar(aes(x = sex, fill = work_more), width = 0.3) +
      theme_minimal() +
      theme(legend.position = "bottom", legend.title = element_blank())
  }
  
  output$want_t <- renderText("Want to work more vs. can work more")
  output$pop_want1 <- renderPlot(want1(personas))
  output$with_job_want1 <- renderPlot(want1(with_job))
  output$unpaid_want1 <- renderPlot(want1(unpaid_job))
  output$unpaid_sec_want1 <- renderPlot(want1(unpaid_sec_job))
  output$pop_want2 <- renderPlot(want2(personas))
  output$with_job_want2 <- renderPlot(want2(with_job))
  output$unpaid_want2 <- renderPlot(want2(unpaid_job))
  output$unpaid_sec_want2 <- renderPlot(want2(unpaid_sec_job))
  
  # Tab panel: stories -----------------------------------
  
  c1_df <- unpaid_job1 %>%
    filter(sex == "2.Mujer", higher_ed == T) %>%
    select(area, age, marital, indigenous, edu, in_school, chronic_disease_1, num_alive_child,
           primary_job, work_type, want_work_more, avail_work_more)
  c1_df2 <- eventReactive(input$b1, {
    c1_df[sample(nrow(c1_df), 1), ]
  })
  
  output$t1 <- renderText(paste("Women with higher education working an unpaid job:", nrow(c1_df)))
  observeEvent(input$b1, {
    updateActionButton(session, "b1", label = " Tell me another story", icon = icon("redo"))
  })
  output$c1 <- renderTable({ c1_df2() })
  
  c2_df <- unpaid_sec_job %>%
    select(area, sex, age, marital, indigenous, edu, in_school, chronic_disease_1, num_alive_child,
           primary_job, work_type, primary_salary, primary_salary_freq, primary_nonsalaried_income, primary_nonsalaried_income_freq,
           sec_job, sec_employer_industry, sec_work_type,
           want_work_more, avail_work_more)
  c2_df2 <- eventReactive(input$b2, {
    c2_df[sample(nrow(c2_df), 1), ]
  })
  
  output$t2 <- renderText(paste("People with a paid primary job and an unpaid secondary job:", nrow(c2_df)))
  observeEvent(input$b2, {
    updateActionButton(session, "b2", label = " Tell me another story", icon = icon("redo"))
  })
  output$c2 <- renderTable({ c2_df2() })
  
  c3_df <- unpaid_pri_job %>%
    select(area, sex, age, marital, indigenous, edu, in_school, chronic_disease_1, num_alive_child,
           primary_job, work_type, sec_job, sec_employer_industry, sec_work_type, sec_salary, sec_salary_freq, sec_nonsalaried_income, sec_nonsalaried_income_freq,
           want_work_more, avail_work_more)
  c3_df2 <- eventReactive(input$b3, {
    c3_df[sample(nrow(c3_df), 1), ]
  })
  
  output$t3 <- renderText(paste("People with an unpaid primary job and a paid secondary job:", nrow(c3_df)))
  observeEvent(input$b3, {
    updateActionButton(session, "b3", label = " Tell me another story", icon = icon("redo"))
  })
  output$c3 <- renderTable({ c3_df2() })
  
  c4_df <- unpaid_job %>%
    filter(union_member == "1. Si") %>%
    select(area, sex, age, marital, indigenous, edu, in_school, literate, chronic_disease_1, num_alive_child,
           primary_job, work_type, sec_job, sec_employer_industry, sec_work_type, sec_salary, sec_salary_freq, sec_nonsalaried_income, sec_nonsalaried_income_freq,
           want_work_more, avail_work_more)
  c4_df2 <- eventReactive(input$b4, {
    c4_df[sample(nrow(c4_df), 1), ]
  })
  
  output$t4 <- renderText(paste("Union members with unpaid jobs:", nrow(c4_df)))
  observeEvent(input$b4, {
    updateActionButton(session, "b4", label = " Tell me another story", icon = icon("redo"))
  })
  output$c4 <- renderTable({ c4_df2() })
  
  c5_df <- with_job %>%
    filter(age < 10) %>%
    select(area, sex, age, indigenous, edu, in_school,
           primary_job, work_type, primary_salary, primary_salary_freq, primary_nonsalaried_income, primary_nonsalaried_income_freq,
           sec_job, sec_employer_industry, sec_work_type, union_member)
  c5_df2 <- eventReactive(input$b5, {
    c5_df[sample(nrow(c5_df), 1), ]
  })
  
  output$t5 <- renderText(paste("Child laborers under ten:", nrow(c5_df)))
  observeEvent(input$b5, {
    updateActionButton(session, "b5", label = " Tell me another story", icon = icon("redo"))
  })
  output$c5 <- renderTable({ c5_df2() })
}

shinyApp(ui = ui, server = server)
