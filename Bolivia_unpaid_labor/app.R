library(shiny)
library(shinydashboard)
library(tidyverse)
library(shinyWidgets)

source("EDA.R")
c1 <- "General population"
c2 <- "People with at least 1 job, paid or unpaid"
c3 <- "People with at least 1 unpaid job"
c4 <- "People with a paid primary job and an unpaid secondary job"

# UI -------------------------------
ui <- fluidPage(
  tags$style(type = "text/css",
             "h1, h2, h4 { text-align: center; }",
             "p { text-align: center; color: grey; }",
             "hr { margin-top: 2em; margin-bottom: 2em; }"),
  
  tabsetPanel(
    # Tab panel: figures -----------------
    tabPanel("Figures",
             box(h2("Column overview"),
                 fluidRow(
                   column(3,
                          p(c1),
                          h1(textOutput("pop")),
                          p("people")),
                   column(3,
                          p(c2),
                          h1(textOutput("with_job")),
                          p("people")),
                   column(3,
                          p(c3),
                          h1(textOutput("unpaid")),
                          p("people")),
                   column(3,
                          p(c4),
                          h1(textOutput("unpaid_sec")),
                          p("people"))
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
             actionBttn("b1", icon = icon("redo"), style = "material-circle"),
             tableOutput("c1"),
             hr(),
             h3(textOutput("t2")),
             actionBttn("b2", icon = icon("redo"), style = "material-circle"),
             tableOutput("c2"),
             hr(),
             h3(textOutput("t3")),
             actionBttn("b3", icon = icon("redo"), style = "material-circle"),
             tableOutput("c3"))
  )
)

# Server -----------------------------
server <- function(input, output, session) {
  output$pop <- renderText(nrow(personas))
  output$with_job <- renderText(nrow(with_job))
  output$unpaid <- renderText(nrow(unpaid_job))
  output$unpaid_sec <- renderText(nrow(unpaid_sec_job))
  
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
    df$primary_job_salary <- replace(df$primary_job_salary, is.na(df$primary_job_salary), 0)
    
    ggplot(df) +
      geom_violin(aes(x = sex, y = primary_job_salary, fill = sex), color = NA, alpha = 0.75, width = 1) +
      geom_boxplot(aes(x = sex, y = primary_job_salary), width = 0.3, color = "black", fill = NA) +
      theme_minimal() +
      theme(legend.position = "bottom", legend.title = element_blank())
  }
  
  output$salary_t <- renderText("Wage income:")
  output$pop_salary <- renderPlot(salary(personas))
  output$with_job_salary <- renderPlot(salary(with_job))
  output$unpaid_salary <- renderPlot(salary(unpaid_job))
  output$unpaid_sec_salary <- renderPlot(salary(unpaid_sec_job))
  
  irr <- function(df) {
    df$primary_job_nonsalaried_income <- replace(df$primary_job_nonsalaried_income, is.na(df$primary_job_nonsalaried_income), 0)
    
    ggplot(df) +
      geom_violin(aes(x = sex, y = primary_job_nonsalaried_income, fill = sex), color = NA, alpha = 0.75, width = 1) +
      geom_boxplot(aes(x = sex, y = primary_job_nonsalaried_income), width = 0.3, color = "black", fill = NA) +
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
  
  c1_df <- unpaid_job1 %>%
    filter(sex == "2.Mujer", higher_ed == T) %>%
    select(area, age, marital, indigenous, edu, in_school, chronic_disease_1, num_alive_child,
           primary_job, work_type, want_work_more, avail_work_more)
  c1_df2 <- eventReactive(input$b1, {
    c1_df[sample(nrow(c1_df), 1), ]
  })
  
  output$t1 <- renderText(paste("Women with higher education working an unpaid job:", nrow(c1_df)))
  output$c1 <- renderTable({ c1_df2() })
  
  c2_df <- unpaid_pri_job %>%
    select(area, sex, age, marital, indigenous, edu, in_school, chronic_disease_1, num_alive_child,
           primary_job, work_type, sec_job, sec_employer_industry, sec_work_type, sec_salary, sec_salary_freq, sec_income, sec_income_freq,
           want_work_more, avail_work_more)
  c2_df2 <- eventReactive(input$b2, {
    c2_df[sample(nrow(c2_df), 1), ]
  })
  
  output$t2 <- renderText(paste("People with an unpaid primary job and a paid secondary job:", nrow(c2_df)))
  output$c2 <- renderTable({ c2_df2() })
  
  c3_df <- unpaid_job %>%
    filter(union_member == "1. Si") %>%
    select(area, sex, age, marital, indigenous, edu, in_school, chronic_disease_1, num_alive_child,
           primary_job, work_type, sec_job, sec_employer_industry, sec_work_type, sec_salary, sec_salary_freq, sec_income, sec_income_freq,
           want_work_more, avail_work_more)
  c3_df2 <- eventReactive(input$b3, {
    c3_df[sample(nrow(c3_df), 1), ]
  })
  
  output$t3 <- renderText(paste("Union members with unpaid jobs:", nrow(c3_df)))
  output$c3 <- renderTable({ c3_df2() })
}

shinyApp(ui = ui, server = server)
