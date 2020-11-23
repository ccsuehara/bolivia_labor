library(tidyverse)
library(shiny)
library(shinythemes)

# olive_green <- "#99b18f"
# 
# headings <- data.frame(
#   "title" = c("Age", "Education", "Labor force\nparticipation", "Rural/urban", "Indigenous\nidentity"),
#   "x" = c(2, 9.5, 6.5, 8.9, 1.5),
#   "y" = c(10, 9, 6.4, 2.8, 1.2)
# )
# 
# subh <- data.frame(
#   "title" = c("Age", "Education", "Labor force\nparticipation", "Rural/urban", "Indigenous\nidentity")
# )
# 
# ggplot(headings) +
#   geom_point(aes(x = x, y = y), size = 60, color = olive_green) +
#   geom_point(aes(x = x, y = y), size = 55, color = "white") +
#   geom_text(aes(x = x, y = y, label = title), color = olive_green, size = 5.5) +
#   # theme_void() +
#   coord_fixed() +
#   xlim(0, 11) + ylim(0, 11)

ui <- fluidPage(
  theme = shinytheme("sandstone"),
  navbarPage("Landing page",
             tabPanel("Home",
                      imageOutput("landing",
                                  width = "1130px",
                                  # height = "630px",
                                  click = clickOpts(id = "hover")),
                      verbatimTextOutput("coords")))
)

server <- function(input, output, session) {
  output$landing <- renderImage(list(src = "www/landing_page.png", width = "100%"), deleteFile = F)
  output$coords <- renderPrint(input$hover)
}

shinyApp(ui = ui, server= server)