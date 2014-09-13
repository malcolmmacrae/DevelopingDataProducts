library(shiny)

d.max <- Sys.Date()-60
now.he <- as.POSIXlt(Sys.time())$hour + 1

shinyUI(pageWithSidebar(
    headerPanel("Alberta Energy-Market Merit Order"),
    sidebarPanel(
        dateInput("date", "Date:", d.max, max=d.max),
        sliderInput("he", "Hour Ending:", value=now.he, min=1, max=24, step=1),
        submitButton("Submit")
    ),
    mainPanel(
        plotOutput("emmo.plot"),
        p(a("Documentation", href="index.html"))
    )
))
