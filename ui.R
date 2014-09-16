library(shiny)

#   Get current time from system, and convert to local Alberta time.
now.ab <- Sys.time()
attr(now.ab, "tzone") <- "America/Denver"

#   Calculate initial arguments to date and HE parameters.
d.max <- as.Date(now.ab) - 60
now.he <- as.POSIXlt(now.ab)$hour + 1

#   Define Shiny user interface.
shinyUI(pageWithSidebar(
    headerPanel("Alberta Energy-Market Merit Order"),
    sidebarPanel(
        dateInput("date", "Date:", d.max, max=d.max),
        sliderInput("he", "Hour Ending:", value=now.he, min=1, max=24, step=1),
        submitButton("Submit")
    ),
    mainPanel(
        plotOutput("pp.plot"),
        plotOutput("emmo.plot"),
        p(a("Documentation", href="index.html"))
    )
))
