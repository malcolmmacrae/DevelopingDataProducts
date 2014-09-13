my.tz = Sys.timezone()

#   Check if EMMO was already downloaded; otherwise download to date-stamped file.
get.emmo <- function(d) {
    
    #   Create EMMO subdirectory if it does not already exist.
    if(file.exists("./emmo") == FALSE) dir.create("emmo")
    
    #   Determine EMMO filename from input date.
    d.mdyn <- format(d, "%m%d%Y")
    emmo.csv <- paste("./emmo/", d.mdyn, ".csv", sep="")
    
    #   Download EMMO CSV file to subdirectory if it does not already exist.
    if (file.exists(emmo.csv) == FALSE) {
        
        emmo.url <- paste("http://ets.aeso.ca/ets_web/ip/Market/Reports/MeritOrderSnapshotEnergyReportServlet?",
                          "beginDate=",d.mdyn,
                          "&endDate=",
                          "&contentType=csv",
                          sep="")
        download.file(emmo.url, emmo.csv, method="internal", mode="wb")
        
    }
    
    #   Return EMMO dataset to caller.
    return(read.table(emmo.csv, sep=",", blank.lines.skip=TRUE, skip=1, header=TRUE))
    
}


#   Load and clean daily EMMO.
load.emmo <- function(d) {
    
    emmo <- transform(get.emmo(d),
                      Date = as.POSIXct(Date, format="%m/%d/%Y"),
                      HE = as.factor(HE))
    emmo <- emmo[order(emmo$Date, emmo$HE, emmo$Price, -emmo$Size, emmo$Asset.Id), ]
    
    return(emmo)
}


#   Subset daily EMMO to hourly.
load.emmo.he <- function(emmo, he) {
    
    #   Subset daily EMMO to hourly.
    emmo.he <- emmo[emmo$HE == he,]
    row.names(emmo.he) <- NULL
    
    #   Calculate cumulative sum of offer sizes.
    emmo.he$Size.cum <- cumsum(emmo.he$Size)
    
    #   Return hourly EMMO to caller.
    return(emmo.he)
    
}


#   Check if pool price was already downloaded; otherwise download to date-stamped file.
get.pp <- function(d) {
    
    #   Create pool price subdirectory if it does not already exist.
    if(file.exists("./pp") == FALSE) dir.create("pp")
    
    #   Determine pool price filename from input date.
    d.mdyn <- format(d, "%m%d%Y")
    pp.csv <- paste("./pp/", d.mdyn, ".csv", sep="")
    
    #   Download pool price CSV file to subdirectory if it does not already exist.
    if (file.exists(pp.csv) == FALSE) {
        
        pp.url <- paste("http://ets.aeso.ca/ets_web/ip/Market/Reports/HistoricalPoolPriceReportServlet?",
                        "beginDate=",d.mdyn,
                        "&endDate=",d.mdyn,
                        "&contentType=csv",
                        sep="")
        download.file(pp.url, pp.csv, method="internal", mode="wb")
        
    }
    
    #   Return daily pool price dataset to caller.
    return(read.table(pp.csv, sep=",", blank.lines.skip=TRUE, skip=3, header=TRUE))
    
}

#   Load and clean daily pool price
load.pp <- function(d) {
    
    pp <- get.pp(d)
    names(pp) <- c("Date.HE", "Price", "RAvg.30d", "System.Demand")
    
    pp <- cbind(pp,
                t(matrix(unlist(strsplit(as.character(pp$Date.HE), " ")), nrow=2, ncol=24, dimnames=list(c("Date","HE"), NULL))))
    pp <- transform(pp, 
                    Date = as.POSIXct(Date, format="%m/%d/%Y"),
                    HE = as.numeric(HE))
    
    return(pp)
}


shinyServer(
    function(input, output) {
        
        output$d <- renderText({format(input$date, "%Y-%m-%d")})
        
        output$emmo.plot <- renderPlot({
            
            d <- as.POSIXct(format(input$date, "%Y-%m-%d"), format="%Y-%m-%d")
            
            emmo <- load.emmo(d)
            emmo.he <- load.emmo.he(emmo, input$he)

            pp <- load.pp(d)
            pp.he <- pp$Price[which(pp$HE == input$he)]
            
            plot(c(0, emmo.he$Size.cum), c(0, emmo.he$Price), col="blue", type="S",
                 xlab="System Load (MW)", ylab="Offer Price ($/MWh)",
                 main=paste(format(d, "%d-%b-%y"),
                            "HE", formatC(input$he, width=2, format="d", flag="0")))
            legend("topleft", paste("Pool Price: $", formatC(pp.he, digits=2, format="f"), sep=""), bty="n")
            abline(pp.he,0,col="red")
            
        })
    }
)