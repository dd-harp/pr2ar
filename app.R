###
# Interactive visualization of deriving attack rate from PfPR
###
library(devtools)
if(!("pr2ar"  %in% rownames(installed.packages()))) devtools::install_github("dd-harp/pr2ar")
library(shiny); library(data.table); library(ggplot2); library(pr2ar)

# Define UI for application that draws an SIS model
ui <- fluidPage(

    # Application title
    titlePanel("PR2AR"),

    # Sidebar with a slider input for number of bins
    sidebarLayout(
        sidebarPanel(
            # sliderInput("N",
            #             "Population size",
            #             min = 100,
            #             max = 10000,
            #             value = 100),
            # sliderInput("beta",
            #             "Force of infection",
            #             min = 0,
            #             max = 1,
            #             value = 0.5),
            # sliderInput("alpha",
            #             "Recovery rate",
            #             min = 0,
            #             max = 1,
            #             value = 0.2),
            sliderInput("PR",
                        "Observed prevalence",
                        min = 0,
                        max = 1,
                        step = 0.05,
                        value = 0.1,
                        animate = T),
            # sliderInput("Tx",
            #             "Case management rate (prop cases treated promptly)",
            #             min = 0,
            #             max = 1,
            #             step = 0.05,
            #             value = 0.1,
            #             animate = T),
            width = 3
        ),
        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("RcPlot")
        )
    )
)

# Define server logic required to draw plot
server <- function(input, output) {

    # R0 <- function(X, E = 5) {
    #     S = 1
    #     c = 2
    #     b_r = 0.15
    #     alpha = 0.5
    #     R0 = E * (b_r) * (1 + c*S*X) / X * ( 1 + alpha)
    #     return(R0)
    # }

    plot.fn <- function(input) {
        PR = input$PR
        PR = 1
        Tx = seq(0, 0.98, 0.02)
        PAR = list(In = 1, Cn = 1, A = 0.2, Q = 0.95, dt = 10, rho = Tx, d = 0)
        Bfn = makeBdrugs

        outA <- c()
        for(i in 1:length(Tx)) {
            PAR$rho <- Tx[i]
            A <- PR2AReq(PR, Tx, PAR, Bfn)$A
            outA <- c(outA, A)
        }
        outA[is.na(outA)] <- 1

        plot(Tx, outA, type = "l", ylim = c(0, 1), xlim = c(0, 1), xlab = "Treatment Rate", ylab = "Attack-rate")
    }


    output$RcPlot <- renderPlot({
        plot.fn(input)
    }, width = 600, height = 500)
}

# Run the application
shinyApp(ui = ui, server = server)
