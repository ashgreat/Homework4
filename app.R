library(ggplot2)
library(haven)
library(hrbrthemes) # I used this package for my plot theme
library(shiny)
library(shinydashboard)
library(MASS)

# We want more variables in the dropdown so we create a list beforehand

l1 <- c("SALEQ", "CHEQ", "ATQ", "OIADPQ", "XRDQ", "XSGAQ")
l2 <- c("Sales", "Cash", "Assets", "Profits", "R&D", "SG&A")


l12 <- setNames(l1, l2)

l3 <- c("lm", "loess", "rlm", NA)
l4 <- c("Linear Model", "LOESS", "Robust Linear", "None")
l34 <- setNames(l3, l4)

ui <- dashboardPage(
  dashboardHeader(title = "Apple Financials"),
  dashboardSidebar(sidebarMenu(
          fileInput("mydata",
            "Upload SAS Data:",
            accept = ".sas7bdat"
          ),
          selectInput("finvar1",
            "X-Axis Variable:",
            choices = l12,
            selected = "SALEQ"
          ),
          selectInput("finvar2",
            "Y-Axis Variable:",
            choices = l12,
            selected = "XRDQ"
          ),
          selectInput(
            "varscale",
            "Choose the Scale:",
            choices = c("Levels",
                        "Log 10")
          ),
          radioButtons("smooth",
            "Choose the Model:",
            choices = l34,
            selected = "loess"
          ),
          checkboxInput("se",
            "Standard Error Ribbon",
            value = TRUE
          ),
          conditionalPanel(
            condition = "input.smooth == 'loess'",
            sliderInput("span",
              "Span for LOESS",
              min = 0,
              max = 1,
              value = 0.75
            )
          )
          )
          ),
  dashboardBody(
    fluidRow(
      box(plotOutput("p1"), width = 500)
    )
  )
)

server <- function(input, output) {
  output$p1 <- renderPlot({
    text <- paste(
      "Please upload a SAS data file (sas7bdat extension) \n",
      "Make sure that it has the following variables: \n",
      "SALEQ, CHEQ, ATQ, OIADPQ, XRDQ, XSGAQ"
    )

    validate(
      need(!is.null(input$mydata), text)
    )
    
    validate(
      need(
        grepl(
          ".sas7bdat",
          input$mydata$name
        ),
        "File is not a SAS data set (.sas7bdat extension)"
      )
    )

    validate(
      need(
        input$finvar1 != input$finvar2,
        "X and Y variables have to be different"
      )
    )

        xlab <- setNames(l2, l1)[input$finvar1]
        ylab <- setNames(l2, l1)[input$finvar2]

    # Read the input data set

        d1 <- read_sas(input$mydata$datapath)

        p <- ggplot(d1, aes_string(x = input$finvar1,
                                   y = input$finvar2)) +
          geom_point() +
          labs(
            x = paste(xlab, "(million $)"),
            y = paste(ylab, "(million $)")
          ) +
        #  theme_bw()
        theme_ipsum_rc() # This is the theme I used.
    
        if (input$varscale == "Log 10") {
          p <- p +
            scale_x_log10() +
            scale_y_log10()
        }
        return(p + geom_smooth(
          method = input$smooth,
          se = input$se,
          span = input$span
        ))
  })
}

shinyApp(ui = ui, server = server)
