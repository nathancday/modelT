library(Hmisc)
library(shiny)
library(tidyverse)

# num <- 20

theme_set(theme_gray() + theme(axis.text = element_text(size = 16),
                               plot.title = element_text(size = 24)))

# Define UI for application that draws a histogram
ui <- fluidPage(
    titlePanel("Model-T"),
    fluidRow(
        column(width = 3,
               wellPanel(
                   h3("Red:"),
                   sliderInput("red_mu", "mean", min = -5, max = 5, value = 0, step = .1),
                   sliderInput("red_sd", "sd", min = .5, max = 3, value = 2, step = .1)
               )
        ),
        column(width = 6,
               splitLayout(
                  sliderInput("num", "Number of samples:", min = 2, max = 20, value = 10, step = 1),
                  actionButton("resample", "Re-sample"),
                  cellWidths = c(300, 100)
               ),
              verbatimTextOutput("tTest")
        ),
        column(width = 3,
               wellPanel(
                   h3("Blue:"),
                   sliderInput("blue_mu", "mean", min = -5, max = 5, value = 0, step = .1),
                   sliderInput("blue_sd", "sd", min = .5, max = 3, value = 2, step = .1)
               )
        )
    ),
    fluidRow(
        plotOutput("dotPlot", height = "150px")
    ),
    fluidRow(
        plotOutput("distPlot", height = "200px")
    )
)

server <- function(input, output) {
    
    rv <- reactiveValues()
    
    observeEvent(c(input$num,
                   input$red_mu, input$red_sd,
                   input$blue_mu, input$blue_sd,
                   input$resample), {
        red <- data.frame(x = rnorm(input$num, input$red_mu, input$red_sd),
                          pop = "red")
        blue <- data.frame(x = rnorm(input$num, input$blue_mu, input$blue_sd),
                           pop = "blue")
        
        rv$dat <- bind_rows(red, blue)
    })
    
    test <- reactive({
        t.test(x ~ pop, data = rv$dat)
    })
    
    output$dotPlot <- renderPlot({
        ggplot(rv$dat, aes(x = pop, y = x, color = pop)) +
            geom_point(size = 5, alpha = .5) +
            stat_summary(geom = "crossbar", fun.data = mean_sdl, fun.args = list(mult = 1)) +
            scale_color_manual(values = c("red" = "red", "blue" = "blue"), guide = FALSE) +
            coord_flip() +
            scale_y_continuous(limits = c(-10, 10)) +
            labs(title = "Sample of values w/ mean +/ sd",
                 y = NULL, x = NULL)
    })
    
    output$distPlot <- renderPlot({
        
        mus <- rv$dat %>% 
            group_by(pop) %>% 
            summarise(mean = mean(x)) %>% 
            pull(mean)
        
        # individual variance
        sds <- rv$dat %>%
            group_by(pop) %>%
            summarise(sd = sd(x)) %>%
            pull(sd)
        
        # pooled variance
        # sds <- rv$dat %>% 
        #     summarise(sd = sd(x)) %>% 
        #     pull(sd) %>% 
        #     rep(2)
        
        ggplot(data.frame(x = -10:10), aes(x = x)) +
            stat_function(fun = dnorm, args = list(mean = mus[1], sd = sds[1]),
                          fill = "blue", geom = "area", alpha = .25) +
            geom_vline(xintercept = mus[1], color = "blue", linetype = 2) +
            geom_text(x = mus[1], y = Inf,
                      label = paste0("mean=", signif(mus[1], 3), "\n", "sd=", signif(sds[1], 3)),
                      color = "blue", vjust = 1.2, size = 6) +
            stat_function(fun = dnorm, args = list(mean = mus[2], sd = sds[2]),
                          fill = "red", geom = "area", alpha = .25) +
            geom_vline(xintercept = mus[2], color = "red", linetype = 2) +
            geom_text(x = mus[2], y = -Inf,
                      label = paste0("mean=", signif(mus[2], 3), "\n", "sd=", signif(sds[2], 3)),
                      color = "red", vjust = -.5, size = 6) +
            labs(title = "Distributions approximated by the sample",
                 y = NULL, x = NULL)
    })
    
    output$tTest <- renderPrint({
        test()
    })
}

shinyApp(ui = ui, server = server)
