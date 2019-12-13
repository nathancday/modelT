
library(shiny)
library(tidyverse)

input_n <- 20

theme_set(theme_gray() + theme(axis.text = element_text(size = 16)))

# Define UI for application that draws a histogram
ui <- fluidPage(
    titlePanel("Model-T"),
    fluidRow(
        column(width = 3,
               wellPanel(
                   h3("Red:"),
                   sliderInput("red_mu", "mean", min = -4, max = 2, value = -1, step = .1),
                   sliderInput("red_sd", "std dev", min = .5, max = 3, value = 2, step = .1)
               )
        ),
        column(width = 6,
              verbatimTextOutput("tTest")
        ),
        column(width = 3,
               wellPanel(
                   h3("Blue:"),
                   sliderInput("blue_mu", "mean", min = -2, max = 4, value = 1, step = .1),
                   sliderInput("blue_sd", "std dev", min = .5, max = 3, value = 2, step = .1)
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
    
    dat <- reactive({
        
        red <- data.frame(x = rnorm(input_n, input$red_mu, input$red_sd),
                           pop = "red")
        blue <- data.frame(x = rnorm(input_n, input$blue_mu, input$blue_sd),
                           pop = "blue")
        
        bind_rows(red, blue)
        
    })
    
    test <- reactive({
        t.test(x ~ pop, data = dat())
    })
    
    output$dotPlot <- renderPlot({
        ggplot(dat(), aes(x = pop, y = x, color = pop)) +
            geom_point(size = 5, alpha = .5) +
            stat_summary(geom = "crossbar", fun.data = mean_sdl, fun.args = list(mult = 1)) +
            scale_color_manual(values = c("red" = "red", "blue" = "blue"), guide = FALSE) +
            coord_flip() +
            scale_y_continuous(limits = c(-10, 10)) +
            labs(title = "Sampled values with mean +/ standard deviation",
                 y = NULL, x = NULL)
    })
    
    output$distPlot <- renderPlot({
        
        print(test())
        
        mus <- dat() %>% 
            group_by(pop) %>% 
            summarise(mean = mean(x)) %>% 
            pull(mean)
        sds <- dat() %>% 
            group_by(pop) %>% 
            summarise(sd = sd(x)) %>% 
            pull(sd)
        
        ggplot(data.frame(x = -10:10), aes(x = x)) +
            stat_function(fun = dnorm, args = list(mean = mus[1], sd = input$red_sd),
                          fill = "blue", geom = "area", alpha = .25) +
            geom_vline(xintercept = mus[1], color = "blue", linetype = 2) +
            geom_text(x = mus[1], y = Inf,
                      label = paste0("mean=", signif(mus[1], 3), "\n", "sd=", signif(sds[1], 3)),
                      color = "blue", vjust = 1.2, size = 6) +
            stat_function(fun = dnorm, args = list(mean = mus[2], sd = input$blue_sd),
                          fill = "red", geom = "area", alpha = .25) +
            geom_vline(xintercept = mus[2], color = "red", linetype = 2) +
            geom_text(x = mus[2], y = -Inf,
                      label = paste0("mean=", signif(mus[2], 3), "\n", "sd=", signif(sds[2], 3)),
                      color = "red", vjust = -.5, size = 6) +
            labs(title = "Normal distributions approximated by sample stats",
                 y = NULL, x = NULL)
    })
    
    output$tTest <- renderPrint({
        test()
    })
}

shinyApp(ui = ui, server = server)
