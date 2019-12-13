
library(shiny)
library(tidyverse)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Model-T"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            wellPanel(
                h3("Population 1:"),
                sliderInput("pop1_n",
                            "N",
                            min = 2,
                            max = 20,
                            value = 5),
                sliderInput("pop1_mu",
                            "mean",
                            min = -4,
                            max = 2,
                            value = -1,
                            step = .1),
                sliderInput("pop1_sd",
                            "std dev",
                            min = .5,
                            max = 3,
                            value = 2,
                            step = .1)   
            ),
            wellPanel(
                h3("Population 2:"),
                sliderInput("pop2_n",
                            "N",
                            min = 2,
                            max = 20,
                            value = 5),
                sliderInput("pop2_mu",
                            "mean",
                            min = -2,
                            max = 4,
                            value = 1,
                            step = .1),
                sliderInput("pop2_sd",
                            "std dev",
                            min = .5,
                            max = 3,
                            value = 2,
                            step = .1)
            )
        ),
        mainPanel(
            fluidRow(
                plotOutput("dotPlot", height = "200px")
            ),
            fluidRow(
                plotOutput("distPlot")
            ),
           fluidRow(
               verbatimTextOutput("tTest")
           )
        )
    )
)

server <- function(input, output) {
    
    dat <- reactive({
        
        pop1 <- data.frame(x = rnorm(input$pop1_n, input$pop1_mu, input$pop1_sd),
                           pop = "pop1")
        pop2 <- data.frame(x = rnorm(input$pop2_n, input$pop2_mu, input$pop2_sd),
                           pop = "pop2")
        
        bind_rows(pop1, pop2)
        
    })
    
    test <- reactive({
        t.test(x ~ pop, data = dat())
    })
    
    output$dotPlot <- renderPlot({
        ggplot(dat(), aes(x = pop, y = x, color = pop)) +
            geom_point(size = 4, alpha = .7) +
            stat_summary(geom = "crossbar", fun.data = mean_sdl, fun.args = list(mult = 1)) +
            scale_color_manual(values = c("pop1" = "red", "pop2" = "blue"), guide = FALSE) +
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
            stat_function(fun = dnorm, args = list(mean = mus[1], sd = input$pop1_sd),
                          fill = "red", geom = "area", alpha = .5) +
            geom_vline(xintercept = mus[1], color = "red", linetype =3) +
            geom_text(x = mus[1], y = Inf,
                      label = paste0("mean=", signif(mus[1], 3), "\n", "sd=", signif(sds[1], 3)),
                      color = "red", vjust = 1.2, size = 6) +
            stat_function(fun = dnorm, args = list(mean = mus[2], sd = input$pop2_sd),
                          fill = "blue", geom = "area", alpha = .5) +
            geom_vline(xintercept = mus[2], color = "blue", linetype =3) +
            geom_text(x = mus[2], y = -Inf,
                      label = paste0("mean=", signif(mus[2], 3), "\n", "sd=", signif(sds[2], 3)),
                      color = "blue", vjust = -.5, size = 6) +
            labs(title = "Normal distributions approximated by sample stats",
                 y = NULL, x = NULL)
    })
    
    output$tTest <- renderPrint({
        test()
    })
}

shinyApp(ui = ui, server = server)
