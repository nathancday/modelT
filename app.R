library(Hmisc)
library(ggrepel)
library(shiny)
library(tidyverse)

theme_set(ggthemes::theme_fivethirtyeight() +
              theme(axis.text = element_text(size = 16),
                    plot.title = element_text(size = 24),
                    legend.position = "none"))

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
                   actionButton("resample", "Re-sample"),
                   sliderInput("num", "Number of samples:", min = 2, max = 20, value = 10, step = 1),
                   cellWidths = c("20%", "80%")
               ),
               checkboxInput("var_equal", "Equal varaince", FALSE),
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
                   input$resample,
                   input[["kb"]], input[["kb2"]]), {
                       print(input$kb2)
        red <- data.frame(x = rnorm(input$num, input$red_mu, input$red_sd),
                          pop = "red")
        blue <- data.frame(x = rnorm(input$num, input$blue_mu, input$blue_sd),
                           pop = "blue")
        
        rv$dat <- bind_rows(red, blue)
    })
    
    test <- reactive({
        
        # individual variance
        if (input$var_equal) {
            rv$sds <- rv$dat %>%
                summarise(sd = sd(x)) %>%
                pull(sd) %>%
                rep(2)
        } else {
            rv$sds <- rv$dat %>%
                group_by(pop) %>%
                summarise(sd = sd(x)) %>%
                pull(sd)
        }
        
        t.test(x ~ pop, data = rv$dat, var.equal = input$var_equal)
    })
    
    output$dotPlot <- renderPlot({
        ggplot(rv$dat, aes(x = pop, y = x, color = pop)) +
            geom_point(size = 5, alpha = .5) +
            scale_color_manual(values = c("red" = "red", "blue" = "blue"), guide = FALSE) +
            coord_flip() +
            scale_y_continuous(limits = c(-10, 10)) +
            labs(title = "Sampled values",
                 y = NULL, x = NULL)
    })
    
    output$distPlot <- renderPlot({
        
        mus <- rv$dat %>% 
            group_by(pop) %>% 
            summarise(mean = mean(x)) %>% 
            pull(mean)
        
        dat <- data.frame(
            group = c("blue", "red"),
            mu = mus,
            sd = rv$sds,
            y = c(Inf)
        )
        print(dat)
        
        ggplot(data.frame(x = -10:10), aes(x = x)) +
            stat_function(fun = dnorm, args = list(mean = mus[1], sd = rv$sds[1]),
                          fill = "blue", geom = "area", alpha = .25) +
            stat_function(fun = dnorm, args = list(mean = mus[2], sd = rv$sds[2]),
                          fill = "red", geom = "area", alpha = .25) +
            geom_vline(data = dat, aes(xintercept = mu, color = group)) +
            geom_label_repel(data = dat, aes(x = mus, y = y, color = group,
                                             label = paste0("mu = ", signif(mu, 3), "\n", "sd = ", signif(sd, 3))),
                             direction = "x", vjust = 1.2, size = 5, segment.size = 0) +
            scale_color_manual(values = c("blue", "red")) +
            scale_y_continuous(expand = expansion(mult = c(0, .5))) +
            labs(title = "Normal distributions approximated",
                 y = NULL, x = NULL)
    })
    
    output$tTest <- renderPrint({
        test()
    })
}

shinyApp(ui = ui, server = server)
