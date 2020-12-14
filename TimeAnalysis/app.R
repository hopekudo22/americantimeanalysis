library(shiny)
library(tidyverse)
library(ggplot2)
library(atus)
library(shinythemes)
library(readr)
library(dplyr, warn.conflicts = FALSE)
library(ggforce)
library(gt)
library(gtsummary)
library(ggthemes)
library(rstanarm)
library(broom.mixed)

#Load ATUS data
fulldata <- read.csv("fullset.csv")


# Define UI for application that draws a histogram
ui <- navbarPage(
    
    theme = shinytheme("yeti"),
    "How Americans Spend Time During the Day",
    
    tabPanel("Sleep Models",
             titlePanel("How Does Family Income Influence Sleep?"),
             fluidRow(column(12, 
                             h3("Distribution of Hours Slept Based on Income"),
                             h4("Determining the distribution of hours slept based on family 
                     income from survey responses 2012-2016"),
                             plotOutput("Plot1"))),
    sidebarPanel(
    titlePanel("Interpretation of Regression Tables"),
                 p("Select a factor to see a regression table detailing the results. The regression tables 
                 were created using a Bayesian generalized linear model, using stan_glm, to model the 
                 relation between the average amount of hours slept during the night and the factors of 
                 family income, race, sex, and education level. Refer to the Discussion tab for an analysis
                 of these Regression tables."),
                 selectInput(inputId = "regressiontable",
                             label = "Select Factor:",
                             choices = c("Sleep and Family Income",
                                         "Sleep and Race",
                                         "Sleep and Gender",
                                         "Sleep and Education Level")),
    gt_output(outputId = "regressiontable")),
    
             mainPanel(
                 titlePanel("Sleep and Influencing Factors"),
                 plotOutput(outputId = "sleepincome"),
                 plotOutput(outputId = "sleeprace"),
                 plotOutput(outputId = "sleepgender"),
                 plotOutput(outputId = "sleepedu"))),
    tabPanel("Discussion",
             titlePanel("Interpreting Sleep Regression Tables"),
             p("This section provides an analysis of the Interactive Regression Tables
               displayed on the Sleep Models tab."),
             h3("Sleep and Family Income"),
             p("The first model regresses"),
             h3("Sleep and Race"),
             p("This model .."),
             h3("Sleep and Gender"),
             p("This model..."),
             h3("Sleep and Level of Education"),
             p("This model...")
             ),
    
    tabPanel("State Comparison",
             fluidPage(
                 fluidRow(column(12,
                                 h3("Time Usage Distriubtions Based on State"),
                                 p("To further analyze the data, I filtered the information for specific states."))),
                 fluidRow(column(12,
                                 h4("Activities by State"),
                                 selectizeInput(inputId = "stateInput",
                                                label = "State",
                                                choices = unique(fulldata$state), #add data set
                                                selected = "Hawaii"),
                                 plotOutput("Plot2")))
                 )),
             
  tabPanel("About",
           fluidPage(
               fluidRow(column(12,
                               h3("Project Background"),
                               p("This project utilizes data from the American Time Use Survey (ATUS) that collects 
                                 information on how respondents spend their time along with socioeconomic demographics.
                                 Through data visualizations and analysis, we are able to take a deeper dive into how
                                 Americans spend their time and how different variables such as income, location, or sex,
                                 among others can influence time usage. I modelled the differences in time spent in different states. I chose to focus on how the average American spends their time. 
                                 By looking at a wide range of activities such as how much someone sleeps in a day, the amount of housework they do, the time
                                 they spendon sporting events, and so forth. The time data can be organized by state, family income,
                                 education, age, and type of job."),
                               h3("Source Information"),
                               p("I used data from the American Time Usage Survey over the years of 2003 - 2016 from library(atus).
                               The American Time Use Survey is an annual survey that is conducted on a sample of individuals across the
                               United States and studies how individuals spend their time over the course of a day. Individual respondents
                               are interviewed about what activities they engaged in, the time duration spent on those activities, at what
                               locations, and int he presence of which individuals. These activities are subsequently encoded into tier codes
                               of activities. I filtered the data in order to make it easier to work with, going from over 2 million data 
                               points to approximately 15,000. I combined individual tier codes of specific activities into broader categories
                               that comprise of time spent sleeping, on housework, volunteering, playing sports, practicing religion, eating
                               and drinking, shopping, research and homework, time in class, watching television, drug usage, and computer usage.
                                 thus, the data is from the years 2012-2017."),
                               h3("About Me"),
                               p("My name is Hope Kudo and I'm a junior at Harvard studying Government and Psychology. As a proud
                               member of the Class of 2022 and Quincy House, living at home this semester has been quite a shake up,
                               but this project has been a great to delve into interesting data and a great way to experiment with R!
                               I'm excited to put the skills I've learned this semester into use. I can be reached at hopekudo@college.harvard.edu. 
                               The code for this project can be found at my", a("GitHub page here.", href = "https://github.com/hopekud22/50proj"))
                      )
             )
    )))
    
#Define server logic
    
server <- function(input, output, session) {
        
    output$Plot1 <- renderPlot({
        fulldata %>%
            ggplot(aes(x = sleep, fill = famincome)) +
            geom_histogram() +
            facet_wrap(~ famincome) +
            theme(axis.text = element_text(size = 5), strip.text = element_text(size = 7),
                  panel.grid = element_blank(), panel.spacing.x = unit(3, "mm"),
                  axis.ticks = element_blank(), axis.ticks.y = element_blank()) +
            scale_fill_discrete(name = "Family Income") +
            labs(title = "Distribution of Hours Slept Based on Income",
                 subtitle = "Determining the distribution of hours slept based on family income from survey responses 2012-2016",
                 x = "Hours Slept",
                 y = "",
                 caption = "Source: ATUS data") +
            theme_linedraw()
    })

    output$sleepincome <- renderPlot({
        sleepincome <- data %>%
            ggplot(aes(x = sleep, y = famincome)) +
            geom_boxplot() +
            theme_bw() +
            labs(x = "Hours of Sleep",
                 y = "Family Income ($)",
                 title = "Distribution of Hours of Sleep Linked to Income")
        
        sleepincome
    })
        

    output$sleeprace <- renderPlot ({
        sleeprace <- data %>%
            ggplot(aes(x = sleep, y = race)) +
            geom_boxplot() +
            theme_bw() +
            labs(x = "Hours of Sleep",
                 y = "Race",
                 title = "Distribution of Hours of Sleep Linked to Race")
        
        sleeprace
    })
    
    output$sleepgender <- renderPlot({
        sleepgender <- data %>%
            ggplot(aes(x = sleep, y = sex)) +
            geom_boxplot() +
            theme_bw() +
            labs(x = "Hours of Sleep",
                 y = "Sex",
                 title = "Distribution of Hours of Sleep Linked to Sex")
        
        sleepgender
    })
        
    output$sleepedu <- renderPlot ({
        sleepedu <- data %>%
            ggplot(aes(x = sleep, y = edu)) +
            geom_boxplot() +
            theme_bw() +
            labs(x = "Hours of Sleep",
                 y = "Level of Education",
                 title = "Distribution of Hours of Sleep Linked to Level of Education")
        
        sleepedu
    })
        
        regressiontableInput <- reactive ({
            switch(input$regressiontable,
                   "Sleep and Family Income" = formula(sleep ~ famincome),
                   "Sleep and Race" = formula(sleep ~ race),
                   "Sleep and Gender" = formula(sleep ~ sex),
                   "Sleep and Education Level" = formula(sleep ~ edu))
            
        })
        
        output$regressiontable <- render_gt({
            formula <- regressiontableInput()
            set.seed(100)
            fit_obj <- stan_glm(formula,
                                data = fulldata,
                                family = gaussian(),
                                refresh = 0) 
            fit_obj %>%
                tidy() %>%
                gt() %>%
                tab_header(title = "Regression of Factors Impact on Sleep") %>% 
                tab_source_note("Source: ATUS data") 
            
        })
        
        #new model
        output$Plot2 <- renderPlot({
            ggplot(data = fulldata, aes(x = sleep, .data[[input$y]])) +
                geom_histogram(state = "fill") +
                labs(title = "Demographic Distributions by State",
                     x = "States") +
                theme_linedraw()
        }, res = 96)
        
    }
    
    # Run the application 
    shinyApp(ui = ui, server = server)
    