#pathway
setwd("~/Downloads/TJFV5-SOCS0100-Assignment-2")

#required packages
if (!require("pacman")) {
  install.packages("pacman")
}
library(pacman)
pacman::p_load(
  shiny,
  shinydashboard,
  ggplot2,
  dplyr,
  purrr,
  tidyverse,
  leaflet,
  plotly,
  tidyr
)

#load the CSV files
dataset <- read.csv("education-expenditure-and-gender-gap.csv")
str(dataset)
dataset <- dataset %>%
  janitor::clean_names()%>%
  mutate(`gender_literacy_gap`=(x_literacy_rate_15_years_over_males - x_literacy_rate_15_years_over_females))

view(dataset)

#Creating header
header <- dashboardHeader(title = "Directory")

#Creating sidebar
sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("Introduction",tabName = "introduction"),
    menuItem("Education Spending, GDP and Literacy Rates", tabName = "edu_GDP_literacy"),
    menuItem("Education Spending and Literacy Rates by Gender", tabName = "edu_and_literacy_gender"),
    menuItem("Education Expenditure and WBL Index",tabName = "education_and_WBL"),
    menuItem("Conclusion", tabName="conclusion")
  )
)

#Tab 1: Introduction
introduction <- tabItem(
  tabName = "introduction",
  box(
    width = 10,
  h1("Education Spending and Gender Equality in Singapore"),
  h3("Introduction"),
  "Singapore's rapid economic development, from the nation's independence in 1965 to the present, is oftentimes lauded as one of the biggest 
  success stories of development from third world to first. Singapore's education system has been apportioned much credit in shaping this economic trajectory,
  where Singapore's education reforms and programmes has sustained its international credibility as a high-quality public education system. Concurrently, its 
  progress in gender equality as come a long way, where Singapore was ranked first in Asia-Pacific for having a low level of gender inequality in the UN Human
  Development Report’s Gender Inequality Index (GII) published in March 2024.",
  br(),
  "This study thus aims to explore the relationships between Singapore's education expenditure, GDP growth and gender equality. Utilising the World Bank Data360 API
  and data sets made available by the Department of Statistics Singapore (Singstat), this study undertakes data wrangling and processing methods in RScript, 
  producing interactive data visualisation outputs using RShiny.",
  br(),
  h3("Interactive Data Visualisation"),
  "This RShiny Dashboard produces the interactive data visualisation elements that 
  Within the Directory, navigate between topics and explore the data through the interactive elements. Each page also presents a summary of key data features."
))

#Tab 2: Education, GDP and female literacy rates
edu_GDP_literacy_ui <- tabItem(
  tabName = "edu_GDP_literacy",
  h1("Education Spending per Capita, GDP and Literacy Rates"),
  fluidRow(
    box(
      width = 6,
      h3("Overview"),
      "This plot maps education spending per capita, GDP per capita and literacy rates (15 years and over)
      from the year 2000 to 2020. With Singapore's fast growing economy, the illustrated sustained increases in all variables
      is as expected.
      What is notable is that the education spending per capita to GDP per capita ratio slowly decreases over time, yet literacy rates
      continue sustaining increase. This suggests that upfront education costs may have long-term impacts on the population's literacy rates
      as a whole.",
      br()
    ),
    box(
      width = 6,
      h4("Explore by Year"),
      br(),
      sliderInput(
        inputId = "year",
        label = "Select Year",
        min = min(dataset$year),
        max = max(dataset$year),
        value = c(min(dataset$year),max(dataset$year)),
        step = 1,
        sep = ''
      )
    )),
  fluidRow(
    box(
      width = 12,
      h3("Graph"),
      plotOutput("plot1")
        )
    )
)

#Tab 3: Literacy Rates by Gender
edu_and_literacy_ui <- tabItem(
  tabName = "edu_and_literacy_gender",
  h1("Literacy Rates by Gender"),
  fluidRow(
    box(
      width = 12,
      h3("Overview"),
      "This section explores the relationships between education spending per capita, literacy rates and gender.",
      "Based on the bar graph, literacy rates for females 15 years old and older is shown to increase at a faster rate
    than males. When viewed in relation to education spending per capita, this illustrates that females benefitted more in terms
    of literacy rates from the increase in education spending than males.",
      br()
    )),
  fluidRow(
    box(
      width = 12,
      h3("Education Spending per Capita and Literacy Rates by Gender"),
      br(),
      selectInput(
        inputId = "gender",
        label = "Select gender",
        choices = c("Male","Female"),
        selected = "Male"
      ),
      plotOutput("plot3")
    )
  ),
  fluidRow(
    box(
      width = 6,
      h3("Literacy Rates by Gender"),
      br(),
      sliderInput(
        inputId = "year1",
        label = "Select Year",
        min = min(dataset$year),
        max = max(dataset$year),
        value = 2000,
        step = 1,
        sep = ''
      )),
    box(
      width = 6,
      br(),
      tableOutput("plot6"),
      br(),
      plotOutput("plot2")
    ))
)

#Tab 4:Education Expenditure, GDP per capita and WBL Index
education_and_WBL <- tabItem(
  tabName = "education_and_WBL",
  h2("Education Expenditure and WBL Index"),
  fluidRow(
    box(
      width=12,
      h3("Overview"),
      "The Women, Business and the Law Index evaluates how laws, regulations and government policies affect the economic opportunity
      that women have. Created by the World Bank Group, it measures women's economic opportunity across the following topics: Safety, Mobility, 
      Workplace, Pay, Marriage, Parenthood,Childcare, Entrepreneurship, Assets, and Pension. Weighing 40 indicators across the above 10 topics, 
      the WBL Index takes the unweighted average of the indicators scaled to 100.",
      br(),
      br(),
      "There is strong evidence to suggest that economic opportunity for women increase in tandem with increasing education attainment and economic growth in a country.
      This section thus displays the relationship between the WBL Index, GDP per capita and the gender literacy gap by year in a table. Users can select the year and explore
      the data accordingly."
    )
  ),
  fluidRow(
    box(
      width = 4,
      h3("Select Year"),
      selectInput(
        inputId = "year3",
        label = "Select Year",
        choices = dataset$year,
        selected = 2000
      ),
      tableOutput("plot4")
    ),
      box(
        width = 8,
        h3("Relationship between Education Expenditure and WBL Index"),
        selectInput(
          inputId = "factor",
          label = "Select",
          choices = c("GDP per capita","Education Expenditure per capita")
        ),
        plotOutput("plot5")
      )
    )
  )

#Tab 5: Conclusion
conclusion_ui <- tabItem(
  tabName = "conclusion",
  box(
    width = 10,
  h1("Conclusion"),
  "In conclusion, education spending is shown to be pivotal in uplifting women in Singapore. Through increasing literacy rates and
  educational attainment, it has resulted in a general increase in economic opportunity for women."
  )
) 

#putting dashboard body together 
body <- dashboardBody(
  tabItems(
    introduction,
    edu_GDP_literacy_ui,
    edu_and_literacy_ui,
    education_and_WBL,
    conclusion_ui
  )
)

#Creating User Interface
ui <- dashboardPage(
  header,
  sidebar,
  body
)

#Defining server
server <- function(input,output){
  output$plot1 <- renderPlot({
    df <- dataset %>%
      filter(year >= input$year[1],
             year <= input$year[2])
    
      ggplot(df)+
        geom_line(aes(x=year,y=education_spending_per_capita*37),col="#9d46db",linewidth=1)+
        geom_point(aes(x=year,y=gdp_per_capita_constant_lcu,size=literacy_rate_15_years_over),
                       color="#b84090",
                   alpha = 0.7)+
      scale_y_continuous(name="GDP per Capita", sec.axis=sec_axis(~./37,name="Education Spending per Capita"))+
      theme(
        axis.text.y.left=element_text(color="#b84090"),
        axis.text.y.right=element_text(color="#9d46db")
      )+ 
      scale_size(range=c(1,25),name="Literacy Rates (15 Years and over)") +
      labs(title = "Relationship between Education Spending per Capita, GDP per Capita and Literacy Rates",
           x = "Year",
           y = "GDP Per Capita")
  })
  
  output$plot2 <- renderPlot({
    df <- dataset %>%
      filter(year == input$year1) %>%
      mutate(across(c(x_literacy_rate_15_years_over_males,
                      x_literacy_rate_15_years_over_females),~ as.numeric(gsub(",", "", trimws(.)))
      )) %>%
      select(x_literacy_rate_15_years_over_males,
              x_literacy_rate_15_years_over_females) %>%
      pivot_longer(cols = everything(), names_to = "series", values_to = "number")
    
    ggplot(df, aes(x = series, y = number, fill = series)) +
      geom_col() +
      scale_x_discrete(labels = c(
        x_literacy_rate_15_years_over_males = "Male",
        x_literacy_rate_15_years_over_females = "Female"
      )) +
      labs(title = paste("Literacy rates (15 and over) in", input$year1),
           x = NULL, y = "Literacy rate (%)") +
      theme_minimal() +
      theme(legend.position = "none")
  })
  
  output$plot3 <- renderPlot({
    variable <- switch(input$gender,
                       "Male" = dataset$x_literacy_rate_15_years_over_males,
                       "Female" = dataset$x_literacy_rate_15_years_over_females)
    
    ggplot(dataset)+
      geom_smooth(aes(x=year,y=variable*20),fill = "#308dc2")+
      geom_line(aes(x=year,y=education_spending_per_capita),col="#28bd90",linewidth=1.5)+
      scale_y_continuous(name="Education Spending per Capita",
                         limits = c(1600,2500),
                         sec.axis = sec_axis(~./20,name = "Literacy Rate (%)"))
  })
  
  output$plot6 <- renderTable(
    dataset%>%
      filter(year == input$year1)%>%
      select(`Male Literacy Rate (15 and over)` = x_literacy_rate_15_years_over_males,
             `Female Literacy Rate (15 and over)` = x_literacy_rate_15_years_over_females))
  
  output$plot4 <- renderTable(
      dataset%>%
        filter(year == input$year3)%>%
          select(`WBL Index` = wbl_index_score,
                 `GDP per Capita` = gdp_per_capita_constant_lcu,
                 `Gender Literacy Gap (%)`= gender_literacy_gap)
  )
  
  output$plot5 <- renderPlot({
    variable <- switch(input$factor,
                       "GDP per capita"=dataset$gdp_per_capita_constant_lcu,
                       "Education Expenditure per capita"=dataset$education_spending_per_capita)
    
    variable_names <- c( "GDP per capita" = "GDP per Capita", 
                            "Education Expenditure per capita" = "Education Expenditure per Capita" ) 
    label_variable <- variable_names[input$factor] 
    if (is.na(label_variable)) label_variable <- input$factor
    
    scale_factors <- c("Education Expenditure per capita" = 23,
                       "GDP per capita" = 850)
    scale <- scale_factors[input$factor]
    if (is.na(scale)) scale <- input$factor

    ggplot(dataset)+
      geom_col(aes(x=year,y=variable),fill="#09839e",alpha=0.7)+
      geom_line(aes(x=year,y=wbl_index_score*scale),col="#a60a65",linewidth=2)+
      scale_y_continuous(name = label_variable,
        sec.axis = sec_axis(~./scale,name = "WBL Index Score"))+
      theme(
        axis.text.y.left=element_text(color="#09839e"),
        axis.text.y.right=element_text(color="#a60a65")
      )
  })
}

#Run the app
shinyApp(ui,server)