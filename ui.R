install.packages("shiny")
install.packages("leaflet")
install.packages("sf")
install.packages("dplyr")
install.packages("ggplot2")
install.packages("tidyr")
install.packages("shinyjs")
install.packages("bslib")
install.packages("gganimate")
install.packages("gifski")
install.packages("shinyBS")


library(shiny)
#Package to create leaflet map
library(leaflet)
#Library to load shape files
library(sf)
library(dplyr)
library(ggplot2)
library(tidyr)
#library for javascript functions
library(shinyjs)

library(bslib)
library(gganimate)
library(gifski)
library(shinyBS)


#Define the HTML and CSS features for the cards
shinyUI(fluidPage(
  useShinyjs(),
  tags$style(HTML('body {background-color: #F8F9F9;}')),
  tags$head(
    tags$style(HTML("
      body, .my-card, .card-header, .card-body, .card-header-trends, .card-body-trends, .card-header-plot, .card-body-plot {
        font-family: 'Times New Roman', Times, serif;
      }
    "))
  ),
  
  tags$style(
    
    HTML(
      '
      #Add font abd background colour
      body {
      background-color: #F8F9F9;
      font-family: "Times New Roman", sans-serif;
    }
      .my-card {
      #to add curved corners
        border-radius: 10px; 
        border-top: 2px solid #ADDFFF;
        margin-top: 10px;
        background-color: white;
        
      }
      .card-header {
        color: black; 
        #curved corners
        border-radius: 15px 15px 0 0; 
        padding: 5px; 
        margin-bottom: 5px; 
      }
      .card-body {
        padding: 10px; 
      }
      .my-card-trends {
        border-radius: 10px; 
        border-top: 2px solid #E2F516;
        margin-top: 10px;
        background-color: white;
      }
      #add features for the trends cards
      .card-header-trends {
        
        color: black;
        border-radius: 15px 15px 0 0; 
        padding: 5px;
        margin-bottom: 5px; 
      }
      .card-body-trends {
        padding: 5px;
      }

    .bold-text {
      font-weight: bold;
    }
    .map-column {
      height: 450px; 
    }
      .my-card-plot {
        border-radius: 10px;
        border-top: 2px solid #4BFAC8;
        margin-top: 10px;
        background-color: white;
      }
      .card-header-plot {
        
        color: black; 
        border-radius: 15px 15px 0 0; 
        padding: 5px; 
        margin-bottom: 5px; 
      }
      .card-body-plot {
        padding: 10px; 
      }
      '
      
    )
  ),
  #Set the title
  titlePanel(
    "Global Renewable Energy : Dependencies on C02|GDP|Weather Parameters"
  ),
  #Create the main panel
  mainPanel(
    width = 12,
    #create the individual tabs
    tabsetPanel(
      id = "tabs",
      tabPanel(
        "Main",
        #create 2 columns 
        fluidRow(column(
          width = 4,
          #The column to hold the About card
          div(
            class = "my-card",
            div(class = "card-header", "About"),
            div(
              class = "card-body",
              p(
                "The world is transitioning to a net-zero emissions focus, for future electricity supply.
                Most of the technologies used to achieve this are dependent on weather, such as wind and solar farms.
                Consequently,weather will play a substantial role in the energy produced from these technologies."),
              p("Through this narrative
                journey we take look at how Weather, C02 and GDP has affected Renewable Energy Generation from 2018 to 2019"),
                p("Take a look at the map on the right."
              )
              
            )
          ),
          #This column is for the trends card
          div(
            class = "my-card-trends",
            div(class = "card-header-trends", "Trends"),
            div(
              class = "card-body-trends",
              "The map shows that majority of the countries with highest renewable energy generation have high 
              temperature , while some countries with lower temperatures also contribute towards 
              renewable energy generation they could be dependent on Wind speed as their source of 
              renewable energy. Go ahead and explore the map, zoom in to see small countries and their weather 
              parameter distribution."
            )
          ),
          #This is for the description card
          div(
            class = "my-card",
            div(class = "card-header", "Description"),
            div(
              class = "card-body",
              p(
                "Climate change, characterised by global warming, is the defining challenge of our time.
                     It results from the excessive emission of greenhouse gases, primarily carbon dioxide (CO2) and methane (CH4),
                     into the atmosphere.
                     This rise in temperature negatively impacts the effectiveness of systems that generate renewable energy.
                   A factor that is positively related to the increase in CO2 emissions is GDP. This is due to the fact that
                     economic growth is often accompanied by increased energy consumption, which leads to countries relying on non-renewable
                     energy sources which in turn causes higher CO2 emissions.To understand this better click on the blue highlighted GDP or CO2 box."
              )
            )
          )
        ), column(
          #Display your main map
          width = 8, div(
            class = "my-card-plot",
            div(class = "card-header-plot", "MAP"),
            div(
              class = "card-body-plot",
              p(
                "The map represents distribution of weather across countries around the world."
              ),
              #Call the leaflet map
              div(class = "map-column", leafletOutput("main_map")),
              p(
                "Blue markers represent the 5 countries with maximum/minimum renewable
                energy generation. Toggle the buttons below to change the markers. 
                Hover on each country to gain more insights."
              )
            ),
            div(
              class = "card-footer",
              #Add buttons to sleect top 5 or bottom 5 
              radioButtons(
                "topBottom",
                "Select Top or Bottom 5:",
                choices = c("Top 5", "Bottom 5"),
                selected = "Top 5",
                inline = TRUE
              ),
              #add pop over when you hover on a button 
              bsPopover(
                id = "topBottom",
                title = "Instructions",
                content = "Select  to display the top 5 or bottom 5 countries based on renewable energy.",
                placement = "left",
                trigger = "hover"
              ),
              #add pop over when you hover on a button 
              bsPopover(
                id = "gdp",
                title = "Instructions",
                content = "Click to display bar chart",
                placement = "left",
                trigger = "hover"
              ),
              #add pop over when you hover on a button 
              bsPopover(
                id = "co",
                title = "Instructions",
                content = "Click to display bar chart",
                placement = "left",
                trigger = "hover"
              ),
              p(
                class = "bold-text",
                "Disclaimer: The visualization does not cover all countries around the globe."
              )
              
            )
          )
        )),
        fluidRow(
          #Trends information
          column(
            width = 4,
            div(
              id = "trends_section",
              class = "my-card-trends",
              div(class = "card-header-trends", "Trends"),
              div(
                class = "card-body-trends",
                p(
                  "From the graph notice, the average GDP of the top 5 countries is lower
                 when compared to the average GDP of the bottom 5 countries.
                 Similarly the average CO2 of the top 5 countries is lower when compared to
                 the average CO2 of the bottom 5 countries. A trend is noticed where countries with major
                 contribution towards renewable energy have lower GDP and more renewable energy sources also lowers their CO2
                 emissions whereas countries with a lower contribution towards renewable energy have higher GDP and more
                   dependence on non- renewable energy sources also increases their CO2 emissions."
                )
                
              )
            )
            
            ,
            #Trends information
            div(
              id = "trends_section",
              class = "my-card",
              
              div(class = "card-header", "Explore More"),
              div(
                class = "card-body",
                p(
                  "Countries around the globe are categorised into continents or regions,
              each region has different weather characteristics, to study individual regions and
              their efforts towards a sustainable environment navigate to the
              tabs at the top of the page , click on each tab to explore different regions."
                )
              )
            )
          ),
          
          column(width = 4, div(
            class = "my-card-plot",
            div(
              class = "card-header-plot",
              "GDP Animation",
              #Create the action button for GDP animation
              tags$div(style = "float: right;border: 2px solid #2D50EB;", actionButton("gdp", "GDP"))
            ),
            div(class = "card-body-plot", imageOutput("gdp_animation"))
          )),
          column(width = 4, div(
            class = "my-card-plot",
            div(
              class = "card-header",
              #Create the action button for CO2 animation
              "CO2 Animation",
              tags$div(style = "float: right;border: 2px solid #2D50EB;", actionButton("co", "CO2"))
            ),
            div(class = "card-body", imageOutput("ren_animation"))
          ))
        )
      ),
      #Africa tab
      tabPanel(
        "Africa",
        fluidRow(div(
          class = "my-card",
          #About information
          div(class = "card-header", "About"),
          div(
            class = "card-body",
            p(
              "Africa only accounts a tiny percentage of global CO2 emissions, renewables deployment has grown
                     substantially over the last decade, doubling between 2012 and 2022.
                     Some of the countries globally with the highest renewable energy contribution are in Africa.
                     Given the hot and airy weather of Africa solar energy is the
                     fastest-growing renewable energy resource with an average year on-year growth rate of 89%."
            ),
            p(
              class = "bold-text",
              "Disclaimer: The visualization does not cover all countries around the globe."
            ),
          )
        )),
        fluidRow(column(
          width = 8, div(
            #Plot the africa region map
            class = "my-card-plot",
            div(class = "card-header", "MAP"),
            div(
              class = "card-body",
              p(
                "The map on represents distribution of weather across countries around the world."
              ),
              #plotthe leaflet map
              leafletOutput("africa_map"),
              
              p(
                "Blue markers represent the 5 countries with maximum/minimum renewable
                energy generation."
              )
            ),
            div(class = "card-footer", )
          )
        ), column(
          
          width = 4, div(
            class = "my-card-plot",
            div(class = "card-header", "Nested Chart"),
            div(
              class = "card-body",
              p(
                "In order to understand how CO2 and GDP compare to the renewable energy
                                                    generation click on each country to view a
                                                   graph of varying sizes to represent CO2 , GDP and
                                                   Renewable energy generated in percentages."
              ),
              #plot the nested chart map
              plotOutput("nested_chart_africa")
            )
          )
        )),
        fluidRow(column(
          width = 4,
          
          div(
            id = "trends_section",
            class = "my-card-trends",
            div(class = "card-header-trends", "Trends"),
            div(
              class = "card-body-trends",
              p(
                "The map above represents the average temperature of
                       each country in Africa.
                       Notice countries with higher temperature have higher renewable energy
                       generated. To understand the trends as seen in the map, take a look at the
                       stacked chart on the right to view the renewable energy and weather parameter ditribution,select the weather
                       parameter from the weather drop down. To understand the stacked chart better smaller the area between the
                       two stacks higher is the dependency of renewable energy on the weather parameter."
              )
            )
          )
        ), column(
          #Plot the stacked chart
          width = 8, div(
            class = "my-card-plot",
            div(class = "card-header", "Stacked Chart"),
            div(class = "card-body", plotOutput("stacked_area_chart_africa"))
          )
        ))
      ),
      #Asia tab
      tabPanel(
        "Asia",
        fluidRow(div(
          class = "my-card",
          div(class = "card-header", "About"),
          div(
            class = "card-body",
            p(
              "Asia is the world’s largest and fastest growing consumer of energy as well as the largest emitter of CO2. This is a result of its rapid economic development, dominant manufacturing base and dependency on coal as the primary source of energy.
Rising to the occasion, few countries have recently pledged net zero targets by the middle of this century.
The prominent weather across the continent is usually arid hot summers and cold winters this leads to an uneven availability of both wind and solar resources, which are necessary for smoothing output, improving reliability of supply and reducing the need for storage.
"
            ),
            p(
              class = "bold-text",
              "Disclaimer: The visualization does not cover all countries around the globe."
            ),
          )
        )),
        fluidRow(column(
          width = 8, div(
            class = "my-card-plot",
            div(class = "card-header", "MAP"),
            div(
              class = "card-body",
              p(
                "The map on represents distribution of weather across countries around the world."
              ),#Plot the leaflet map
              leafletOutput("asia_map"),
              p(
                "Blue markers represent the 5 countries with maximum/minimum renewable
                energy generation."
              )
            )
          )
        ), column(
          width = 4, div(
            class = "my-card-plot",
            div(class = "card-header", "Nested Chart"),
            
            div(
              class = "card-body",
              p(
                "In order to understand how CO2 and GDP compare to the renewable energy
                                                    generation click on each country to view a
                                                   graph of varying sizes to represent CO2 , GDP and
                                                   Renewable energy generated in percentages."
              ),#Plot the nested chart
              plotOutput("nested_chart_asia")
            )
          )
        )),
        fluidRow(column(
          width = 4,
          
          div(
            id = "trends_section",
            class = "my-card-trends",
            div(class = "card-header-trends", "Trends"),
            div(
              class = "card-body-trends",
              p(
                "The map above represents the weather distribution of
                       countries in Asia.
                       Given the tropical and even weather of Asia notice a higher dependency of renewable
                       energy on wind over temperature in Asian countries.To understand the trends as seen in the map, take a look at the
                       stacked chart on the right to view the renewable energy and weather parameter ditribution,select the weather
                       parameter from the weather drop down. To understand the stacked chart better smaller the area between the
                       two stacks higher is the dependency of renewable energy on the weather parameter."
              )
              
            )
          )
        ), column(
          width = 8, div(
            class = "my-card-plot",
            div(class = "card-header", "Stacked Chart"),
            #plot the stacked chart
            div(class = "card-body", plotOutput("stacked_area_chart_asia"))
          )
        ))
      ),
      
      #Europe tab
      tabPanel(
        "Europe",
        fluidRow(div(
          class = "my-card",
          div(class = "card-header", "About"),
          div(
            class = "card-body",
            p(
              "The majority of Europe’s largest countries have fossil fuels as their
                     largest primary single source of electricity. Europe has been shifting towards
                     renewable energy sources for electricity generation over the past decade.
                    The expansion of wind and solar generation have been the primary drivers in this shift towards renewables,
                    Renewables now make up 44 percent of the electricity mix in the EU. Wind energy in particular soared
                    which generated 18 percent of electricity. Solar grew to 9 percent of the mix. Europe is known to have cool
                       summers and cold winters."
            ),
            p(
              class = "bold-text",
              "Disclaimer: The visualization does not cover all countries around the globe."
            ),
          )
        )),
        fluidRow(column(
          width = 8, div(
            class = "my-card-plot",
            div(class = "card-header", "MAP"),
            div(
              class = "card-body",
              p(
                "The map on represents distribution of weather across countries around the world."
              ),#plot the map
              leafletOutput("europe_map"),
              p(
                "Blue markers represent the 5 countries with maximum/minimum renewable
                energy generation."
              )
            )
          )
        ), column(
          width = 4, div(
            class = "my-card-plot",
            #plot the nested chart
            div(class = "card-header", "Nested Chart"),
            
            div(
              class = "card-body",
              p(
                "In order to understand how CO2 and GDP compare to the renewable energy
                                                    generation click on each country to view a
                                                   graph of varying sizes to represent CO2 , GDP and
                                                   Renewable energy generated in percentages."
              ),
              plotOutput("nested_chart_europe")
            )
          )
        )),
        fluidRow(column(
          width = 4,
          
          div(
            id = "trends_section",
            class = "my-card-trends",
            div(class = "card-header-trends", "Trends"),
            div(
              class = "card-body-trends",
              p(
                "The map above represents the weather distribution of
                       countries in Europe.
                       Given the cool continental weather of Europe notice an equal dependency of renewable
                       energy on wind and temperature in European countries.To understand the trends as seen in the map, take a look at the
                       stacked chart on the right to view the renewable energy and weather parameter ditribution,select the weather
                       parameter from the weather drop down. To understand the stacked chart better smaller the area between the
                       two stacks higher is the dependency of renewable energy on the weather parameter."
              )
              
            )
          )
        ), column(
          width = 8, div(
            #plot the stacked chart
            class = "my-card-plot",
            div(class = "card-header", "Stacked Chart"),
            div(class = "card-body", plotOutput("stacked_area_chart_europe"))
          )
        ))
      ),
      #america panel
      tabPanel(
        "Americas",
        fluidRow(div(
          class = "my-card",
          div(class = "card-header", "About"),
          div(
            class = "card-body",
            p(
              "North and South America has experienced a decoupling of GDP growth and CO2 emissions in
              recent years, meaning its economy has grown while emissions have decreased or 
              remained relatively flat.They use and produces many different types 
              and sources of energy, which can be grouped into general categories such as primary 
              and secondary, renewable, and fossil fuels.Primary energy sources include fossil fuels 
              (petroleum, natural gas, and coal), nuclear energy, and renewable sources of energy.
              Renewable energy production and consumption both reached record highs in recently at about 13%  
              of total energy consumption. The increases in recent years have been driven mainly by record-high solar and 
              wind energy production.The weather ranges from tropical conditions to artic , alpine conditions."
            ),
            p(
              class = "bold-text",
              "Disclaimer: The visualization does not cover all countries around the globe."
            ),
          )
        )),
        fluidRow(column(
          width = 8, div(
            #plot the map
            class = "my-card-plot",
            div(class = "card-header", "MAP"),
            div(
              class = "card-body",
              p(
                "The map on represents distribution of weather across countries around the world."
              ),
              leafletOutput("americas_map"),
              p(
                "Blue markers represent the 5 countries with maximum/minimum renewable
                energy generation."
              )
            )
          )
        ), column(
          width = 4, div(
            class = "my-card-plot",
            #plot the nested chart
            div(class = "card-header", "Nested Chart"),
            
            div(
              class = "card-body",
              p(
                "In order to understand how CO2 and GDP compare to the renewable energy
                                                    generation click on each country to view a
                                                   graph of varying sizes to represent CO2 , GDP and
                                                   Renewable energy generated in percentages."
              ),
              #plot the america chart
              plotOutput("nested_chart_americas")
            )
          )
        )),
        fluidRow(column(
          width = 4,
          
          div(
            id = "trends_section",
            class = "my-card-trends",
            div(class = "card-header-trends", "Trends"),
            div(
              class = "card-body-trends",
              p(
                "The map above represents the weather distribution of
                       countries in South and North America.
                       Given the alpine  weather of South and North America nmore dependent on Wind energy over solar energy 
                       because of its weather conditions.To understand the trends as seen in the map, take a look at the
                       stacked chart on the right to view the renewable energy and weather parameter ditribution,select the weather
                       parameter from the weather drop down. To understand the stacked chart better smaller the area between the
                       two stacks higher is the dependency of renewable energy on the weather parameter."
              )
              
            )
          )
        ), column(
          width = 8, div(
            class = "my-card-plot",
            div(class = "card-header", "Stacked Chart"),
            #plot the stacked chart
            div(class = "card-body", plotOutput("stacked_area_chart_americas"))
          )
        ))
      ),
      #oceania tab
      tabPanel(
        "Oceania",
        fluidRow(div(
          class = "my-card",
          div(class = "card-header", "About"),
          div(
            class = "card-body",
            p(
              "New Zealand and Fiji, along with other countries in Oceania, have made significant
                     strides in adopting renewable energy sources to meet their energy needs
                    Wind and solar energy are emerging as promising sources, with the government actively
                    promoting their development through regulatory reforms and funding initiatives
                    The Fijian government has implemented policies and incentives to promote the adoption of
                    renewable energy technologies, such as solar photovoltaic (PV) s. While New Zealand has a more temperate
                    climate with cooler temperatures and evenly distributed rainfall, Fiji experiences a tropical
                    climate with a distinct wet and dry season pattern, higher temperatures year-round.
                    The distinct wet patterns can affect the generation of wind an solar energy.
"
            ),
            p(
              class = "bold-text",
              "Disclaimer: The visualization does not cover all countries around the globe."
            ),
          )
        )),
        fluidRow(column(
          width = 8, div(
            #plot the map
            class = "my-card-plot",
            div(class = "card-header", "MAP"),
            div(
              class = "card-body",
              p(
                "The map on represents distribution of weather across countries around the world."
              ),
              leafletOutput("oceania_map"),
              p(
                "Blue markers represent the 5 countries with maximum/minimum renewable
                energy generation."
              )
            )
          )
        ), column(
          width = 4, div(
            class = "my-card-plot",
            #plot the nested chart
            div(class = "card-header", "Nested Chart"),
            
            div(
              class = "card-body",
              p(
                "In order to understand how CO2 and GDP compare to the renewable energy
                                                    generation click on each country to view a
                                                   graph of varying sizes to represent CO2 , GDP and
                                                   Renewable energy generated in percentages."
              ),
              plotOutput("nested_chart_oceania")
            )
          )
        )),
        fluidRow(column(
          width = 4,
          
          div(
            id = "trends_section",
            class = "my-card-trends",
            div(class = "card-header-trends", "Trends"),
            div(
              class = "card-body-trends",
              p(
                "The map above represents the weather distribution of
                       countries in Oceania.
                       Given the varying weather patterns in the Oceania coutries notice an varying dependency of renewable
                       energy on wind and temperature.To understand the trends as seen in the map, take a look at the
                       stacked chart on the right to view the renewable energy and weather parameter ditribution,select the weather
                       parameter from the weather drop down. To understand the stacked chart better smaller the area between the
                       two stacks higher is the dependency of renewable energy on the weather parameter."
              )
              
            )
          )
        ), column(
          width = 8, div(
            class = "my-card-plot",
            div(class = "card-header", "Stacked Chart"),
            #plot the stacked chart
            div(class = "card-body", plotOutput("stacked_area_chart_oceania")
               
          )
        )
        )
        
        
          
        )
      )),
      # add all data sources at the bottom of the visualization
      fluidRow(
        align = "center",
        div(
          class = "my-card",
          div(class = "card-header"),
          div(class = "card-body",
        p("All data used for the visualizations were obtained from the following sources"),
        uiOutput("tab"),
        uiOutput("tab2")
        )
        )
      )
    
  ),
  sidebarPanel(
    #to provide a moving drop down for weather parameters
    width = 12,
    style = "padding: 10px; position: fixed; top: 60px; right: 15px;border: 2px solid #2D50EB;",
    tags$div(
      style = "width: 150px;",
      selectInput(
        "data_type",
        "Weather Parameter",
        choices = c("Temperature", "Wind"),
        selected = "Temperature"
      )
    )
  )
)
)
