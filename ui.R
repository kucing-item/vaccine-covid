shinyUI(fluidPage(
  tags$head(tags$style(
    HTML(
      "
          table.dataTable tbody th, table.dataTable tbody td {
    padding: 8px 10px;
    background: grey;
    font-size: 12px;
}"
    )
  )),

use_sever(),
useShinyalert(),

navbarPage(
  "COVID-19 and Vaccines Update",
  theme = shinytheme("sandstone"),
  
  
  tabPanel("Covid-19 Update",
           
           sidebarLayout(
             sidebarPanel(
               width = 4,
               p(paste(
                 "Updated: ",
                 format(
                   x = Sys.time() %>% ymd_hms(),
                   format("%A, %d %B %Y %H:%M:%S")
                 )
               )),
               
               p("Source: ",
                 a("api.kawalcorona.com", href = "api.kawalcorona.com")),
               
               
               p(
                 "Created by",
                 a("Inayatus Sholikhah", href = "https://github.com/inytss")
               ),
               p(
                 "Inspired by",
                 a("Ahmad Husain Abdullah", href = "https://github.com/ahmadhusain")
               ),
               dateRangeInput(
                   "dateSelector",
                   label = "Observation Period",
                   start = ymd("2020-01-01"),
                   end = ymd(Sys.Date() + 1),
                   separator = "to"
                 ),
               
               dataTableOutput(outputId = "fulldata"),
               
               hr(),
               
               p(
                 "Developed using R shiny.",
                 a("Get the code!", href = "https://github.com/inytss")
               )
               
             ),
             
             mainPanel(
               
                 column(
                 width = 6,
                 selectInput(
                   inputId = "country",
                   label = "Select country",
                   choices = unique(ts_confirmed_long$country),
                   multiple = TRUE,
                   selected = c("Indonesia", "US", "United Kingdom")
                 )
               ),
               
               column(
                 width = 6,
                 
                 selectInput(
                   inputId = "option",
                   label = "Choose case",
                   selected = "confirmed",
                   choices = c("confirmed", "death", "recovered")
                   )
                 ),
                 
               
               br(),
               
               highchartOutput(outputId = "plot") %>% withSpinner(type = 4, color = "#3e3f3a", size = 0.5),
               
               hr(),
               
               highchartOutput(outputId = "map", height = "600px")
              )
  
             )
           ),
  
  
  # Vaccine  -----------
  
  tabPanel("Vaccines",

           sidebarLayout(
             sidebarPanel(
               width = 4,

               p(paste(
                 "Updated: ",
                 format(
                   x = Sys.time() %>% ymd_hms(),
                   format("%A, %d %B %Y %H:%M:%S")
                 )
               )),

               p("Source:",
                 a("https://ourworldindata.org/coronavirus", href = "https://ourworldindata.org/coronavirus"))
             ),

             mainPanel(

               highchartOutput(outputId = "type_vaccines"),

               br(),
               hr(),
               
               column(
                 width = 6,
                 
                 selectInput(
                   inputId = "country_name",
                   label = "Choose counrty",
                   selected = "Indonesia",
                   choices = choose_country
                 )
               ),
               br(),
               hr(),
               dataTableOutput(outputId = "vaccine_data")
             )
           )),
  
  # Vaccine Distribute -----------
  
  tabPanel("Vaccines Distribute",

           sidebarLayout(
             sidebarPanel(
               width = 4,

               p(paste(
                 "Updated: ",
                 format(
                   x = Sys.time() %>% ymd_hms(),
                   format("%A, %d %B %Y %H:%M:%S")
                 )
               )),

               p("Source:",
                 a("https://ourworldindata.org/coronavirus", href = "https://ourworldindata.org/coronavirus")),
               
               p("This is vaccines ditribute status around the world each day.")
             ),
             
             mainPanel(
               
               highchartOutput(outputId = "top10"),
               
               br(),
               hr(),
               
               column(
                 width = 8,
                 pickerInput(
                   inputId = "pick_country",
                   label = "Pick your country",
                   choices = vak_clean %>% distinct(country) %>% pull(country),
                   selected = c("Indonesia", "United States", "China", "Israel"),
                   multiple = TRUE,
                   options = pickerOptions(actionsBox = T, title = "Please select the country")
                 )
               ),
               br(),
               hr(),
               highchartOutput(outputId = "plot_vaccines") %>% withSpinner(type = 4, color = "#3e3f3a", size = 0.5),
             )
          )
)
)
))