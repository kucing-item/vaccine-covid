shinyServer(function(input, output) {
  sever()
  
  shinyInput <- function(FUN, len, id, ...) {
    inputs <- character(len)
    for (i in seq_len(len)) {
      inputs[i] <- as.character(FUN(paste0(id, i), ...))
    }
    inputs
  }

  
  output$plot <- renderHighchart({
    if (input$option == "death") {
      ts_deaths_long %>%
        mutate(datetime = ymd(datetime)) %>%
        filter(country %in% input$country) %>%
        group_by(country, datetime) %>%
        summarise(deaths = sum(deaths)) %>%
        filter(datetime >= input$dateSelector[1] &
                 datetime < input$dateSelector[2]) %>%
        
        hchart(.,
               type = "line",
               hcaes(x = datetime,
                     y = deaths,
                     group = country)) %>%
        hc_title(text = "The Total Number of Deaths") %>%
        hc_subtitle(text = "The data is compiled by the Johns Hopkins University Center") %>%
        hc_plotOptions(line = list(
          lineWidth = 4,
          allowPointSelect = TRUE,
          marker = list(
            enabled = FALSE,
            radius = 1,
            symbol = "circle"
          )
        )) %>%
        hc_add_theme(hc_theme_538()) %>%
        hc_exporting(enabled = TRUE)
    }
    
    else if (input$option == "recovered") {
      ts_recovered_long %>%
        mutate(datetime = ymd(datetime)) %>%
        filter(country %in% input$country) %>%
        group_by(country, datetime) %>%
        summarise(recovered = sum(recovered)) %>%
        filter(datetime >= input$dateSelector[1] &
                 datetime < input$dateSelector[2]) %>%
        hchart(.,
               type = "line",
               hcaes(x = datetime,
                     y = recovered,
                     group = country)) %>%
        hc_title(text = "The Total Number of Recovered") %>%
        hc_subtitle(text = "The data is compiled by the Johns Hopkins University Center ") %>%
        hc_plotOptions(line = list(
          lineWidth = 4,
          allowPointSelect = TRUE,
          marker = list(
            enabled = FALSE,
            radius = 1,
            symbol = "circle"
          )
        )) %>%
        hc_add_theme(hc_theme_538()) %>%
        hc_exporting(enabled = TRUE)
      
    }
    
    else {
      ts_confirmed_long %>%
        mutate(datetime = ymd(datetime)) %>%
        filter(country %in% input$country) %>%
        group_by(country, datetime) %>%
        summarise(confirmed = sum(confirmed)) %>%
        filter(datetime >= input$dateSelector[1] &
                 datetime < input$dateSelector[2]) %>%
        hchart(.,
               type = "line",
               hcaes(x = datetime,
                     y = confirmed,
                     group = country)) %>%
        hc_title(text = "The Total Number of Confirmed Cases") %>%
        hc_subtitle(text = "The data is compiled by the Johns Hopkins University Center ") %>%
        hc_plotOptions(line = list(
          lineWidth = 4,
          allowPointSelect = TRUE,
          marker = list(
            enabled = FALSE,
            radius = 1,
            symbol = "circle"
          )
        )) %>%
        hc_add_theme(hc_theme_538()) %>%
        hc_exporting(enabled = TRUE)
      
    }
    
    
  })
  
  output$fulldata <- renderDataTable({
    dat_covid %>%
      mutate_if(is.numeric, ~ comma(., digits = 0)) %>%
      arrange(desc(Confirmed)) %>%
      head(20) %>% 
      datatable(
        caption = 'Table 1: Top 20 Confirmed Cases, Deaths, Recovered by Country',
        options = list(
          dom = "ft",
          initComplete = JS(
            "function(settings, json) {",
            "$(this.api().table().header()).css({'background-color': '#000', 'color': '#fff'});",
            "}"
          ),
          scrollX = TRUE,
          scrollY = "850px",
          pageLength = 185
        ),
        rownames = T
      ) %>%
      formatStyle(
        names(dat_covid),
        backgroundColor = "#3e3f3a",
        background = "#3e3f3a",
        target = "row",
        color = "white",
        fontSize = "90%"
      ) %>%
      formatCurrency(
        mark = ",",
        columns = 2:4,
        interval = 3,
        currency = "",
        digits = 0
      )
    
  })
  
  output$map <- renderHighchart({
    hcmap(
      map = "custom/world.js",
      data = df_map,
      value = "Confirmed",
      joinBy = c("iso-a3"),
      name = "Total Cases",
      dataLabels = list(enabled = TRUE, format = '{point.name}'),
      borderColor = "black",
      borderWidth = 0.1,
      tooltip = list(valueDecimals = 0)
    ) %>%
      hc_add_theme(hc_theme_538()) %>%
      hc_colorAxis(minColor = "#C5C889",
                   maxColor = "#434348",
                   type = "logarithmic") %>%
      hc_exporting(enabled = TRUE) %>%
      hc_title(text = "Mapping Cumulative Confirmed Cases by Country") %>%
      
      hc_subtitle(text = paste("Updated:", Sys.time())) %>%
      hc_mapNavigation(enabled = TRUE)
  })
  
  output$type_vaccines <- renderHighchart({
    
    vaksin_per_country <- vak_clean %>% 
      distinct(country, vaccines)
    
    vac <- unlist(strsplit(vaksin_per_country$vaccines, ", "))
    as.data.frame(vac) %>% 
      count(vac) %>% 
      arrange(desc(n)) %>% 
      hchart(.,
             type = "column",
             hcaes(x = vac,
                   y = n,
                   color = n)) %>%
      hc_title(text = "Vaccine Type Used Around The World") %>%
      hc_add_theme(hc_theme_538()) %>%
      hc_exporting(enabled = TRUE)
  })
  
  
  output$vaccine_data <- renderDataTable({
    vak_clean %>%
      filter(country == input$country_name) %>%
      datatable(
        caption = paste('Vaccination Dataset in', input$country_name),
        options = list(
          dom = "ft",
          initComplete = JS(
            "function(settings, json) {",
            "$(this.api().table().header()).css({'background-color': '#000', 'color': '#fff'});",
            "}"
          ),
          scrollX = TRUE,
          scrollY = "850px",
          pageLength = 185
        ),
        rownames = T
      ) %>%
      formatStyle(
        names(vak_clean),
        backgroundColor = "#3e3f3a",
        background = "#3e3f3a",
        target = "row",
        color = "white",
        fontSize = "90%"
      ) %>%
      formatCurrency(
        mark = ",",
        columns = 2:4,
        interval = 3,
        currency = "",
        digits = 0
      )
    
  })
  
  output$top10 <- renderHighchart({
    vak_clean %>% 
      filter(!country %in% c("World", "Asia", "Upper middle income", "High income", "Lower middle income", "European Union")) %>% 
      group_by(country) %>% 
      summarise(total = sum(daily_vaccinations, na.rm=TRUE)) %>% 
      arrange(desc(total)) %>% 
      head(10) %>% 
      hchart(.,
             type = "column",
             hcaes(x = country,
                   y = total,
                   color = country)) %>%
      hc_title(text = "Top 10 Country Vaccinated") %>%
      hc_add_theme(hc_theme_538()) %>%
      hc_exporting(enabled = TRUE)
  })
  
  output$plot_vaccines <- renderHighchart({
        
    hchart(vak_clean %>% filter(country %in% input$pick_country),
               type = "line",
               hcaes(x = date,
                     y = daily_vaccinations,
                     group = country)) %>%
        hc_title(text = "Daily Vaccines Distribute") %>%
        # hc_subtitle(text = "The data is compiled by the Johns Hopkins University Center ") %>%
        hc_plotOptions(line = list(
          lineWidth = 4,
          allowPointSelect = TRUE,
          marker = list(
            enabled = FALSE,
            radius = 1,
            symbol = "circle"
          )
        )) %>%
        hc_add_theme(hc_theme_538()) %>%
        hc_exporting(enabled = TRUE)
    
    
  })
  
})