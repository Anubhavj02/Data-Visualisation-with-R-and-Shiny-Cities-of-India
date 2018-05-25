library(shiny)



# Define server logic required to draw state.filter histogram
shinyServer(function(input, output, session) {
  observe({
    updateCheckboxGroupInput(
      session,
      'state.checkbox.filter',
      choiceNames = as.character(by_state_order$state_name),
      choiceValues = by_state_order$State_Code,
      selected = if (input$all.none)
        by_state_order$State_Code
    )
  })
  
  checkbox_state_filter <- reactive({
    state_group %>%
      filter(State_Code %in% input$state.checkbox.filter)
    
  })
  
  
  df3 <- reactive({
    state_group %>%
      filter(State_Code %in% input$state.checkbox.filter)
    
  })
  
  
  df1 <- reactive({
    state.filter <-
      subset(
        cities_dataset,
        select = c(state_code , name_of_city, state_name, population_total)
      ) %>%
      filter(state_code %in% input$state.checkbox.filter)
    state.filter$pathString <- paste("world",
                          state.filter$state_name,
                          state.filter$name_of_city,
                          sep = "/")
    population <- as.Node(state.filter)
    return(population)
    
  })
  
  df2 <- reactive({
    cities_dataset %>%
      filter(state_code %in% input$state.checkbox.filter)
    
  })
  
  
################################################################################
  # TAB1 graphs and outputs- Start
################################################################################
  
  # Leaflet Map Plot Generation
  output$tab1_leaflet_map <- renderLeaflet({
    
    # If All States option is selected in the dropdown
    if (input$tab1_dropdown_states == "All") {
      
      leaflet.map <- leaflet()  %>% addTiles()
      
      names(spllitted_cities) %>%
        purrr::walk(function(city.data.frame) {
          leaflet.map <<- leaflet.map %>%
            # Marker HTML layout to show on the popup
            addMarkers(
              data = spllitted_cities[[city.data.frame]],
              lng =  ~ Latitude,
              lat =  ~ Longitude,
              popup = paste(
                "<h4>",
                spllitted_cities[[city.data.frame]]$name_of_city,
                "</h4>",
                "<b>Population:</b>",
                spllitted_cities[[city.data.frame]]$population_total,
                "<br>",
                "<b>Population Male:</b>",
                spllitted_cities[[city.data.frame]]$population_male,
                "<br>",
                "<b>Population Female:</b>",
                spllitted_cities[[city.data.frame]]$population_female,
                "<br>",
                "<b>Total Literacy:</b>",
                spllitted_cities[[city.data.frame]]$literates_total,
                "<br>",
                "<b>Male Literacy:</b>",
                spllitted_cities[[city.data.frame]]$literates_male,
                "<br>",
                "<b>Female Literacy:</b>",
                spllitted_cities[[city.data.frame]]$literates_female,
                "<br>",
                "<b>Sex Ratio:</b>",
                spllitted_cities[[city.data.frame]]$sex_ratio
              ),
              group = city.data.frame,
              clusterOptions = markerClusterOptions(removeOutsideVisibleBounds = F),
              labelOptions = labelOptions(noHide = F,
                                          direction = 'auto')
            )
        })
      
      leaflet.map
      
    } else{
      cities_dataset_filtered <-
        filter(cities_dataset,
               cities_dataset$state_code == input$tab1_dropdown_states)
      leaflet.map <- leaflet()  %>% addTiles() %>%
        addMarkers(
          data = cities_dataset_filtered,
          lng =  ~ Latitude,
          lat =  ~ Longitude,
          popup = paste(
            "<h4>",
            cities_dataset_filtered$name_of_city,
            "</h4>",
            "<b>Population:</b>",
            cities_dataset_filtered$population_total,
            "<br>",
            "<b>Population Male:</b>",
            cities_dataset_filtered$population_male,
            "<br>",
            "<b>Population Female:</b>",
            cities_dataset_filtered$population_female,
            "<br>",
            "<b>Total Literacy:</b>",
            cities_dataset_filtered$literates_total,
            "<br>",
            "<b>Male Literacy:</b>",
            cities_dataset_filtered$literates_male,
            "<br>",
            "<b>Female Literacy:</b>",
            cities_dataset_filtered$literates_female,
            "<br>",
            "<b>Sex Ratio:</b>",
            cities_dataset_filtered$sex_ratio
          )
        )
      
      leaflet.map
      
    }
  })
  
  # Total Cities Value Box output generation
  output$tab1_valuebox_total_cities <- renderValueBox({
    if (input$tab1_dropdown_states == "All") {
      valueBox(
        length(cities_dataset$name_of_city),
        "Total Cities",
        color = "purple",
        width = 6
      )
    } else{
      cities_dataset_filtered <- filter(state_group, state_group$State_Code == input$tab1_dropdown_states)
      valueBox(cities_dataset_filtered$Total,
               "Total Cities",
               color = "purple",
               width = 6)
    }
    
  })
  
  # Total Popuation Value Box output generation
  output$tab1_valuebox_total_population <- renderValueBox({
    if (input$tab1_dropdown_states == "All") {
      valueBox(
        sum(cities_dataset$population_total),
        "Total Population",
        color = "orange",
        width = 6
      )
    } else{
      cities_dataset_filtered <- filter(state_group, state_group$State_Code == input$tab1_dropdown_states)
      valueBox(cities_dataset_filtered$Population,
               "Total Population",
               color = "orange",
               width = 6)
    }
  })
  
  # Total Male Population Percent Value Box output generation
  output$tab1_valuebox_male_pop_perc <- renderValueBox({
    if (input$tab1_dropdown_states == "All") {
      valueBox(round((
        sum(cities_dataset$population_male) / sum(cities_dataset$population_total)
      ) * 100, 2),
      "% Male Population",
      color = "green",
      width = 6)
    } else{
      cities_dataset_filtered <- filter(state_group, state_group$State_Code == input$tab1_dropdown_states)
      valueBox(
        round(cities_dataset_filtered$Male_Percent, 2),
        "% Male Population",
        color = "green",
        width = 6
      )
    }
  })
  
  # Total Female Population Percent Value Box output generation
  output$tab1_valuebox_female_pop_perc <- renderValueBox({
    if (input$tab1_dropdown_states == "All") {
      valueBox(
        round(
          sum(cities_dataset$population_female) / sum(cities_dataset$population_total) *
            100,
          2
        ),
        "% Female Population",
        color = "blue",
        width = 6
      )
    } else{
      cities_dataset_filtered <- filter(state_group, state_group$State_Code == input$tab1_dropdown_states)
      valueBox(
        round(cities_dataset_filtered$Female_Percent, 2),
        "% Female Population",
        color = "blue",
        width = 6
      )
    }
    
  })
  
  # Total Literacy Rate Value Box output generation
  output$tab1_valuebox_literacy_rate <- renderValueBox({
    if (input$tab1_dropdown_states == "All") {
      valueBox(round(
        sum(cities_dataset$literates_total) / sum(cities_dataset$population_total) *
          100,
        2
      ),
      "Literacy Rate",
      color = "red",
      width = 6)
    } else{
      cities_dataset_filtered <- filter(state_group, state_group$State_Code == input$tab1_dropdown_states)
      valueBox(
        round(cities_dataset_filtered$Literates_Percent, 2),
        "Literacy Rate",
        color = "red",
        width = 6
      )
    }
    
  })
  
  # Sex Ratio Value Box output generation
  output$tab1_valuebox_sex_ratio <- renderValueBox({
    if (input$tab1_dropdown_states == "All") {
      valueBox(round(
        sum(cities_dataset$sex_ratio) / length(cities_dataset$sex_ratio),
        2
      ),
      "Sex Ratio",
      color = "yellow",
      width = 6)
    } else{
      cities_dataset_filtered <- filter(state_group, state_group$State_Code == input$tab1_dropdown_states)
      valueBox(round(cities_dataset_filtered$Sex_Ratio, 2),
               "Sex Ratio",
               color = "yellow",
               width = 6)
    }
    
  })
  
  # Polar Plot output generation using Highcharter Library
  output$tab1_polar_plot <- renderHighchart({
    if (input$tab1_dropdown_states == "All") {
      hc <- highchart() %>%
        hc_chart(polar = TRUE)  %>%
        hc_xAxis(
          categories = c(
            "Male Population %",
            "Female Population %",
            "Literacy %",
            "Graduates %"
          ),
          tickmarkPlacement = "on",
          lineWidth = 0
        ) %>%
        hc_yAxis(
          gridLineInterpolation = "polygon",
          lineWidth = 0,
          min = 0
        ) %>%
        hc_series(
          list(
            name =  "All States",
            data = c(
              round((
                sum(cities_dataset$population_male) / sum(cities_dataset$population_total)
              ) * 100, 2),
              round((
                sum(cities_dataset$population_female) / sum(cities_dataset$population_total)
              ) * 100, 2),
              round(
                sum(cities_dataset$literates_total) / sum(cities_dataset$population_total) *
                  100,
                2
              ),
              round(
                sum(cities_dataset$total_graduates) / sum(cities_dataset$population_total) *
                  100,
                2
              )
            ),
            pointPlacement = "on",
            colorByPoint = TRUE,
            type = "column",
            colors = c("#F00", "#0F0", "#00F", "#F0F")
          )
        )
      
      hc
    } else{
      cities_dataset_filtered <- filter(state_group, state_group$State_Code == input$tab1_dropdown_states)
      hc <- highchart() %>%
        hc_chart(polar = TRUE)  %>%
        hc_xAxis(
          categories = c(
            "Male Population %",
            "Female Population %",
            "Literacy %",
            "Graduates %"
          ),
          tickmarkPlacement = "on",
          lineWidth = 0
        ) %>%
        hc_yAxis(
          gridLineInterpolation = "polygon",
          lineWidth = 0,
          min = 0
        ) %>%
        hc_series(
          list(
            name = cities_dataset_filtered$state_name,
            data = c(
              cities_dataset_filtered$Male_Percent,
              cities_dataset_filtered$Female_Percent,
              cities_dataset_filtered$Literates_Percent,
              cities_dataset_filtered$Grads_Percent
            ),
            pointPlacement = "on",
            colorByPoint = TRUE,
            type = "column",
            colors = c("#F00", "#0F0", "#00F", "#F0F")
          )
        )
      hc
    }
  })
  
################################################################################
  # TAB1 graphs and outputs- End
################################################################################
  
################################################################################
  # TAB2 graphs and outputs- Start
################################################################################
  
  output$tab2_bubble_map <- renderHighchart({
    if (input$map.type.filter == "all_view") {
      hc <-
        hcmap(
          "countries/in/custom/in-all-andaman-and-nicobar",
          data = cities_dataset.merge,
          value = input$attribute.filters,
          joinBy = c("hc-a2", "hc-a2"),
          name = input$attribute.filters,
          dataLabels = list(enabled = TRUE, format = "{point.name}"),
          borderColor = "#FAFAFA",
          borderWidth = 0.1,
          tooltip = list(valueDecimals = 2)
        ) %>% hc_colorAxis(minColor = "blue",
                           maxColor = "red",
                           stops = color_stops(n = 5))
      
      hc
      
    } else{
      if (input$attribute.filters == "Population") {
        top.cities <- cities_dataset %>% arrange(desc(population_total))
        top.cities.20 <- top.cities[1:20, ]
        sel_attr <- top.cities.20$population_total
      }
      else {
        if (input$attribute.filters == "Male_Percent") {
          top.cities <-
            cities_dataset %>% mutate(population_male_m = population_male / population_total) %>% arrange(desc(population_male_m))
          top.cities.20 <- top.cities[1:20, ]
          sel_attr <- top.cities.20$population_male_m
          
        } else{
          if (input$attribute.filters == "Female_Percent") {
            top.cities <-
              cities_dataset %>% mutate(population_female_m = population_female / population_total) %>% arrange(desc(population_female_m))
            top.cities.20 <- top.cities[1:20, ]
            sel_attr <- top.cities.20$population_female_m
            
          } else{
            if (input$attribute.filters == "Literates_Percent") {
              top.cities <-
                cities_dataset %>% mutate(literates_total_m = literates_total / population_total) %>% arrange(desc(literates_total_m))
              top.cities.20 <- top.cities[1:20, ]
              sel_attr <- top.cities.20$literates_total_m
            } else{
              if (input$attribute.filters == "Male_Literates_Percent") {
                top.cities <-
                  cities_dataset %>% mutate(literates_male_m = literates_male / population_total) %>% arrange(desc(literates_male_m))
                top.cities.20 <- top.cities[1:20, ]
                sel_attr <- top.cities.20$literates_male_m
              } else{
                if (input$attribute.filters == "Female_Literates_Percent") {
                  top.cities <-
                    cities_dataset %>% mutate(literates_female_m = literates_female / population_total) %>% arrange(desc(literates_female_m))
                  top.cities.20 <- top.cities[1:20, ]
                  sel_attr <- top.cities.20$literates_female_m
                } else{
                  if (input$attribute.filters == "Sex_Ratio") {
                    top.cities <- cities_dataset %>% mutate(desc(sex_ratio))
                    top.cities.20 <- top.cities[1:20, ]
                    sel_attr <- top.cities.20$sex_ratio
                  } else{
                    if (input$attribute.filters == "Grads_Percent") {
                      top.cities <-
                        cities_dataset %>% mutate(total_graduates_m = total_graduates / population_total) %>% arrange(desc(total_graduates_m))
                      top.cities.20 <- top.cities[1:20, ]
                      sel_attr <- top.cities.20$total_graduates_m
                    } else{
                      if (input$attribute.filters == "Male_Grads_Percent") {
                        top.cities <-
                          cities_dataset %>% mutate(male_graduates_m = male_graduates / population_total) %>% arrange(desc(male_graduates_m))
                        top.cities.20 <- top.cities[1:20, ]
                        sel_attr <- top.cities.20$male_graduates_m
                      } else{
                        if (input$attribute.filters == "Female_Grads_Percent") {
                          top.cities <-
                            cities_dataset %>% mutate(female_graduates_m = female_graduates / population_total) %>% arrange(desc(female_graduates_m))
                          top.cities.20 <- top.cities[1:20, ]
                          sel_attr <- top.cities.20$female_graduates_m
                        } else{
                          top.cities <- cities_dataset  %>% mutate(desc(population_total))
                          top.cities.20 <- top.cities[1:20, ]
                          sel_attr <- top.cities.20$population_total
                        }
                      }
                    }
                  }
                }
              }
            }
          }
        }
      }
      
      
      cities_20 <- data_frame(
        name = top.cities.20$name_of_city,
        lat = top.cities.20$Longitude,
        lon = top.cities.20$Latitude,
        z = sel_attr,
        color = colorize(z)
      )
      hcmap(
        "countries/in/custom/in-all-andaman-and-nicobar",
        showInLegend = FALSE,
        borderColor = "black",
        borderWidth = 1
      ) %>%
        hc_add_series(
          data = cities_20,
          type = "mapbubble",
          name = "Cities",
          maxSize = '10%',
          dataLabels = list(enabled = TRUE, format = '{point.name}'),
          showInLegend = FALSE
        )
    }
  })
  
################################################################################
  # TAB2 graphs and outputs- End
################################################################################
  
  
################################################################################
  # TAB3 graphs and outputs- Start
################################################################################
  
  output$cities_table <- DT::renderDataTable({
    action <- DT::dataTableAjax(session, cities_dataset)
    DT::datatable(
      cities_dataset,
      options = list(
        searching = T,
        pageLength = 15,
        scrollX = T
      ),
      escape = FALSE
    )
  })
  
################################################################################
  # TAB3 graphs and outputs- End
################################################################################
  
################################################################################
  # TAB4 graphs and outputs- Start
################################################################################
  
  output$tab4_column_chart1 <- renderHighchart({
    state.filter <- checkbox_state_filter()
    if(length(state.filter$state_name)>0){
      hc <-
        hchart(state.filter,
               "column",
               hcaes(x = state_name, y = Total, color = state_name))  %>%
        hc_add_theme(hc_theme_google()) %>%
        hc_tooltip(
          crosshairs = TRUE,
          backgroundColor = "#FCFFC5",
          shared = TRUE,
          borderWidth = 5,
          pointFormat = "<b>Total Population</b>: {point.y}"
        ) %>%
        hc_xAxis(title = list(text = "States"))
      
      hc
    }else{
      hc <- highchart()  %>%
        hc_chart(type = "column") %>%
        hc_plotOptions(series = list(stacking = "normal")) %>%
        hc_xAxis(title = list(text = "States"),
                 categories = state.filter$state_name) %>%
        hc_add_series(
          name = "Male Polulation",
          data = state.filter$Male_Population_Mean,
          color = "green"
        ) %>%
        hc_add_series(
          name = "Female Polulation",
          data = state.filter$Female_Population_Mean,
          color = "blue"
        )
      
      hc
    }
  })
  
  output$tab4_column_chart2 <- renderHighchart({
    state.filter <- checkbox_state_filter()
    hc <- highchart()  %>%
      hc_chart(type = "column") %>%
      hc_plotOptions(series = list(stacking = "normal")) %>%
      hc_xAxis(title = list(text = "States"),
               categories = state.filter$state_name) %>%
      hc_add_series(
        name = "Male Polulation",
        data = state.filter$Male_Population_Mean,
        color = "green"
      ) %>%
      hc_add_series(
        name = "Female Polulation",
        data = state.filter$Female_Population_Mean,
        color = "blue"
      )
    
    # Print highchart -----------------------------------------------
    hc
  })
  
  output$tab4_column_chart3 <- renderHighchart({
    state.filter <- checkbox_state_filter()
    hc <- highchart()  %>%
      hc_chart(type = "column") %>%
      hc_plotOptions(series = list(stacking = "normal")) %>%
      hc_xAxis(title = list(text = "States"),
               categories = state.filter$state_name) %>%
      hc_add_series(name = "Male Graduates",
                    data = state.filter$Male_Grads_Mean,
                    color = "orange") %>%
      hc_add_series(
        name = "Female Graduates",
        data = state.filter$Female_Grads_Mean,
        color = "red"
      )
    
    # Print highchart -----------------------------------------------
    hc
  })
  
  output$tab4_column_chart4 <- renderHighchart({
    state.filter <- checkbox_state_filter()
    hc <- highchart()  %>%
      hc_chart(type = "column") %>%
      hc_xAxis(title = list(text = "States"),
               categories = state.filter$state_name) %>%
      hc_add_series(name = "Sex Ratio",
                    data = state.filter$Sex_Ratio,
                    color = "green")
    
    # Print highchart -----------------------------------------------
    hc
  })
  
  
  output$tab4_scatter_chart1 <- renderHighchart({
    state.filter <- df2()
    if(length(state.filter$state_name)>0){
      hchart(
        state.filter,
        "scatter",
        hcaes(x = effective_literacy_rate_male, y = effective_literacy_rate_female, group = state_name)
      ) %>%
        hc_add_theme(hc_theme_monokai()) %>%
        hc_xAxis(title = list(text = "Effective Literacy Rate Male")) %>%
        hc_yAxis(title = list(text = "Effective Literacy Rate Female")) %>%
        hc_tooltip(
          crosshairs = TRUE,
          backgroundColor = "#FCFFC5",
          shared = TRUE,
          borderWidth = 5,
          pointFormat = "<h3>{point.label}</h3><br><b>Literacy Rate Male</b>: {point.x}<br><b>Literacy Rate Female</b>: {point.y}"
        )
    }else{
      hc <- highchart()  %>%
        hc_chart(type = "column") %>%
        hc_plotOptions(series = list(stacking = "normal")) %>%
        hc_xAxis(title = list(text = "States"),
                 categories = state.filter$state_name) %>%
        hc_add_series(
          name = "Male Polulation",
          data = state.filter$Male_Population_Mean,
          color = "green"
        ) %>%
        hc_add_series(
          name = "Female Polulation",
          data = state.filter$Female_Population_Mean,
          color = "blue"
        )
      
      # Print highchart -----------------------------------------------
      hc
    }
    
  })
  
  output$tab4_scatter_chart2 <- renderHighchart({
    state.filter <- df2()
    if(length(state.filter$state_name)>0){
      hchart(state.filter,
             "scatter",
             hcaes(x = male_graduates, y = female_graduates, group = state_name)) %>%
        hc_add_theme(hc_theme_monokai()) %>%
        hc_xAxis(title = list(text = "Male Graduates")) %>%
        hc_yAxis(title = list(text = "Female Graduates")) %>%
        hc_tooltip(
          crosshairs = TRUE,
          backgroundColor = "#FCFFC5",
          shared = TRUE,
          borderWidth = 5,
          pointFormat = "<h3>{point.label}</h3><br><b>Male Graduates</b>: {point.x}<br><b>Female Graduates</b>: {point.y}"
        )
    }else{
      hc <- highchart()  %>%
        hc_chart(type = "column") %>%
        hc_plotOptions(series = list(stacking = "normal")) %>%
        hc_xAxis(title = list(text = "States"),
                 categories = state.filter$state_name) %>%
        hc_add_series(
          name = "Male Polulation",
          data = state.filter$Male_Population_Mean,
          color = "green"
        ) %>%
        hc_add_series(
          name = "Female Polulation",
          data = state.filter$Female_Population_Mean,
          color = "blue"
        )
      
      # Print highchart -----------------------------------------------
      hc
    }
    
  })
  
  output$tab4_scatter_chart3 <- renderHighchart({
    state.filter <- df2()
    if(length(state.filter$state_name) >0){
      hchart(state.filter,
             "scatter",
             hcaes(x = sex_ratio, y = child_sex_ratio, group = state_name)) %>%
        hc_add_theme(hc_theme_monokai()) %>%
        hc_xAxis(title = list(text = "Sex Ratio")) %>%
        hc_yAxis(title = list(text = "Child Sex Ratio")) %>%
        hc_tooltip(
          crosshairs = TRUE,
          backgroundColor = "#FCFFC5",
          shared = TRUE,
          borderWidth = 5,
          pointFormat = "<h3>{point.label}</h3><br><b>Sex ratio</b>: {point.x}<br><b>Child Sex Ratio</b>: {point.y}"
        )
    }else{
      hc <- highchart()  %>%
        hc_chart(type = "column") %>%
        hc_plotOptions(series = list(stacking = "normal")) %>%
        hc_xAxis(title = list(text = "States"),
                 categories = state.filter$state_name) %>%
        hc_add_series(
          name = "Male Polulation",
          data = state.filter$Male_Population_Mean,
          color = "green"
        ) %>%
        hc_add_series(
          name = "Female Polulation",
          data = state.filter$Female_Population_Mean,
          color = "blue"
        )
      
      # Print highchart -----------------------------------------------
      hc
    }
    
  })
  
  output$tab4_scatter_chart4 <- renderHighchart({
    state.filter <- df2()
    if(length(state.filter$state_name)>0){
      hchart(state.filter,
             "scatter",
             hcaes(x = population_male, y = population_female, group = state_name)) %>%
        hc_add_theme(hc_theme_monokai()) %>%
        hc_xAxis(title = list(text = "Male Population")) %>%
        hc_yAxis(title = list(text = "Female Population")) %>%
        hc_tooltip(
          crosshairs = TRUE,
          backgroundColor = "#FCFFC5",
          shared = TRUE,
          borderWidth = 5,
          pointFormat = "<h3>{point.label}</h3><br><b>Male Population</b>: {point.x}<br><b>Female Population</b>: {point.y}"
        )
    }else{
      hc <- highchart()  %>%
        hc_chart(type = "column") %>%
        hc_plotOptions(series = list(stacking = "normal")) %>%
        hc_xAxis(title = list(text = "States"),
                 categories = state.filter$state_name) %>%
        hc_add_series(
          name = "Male Polulation",
          data = state.filter$Male_Population_Mean,
          color = "green"
        ) %>%
        hc_add_series(
          name = "Female Polulation",
          data = state.filter$Female_Population_Mean,
          color = "blue"
        )
      
      # Print highchart -----------------------------------------------
      hc
    }
    
  })
  
  output$tab4_pie_chart1 <- renderHighchart({
    state.filter <- checkbox_state_filter()
    highchart() %>%
      hc_chart(type = "pie",
               options3d = list(
                 enabled = TRUE,
                 alpha = 70,
                 beta = 0
               )) %>%
      hc_plotOptions(pie = list(depth = 70)) %>%
      hc_add_series_labels_values(state.filter$state_name, state.filter$Population) %>%
      hc_tooltip(pointFormat = "<b>Total Population</b>: {point.y}")
    
  })
  
  output$tab4_pie_chart2 <- renderHighchart({
    state.filter <- checkbox_state_filter()
    highchart() %>%
      hc_chart(type = "pie",
               options3d = list(
                 enabled = TRUE,
                 alpha = 70,
                 beta = 0
               )) %>%
      hc_plotOptions(pie = list(depth = 70)) %>%
      hc_add_series_labels_values(state.filter$state_name, state.filter$Graduates) %>%
      hc_tooltip(pointFormat = "<b>Total Graduates</b>: {point.y}")
    
  })
  
  output$tab4_pie_chart3 <- renderHighchart({
    state.filter <- checkbox_state_filter()
    highchart() %>%
      hc_chart(type = "pie",
               options3d = list(
                 enabled = TRUE,
                 alpha = 70,
                 beta = 0
               )) %>%
      hc_plotOptions(pie = list(depth = 70)) %>%
      hc_add_series_labels_values(state.filter$state_name, state.filter$Literates) %>%
      hc_tooltip(pointFormat = "<b>Total Literates</b>: {point.y}")
    
  })
  
  output$tab4_pie_chart4 <- renderHighchart({
    state.filter <- checkbox_state_filter()
    highchart() %>%
      hc_chart(type = "pie",
               options3d = list(
                 enabled = TRUE,
                 alpha = 70,
                 beta = 0
               )) %>%
      hc_plotOptions(pie = list(depth = 70)) %>%
      hc_add_series_labels_values(state.filter$state_name, state.filter$Sex_Ratio) %>%
      hc_tooltip(pointFormat = "<b>Sex Ratio</b>: {point.y}")
    
  })
  
  output$tab4_heatmap_chart1 <- renderHighchart({
    state.filter <- df3()
    if(length(state.filter$state_name)>0){
      hchart(state.filter,
             type = "treemap",
             hcaes(
               x = state_name,
               value = Population,
               color = Population
             ))  %>% hc_add_theme(hc_theme_538()) %>%
        hc_colorAxis(minColor = "#FF0000", maxColor = "#008000")
    }else{
      hc <- highchart()  %>%
        hc_chart(type = "column") %>%
        hc_plotOptions(series = list(stacking = "normal")) %>%
        hc_xAxis(title = list(text = "States"),
                 categories = state.filter$state_name) %>%
        hc_add_series(
          name = "Male Polulation",
          data = state.filter$Male_Population_Mean,
          color = "green"
        ) %>%
        hc_add_series(
          name = "Female Polulation",
          data = state.filter$Female_Population_Mean,
          color = "blue"
        )
      
      # Print highchart -----------------------------------------------
      hc
    }
  })
  
  output$tab4_heatmap_chart2 <- renderHighchart({
    state.filter <- df3()
    if(length(state.filter$state_name)>0){
      hchart(state.filter,
             type = "treemap",
             hcaes(
               x = state_name,
               value = Literates_Percent,
               color = Literates_Percent
             ))  %>% hc_add_theme(hc_theme_538()) %>%
        hc_colorAxis(minColor = "#FFFF00", maxColor = "#FF0000")
    }else{
      hc <- highchart()  %>%
        hc_chart(type = "column") %>%
        hc_plotOptions(series = list(stacking = "normal")) %>%
        hc_xAxis(title = list(text = "States"),
                 categories = state.filter$state_name) %>%
        hc_add_series(
          name = "Male Polulation",
          data = state.filter$Male_Population_Mean,
          color = "green"
        ) %>%
        hc_add_series(
          name = "Female Polulation",
          data = state.filter$Female_Population_Mean,
          color = "blue"
        )
      
      # Print highchart -----------------------------------------------
      hc
    }
  })
  
  output$tab4_heatmap_chart3 <- renderHighchart({
    state.filter <- df3()
    if(length(state.filter$state_name)>0){
      hchart(state.filter,
             type = "treemap",
             hcaes(
               x = state_name,
               value = Grads_Percent,
               color = Grads_Percent
             ))  %>% hc_add_theme(hc_theme_538()) %>%
        hc_colorAxis(minColor = "#0000FF", maxColor = "#008000")
    }else{
      hc <- highchart()  %>%
        hc_chart(type = "column") %>%
        hc_plotOptions(series = list(stacking = "normal")) %>%
        hc_xAxis(title = list(text = "States"),
                 categories = state.filter$state_name) %>%
        hc_add_series(
          name = "Male Polulation",
          data = state.filter$Male_Population_Mean,
          color = "green"
        ) %>%
        hc_add_series(
          name = "Female Polulation",
          data = state.filter$Female_Population_Mean,
          color = "blue"
        )
      
      # Print highchart -----------------------------------------------
      hc
    }
  })
  
  output$tab4_heatmap_chart4 <- renderHighchart({
    state.filter <- df3()
    if(length(state.filter$state_name)>0){
      hchart(state.filter,
             type = "treemap",
             hcaes(
               x = state_name,
               value = Sex_Ratio,
               color = Sex_Ratio
             ))  %>% hc_add_theme(hc_theme_538()) %>%
        hc_colorAxis(minColor = "#FFFF00", maxColor = "#008000")
    }else{
      hc <- highchart()  %>%
        hc_chart(type = "column") %>%
        hc_plotOptions(series = list(stacking = "normal")) %>%
        hc_xAxis(title = list(text = "States"),
                 categories = state.filter$state_name) %>%
        hc_add_series(
          name = "Male Polulation",
          data = state.filter$Male_Population_Mean,
          color = "green"
        ) %>%
        hc_add_series(
          name = "Female Polulation",
          data = state.filter$Female_Population_Mean,
          color = "blue"
        )
      
      # Print highchart -----------------------------------------------
      hc
    }
  })
  
  output$tab4_3d_chart1 <- renderPlotly({
    state.filter <- checkbox_state_filter()
    
    color <- colorize(length(state.filter$state_name))
    plot_ly(
      state.filter,
      x = ~ Population,
      y = ~ Male_Population_Mean,
      z = ~ Female_Population_Mean,
      color = ~ state_name
    ) %>%
      add_markers() %>%
      layout(scene = list(
        xaxis = list(title = 'Total Population'),
        yaxis = list(title = 'Male Popualtion'),
        zaxis = list(title = 'Female Population')
      ))
  })
  
  output$tab4_3d_chart2 <- renderPlotly({
    state.filter <- checkbox_state_filter()
    
    color <- colorize(length(state.filter$state_name))
    plot_ly(
      state.filter,
      x = ~ Graduates,
      y = ~ Male_Grads_Mean,
      z = ~ Female_Grads_Mean,
      color = ~ state_name
    ) %>%
      add_markers() %>%
      layout(scene = list(
        xaxis = list(title = 'Graduates'),
        yaxis = list(title = 'Male Grads'),
        zaxis = list(title = 'Female Grads')
      ))
  })
  
  output$tab4_line_chart1 <- renderHighchart({
    state.filter <- checkbox_state_filter()
    state.filter <- state.filter[order(state.filter$state_name), ]
    highchart() %>%
      hc_xAxis(categories = state.filter$state_name) %>%
      hc_add_series(name = "Total Population",
                    data = state.filter$Population,
                    color = "green") %>%
      hc_add_series(name = "Male Population",
                    data = state.filter$Male_Population,
                    color = "red") %>%
      hc_add_series(
        name = "Female Population",
        data = state.filter$Female_Population,
        color = "blue"
      )
  })
  
  output$tab4_line_chart2 <- renderHighchart({
    state.filter <- checkbox_state_filter()
    state.filter <- state.filter[order(state.filter$state_name), ]
    highchart()  %>%
      hc_xAxis(categories = state.filter$state_name) %>%
      hc_add_series(name = "Total Literates",
                    data = state.filter$Literates,
                    color = "green") %>%
      hc_add_series(name = "Male Literates",
                    data = state.filter$Male_Literates,
                    color = "red") %>%
      hc_add_series(
        name = "Female Literates",
        data = state.filter$Female_Literates,
        color = "blue"
      )
  })
  
  output$tab4_line_chart3 <- renderHighchart({
    state.filter <- checkbox_state_filter()
    state.filter <- state.filter[order(state.filter$state_name), ]
    highchart()  %>%
      hc_xAxis(categories = state.filter$state_name) %>%
      hc_add_series(name = "Total Graduates",
                    data = state.filter$Graduates,
                    color = "green") %>%
      hc_add_series(name = "Male Grads",
                    data = state.filter$Male_Grads,
                    color = "red") %>%
      hc_add_series(name = "Femlae Grads",
                    data = state.filter$Female_Grads,
                    color = "blue")
  })
  
  output$tab4_line_chart4 <- renderHighchart({
    state.filter <- checkbox_state_filter()
    state.filter <- state.filter[order(state.filter$state_name), ]
    highchart()  %>%
      hc_xAxis(categories = state.filter$state_name) %>%
      hc_add_series(name = "Sex Ratio",
                    data = state.filter$Sex_Ratio,
                    color = "green") %>%
      hc_add_series(name = "Child Sex Ratio",
                    data = state.filter$Child_Sex_Ratio,
                    color = "red")
  })
  
  output$tab4_box_plot1 <- renderHighchart({
    state.filter <- df2()
    if(length(state.filter$state_name)>0){
      hcboxplot(
        x = state.filter$population_total,
        var = state.filter$state_name,
        name = "Population",
        color = "#2980b9",
        outliers = FALSE
      )
    }else{
      hc <- highchart()  %>%
        hc_chart(type = "column") %>%
        hc_plotOptions(series = list(stacking = "normal")) %>%
        hc_xAxis(title = list(text = "States"),
                 categories = state.filter$state_name) %>%
        hc_add_series(
          name = "Male Polulation",
          data = state.filter$Male_Population_Mean,
          color = "green"
        ) %>%
        hc_add_series(
          name = "Female Polulation",
          data = state.filter$Female_Population_Mean,
          color = "blue"
        )
      
      # Print highchart -----------------------------------------------
      hc
    }
  })
  
  output$tab4_box_plot2 <- renderHighchart({
    state.filter <- df2()
    if(length(state.filter$state_name)>0){
      hcboxplot(
        x = state.filter$literates_total,
        var = state.filter$state_name,
        name = "Total Literates",
        color = "red",
        outliers = FALSE
      )
    }else{
      hc <- highchart()  %>%
        hc_chart(type = "column") %>%
        hc_plotOptions(series = list(stacking = "normal")) %>%
        hc_xAxis(title = list(text = "States"),
                 categories = state.filter$state_name) %>%
        hc_add_series(
          name = "Male Polulation",
          data = state.filter$Male_Population_Mean,
          color = "green"
        ) %>%
        hc_add_series(
          name = "Female Polulation",
          data = state.filter$Female_Population_Mean,
          color = "blue"
        )
      
      # Print highchart -----------------------------------------------
      hc
    }
  })
  
  output$tab4_box_plot3 <- renderHighchart({
    state.filter <- df2()
    if(length(state.filter$state_name)>0){
      hcboxplot(
        x = state.filter$total_graduates,
        var = state.filter$state_name,
        name = "Total Graduates",
        color = "green",
        outliers = FALSE
      )
    }else{
      hc <- highchart()  %>%
        hc_chart(type = "column") %>%
        hc_plotOptions(series = list(stacking = "normal")) %>%
        hc_xAxis(title = list(text = "States"),
                 categories = state.filter$state_name) %>%
        hc_add_series(
          name = "Male Polulation",
          data = state.filter$Male_Population_Mean,
          color = "green"
        ) %>%
        hc_add_series(
          name = "Female Polulation",
          data = state.filter$Female_Population_Mean,
          color = "blue"
        )
      
      # Print highchart -----------------------------------------------
      hc
    }
  })
  
  output$tab4_box_plot4 <- renderHighchart({
    state.filter <- df2()
    if(length(state.filter$state_name)>0){
      hcboxplot(
        x = state.filter$sex_ratio,
        var = state.filter$state_name,
        name = "Sex Ratio",
        color = "orange",
        outliers = FALSE
      )
    }else{
      hc <- highchart()  %>%
        hc_chart(type = "column") %>%
        hc_plotOptions(series = list(stacking = "normal")) %>%
        hc_xAxis(title = list(text = "States"),
                 categories = state.filter$state_name) %>%
        hc_add_series(
          name = "Male Polulation",
          data = state.filter$Male_Population_Mean,
          color = "green"
        ) %>%
        hc_add_series(
          name = "Female Polulation",
          data = state.filter$Female_Population_Mean,
          color = "blue"
        )
      
      # Print highchart -----------------------------------------------
      hc
    }
  })

################################################################################
  # TAB4 graphs and outputs- End
################################################################################
})
