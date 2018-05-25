###############################################################################################
# UI Section of the Application
###############################################################################################

# Uniques State names
states.names <- unique(as.character(cities_dataset$state_name))

# Converting state codes to factores
cities_dataset$state_code <- as.factor(cities_dataset$state_code)

# Finding unique state codes
states.codes <- unique(cities_dataset$state_code)

# Options Names to display values on Map
map.view.options.names <-
  c(
    "Total Population",
    "Male Population",
    "Female Population",
    "Total Literates",
    "Male Literates",
    "Female Literates",
    "Sex Ratio",
    "Total Graduates",
    "Male Graduates",
    "Female Graduates"
  )

# Options Values to display values on Map
map.view.options.values <-
  c(
    "Population",
    "Male_Percent",
    "Female_Percent",
    "Literates_Percent",
    "Male_Literates_Percent",
    "Female_Literates_Percent",
    "Sex_Ratio",
    "Grads_Percent",
    "Male_Grads_Percent",
    "Female_Grads_Percent"
  )

# Options Icons to display values on Map
map.view.options.icons <- c(
  "icon1.svg",
  "icon2.svg",
  "icon3.png",
  "icon4.png",
  "icon5.jpeg",
  "icon6.jpeg",
  "icon7.jpeg",
  "icon8.png",
  "icon9.jpeg",
  "icon10.jpeg"
)

# Radio Button names to switch between Map and Chorepleth
top.states.names <- c("All States View",
                      "Top States")

# Radio Button values to switch between Map and Chorepleth
top.states.values <- c("all_view", "top_state")

# Radio Button icons to switch between Map and Chorepleth
top.states.icons <- c("icon12.png",
                      "icon11.jpg")


navbarPage(
  "Cities Of India",
  id = "nav",
  theme = shinytheme("flatly"),
  
  # Tab 1- Interactive Map
  tabPanel(
    "Interactive Spatial map",
    div(
      class = "outer",
      
      tags$head(# Include the custom CSS
        includeCSS("styles.css"),
        includeScript("gomap.js")),
      
      # Leaflet Library Map
      leafletOutput("tab1_leaflet_map", width = "100%", height = "100%"),
      
      # Right Filter Panel for dropdown and the graph
      absolutePanel(
        id = "controls",
        class = "panel panel-default",
        fixed = TRUE,
        draggable = TRUE,
        top = 80,
        left = "auto",
        right = 20,
        bottom = "auto",
        width = 490,
        height = 870,
        
        # Dropdown Header
        h2("Choose State"),
        
        # Initializing the dropdown
        selectInput(
          "tab1_dropdown_states",
          NULL,
          c(
            "ALL STATES" = "All",
            structure(
              by_state_order$State_Code,
              names = as.character(by_state_order$state_name)
            )
          )
        ),
        
        # Initializing all the value boxes
        valueBoxOutput("tab1_valuebox_total_cities", width = 6),
        valueBoxOutput("tab1_valuebox_total_population", width = 6),
        valueBoxOutput("tab1_valuebox_male_pop_perc", width = 6),
        valueBoxOutput("tab1_valuebox_female_pop_perc", width = 6),
        valueBoxOutput("tab1_valuebox_literacy_rate", width = 6),
        valueBoxOutput("tab1_valuebox_sex_ratio", width = 6),
        
        # Initializing the polar chart/ spider chart
        highchartOutput("tab1_polar_plot", height = 400)
      )
    )
  ),
  
  # Tab 2- Choropleth and Bubble Map
  tabPanel(
    "India Mapper",
    dashboardPage(
      dashboardHeader(disable = T),
      dashboardSidebar(disable = T),
      dashboardBody(
        tags$script(
          'window.onload = function() {
          function fixBodyHeight() {
          var el = $(document.getElementsByClassName("content-wrapper")[0]);
          var h = el.height();
          el.css("min-height", h + 50 + "px");
          };
          window.addEventListener("resize", fixBodyHeight);
          fixBodyHeight();
          };'
),
fluidRow(
  box(
    # Radio button to switch between two Map Types
    radioButtons(
      "map.type.filter",
      "",
      choiceNames = mapply(
        top.states.names,
        top.states.icons,
        FUN = function(state, iconUrl) {
          tagList(tags$img(
            src = iconUrl,
            width = 35,
            height = 35
          ),
          state)
        },
        SIMPLIFY = FALSE,
        USE.NAMES = FALSE
      ),
      choiceValues = top.states.values
    ),
    width = 2
  ),
  
  column(width = 1),
  
  # Initializing the Highcharter Map
  column(highchartOutput("tab2_bubble_map", height = "800px"),
         width = 6),
  
  # Right Panel different attribute filters
  box(
    radioButtons(
      "attribute.filters",
      "",
      choiceNames = mapply(
        map.view.options.names,
        map.view.options.icons,
        FUN = function(country, flagUrl) {
          tagList(tags$img(
            src = flagUrl,
            width = 70,
            height = 65
          ),
          country)
        },
        SIMPLIFY = FALSE,
        USE.NAMES = FALSE
      ),
      choiceValues = map.view.options.values
    ),
    width = 3
  )
)
        )
    )
),

# Tab 3- Data Table 
tabPanel(
  "Data explorer",
  box(
    title = "Cities Dataset",
    width = 12,
    status = "primary",
    height = "850",
    solidHeader = T,
    DT::dataTableOutput("cities_table", height = 800)
  )
),

# Tab 4- Different type of plots
tabPanel(
  "Plots",
  tags$head(tags$style(HTML(
    "
    div#checkGroup {
    font-size: 75%;
    }
    "
  ))),
  sidebarLayout(
    
    # Left Sidbar State Filter Panel
    sidebarPanel(
      h4("Select States"),
      checkboxInput('all.none', 'All/None', value = F),
      checkboxGroupInput(
        "state.checkbox.filter",
        label = NULL,
        choiceNames = as.character(by_state_order$state_name),
        choiceValues = by_state_order$State_Code,
        selected = c(35, 28, 22, 34, 3, 8, 19, 21, 20)
      )
      ,
      width = 2
    ),
    
    mainPanel(box(
      tabsetPanel(
        position = "below",
        tabPanel(
          "Bar Plots",
          fluidRow(column(
            6,
            box(
              title = "Frequency Plot of Number of cities in the states",
              status = "primary",
              solidHeader = TRUE,
              highchartOutput("tab4_column_chart1", height = "320px")
              ,
              width = 12
            )
          ),
          column(
            6,
            box(
              title = "Mean Population of Males and Females in the state",
              status = "primary",
              solidHeader = TRUE,
              highchartOutput("tab4_column_chart2", height = "320px"),
              width = 12
            )
          )),
          fluidRow(column(
            6,
            box(
              title = "Mean No. of Graduates of Males and Females in the state",
              status = "primary",
              solidHeader = TRUE,
              highchartOutput("tab4_column_chart3", height = 320)
              ,
              width = 12
            )
          ),
          column(
            6,
            box(
              title = "Mean Sex Ratio Plot",
              status = "primary",
              solidHeader = TRUE,
              highchartOutput("tab4_column_chart4", height = 320),
              width = 12
            )
          ))
        ),
        tabPanel(
          "Scatter Plots",
          fluidRow(column(
            6,
            box(
              title = "Male vs Female Literacy Rate",
              status = "primary",
              solidHeader = TRUE,
              highchartOutput("tab4_scatter_chart1", height = "320px")
              ,
              width = 12
            )
          ),
          column(
            6,
            box(
              title = "Male vs Female Graduate",
              status = "primary",
              solidHeader = TRUE,
              highchartOutput("tab4_scatter_chart2", height = "320px"),
              width = 12
            )
          )),
          fluidRow(column(
            6,
            box(
              title = "Sex vs Child Ratio",
              status = "primary",
              solidHeader = TRUE,
              highchartOutput("tab4_scatter_chart3", height = 320)
              ,
              width = 12
            )
          ),
          column(
            6,
            box(
              title = "Male vs Female Population",
              status = "primary",
              solidHeader = TRUE,
              highchartOutput("tab4_scatter_chart4", height = 320),
              width = 12
            )
          ))
        ),
        tabPanel(
          "Pie Charts",
          fluidRow(column(
            6,
            box(
              title = "Population Pie Chart",
              status = "primary",
              solidHeader = TRUE,
              highchartOutput("tab4_pie_chart1", height = "320px")
              ,
              width = 12
            )
          ),
          column(
            6,
            box(
              title = "Total Graduates Pie Chart",
              status = "primary",
              solidHeader = TRUE,
              highchartOutput("tab4_pie_chart2", height = "320px"),
              width = 12
            )
          )),
          fluidRow(column(
            6,
            box(
              title = "Total Literates Pie Chart",
              status = "primary",
              solidHeader = TRUE,
              highchartOutput("tab4_pie_chart3", height = 320)
              ,
              width = 12
            )
          ),
          column(
            6,
            box(
              title = "Sex Ratio Pie Chart",
              status = "primary",
              solidHeader = TRUE,
              highchartOutput("tab4_pie_chart4", height = 320),
              width = 12
            )
          ))
        ),
        tabPanel(
          "Heat Map",
          fluidRow(column(
            6,
            box(
              title = "Population HeatMap",
              status = "primary",
              solidHeader = TRUE,
              highchartOutput("tab4_heatmap_chart1", height = "320px")
              ,
              width = 12
            )
          ),
          column(
            6,
            box(
              title = "Total Literates HeatMap",
              status = "primary",
              solidHeader = TRUE,
              highchartOutput("tab4_heatmap_chart2", height = "320px"),
              width = 12
            )
          )),
          fluidRow(column(
            6,
            box(
              title = "Total Grads HeatMap",
              status = "primary",
              solidHeader = TRUE,
              highchartOutput("tab4_heatmap_chart3", height = 320)
              ,
              width = 12
            )
          ),
          column(
            6,
            box(
              title = "Sex Ratio HeatMap",
              status = "primary",
              solidHeader = TRUE,
              highchartOutput("tab4_heatmap_chart4", height = 320),
              width = 12
            )
          ))
        ),
        tabPanel(
          "Line Graphs",
          fluidRow(column(
            6,
            box(
              title = "Total vs Male vs Female Population",
              status = "primary",
              solidHeader = TRUE,
              highchartOutput("tab4_line_chart1", height = "320px")
              ,
              width = 12
            )
          ),
          column(
            6,
            box(
              title = "Total vs Male vs Female Literates",
              status = "primary",
              solidHeader = TRUE,
              highchartOutput("tab4_line_chart2", height = "320px"),
              width = 12
            )
          )),
          fluidRow(column(
            6,
            box(
              title = "Total vs Male vs Female Graduates",
              status = "primary",
              solidHeader = TRUE,
              highchartOutput("tab4_line_chart3", height = 320)
              ,
              width = 12
            )
          ),
          column(
            6,
            box(
              title = "Sex Ratio vs Child Ratio",
              status = "primary",
              solidHeader = TRUE,
              highchartOutput("tab4_line_chart4", height = 320),
              width = 12
            )
          ))
        ),
        tabPanel("3D Plot",
                 fluidRow(column(
                   12,
                   box(
                     title = "3D Scatter Plot for Total vs Male vs Female Population",
                     status = "primary",
                     solidHeader = TRUE,
                     plotlyOutput("tab4_3d_chart1", height = "320px")
                     ,
                     width = 12
                   )
                 )),
                 fluidRow(column(
                   12,
                   box(
                     title = "3D Scatter Plot for Total vs Male vs Female Graduates",
                     status = "primary",
                     solidHeader = TRUE,
                     plotlyOutput("tab4_3d_chart2", height = "320px")
                     ,
                     width = 12
                   )
                 ))),
        tabPanel(
          "Box Plots",
          fluidRow(column(
            6,
            box(
              title = "Box Plot for States with respect to Population",
              status = "primary",
              solidHeader = TRUE,
              highchartOutput("tab4_box_plot1", height = "320px")
              ,
              width = 12
            )
          ),
          column(
            6,
            box(
              title = "Box Plot for States with respect to Total Literates",
              status = "primary",
              solidHeader = TRUE,
              highchartOutput("tab4_box_plot2", height = "320px")
              ,
              width = 12
            )
          )),
          fluidRow(column(
            6,
            box(
              title = "Box Plot for states with respect to Total Grads",
              status = "primary",
              solidHeader = TRUE,
              highchartOutput("tab4_box_plot3", height = "320px")
              ,
              width = 12
            )
          ),
          column(
            6,
            box(
              title = "Box Plot for states with respect to Sex Ratio",
              status = "primary",
              solidHeader = TRUE,
              highchartOutput("tab4_box_plot4", height = "320px")
              ,
              width = 12
            )
          ))
        )
      ),
      width = 12
    ), width = 10)
  )
  ),


conditionalPanel("false", icon("crosshair"))
  )