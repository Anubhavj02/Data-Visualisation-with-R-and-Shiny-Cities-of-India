
[![GitHub Icon](https://cdn4.iconfinder.com/data/icons/ionicons/512/icon-social-github-32.png)](https://github.com/Anubhavj02) [![LinkedIn Icon](https://cdn3.iconfinder.com/data/icons/free-social-icons/67/linkedin_circle_color-32.png)](https://www.linkedin.com/in/anubhav-jain02/) [![Email Icon](https://cdn4.iconfinder.com/data/icons/miu-flat-social/60/mail-32.png)](mailto:jainan@tcd.ie)
# Exploring Top Cities of India using R and Shiny
It is an interactive **Data Visualisation** application developed to represent various attributes and details of **Top Indian Cities** using interactive, visually appealing and analyst friendly graphs. It lets the users interact with the graph, change and filter the data so that the user always gets the informations that is required.

It performs a state and city wise analysis/comparison through visualisation considering different parameters of the dataset.

## Application Youtube and Website Link

 - **Youtube Link**: [Cities of India Data Visualization Youtube Link](https://www.youtube.com/watch?v=KuYK-U7wDWU)
 [![IMAGE ALT TEXT HERE](https://github.com/Anubhavj02/Data-Visualisation-with-R-and-Shiny-Cities-of-India/blob/master/images/screenshot2.png)](https://www.youtube.com/watch?v=KuYK-U7wDWU)

- **Website Link**: [https://cities-of-india-data-visualization.shinyapps.io/CityDataVisualization/](https://cities-of-india-data-visualization.shinyapps.io/CityDataVisualization/)
 [![IMAGE ALT TEXT HERE](https://github.com/Anubhavj02/Data-Visualisation-with-R-and-Shiny-Cities-of-India/blob/master/images/screenshot1.png)](https://cities-of-india-data-visualization.shinyapps.io/CityDataVisualization/)

## Dataset Description
The dataset ["Top Cities of India, 2011"](https://www.kaggle.com/zed9941/top-500-indian-cities/data) is taken from **Kaggle**. This dataset is constructed and aggregated from Indian Cities **Census 2011**; cities with population of more than 1 Lac have been taken into consideration for this dataset. It does not give information of the current scenario as the dataset is old and constructed from 2011 census.


|Type|  Attributes|
|--|--|
|  **Categorical Data**| State code, State Name, Name of City |
|  **Ordered/ Quantitative**| Population, Male/Female Population, Total Literates, Male/Female Literates, Sex Ratio, Child Sex Ratio, Total Graduates, Male/ Female Graduates |


 - **name_of_city:** Name of the city
 - **State Attributes:** State code(unique) and State Name
 - **Population attributes:** Total Population, Male Population, Female Population
 - **Literacy Attributes:** Total Literates, Female Literates, Male Literates
 - **Sex Ratio:** Ratio males to females in the population
 - **Child Sex Ratio:** Sex ratio for the age group 0 to 6
 - **Graduates:** Total Graduates, Male Graduates, Female Graduates
 
 ## R Libraries and Packages used
 - **Visualization Packages:** [Plotly](https://plot.ly/), [Highcharter](http://jkunst.com/highcharter/), CirclePackeR, Data.tree,
   treemap
 - **Mapping/Spatial Graph Packages:** [Leaflet R](https://rstudio.github.io/leaflet/) and [Highcharter](http://jkunst.com/highcharter/)
 - **Dashboard and Appearance:** [Shiny Dashboard](https://rstudio.github.io/shinydashboard/), [Shiny Themes](https://rstudio.github.io/shinythemes/) and Widgets

## Steps to run

1. Install R in the system
2. Clone this repository
3. After installing, open Terminal type
```console
cmd:~$ R
```
4. R Terminal will open in the console, install the following packages
```console
> install.packages("shiny")
> install.packages("plotly")
> install.packages("highcharter")
> install.packages("shinydashboard")
> install.packages("data.tree")
> install.packages("treemap")
> install.packages("leaflet")
> install.packages("shinyWidgets")
> install.packages("dplyr")
> install.packages("shinythemes")
> install.packages("stringr")
```
5. Run the app
```console
> runApp("--Path_To_The_Cloned_Folder--/CityDataVisualization")
```

## ScreenShots

 - **Leaflet Spatial Map**
 ![enter image description here](https://github.com/Anubhavj02/Data-Visualisation-with-R-and-Shiny-Cities-of-India/blob/master/images/screenshot3.png)
 
 
 - **Choropleth and Bubble Spatial Map**
![enter image description here](https://github.com/Anubhavj02/Data-Visualisation-with-R-and-Shiny-Cities-of-India/blob/master/images/screenshot4.png)
![enter image description here](https://github.com/Anubhavj02/Data-Visualisation-with-R-and-Shiny-Cities-of-India/blob/master/images/screenshot5.png)


 - **Bar Plots and Histogram**
 ![enter image description here](https://github.com/Anubhavj02/Data-Visualisation-with-R-and-Shiny-Cities-of-India/blob/master/images/screenshot6.png)
 
 
 - **Line Charts**
 ![enter image description here](https://github.com/Anubhavj02/Data-Visualisation-with-R-and-Shiny-Cities-of-India/blob/master/images/screenshot10.png)
 
 
 - **Pie Charts**
 ![enter image description here](https://github.com/Anubhavj02/Data-Visualisation-with-R-and-Shiny-Cities-of-India/blob/master/images/screenshot8.png)
 
 
 - **Heatmap**
 ![enter image description here](https://github.com/Anubhavj02/Data-Visualisation-with-R-and-Shiny-Cities-of-India/blob/master/images/screenshot9.png)
 
 
 - **Dot Plots**
 ![enter image description here](https://github.com/Anubhavj02/Data-Visualisation-with-R-and-Shiny-Cities-of-India/blob/master/images/screenshot7.png)
 
 
 - **3D plots**
 ![enter image description here](https://github.com/Anubhavj02/Data-Visualisation-with-R-and-Shiny-Cities-of-India/blob/master/images/screenshot11.png)
 
 
 - **Box Plots**
 ![enter image description here](https://github.com/Anubhavj02/Data-Visualisation-with-R-and-Shiny-Cities-of-India/blob/master/images/screenshot12.png)

