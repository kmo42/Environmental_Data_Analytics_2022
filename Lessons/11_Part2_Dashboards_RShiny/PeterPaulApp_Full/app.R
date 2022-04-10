#### Load packages ----
library(shiny)
library(shinythemes)
library(tidyverse)

#### Load data ----
nutrient_data <- read_csv("Data/NTL-LTER_Lake_Nutrients_PeterPaul_Processed.csv")
nutrient_data$sampledate <- as.Date(nutrient_data$sampledate, format = "%Y-%m-%d")
nutrient_data <- nutrient_data %>%
  filter(depth_id > 0) %>%
  select(lakename, sampledate:po4)

#### Define UI ----
ui <- fluidPage(theme = shinytheme("readable"),
  titlePanel("Nutrients in Peter Lake and Paul Lake"),
  sidebarLayout(
    sidebarPanel(
      
      # Select nutrient to plot
      selectInput(inputId = "y", 
                  label = "Nutrient",
                  choices = c("tn_ug", "tp_ug", "nh34", "no23", "po4"), 
                  selected = "tp_ug"),
      
      # Select depth
      checkboxGroupInput(inputId = "fill",
                         label = "Depth ID",
                         choices = unique(nutrient_data$depth_id),
                         selected = c(1, 7)),
      
      # Select lake
      checkboxGroupInput(inputId = "shape",
                         label = "Lake",
                         choices = c("Peter Lake", "Paul Lake"),
                         selected = "Peter Lake"),

      # Select date range to be plotted
      sliderInput(inputId = "x",
                  label = "Date",
                  min = as.Date("1991-05-01"),
                  max = as.Date("2016-12-31"),
                  value = c(as.Date("1995-01-01"), as.Date("1999-12-31")))),

    # Output
    mainPanel(
      plotOutput("scatterplot", brush = brushOpts(id = "scatterplot_brush")), 
      tableOutput("mytable")
    )))

#### Define server  ----
server <- function(input, output) {
  
    # Define reactive formatting for filtering within columns
     filtered_nutrient_data <- reactive({
       nutrient_data %>%
         filter(sampledate >= input$x[1] & sampledate <= input$x[2]) %>%
         filter(depth_id %in% input$fill) %>%
         filter(lakename %in% input$shape) 
     })
    
    # Create a ggplot object for the type of plot you have defined in the UI  
       output$scatterplot <- renderPlot({
        ggplot(filtered_nutrient_data(), 
               aes_string(x = "sampledate", y = input$y, 
                          fill = "depth_id", shape = "lakename")) +
          geom_point(alpha = 0.6, size = 4) +
          theme_classic(base_size = 14) +
          scale_shape_manual(values = c(21, 24)) +
          labs(x = "Date", y = expression(Concentration ~ (mu*g / L)), shape = "Lake", fill = "Depth ID") +
          #scale_fill_distiller(palette = "YlOrBr", guide = "colorbar", direction = 1)
          scale_fill_viridis_c(option = "viridis", begin = 0, end = 0.8, direction = -1)
      })
       
    # Create a table that generates data for each point selected on the graph  
       output$mytable <- renderTable({
         brush_out <- brushedPoints(filtered_nutrient_data(), input$scatterplot_brush)
       })
       
  }


#### Create the Shiny app object ----
shinyApp(ui = ui, server = server)

#### Questions for coding challenge ----
#1. Play with changing the options on the sidebar. 

# Choose a shinytheme that you like. The default here is "yeti"
     ## Chose "readable".

# How do you change the default settings? 
     ## Google how to change default settings, and pick a shinyoptions(). To change 
     ## the theme, first you must download shinythemes, then change the part in 
     ## quotes on the fluidpage line to desired theme. 

# How does each type of widget differ in its code and how it references the dataframe? 
     ## One widget has a select dropdown menu, another has a select depth box to check, 
     ## a third has a select box for which lake, and the fourth has a sliding date bar 
     ## to choose a date range.

#2. How is the mainPanel component of the UI structured? 
     ## It runs the data, and functions in the widgets and pops out a scatterplot 
     ## with different colored points with color based on depth. Time is on the 
     ## x-axis and concentrations is on the y axis. 

# How does the output appear based on this code? 
     ## The output is a total phosphorus scatterplot with dark red, orange, and 
     ## white triangles and circles points for Peter and Paul Lake. 

#3. Explore the reactive formatting within the server.

# Which variables need to have reactive formatting? 
     ## The nutrient variable, depth, lake, and date need to have reactive formatting, 
     ## plus with fluidpage the outputs will form to whatever shape the output window 
     ## is set to be. 

# How does this relate to selecting rows vs. columns from the original data frame? 
     ## Nutrient type will select a column, depth will pull rows from the depth 
     ## column, lake will select a column, and data will select row range from 
     ## sampledate column. 

#4. Analyze the similarities and differences between ggplot code for a rendered vs. static plot.

# Why are the aesthetics for x, y, fill, and shape formatted the way they are? 
     ## They are created like an object that is referenced in the server function
     ## function, and then are given a name that is shown to the user to describe 
     ## what the variable is. Then the filtering or choices are listed to show 
     ## all the options, and the last row shows what is selected for the output. 
     ## This is formatted this way to tell the output function what to reference
     ## for the main panel. 

# Note: the data frame has a "()" after it. This is necessary for reactive formatting.
     ## Okay, good to know.

# Adjust the aesthetics, playing with different shapes, colors, fills, sizes, transparencies, etc.
     ## I tried out a transparency of 0.6 instead of 0.8, turned on the viridis color palette, 
     ## increased the point sizes to 4 instead of 2, and changed the theme_classic base_theme 
     ## size from 14 to 20 and didn't see a difference in the output. 

#5. Analyze the code used for the renderTable function. 

# Notice where each bit of code comes from in the UI and server. 
     ## Pulled the data from the filtered nutrient data. 

# Note: renderTable doesn't work well with dates. "sampledate" appears as # of days since 1970.
     ## Okay. 

