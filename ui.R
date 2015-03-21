# ui.R

shinyUI(navbarPage(position = "fixed-top", theme = "bootstrap.css",
  title = "Heatmap App",

  #################### Analysis Tab ####################  
  tabPanel("Analysis",  
           
    ##########  Heatmap Plot Output ##########        
    conditionalPanel(condition = "input.display == 'heatmap'",
      absolutePanel(id="heatmapPanel",
                    top = 0, left = 0, width="100%",
                    draggable = TRUE,
                    fixed = FALSE,
                    plotOutput(outputId='heatmap', width= "95%"))),
    
    ##########  Options Menu ##########   
    absolutePanel(style = "opacity: 0.92", id="dropdownMenu", 
      top = 60, left = 0, width="100%", 
      draggable = FALSE,
      fixed = TRUE,
    
      ########## Option Menu Button ##########  
      column(3,actionButton("showOptionsButton", label = h4("Options Menu")),
    
      conditionalPanel(condition = "input.showOptionsButton%2 == 0",   
          ########## File input options ##########  
            h3("File Input:"),
            fileInput('file1',
                      'Choose File',
                      accept=c('text/csv','text/comma-separated-values,text/plain','.csv')),
            tags$textarea(id="textInput", rows=5, cols=25, ""),
            
            radioButtons('sep', 
                     'File separator',
                     c(Tab='\t', Comma=',', Semicolon=';'),
                     '\t'))),
          
      
        # needed to repeat because of col(3) end bracket
        conditionalPanel(condition = "input.showOptionsButton%2 == 0",  
          column(3, 
            h3("Analysis:"),
            selectInput("clusterMethod", label = "Clustering Method", 
                        choices = list( 
                          "none" = 'none',
                          "single linkage" = 'single',
                          "complete linkage" = 'complete',
                          "average linkage" = 'average',
                          "centroid linkage" = 'centroid',
                          "mcquitty" = 'mcquitty'), 
                        selected = 'none'), 
            
            conditionalPanel(condition = "input.clusterMethod != \'none\'", 
                             strong("Apply Clustering To"),
                             checkboxInput('rowv', 'Rows', FALSE),
                             checkboxInput('colv', 'Columns', FALSE),
                             
                             strong("Show Dendrogram"),
                             checkboxInput('dendRow', 'Rows', FALSE),
                             checkboxInput('dendCol', 'Columns', FALSE)),
            
            selectInput("distanceMethod", label = "Distance Measurement Method", 
                        choices = list( 
                          "euclidean" = 'euclidean',
                          "manhattan" = 'manhattan'), #aka city block
                        selected = 'none'), 
            
            selectInput("scale", label = "Scale Type", 
                        choices = list(
                          "row" = 'row',
                          "column" = 'column',
                          "none"='none'),
                        selected = 'row')
            
          ),  
                 
                 
          ########## Customize options ##########
          column(3,  
            h3("Customize Image:"),
            radioButtons('display', 'Display',
                         c(Table='table', Heatmap='heatmap'),
                         'heatmap'),
            sliderInput("heightSlider",
                        "Height",
                        min = 600, max = 2000, value = 700),
            checkboxInput('previewFullSize', 'Preview Full Size', FALSE),
            
            selectInput("startColour", label = "Positive Colour", 
                        choices = list( 
                          "red" = 'red',
                          "orange" = 'orange',
                          "yellow" = 'yellow',
                          "green" = 'green', 
                          "blue" = 'blue',
                          "purple" = 'purple'), 
                        selected = 1),
            selectInput("endColour", label = "Negative Colour", 
                        choices = list(
                          "green" = 'green',
                          "red" = 'red',
                          "orange" = 'orange',
                          "yellow" = 'yellow', 
                          "blue" = 'blue',
                          "purple" = 'purple'), 
                        selected = 1),
           
            sliderInput("binSlider",
                          "Number of shades",
                           min = 3, max = 299, value = 160)
        ),
          
          ########## Save options ##########  
          column(3,
            h3("Save Output:"),
            
            textInput("imageTitle", label = "Title", value = ""),
            textInput("xaxis", label = "X Axis Label", value = ""),
            textInput("yaxis", label = "Y Axis Label", value = ""),
            checkboxInput('downloadFullSize', 'Download Full Size', TRUE),
            radioButtons('downloadFormat', 
                         'File Format',
                         c("PNG"='png', "PDF"='pdf', "JPEG"='jpeg'),
                         'png'),
            
            downloadButton('downloadData', 'Save Image')))),
    
    ########## Figures output ##########
    conditionalPanel(condition = "input.display == 'table'",
      tableOutput('dataTable'))
    
  ),
  
  #################### Gallery Tab ####################
  tabPanel("Gallery"), 
  
  #################### Instructions Tab ####################
  tabPanel("Instructions",
    h3("Instructions"),
    wellPanel(
    helpText("Tutiorial on how to use app"))),
  
  #################### Contact Tab ####################
  tabPanel("Contact", 
    wellPanel(paste("Contact me by email: babicki@ualberta.ca"))) 
  
))