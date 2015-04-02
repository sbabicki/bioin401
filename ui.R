# ui.R

shinyUI(navbarPage(position = "fixed-top", theme = "bootstrap.css",

  title = "Heatmap App",

  #################### Analysis Tab ####################  
  tabPanel("Analysis",  
    tags$link(rel = "stylesheet", type = "text/css", href = "myStyle.css"),  
    tags$script(type="text/javascript", src = "busy.js"),
    div(class = "busy", includeHTML("www/spinner.html")),
    
    ##########  Heatmap Plot Output ##########        
    conditionalPanel(condition = "input.display == 'heatmap'", 
      absolutePanel(id="heatmapPanel",
                    top = "70px", left = 0, width="100%",
                    draggable = FALSE,
                    fixed = FALSE,
                    plotOutput(outputId='heatmap'))),
    
    ########## Table Output ##########
    conditionalPanel(condition = "input.display == 'table'",
      absolutePanel(id="tablePanel",
                    top = "70px", left = 0, width="100%",
                    draggable = FALSE,
                    fixed = FALSE,
                    tableOutput('dataTable'))),
    
    ##########  Options Menu ##########   
    absolutePanel(style = "z-index:10000;", id="dropdownMenu", 
                  top = 0, right = 0, width="360px", 
                  draggable = FALSE,  
                  fixed=TRUE,
    
    ########## Option Menu Button ##########                
    actionButton("showOptionsButton", label = h4("Options Menu"),),
    tags$div(style = "margin-top:41px; right:40px; position:fixed; z-index:1000000; float:right;", "Click to expand"),
    
    conditionalPanel(condition = "input.showOptionsButton%2 == 0",  
      tags$div(style = "padding:1em; opacity: 0.92; background-color:#E1E5E5; float:right;
              top:63px; bottom:0; right:0; position:fixed; overflow-y:scroll;", 
      tabsetPanel(
        
        ########## File input tab ##########  
        tabPanel(title = "File Input",
            h3("File Input:"),
            radioButtons("chooseInput",
                         'Choose Input Type',
                         c("Upload File" = 'fileUpload',
                           "Example File" = 'examples'
                           ),
                         'fileUpload'),
            
            conditionalPanel(condition = "input.chooseInput == \'examples\'",
              selectInput("exampleFiles", label = "Choose Example File", 
                        choices = list(
                          "Example 1" = 'example_input/example1.txt',
                          "Example 2" = 'example_input/example2.txt',
                          "Example 3" = 'example_input/example3.txt'),
                        selected = 1),
              downloadButton('downloadExample', 'Download Example File')),  
            
            conditionalPanel(condition = "input.chooseInput == \'fileUpload\'",
              fileInput('file1',
                      'Upload File',
                      accept=c('text/csv','text/comma-separated-values,text/plain','.csv')),
              radioButtons('sep', 
                           'File separator',
                           c(Tab='\t', Comma=',', Semicolon=';'),
                           '\t'))),
            
            #tags$textarea(id="textInput",  rows=12, cols=38, NULL),

          ########## Analysis tab ##########  
          tabPanel(title = "Analysis",
            h3("Analysis:"),
            selectInput("clusterMethod", label = "Clustering Method", 
                        choices = list( 
                          "none" = 'none',
                          "single linkage" = 'single',
                          "complete linkage" = 'complete',
                          "average linkage" = 'average',
                          "centroid linkage" = 'centroid'
                          ), 
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
                        selected = 'row')),  
                 
                 
          ########## Customize image tab ##########
          tabPanel(title =  "Image",
            h3("Customize Image:"),
            
            radioButtons('display', 'Display',
                         c(Table='table', Heatmap='heatmap'),
                         'heatmap'),
            
            sliderInput("heightSlider",
                        "Height",
                        min = 600, max = 2000, value = 700),
            sliderInput("widthSlider",
                        "Width",
                        min = 760, max = 1500, value = 800),
            
            checkboxInput('previewFullSize', 'Preview Full Size', FALSE),
            
            selectInput("endColour", label = "Low Colour", 
                        choices = list(
                          "green" = 'green',
                          "red" = 'red',
                          "orange" = 'orange',
                          "yellow" = 'yellow', 
                          "blue" = 'blue',
                          "purple" = 'purple'), 
                        selected = 1),
            
            selectInput("startColour", label = "High Colour", 
                        choices = list( 
                          "red" = 'red',
                          "orange" = 'orange',
                          "yellow" = 'yellow',
                          "green" = 'green', 
                          "blue" = 'blue',
                          "purple" = 'purple'), 
                        selected = 1),
           
            sliderInput("binSlider",
                          "Number of shades",
                           min = 3, max = 299, value = 160),
            
            textInput("imageTitle", label = "Title", value = ""),
            textInput("xaxis", label = "X Axis Label", value = ""),
            textInput("yaxis", label = "Y Axis Label", value = "")),
          
          ########## Save options ##########  
          tabPanel(title = "Save As",
            h3("Save Output:"),
            checkboxInput('downloadFullSize', 'Download Full Size', TRUE),
            radioButtons('downloadFormat', 
                         'File Format',
                         c("PNG"='png', "PDF"='pdf', "JPEG"='jpeg'),
                         'png'),
            sliderInput("resSlider",
                        "Resolution",
                        min = 72, max = 600, value = 72),
            downloadButton('downloadData', 'Save Image')))
  )))),
  
  #################### Gallery Tab ####################
  tabPanel("Gallery",
    tags$div(class="myTabs", includeHTML("www/gallery.html"))), 
  
  #################### Instructions Tab ####################
  tabPanel("Instructions",
    tags$div(class="myTabs", includeHTML("www/instructions.html"))),
  
  #################### Contact Tab ####################
  tabPanel("Contact",
    tags$div(class="myTabs", includeHTML("www/contact.html"))) 
))