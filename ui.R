# ui.R

shinyUI(navbarPage(position = "fixed-top", theme = "theme.css",

  title = "Heatmap App",

  #################### Analysis Tab ####################  
  tabPanel("Analysis",  
    tags$script(type="text/javascript", src = "busy.js"),
    div(class = "busy", includeHTML("www/spinner.html")),

    ##########  Heatmap Plot Output ##########        
    conditionalPanel(condition = "input.display == 'heatmap'", 
      absolutePanel(id="heatmapPanel",
                    top = "70px", left = 0, width="100%",
                    draggable = FALSE,
                    fixed = FALSE,
                    plotOutput(outputId='heatmap')
                     )),
    
    ########## Table Output ##########
    conditionalPanel(condition = "input.display == 'table'",
      absolutePanel(id="tablePanel",
                    top = "70px", left = 0, width="100%",
                    draggable = FALSE,
                    fixed = FALSE,
                    dataTableOutput('dataTable'))),

    ##########  Options Menu ##########   
    absolutePanel(style = "z-index:10000;", id="dropdownMenu", 
                  top = 0, right = 0, width="360px", 
                  draggable = FALSE,  
                  fixed=TRUE,
                  
    ########## Option Menu Button ##########                
    actionButton("showOptionsButton", label = h4("Toggle Options Menu")),
    conditionalPanel(condition = "input.showOptionsButton%2 == 0", 
      tags$div(id = "optionsMenu", 
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
              tags$div(class="exampleInfo", 
                       conditionalPanel(condition = "input.exampleFiles == \'example_input/example1.txt\'",includeHTML("www/example1info.html")),
                       conditionalPanel(condition = "input.exampleFiles == \'example_input/example2.txt\'",includeHTML("www/example2info.html")),
                       conditionalPanel(condition = "input.exampleFiles == \'example_input/example3.txt\'",includeHTML("www/example3info.html"))),
              downloadButton(class='btn-info', outputId='downloadExample', label='Download Example Text File')),  
            
            conditionalPanel(condition = "input.chooseInput == \'fileUpload\'",
              fileInput('file',
                      'Upload File (4MB maximum file size)',
                      accept=c('text/csv','text/comma-separated-values,text/plain','.csv')),
              radioButtons('sep', 
                           'File separator',
                           c(Tab='\t', Comma=',', Semicolon=';'),
                           '\t'))),

          ########## Data tab ##########  
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
                             checkboxInput('rowv', 'Rows', TRUE),
                             checkboxInput('colv', 'Columns', FALSE),
                        conditionalPanel(condition = "input.display == 'heatmap'",
                             strong("Show Dendrogram"),
                             checkboxInput('dendRow', 'Rows', TRUE),
                             checkboxInput('dendCol', 'Columns', FALSE))),
            
            selectInput("distanceMethod", label = "Distance Measurement Method", 
                        choices = list( 
                          "euclidean" = 'euclidean',
                          "pearson" = 'pearson',
                          "kendall's tau" = 'kendall', 
                          "spearman rank correlation" = 'spearman', 
                          "manhattan" = 'manhattan'), #aka city block
                        selected = 'none'), 
            
            radioButtons('display', 'Display As',
                         c("Heatmap"='heatmap', "Table"='table'),
                         'heatmap')
            
            ),  
                 
                 
          ########## Customize image tab ##########
          tabPanel(title =  "Image",
            h3("Customize Image:"),
            
          conditionalPanel(condition = "input.display == 'heatmap'",  
           selectInput("scale", label = "Scale Type", 
                         choices = list(
                         "row" = 'row',
                         "column" = 'column',
                         "none"='none'),
                          selected = 'none'),       
           
           selectInput("cexRow", label = "Row Label Size",
                        choices = list(
                        "extra large" = 2,
                        "large" = 1,
                        "medium" = 0.5,
                        "small" = 0.25,
                        "extra small" = 0.125),
                         selected=1),            
            
            sliderInput("widthSlider",
                        "Width (in px)",
                        min = 760, max = 1500, value = 800),
           
           checkboxInput('previewFullSize', 'Preview Full Height (not recomended for large files)', FALSE),
           conditionalPanel(condition = "input.previewFullSize == 0",
                sliderInput("heightSlider",
                          "Height (in px)",
                          min = 600, max = 2000, value = 700)
            ),
            
            selectInput("startColour", label = "Low Colour", 
                        choices = list(
                          "green" = 'green',
                          "red" = 'red',
                          "orange" = 'orange',
                          "yellow" = 'yellow', 
                          "blue" = 'blue',
                          "purple" = 'purple'),
                        selected = 1),
            
            selectInput("missingDataColour", label = "Missing Data Colour", 
                        choices = list( 
                          "black" = 'black',
                          "grey" = 'grey',
                          "white" = 'white'),
                        selected = 1),
            
            selectInput("endColour", label = "High Colour", 
                        choices = list( 
                          "red" = '#FF0000',
                          "orange" = 'orange',
                          "yellow" = 'yellow',
                          "green" = 'green', 
                          "blue" = 'blue',
                          "purple" = 'purple'),
                        selected = 1),
						
           sliderInput("binSlider",
												"Number of bins",
												min = 3, max = 299, value = 160),
						conditionalPanel(condition = "input.scale == 'none'",
							sliderInput("brightness",
													"Brightness",
													min = 1, max = 45, value = 5)),
            
            textInput("imageTitle", label = "Title", value = ""),
            textInput("xaxis", label = "X Axis Label", value = ""),
            textInput("yaxis", label = "Y Axis Label", value = ""))),
          
          ########## Save options ##########  
          tabPanel(title = "Save",
            h3("Save Output As Heatmap:"),
            
              radioButtons('downloadFormat', 
                         'File Format',
                         c("PDF"='pdf', "PNG"='png'),
                         'pdf'),
            	checkboxInput('downloadFullSize', 'Download Full Size', TRUE),
              conditionalPanel(condition="input.downloadFormat=='png'", 
                        sliderInput("resSlider", "PNG Resolution (in ppi)",
                        min = 72, max = 600, value = 72)),
            
              downloadButton(class='btn-info', outputId = 'downloadHeatmap', 'Save Image As'),
            h3("Save Output As Table:"),
              radioButtons('sepSave', 
                           'File separator for text file',
                           c(Tab='\t', Comma=',', Semicolon=';'),
                           '\t'), 
              downloadButton(class='btn-info', outputId = 'downloadTable', 'Save Table As')    
            )))))),
  
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