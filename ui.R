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
				plotOutput(outputId='heatmap'))),

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
							
							radioButtons('chooseInput',
								label = "Choose Input Type",
								choices = c(
									"Upload File" = 'fileUpload',
									"Example File" = 'examples'),
								selected = 'fileUpload'),
							
							conditionalPanel(condition = "input.chooseInput == \'examples\'",
								
								selectInput('exampleFiles', 
									label = "Choose Example File",
									choices = c(
										"Example 1" = 'example_input/example1.txt',
										"Example 2" = 'example_input/example2.txt',
										"Example 3" = 'example_input/example3.txt'),
									selected = 1),
								
								tags$div(class="exampleInfo",
									conditionalPanel(condition = "input.exampleFiles == \'example_input/example1.txt\'", includeHTML("www/example1info.html")),
									conditionalPanel(condition = "input.exampleFiles == \'example_input/example2.txt\'", includeHTML("www/example2info.html")),
									conditionalPanel(condition = "input.exampleFiles == \'example_input/example3.txt\'", includeHTML("www/example3info.html"))),
								
								downloadButton(class = "btn-info", outputId = 'downloadExample', label = "Download Example Text File")),  
							
							conditionalPanel(condition = "input.chooseInput == \'fileUpload\'",
								
								fileInput('file',
									label = "Upload File (4MB maximum file size)"),
								
								radioButtons('sep',
									label = "File Separator",
									choices = c(
										"Tab" = '\t', 
										"Comma" = ',', 
										"Semicolon" = ';'),
									selected = '\t'))),
						
						########## Data tab ##########
						tabPanel(title = "Analysis",
							
							h3("Analyze Data:"),
							
							selectInput('clusterMethod', 
								label = "Clustering Method",
								choices = c(
									"none" = 'none',
									"single linkage" = 'single',
									"complete linkage" = 'complete',
									"average linkage" = 'average',
									"centroid linkage" = 'centroid'),
								selected = 'none'),

							conditionalPanel(condition = "input.clusterMethod != 'none'", 
								
								strong("Apply Clustering To"),
							
								checkboxInput('rowv', 
									label = "Rows", 
									value = TRUE),
								
								checkboxInput('colv', 
									label = "Columns", 
									value = FALSE),
								
								conditionalPanel(condition = "input.display == 'heatmap'",
									strong("Show Dendrogram"),
									
									checkboxInput('dendRow', 
										label = "Rows", 
										value = TRUE),
									
									checkboxInput('dendCol', 
										label = "Columns", 
										value = FALSE))),
							
							selectInput('distanceMethod', 
								label = "Distance Measurement Method",
								choices = c(
									"euclidean" = 'euclidean',
									"pearson" = 'pearson',
									"kendall's tau" = 'kendall',
									"spearman rank correlation" = 'spearman',
									"manhattan" = 'manhattan'),
								selected = 'none'),
							
							h3("Display Options:"),
							
							radioButtons('display',
								label = "Display Data As",
								choices = c(
									"Heatmap"='heatmap', 
									"Table"='table'),
								selected = 'heatmap')),
						
						
						########## Customize image tab ##########
						
						tabPanel(title =  "Image",
							
							h3("Size Options:"),
							
							conditionalPanel(condition = "input.display == 'heatmap'",
								
								selectInput('cexRow', 
									label = "Row Label Size",
									choices = c(
										"extra large" = 2,
										"large" = 1,
										"medium" = 0.5,
										"small" = 0.25,
										"extra small" = 0.125),
									selected = 1),
								
								sliderInput('widthSlider',
									label = "Width (in px)",
									min = 760, max = 1500, value = 800),
								
								checkboxInput('previewFullSize', 
									label = "Preview Full Height (not recomended for large files)", 
									value = FALSE),
								
								conditionalPanel(condition = "input.previewFullSize == 0",
									
									sliderInput('heightSlider',
										label = "Height (in px)",
										min = 600, max = 2000, value = 700)),
								
								h3("Colour Options:"),
								
								selectInput('startColour', 
									label = "Low Colour",
									choices = c(
										"green" = 'green',
										"red" = 'red',
										"orange" = 'orange',
										"yellow" = 'yellow',
										"blue" = 'blue',
										"purple" = 'purple'),
									selected = 1),
								
								selectInput("endColour", 
									label = "High Colour", 
									choices = c(
										"red" = 'red',
										"orange" = 'orange',
										"yellow" = 'yellow',
										"green" = 'green', 
										"blue" = 'blue',
										"purple" = 'purple'),
									selected = 1),
								
								selectInput('missingDataColour', 
									label = "Missing Data Colour",
									choices = c(
										"black" = 'black',
										"grey" = 'grey',
										"white" = 'white'),
									selected = 1),
								
								sliderInput('binSlider',
									label = "Number of bins",
									min = 3, max = 299, value = 160),

								conditionalPanel(condition = "input.scale == 'none'",
									
									sliderInput('brightness',
										label = "Brightness",
										min = 1, max = 45, value = 5)),
								
								selectInput('scale', 
									label = "Scale Type",
									choices = c(
										"row" = 'row',
										"column" = 'column',
										"none" = 'none'),
									selected = 'none'))),
						
						########## Save options ##########
						tabPanel(title = "Save",
							
							h3("Add Heatmap Labels:"),
							
							textInput('imageTitle', 
								label = "Title", 
								value = ""),
							
							textInput('xaxis', 
								label = "X Axis Label", 
								value = ""),
							
							textInput('yaxis', 
								label = "Y Axis Label", 
								value = ""),
							
							h3("Save Output As Heatmap:"),
							
							radioButtons('downloadFormat',
								label = "File Format",
								choices = c(
									"PDF"='pdf', 
									"PNG"='png'),
								selected = 'pdf'),
							
							checkboxInput('downloadFullSize', 
								label = "Download Full Size", 
								value = TRUE),
							
							conditionalPanel(condition="input.downloadFormat=='png'",
								
								sliderInput('resSlider', 
									label = "PNG Resolution (in ppi)",
									min = 72, max = 600, value = 72)),
							
							downloadButton(class="btn-info", outputId = 'downloadHeatmap', label = "Save Image As"),
							
							h3("Save Output As Table:"),
							radioButtons('sepSave',
								label = "File separator for text file",
								choices = c(
									"Tab" ='\t', 
									"Comma" =',', 
									"Semicolon" =';'),
								selected = '\t'),
							
							downloadButton(class="btn-info", outputId = 'downloadTable', label = "Save Table As"))))))),
	
	#################### Gallery Tab ####################
	tabPanel("Gallery",
		tags$div(class="myTabs", includeHTML("www/gallery.html"))), 
	
	#################### Instructions Tab ####################
	tabPanel("Instructions",
		tags$div(class="myTabs", includeHTML("www/instructions.html"))),

	#################### Contact Tab ####################
	tabPanel("Contact",
		tags$div(class="myTabs", includeHTML("www/contact.html")))))