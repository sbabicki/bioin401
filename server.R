library(shiny)
library(gplots)
library(ctc)
library(pryr)
source("helpers.R")

# max upload size is 4MB
options(shiny.maxRequestSize=4*1024^2)

# maximum number of nested expressions to be evaluated
options(expressions = 500000)

# source: bit.ly/1Q8CPL2
Sys.setenv(R_ZIPCMD="/usr/bin/zip")

shinyServer(function(input, output,session){

	########################################### FUNCTIONS ###########################################

	################# get_file ################# 
	# retrieves the file input data
	get_file <- reactive({
		
		sep = input$sep
		
		# input$file is NULL before upload
		inFile <- input$file
		path <- inFile$datapath
		
		# if user chooses example file
		if(input$chooseInput == 'examples'){
			path <- input$exampleFiles
			sep <- "\t"
    }
		
		if (is.null(path)){
			return(NULL)
		}

    # if file has been uploaded
    # data frame with 'name', 'size', 'type', and 'datapath' columns. 
    # 'datapath' column contains the local filenames where the data can be found.
		else{
			data <- read.delim(path, header = TRUE, sep = sep)
			if(input$display == "heatmap"){
				data <- remove_strings(data)
			}
    }
		
    if (is.null(data)){
      return(NULL)
    }
    else{
    	return(data)
    }
	})
	
	################# remove_strings ################# 
	# removes strings from file content and assigns the 'NAME' column as the row labels
	remove_strings <- function(x){
		nums <- sapply(x, is.numeric)
		y <- x[,nums]
		
		# try to find a column with title name
		name = 'NAME'
		tryCatch({
			nameRow <- x[,name]
			rownames(y) <- make.names(nameRow, unique=TRUE)
			},
			finally = {return(y)}
		)
	}
	
	################# get_data_matrix ################# 
	# returns file input data as a data matrix
	get_data_matrix <- function(){
		
		# get the file data
		fileData <- get_file()
		if(is.null(fileData)){
			return(NULL)
		}
		
		# convert to data matrix
		return(data.matrix(fileData))
	}
	
	################# get_colv #################
	# boolean, true if cluster col is selected in ui.r
	get_colv <- function(){
		if(input$clusterMethod == 'none'){
			return(FALSE)
		}
    else{
    	return(input$colv)
    }
	}
	
	################# get_rowv #################
	# boolean, true if cluster row is selected in ui.r
	get_rowv <- function(){
		if(input$clusterMethod == 'none'){
			return(FALSE)
		}
    else{
    	return(input$rowv)
    }
	}
	
	################# get_dendrogram ################# 
	# returns 'both', 'column', 'row', or 'none'; depending on the slider settings in ui.r
	get_dendrogram <- function(){
		if(input$dendCol && input$dendRow){
			return("both")
		}
		else if(input$dendCol){
			return("column")
		}
		else if(input$dendRow){
			return("row")
		}
		else{
			return("none")
		}
	}
	
	################# get_dist #################
	# calculates a distance matrix 
	get_dist <- function(x){
		# source http://stackoverflow.com/questions/15773189/remove-na-nan-inf-in-a-matrix
		# replace all non-finite values with 0
		x[!rowSums(!is.finite(x)),]
		x[!is.finite(x)] <- 0
		
		if(input$distanceMethod == 'euclidean' || input$distanceMethod == 'manhattan'){
			x <- dist(x, method = input$distanceMethod)
		}
		else{
			x <- as.dist(1-cor(t(data.matrix(x)), method=input$distanceMethod))
		}
		return(x)
	}
	
	################# get_hclust #################
	# uses hclust to cluster data using get_dist distance matrix
	get_hclust <- function(x){
		x <- hclust(get_dist(x), method = input$clusterMethod)
		return(x)
	}
	
	################# get_table_data #################
	# clusters and returns data for displaying when table option is selected in ui.r
	get_table_data <- function(){
		data <- get_file()
		colv <- get_colv()
		rowv <- get_rowv()
		
		if(is.null(data)){
			return(NULL)
		}
		
    # cluster rows
    if(rowv){
    	row <- get_hclust(data.matrix(data))
    	data <- data[row$order,]
    }
		
		# cluster cols
		if(colv){
			# only cluster cols with numbers
			nums <- sapply(data, is.numeric)
			numericCols <- data[,nums]
			nonNums <- !sapply(data, is.numeric)
			omittedCols <- data[,nonNums]
			
			col <- get_hclust(t(numericCols))
			data <- cbind2(omittedCols, numericCols[,col$order])
		}
		
		return(data)
	}
	
	################# get_heatmap ################# 
	# returns a heatmap created from the file input data
	get_heatmap <- function(){
		
		# get the data matrix to convert to heatmap
		heatmapDataMatrix <- get_data_matrix()
		colv <- get_colv()
		rowv <- get_rowv()
		dendrogram <- get_dendrogram()
		
		if(is.null(heatmapDataMatrix)){
			return(NULL)
		}
		
		# makes heatmap brighter when scale is 'none'
		# https://mintgene.wordpress.com/2012/01/27/heatmaps-controlling-the-color-representation-with-set-data-range/
		if(input$scale == "none"){
			quantile.range <- quantile(na.rm = TRUE, heatmapDataMatrix, probs = seq(0, 1, 0.01))
			low_cutoff <- quantile.range[1+input$brightness]
			high_cutoff <- quantile.range[101-input$brightness]
			breaks <- seq(low_cutoff, high_cutoff, (high_cutoff - low_cutoff)/input$binSlider)
		}
		else{
			breaks <- input$binSlider
		}
		
		my_palette <- colorRampPalette(c(input$startColour, "black", input$endColour))
		
		# cluster cols
		if(rowv){
			row <- as.dendrogram(get_hclust(heatmapDataMatrix))
		}
		else{
			row <- FALSE
		}
		
		# cluster cols
		if(colv){
			col <- as.dendrogram(get_hclust(t(heatmapDataMatrix)))
		}
		else{
			col <- FALSE
		}
		
		# create the heatmap
		heatmap.2(heatmapDataMatrix,
			col=my_palette, 
			scale=input$scale, 
			na.color=input$missingDataColour,
			key=FALSE, 
			symkey=FALSE, 
			density.info="none", 
			trace="none", 
			Rowv = row, 
			Colv = col, 
			dendrogram = dendrogram, 
			keysize=0.5, 
			cexRow=input$cexRow,
			main=input$imageTitle, 
			xlab=input$xaxis, 
			ylab=input$yaxis, 
			offsetCol = 0, 
			offsetRow = 0, 
			margins=c(5,10), 
			lhei=c(1,8), 
			lwid=c(0.1,0.5), 
			breaks = breaks
			)
		
		graphics.off()
	}
	
	################# get_height ################# 
	# calculate the height based on matrix dimentions and current width
	get_height<-function(cond){
		x <- get_file()
		if(is.null(x)){
			return(0)
		}
		else{
			if(cond){
				size <- input$widthSlider/ncol(x) * nrow(x)
			}
			else{
				size <- input$heightSlider
			}
			return(size)
		}
  }
  
	################# get_width ################# 
	get_width <- function(){
		return(input$widthSlider)
	}

	get_res <- function(){
		return(input$resSlider)
	}
	
	########################################### OUTPUT ###########################################

	################# Display Table ################# 
	output$dataTable <- renderDataTable({
		fileData <- get_table_data()
		
		if(is.null(fileData)){
			return(NULL)
		}
		return(fileData)
	})
	
	################# Display Heatmap ################# 
	output$heatmap <- renderPlot(
		get_heatmap(),
		height = reactive({get_height(input$previewFullSize)}),
		width = reactive({get_width()}))
	
	################# Save Example File ################# 
	output$downloadExample <- downloadHandler(
		filename = "example.txt",
		content = function(file){
			write.table(read.delim(input$exampleFiles, header=TRUE, sep="\t"), sep = "\t",  file)})
	
	################# Print Opening Message #################
	output$openingMessage <- renderUI(
		if(is.null(input$file) && input$chooseInput != 'examples') {
			div(class = "openingMessage", includeHTML(path = "www/index.html"))
		}
	)
	
	################# Save File As Table ################# 
	output$downloadTable <- downloadHandler(
		filename = "data.zip",
		content = function(file){
			data <- get_data_matrix()
			
			save_wd <- getwd()
			setwd(tempdir())
			
			fs <- c("text_file.txt")
			write.table(data, "text_file.txt", sep = input$sepSave)
    	
			if(input$clusterMethod != 'none'){
				
				hr <- NULL
				hc <- NULL
				
				if(get_rowv()){
					fs <- c(fs, "cluster.gtr")
					hr <- get_hclust(data)
					r2gtr(hr=hr,file="cluster.gtr",distance=input$distanceMethod,dec='.',digits=5)
				}
				if(get_colv()){
					fs <- c(fs,"cluster.atr")
					hc <- get_hclust(t(data))
					r2atr(hc=hc,file="cluster.atr",distance=input$distanceMethod,dec='.',digits=5)
				}
				fs <- c(fs, "cluster.cdt")
				r2cdt2(hr=hr,hc=hc,data=data,labels=FALSE,description=FALSE,file="cluster.cdt",dec='.')

    	}
			zip(zipfile=file, files=fs)
    	setwd(save_wd)
  	}
  )
	
	################# Save File As Image ################# 
	output$downloadHeatmap <- downloadHandler(
		
		filename = reactive({paste("heatmap.", input$downloadFormat, sep="")}),
		
		content = function(file) {
			if(input$downloadFormat == "pdf"){
				pdf(file, width=get_width()/72, height=get_height(input$downloadFullSize)/72)
				get_heatmap()
			}
			else{
				tryCatch({
					png(file, units="in", width = get_width()/72, height=get_height(input$downloadFullSize)/72, res=get_res())	
					get_heatmap()
				}, 
				error = function(err){
					validate(need(FALSE,"PNG image too large. Please decrease the dimensions or resolution of the image."))
					return(NULL)
				})
			}
		}) 
})
