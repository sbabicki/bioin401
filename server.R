library(shiny)
library(gplots)
library(ctc)
library(pryr)
#library(hyperSpec)

# setwd("~/")
# runApp("bioin401")

# shinyapps::deployApp('~/bioin401')
# shinyapps::terminateApp('bioin401')
# https://www.shinyapps.io/admin/#/dashboard

#max upload size is 4MB
options(shiny.maxRequestSize=4*1024^2)

shinyServer(function(input, output,session) {

  ########################################### FUNCTIONS ###########################################
  
  ################# get_file ################# 
  # retrieves the file input data
  get_file<-reactive({
    
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
      data <- read.csv(path, header=TRUE, sep=sep)
      if(input$display == "heatmap")
        data <- remove_strings(data)
    }
    
    if (is.null(data)){
      return(NULL)
    }
    
    # return the non empty file
    else{
      return(data)
    }
  })
  
  ################# remove_strings ################# 
  # removes strings from file content and assigns the 'NAME' column as the row labels
  remove_strings<-function(x){
    nums <- sapply(x, is.numeric)
    y<- x[,nums]
    
    # try to find a column with title name
    name = 'NAME'
    tryCatch({
      nameRow<-x[,name]
      rownames(y) <-make.names(nameRow, unique=TRUE) #paste(rownames(y),nameRow) 
      }, 
      finally = {return(y)})
  }
  
  ################# get_data_matrix ################# 
  # returns file input data as a data matrix
  get_data_matrix<-function(){
    
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
    if(input$clusterMethod == 'none')
      FALSE
    else
      input$colv
  }
  
  ################# get_rowv #################
  # boolean, true if cluster row is selected in ui.r
  get_rowv <- function(){
    if(input$clusterMethod == 'none')
      FALSE
    else
      input$rowv
  }
  
  ################# get_dendrogram ################# 
  # returns 'both', 'column', 'row', or 'none'; depending on the slider settings in ui.r
  get_dendrogram <- function(){
    if(input$dendCol && input$dendRow)
      "both"
    else if(input$dendCol)
      "column"
    else if(input$dendRow)
      "row"
    else
      "none"
  }
  
  ################# get_dist #################
  # calculates a distance matrix 
  get_dist <- function(x){
  	
  	#if(input$distanceMethod == 'pearson'){
  	#	return(pearson.dist(x))
  	#}
  	#else{
  		return(dist(x, method = input$distanceMethod))
  	#}
  }
  
  ################# get_hclust #################
  # uses hclust to cluster data using get_dist distance matrix
  get_hclust <- function(x){

		write("get_dist x", stderr()) ################################################### DEBUG ##
		x <- get_dist(x)
		write(object.size(x), stderr()) ############################################# DEBUG ##
		
		write("mem_used", stderr()) ################################################### DEBUG ##
		write(mem_used(), stderr()) ############################################# DEBUG ##
		
		write("get_hclust x", stderr()) ################################################### DEBUG ##
		x <- hclust(x, method = input$clusterMethod)
		write(object.size(x), stderr()) ############################################# DEBUG ##
		
		write("mem_used", stderr()) ################################################### DEBUG ##
		write(mem_used(), stderr()) ############################################# DEBUG ##
		
  	return(x)
  }
  
  ################# get_table_data #################
  # clusters and returns data for displaying when table option is selected in ui.r
  get_table_data <- function(){
  	write("get_table_data", stderr()) ################################################### DEBUG ##
  	write("get_file", stderr()) ################################################### DEBUG ##
    data <- get_file()
  	write("get_colv/rowv", stderr()) ################################################### DEBUG ##
    colv <- get_colv()
    rowv <- get_rowv()
    
    if(is.null(data))
      return(NULL)
  	write("start cluster", stderr()) ################################################### DEBUG ##
    # cluster rows
    if(rowv){
    	write("table rowv", stderr()) ################################################### DEBUG ##
      row <- get_hclust(data)
      data <- data[row$order,]
    }
    
    # cluster cols
    if(colv){
    	write("table colv", stderr()) ################################################### DEBUG ##
      # only cluster cols with numbers
      nums <- sapply(data, is.numeric)
      numericCols <- data[,nums]
      nonNums <- !sapply(data, is.numeric)
      omittedCols <- data[,nonNums]
      
      col <- get_hclust(t(numericCols))
      
      write("cbind2", stderr()) ################################################### DEBUG ##
      data <- cbind2(omittedCols, numericCols[,col$order])
    }
  	write("return(data)", stderr()) ################################################### DEBUG ##
    return(data)
  }
  
  ################# get_heatmap ################# 
  # returns a heatmap created from the file input data
  get_heatmap<-function(){
    
		write("mem used start heatmap", stderr()) ##################################### DEBUG ##
		write(mem_used(),stderr()) #################################################### DEBUG ##

    # maximum number of nested expressions to be evaluated
    options(expressions = 500000)
    
    # get the data matrix to convert to heatmap
    heatmapDataMatrix <- get_data_matrix()
    colv <- get_colv()
    rowv <- get_rowv()
    dendrogram <- get_dendrogram()
    if(is.null(heatmapDataMatrix))
      return(NULL)
    
    # makes heatmap brighter when scale is 'none'
    # https://mintgene.wordpress.com/2012/01/27/heatmaps-controlling-the-color-representation-with-set-data-range/
    if(input$scale == "none"){
      quantile.range <- quantile(na.rm = TRUE, heatmapDataMatrix, probs = seq(0, 1, 0.01))
      breaks <- seq(quantile.range["5%"], quantile.range["95%"], 0.1)
      my_palette <- colorRampPalette(c(input$startColour, "black", input$endColour))
    }
    else{
      my_palette <- colorRampPalette(c(input$startColour, "black", input$endColour))(n = input$binSlider)
      breaks <- NULL
    }
		write("heatmap_data_matrix", stderr()) ######################################## DEBUG ##
		write(object.size(heatmapDataMatrix), stderr()) ############################### DEBUG ##

		write("mem used before cluster heatmap", stderr()) ############################ DEBUG ##
		write(mem_used(),stderr()) #################################################### DEBUG ##

		# cluster cols
    if(rowv){
			write("clustering rows", stderr()) ########################################## DEBUG ##

    	row <- as.dendrogram(get_hclust(heatmapDataMatrix))
    }
    else{
    	row <- FALSE
    }

    # cluster cols
    if(colv){
			write("clustering cols", stderr()) ########################################## DEBUG ##   	
      col <- as.dendrogram(get_hclust(t(heatmapDataMatrix)))
    }
    else{
    	col <- FALSE
    }

    # create the heatmap
    heatmap.2(heatmapDataMatrix,
              col=my_palette, scale=input$scale, na.color=input$missingDataColour,
              key=FALSE, symkey=FALSE, density.info="none", trace="none", 
              Rowv = row, Colv = col, dendrogram = dendrogram, 
              keysize=0.5, cexRow=input$cexRow, #cellnote=heatmapDataMatrix,
              main=input$imageTitle, xlab=input$xaxis, ylab=input$yaxis, 
              offsetCol = 0, offsetRow = 0, 
              margins=c(5,10), lhei=c(1,8), lwid=c(0.1,0.5), breaks = breaks 
              )
		write("mem used end heatmap", stderr()) ###################################### DEBUG ##
		write(mem_used(),stderr()) ################################################### DEBUG ##
    
		graphics.off()
  }

  ################# get_height ################# 
  # calculate the height based on matrix dimentions and current width
  get_height<-function(cond, x){
    x<-get_data_matrix()
    if(is.null(x))
      return(0)
    else{
      if(cond)
        size <- input$widthSlider/ncol(x) * nrow(x)
      else
        size <- input$heightSlider
      return(size)
    }
  }

  ########################################### OUTPUT ###########################################
  
  ################# Display Table ################# 
  output$dataTable <- renderDataTable({
    fileData <- get_table_data()
    write("renderTable", stderr()) ################################################### DEBUG ##
    if(is.null(fileData)){
      return(NULL)
    }
    return(fileData)
  })

  ################# Display Heatmap ################# 
  output$heatmap <- renderPlot(
      get_heatmap(), 
      height=reactive({get_height(input$previewFullSize)}), 
      width=reactive({input$widthSlider}))
   
  ################# Save Example File ################# 
  output$downloadExample <- downloadHandler(
    filename = "example.txt",
    content = function(file) {write.csv(read.csv(input$exampleFiles, header=TRUE, sep=input$sep), file)})

  ################# Save File As Table ################# 
  output$downloadTable <- downloadHandler(
    filename = "cluster_files.zip",
    
    content = function(file) {
    	data <- get_data_matrix()
    	fs <- c("text_file.txt")
    	write.table(get_table_data(), "text_file.txt", sep = input$sepSave)
    	
    	if(input$clusterMethod != 'none'){
    		fs <- c(fs, "cluster.cdt", "cluster.atr", "cluster.gtr")
    		r2cdt(hr=get_hclust(data), hc=get_hclust(t(data)),data=data,labels=FALSE,description=FALSE,file="cluster.cdt",dec='.')
    		r2atr(hc=get_hclust(t(data)),file="cluster.atr",distance=input$distanceMethod,dec='.',digits=5)
    		r2gtr(hr=get_hclust(data),file="cluster.gtr",distance=input$distanceMethod,dec='.',digits=5)
    	}
    	zip(zipfile=file, files=fs)
  	}
  )
  
  ################# Save File As Image ################# 
  output$downloadHeatmap <- downloadHandler(

    filename = reactive({paste("heatmap.", input$downloadFormat, sep="")}),
    
    content = function(file) {
      if(input$downloadFormat == "pdf"){
        pdf(file, width=input$widthSlider/72, height=get_height(input$downloadFullSize)/72)
      }
      else{
        png(file, units="in", width = input$widthSlider/72, height=get_height(input$downloadFullSize)/72, res=input$resSlider,type = "cairo")            
      }
  
      get_heatmap() 
      
    }) 
  })
