library(shiny)
library(gplots)
library(ReorderCluster)
# setwd("~/")
# runApp("bioin401")

# shinyapps::deployApp('~/bioin401')
# shinyapps::terminateApp('bioin401')
# https://www.shinyapps.io/admin/#/dashboard

#max upload size is 10MB
options(shiny.maxRequestSize=10*1024^2)

shinyServer(function(input, output,session) {

  ########################################### FUNCTIONS ###########################################
  
  ################# get_file ################# 
  # retrieves the file input data
  get_file<-reactive({
    sep=input$sep
    # input$file is NULL before upload
    inFile <- input$file
    path <- inFile$datapath

    if(input$chooseInput == 'examples'){
      path <- input$exampleFiles
      sep <- "\t"
    }
    
    if (is.null(path)){
      return(NULL)
    }

    # file has been uploaded
    # data frame with 'name', 'size', 'type', and 'datapath' columns. 
    # 'datapath' column contains the local filenames where the data can be found.
    else{  
      x <- read.csv(path, header=TRUE, sep=sep)
      if(input$display == "heatmap")
        y <- remove_strings(x)
      else
        y <- x
    }
    
    if (is.null(y)){
      return(NULL)
    }
    
    # return the non empty file
    else{
      return(y)
    }
  })
  
  ################# remove_strings ################# 
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
  get_colv <- function(){
    if(input$clusterMethod == 'none')
      FALSE
    else
      input$colv
  }
  
  ################# get_rowv #################
  get_rowv <- function(){
    if(input$clusterMethod == 'none')
      FALSE
    else
      input$rowv
  }
  
  ################# get_dendrogram ################# 
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
  
  get_table_data <- function(){
    
    data <- get_file()
    colv <- get_colv()
    rowv <- get_rowv()
    
    if(is.null(data))
      return(NULL)
    
    # cluster rows
    if(rowv){
      row_dist_matrix <- dist(data, method = input$distanceMethod)
      row <- hclust(row_dist_matrix, method = input$clusterMethod)
      data <- data[row$order,]
    }
    else
      row <- FALSE
    
    # cluster cols
    if(colv){
      col_dist_matrix <- dist(t(data), method = input$distanceMethod)
      col <- hclust(col_dist_matrix, method = input$clusterMethod)
      data <- data[,col$order]
    }
    return(data)
  }
  
  get_table_data2 <- function(){
    x<-get_file()
    y<-x
    
    if(get_rowv()){
      row_dist_matrix <- dist(y, method = input$distanceMethod)
      row <- hclust(row_dist_matrix, method = input$clusterMethod)
      #row.order <- hclust(dist(y,method=input$distanceMethod),method=input$clusterMethod)$order
      y<-x[row$order,]
    }
    if(get_colv()){
      nums <- sapply(x, is.numeric)
      z<- x[,nums]
      col.order <- hclust(dist(t(z),method=input$distanceMethod),method=input$clusterMethod)$order
      
      #nonNums <- !sapply(y, is.numeric)
      #omittedCols <- y[,nonNums]
      #y<-cbind2(omittedCols, z[,col.order])
    }
    
    return(y)
  }
  
  ################# get_heatmap ################# 
  # returns a heatmap created from the file input data
  get_heatmap<-function(){
    
    # get the data matrix to convert to heatmap
    heatmapDataMatrix <- get_data_matrix()
    colv <- get_colv()
    rowv <- get_rowv()
    dendrogram <- get_dendrogram()
    
    if(is.null(heatmapDataMatrix))
      return(NULL)
    
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
   
    # cluster rows
    if(rowv){
      row_dist_matrix <- dist(heatmapDataMatrix, method = input$distanceMethod)
      row <- hclust(row_dist_matrix, method = input$clusterMethod)
      #heatmapDataMatrix <- heatmapDataMatrix[row$order,]
      row <- as.dendrogram(row)
    }
    else
      row <- FALSE
    
    # cluster cols
    if(colv){
      col_dist_matrix <- dist(t(heatmapDataMatrix), method = input$distanceMethod)
      col <- hclust(col_dist_matrix, method = input$clusterMethod)
      #heatmapDataMatrix <- heatmapDataMatrix[,col$order]
      col <- as.dendrogram(col)
    }
    else
      col <- FALSE

    # maximum number of nested expressions to be evaluated
    options(expressions = 500000)
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
    graphics.off()
  }

  ################# get_height ################# 
  get_height<-function(cond){
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
  
  ################# Table ################# 
  output$dataTable <- renderTable({
    fileData <- get_table_data()
    if(is.null(fileData)){
      return(NULL)
    }
    y <- fileData
  })

  ################# Heatmap ################# 
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
    filename = "data.txt",
    content = function(file) {write.table(get_table_data(), file, sep = input$sepSave)}
    )
  
  ################# Save File As Image ################# 
  output$downloadHeatmap <- downloadHandler(

    filename = reactive({paste("heatmap.", input$downloadFormat, sep="")}),
    
    content = function(file) {
      if(input$downloadFormat == "pdf"){
        pdf(file, width=input$widthSlider, height=get_height(input$downloadFullSize), paper="a4r")
      }
      else{
        png(file, units="in", width = input$widthSlider/72, height=get_height(input$downloadFullSize)/72, res=input$resSlider)
      }
      
      get_heatmap() 
      
    }) 
  })
