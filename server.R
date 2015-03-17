library(shiny)
library(gplots)
# setwd("~/bioin401")
# ~/bioin/program
# runApp("program")

# shinyapps::deployApp('~/bioin401')

shinyServer(function(input, output,session) {
  
  ########################################### FUNCTIONS ###########################################
  
  ################# get_file ################# 
  # retrieves the file input data
  get_file<-reactive({
   
    # input$file1 is NULL before upload
    inFile <- input$file1
    
    if (is.null(inFile)){
      return(NULL)
    }
    
    # file has been uploaded
    # data frame with 'name', 'size', 'type', and 'datapath' columns. 
    # 'datapath' column contains the local filenames where the data can be found.
    else{  
      x <- read.csv(inFile$datapath, header=TRUE, sep=input$sep)
      y <- remove_strings(x)
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
    
    nonNums <- !sapply(x, is.numeric)
    omittedCols <- x[,nonNums]
    
    # try to find a column with title name
    name = 'NAME'
    tryCatch({
      nameRow<-x[,name]
      rownames(y) <- make.names(nameRow, unique=TRUE)
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
  
  ################# data_analysis #################
  data_analysis <- function(x){
    if(input$clusterMethod == "kmeans"){
      set.seed(1)
      km<- kmeans(x,input$n) # determine how many cluster you want, I specify 2 here
      
      m.kmeans<- cbind(x, km$cluster) # combine the cluster with the matrix
      
      dim(m.kmeans)
      # [1] 903 602
      # the last column is 602
      z <- ncol(x)-1
      o<- order(m.kmeans[,z]) # order the last column
      
      m.kmeans<- m.kmeans[o,] # order the matrix according to the order of the last column
      
    }
    else
      x
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
  
  ################# get_heatmap ################# 
  # returns a heatmap created from the file input data
  get_heatmap<-function(){
    
    # get the data matrix to convert to heatmap
    heatmapDataMatrix <- get_data_matrix()
    # heatmapDataMatrix <- data_analysis(heatmapDataMatrix)
    colv <- get_colv()
    rowv <- get_rowv()
    dendrogram <- get_dendrogram()
    
    if(is.null(heatmapDataMatrix))
      return(NULL)
    # customize colors
    my_palette <- colorRampPalette(c(input$startColour, "black", input$endColour))(n = input$binSlider)
    
    # create the heatmap
    heatmap.2(heatmapDataMatrix,
              col=my_palette, scale=input$scale,
              key=FALSE, symkey=FALSE, density.info="none", trace="none", 
              Rowv = rowv, Colv = colv, dendrogram = dendrogram, 
              hclustfun=function(c){
                if(input$clusterMethod == "none") # || input$clusterMethod == "kmeans")
                  return("none")
                else
                  return(hclust(c, method=input$clusterMethod))
                }, 
              distfun = function(c){dist(c, method=input$distanceMethod)},
              keysize=0.5, cexRow=1, 
              main=input$imageTitle, xlab=input$xaxis, ylab=input$yaxis
              #lmat = rbind(c(0,3),c(2,1),c(0,4)),
              #lwid = c(1.5,4),
              #lhei = c(1.5,4,1))
              )
    graphics.off()
    #dev.off()
  }

  ################# get_height ################# 
  get_height<-function(cond){
    x<-get_data_matrix()
    if(is.null(x))
      return(0)
    else{
      if(cond == 1)
        size <- nrow(x)*ncol(x)
      else
        size <- input$heightSlider
      return(size)
    }
  }

  ########################################### OUTPUT ###########################################
  
  ################# Table ################# 
  output$dataTable <- renderTable({
      get_file() 
  })

  ################# Heatmap ################# 
  output$heatmap <- renderPlot({
      get_heatmap()
    }, height=reactive({get_height(input$previewFullSize)}))

  ################# Save File ################# 
  output$downloadData <- downloadHandler(
    
    filename = "heatmapNew",
    
    content = function(file) {
        png(file,width=800, height=get_height(input$downloadFullSize))
      
        heatmapData <- get_heatmap()
        
    }, 
    contentType="image/png")
  })
