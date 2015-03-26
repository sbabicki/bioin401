library(shiny)
library(gplots)
# setwd("~/bioin401")
# ~/bioin/program
# runApp("program")

# shinyapps::deployApp('~/bioin401')
# shinyapps::terminateApp('bioin401')
# https://www.shinyapps.io/admin/#/dashboard

shinyServer(function(input, output,session) {
  
  ########################################### FUNCTIONS ###########################################
  
  ################# get_file ################# 
  # retrieves the file input data
  get_file<-reactive({
    
    # input$file1 is NULL before upload
    inFile <- input$file1

    path <- inFile$datapath
    
    if(input$chooseInput == 'examples'){
      path <- input$exampleFiles
    }
    
    if (is.null(path)){
      return(NULL)
    }

    # file has been uploaded
    # data frame with 'name', 'size', 'type', and 'datapath' columns. 
    # 'datapath' column contains the local filenames where the data can be found.
    else{  
      x <- read.csv(path, header=TRUE, sep=input$sep)
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
    
    nonNums <- !sapply(x, is.numeric)
    omittedCols <- x[,nonNums]
    
    # try to find a column with title name
    name = 'NAME'
    tryCatch({
      nameRow<-x[,name]
      nameRow <- paste(substr(nameRow, 0, 7),"...")
      rownames(y) <- paste(rownames(y),nameRow) #make.names(nameRow, unique=TRUE)
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
  
  get_table_data <- function(x){
#    col <- get_colv()
#    row <- get_rowv()
#    if(input$clusterMethod != "none"){
#     if(col){
 #       col_dist <- dist(x, method=input$distanceMethod)
 #       col_cluster <- hclust(col_dist, method=input$clusterMethod)
 #     }       
#      if(row){
#       row_dist <- dist(x, method=input$distanceMethod)
#        row_cluster <- hclust(row_dist, method=input$clusterMethod)
#      } 
#    }

  ## Row clustering (adjust here distance/linkage methods to what you need!)
  hr <- hclust(as.dist(1-cor(t(x), method="pearson")),
             method="complete")

  ## Column clustering (adjust here distance/linkage methods to what you need!)
  hc <- hclust(as.dist(1-cor(x, method="spearman")), method="complete")
  ## Return matrix with row/column sorting as in heatmap
  x[rev(hr$labels[hr$order]), hc$labels[hc$order]]


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
    # customize colors
    my_palette <- colorRampPalette(c(input$startColour, "black", input$endColour))(n = input$binSlider)
   
    # maximum number of nested expressions to be evaluated
    options(expressions = 10000)
    
    # create the heatmap
    heatmap.2(heatmapDataMatrix,
              col=my_palette, scale=input$scale, 
              key=FALSE, symkey=FALSE, density.info="none", trace="none", 
              Rowv = rowv, Colv = colv, dendrogram = dendrogram, 
              hclustfun=function(c){
                if(input$clusterMethod == "none")
                  return("none")
                else
                  return(hclust(c, method=input$clusterMethod))
                }, 
              distfun = function(c){dist(c, method=input$distanceMethod)},
              keysize=0.5, cexRow=1, 
              main=input$imageTitle, xlab=input$xaxis, ylab=input$yaxis, 
              margins = c(5,7), offsetCol = 0, offsetRow = 0
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

  get_content_type <- function(cont) {
    if(cont == "pdf"){
      print("application/pdf")
      return("application/pdf")
    } 
    else{
      print(paste("image/", cont, sep=""))
      return(paste("image/", cont, sep="")  )
    }
  }

  ########################################### OUTPUT ###########################################
  
  ################# Table ################# 
  output$dataTable <- renderTable({
   
   y <- data.matrix(get_file())
    #te <- heatmap.2(y)
  #  data.frame(y[rev(te$rowInd), te$colInd])
    ## Some input sample matrix
   # y <- matrix(rnorm(50), 10, 5, dimnames=list(paste("g", 1:10, sep=""), paste("t", 1:5, sep="")))
    
    
    ## Run heatmap.2 on this matrix
#    library(gplots)
 #   test <- heatmap.2(y)
  #  y[rev(test$rowInd), test$colInd]


  })

  ################# Heatmap ################# 
  output$heatmap <- renderPlot({
      get_heatmap()
    }, height=reactive({get_height(input$previewFullSize)}), width=reactive({input$widthSlider}))

  output$downloadExample <- downloadHandler(
    filename = "example.txt",
    content = function(file) {write.csv(read.csv(input$exampleFiles, header=TRUE, sep=input$sep), file)})

  ################# Save File ################# 
  output$downloadData <- downloadHandler(

    filename = reactive({paste("heatmap.", input$downloadFormat, sep="")}),
    
    content = function(file) {
      if(input$downloadFormat == "pdf"){
        pdf(file,width=input$widthSlider, height=get_height(input$downloadFullSize))
      }
      else if(input$downloadFormat == "jpeg"){
        return(NULL)
      }
      else{
        png(file,width=input$widthSlider, height=get_height(input$downloadFullSize))
      }
      
      heatmapData <- get_heatmap() 
      #print(input$downloadFormat)
    }, 
    
    contentType=reactive({get_content_type(input$downloadFormat)})) 
      
  })
