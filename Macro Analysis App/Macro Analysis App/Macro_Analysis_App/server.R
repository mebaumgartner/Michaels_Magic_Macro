
#Create data_summary variables for standard deviation
data_summary <- function(x) {
    m <- mean(x)
    ymin <- m
    ymax <- m+sd(x)
    return(c(y=m,ymin=m,ymax=ymax))
}

data_summary2 <- function(x) {
    m <- median(x)
    ymin <- m
    ymax <- m+sd(x)
    return(c(y=m,ymin=m,ymax=m))
}

############################################################################################################################################################
############################################################################################################################################################
#############################################################################################################################################################

## This is the app server, i.e. the part of the app that does all the background analysis  

server <- function(input, output) {
    
    
    #Assign some reactive values to hold outputs and key variables
    values = reactiveValues()
    valuesRestore <- reactiveValues()
    values$choices <- c("First, analyze your data!")
    values$rawChoices <- c("First, analyze your data!")
    values$genAssign <-data.frame()
    values$statistics <- data.frame()
    values$loaded <- 0
    
    output$plotso <- renderPlot(ggplot())
    output$plotso2 <- renderPlot(ggplot())
    
    
    output$statsTable <- renderTable(values$statistics, hover=TRUE, border=TRUE, spacing="s", digits=5, align ="l")
    
    #Create a UI based on the analyzed values put in
    output$plotSelection = renderUI({
        mydata = values$choices
        selectInput('plotter', 'Select plot to generate', mydata)
    })
    output$stats = renderUI({
        if ((input$plotType == "Scatter Plot") | (input$plotType == "Scatter Plot w/ Regression Line")){
            statsOptions <- c("Pearson r Correlation", "Kendall Tau Correlation", "Spearman's Rho Correlation", "Hotellings T-Squared Test")
        } else {
            statsOptions <- c("wilcox.test","t.test", "kruskal.test", "anova") 
        }
        selectInput('statsType', 'Select statistical test', statsOptions)
    })
    
    
    # #Create our regression variable options
     output$regressionSelect1 = renderUI({
        if (input$linkFunction == "Logistic"){
            regTitle <- "Select Negative Dependent Variable:"
            regOptions1 <- values$choices
            regOptions1 <- regOptions1[grepl("Number.of.Center.Cells|Number.of.Border.Cells|Total.Number.of.Cells|Number.of.Clones|Total.Caspase.Cell.Count|Center.Caspase.Cell.Count|Border.Caspase.Cell.Count|NonCaspase.Cells.in.Border|NonCaspase.Cells.in.Center", regOptions1)]
            
        } else if (input$linkFunction == "Poisson"){
            regTitle <-"Select Dependent variable"
            regOptions1 <- values$choices
            regOptions1 <- regOptions1[grepl("Number.of.Center.Cells|Number.of.Border.Cells|Total.Number.of.Cells|Number.of.Clones|Total.Caspase.Cell.Count|Center.Caspase.Cell.Count|Border.Caspase.Cell.Count|NonCaspase.Cells.in.Border|NonCaspase.Cells.in.Center", regOptions1)]
        } else {
            regTitle <- "Select Dependent variable"
            regOptions1 <- values$choices
        }
        selectInput('regressionParameter1', regTitle, regOptions1)
     })
    
    output$regressionSelect2 = renderUI({
        validate(
            need(input$linkFunction == "Logistic", "Select logistic analysis")
        )
        regOptions2 <- values$choices
        regOptions2 <- regOptions2[grepl("Number.of.Center.Cells|Number.of.Border.Cells|Total.Number.of.Cells|Number.of.Clones|Total.Caspase.Cell.Count|Center.Caspase.Cell.Count|Border.Caspase.Cell.Count|NonCaspase.Cells.in.Border|NonCaspase.Cells.in.Center", regOptions2)]
        
        selectInput('regressionParameter2', 'Select Positive Dependent Variable:', regOptions2)
    })
    
    output$factorCheckboxes = renderUI({
        factorChoices <- values$rawChoices
        factorValues <- c(1:length(factorChoices))
        checked <- grep("GeneWeek|Pouch.Volume|Percent.Clone.Coverage.of.Pouch", factorChoices)
        checkboxGroupInput('factorCheckboxes', 'Select variables to include as factors:', choiceNames = factorChoices, choiceValues = factorValues, selected = checked)
    })
    
    
    
    
    
    
    output$downloadData <- downloadHandler(
        
        
        
        filename = function() {
            paste("data-", Sys.Date(), ".csv", sep="")
        },
        content = function(file) {
            write.csv(values$wwda2, file)
        })
    
    
    
    output$plotPaired = renderUI({
        validate(
            need((input$plotType == "Paired Box Plot") | (input$plotType == "Scatter Plot") | (input$plotType == "Scatter Plot w/ Regression Line"), "Needs two variables selection")
        )
        mydata2 = values$choices
        selectInput('plotterpaired', 'Select paired dataset', mydata2)
    })
    
    #input$excludeChoice, input$excludeOperator,input$excludeValue
    
    output$exclusioninput1 = renderUI({
        validate(
            need(input$excluded == TRUE, "Select exclusion")
        )
        mydata3 = values$rawChoices
        selectInput('excludeChoice', 'Select data column to exclude by:', mydata3)
    })
    
    output$exclusioninput2 = renderUI({
        validate(
            need(input$excluded == TRUE, "Select exclusion")
        )
        selectInput('excludeOperator', 'Operation:', choices = c(">", ">=", "<", "<=", "==", "!="))
    })
    
    output$exclusioninput3 = renderUI({
        validate(
            need(input$excluded == TRUE, "Select exclusion")
        )
        numericInput('excludeValue', 'Specify comparison value:', 0)
    })
    
    valuesRestore <- values
    
    
    
    #Here we generate the reactive input table for genotype assignments
    
    
    observeEvent(input$saveBtn, {
      
      
        wda <- data.frame(
            Image.Name = c(1,2,3,4,5)
            
        )
        
        tryCatch(
            # This is what I want to do...
            {
                values <- valuesRestore
                wda <- rbindlist(lapply(input$csvs$datapath, read.csv), use.names = TRUE, fill = TRUE)
                Image.Name <- unique(wda$Image.Name)
                values$genAssign<- data.frame(Image.Name)
                
                values$genAssign$week <- "1"
                values$genAssign$Gene.Name <- "1"
            },
            # ... but if an error occurs, tell me what happened: 
            error=function(error_message) {
                message("No data file loaded")
                return(NA)
            }
        )
        
        
        
        
        tryCatch(
            # This is what I want to do...
            {
                wda <- rbindlist(lapply(input$csvs$datapath, read.csv), use.names = TRUE, fill = TRUE)
                Image <- unique(wda$Image)
                values$genAssign<- data.frame(Image)
                values$genAssign$week <- "Specify experiment/grouping"
                values$genAssign$Gene.Name <- "Specify genotype"
            },
            # ... but if an error occurs, tell me what happened: 
            error=function(error_message) {
                message("No data file loaded")
                return(NA)
            }
        )
        
        
    })
    output$table <- renderRHandsontable({
        rhandsontable(values$genAssign) 
    })
    
    observeEvent (input$loadBtn, {
        values$loaded <-1
        values$control <- toString(input$reference)
        values$genAssign <- hot_to_r(input$table)
    })
    
    
    ############################################################################################################################
    ##################################################### Re-Load analyzed data ###################################################
    ############################################################################################################################
    
    
    
    analysis <- observeEvent(input$go,{
      
        #Reset all our outputs so we don't get confused
        output$statsTable <- renderTable(data.frame(), hover=TRUE, border=TRUE, spacing="s", digits=5, align ="l")
        output$statsTableP <- renderTable(data.frame(), hover=TRUE, border=TRUE, spacing="s", digits=5, align ="l")
        output$contents <- renderTable(data.frame(), hover=TRUE, border=TRUE, spacing="s", digits=5, align ="l")
        output$deathTable <- renderTable(data.frame(), hover=TRUE, border=TRUE, spacing="s", digits=5, align ="l")
        output$statsHeader <- renderText("")
        output$statsHeaderP <- renderText("")
        output$normality <- renderTable(data.frame(), hover=TRUE, border=TRUE, spacing="s", digits=5, align ="l")
        output$normality2 <- renderTable(data.frame(), hover=TRUE, border=TRUE, spacing="s", digits=5, align ="l")
        output$normalityHeader <- renderText("")
        output$normalityHeader2 <- renderText("")
        output$fligner <- renderTable(data.frame())
        output$leveneHeader <- renderText("")
        output$fishyPrint <- renderText("")
        output$siggy <- renderTable(data.frame(), hover=TRUE, border=TRUE, spacing="s", digits=5, align ="l")
        output$fishyCounty <- renderTable(data.frame(), hover=TRUE, border=TRUE, spacing="s", digits=5, align ="l")
        output$plotso <- renderPlot(ggplot())
        output$plotso2 <- renderPlot(ggplot())
        
    
        tryCatch(
          {
        
        #Read dataset csv files to the variable 'wda'
        wwda2 <- rbindlist(lapply(input$csvs$datapath, read.csv), use.names = TRUE, fill = TRUE)
        
        
        #Check what type of dataframe has been uploaded
        okayGo <- FALSE
        for (i in colnames(wwda2)){
          if(grepl("CloneID", i, fixed = TRUE)) {
            okayGo <- TRUE
            colnames(wwda2)[1] <-"File"
          }
          
          if(grepl("Pouch.Volume.", i, fixed = TRUE)) {
            colnames(wwda2)[1] <-"File"
            okayGo <- TRUE
            }
        }
        
        #Here we make sure the correct steps have been taken (i.e. right buttons pressed, right files uploaded)
        validate(
            need(okayGo == TRUE, "Please load a previously analyzed data table")
        )

        wwda2<- as.data.frame(wwda2)
        #Output data into analyzed data tab
        output$contents <- renderTable(wwda2, hover=TRUE, border=TRUE, spacing="s", digits=5, align ="l")
        values$rawChoices <- colnames(wwda2)
        values$choices <- colnames(wwda2)
        
        #Backup data in reactive values
        values$wwda2 <- wwda2
        values$wwdaArchive <- wwda2
        
        #Output table
        output$contents <- renderTable(wwda2, hover=TRUE, border=TRUE, spacing="s", digits=5, align ="l")
      
        
        
        TrueCasCounts <- 0
        TrueNonCasCounts <- 0
        genotypeNames <- c()
    
        for (i in colnames(wwda2)){
          if(grepl("Number.of.Center.Cells", i, fixed = TRUE)){
            TrueNonCasCounts <- 1
            genotypeNames <- c(genotypeNames, gsub("Number.of.Center.Cells.", "", i ))
          }
          if(grepl("Border.Caspase.Cell.Count", i, fixed = TRUE)){
            TrueCasCounts <- 1
            genotypeNames <- c(genotypeNames, gsub("Border.Caspase.Cell.Count.", "", i ))
          }
        }
    
        genotypeNames <- unique(genotypeNames)
        
        
        
         tryCatch(
        #Restore caspase counts
          {
        
        #If we have the actual counts from the macro, we prefer to use those. Otherwise, we revert to the mathematical approximation
        
        if (TrueCasCounts == 1 && TrueNonCasCounts == 1){
          
          for (i in genotypeNames){
    
            

            queryString <- paste(i, "GeneWeek", sep = "|")
            genotypeVals <- wwda2[ , grepl( queryString , names( wwda2) ) ]
            

            headers <- colnames(genotypeVals)
            newHeaders <- c()
            for (z in headers) {
              query <- paste(".", i, sep="")
              
              newHead <- gsub(query, "", z)
              
              newHeaders <- c(newHeaders, newHead)
            }
            
            colnames(genotypeVals) <- newHeaders
            
   
        
          
          borderCells <- aggregate(genotypeVals$NonCaspase.Cells.in.Border, list(genotypeVals$GeneWeek), sum)
          
          centerCells <- aggregate(genotypeVals$NonCaspase.Cells.in.Center, list(genotypeVals$GeneWeek), sum)
          
          borderCasCells <- aggregate(genotypeVals$Border.Caspase.Cell.Count, list(genotypeVals$GeneWeek), sum)
          
          centerCasCells <- aggregate(genotypeVals$Center.Caspase.Cell.Count, list(genotypeVals$GeneWeek), sum)
          

          #Format these values into a table suitable for a fisher test
          deathTable <- data.frame(
              Genotype = borderCells[,1],
              NotCasCellsInBorder = borderCells[,2],
              CasCellsInBorder = borderCasCells[,2],
              NotCasCellsInCenter = centerCells[,2],
              CasCellsInCenter = centerCasCells[,2]
          )

          values$deathTable <- deathTable
          output$deathTable <- renderTable(values$deathTable, hover=TRUE, border=TRUE, spacing="s", digits=5, align ="l")
          }
        }
        
        
          },
         error=function(error_message) {
            message("Unable to prepare death table")
            return(NA)
           }
         )
        },
          error=function(error_message) {
             message("Unable to re-analyze data")
            return(NA)
           }
         )
        
    }
    )
    
    
    ############################################################################################################################
    ##################################################### Clone Tracking Analysis ###################################################
    ############################################################################################################################
    
    
    analysis <- observeEvent(input$go,{
        
        
      tryCatch(
        {    
        
        
        #Read dataset csv files
        cta <- rbindlist(lapply(input$csvs$datapath, read.csv), use.names = TRUE, fill = TRUE)
        
        #Check what type of dataframe has been uploaded
        okayGo <- FALSE
        for (i in colnames(cta)){
          if(grepl("Clone.ID", i, fixed = TRUE)) {okayGo <- TRUE}
        }
    
        
        #Only allow analysis to proceed if the correct filetype has been loaded and genotype assignments have been made
        
        validate(
            need(cta$Clone.ID, "Please load a clone tracking analysis file and assign genotypes"),
            need(okayGo == TRUE, "Please load a clone tracking analysis file and assign genotypes"),
            need(values$loaded == 1, "Load genotype assignments first!")
        )
        
        #Read genotype assignment csv file
        trad  <- values$genAssign
        
        #Merge the CSV file input with the genotype assignments
        ccta <- merge(x = cta, y = trad)
        
        #Make a unique identifier for a given clone at a given z level
        ccta2 <- cbind( CloneZName = paste(ccta$Image.Name ,ccta$week, ccta$Clone.ID , ccta$Z.level) , ccta )
        
        #Determine the height of the clone based on the minimum and maximum z-level values
        tryCatch(
            # This is a try/catch function. I use this a lot to account for how many possible data-tables this app can see
            {
                cctz <- cbind( CloneZID = paste(ccta$Image.Name ,ccta$week, ccta$Clone.ID) , ccta )
                MAX <- aggregate(cctz$Z.level, list(cctz$CloneZID), max)
                MIN <- aggregate(cctz$Z.level, list(cctz$CloneZID), min)
                Diff <- MAX[,2] - MIN[,2]
                Diff <- Diff + 1
            },
            # ... but if an error occurs, tell me what happened: 
            error=function(error_message) {
                message("old macro")
                return(NA)
            }
        )

        
        #Condense the data down, extracting unique gene ID's and summing areas. THis collates all the unique z-level information down to individual clone information
        ccta2sum <- data.frame(
            
            GeneName =  as.character(tapply(as.character(ccta2$Gene.Name) , ccta2$CloneZName , unique)),
            CloneZName = as.character(tapply(as.character(ccta2$CloneZName) , ccta2$CloneZName , unique)),
            week =  as.character(tapply(as.character(ccta2$week) , ccta2$CloneZName , unique)),
            Genotype = as.character(tapply(as.character(ccta2$Clone.Genotype) , ccta2$CloneZName , unique )),
            Clone.ID = as.character(tapply(as.character(ccta2$Clone.ID) , ccta2$CloneZName , unique)),
            Image.Name = as.character(tapply(as.character(ccta2$Image.Name) , ccta2$CloneZName , unique)),
            Z.level = as.character(tapply(as.character(ccta2$Z.level) , ccta2$CloneZName , unique)),
            Clone.Area = tapply(ccta2$Clone.Area , ccta2$CloneZName , sum),
            Border.Area = tapply(ccta2$Border.Area , ccta2$CloneZName , sum),
            Center.Area = tapply(ccta2$Center.Area , ccta2$CloneZName , sum)
            
        )
        
        
      
        ccta3 <- ccta2sum
        #Add in the caspase data, if applicable
        tryCatch(
            {
              if("Caspase.Coverage" %in% colnames(ccta2))
              {
                ccta3 <- cbind(ccta3, Caspase.Coverage = tapply(ccta2$Caspase.Coverage , ccta2$CloneZName , sum))
                ccta3 <- cbind(ccta3, Caspase.Border = tapply(ccta2$Caspase.Border , ccta2$CloneZName , sum))
                ccta3 <- cbind(ccta3, Caspase.Center = tapply(ccta2$Caspase.Center , ccta2$CloneZName , sum))
                }
                
            },
            # ... but if an error occurs, tell me what happened: 
            error=function(error_message) {
                message("no caspase")
                return(NA)
            }
        )
        
        
        #Get the overall height of the disc, if applicable
        tryCatch(
            # This is what I want to do...
            {
                ccta3 <- cbind(ccta3, Disc.Height = tapply(ccta2$Pouch.Height , ccta2$CloneZName , unique))
            },
            # ... but if an error occurs, tell me what happened:
            error=function(error_message) {
                message("old macro 1")
                return(NA)
            }
        )
        ccta3 <- cbind(ccta3, CloneName = paste(ccta2sum$Image.Name , ccta2sum$Clone.ID) )
        
        
        #Sum the integrated density values (fluorescence intensities non normalized to area)
        tryCatch(
            {             
              if("Clone.Fluorescence" %in% colnames(ccta2))
              {
                #sum integrated fluorescence density
                ccta3 <- cbind(ccta3, Clone.Fluorescence = tapply(ccta2$Clone.Fluorescence , ccta2$CloneZName , sum))
                ccta3 <- cbind(ccta3, Border.Fluorescence = tapply(ccta2$Border.Fluorescence , ccta2$CloneZName , sum))
                ccta3 <- cbind(ccta3, Center.Fluorescence = tapply(ccta2$Center.Fluorescence , ccta2$CloneZName , sum))
                }
            },
            # ... but if an error occurs, tell me what happened:
            error=function(error_message) {
                message("No fluorescence")
                return(NA)
            }
        )      
        
        
        #This is the skeleton of our final output table. We will add other values, such as they are applicable based on the analysis the user has run
        ccta3sum <- data.frame(
            
            GeneName =  tapply(as.character(ccta3$GeneName) , ccta3$CloneName , unique),
            CloneName = tapply(as.character(ccta3$CloneName) , ccta3$CloneName , unique),
            week =  tapply(as.character(ccta3$week) , ccta3$CloneName , unique),
            CloneID = tapply(as.character(ccta3$Clone.ID) , ccta3$CloneName , unique),
            Genotype = tapply(as.character(ccta3$Genotype) , ccta3$CloneName , unique ),
            Clone.Volume = tapply(ccta3$Clone.Area , ccta3$CloneName , sum),
            Clone.Border.Volume. = tapply(ccta3$Border.Area , ccta3$CloneName , sum),
            Clone.Center.Volume = tapply(ccta3$Center.Area , ccta3$CloneName , sum)
            
        )
        

        
        ccta3sumZ2 <- ccta3sum
        tryCatch(
            # accounts for a change in format between older macro and newer macro
            {
                ccta3sumZ2 <- cbind(ccta3sum, Pouch.Volume = tapply(ccta3$Pouch.Area, ccta3$CloneName, unique) * tapply(ccta3$Disc.Height, ccta3$CloneName, unique))
            },
            # ... but if an error occurs, tell me what happened:
            error=function(error_message) {
                message("old macro 2")
                return(NA)
            }
        )
        
        tryCatch(
            #Add caspase data if applicable
            {
              if("Caspase.Coverage" %in% colnames(ccta3))
              {
                ccta3sumZ2 <- cbind(ccta3sumZ2, Caspase.Volume = tapply(ccta3$Caspase.Coverage , ccta3$CloneName , sum))
                ccta3sumZ2 <- cbind(ccta3sumZ2, Caspase.in.Border.Volume = tapply(ccta3$Caspase.Border , ccta3$CloneName , sum))
                ccta3sumZ2 <- cbind(ccta3sumZ2, Caspase.in.Center.Volume = tapply(ccta3$Caspase.Center , ccta3$CloneName , sum))
                ccta3sumZ2 <- cbind(ccta3sumZ2, Percent.Caspase.Coverage.of.Clone = tapply(ccta3$Caspase.Coverage , ccta3$CloneName , sum) *100/ tapply(ccta3$Clone.Area , ccta3$CloneName , sum))
                ccta3sumZ2 <- cbind(ccta3sumZ2, Percent.Caspase.Coverage.of.Border = tapply(ccta3$Caspase.Border , ccta3$CloneName , sum) *100/ tapply(ccta3$Border.Area , ccta3$CloneName , sum))
                ccta3sumZ2 <- cbind(ccta3sumZ2, Percent.Caspase.Coverage.of.Center = tapply(ccta3$Caspase.Center , ccta3$CloneName , sum) *100/ tapply(ccta3$Center.Area , ccta3$CloneName , sum))
                ccta3sumZ2 <- cbind(ccta3sumZ2, Competitive.index = c( tapply(ccta3$Caspase.Center , ccta3$CloneName , sum) / tapply(ccta3$Center.Area , ccta3$CloneName , sum) )
                                    /
                                        c( tapply(ccta3$Caspase.Border , ccta3$CloneName , sum) / tapply(ccta3$Border.Area , ccta3$CloneName , sum) ))
                ccta3sumZ2 <- cbind(ccta3sumZ2, log2.Competitive.index = log(ccta3sumZ2$Competitive.index, 2))
                }
            },
            # ... but if an error occurs, tell me what happened: 
            error=function(error_message) {
                message("no caspase")
                return(NA)
            }
        )
        

        
        
        tryCatch(
            #Add fluorescence data if applicable
            {
              if("Clone.Fluorescence.IntDen" %in% colnames(ccta3))
              {                
                ccta3sumZ2 <- cbind(ccta3sumZ2,  Mean.Fluorescence.of.Clone = tapply(ccta3$Clone.Fluorescence.IntDen , ccta3$CloneName , sum)/ tapply(ccta3$Clone.Area , ccta3$CloneName , sum))
                ccta3sumZ2 <- cbind(ccta3sumZ2,  Mean.Fluorescence.of.Border = tapply(ccta3$Border.Fluorescence.IntDen , ccta3$CloneName , sum) / tapply(ccta3$Border.Area , ccta3$CloneName , sum))
                ccta3sumZ2 <- cbind(ccta3sumZ2,  Mean.Fluorescence.of.Center = tapply(ccta3$Center.Fluorescence.IntDen , ccta3$CloneName , sum) / tapply(ccta3$Center.Area , ccta3$CloneName , sum))
                
              }
           
             },
            # ... but if an error occurs, tell me what happened: 
            error=function(error_message) {
                message("no fluorescence")
                return(NA)
            }
        )  
        
        tryCatch(
            # This is what I want to do...
            {
              if("Clone.Volume" %in% colnames(ccta3sumZ2))
              {  
                clonepercent = ccta3sumZ2$Clone.Volume*100 /ccta3sumZ2$Pouch.Volume
                ccta3sumZ2 <- cbind(ccta3sumZ2, Number.of.Z.Slices = Diff)
                ccta3sumZ2 <- cbind(ccta3sumZ2, Clone.Volume.Normalized.to.Pouch= clonepercent)
                }
            },
            # ... but if an error occurs, tell me what happened:
            error=function(error_message) {
                message("old output")
                return(NA)
            }
        )
        
        

        # # # # # Remove na values # # # # # #
        data_subset <- ccta3sumZ2[ , c("CloneName")]
        ccta3sumZ2 <- ccta3sumZ2[complete.cases(data_subset), ]
        
        #Rename table and add GeneWeek unique identifier
        wwda2 <- ccta3sumZ2
        wwda2$GeneWeek <- paste(wwda2$GeneName, wwda2$week, wwda2$Genotype, sep=", ")
        
        wwda2<-wwda2[order(wwda2$GeneWeek), ]
        
        #Output data into analyzed data tab
        output$contents <- renderTable(wwda2, hover=TRUE, border=TRUE, spacing="s", digits=5, align ="l")
        values$rawChoices <- colnames(wwda2)
        wwda2sub <- subset(wwda2, select = -c(CloneID, GeneName, CloneName, week, GeneWeek))
        values$choices <- colnames(wwda2sub)
        
        #Backup data in reactive values
        values$wwda2 <- wwda2
        values$wwdaArchive <- wwda2
        
        
        
   
    },
    # ... but if an error occurs, tell me what happened: 
    error=function(error_message) {
      message("no caspase")
      return(NA)
    }
    )
    })
    
    
    ############################################################################################################################
    ##################################################### Wing Disc Analysis ###################################################
    ############################################################################################################################
    
    analysis <- observeEvent(input$go, {
        
      tryCatch(
        {       
        #Read genotype assignment csv file to the variable 'trad'
        trad  <- values$genAssign
        
        #Read dataset csv files to the variable 'wda'
        wda <- read.csv(input$csvs$datapath[1], check.names=FALSE)
        count = 2
        while (count <= length(input$csvs$datapath)) {
            toBind <- read.csv(input$csvs$datapath[count], check.names=FALSE)
            wda <- rbind(wda, toBind)
            count = count + 1
        }
        
        
       
        
        #Check what type of dataframe has been uploaded
        okayGo <- FALSE
        for (i in colnames(wda)){
          if(grepl("Pouch Area<", i, fixed = TRUE)) {okayGo <- TRUE}
        }
        
        
        #Here we make sure the correct steps have been taken (i.e. right buttons pressed, right files uploaded)
        validate(
            need(wda$File, "Please load a whole disc analysis file and assign genotypes"),
            need(okayGo == TRUE, "Please load a whole disc analysis file and assign genotypes"),
            need(values$loaded == 1, "Load genotype assignments first!")
        )
       
        
        colnames(trad) <- c("Image Name", "week", "Gene.Name")
        #We tack the assignments onto the input data, creating a dataframe 'wwda2' with our raw data and assignments
        wwdaFull <- merge(x = wda, y = trad)
        
        
        
        #We split the 'wwdaFull' vector. The table contains many measurements that have been repeated on several genotypes. We want to repeat this analysis for each genotype
        #We loop through the analysis, doing it once per genotype
        headers = list(colnames(wwdaFull))
        
        
        #Extract all the unique genotype names
        name <- str_extract_all(headers, "\\<[^<>]+\\>")[[1]]
        name <- str_extract_all(name, "\\<[^<>]+\\_")
        name <- gsub("<|_", "", name)
        genotypeNames <- unique(name)
        values$genotypeNames <- genotypeNames
        
        
        #Check for the number of genotypes
        baseString <- "Genotype"
        count <- 1
        breakLoop <- FALSE
        
        while (breakLoop == FALSE){
            for (i in headers){
                
                breakLoop <- TRUE
                breaker <- FALSE
                queryString <- paste(baseString, toString(count), sep="_")
                count <- count + 1
                
                isGenotypeNum <- grepl(queryString, i, fixed = TRUE)
                
                for (z in isGenotypeNum){ if (z == TRUE){ breaker = TRUE } }
                if (breaker == TRUE){breakLoop <- FALSE}
            }
        }
        count <- count - 2
        
        #Get only the columns that are not genotype specific
        staticVals <- wwdaFull[ , !grepl( "Genotype" , names( wwdaFull) ) ]

        #Loop through, get all the columns corresponding to one genotype, and store them in an array
        chal <- 1
        while(chal <= count){
            queryString1 <- paste(baseString, toString(chal), sep="_")
            queryString2 <- paste(baseString, toString(chal), sep=":")
            queryString <- paste(queryString1, queryString2, sep="|")
            
            genotypeVals <- wwdaFull[ , grepl( queryString , names( wwdaFull) ) ]
            genotypeVals <- cbind(staticVals, genotypeVals)
            
            headers <- colnames(genotypeVals)
            
            headers <- gsub(queryString1, "", headers, ignore.case = FALSE, fixed = TRUE)
            headers <- gsub("<[^>]+>", "", headers)
            headers <- gsub(" ", ".", headers)
            headers <- gsub("#", "X.", headers)
            
            
            colnames(genotypeVals) <- headers
            
            wwda <- genotypeVals


            
            #Here we make our dataframe 'wwda2,' which has all of our data from all our z-levels collated per wing disc
            wwda2 <- data.frame(
                File = tapply(as.character(wwda$File) , wwda$Image.Name , unique),
                Image.Name = tapply(as.character(wwda$Image.Name) , wwda$Image.Name , unique),
                GeneName =  tapply(as.character(wwda$Gene.Name) , wwda$Image.Name , unique),
                week = tapply(as.character(wwda$week) , wwda$Image.Name , unique),
                
                Pouch.Volume = tapply(wwda$Pouch.Area , wwda$Image.Name , sum),
                Clone.Volume = tapply(wwda$Clone.Area , wwda$Image.Name , sum),
                Clone.Center.Volume = tapply(wwda$Center.Area , wwda$Image.Name , sum),
                Clone.Border.Volume = tapply(wwda$Border.Area , wwda$Image.Name , sum),
                Not.Clone.Volume = tapply(wwda$Pouch.Area , wwda$Image.Name, sum) - tapply(wwda$Clone.Area , wwda$Image.Name, sum),
                
                Number.of.Z.Slices = tapply(wwda$Z.level, list(wwda$Image.Name), max) - tapply(wwda$Z.level, list(wwda$Image.Name), min) + 1,
                Percent.Clone.Coverage.of.Pouch = tapply(wwda$Clone.Area , wwda$Image.Name , sum)*100 / tapply(wwda$Pouch.Area , wwda$Image.Name , sum)
                
            )
            
            #Here we add the individual cell counts information to the table, if applicable

            tryCatch(
                {
                  if("Cell.Count.in.Center" %in% colnames(wwda))
                  {
                    
                  
                    wwda2 <- cbind(wwda2, Number.of.Center.Cells = tapply(wwda$Cell.Count.in.Center , wwda$Image.Name , unique))
                    wwda2 <- cbind(wwda2, Number.of.Border.Cells = tapply(wwda$Cell.Count.in.Border , wwda$Image.Name , unique))
                    wwda2 <- cbind(wwda2, Total.Number.of.Cells = tapply(wwda$Total.Cell.count, wwda$Image.Name, unique))
                    
                    wwda2 <- cbind(wwda2, Density.of.Center.Cells = tapply(wwda$Cell.Count.in.Center , wwda$Image.Name , unique) / tapply(wwda$Center.Area , wwda$Image.Name , sum))
                    wwda2 <- cbind(wwda2, Density.of.Border.Cells = tapply(wwda$Cell.Count.in.Border , wwda$Image.Name , unique) / tapply(wwda$Border.Area , wwda$Image.Name , sum))
                    wwda2 <- cbind(wwda2, Density.of.Cells.in.Clones = (tapply(wwda$Cell.Count.in.Border , wwda$Image.Name , unique)+tapply(wwda$Cell.Count.in.Center , wwda$Image.Name , unique) / tapply(wwda$Clone.Area , wwda$Image.Name , sum)))
                    wwda2 <- cbind(wwda2, Average.Cell.Volume = tapply(wwda$Clone.Area , wwda$Image.Name , sum) / (tapply(wwda$Cell.Count.in.Border , wwda$Image.Name , unique)+tapply(wwda$Cell.Count.in.Center , wwda$Image.Name , unique)))
                    wwda2 <- cbind(wwda2, Average.Cell.Area.Per.Z.Plane = (tapply(wwda$Clone.Area , wwda$Image.Name , sum) / (tapply(wwda$Cell.Count.in.Border , wwda$Image.Name , unique)+tapply(wwda$Cell.Count.in.Center , wwda$Image.Name , unique)) / (tapply(wwda$Z.level, list(wwda$Image.Name), max) - tapply(wwda$Z.level, list(wwda$Image.Name), min))))
               
                  }
                 },
                error=function(error_message) {
                    message("no complete counts")
                    return(NA)
                }
            )
            
            tryCatch(
                # We add the number of clones information, if its in the data table
                { 
                  
                  if("Total.Number.of.Clones" %in% colnames(wwda))
                  {
                    wwda2 <- cbind(wwda2, Number.of.Clones = tapply(wwda$Total.Number.of.Clones , wwda$Image.Name , unique))
                  }
                },
                error=function(error_message) {
                    message("no clone counts")
                    return(NA)
                }
            )
            
            
            # This variable determines if there were DCP1 counts or not
            TrueCasCounts <- 0
            #This variable tracks whether or not we were able to do individual cell counts or not
            TrueNonCasCounts <- 0    
            #Add the caspase information to the table, if applicable
            tryCatch(
                # We attempt to analyze the caspase coverage data. If it exists, it will be added to the table. Otherwise, we skip it.
                {
                  
                  if("Caspase.Area" %in% colnames(wwda))
                  {
                    wwda2 <- cbind(wwda2, Total.Caspase.Volume = tapply(wwda$Caspase.Area , wwda$Image.Name , sum))
                    wwda2 <- cbind(wwda2, Clone.Caspase.Volume = tapply(wwda$Clone.Caspase , wwda$Image.Name , sum))
                    wwda2 <- cbind(wwda2, Center.Caspase.Volume = tapply(wwda$Center.Caspase , wwda$Image.Name , sum))
                    wwda2 <- cbind(wwda2, Border.Caspase.Volume = tapply(wwda$Border.Caspase, wwda$Image.Name , sum))
                    wwda2 <- cbind(wwda2, Not.Clones.Caspase.Volume = tapply(wwda$Not.Clones.Caspase.Area, wwda$Image.Name , sum))
                    
                    wwda2 <- cbind(wwda2, Percent.Caspase.Coverage.of.Pouch = tapply(wwda$Caspase.Area , wwda$Image.Name , sum) *100/ tapply(wwda$Pouch.Area , wwda$Image.Name , sum))
                    wwda2 <- cbind(wwda2, Percent.Caspase.Coverage.of.Clones = tapply(wwda$Clone.Caspase , wwda$Image.Name , sum) *100/ tapply(wwda$Clone.Area , wwda$Image.Name , sum))
                    wwda2 <- cbind(wwda2, Percent.Caspase.Coverage.of.Border = tapply(wwda$Border.Caspase, wwda$Image.Name , sum) *100/ tapply(wwda$Border.Area , wwda$Image.Name , sum))
                    wwda2 <- cbind(wwda2, Percent.Caspase.Coverage.of.Center = tapply(wwda$Center.Caspase, wwda$Image.Name , sum) *100/ tapply(wwda$Center.Area , wwda$Image.Name , sum))
                    wwda2 <- cbind(wwda2, Percent.Caspase.Coverage.of.Not.Clones = tapply(wwda$Not.Clones.Caspase.Area, wwda$Image.Name , sum) *100/ tapply(wwda$Not.Clones.Area , wwda$Image.Name , sum))
                    wwda2 <- cbind(wwda2, Competitive.Index = c( c( tapply(wwda$Center.Caspase , wwda$Image.Name , sum) / tapply(wwda$Center.Area , wwda$Image.Name , sum) )
                                                                 /
                                                                     c( tapply(wwda$Border.Caspase, wwda$Image.Name , sum) / tapply(wwda$Border.Area , wwda$Image.Name , sum) ) ))
                    wwda2 <- cbind(wwda2, log2.Competitive.Index = log(wwda2$Competitive.Index, 2))
                  }
                    
                    tryCatch(
                        # This is where we incorporate the dcp1 counts, if applicable
                        {
                          if("Total.Caspase.Cells" %in% colnames(wwda))
                          {
                            wwda2 <- cbind(wwda2, Total.Caspase.Cell.Count = tapply(wwda$Total.Caspase.Cells , wwda$Image.Name , unique))
                            wwda2 <- cbind(wwda2, Center.Caspase.Cell.Count = tapply(wwda$Caspase.Count.in.Center , wwda$Image.Name , unique))
                            wwda2 <- cbind(wwda2, Border.Caspase.Cell.Count = tapply(wwda$Caspase.Count.in.Border , wwda$Image.Name , unique))
                            
                            wwda2 <- cbind(wwda2, Caspase.Cell.Density.in.Center = tapply(wwda$Caspase.Count.in.Center , wwda$Image.Name , unique)/ tapply(wwda$Center.Area , wwda$Image.Name , sum))
                            wwda2 <- cbind(wwda2, Caspase.Cell.Density.in.Border = tapply(wwda$Caspase.Count.in.Border , wwda$Image.Name , unique)/ tapply(wwda$Border.Area , wwda$Image.Name , sum))
                            wwda2 <- cbind(wwda2, Average.Cas.Cell.Volume = tapply(wwda$Total.Caspase.Cells , wwda$Image.Name , unique)/ tapply(wwda$Caspase.Area , wwda$Image.Name , sum))
                            wwda2 <- cbind(wwda2, Average.Cas.Cell.Area.Per.Z.Plane = (tapply(wwda$Total.Caspase.Cells , wwda$Image.Name , unique)/ tapply(wwda$Caspase.Area , wwda$Image.Name , sum)) / (tapply(wwda$Z.level, list(wwda$Image.Name), max) - tapply(wwda$Z.level, list(wwda$Image.Name), min)))
                            TrueCasCounts <- 1
                            
                            tryCatch(
                                # This is where we do the viable/nonviable cell counts
                                {
                                  if("Cell.Count.in.Border" %in% colnames(wwda))
                                  {
                                    wwda2 <- cbind(wwda2, NonCaspase.Cells.in.Border = tapply(wwda$Cell.Count.in.Border , wwda$Image.Name , unique) - tapply(wwda$Caspase.Count.in.Border , wwda$Image.Name , unique))
                                    wwda2 <- cbind(wwda2, NonCaspase.Cells.in.Center = tapply(wwda$Cell.Count.in.Center , wwda$Image.Name , unique) - tapply(wwda$Caspase.Count.in.Center , wwda$Image.Name , unique))
                                    wwda2 <- cbind(wwda2, Percent.Cells.Dying.in.Border = tapply(wwda$Caspase.Count.in.Border , wwda$Image.Name , unique) * 100/ tapply(wwda$Cell.Count.in.Border , wwda$Image.Name , unique))
                                    wwda2 <- cbind(wwda2, Percent.Cells.Dying.in.Center = tapply(wwda$Caspase.Count.in.Center , wwda$Image.Name , unique) * 100/ tapply(wwda$Cell.Count.in.Center , wwda$Image.Name , unique))
                                    TrueNonCasCounts <- 1
                                  }
                                },
                                error=function(error_message) {
                                    message("no complete counts")
                                    return(NA)
                                }
                            ) 
                          }
                            
                        },
                        error=function(error_message) {
                            message("no caspase counts")
                            return(NA)
                        }
                    )
                    
                },
                error=function(error_message) {
                    message("no caspase")
                    return(NA)
                }
            )
            
            tryCatch(
                # This is where we add the fluorescence data to the table, if applicable
                {
                  if("Clone.Fluorescence" %in% colnames(wwda))
                  {
                 
                    wwda2 <- cbind(wwda2, Clone.Fluorescence = tapply(wwda$Clone.Fluorescence , wwda$Image.Name , sum) / tapply(wwda$Clone.Area , wwda$Image.Name , sum))
                    wwda2 <- cbind(wwda2, Center.Fluorescence = tapply(wwda$Center.Fluorescence , wwda$Image.Name , sum)/ tapply(wwda$Center.Area , wwda$Image.Name , sum))
                    wwda2 <- cbind(wwda2, Border.Fluorescence = tapply(wwda$Border.Fluorescence, wwda$Image.Name , sum) / tapply(wwda$Border.Area , wwda$Image.Name , sum))
                    wwda2 <- cbind(wwda2, Not.Clone.Fluorescence = tapply(wwda$Not.Clone.Fluorescence , wwda$Image.Name , sum) / tapply(wwda$Not.Clones.Area, wwda$Image.Name , sum))
                    
                    wwda2 <- cbind(wwda2, Ratio.Fluorescence.Clones.vs.Not.Clones = wwda2$Clone.Fluorescence / wwda2$Not.Clone.Fluorescence)
                    wwda2 <- cbind(wwda2, Ratio.Fluorescence.Border.vs.Center = wwda2$Border.Fluorescence / wwda2$Center.Fluorescence)
                  }
                    
                },
                error=function(error_message) {
                    message("no fluorescence")
                    return(NA)
                }
            )
            
            tryCatch(
                # This is where we add the speckle data
                {
                  if("X.Speckles.in.Clones" %in% colnames(wwda))
                  {
                    
                    wwda2 <- cbind(wwda2, Density.of.Speckles.in.Clones = tapply(wwda$X.Speckles.in.clones , wwda$Image.Name , sum) / tapply(wwda$Clone.Area , wwda$Image.Name , sum))
                    wwda2 <- cbind(wwda2, Density.of.Speckles.in.Border = tapply(wwda$X.Speckles.in.border , wwda$Image.Name , sum) / tapply(wwda$Border.Area , wwda$Image.Name , sum))
                    wwda2 <- cbind(wwda2, Density.of.Speckles.in.Center = tapply(wwda$X.Speckles.in.Center , wwda$Image.Name , sum) / tapply(wwda$Center.Area , wwda$Image.Name , sum))
                    wwda2 <- cbind(wwda2, Density.of.Speckles.in.Not.Clones = tapply(wwda$X.Speckles.in.Not.Clones , wwda$Image.Name , sum) / tapply(wwda$Not.Clone.Volume, wwda$Image.Name , sum))
                    wwda2 <- cbind(wwda2, Speckle.Density.Clones.vs.Not.Clones = wwda2$Density.of.Speckles.in.Clones / wwda2$Density.of.Speckles.in.Not.Clones)
                    wwda2 <- cbind(wwda2, Speckle.Density.Border.vs.Center = wwda2$Density.of.Speckles.in.Border / wwda2$Density.of.Speckles.in.Center)
                    
                    tryCatch({
                      if("Speckle.Area.in.Clones.IntDen" %in% colnames(wwda))
                      {
                        
                        wwda2 <- cbind(wwda2, Percent.Speckle.Coverage.of.Clones = tapply(wwda$Speckle.Area.in.Clones.IntDen , wwda$Image.Name , sum) * 100/ tapply(wwda$Clone.Area , wwda$Image.Name , sum))
                        wwda2 <- cbind(wwda2, Percent.Speckle.Coverage.of.Border = tapply(wwda$Speckle.Area.in.Border.IntDen , wwda$Image.Name , sum) * 100/ tapply(wwda$Border.Area , wwda$Image.Name , sum))
                        wwda2 <- cbind(wwda2, Percent.Speckle.Coverage.of.Center = tapply(wwda$Speckle.Area.in.Center.IntDen , wwda$Image.Name , sum) * 100/ tapply(wwda$Center.Area , wwda$Image.Name , sum))
                        wwda2 <- cbind(wwda2, Percent.Speckle.Coverage.of.Not.Clones = tapply(wwda$Speckle.Area.in.Not.Clones.IntDen , wwda$Image.Name , sum) * 100/ tapply(wwda$Not.Clones.Area , wwda$Image.Name , sum))
                        
                        wwda2 <- cbind(wwda2, Mean.Speckle.Fluorescence.Clones = tapply(wwda$Speckle.Fluorescence.in.Clones.IntDen , wwda$Image.Name , sum) / tapply(wwda$Clone.Area , wwda$Image.Name , sum))
                        wwda2 <- cbind(wwda2, Mean.Speckle.Fluorescence.Border = tapply(wwda$Speckle.Fluorescence.in.Border.IntDen , wwda$Image.Name , sum) / tapply(wwda$Border.Area , wwda$Image.Name , sum))
                        wwda2 <- cbind(wwda2, Mean.Speckle.Fluorescence.Center = tapply(wwda$Speckle.Fluorescence.in.Center.IntDen , wwda$Image.Name , sum) / tapply(wwda$Center.Area , wwda$Image.Name , sum))
                        wwda2 <- cbind(wwda2, Mean.Speckle.Fluorescence.Not.Clones = tapply(wwda$Speckle.Fluorescence.in.Not.Clones.IntDen , wwda$Image.Name , sum)/ tapply(wwda$Not.Clones.Area , wwda$Image.Name , sum))
                        
                        wwda2 <- cbind(wwda2, Average.Speckle.Size.Clones = tapply(wwda$Speckle.Area.in.Clones.IntDen , wwda$Image.Name , sum) / tapply(wwda$X.Speckles.in.clones , wwda$Image.Name , sum))
                        wwda2 <- cbind(wwda2, Average.Speckle.Size.Border = tapply(wwda$Speckle.Area.in.Border.IntDen , wwda$Image.Name , sum) / tapply(wwda$X.Speckles.in.border , wwda$Image.Name , sum))
                        wwda2 <- cbind(wwda2, Average.Speckle.Size.Center = tapply(wwda$Speckle.Area.in.Center.IntDen , wwda$Image.Name , sum) / tapply(wwda$X.Speckles.in.Center , wwda$Image.Name , sum))
                        wwda2 <- cbind(wwda2, Average.Speckle.Size.Not.Clones = tapply(wwda$Speckle.Area.in.Not.Clones.IntDen , wwda$Image.Name , sum) / tapply(wwda$X.Speckles.in.Not.Clones , wwda$Image.Name , sum))
                        }
                        
                    },
                    error=function(error_message) {
                        message("no speckles area/fluorescence")
                        return(NA)
                    }
                    )
                  }
                    
                    
                    
                    
                    
                },
                error=function(error_message) {
                    message("no speckles")
                    return(NA)
                }
            )
            
            #Get rid of empty tables
            data_subset <- wwda2[ , c("File")]
            wwda2 <- wwda2[complete.cases(data_subset), ]
            
            #Store the processed data in reactive values
            wwda2$GeneWeek <- paste(wwda2$GeneName, wwda2$week, sep=", ")
            wwda2<-wwda2[order(wwda2$GeneWeek), ]
            
            
            
            
            
            
            #Step for fisher test of competitive index
            
            
            
            
            
            tryCatch(
                
                {
                    
                    #If we have the actual counts from the macro, we prefer to use those. Otherwise, we revert to the mathematical approximation
                  
                  if("NonCaspase.Cells.in.Border" %in% colnames(wwda2))
                  {
                    
                    borderCells <- aggregate(wwda2$NonCaspase.Cells.in.Border, list(wwda2$GeneWeek), sum)
                    centerCells <- aggregate(wwda2$NonCaspase.Cells.in.Center, list(wwda2$GeneWeek), sum)
                    borderCasCells <- aggregate(wwda2$Border.Caspase.Cell.Count, list(wwda2$GeneWeek), sum)
                    centerCasCells <- aggregate(wwda2$Center.Caspase.Cell.Count, list(wwda2$GeneWeek), sum)        
                    
                    
                    #Format these values into a table suitable for a fisher test
                    deathTable <- data.frame(
                        Genotype = borderCells[,1],
                        NotCasCellsInBorder = borderCells[,2],
                        CasCellsInBorder = borderCasCells[,2],
                        NotCasCellsInCenter = centerCells[,2],
                        CasCellsInCenter = centerCasCells[,2]
                    )
                    
                    values$deathTable <- deathTable
                    output$deathTable <- renderTable(values$deathTable, hover=TRUE, border=TRUE, spacing="s", digits=5, align ="l")
                  }
                    
                },
                error=function(error_message) {
                    message("Unable to prepare death table")
                    return(NA)
                }
            )
            
            tryCatch(
                # This is we run the fisher test for individual wing discs
                {        
                    
                    lengthotron <- nrow(wwda2)
                    x <- 0
                    fishResult <- c()
                    fishResultNum <- c()
                    fishConfidence <-c()
                    fishConfidenceUB <- c()
                    fishOddsRatio <- c()
                    
                    
                    while (x < lengthotron){
                        
                        #Get data and format into 2x2 contingency table
                        fishers <- matrix(c(wwda2[x+1, "NonCaspase.Cells.in.Border"],wwda2[x+1, "NonCaspase.Cells.in.Center"], wwda2[x+1, "Border.Caspase.Cell.Count"], wwda2[x+1, "Center.Caspase.Cell.Count"]), ncol = 2)
                        output$fishyPrint <- renderText("Cell numbers were directly counted by the macro")
                        colnames(fishers)<-c("NotDying", "Dying")
                        colnames(fishers)<-c("NotDying", "Dying")
                        rownames(fishers)<-c("Border", "Center")
                        fishers<-as.table(fishers)
                        
                        #Run fisher test and store results
                        fishy <- fisher.test(fishers)
                        
                        
                        cfLB<- as.character(c(fishy$conf.int[1]))
                        cfUB <-as.character(c(fishy$conf.int[2]))
                        odds <- as.character(c(fishy$estimate[1]))
                        fishConfidence <- c(fishConfidence, cfLB)
                        fishConfidenceUB <- c(fishConfidenceUB, cfUB)
                        fishOddsRatio <- c(fishOddsRatio, odds)
                        
                        
                        fishResultNum <- rbind(fishResult, as.numeric(c(fishy$p.value)))
                        fishResult <- rbind(fishResult, as.character(c(fishy$p.value)))
                        
                        x <- x+1
                    }
                    
                    
                    
                    #Run and add our p-adjustment
                    pAdj <- p.adjust(fishResult, method = input$pAdj, n = length(fishResult))
                    colnames(fishResult) <- c("p-value")
                    colnames(fishResultNum) <- c("p-value")
                    fishResult2I <- data.frame(Image.Name = wwda2$Image.Name, GeneWeek = wwda2$GeneWeek, fishResult, conf.interval.from = fishConfidence, conf.interval.to = fishConfidenceUB, p.adjusted = as.numeric(pAdj), p.adjustment.method = input$pAdj, odds.ratio=fishOddsRatio)
                    output$siggy <- renderTable(fishResult2I, hover=TRUE, border=TRUE, spacing="s", digits=5, align ="l")
                    fishResult2 <- data.frame(Image.Name = wwda2$Image.Name, GeneWeek = wwda2$GeneWeek, fishResultNum)
                    weekname <- unique(fishResult2$GeneWeek)
                    
                    significantCount <- c()
                    nonSignificant <-c()
                    
                    for (j in weekname){          
                        fishData <- fishResult2 %>% filter(GeneWeek == j)
                        total <- nrow(fishData)
                        significant <- 0
                        for (x in fishData$p.value){
                            if (x < 0.05){
                                significant <- significant + 1
                            }
                        }
                        significantCount <- c(significantCount, significant)
                        nonSignificant <- c(nonSignificant, total - significant)
                        
                    }
                    
                    
                    fishCounts <- data.frame(GeneWeek = weekname, number.of.competing.wing.discs = significantCount, number.of.noncompeting.wing.discs = nonSignificant)
                    output$fishyCounty <- renderTable(fishCounts, hover=TRUE, border=TRUE, spacing="s", digits=5, align ="l")
                    
                    
                },
                error=function(error_message) {
                    message("Unable to prepare single disk fishers")
                    return(NA)
                }
            )
            
            
            #Add the genotype to the headers
            headers <- colnames(wwda2)
            headers1 <- headers[1:4]
            headers2 <- headers[5:(length(headers)-1)]
            headers3 <- headers[length(headers)]
            headers2<- paste(headers2, genotypeNames[chal], sep =".")
            headers <- c(headers1, headers2, headers3)
            
            colnames(wwda2) <- headers
            
            
            #Merge our genotype tables together
            if (chal == 1){ values$wwda2 <- data.frame(wwda2) }
            else { values$wwda2 <- merge(values$wwda2, wwda2) }
            
            wwda2 <- values$wwda2
            output$contents <- renderTable(wwda2, hover=TRUE, border=TRUE, spacing="s", digits=5, align ="l")
          
            
            #Store these values for later use, and output data visually for user
            values$wwdaArchive <- wwda2
            output$contents <- renderTable(wwda2, hover=TRUE, border=TRUE, spacing="s", digits=5, align ="l")
            
            #Use the column headers to set which parameters the user can choose to analyze
            wwda2sub <- subset(values$wwda2, select = -c(File, Image.Name, GeneName, week, GeneWeek))
            values$choices <- colnames(wwda2sub)
            values$rawChoices <-colnames(values$wwda2)
            
        
            chal <- chal+1
        }
        
    
    },
    # ... but if an error occurs, tell me what happened: 
    error=function(error_message) {
      message("no caspase")
      return(NA)
    }
    )
    
    })
    
    ###################################################################################################
    ############################### This is where all the code for the 'Generate Plots' button is#############
    ###################################################################################################
    
    regressionTesting <- observeEvent(input$goReg, {
        
    
        tryCatch({
            # Attempt to run regression analysis
            
            nullVal <- input$regressionParameter1
            
            
            
            #Get all our data
            logRegData <- values$wwda2
            logRegData <- logRegData[order(logRegData$GeneName, logRegData$week), ]
            
            
            
            #Subset our data based on user specified factors
            factors <- input$factorCheckboxes
            
      
            #Conver characters to factors and get rid of 1 level factors
            logRegDataSub <- logRegData[ , as.numeric(factors), drop=FALSE]
          
            logRegDataSub[sapply(logRegDataSub, is.character)] <- lapply(logRegDataSub[sapply(logRegDataSub, is.character)], 
                                                                         as.factor)
       
            logRegDataSub <- logRegDataSub[, sapply(logRegDataSub, nlevels) != 1]

            if (input$linkFunction == "Logistic") {
                
                #Run our logistic regression
                posVal <- input$regressionParameter2
                regression <- glm(cbind(logRegData[[posVal]], logRegData[[nullVal]])~. , family = binomial(link="logit"), data=logRegDataSub, na.action = na.omit)
                
                regOutput <- summary(regression)
                
                regOutput <-regOutput$coefficients
                
                counter <- 2
                logOddsVec <- c(" ", " ", " ")
                while (count <ncol(regOutput)){
                  
                  logOddsCI <- exp(regOutput[counter,1] + qnorm(c(0.5,0.25,0.975)) * regOutput[counter,2])
                  logOddsVec <- rbind(logOddsVec, logOddsCI)
                  
                  count <- count + 1
                }
                
                colnames(logOddsVec) <- c("Odds Ratio", "Conf. Int. Lower", "Conf. Int. Upper")
                regOutput <- cbind(regOutput, logOddsVec)
                output$regressionData <- renderTable(regOutput, rownames=TRUE, hover=TRUE, border=TRUE, spacing="s", digits=5, align ="l")
                
               
            
                
                r2e <- nagelkerke(regression, restrictNobs = TRUE)
                output$Rsquared <-renderPrint(r2e)
                
                predicted.data <- data.frame(
                  probability.predicted = regression$fitted.values,
                  probability.observed = logRegData[[posVal]] / (logRegData[[nullVal]] + logRegData[[posVal]])
                )
                
                
                predicted.data.plot <- ggplot(data=predicted.data, aes(x=probability.observed, y=probability.predicted))+geom_point()+geom_smooth(method='lm', formula= y~x) + theme_tufte() +ggtitle("Predicted probability vs. observed probability")

                
                
            } else if (input$linkFunction == "Poisson"){
                regression <- glm(logRegData[[nullVal]]~., family="poisson"(link="log"), data=logRegDataSub, na.action = na.omit )
                
 
                predicted.data <- data.frame(
                  value.predicted = regression$fitted.values,
                  value.observed = logRegData[[nullVal]]
                )
                
                predicted.data.plot <- ggplot(data=predicted.data, aes(x=value.observed, y=value.predicted))+geom_point()+geom_smooth(method='lm', formula= y~x) + theme_tufte() +ggtitle("Predicted probability vs. observed probability")
            
                } else {
                  
                  #regression <- glm(logRegData[[nullVal]]~., family = "gaussian"(link="identity"),  data=logRegDataSub, na.action = na.omit )
                  regression <- lm(logRegData[[nullVal]]~.,  data=logRegDataSub, na.action = na.omit )
                  
                  predicted.data <- data.frame(
                    value.predicted = regression$fitted.values,
                    value.observed = logRegData[[nullVal]]
                  )
                  
                  predicted.data.plot <- ggplot(data=predicted.data, aes(x=value.observed, y=value.predicted))+geom_point()+geom_smooth(method='lm', formula= y~x) + theme_tufte() +ggtitle("Predicted probability vs. observed probability")
                               
                  
              }
            
            
            output$predicted.data.plot <- renderPlot(predicted.data.plot)  
            
            
            logSum <- summary(regression)
            output$regressionSummary <- renderPrint(logSum)
            
            
            # logAnova<- anova(regression, test = "Chisq")
            # print (logAnova)
            
            
            
            # anovaComp <- anova(logRegression, logRegressionSS, test = "Chisq")
            # print (anovaComp)
            
            
            
            # Conditions <- unique(values$wwda2$GeneWeek)
            # condlist <- c()
            # modlist <- c()
            # formlist <- c()
            # r2list <- c()
            
            # yCond <- paste("values$wwda2$", input$plotterpaired, sep = "")
            # xCond <- paste("values$wwda2$", input$plotter, sep = "")
            # form <- paste(yCond, xCond, sep=" ~ ")
            # formAnc <- paste(form, " * GeneWeek", sep = "")
            # form <- as.formula(form)
            
            
            
            #Make linear models for each genotype
            # for (k in Conditions){
            #   weekData <- values$wwda2 %>% filter(GeneWeek == k)
            #   mod <- lm(form)
            #   condlist <- rbind(condlist, k)
            #   intercept<- as.character(mod$coefficients[1])
            #   slope <- as.character(mod$coefficients[2])
            #   formOut <- paste(input$plotterpaired, "=", intercept, "+", slope, "*", input$plotter, sep = " ")
            #   formlist <- rbind(formlist, formOut)
            #   r2list <- rbind(r2list, summary(mod)$r.squared)
            #   print(summary(mod)$r.squared)
            #   modlist <- rbind(modlist, mod)
            # }
            # y <- as.vector(values$wwda2[input$plotterpaired])
            # x <- as.vector(values$wwda2[input$plotter])
            # z <- as.vector(values$wwda2$GeneWeek)
            # 
            # colnames(x) <- NULL
            # colnames(y) <- NULL
            # colnames(z) <- NULL
            
            #dats <- data.frame( xV = x, yV = y, zV = z)
            #mod1 <- lm(yV ~ xV, data= dats)
            #mod2 <- lm(yV ~ xV * zV, data= dats)
            #ancova <- anova(mod1, mod2)
            #print (ancova)
            #print (modlist)
            
            

        },
        # return an error message otherwise
        error=function(error_message) {
            message("Unable to run regression analysis")
            return(NA)
        }
        )
        
        
    })
                            
    
    analysis <- observeEvent(input$genPlot, { 

        ############################################################
        ################# Specify statistics! #########################
        #############################################################
        
                
        #Reset all our outputs so we don't get confused
        output$statsTable <- renderTable(data.frame(), hover=TRUE, border=TRUE, spacing="s", digits=5, align ="l")
        output$statsHeader <- renderText("")
        output$normality <- renderTable(data.frame(), hover=TRUE, border=TRUE, spacing="s", digits=5, align ="l")
        output$normality2 <- renderTable(data.frame(), hover=TRUE, border=TRUE, spacing="s", digits=5, align ="l")
        output$normalityHeader <- renderText("")
        output$normalityHeader2 <- renderText("")
        output$fligner <- renderTable(data.frame(), hover=TRUE, border=TRUE, spacing="s", digits=5, align ="l")
        output$leveneHeader <- renderText("")
        output$plotso <- renderPlot(ggplot())
        output$plotso2 <- renderPlot(ggplot())
        
        #This option sets the test we will use for stats.compare.means. Only basic means testing is available here.
        #All other statistical tests will be handled elsewhere
        values$method <- input$statsType
        
        #Here, we allow the user to exclude data points based on a selected criterion
        if (input$excluded == TRUE){
            
            excludedData <- values$wwda2
            excluder <- paste("excludedData$", input$excludeChoice, input$excludeOperator,input$excludeValue, sep = "")
            output$Z <-renderPrint(excluder)
            values$wwda2 <- excludedData[eval(parse(text=excluder)),]
            output$contents <- renderTable(values$wwda2,  hover=TRUE, border=TRUE, spacing="s", digits=5, align ="l")
            
        } else { values$wwda2<- values$wwdaArchive }
        
        if(input$orderWeek == TRUE){ values$wwda2 <- values$wwda2[order(values$wwda2$week)] } 
        if(input$orderGene == TRUE){ values$wwda2 <- values$wwda2[order(values$wwda2$GeneName)] }
        
        #yarp is the data
        yarp <- values$wwda2
        
        #narp is the specified dataset for analysis
        narp <- input$plotter
        plottitle  <-toString(narp)
        
        #We convert these values to a formula that the ggplot algorithm can take
        plotformula <- paste(plottitle, "~ GeneName", sep=" ")
        
        #Extract just the column corresponding to the specified dataset to analyze
        values$idx<- grep(plottitle, colnames(values$wwda2))
        
        ################################################################################################################
        ############################# Here we prepare our paired box plot data #########################################
        ################################################################################################################
        
        if ((input$plotType == "Paired Box Plot")|(input$plotType == "Scatter Plot") | (input$plotType == "Scatter Plot w/ Regression Line")){
            
            #This is the second dataset which we have matched for the paired plot
            zarp <- input$plotterpaired
            
            
            #Get the title of the plot from user input
            plottitle2 <- input$plotterpaired
            plottitle2 <- toString(plottitle2)
            
            
            #xValP and yValP are now each strings over the two variables the user wants to analyze in a paired fashion
            #We will use these to pull the data we need by matching these strings to the headers of our table
            xValP <- toString(narp)
            yValP <- toString(zarp)
            
            #xVal ID refers to the unique identifier for our datapoints in our dataset
            xValID <- "Image.Name"
            if(input$analysisType == "Individual Clone Analysis"){ xValID <- "CloneName" }
            if (input$analysisType == "Individual Clone Analysis"){ xValID <- "CloneID" }
            if (input$analysisType == "No Clones Analysis"){  xValID <- "Image" }
            
            #extract the data column from the second user-specified dataset
            values$plotData2 <- yarp[,c( as.character(zarp))]
            
            
            #Convert to formula for ggplot function to accept
            plotformula2 <- paste(xValP, "GeneWeek", sep=" ~ ")
            values$plotformula2 <- plotformula2
            
            #we extract the titles of all the columns we are interested/need to use for the paired plot
            idx<- grep(plottitle, colnames(values$wwda2))
            wwda2 <- values$wwda2
            
            idx11<- grep(xValP, colnames(values$wwda2))
            idx2<- grep(yValP, colnames(values$wwda2))
            idx3 <- grep(xValID, colnames(values$wwda2))
            idx4 <- grep("GeneWeek", colnames(values$wwda2))
            idx5 <- grep("GeneName", colnames(values$wwda2))
            idx6 <- grep("week", colnames(values$wwda2))
            
            #We use the column headers to make a new dataframe with just what we are interested in for the paired box plot and store it as the variable "pairing"
            wwda2 <- wwda2[, c(idx11, idx2, idx3, idx4, idx5, idx6)]
            pairing <- wwda2
            
            #extract our first dataset from the dataframe as a vector
            dataVector <- wwda2[xValP]
            names(dataVector) <- "xValues"
            
            #extract our second dataset from the dataframe as a vector
            dataVector2 <- wwda2[yValP]
            names(dataVector2) <- "xValues" 
            nameVector<- wwda2["GeneWeek"]
            

            
            #Create a new table for our paired data for statistical analysis
            nameVector2 <- nameVector
            IDvector <- rep(xValP, times =1, length.out = nrow(dataVector))
            IDvector2 <- rep(yValP, times=1, length.out =  nrow(dataVector2))
            IDvector <- c(as.character(IDvector), as.character(IDvector2))
            names(IDvector) <- "Identification"
            names(IDvector2) <- "Identification"
            pairDataStat <- data.frame(
                
                xValues = rbind(dataVector, dataVector2),
                Identification = IDvector,
                GeneWeek = rbind(nameVector,nameVector2)
            )
            
            if ((xValP == "Percent.Caspase.Coverage.of.Center") | (yValP == "Percent.Caspase.Coverage.of.Center")){
                tryCatch({
                    # This is what I want to do...
                    pairDataStat <- data.frame(
                        xValues = rbind(dataVector, dataVector2),
                        Identification = IDvector,
                        GeneWeek = rbind(nameVector,nameVector2),
                        CellsInCloneCenter = rbind(cval, cval2)
                    )
                    pairDataStat <- pairDataStat[pairDataStat$Cells.In.Clone.Center > 5,]
                },
                # ... but if an error occurs, tell me what happened:
                error=function(error_message) {
                    message("Unable to calculate cells in clone center")
                    return(NA)
                }
                )
            }
            
            pairDataStat <- na.omit(pairDataStat)
            values$pairDataStat<-pairDataStat
            
            #Output the processed data to reactive values
            wwda2 <- na.omit(wwda2)
            values$pairedwwda <- wwda2
            values$pairDataStat <-pairDataStat
            wwda2<- values$wwda2
        }
        
        values$plotOutTitle <- plottitle
        if (input$Title != ""){
            values$plotOutTitle <- input$Title
        }
        
        
        dots <- input$dots
        
        ytitle <- toString(input$y)
        
        values$plotData <- yarp[,c(as.character(narp))]
        #######################################################################################################
        
        
        
        
        
        
        
        ##################################################################################################################################
        ############### Plot Generation ##################################################################################################
        ##################################################################################################################################
        
        
        values$backgroundFill <- input$backgroundFill
        values$backgroundColor <- input$backgroundColor
        values$titleSize <- input$titleSize
        values$axisFont <- input$axisFont
        values$font <- input$font
        
        
        if(input$plotType == "Paired Box Plot"){
            values$facetVal <- ". ~ GeneWeek"
        } else{
            values$facetVal <- ". ~ week"
        }
        
        if (input$facet != ""){
            values$facetVal <- input$facet
        }
        tryCatch({
            values$facetVal <- as.formula(values$facetVal)
        },
        # ... but if an error occurs, tell me what happened:
        error=function(error_message) {
            message("Unable to generate facet formula")
            return(NA)
        }
        )
        
        desfacito <- length(unique(values$wwda2$week))
        if(input$plotType == "Paired Box Plot"){
            desfacito <- length(unique(values$wwda2$GeneWeek))
        }
        
        
        #############################################################################################################
        ##################################### Generate plot##########################################################
        #############################################################################################################
        
        
        
        
        
        tryCatch(
            # This is what I want to do...
            {
                
                #######################################
                #Set the correlation statistical method from user input
                if (input$statsType == "Kendall Tau Correlation"){
                    values$corrMethod <- "kendall"
                } else if (input$statsType ==  "Pearson r Correlation"){
                    values$corrMethod <- "pearson"
                } else {
                    values$corrMethod <- "spearman"
                }
                
                ######################################
                # Set our colors based on user input
                
                if (is.null(input$cgDot)){
                    values$dotColor <- input$dotColor
                } else {
                    if (input$cgDot == "No color grouping"){
                        values$dotColor <- input$dotColor
                    } else {
                        values$dotColor <- values$wwda2[,c( as.character(input$cgDot))]
                    }
                }
                
                
                if (is.null(input$cgDotFill)){
                    values$dotFill <- input$dotFill
                } else {
                    if (input$cgDotFill == "No color grouping"){
                        values$dotFill <- input$dotFill
                    } else {
                        values$dotFill <- values$wwda2[,c( as.character(input$cgDotFill))]
                    }
                }
                
                
                
                
                values$colorv <- input$color
                values$fillv <- input$fill
                if (is.null(input$cgColor)){
                    values$color <- input$color
                } else {
                    if (input$cgColor == "No color grouping"){
                        values$color <- input$color
                    } else {
                        values$color <- values$wwda2[,c( as.character(input$cgColor))]
                    }
                }
                
                if (is.null(input$cgColorFill)){
                    values$fill <- input$fill
                } else {
                    if (input$cgColorFill == "No color grouping"){
                        values$fill <- input$fill
                    } else {
                        values$fill <- values$wwda2[,c( as.character(input$cgColorFill))]
                    }
                }
                
                
                
                
                if(input$plotType == "Paired Box Plot"){
                    
                    if ((xValP == "Percent.Caspase.Coverage.of.Center") | (yValP == "Percent.Caspase.Coverage.of.Center")){
                        
                        pairing <- pairing[pairing$Cells.In.Clone.Center > 5,]
                        
                    }
                    
                    p1<- ggpaired(pairing, cond1 = xValP, cond2 = yValP, id = xValID,
                                  line.color = input$pairedLineColor, line.size = input$pairedLineSize,
                                  palette = "jco",  fill = values$fillv, color = values$colorv)
                    pairedvolo = TRUE
                    
                    
                }     
                else if ((input$plotType == "Histogram" ) | (input$plotType == "Histogram w/ Density Plot")) {
                    
                    p1<- ggplot( values$wwda2 , aes( x= values$plotData, color = GeneName,  fill = GeneName )) 
                    if (input$bins != 1){
                        
                        p1 <- p1 + geom_histogram(alpha=0.6)
                        
                    } else {
                        p1 <- p1 + geom_histogram( binwidth = input$bins, alpha=0.6)
                    }
                    if (input$plotType == "Histogram w/ Density Plot") {
                        p1 <- p1 + geom_density(alpha=0.2, color = values$colorv, fill = values$fillv)
                        
                    }}
                else if ((input$plotType == "Scatter Plot")|(input$plotType == "Scatter Plot w/ Regression Line")){
                    p1 <- ggplot(values$wwda2, aes(x=values$plotData, y=values$plotData2, color=values$dotColor, fill = values$dotFill, shape = values$wwda2$GeneName)) + geom_point(size = input$dots)
                    pairedvolo <- FALSE
                    if (input$plotType == "Scatter Plot w/ Regression Line"){
                        if (input$lm == TRUE){
                            p1 <- p1+geom_smooth(method = lm)
                        } else {
                            p1 <- p1+geom_smooth()
                        }
                        
                    }
                    
                }
                
                else { 
                    
                    
                    pairedvolo <- FALSE
                    p1<- ggplot( values$wwda2 , aes( x= values$wwda2$GeneName  , y= values$plotData,  fill=values$fill, color = values$color )) 
                    
                    if (input$plotType == "Dot Plot w/ Box Plot"){
                        
                        p1<- p1 + geom_boxplot( width=0.4,color = unique(values$color), fill = unique(values$fill) )
                        
                    } 
                    else {
                        p1 <- p1 + geom_violin(color = values$colorv, fill = values$fillv)
                    }
                    if (dots > 0 ){
                        
                        
                        p1 <- p1  + geom_dotplot(binaxis='y' , dotsize = dots , stackdir='center' , stackratio = 1, color = values$dotColor, fill = values$dotFill )
                    }
                    
                }
                
                if ((input$plotType != "Scatter Plot") & (input$plotType !="Scatter Plot w/ Regression Line")) {
                    p1 <-  p1+ scale_x_discrete()
                }
                if ((desfacito > 1) & (input$facetChoice == TRUE)){
                    p1<- p1  +  facet_grid(values$facetVal, scales = input$free_axes)
                }
                
                
                
                #add theme
                p1<- p1 + ggtitle(toString(values$plotOutTitle)) +labs( x= input$xValue, y = ytitle )+ theme(
                    
                    plot.title = element_text(face="bold", size=values$titleSize, family = values$font, hjust=0),
                    panel.background = element_rect(fill = values$backgroundColor,colour = values$backgroundFill, size = 0.5, linetype = "solid"),
                    axis.text.x = element_text(angle = 45 , hjust = 1 , face = "bold" , size = values$axisFont),
                    axis.title=element_text(size=values$axisFont, face="bold"),
                    legend.position = "none",
                    panel.grid.major = element_line(size = input$majorGridSize, linetype = 'solid', colour = input$majorGridColor),
                    panel.grid.minor = element_line(size = input$minorGridsize, linetype = 'solid', colour = input$minorGridColor)
                    
                )
                
                
                if (input$pDisplay == TRUE){
                    if ((input$plotType != "Histogram")&(input$plotType != "Histogram w/ Density Plot") & (input$plotType != "Scatter Plot")&(input$plotType != "Scatter Plot w/ Regression Line")){
                        tryCatch({
                            p1<-p1 + stat_compare_means(paired = pairedvolo, p.adjust.method=input$pAdj, method = values$method)
                        },
                        # ... but if an error occurs, tell me what happened:
                        error=function(error_message) {
                            message("Unable to run plot stats")
                            return(NA)
                        } )
                    }
                    
                }
                
                if(input$xDel == TRUE){
                    p1 <- p1 + theme(axis.text.x=element_blank())
                }
                
                
                if(input$limitx == TRUE){
                    p1 <- p1 + coord_cartesian(xlim = c(input$xMin, input$xMax))
                }
                
                if(input$limity == TRUE){
                    p1 <- p1 + coord_cartesian(ylim = c(input$yMin, input$yMax))
                } 
                if (input$legend == TRUE){
                    p1<- p1 + theme(legend.position=input$legendPosition) + labs(color = input$legendTitle, fill = input$legendTitle2, shape = input$legendTitle3)
                    
                }
                
                
                
                p2 <- p1
                
                #p1 <- p1 +stat_compare_means(method = values$method)
                
                output$plotso<- renderPlot({
                    
                    
                    
                    p1
                    
                    
                    
                    
                })
                
                output$plotso2 <-renderPlot({
                    p2
                })
                
            },
            # ... but if an error occurs, tell me what happened:
            error=function(error_message) {
                message("Could not generate plot")
                return(NA)
            }
        )
        
        
        ###########################################################
        ##
        ##
        ## Fisher test for border vs. center 
        ##
        ##
        ###############################################################
        
        
        
        tryCatch(
            # Try to run border center fishers test
            {
                #Get the death table
                deathTable <- values$deathTable
                
                #Create a loop to run one fisher test per condition
                fishResult <- c()
                fishConfidence <-c()
                fishConfidenceUB <- c()
                fishOddsRatio <- c()
                x <- 0
                lengthotron <- length(deathTable$Genotype)
                while (x < lengthotron ){
                    
                    #Get death table and format it into a 2x2 contingency table
                    gen <- deathTable[x+1, 1]
                    fishers <- matrix(c(deathTable[x+1,2], deathTable[x+1,3], deathTable[x+1,4], deathTable[x+1,5]), ncol = 2)
                    colnames(fishers)<-c("NotDying", "Dying")
                    rownames(fishers)<-c("Border", "Center")
                    fishers<-as.table(fishers)
                    
                    #Run fisher test and store results
                    fishy <- fisher.test(fishers)
                    
                    
                    cfLB<- as.character(c(fishy$conf.int[1]))
                    cfUB <-as.character(c(fishy$conf.int[2]))
                    odds <- as.character(c(fishy$estimate[1]))
                    fishConfidence <- c(fishConfidence, cfLB)
                    fishConfidenceUB <- c(fishConfidenceUB, cfUB)
                    fishOddsRatio <- c(fishOddsRatio, odds)
                    
                    fishResult <- rbind(fishResult, as.character(c(fishy$p.value)))

                    
                    x <- x+1
                }
            
                pAdj <- p.adjust(fishResult, method = input$pAdj, n = length(fishResult))
                colnames(fishResult) <- c("p-value")
                fishResultFinal = data.frame(Genotype = deathTable$Genotype, fishResult, conf.interval.from = fishConfidence, conf.interval.to = fishConfidenceUB, p.adjusted = as.character(pAdj), p.adjustment.method = input$pAdj, odds.ratio = fishOddsRatio)
                output$fishBC <- renderTable(fishResultFinal, hover=TRUE, border=TRUE, spacing="s", digits=5, align ="l")  
                output$fishBCHeader <- renderText("Fisher's Exact Test for dying cells in the border vs. dying cells in the center")
            },
            # Error for unable to run fisher test
            error=function(error_message) {
                message("Unable to run fisher test border vs. center")
                return(NA)
            }
        )
        
        
        ###########################################################
        ##
        ##
        ## Correlation coefficient test
        ##
        ##
        ###########################################################
        if ((input$plotType == "Scatter Plot")|(input$plotType == "Scatter Plot w/ Regression Line")){
             if (input$statsType != "Hotellings T-Squared Test"){
                 tryCatch(
                    # Attempt to run correlations tests
                    {
                        
                        #Get the names and identifiers of all our datasets and datapoints
                        GeneWeeks <- unique(values$wwda2$GeneWeek)
                        
                        #Initialize variables for keeping results and looping through datasets
                        corrResult <- c()
                        corrConditions <- c()
                        corrConditions2 <- c()
                        corrGeneWeek <- c()
                        pVals <- c()
                        x <- 0
                        lengthotron <- length(GeneWeeks)
                        
                        
                        # Loop through each experiment
                        while (x < lengthotron ){
                            
                            #Get the name of the x and y variables, save to the weekname variables
                            weekname <- GeneWeeks[x+1]
                            weekname2 <- GeneWeeks[x+2]
                            
                            #Get just those columns from the dataframe
                            weekData <- values$wwda2 %>% filter(GeneWeek == weekname)
                            
                            #Check how many datapoints there are
                            count <- nrow(weekData)
                            genes <- c(xValP, yValP)
                            
  
                            # Loop through each experimental condition
                            
                            priorGenes <- c()
                            for (genename in genes){
                                
                                for (genename2 in setdiff(genes, priorGenes)) {
                                    if (genename != genename2){
                                        
                                        #Run the correlation test
                                        corry <- cor.test(weekData[,xValP], y = weekData[,yValP], use="pairwise.complete.obs", method = values$corrMethod)
                                        if (values$corrMethod == "pearson"){
                                            #Run the Z-score test for pearsons, if appropriate
                                            zScore <- fisherz(corry$estimate)
                                            corrResult <- rbind(corrResult, as.character(c(corry$p.value, corry$estimate, values$corrMethod, count, zScore)))
                                        } else {
                                            corrResult <- rbind(corrResult, as.character(c(corry$p.value, corry$estimate, values$corrMethod)))
                                        }
                                        
                                        #Add the p value to a vector. We use this for the p adjustment
                                        pVals <- rbind(pVals, corry$p.value)
                                        
                                        
                                        #Store results to a dataframe
                                        corrConditions <- rbind(corrConditions, as.character(genename))
                                        corrConditions2 <- rbind(corrConditions2, as.character(genename2))
                                        corrGeneWeek <- rbind(corrGeneWeek, as.character(weekname))
                                        
                                    }
                                }
                                priorGenes <- c(priorGenes, genename)
                            }
                            x <- x+1
                        }
                        
                        #Output our results
                        if (values$corrMethod == "pearson"){
                            colnames(corrResult) <- c("p-value", "Correlation.Coefficient", "Test", "n", "Z.Score")
                        } else {
                            colnames(corrResult) <- c("p-value", "Correlation.Coefficient", "Test")
                        }
                        
                        #Run our p adjustment and add it to the output table
                        pAdj <- p.adjust(pVals, method = input$pAdj, n = length(pVals))
                        corrResult2 = data.frame(Group = corrGeneWeek, X = corrConditions, Y = corrConditions2, corrResult, p.adjusted = as.numeric(pAdj), p.adjustment.method = input$pAdj)
                        output$statsTable <- renderTable(corrResult2, hover=TRUE, border=TRUE, spacing="s", digits=5, align ="l")
                        
                        pVals<-c()
                        G1 <-c()
                        G2<- c()
                        zVals <-c()
                        priorGenes <-c()
                        if (values$corrMethod == "pearson"){
                            for (i in setdiff(corrGeneWeek, priorGenes)){
                                newData <- corrResult2 %>% filter(Group == i)
                                output$contents <-renderTable(newData, hover=TRUE, border=TRUE, spacing="s", digits=5, align ="l")
                                for (j in corrGeneWeek){
                                    newData2 <- corrResult2 %>% filter(Group == j)
                                    
                                    if (i != j){
                                        z1 <- as.numeric(as.character(newData[1,"Correlation.Coefficient"]))
                                        z2 <- as.numeric(as.character(newData2[1,"Correlation.Coefficient"]))
                                        n1 <- as.numeric(as.character(newData[1,"n"]))
                                        n2 <- as.numeric(as.character(newData2[1,"n"]))
                                        pairo <- paired.r(z1, z2, NULL, n1, n2, twotailed = TRUE )
                                        pVals <- rbind(pVals, pairo$p)
                                        zVals <- rbind(zVals, pairo$z)
                                        G1<- rbind(G1, i)
                                        G2<- rbind(G2, j)
                                        priorGenes <- c(priorGenes, i)
                                        
                                    }
                                }
                            }
                            pAdj <- p.adjust(pVals, method = input$pAdj, n = length(pVals))
                            corrValus <- data.frame(Group1 = G1, Group2 = G2, Z.Value = zVals, p.value = pVals, p.adjusted = as.numeric(pAdj), p.adjustment.method = input$pAdj)
                            output$statsHeaderP <- renderText("Comparison of correlation between groups")
                            output$statsTableP<-renderTable(corrValus, hover=TRUE, border=TRUE, spacing="s", digits=5, align ="l")
                        }
                    },
                    # Error fo if unable to run correlation
                    error=function(error_message) {
                        message("Unable to run correlationTest")
                        return(NA)
                    })
                
            }else{
                tryCatch(
                    # Run Hotellings t2
                    {
                        
                        
                        
                        #Get the names and identifiers of all our datasets and datapoints
                        GeneWeeks <- unique(values$wwda2$GeneWeek)
                        
                        #Initialize variables for keeping results and looping through datasets
                        corrResult <- c()
                        corrConditions <- c()
                        corrConditions2 <- c()
                        corrGeneWeek <- c()
                        corrGeneWeek2 <- c()
                        pVals <- c()
                        priorGenes <- c()
                        newData <- cbind(values$wwda2[xValP], values$wwda2[yValP])
                        newData <- cbind(values$wwda2["GeneWeek"], newData)
                        
                        
                        # Loop through each experiment
                        for (weekname in GeneWeeks){
                            priorGenes <- cbind(priorGenes, weekname)
                            for (weekname2 in setdiff(GeneWeeks, priorGenes)){
                                
                                if (weekname != weekname2){
                                    
                                    newData2 <- newData %>% filter((GeneWeek == weekname) | (GeneWeek == weekname2))
                                    output$contents <- renderTable(newData2, hover=TRUE, border=TRUE, spacing="s", digits=5, align ="l")
                                    
                                    newData.vars <- newData2 %>% select(-one_of("GeneWeek"))
                                    multiShap <- mvnormtest::mshapiro.test(t(newData.vars))
                                    varCoVar <- det(cov(newData.vars))
                                    
                                    if ((as.numeric(multiShap$p.value) > 0.05) & (as.numeric(varCoVar) > 0)){
                                        assumptions <- "Assumptions met"
                                    } else {
                                        assumptions <- "Assumptions violated"
                                    }
                                    
                                    
                                    
                                    hotellings <- HotellingsT2(filter(newData2, GeneWeek == weekname )[,2:3],
                                                               filter(newData2, GeneWeek == weekname2 )[,2:3])
                                    corrResult <- rbind(corrResult, as.character(c(multiShap$p.value, varCoVar, assumptions, hotellings$p.value, "Hotellings T-squared Test")))
                                    corrConditions <- rbind(corrConditions, as.character(xValP))
                                    corrConditions2 <- rbind(corrConditions2, as.character(yValP))
                                    corrGeneWeek <- rbind(corrGeneWeek, as.character(weekname))
                                    corrGeneWeek2 <- rbind(corrGeneWeek2, as.character(weekname2))
                                    pVals <- cbind(pVals, hotellings$p.value)
                                }
                            }
                        }
                        
                        #Output our results
                        colnames(corrResult) <-c("Multivariate shapiro p-value", "Determinant of Variance-Covariance Matrix", "Have test assumptions been met?", "Means comparison p-value", "Test")#                        #Run p adjustment and add to table
                        pAdj <- p.adjust(pVals, method = input$pAdj, n = length(pVals))
                        corrResult2 = data.frame(Group1 = corrGeneWeek, Group2 = corrGeneWeek2, X = corrConditions, Y = corrConditions2, corrResult, p.adjusted = as.numeric(pAdj), p.adjustment.method = input$pAdj)
                
                        output$statsTable <- renderTable(corrResult2, hover=TRUE, border=TRUE, spacing="s", digits=5, align ="l")
                
                    },
                    # Error for if unable to run t2
                    error=function(error_message) {
                        message("Unable to run hotelling Test")
                        return(NA)
                    }
                )}
        }
        
        ###########################################################
        ##
        ##
        ## Equivalence of Variance test
        ##
        ##
        ###########################################################
        
        
        if((input$plotType != "Scatter Plot") & (input$plotType != "Scatter Plot w/ Regression Line")){
            
            tryCatch(
                # Try statement for equivalence of variance test
                {
                    
                    
                    
                    #Get the names and identifiers of all our datasets and datapoints
                    flignerData <- values$wwda2[,c(plottitle, "GeneName", "week", "GeneWeek")]
                    flilgnerData <- na.omit(flignerData)
                    weeks <- unique(values$wwda2$week)
                    if (input$plotType == "Paired Box Plot"){
                        weeks <- unique(values$wwda2$GeneWeek)
                    }
                    
                    #Initialize variables for keeping results and looping through datasets
                    flignerResult <- c()
                    fligConditions <- c()
                    fligConditions2 <- c()
                    x <- 0
                    lengthotron <- length(weeks)
                    
                    
                    # Loop through each experiment
                    while (x < lengthotron ){
                        weekname <- weeks[x+1]
                        if (input$plotType == "Paired Box Plot"){
                            weekData <- values$wwda2 %>% filter(GeneWeek == weekname)
                            genes <- c(xValP, yValP)
                        } else {
                            weekData <- flignerData %>% filter(week == weekname)
                            genes <- unique(weekData$GeneName)
                        }
                        
                        # Loop through each experimental condition
                        priorGenes <- c()
                        for (genename in genes){
                            
                            for (genename2 in setdiff(genes, priorGenes)) {
                                
                                if (genename != genename2){
                                    
                                    if (input$plotType == "Paired Box Plot"){
                                        
                                        flignerVector1 <- c(weekData[, genename], weekData[, genename2])
                                        filler <- length(flignerVector1) / 2
                                        flignerVector2 <- c(rep(genename, filler), rep(genename2, filler))
                                        
                                    } else {
                                        geneData <- weekData %>% filter(GeneName %in% c(genename, genename2))
                                        flignerVector1 <- geneData[,plottitle]
                                        flignerVector2 <- as.factor(geneData[,"GeneName"])
                                    }
                                    
                                    fliggy <- fligner.test(flignerVector1 ~ flignerVector2)
                                    flignerResult <- rbind(flignerResult, as.character(c(fliggy$p.value)))
                                    fligConditions <- rbind(fligConditions, as.character(genename))
                                    fligConditions2 <- rbind(fligConditions2, as.character(genename2))
                                    
                                }
                                
                            }
                            
                            
                            priorGenes <- c(priorGenes, genename)
                        }
                        
                        x <- x+1
                        
                    }
                    
                    #Output our results
                    colnames(flignerResult) <- c("p-value")
                    fligResult2 = data.frame(Group1 = fligConditions, Group2 = fligConditions2, flignerResult)
                    output$fligner <- renderTable(fligResult2, hover=TRUE, border=TRUE, spacing="s", digits=5, align ="l")
                    output$flignerHeader <- renderText(plottitle)
                    
                    
                },
                # Error message for when unable to run equivalence of variance test
                error=function(error_message) {
                    message("Unable to run flignerTest")
                    return(NA)
                }
            )
        }
        
        ####################################################################################
        #
        #
        # Run shapiro-wilks normality test
        #
        #
        ####################################################################################
        if( input$statsType != "Hotellings T-Squared Test"){
            tryCatch(
                # Try statement for shapiro-wilks test
                {
                    #Get the data from reactive values
                    wwda2 <- values$wwda2
                    
                    #Run the shapiro test
                    shapiroRes <- tapply(values$wwda2[,plottitle] , values$wwda2$GeneWeek , shapiro.test)
                    shapiroP <- c()
                    
                    
                    for (i in shapiroRes) {
                        z <- 1
                        for (a in i){
                            if (z == 2){
                                pval <- ifelse(nchar(a) > 6, paste0(strtrim(a, 6), '...'), a)
                                shapiroP <- rbind(shapiroP, as.character(pval))
                            }
                            z<- z+1
                            
                        }
                    }
                    
                    ifelse(nchar(a) > 13, paste0(strtrim(a, 10), '...'), a)
                    shapTable <- data.frame (
                        GeneWeek = unique(values$wwda2$GeneWeek),
                        p.value = shapiroP
                    )
                    output$normality <- renderTable(shapTable, hover=TRUE, border=TRUE, spacing="s", digits=5, align ="l")
                    output$normalityHeader <- renderText(plottitle) 
                    
                    
                    if ((input$plotType == "Paired Box Plot") | (input$plotType == "Scatter Plot") | (input$plotType == "Scatter Plot w/ Regression Line")){
                        
                        wwda2 <- values$wwda2
                        
                        shapiroRes <- tapply(values$wwda2[,input$plotterpaired] , values$wwda2$GeneWeek , shapiro.test)
                        shapiroP <- c()
                        
                        for (i in shapiroRes) {
                            z <- 1
                            for (a in i){
                                if (z == 2){
                                    pval <- ifelse(nchar(a) > 6, paste0(strtrim(a, 6), '...'), a)
                                    shapiroP <- rbind(shapiroP, as.character(pval))
                                }
                                z<- z+1
                                
                            }
                        }
                        ifelse(nchar(a) > 13, paste0(strtrim(a, 10), '...'), a)
                        shapTable <- data.frame (
                            GeneWeek = unique(values$wwda2$GeneWeek),
                            p.value = shapiroP
                        )
                        output$normality2 <- renderTable(shapTable, hover=TRUE, border=TRUE, spacing="s", digits=5, align ="l")
                        output$normalityHeader2 <- renderText(input$plotterpaired)
                    }
                },
                # Error for unable to run shapiro wilks test
                error=function(error_message) {
                    message("Unable to run Shapiro-Wilks test")
                    return(NA)
                }
            )
        }
        
        
        
        output$effectSizeTable <- renderTable(data.frame(), hover=TRUE, border=TRUE, spacing="s", digits=5, align ="l")
        output$effectSizeHeader <- renderText("")

        tryCatch(
            # Try statement for effect size
            {
              estimates <- c()
              confHigh <- c()
              confLow <- c()
              magnitudes <- c()
              effectConditions <- c()
              effectConditions2 <-c()
              plotformula <- as.formula(plotformula)
              
              #Get the names and identifiers of all our datasets and datapoints
              effectData <- values$wwda2[,c(plottitle, "GeneName", "week", "GeneWeek")]
              effectData <- na.omit(effectData)
              weeks <- unique(values$wwda2$week)
              if (input$plotType == "Paired Box Plot"){
                  weeks <- unique(values$wwda2$GeneWeek)
              }
              
              x <- 0
              lengthotron <- length(weeks)
              
              
              # Loop through each experiment
              while (x < lengthotron ){
                  weekname <- weeks[x+1]
                  if (input$plotType == "Paired Box Plot"){
                      weekData <- effectData %>% filter(GeneWeek == weekname)
                      genes <- c(xValP, yValP)
                      pairing = TRUE
                  } else {
                      weekData <- effectData %>% filter(week == weekname)
                      genes <- unique(weekData$GeneName)
                      pairing = FALSE
                  }
                  
                  # Loop through each experimental condition
                  priorGenes <- c()
                  for (genename in genes){
                      
                      for (genename2 in setdiff(genes, priorGenes)) {
                          
                          if (genename != genename2){
                              
              
                
                                  if (input$effectSize == "Cohen's D"){ 
                                    hedges <- FALSE
                                    pooler <- TRUE
                                    mode <- 1
                                  } else if ( input$effectSize == "Cohen's D - Not Pooled"){
                                    hedges <- FALSE
                                    pooler <-FALSE
                                    mode <- 1
                                  } else if (input$effectSize == "Hedges' G"){
                                    hedges <- TRUE
                                    pooler <- TRUE
                                    mode <- 1
                                  } else if (input$effectSize == "Hedges' G - Not Pooled") {
                                    hedges <- TRUE
                                    pooler <- FALSE
                                    mode <- 1
                                  } else {
                                    mode <- 2
                                  }
                                  
                                  weekData <- weekData[(weekData$GeneName == genename) |(weekData$GeneName == genename2), ]
                                
               
                                  
                                  effectSize <- cohen.d(plotformula, data=weekData, pooled=pooler, hedges.correction = hedges, paired = pairing, conf.level=0.95, na.rm = TRUE )
                                  estimates <- c(estimates, effectSize$estimate)
                                  magnitudes <- c(magnitudes, effectSize$magnitude)
                                  effectConditions <- rbind(effectConditions, as.character(genename))
                                  effectConditions2 <- rbind(effectConditions2, as.character(genename2))
                                  conf <- effectSize$conf.int
                                  confLow<- c(confLow, effectSize$conf.int[1])
                                  confHigh <- c(confLow, effectSize$conf.int[2]) 
                                  confHigh <- confHigh[2]
                                 
              
                                  
                              
                              
                              
                          }
                          
                          
                      }
                      priorGenes <- c(priorGenes, genename)
                  } 
                  x <- x+1
              }
              
              
              effectSizeRes = data.frame(Group1 = effectConditions, Group2 = effectConditions2, method = input$effectSize, estimate = estimates, magnitude = magnitudes, conf.interval.low = confLow, conf.interval.high = confHigh)
                
              output$effectSizeTable <- renderTable(effectSizeRes, hover=TRUE, border=TRUE, spacing="s", digits=5, align ="l")
              output$effectSizeHeader <- renderText(paste("Effect Size Estimates:", input$effectSize, sep=" "))

            },
        # Error message for when unable to run effect size
        error=function(error_message) {
            message("Unable to run effect size tests")
            return(NA)
        }
        )
        

       
        
        
        
        tryCatch(
            # Try to run compare means.
            {
                if ((input$plotType != "Scatter Plot")&(input$plotType != "Scatter Plot w/ Regression Line")){
                    
                    if (pairedvolo == FALSE){
                        plotformula <- as.formula(plotformula)
                        values$statistics <- compare_means(plotformula, data = values$wwda2, method = values$method, p.adjust.method=input$pAdj,  group.by= "week")

                        output$statsTable <- renderTable(values$statistics, hover=TRUE, border=TRUE, spacing="s", digits=5, align ="l")  
                        output$statsHeader <- renderText("Statistical test:")
                    } else {
                        
                        plotformula2 <- as.formula(values$plotformula2)
                        values$statistics <- compare_means( xValues ~ Identification, data = values$pairDataStat, method = values$method, p.adjust.method=input$pAdj, group.by= "GeneWeek",  paired = TRUE)
                        output$statsHeader <- renderText("Pair-wise statistical test:")
                        output$statsTable <- renderTable(values$statistics, hover=TRUE, border=TRUE, spacing="s", digits=5, align ="l")  
                    }
                }
                
            },
            # Error message for failing to run compare means
            error=function(error_message) {
                message("Could not run compare means")
                return(NA)
            }
        )
        
        #############################################################
        ##
        ##
        ## Fisher test for comparing border death between groups
        ##
        ##
        #############################################################
        
        tryCatch(
            # Try to run the fishers test
            {
                #Get the cell death count data table
                deathTable <- values$deathTable
                
                #Initialize variables to hold our results
                fishResult <- c()
                fishConditions <- c()
                fishConditions2 <-c()
                fishConfidence <-c()
                fishConfidenceUB <- c()
                fishOddsRatio <- c()
                
                x <- 0
                
                #Loop through each row of the death table
                lengthotron <- length(deathTable$Genotype)
                while (x < lengthotron ){
                    
                    y <- x+1
                    while (y < lengthotron){
                        #Find genotype of the groups we are comparing
                        gen <- deathTable[x+1, 1]
                        gen2 <- deathTable[y+1, 1]
                        
                        #format these rows into a 2x2 contingency table and add headers
                        fishers <- matrix(c(deathTable[x+1,2], deathTable[y+1,2], deathTable[x+1,3], deathTable[y+1,3]), ncol = 2)
                        colnames(fishers)<-c("NotDying", "Dying")
                        rownames(fishers)<-c("Condition1", "Condition2")
                        fishers<-as.table(fishers)
                        
                        #Run fisher test
                        fishy <- fisher.test(fishers)
                        
                        cfLB<- as.character(c(fishy$conf.int[1]))
                        cfUB <-as.character(c(fishy$conf.int[2]))
                        odds <- as.character(c(fishy$estimate[1]))
                        fishConfidence <- c(fishConfidence, cfLB)
                        fishConfidenceUB <- c(fishConfidenceUB, cfUB)
                        fishOddsRatio <- c(fishOddsRatio, odds)
                        
                        #Store outputs
                        fishResult <- rbind(fishResult, as.character(c(fishy$p.value)))
                        fishConditions <- rbind(fishConditions, as.character(gen))
                        fishConditions2 <- rbind(fishConditions2, as.character(gen2))
                        
                        y <- y+1
                        
                    }
                    
                    x<- x+1
                }
                
                
                
                #Run and add our p-adjustment
                pAdj <- p.adjust(fishResult, method = input$pAdj, n = length(fishResult))
                colnames(fishResult) <- c("p-value")
                fishResult2Z = data.frame(Group1 = fishConditions, Group2 = fishConditions2, fishResult, conf.interval.from = fishConfidence, conf.interval.to = fishConfidenceUB, p.adjusted = as.numeric(pAdj), p.adjustment.method = input$pAdj, odds.ratio=fishOddsRatio)
                output$fishTable <- renderTable(fishResult2Z, hover=TRUE, border=TRUE, spacing="s", digits=5, align ="l")  
                output$fishHeader <- renderText("Fisher's Exact Test Between Samples")
                
            },
            # Error message for failing to run fisher test between groups
            error=function(error_message) {
                message("Could not run fisher test between groups")
                return(NA)
            }
        )
        
    }
    )
}
