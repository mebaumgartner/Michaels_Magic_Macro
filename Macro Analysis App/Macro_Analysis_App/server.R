


############################################################################################################################################################
############################################################################################################################################################
#############################################################################################################################################################

## This is the app server, i.e. the part of the app that does all the background analysis

server <- function(input, output) {
  #Assign some reactive values to hold outputs and key variables
  ################################################################################################################
  values = reactiveValues()
  valuesRestore <-
    reactiveValues() #This is a backup set of the values which we use when re-loading/overwriting data
  values$choices <-
    c("First, analyze your data!") #This is a list of column header titles from the analyzed datatable
  values$control2 <-
    c("None", "All/Basemean") #This is the choices to be displayed in the 'select reference group' button
  values$rawChoices <-
    c("First, analyze your data!") #This is a list of column header titles from the analyzed datatable, without a few options removed
  values$genAssign <-
    data.frame(Upload = c("First, upload your data"),
               Assign = c("Then, click 'Assign Genotypes'")
               ) #This is the reactive data table for assigning genotypes
  values$statistics <- data.frame()
  values$loaded <- 0 #This is the data loaded into the app
  values$addCount <- 0
  values$refString <- ""
  values$switchO <- FALSE
  values$cols2 <- c()
  output$plotso <-
    renderUI({
      img(src = "Logo.png",
          height = "65%",
          width = "65%")
    }) #This is the logo used in place of the plot
  output$statsTable <-
    renderTable(
      values$statistics,
      hover = TRUE,
      border = TRUE,
      spacing = "s",
      digits = 5,
      align = "l"
    )
  values$singleCellDf <- data.frame()
  values$switchAnalysis <-
    FALSE #Detect if a whole disc analysis csv has been uploaded, and used to render a UI
  values$newFactors <- c("First, run your regression analysis!")
  values$zGroup <- "N/A"
  
  
  #Here we make reactive user input boxes
  #################################################################################################################
  
  
  #Create a UI based on the analyzed values put in. This one is for choosing the first variable to be graphed/plotted
  output$plotSelection = renderUI({
    #The choices are most of the column headers for our analyzed data table
    mydata = values$choices
    
    #In the case that we are looking at Z- or Timepoint data, this value is fixed for the x-axis
    if (isTRUE(values$switchO == TRUE)) {
      mydata2 <- mydata
      mydata <- mydata2[grepl("TimePoint", mydata2)]
      if (isTRUE(input$sepTimePoints ==  "Z-axis analysis")) {
        mydata <- mydata2[grepl("z.level.", mydata2)]
        
      }
    }
    selectInput('plotter', 'Select variable to plot', mydata)
  })
  
  #This button lists the kinds of plots that we can make depending on user input
  output$plotType = renderUI({
    if (isTRUE(values$switchO == FALSE)) {
      choiceO <-
        c(
          "Dot Plot w/ Box Plot",
          "Violin Plot",
          "Paired Box Plot",
          "Histogram",
          "Scatter Plot",
          "Scatter Plot w/ Regression Line"
        )
    } else {
      choiceO <- c("Line Plot")
    }
    selectInput("plotType", "Specify plot format:", choices = choiceO)
  })
  
  #This renders the analysis type selection button
  output$analysisType = renderUI({
    validate(need(values$switchAnalysis == TRUE, ""))
    selectInput(
      "sepTimePoints",
      "Specify Analysis Type:",
      c(
        "Default",
        "Time-lapse analysis",
        "Z-axis analysis",
        "Complete single cell analysis",
        "Complete single cell analysis - timelapse"
      )
    )
  })
  
  
  #This generates the list of checkboxes on the analyzed data-table page which toggles columns to show
  output$checkChoices = renderUI({
    #The values are all the column headers of the analyzed data table
    vchoices <- values$rawChoices
    checkboxGroupInput("columns",
                       "Select columns to display",
                       choices = vchoices,
                       inline = T)
  })
  
  #This generates the list of checkboxes on the regression analysis page which toggles plots to show
  output$checkChoicesReg = renderUI({
    #The values are all the column headers of the analyzed data table
    vchoices <- values$newFactors
    checkboxGroupInput("columnsReg",
                       "Select columns to display",
                       choices = vchoices,
                       inline = T)
  })
  #This generates the list of checkboxes on the regression analysis page which toggles plots to show
  output$checkChoicesReg2 = renderUI({
    #The values are all the column headers of the analyzed data table
    vchoices <- c("N/A", values$newFactors)
    selectInput("columnsReg2", "Group results by:", choices = vchoices)
  })
  
  #This specifies which image to have the order of it's z-plane values inverted for the button on the analyzed data table
  output$checkInversion = renderUI({
    if (isFALSE(input$sepTimePoints == "Default")) {
      vchoices2 <- values$vchoices2
      checkboxGroupInput("columns2",
                         "Select columns to invert",
                         choices = vchoices2,
                         inline = TRUE)
    }
  })
  
  #This button lists the statistical tests available to be run based on the kind of plot selected
  output$stats = renderUI({
    if (isTRUE((input$plotType == "Scatter Plot") |
               (input$plotType == "Scatter Plot w/ Regression Line")
    )) {
      statsOptions <-
        c(
          "Pearson r Correlation",
          "Kendall Tau Correlation",
          "Spearman's Rho Correlation"
        )
    } else if (isTRUE(input$plotType == "Line Plot")) {
      statsOptions <- c('N/A')
    } else {
      statsOptions <- c("wilcox.test", "t.test", "kruskal.test", "anova")
    }
    selectInput('statsType', 'Select statistical test', statsOptions)
  })
  
  #This button lists the statistical tests available to be run based on the kind of plot selected
  output$effectSize = renderUI({
    if (isTRUE((input$plotType == "Scatter Plot") |
               (input$plotType == "Scatter Plot w/ Regression Line") |
               (input$plotType == "Line Plot")
    )) {
      effOptions <- c("N/A")
    } else {
      effOptions <-
        c(
          "Cliff's Delta",
          "Cohen's D - Pooled",
          "Cohen's D - Not Pooled",
          "Hedges' G - Pooled",
          "Hedges' G - Not Pooled"
        )
    }
    selectInput('effectSize', 'Specify effect size output', effOptions)
  })
  
  
  
  #Create control group selection
  output$controlGroup = renderUI({
   

    controlOptions <- values$control2
    print ("Generating interface variables")
    controlOptions <- unique( c(controlOptions, "None", "All/Basemean"))

    if (isTRUE(input$plotType == "Paired Box Plot")) {
      controlOptions = c("N/A")
    }
    selectInput('refgroup',
                'Specify control/reference group',
                controlOptions)
  })
  
  
  #Create our regression variable options
  output$regressionSelect1 = renderUI({
    if (input$linkFunction == "Logistic") {
      if (((
        isTRUE(input$sepTimePoints == "Complete single cell analysis")
      ) |
      isTRUE((
        input$sepTimePoints == "Complete single cell analysis - timelapse"
      )
      ))) {
        regTitle <- "Select dependent variable"
        regOptions1 <- values$regchoices
        output$LogRegressionText <- NULL
      } else {
        regTitle <- "Select Negative Dependent Variable:"
        regOptions1 <- values$choices
        output$LogRegressionText <-
          renderText(
            "Acceptable inputs are (1)  data columns containing the probability of success, (2) a column where each cell is marked as either a success or a failure,  or (3) two separate data columns - one with the number of failures and another with a number of successes. If entering data of type (3), specify the failure count in this field and the success count in the field below."
          )
      }
    } else if (input$linkFunction == "Poisson") {
      regTitle <- "Select dependent variable"
      regOptions1 <- values$choices
      output$LogRegressionText <- NULL
      
    } else {
      regTitle <- "Select Dependent variable"
      regOptions1 <- values$choices
      output$LogRegressionText <- NULL
    }
    selectInput('regressionParameter1', regTitle, regOptions1)
  })
  
  #This is our second regression variable option
  output$regressionSelect2 = renderUI({
    validate(need((input$linkFunction == "Logistic") , ""))
    if (((
      isTRUE(input$sepTimePoints == "Complete single cell analysis")
    ) |
    isTRUE((
      input$sepTimePoints == "Complete single cell analysis - timelapse"
    )
    ))) {
      validate(need((input$excludecheckreg == TRUE) &&
                      (input$excludecheckreg2 == TRUE),
                    ""
      ))
    }
    
    regOptions2 <- values$choices
    
    regOptions2 <- c("None / Not Applicable", regOptions2)
    selectInput('regressionParameter2',
                'Select Positive Dependent Variable:',
                regOptions2)
  })
  output$exclusioninputreg4 = renderUI({
    validate(need((input$linkFunction == "Logistic") &&
                    (((input$sepTimePoints == "Complete single cell analysis") |
                        (
                          input$sepTimePoints == "Complete single cell analysis - timelapse"
                        )
                    )), ""))
    checkboxInput('excludecheckreg2',
                  'Filter dependent variable data?',
                  FALSE)
  })
  
  #This is our second regression variable option
  output$regressionSelectExclude = renderUI({
    validate(need((input$linkFunction == "Logistic") &&
                    (((input$sepTimePoints == "Complete single cell analysis") |
                        (
                          input$sepTimePoints == "Complete single cell analysis - timelapse"
                        )
                    )) && (input$excludecheckreg2 == TRUE),
                  ""
    ))
    regOptions5 <- values$choices
    selectInput('regressionParameterExclude',
                'Select variable to exclude by',
                regOptions5)
  })
  
  
  output$exclusioninputreg = renderUI({
    validate(need((input$linkFunction == "Logistic") &&
                    (((input$sepTimePoints == "Complete single cell analysis") |
                        (
                          input$sepTimePoints == "Complete single cell analysis - timelapse"
                        )
                    )) && (input$excludecheckreg2 == TRUE),
                  ""
    ))
    selectInput('excludeOperatorReg',
                'Operation:',
                choices = c(">", ">=", "<", "<=", "==", "!="))
  })
  
  
  
  output$exclusioninputreg3 = renderUI({
    validate(need((input$linkFunction == "Logistic") &&
                    (((input$sepTimePoints == "Complete single cell analysis") |
                        (
                          input$sepTimePoints == "Complete single cell analysis - timelapse"
                        )
                    )) && (input$excludecheckreg2 == TRUE),
                  ""
    ))
    checkboxInput('excludecheckreg',
                  'Parform comparison against another column?',
                  FALSE)
  })
  
  
  output$exclusioninputreg2 = renderUI({
    validate(need((input$linkFunction == "Logistic") &&
                    (((input$sepTimePoints == "Complete single cell analysis") |
                        (
                          input$sepTimePoints == "Complete single cell analysis - timelapse"
                        )
                    )) &&
                    (input$excludecheckreg == FALSE) &&
                    (input$excludecheckreg2 == TRUE),
                  ""
    ))
    numericInput('excludeValuereg', 'Specify comparison value:', 0)
  })
  
  
  
  
  
  #These are all of the options that can be used as terms or factors in logistic regression
  output$factorCheckboxes = renderUI({
    factorChoices <- values$rawChoices
    factorValues <- c(1:length(factorChoices))
    checked <-
      grep(
        "ConditionAndExperiment|ROI.Volume|Percent.Clone.Coverage.of.ROI",
        factorChoices
      )
    checkboxGroupInput(
      'factorCheckboxes',
      'Select variables to include as factors:',
      choiceNames = factorChoices,
      choiceValues = factorValues,
      selected = checked
    )
  })
  
  #This code controls the download handler for the analyzed data table
  output$downloadData <- downloadHandler(
    #This is the default name
    filename = function() {
      paste("data-", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      write.csv(values$wwda2, file)
    }
  )
  
  #This code controls the download handler for the plot
  output$plotDownload = downloadHandler(
    
    
    # #This is the default name
    filename = function () {
      paste("Wing_Disc_Analysis_Plot_", Sys.Date(), ".pdf", sep = "")
    },
    
    content = function(file){
      cairo_pdf(filename = file,
                width = input$plotwidth, height = input$plotheight, bg = "transparent",
                antialias = "subpixel",fallback_resolution = input$plotres)
      plot(values$plotso)
      dev.off()
    },
    
    contentType = "application/pdf"
    
    

    # content = function(file) {
    #   device <- function(..., width, height) {
    #     grDevices::png(
    #       ...,
    #       width = input$plotwidth,
    #       height = input$plotheight,
    #       res = input$plotres,
    #       units = input$plotUnit
    #     )
    #   }
    #   ggsave(file, plot = values$plotso, device = device)
    # }
  )
  
  #This code controls the download handler for the regression plot
  output$plotDownloadReg = downloadHandler(
    
    # #This is the default name
    filename = function () {
      paste("Wing_Disc_Analysis_Plot_", Sys.Date(), ".pdf", sep = "")
    },
    
    content = function(file){
      cairo_pdf(filename = file,
                width = input$plotwidthreg, height = input$plotheightreg, bg = "transparent",
                antialias = "subpixel",fallback_resolution = input$plotresreg)
      plot(
              predictorEffects(values$regression, values$formulaP),
               lines = list(multipline = TRUE),
               type = "response"
             )
      dev.off()
    },
    
    contentType = "application/pdf"
    # #This is the default name
    # filename = function () {
    #   paste("Regression_Effects_Plot_", Sys.Date(), ".png", sep = "")
    # },
    # content = function(file) {
    #   device <- function(..., width, height) {
    #     grDevices::png(
    #       ...,
    #       width = input$plotwidthreg,
    #       height = input$plotheightreg,
    #       res = input$plotresreg,
    #       units = input$plotUnitreg
    #     )
    #   }
    #   ggsave(
    #     file,
    #     plot =  plot(
    #       predictorEffects(values$regression, values$formulaP),
    #       lines = list(multipline = TRUE),
    #       type = "response"
    #     ),
    #     device = device
    #   )
    # }
  )
  
  #This generates the second set of choices for plotting, if
  output$plotPaired = renderUI({
    validate(need((input$plotType == "Paired Box Plot") |
                    (input$plotType == "Scatter Plot") |
                    (input$plotType == "Scatter Plot w/ Regression Line") |
                    (input$plotType == "Line Plot") |
                    (input$plotType == "Histogram") |
                    (input$plotType == "Histogram w/ Density Plot"),
                  "Needs two variables selection"
    ))
    mydata2 = values$choices
    
    if ((input$plotType == "Histogram") |
        (input$plotType == "Histogram w/ Density Plot")) {
      mydata2 <- c("N/A", mydata2)
    }
    
    
    
    selectInput('plotterpaired', 'Select second variable to plot', mydata2)
  })
  
  
  
  #Render User Input widgets for if the user wants to perform a data operation
  output$ColumnChoice = renderUI({
    myChoices = values$choices
    selectInput('dataToOperate', 'Select data to transform', myChoices)
  })
  
  output$columnOrFixed = renderUI({
    if (input$valueOrColumn == TRUE) {
      myChoices2 = values$choices
      selectInput('operatorColumn', 'Select second dataset', myChoices2)
    } else {
      numericInput('operatorValue', 'Input operator value', value = 1)
    }
  })
  
  #This is the single cell analysis csv upload button
  output$secondCsv = renderUI({
    validate(need(((input$sepTimePoints == "Complete single cell analysis") |
                     (
                       input$sepTimePoints == "Complete single cell analysis - timelapse"
                     )
    ), ""))
    fileInput("csvs2", label = NULL, multiple = TRUE)
    
  })
  
  valuesRestore <- values
  
  
  
  #Here we have the 'assign experimental conditions button'
  ######################################################################################################################################################
  #Here we generate the reactive input table for genotype assignments
  observeEvent(input$saveBtn, {
    randoTable <-
      TRUE  #This variable tracks if an unformatted table has been uploaded
    wda <-
      data.frame(Image.Name = c(1, 2, 3, 4, 5)) #This is a placeholder for our first dataframe, wda for Experiment-data-assign
    
    
    #I make liberal use of tryCatch loops to account for how many possible combinations of user inputs go on in this program. Go ahead, sue me
    
    #we accept the imported data and format it to a dataframe, wda
    #The first loop is for if a clone analysis data file has been loaded
    tryCatch({
      values <- valuesRestore
      wda <-
        rbindlist(
          lapply(input$csvs$datapath, read.csv),
          use.names = TRUE,
          fill = TRUE
        )
      Image.Name <- unique(wda$Image.Name)
      
      
      
      values$genAssign <- data.frame(Image.Name)
      values$genAssign$Experiment <- "Specify experiment/grouping"
      values$genAssign$Condition <- "Specify condition/genotype"
      if (colnames(wda)[1] != "X"){
        randoTable <- FALSE
      }
    },
    
    error = function(error_message) {
      message("No data file loaded")
      return(NA)
    })
    
    #This one is for if a wing disc analysis file has been loaded
    tryCatch({
      wda <-
        rbindlist(
          lapply(input$csvs$datapath, read.csv),
          use.names = TRUE,
          fill = TRUE
        )
      
      FileImage <-
        data.frame(File = wda$File,
                   Image.Name = wda$Image.Name)
      
      FileImage[with(FileImage, order(File, Image.Name)),]
      
      FileImage <- data.frame(unique(FileImage))
      
      values$genAssign <- FileImage
      values$genAssign$Experiment <- "Specify experiment/grouping"
      values$genAssign$Condition <- "Specify condition/genotype"
      if (colnames(wda)[1] != "X"){
  
        randoTable <- FALSE
        values$switchAnalysis <- TRUE
      }
      
    },
    
    error = function(error_message) {
      message("Couldn't create genotype assignment table")
      return(NA)
    })
    
    
    
    
    #If an unformatted dataframe was loaded in, we adapt it to be compatible with the app
    tryCatch({
      if (randoTable == TRUE) {
        wda <-
          rbindlist(
            lapply(input$csvs$datapath, read.csv, stringsAsFactors = FALSE),
            use.names = TRUE,
            fill = TRUE
          )
        
        ID <- 1:nrow(wda)
        if (colnames(wda)[2:3] != c("File", "Image.Name")){
            Experiment <-
              rep("Specify experiment/grouping", nrow(wda))
            Condition <- rep("Specify genotype", nrow(wda))
            wda <- cbind(ID, Experiment, Condition, wda)
        }
        values$genAssign <- data.frame(wda)
      }
    },
    
    error = function(error_message) {
      message("Couldn't create and load unformatted data table")
      return(NA)
    })
    values$randoTable <- randoTable
    
    
  })
  
  #We render the table for modification by the user
  output$table <- renderRHandsontable({
    genAssign <- values$genAssign
    rownames(genAssign) <- NULL
    rhandsontable(genAssign)
  })
  
  
  ##############################################################################################################################
  
  
  
  
  ############################################################################################################################
  ##################################################### Re-Load analyzed data ###################################################
  ############################################################################################################################
  
  
  
  analysis <- observeEvent(input$go, {
    #Reset all our outputs so we don't get confused
    output$statsTable <-
      renderTable(
        data.frame(),
        hover = TRUE,
        border = TRUE,
        spacing = "s",
        digits = 5,
        align = "l"
      )
    output$statsTableP <-
      renderTable(
        data.frame(),
        hover = TRUE,
        border = TRUE,
        spacing = "s",
        digits = 5,
        align = "l"
      )
    output$contents <-
      renderTable(
        data.frame(),
        hover = TRUE,
        border = TRUE,
        spacing = "s",
        digits = 5,
        align = "l"
      )
    output$deathTable <-
      renderTable(
        data.frame(),
        hover = TRUE,
        border = TRUE,
        spacing = "s",
        digits = 5,
        align = "l"
      )
    output$statsHeader <- renderText("")
    output$statsHeaderP <- renderText("")
    output$normality <-
      renderTable(
        data.frame(),
        hover = TRUE,
        border = TRUE,
        spacing = "s",
        digits = 5,
        align = "l"
      )
    output$normality2 <-
      renderTable(
        data.frame(),
        hover = TRUE,
        border = TRUE,
        spacing = "s",
        digits = 5,
        align = "l"
      )
    output$normalityHeader <- renderText("")
    output$normalityHeader2 <- renderText("")
    output$fligner <- renderTable(data.frame())
    output$leveneHeader <- renderText("")
    output$fishyPrint <- renderText("")
    output$siggy <-
      renderTable(
        data.frame(),
        hover = TRUE,
        border = TRUE,
        spacing = "s",
        digits = 5,
        align = "l"
      )
    output$fishyCounty <-
      renderTable(
        data.frame(),
        hover = TRUE,
        border = TRUE,
        spacing = "s",
        digits = 5,
        align = "l"
      )
    output$plotso <-
      renderUI({
        img(src = "Logo.png",
            height = "65%",
            width = "65%")
      })
    
    output$corrMat <- NULL
    output$vifHeader <- NULL
    output$regressionVif <- NULL
    
    
    tryCatch({
      values$loaded <- 1
      values$control <- toString(input$reference)
      values$genAssign <- hot_to_r(input$table)
      
      #Read dataset csv files to the variable 'wda'
      wwda2 <-
        rbindlist(
          lapply(input$csvs$datapath, read.csv),
          use.names = TRUE,
          fill = TRUE
        )
      
      
      #Check what type of dataframe has been uploaded
      okayGo <- FALSE
      for (i in colnames(wwda2)) {
        if (grepl("CloneID", i, fixed = TRUE)) {
          okayGo <- TRUE
          values$analysisType <- "cloneTable"
          
        }
        
        if (grepl("ROI.Volume.", i, fixed = TRUE)) {
          values$analysisType <- "wingDisc"
          okayGo <- TRUE
        }
      }
      
      #Here we make sure the correct steps have been taken (i.e. right buttons pressed, right files uploaded)
      validate(
        need(
          okayGo == TRUE,
          "Please load a previously analyzed data table"
        ),
        need(
          values$randoTable == FALSE,
          "Please load a previously analyzed data table"
        )
      )
      
      wwda2 <- as.data.frame(wwda2)
      
      
      
      
      #Output data into analyzed data tab
      output$contents <-
        renderTable(
          wwda2,
          hover = TRUE,
          border = TRUE,
          spacing = "s",
          digits = 5,
          align = "l",
          width = "auto"
        )
      values$rawChoices <- colnames(wwda2)
      values$choices <- colnames(wwda2)
      values$control2 <- as.character(unique(wwda2$Condition))
      
      
      #Backup data in reactive values
      values$wwda2 <- wwda2
      
      
      #Output table
      output$contents <-
        renderTable(
          wwda2,
          hover = TRUE,
          border = TRUE,
          spacing = "s",
          digits = 5,
          align = "l",
          width = "auto"
        )
      TrueCasCounts <- 0
      TrueNonCasCounts <- 0
      genotypeNames <- c()
      
      for (i in colnames(wwda2)) {
        if (grepl("Number.of.Center.Cells", i, fixed = TRUE)) {
          TrueNonCasCounts <- 1
          genotypeNames <-
            c(genotypeNames,
              gsub("Number.of.Center.Cells.", "", i))
        }
        if (grepl("Border.Foci.Count", i, fixed = TRUE)) {
          TrueCasCounts <- 1
          genotypeNames <-
            c(genotypeNames, gsub("Border.Foci.Count.", "", i))
        }
      }
      
      genotypeNames <- unique(genotypeNames)
      
      
      
      tryCatch(
        #Restore Foci counts
        {
          #If we have the actual counts from the macro, we prefer to use those. Otherwise, we revert to the mathematical approximation
          
          if (TrueCasCounts == 1 && TrueNonCasCounts == 1) {
            for (i in genotypeNames) {
              queryString <- paste(i, "ConditionAndExperiment", sep = "|")
              genotypeVals <-
                wwda2[, grepl(queryString , names(wwda2))]
              
              
              headers <- colnames(genotypeVals)
              newHeaders <- c()
              for (z in headers) {
                query <- paste(".", i, sep = "")
                
                newHead <- gsub(query, "", z)
                
                newHeaders <- c(newHeaders, newHead)
              }
              
              colnames(genotypeVals) <- newHeaders
              
              
              
              
              borderCells <-
                aggregate(
                  genotypeVals$Number.of.Border.Cells,
                  list(genotypeVals$ConditionAndExperiment),
                  sum
                )
              
              centerCells <-
                aggregate(
                  genotypeVals$Number.of.Center.Cells,
                  list(genotypeVals$ConditionAndExperiment),
                  sum
                )
              
              borderCasCells <-
                aggregate(
                  genotypeVals$Border.Foci.Count,
                  list(genotypeVals$ConditionAndExperiment),
                  sum
                )
              
              centerCasCells <-
                aggregate(
                  genotypeVals$Center.Foci.Count,
                  list(genotypeVals$ConditionAndExperiment),
                  sum
                )
              
              
              #Format these values into a table suitable for a fisher test
              deathTable <- data.frame(
                Genotype = borderCells[, 1],
                CellsInBorder = borderCells[, 2],
                FociInBorder = borderCasCells[, 2],
                CellsInCenter = centerCells[, 2],
                FociInCenter = centerCasCells[, 2]
              )
              
              values$deathTable <- deathTable
              output$deathTable <-
                renderTable(
                  values$deathTable,
                  hover = TRUE,
                  border = TRUE,
                  spacing = "s",
                  digits = 5,
                  align = "l"
                )
            }
          }
          
          
        },
        error = function(error_message) {
          message("Unable to prepare death table")
          return(NA)
        }
      )
    },
    error = function(error_message) {
      message("Unable to re-analyze data")
      return(NA)
    })
    
  })
  
  
  ############################################################################################################################
  ##################################################### Load random data table ###################################################
  ############################################################################################################################
  analysis <- observeEvent(input$go, {
    tryCatch({
      validate(need(
        values$randoTable == TRUE,
        "Please load an unformatted, non macro table"
      ))
      
      #Load random table without any modifications
      wwda2 <- values$genAssign
      
      
      wwda2$ConditionAndExperiment <-
        paste(wwda2$Condition, wwda2$Experiment, sep = ", ")
      
      
      #Output data into analyzed data tab
      output$contents <-
        renderTable(
          wwda2,
          hover = TRUE,
          border = TRUE,
          spacing = "s",
          digits = 5,
          align = "l",
          width = "auto"
        )
      values$rawChoices <- colnames(wwda2)
      values$choices <- colnames(wwda2)
      values$control2 <- as.character(unique(wwda2$Condition))
      
      #Backup data in reactive values
      values$wwda2 <- wwda2
      
      
      
    },
    error = function(error_message) {
      message("Unable to analyze unformatted table")
      return(NA)
    })
    
  })
  
  
  ############################################################################################################################
  ##################################################### Clone Tracking Analysis ###################################################
  ############################################################################################################################
  analysis <- observeEvent(input$go, {
    tryCatch({
      #Read dataset csv files
      cta <-
        rbindlist(
          lapply(input$csvs$datapath, read.csv),
          use.names = TRUE,
          fill = TRUE
        )
      
      #Check what type of dataframe has been uploaded
      okayGo <- FALSE
      for (i in colnames(cta)) {
        if (grepl("Clone.ID", i, fixed = TRUE)) {
          okayGo <- TRUE
        }
      }
      
      values$analysisType <- "cloneTable"
      
      
      #Only allow analysis to proceed if the correct filetype has been loaded and genotype assignments have been made
      
      validate(
        need(
          cta$Clone.ID,
          "Please load a clone tracking analysis file and assign genotypes"
        ),
        need(
          okayGo == TRUE,
          "Please load a clone tracking analysis file and assign genotypes"
        ),
        need(values$loaded == 1, "Load genotype assignments first!"),
        need(
          values$randoTable == FALSE,
          "Please load a previously analyzed data table"
        )
      )
      
      
      #Read genotype assignment csv file
      trad  <- values$genAssign
      
      #This step is for backwards compatibility with old input files. We update some outdated column names to reflect their new names
      names(cta) <-
        gsub(x = names(cta),
             pattern = "Pouch",
             replacement = "ROI")
      names(cta) <-
        gsub(x = names(cta),
             pattern = "Caspase",
             replacement = "Foci")
      
      #Merge the CSV file input with the genotype assignments
      colnames(trad) <- c("Image.Name", "Experiment", "Condition")
      ccta <- merge(x = cta, y = trad)
      
      
      #Make a unique identifier for a given clone at a given z level
      ccta2 <-
        cbind(
          CloneZName = paste(
            ccta$Image.Name ,
            ccta$Experiment,
            ccta$Clone.ID ,
            ccta$Z.level,
            ccta$TimePoint,
            sep = ", "
          ) ,
          ccta
        )
      
      #Determine the height of the clone based on the minimum and maximum z-level values
      tryCatch(
        # This is a try/catch function. I use this a lot to account for how many possible data-tables this app can see
        {
          cctz <-
            cbind(CloneZID = paste(ccta$Image.Name , ccta$Experiment, ccta$Clone.ID) ,
                  ccta)
          MAX <- aggregate(cctz$Z.level, list(cctz$CloneZID), max)
          MIN <- aggregate(cctz$Z.level, list(cctz$CloneZID), min)
          Diff <- MAX[, 2] - MIN[, 2]
          Diff <- Diff + 1
        },
        # ... but if an error occurs, tell me what happened:
        error = function(error_message) {
          message("old macro")
          return(NA)
        }
      )
      
      
      #Condense the data down, extracting unique gene ID's and summing areas. THis collates all the unique z-level information down to individual clone information
      
      ccta2sum <- data.frame(
        Condition =  as.character(tapply(
          as.character(ccta2$Condition) ,
          ccta2$CloneZName ,
          unique
        )),
        CloneZName = as.character(tapply(
          as.character(ccta2$CloneZName) ,
          ccta2$CloneZName ,
          unique
        )),
        Experiment =  as.character(tapply(
          as.character(ccta2$Experiment) ,
          ccta2$CloneZName ,
          unique
        )),
        Genotype = as.character(tapply(
          as.character(ccta2$Clone.Genotype) ,
          ccta2$CloneZName ,
          unique
        )),
        Clone.ID = as.character(tapply(
          as.character(ccta2$Clone.ID) , ccta2$CloneZName , unique
        )),
        Image.Name = as.character(tapply(
          as.character(ccta2$Image.Name) ,
          ccta2$CloneZName ,
          unique
        )),
        Z.level = as.character(tapply(
          as.character(ccta2$Z.level) , ccta2$CloneZName , unique
        )),
        Clone.Area = tapply(ccta2$Clone.Area , ccta2$CloneZName , sum),
        Border.Area = tapply(ccta2$Border.Area , ccta2$CloneZName , sum),
        Center.Area = tapply(ccta2$Center.Area , ccta2$CloneZName , sum)
        
      )
      
      
      
      
      ccta3 <- ccta2sum
      
      tryCatch({
        ccta3 <-
          cbind(ccta3,
                Time.Point = tapply(ccta2$TimePoint, ccta2$CloneZName, unique))
        
      },
      # ... but if an error occurs, tell me what happened:
      error = function(error_message) {
        message("no timepoint")
        return(NA)
      })
      
      #Add in the Foci data, if applicable
      tryCatch({
        if ("Foci.Coverage" %in% colnames(ccta2))
        {
          ccta3 <-
            cbind(ccta3,
                  Foci.Coverage = tapply(ccta2$Foci.Coverage , ccta2$CloneZName , sum))
          ccta3 <-
            cbind(ccta3,
                  Foci.Border = tapply(ccta2$Foci.Border , ccta2$CloneZName , sum))
          ccta3 <-
            cbind(ccta3,
                  Foci.Center = tapply(ccta2$Foci.Center , ccta2$CloneZName , sum))
        }
        
      },
      # ... but if an error occurs, tell me what happened:
      error = function(error_message) {
        message("no Foci")
        return(NA)
      })
      
      
      
      #Get the overall height of the disc, if applicable
      tryCatch(
        # This is what I want to do...
        {
          ccta3 <-
            cbind(ccta3,
                  Disc.Height = tapply(ccta2$ROIHeight , ccta2$CloneZName , unique))
        },
        # ... but if an error occurs, tell me what happened:
        error = function(error_message) {
          message("old macro 1")
          return(NA)
        }
      )
      ccta3 <-
        cbind(
          ccta3,
          CloneName = paste(
            ccta2sum$Image.Name ,
            ccta2sum$Clone.ID,
            ccta2sum$Time.Point,
            sep = ", "
          )
        )
      
      
      
      #Sum the integrated density values (fluorescence intensities non normalized to area)
      tryCatch({
        if ("Clone.Fluorescence" %in% colnames(ccta2))
        {
          #sum integrated fluorescence density
          ccta3 <-
            cbind(
              ccta3,
              Clone.Fluorescence = tapply(ccta2$Clone.Fluorescence , ccta2$CloneZName , sum)
            )
          ccta3 <-
            cbind(
              ccta3,
              Border.Fluorescence = tapply(ccta2$Border.Fluorescence , ccta2$CloneZName , sum)
            )
          ccta3 <-
            cbind(
              ccta3,
              Center.Fluorescence = tapply(ccta2$Center.Fluorescence , ccta2$CloneZName , sum)
            )
        }
      },
      # ... but if an error occurs, tell me what happened:
      error = function(error_message) {
        message("No fluorescence")
        return(NA)
      })
      
      
      #This is the skeleton of our final output table. We will add other values, such as they are applicable based on the analysis the user has run
      ccta3sum <- data.frame(
        Condition =  tapply(as.character(ccta3$Condition) , ccta3$CloneName , unique),
        CloneName = tapply(as.character(ccta3$CloneName) , ccta3$CloneName , unique),
        Experiment =  tapply(
          as.character(ccta3$Experiment) ,
          ccta3$CloneName ,
          unique
        ),
        
        CloneID = tapply(as.character(ccta3$Clone.ID) , ccta3$CloneName , unique),
        Genotype = tapply(as.character(ccta3$Genotype) , ccta3$CloneName , unique),
        Clone.Volume = tapply(ccta3$Clone.Area , ccta3$CloneName , sum),
        Clone.Border.Volume. = tapply(ccta3$Border.Area , ccta3$CloneName , sum),
        Clone.Center.Volume = tapply(ccta3$Center.Area , ccta3$CloneName , sum)
        
        
      )
      
      
      ccta3sumZ2 <- ccta3sum
      tryCatch(
        # accounts for a change in format between older macro and newer macro
        {
          ccta3sumZ2 <-
            cbind(
              ccta3sum,
              ROI.Volume = tapply(ccta3$ROI.Area, ccta3$CloneName, unique) * tapply(ccta3$Disc.Height, ccta3$CloneName, unique)
            )
        },
        # ... but if an error occurs, tell me what happened:
        error = function(error_message) {
          message("old macro 2")
          return(NA)
        }
      )
      
      
      tryCatch(
        # accounts for a change in format between older macro and newer macro
        {
          ccta3sumZ2 <-
            cbind(ccta3sum,
                  Time.Point = tapply(
                    as.character(ccta3$Time.Point) ,
                    ccta3$CloneName ,
                    unique
                  ))
        },
        # ... but if an error occurs, tell me what happened:
        error = function(error_message) {
          message("no timepoint")
          return(NA)
        }
      )
      tryCatch(
        #Add Foci data if applicable
        {
          if ("Foci.Coverage" %in% colnames(ccta3))
          {
            ccta3sumZ2 <-
              cbind(ccta3sumZ2,
                    Foci.Volume = tapply(ccta3$Foci.Coverage , ccta3$CloneName , sum))
            ccta3sumZ2 <-
              cbind(
                ccta3sumZ2,
                Foci.in.Border.Volume = tapply(ccta3$Foci.Border , ccta3$CloneName , sum)
              )
            ccta3sumZ2 <-
              cbind(
                ccta3sumZ2,
                Foci.in.Center.Volume = tapply(ccta3$Foci.Center , ccta3$CloneName , sum)
              )
            ccta3sumZ2 <-
              cbind(
                ccta3sumZ2,
                Percent.Foci.Coverage.of.Clone = tapply(ccta3$Foci.Coverage , ccta3$CloneName , sum) *
                  100 / tapply(ccta3$Clone.Area , ccta3$CloneName , sum)
              )
            ccta3sumZ2 <-
              cbind(
                ccta3sumZ2,
                Percent.Foci.Coverage.of.Border = tapply(ccta3$Foci.Border , ccta3$CloneName , sum) *
                  100 / tapply(ccta3$Border.Area , ccta3$CloneName , sum)
              )
            ccta3sumZ2 <-
              cbind(
                ccta3sumZ2,
                Percent.Foci.Coverage.of.Center = tapply(ccta3$Foci.Center , ccta3$CloneName , sum) *
                  100 / tapply(ccta3$Center.Area , ccta3$CloneName , sum)
              )
            # ccta3sumZ2 <- cbind(ccta3sumZ2, Competitive.index = c( tapply(ccta3$Foci.Center , ccta3$CloneName , sum) / tapply(ccta3$Center.Area , ccta3$CloneName , sum) )
            #                     /
            #                         c( tapply(ccta3$Foci.Border , ccta3$CloneName , sum) / tapply(ccta3$Border.Area , ccta3$CloneName , sum) ))
            # ccta3sumZ2 <- cbind(ccta3sumZ2, log2.Competitive.index = log(ccta3sumZ2$Competitive.index, 2))
          }
        },
        # ... but if an error occurs, tell me what happened:
        error = function(error_message) {
          message("no Foci")
          return(NA)
        }
      )
      
      
      
      
      tryCatch(
        #Add fluorescence data if applicable
        {
          if ("Clone.Fluorescence.IntDen" %in% colnames(ccta3))
          {
            ccta3sumZ2 <-
              cbind(
                ccta3sumZ2,
                Mean.Fluorescence.of.Clone = tapply(
                  ccta3$Clone.Fluorescence.IntDen ,
                  ccta3$CloneName ,
                  sum
                ) / tapply(ccta3$Clone.Area , ccta3$CloneName , sum)
              )
            ccta3sumZ2 <-
              cbind(
                ccta3sumZ2,
                Mean.Fluorescence.of.Border = tapply(
                  ccta3$Border.Fluorescence.IntDen ,
                  ccta3$CloneName ,
                  sum
                ) / tapply(ccta3$Border.Area , ccta3$CloneName , sum)
              )
            ccta3sumZ2 <-
              cbind(
                ccta3sumZ2,
                Mean.Fluorescence.of.Center = tapply(
                  ccta3$Center.Fluorescence.IntDen ,
                  ccta3$CloneName ,
                  sum
                ) / tapply(ccta3$Center.Area , ccta3$CloneName , sum)
              )
            
          }
          
        },
        # ... but if an error occurs, tell me what happened:
        error = function(error_message) {
          message("no fluorescence")
          return(NA)
        }
      )
      
      tryCatch(
        # This is what I want to do...
        {
          if ("Clone.Volume" %in% colnames(ccta3sumZ2))
          {
            clonepercent = ccta3sumZ2$Clone.Volume * 100 / ccta3sumZ2$ROI.Volume
            ccta3sumZ2 <-
              cbind(ccta3sumZ2, Number.of.Z.Slices = Diff)
            ccta3sumZ2 <-
              cbind(ccta3sumZ2, Clone.Volume.Normalized.to.ROI = clonepercent)
          }
        },
        # ... but if an error occurs, tell me what happened:
        error = function(error_message) {
          message("old output")
          return(NA)
        }
      )
      
      
      
      # # # # # Remove na values # # # # # #
      data_subset <- ccta3sumZ2[, c("CloneName")]
      ccta3sumZ2 <- ccta3sumZ2[complete.cases(data_subset),]
      
      #Rename table and add ConditionAndExperiment unique identifier
      wwda2 <- ccta3sumZ2
      wwda2$ConditionAndExperiment <-
        paste(wwda2$Condition, wwda2$Experiment, wwda2$Genotype, sep = ", ")
      
      wwda2 <- wwda2[order(wwda2$ConditionAndExperiment),]
      
      #Output data into analyzed data tab
      output$contents <-
        renderTable(
          wwda2,
          hover = TRUE,
          border = TRUE,
          spacing = "s",
          digits = 5,
          align = "l",
          width = "auto"
        )
      values$rawChoices <- colnames(wwda2)
      wwda2sub <-
        subset(
          wwda2,
          select = -c(
            CloneID,
            Condition,
            CloneName,
            Experiment,
            ConditionAndExperiment
          )
        )
      values$choices <- colnames(wwda2sub)
      
      
      #Backup data in reactive values
      values$wwda2 <- wwda2
      
      
      
      
      
    },
    # ... but if an error occurs, tell me what happened:
    error = function(error_message) {
      message("Unable to analyze clone tracking data")
      return(NA)
    })
    
    
  })
  
  
  ############################################################################################################################
  ##################################################### Wing Disc Analysis ###################################################
  ############################################################################################################################
  
  analysis <- observeEvent(input$go, {
    #This trycatch loop encloses the entire wing disc analysis function
    tryCatch({
      #Read genotype assignment csv file to the variable 'trad'
      trad  <- values$genAssign
      
      #Read dataset csv files to the variable 'wda'
      wda <- read.csv(input$csvs$datapath[1], check.names = FALSE)
      count = 2
      while (count <= length(input$csvs$datapath)) {
        toBind <- read.csv(input$csvs$datapath[count], check.names = FALSE)
        wda <- rbind(wda, toBind)
        count = count + 1
      }
      
      
      #This is for backwards compatibility, updating out-modded names to current ones
      #Remove from the production version
      
      names(wda) <-
        gsub(x = names(wda),
             pattern = "Border Caspase",
             replacement = "Border Foci Area")
      names(wda) <-
        gsub(x = names(wda),
             pattern = "Center Caspase",
             replacement = "Center Foci Area")
      names(wda) <-
        gsub(x = names(wda),
             pattern = "Clone Caspase",
             replacement = "Clone Foci Area")
      names(wda) <-
        gsub(x = names(wda),
             pattern = "Total Caspase Cells",
             replacement = "Total number of Foci")
      names(wda) <-
        gsub(x = names(wda),
             pattern = "Pouch",
             replacement = "ROI")
      names(wda) <-
        gsub(x = names(wda),
             pattern = "Caspase",
             replacement = "Foci")
      
      
      #Check what type of dataframe has been uploaded
      okayGo <- FALSE
      for (i in colnames(wda)) {
        if (grepl("ROI Area<", i, fixed = TRUE)) {
          okayGo <- TRUE
        }
      }
      
      
      #Here we make sure the correct steps have been taken (i.e. right buttons pressed, right files uploaded)
      validate(
        need(
          wda$File,
          "Please load a whole disc analysis file and assign genotypes"
        ),
        need(
          okayGo == TRUE,
          "Please load a whole disc analysis file and assign genotypes"
        ),
        need(values$loaded == 1, "Load genotype assignments first!"),
        need(
          values$randoTable == FALSE,
          "Please load a previously analyzed data table"
        )
      )
      
      values$analysisType <- "wingDisc"
      
      
      
      
      colnames(trad) <-
        c("File", "Image Name", "Experiment", "Condition")
      #We tack the assignments onto the input data, creating a dataframe 'wwda2' with our raw data and assignments
      wwdaFull <-
        merge(x = wda,
              y = trad,
              by = c("File", "Image Name"))
      
      
      if (isFALSE("TimePoint" %in% names(wwdaFull))) {
        wwdaFull$"Image Name" <-
          paste(wwdaFull$"File", wwdaFull$"Image Name", sep = "")
      } else {
        if (isFALSE(input$sepTimePoints == "Time-lapse analysis"))
          wwdaFull$"Image Name" <-
            paste(wwdaFull$"Image Name", wwdaFull$"TimePoint", sep = ", timepoint:")
      }
      
      print("Tables Merged")
      
      values$vchoices2 <- unique(wwdaFull$"Image Name")
      
      tester <- "TimePoint" %in% names(wwdaFull)
      tester2 <- input$sepTimePoints == "Time-lapse analysis"
      tester3 <- input$sepTimePoints == "Z-axis analysis"
      
      if (isTRUE(tester2)) {
        if (isTRUE(tester)) {
          wwdaFull$"Image Name" <-
            paste(wwdaFull$"Image Name", wwdaFull$TimePoint, sep = ".tp.")
          values$switchO <- TRUE
          
        }
      } else if (isTRUE(tester3)) {
        wwdaFull$"Image Name" <-
          paste(wwdaFull$"Image Name", wwdaFull$"Z level", sep = ".tp.")
        values$switchO <- TRUE
        
      } else {
        values$switchO <- FALSE
      }
      
      
      
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
      
      while (breakLoop == FALSE) {
        for (i in headers) {
          breakLoop <- TRUE
          breaker <- FALSE
          queryString <-
            paste(baseString, toString(count), sep = ":")
          count <- count + 1
          
          isGenotypeNum <- grepl(queryString, i, fixed = TRUE)
          
          for (z in isGenotypeNum) {
            if (z == TRUE) {
              breaker = TRUE
            }
          }
          if (breaker == TRUE) {
            breakLoop <- FALSE
          }
        }
      }
    
      count <- count - 2
      print("Genotypes determined")
      
      #Get only the columns that are not genotype specific
      staticVals <-
        wwdaFull[,!grepl("Genotype" , names(wwdaFull))]
      
      
      ROI.Area <- wwdaFull[, grepl("ROI.Area", names(wwdaFull))]
      
      
      
      #Loop through, get all the columns corresponding to one genotype, and store them in an array
      chal <- 1
      while (chal <= count) {
        print("Genotypes loop initiated")
        queryString1 <- paste(baseString, toString(chal), sep = "_")
        queryString2 <- paste(baseString, toString(chal), sep = ":")
        queryString <- paste(queryString1, queryString2, sep = "|")
        
        genotypeVals <-
          wwdaFull[, grepl(queryString , names(wwdaFull))]
        genotypeVals <- cbind(staticVals, genotypeVals)
        
        headers <- colnames(genotypeVals)
        
        headers <-
          gsub(queryString1,
               "",
               headers,
               ignore.case = FALSE,
               fixed = TRUE)
        headers <- gsub("<[^>]+>", "", headers)
        headers <- gsub(" ", ".", headers)
        headers <- gsub("#", "X.", headers)
        
        
        colnames(genotypeVals) <- headers
        tester <- "ROI.Area" %in% names(genotypeVals)
        
        if (tester == FALSE) {
          genotypeVals$ROI.Area <- ROI.Area
          
        }
        
        wwda <- genotypeVals
        
        
        #Here we make our dataframe 'wwda2,' which has all of our data from all our z-levels collated per wing disc
        
        print ('Creating collated data table')
        wwda2 <- data.frame(
          File = tapply(as.character(wwda$File) , wwda$Image.Name , unique),
          Image.Name = tapply(
            as.character(wwda$Image.Name) ,
            wwda$Image.Name ,
            unique
          ),
          Condition =  tapply(
            as.character(wwda$Condition) ,
            wwda$Image.Name ,
            unique
          ),
          Experiment = tapply(
            as.character(wwda$Experiment) ,
            wwda$Image.Name ,
            unique
          ),
          
          
          
          
          Clone.Volume = tapply(wwda$Clone.Area , wwda$Image.Name , sum),
          Clone.Center.Volume = tapply(wwda$Center.Area , wwda$Image.Name , sum),
          Clone.Border.Volume = tapply(wwda$Border.Area , wwda$Image.Name , sum),
          ROI.Volume = tapply(wwda$ROI.Area , wwda$Image.Name , sum),
          Not.Clone.Volume = tapply(wwda$ROI.Area , wwda$Image.Name, sum) - tapply(wwda$Clone.Area , wwda$Image.Name, sum),
          
          
          
          Starting.Z.Plane = tapply(wwda$Z.level, list(wwda$Image.Name), min),
          Number.of.Z.Slices = tapply(wwda$Z.level, list(wwda$Image.Name), max) - tapply(wwda$Z.level, list(wwda$Image.Name), min) + 1,
          Percent.Clone.Coverage.of.ROI = tapply(wwda$Clone.Area , wwda$Image.Name , sum) *
            100 / tapply(wwda$ROI.Area , wwda$Image.Name , sum),
          Border.Coverage.of.Clones = tapply(wwda$Border.Area , wwda$Image.Name , sum) *
            100 / tapply(wwda$Clone.Area , wwda$Image.Name , sum)
          
          
        )
        
        print ("Table Initiated")
        
        if (isTRUE(chal == 1)) {
          if (isTRUE(tester2)) {
            if (isTRUE(tester)) {
              wwda2 <-
                cbind(wwda2,
                      TimePoint = tapply(wwda$TimePoint, wwda$Image.Name, unique))
              
            }
          }
          if (isTRUE(tester3)) {
            wwda2 <-
              cbind(wwda2,
                    z.level = tapply(wwdaFull$"Z level", wwda$Image.Name, unique))
          }
        }
        
        
        
        
        
        #Here we add the individual cell counts information to the table, if applicable
        
        if ("Cell.Count.in.Center" %in% colnames(wwda))
        {
          wwda2 <-
            cbind(
              wwda2,
              Number.of.Center.Cells = tapply(wwda$Cell.Count.in.Center , wwda$Image.Name , unique)
            )
          wwda2 <-
            cbind(
              wwda2,
              Number.of.Border.Cells = tapply(wwda$Cell.Count.in.Border , wwda$Image.Name , unique)
            )
          wwda2 <-
            cbind(
              wwda2,
              Total.Number.of.Cells = tapply(wwda$Cell.Count.in.Border , wwda$Image.Name , unique) +
                tapply(wwda$Cell.Count.in.Center , wwda$Image.Name , unique)
            )
          
          wwda2 <-
            cbind(
              wwda2,
              Density.of.Center.Cells = tapply(wwda$Cell.Count.in.Center , wwda$Image.Name , unique) / tapply(wwda$Center.Area , wwda$Image.Name , sum)
            )
          wwda2 <-
            cbind(
              wwda2,
              Density.of.Border.Cells = tapply(wwda$Cell.Count.in.Border , wwda$Image.Name , unique) / tapply(wwda$Border.Area , wwda$Image.Name , sum)
            )
          wwda2 <-
            cbind(wwda2, Density.of.Cells.in.Clones = ((
              tapply(wwda$Cell.Count.in.Border , wwda$Image.Name , unique) + tapply(wwda$Cell.Count.in.Center , wwda$Image.Name , unique)
            ) / tapply(wwda$Clone.Area , wwda$Image.Name , sum)
            ))
          #wwda2 <- cbind(wwda2, Average.Cell.Volume = tapply(wwda$Clone.Area , wwda$Image.Name , sum) / (tapply(wwda$Cell.Count.in.Border , wwda$Image.Name , unique)+tapply(wwda$Cell.Count.in.Center , wwda$Image.Name , unique)))
          #wwda2 <- cbind(wwda2, Average.Cell.Area.Per.Z.Plane = (tapply(wwda$Clone.Area , wwda$Image.Name , sum) / (tapply(wwda$Cell.Count.in.Border , wwda$Image.Name , unique)+tapply(wwda$Cell.Count.in.Center , wwda$Image.Name , unique)) / (tapply(wwda$Z.level, list(wwda$Image.Name), max) - tapply(wwda$Z.level, list(wwda$Image.Name), min))))
          
        }
        print("Cell count data processed")
        
        
        
        
        
        # Add number of clones information, if available (only generated if individual clone analysis was perforemd)
        if ("Total.Number.of.Clones" %in% colnames(wwda))
        {
          if (isFALSE(all(wwda$Total.Number.of.Clones, na.rm = TRUE) == 0)) {
            wwda2 <-
              cbind(
                wwda2,
                Number.of.Clones = tapply(
                  wwda$Total.Number.of.Clones ,
                  wwda$Image.Name ,
                  unique
                )
              )
          }
        }
        
        
        
        # This variable determines if there were DCP1 counts or not
        TrueCasCounts <- 0
        #This variable tracks whether or not we were able to do individual cell counts or not
        TrueNonCasCounts <- 0
        # #Add the Foci information to the table, if applicable
        
        
        #Adds foci area data, if available (only available if user specified foci analysis in the macro)
        if ("Foci.Area" %in% colnames(wwda))
        {
          wwda2 <-
            cbind(wwda2,
                  Total.Foci.Volume = tapply(wwda$Foci.Area , wwda$Image.Name , sum))
          wwda2 <-
            cbind(wwda2,
                  Clone.Foci.Volume = tapply(wwda$Clone.Foci.Area , wwda$Image.Name , sum))
          wwda2 <-
            cbind(wwda2,
                  Center.Foci.Volume = tapply(wwda$Center.Foci.Area , wwda$Image.Name , sum))
          wwda2 <-
            cbind(wwda2,
                  Border.Foci.Volume = tapply(wwda$Border.Foci.Area, wwda$Image.Name , sum))
          wwda2 <-
            cbind(
              wwda2,
              Not.Clones.Foci.Volume = tapply(wwda$Not.Clones.Foci.Area, wwda$Image.Name , sum)
            )
          
          wwda2 <-
            cbind(
              wwda2,
              Percent.Foci.Coverage.of.ROI = tapply(wwda$Foci.Area , wwda$Image.Name , sum) *
                100 / tapply(wwda$ROI.Area , wwda$Image.Name , sum)
            )
          wwda2 <-
            cbind(
              wwda2,
              Percent.Foci.Coverage.of.Clones = tapply(wwda$Clone.Foci.Area , wwda$Image.Name , sum) *
                100 / tapply(wwda$Clone.Area , wwda$Image.Name , sum)
            )
          wwda2 <-
            cbind(
              wwda2,
              Percent.Foci.Coverage.of.Border = tapply(wwda$Border.Foci.Area, wwda$Image.Name , sum) *
                100 / tapply(wwda$Border.Area , wwda$Image.Name , sum)
            )
          wwda2 <-
            cbind(
              wwda2,
              Percent.Foci.Coverage.of.Center = tapply(wwda$Center.Foci.Area, wwda$Image.Name , sum) *
                100 / tapply(wwda$Center.Area , wwda$Image.Name , sum)
            )
          wwda2 <-
            cbind(
              wwda2,
              Percent.Foci.Coverage.of.Not.Clones = tapply(wwda$Not.Clones.Foci.Area, wwda$Image.Name , sum) *
                100 / tapply(wwda$Not.Clones.Area , wwda$Image.Name , sum)
            )

        }
        
        print ("Foci area processed")
        
        
        #Add individual foci count data, if available
        if ("Total.number.of.Foci" %in% colnames(wwda))
        {
          wwda2 <-
            cbind(
              wwda2,
              Total.Foci.Count = tapply(wwda$Total.number.of.Foci , wwda$Image.Name , unique)
            )
          wwda2 <-
            cbind(
              wwda2,
              Center.Foci.Count = tapply(wwda$Foci.Count.in.Center , wwda$Image.Name , unique)
            )
          wwda2 <-
            cbind(
              wwda2,
              Border.Foci.Count = tapply(wwda$Foci.Count.in.Border , wwda$Image.Name , unique)
            )
          wwda2 <-
            cbind(
              wwda2,
              Clone.Foci.Count = tapply(wwda$Foci.Count.in.Border , wwda$Image.Name , unique) + tapply(wwda$Foci.Count.in.Center , wwda$Image.Name , unique)
            )
          wwda2 <-
            cbind(
              wwda2,
              Non.Clones.Foci.Count = tapply(wwda$Total.number.of.Foci , wwda$Image.Name , unique) - tapply(wwda$Foci.Count.in.Border , wwda$Image.Name , unique) - tapply(wwda$Foci.Count.in.Center , wwda$Image.Name , unique)
            )
          
          wwda2 <-
            cbind(
              wwda2,
              Foci.Density.in.Center = tapply(wwda$Foci.Count.in.Center , wwda$Image.Name , unique) / tapply(wwda$Center.Area , wwda$Image.Name , sum)
            )
          wwda2 <-
            cbind(
              wwda2,
              Foci.Density.in.Border = tapply(wwda$Foci.Count.in.Border , wwda$Image.Name , unique) / tapply(wwda$Border.Area , wwda$Image.Name , sum)
            )
          wwda2$Foci.Density.in.Clones <-
            wwda2$Clone.Foci.Count / wwda2$Clone.Volume
          TrueCasCounts <- 1
          
          
          #Add individual cell count data, if available - this is where we bin cell count and foci count data together, so it's only available if the user specified individual foci and individual cell count analyses
          if ("Total.Number.of.Cells" %in% colnames(wwda2))
          {
            wwda2$Number.of.Clone.Cells.And.Foci <-
              wwda2$Total.Number.of.Cells + wwda2$Clone.Foci.Count
            wwda2$Number.of.Center.Cells.And.Foci <-
              wwda2$Number.of.Center.Cells + wwda2$Center.Foci.Count
            wwda2$Number.of.Border.Cells.And.Foci <-
              wwda2$Number.of.Border.Cells + wwda2$Border.Foci.Count
          }
          
        }
        
        #Add individual cell count data, if available
        if ("Cell.Count.in.Border" %in% colnames(wwda))
        {
          wwda2 <-
            cbind(
              wwda2,
              Percent.Foci.Positive.Cells.in.Clones = wwda2$Clone.Foci.Count * 100 / wwda2$Number.of.Clone.Cells.And.Foci
            )
          wwda2 <-
            cbind(
              wwda2,
              Percent.Foci.Positive.Cells.in.Border = wwda2$Border.Foci.Count * 100 / wwda2$Number.of.Border.Cells.And.Foci
            )
          wwda2 <-
            cbind(
              wwda2,
              Percent.Foci.Positive.Cells.in.Center = wwda2$Center.Foci.Count * 100 / wwda2$Number.of.Center.Cells.And.Foci
            )
          
          TrueNonCasCounts <- 1
        }
        
        print("Foci data processed")
        
        #Add fluorescence intensity data, if available
        if ("Clone.Fluorescence.IntDen" %in% colnames(wwda))
        {
          if (max(wwda[, c(
            "Clone.Fluorescence.IntDen",
            "Center.Fluorescence.IntDen",
            "Border.Fluorescence.IntDen",
            "Not.Clone.Fluorescence.IntDen"
          )], na.rm = TRUE) > 1) {
            wwda2 <-
              cbind(
                wwda2,
                Clone.Fluorescence = tapply(
                  wwda$Clone.Fluorescence.IntDen ,
                  wwda$Image.Name ,
                  sum
                ) / tapply(wwda$Clone.Area , wwda$Image.Name , sum)
              )
            wwda2 <-
              cbind(
                wwda2,
                Center.Fluorescence = tapply(
                  wwda$Center.Fluorescence.IntDen ,
                  wwda$Image.Name ,
                  sum
                ) / tapply(wwda$Center.Area , wwda$Image.Name , sum)
              )
            wwda2 <-
              cbind(
                wwda2,
                Border.Fluorescence = tapply(
                  wwda$Border.Fluorescence.IntDen,
                  wwda$Image.Name ,
                  sum
                ) / tapply(wwda$Border.Area , wwda$Image.Name , sum)
              )
            wwda2 <-
              cbind(
                wwda2,
                Not.Clone.Fluorescence = tapply(
                  wwda$Not.Clone.Fluorescence.IntDen ,
                  wwda$Image.Name ,
                  sum
                ) / tapply(wwda$Not.Clones.Area, wwda$Image.Name , sum)
              )
          }

        }
        
        
        
        print("Fluorescence data processed")
        
        
        #Add speckles data, if available
        if ("X.Speckles.in.clones" %in% colnames(wwda))
        {
          wwda2 <-
            cbind(
              wwda2,
              Density.of.Speckles.in.Clones = tapply(wwda$X.Speckles.in.clones , wwda$Image.Name , sum) / tapply(wwda$Clone.Area , wwda$Image.Name , sum)
            )
          wwda2 <-
            cbind(
              wwda2,
              Density.of.Speckles.in.Border = tapply(wwda$X.Speckles.in.border , wwda$Image.Name , sum) / tapply(wwda$Border.Area , wwda$Image.Name , sum)
            )
          wwda2 <-
            cbind(
              wwda2,
              Density.of.Speckles.in.Center = tapply(wwda$X.Speckles.in.Center , wwda$Image.Name , sum) / tapply(wwda$Center.Area , wwda$Image.Name , sum)
            )
          wwda2 <-
            cbind(
              wwda2,
              Density.of.Speckles.in.Not.Clones = tapply(
                wwda$X.Speckles.in.Not.Clones ,
                wwda$Image.Name ,
                sum
              ) / tapply(wwda$Not.Clones.Area, wwda$Image.Name , sum)
            )
          wwda2 <-
            cbind(
              wwda2,
              Speckle.Density.Clones.vs.Not.Clones = wwda2$Density.of.Speckles.in.Clones / wwda2$Density.of.Speckles.in.Not.Clones
            )
          wwda2 <-
            cbind(
              wwda2,
              Speckle.Density.Border.vs.Center = wwda2$Density.of.Speckles.in.Border / wwda2$Density.of.Speckles.in.Center
            )
          
          
          if ("Speckle.Area.in.Clones.IntDen" %in% colnames(wwda))
          {
            wwda2 <-
              cbind(
                wwda2,
                Percent.Speckle.Coverage.of.Clones = tapply(
                  wwda$Speckle.Area.in.Clones.IntDen ,
                  wwda$Image.Name ,
                  sum
                ) * 100 / tapply(wwda$Clone.Area , wwda$Image.Name , sum)
              )
            wwda2 <-
              cbind(
                wwda2,
                Percent.Speckle.Coverage.of.Border = tapply(
                  wwda$Speckle.Area.in.Border.IntDen ,
                  wwda$Image.Name ,
                  sum
                ) * 100 / tapply(wwda$Border.Area , wwda$Image.Name , sum)
              )
            wwda2 <-
              cbind(
                wwda2,
                Percent.Speckle.Coverage.of.Center = tapply(
                  wwda$Speckle.Area.in.Center.IntDen ,
                  wwda$Image.Name ,
                  sum
                ) * 100 / tapply(wwda$Center.Area , wwda$Image.Name , sum)
              )
            wwda2 <-
              cbind(
                wwda2,
                Percent.Speckle.Coverage.of.Not.Clones = tapply(
                  wwda$Speckle.Area.in.Not.Clones.IntDen ,
                  wwda$Image.Name ,
                  sum
                ) * 100 / tapply(wwda$Not.Clones.Area , wwda$Image.Name , sum)
              )
            
            wwda2 <-
              cbind(
                wwda2,
                Mean.Speckle.Fluorescence.Clones = tapply(
                  wwda$Speckle.Fluorescence.in.Clones.IntDen ,
                  wwda$Image.Name ,
                  sum
                ) / tapply(wwda$Clone.Area , wwda$Image.Name , sum)
              )
            wwda2 <-
              cbind(
                wwda2,
                Mean.Speckle.Fluorescence.Border = tapply(
                  wwda$Speckle.Fluorescence.in.Border.IntDen ,
                  wwda$Image.Name ,
                  sum
                ) / tapply(wwda$Border.Area , wwda$Image.Name , sum)
              )
            wwda2 <-
              cbind(
                wwda2,
                Mean.Speckle.Fluorescence.Center = tapply(
                  wwda$Speckle.Fluorescence.in.Center.IntDen ,
                  wwda$Image.Name ,
                  sum
                ) / tapply(wwda$Center.Area , wwda$Image.Name , sum)
              )
            wwda2 <-
              cbind(
                wwda2,
                Mean.Speckle.Fluorescence.Not.Clones = tapply(
                  wwda$Speckle.Fluorescence.in.Not.Clones.IntDen ,
                  wwda$Image.Name ,
                  sum
                ) / tapply(wwda$Not.Clones.Area , wwda$Image.Name , sum)
              )
            
            wwda2 <-
              cbind(
                wwda2,
                Average.Speckle.Size.Clones = tapply(
                  wwda$Speckle.Area.in.Clones.IntDen ,
                  wwda$Image.Name ,
                  sum
                ) / tapply(wwda$X.Speckles.in.clones , wwda$Image.Name , sum)
              )
            wwda2 <-
              cbind(
                wwda2,
                Average.Speckle.Size.Border = tapply(
                  wwda$Speckle.Area.in.Border.IntDen ,
                  wwda$Image.Name ,
                  sum
                ) / tapply(wwda$X.Speckles.in.border , wwda$Image.Name , sum)
              )
            wwda2 <-
              cbind(
                wwda2,
                Average.Speckle.Size.Center = tapply(
                  wwda$Speckle.Area.in.Center.IntDen ,
                  wwda$Image.Name ,
                  sum
                ) / tapply(wwda$X.Speckles.in.Center , wwda$Image.Name , sum)
              )
            wwda2 <-
              cbind(
                wwda2,
                Average.Speckle.Size.Not.Clones = tapply(
                  wwda$Speckle.Area.in.Not.Clones.IntDen ,
                  wwda$Image.Name ,
                  sum
                ) / tapply(
                  wwda$X.Speckles.in.Not.Clones ,
                  wwda$Image.Name ,
                  sum
                )
              )
          }
        }
        
        
        print("Speckles data processed")
        
        
        #Get rid of empty tables
        data_subset <- wwda2[, c("File")]
        wwda2 <- wwda2[complete.cases(data_subset),]
        
        #Store the processed data in reactive values
        wwda2$ConditionAndExperiment <-
          paste(wwda2$Condition, wwda2$Experiment, sep = ", ")
        wwda2 <- wwda2[order(wwda2$ConditionAndExperiment),]
        
        
        
        #Step for fisher test of competitive index - here we prepare the contingency table

        if ("Number.of.Border.Cells" %in% colnames(wwda2))
        {
          borderCells <-
            aggregate(wwda2$Number.of.Border.Cells,
                      list(wwda2$ConditionAndExperiment),
                      sum)
          centerCells <-
            aggregate(wwda2$Number.of.Center.Cells,
                      list(wwda2$ConditionAndExperiment),
                      sum)
          borderCasCells <-
            aggregate(wwda2$Border.Foci.Count,
                      list(wwda2$ConditionAndExperiment),
                      sum)
          centerCasCells <-
            aggregate(wwda2$Center.Foci.Count,
                      list(wwda2$ConditionAndExperiment),
                      sum)
          
          
          #Format these values into a table suitable for a fisher test
          deathTable <- data.frame(
            Genotype = borderCells[, 1],
            CellsInBorder = borderCells[, 2],
            FociInBorder = borderCasCells[, 2],
            CellsInCenter = centerCells[, 2],
            FociInCenter = centerCasCells[, 2]
          )
          
          values$deathTable <- deathTable
          output$deathTable <-
            renderTable(
              values$deathTable,
              hover = TRUE,
              border = TRUE,
              spacing = "s",
              digits = 5,
              align = "l"
            )
        }
        
        
        
        tryCatch(
          # This is we run the fisher test for individual wing discs
          {
            if (input$runFishers2 == TRUE) {
              lengthotron <- nrow(wwda2)
              x <- 0
              fishResult <- c()
              fishResultNum <- c()
              fishConfidence <- c()
              fishConfidenceUB <- c()
              fishOddsRatio <- c()
              
              
              while (x < lengthotron) {
                #Get data and format into 2x2 contingency table
                fishers <-
                  matrix(c(wwda2[x + 1, "Number.of.Border.Cells"], wwda2[x + 1, "Number.of.Center.Cells"], wwda2[x +
                                                                                                                   1, "Border.Foci.Count"], wwda2[x + 1, "Center.Foci.Count"]), ncol = 2)
                
                colnames(fishers) <- c("Foci-", "Foci+")
                colnames(fishers) <- c("Foci-", "Foci+")
                rownames(fishers) <- c("Border", "Center")
                fishers <- as.table(fishers)
                
                #Run fisher test and store results
                fishy <- fisher.test(fishers)
                
                
                cfLB <- as.character(c(fishy$conf.int[1]))
                cfUB <- as.character(c(fishy$conf.int[2]))
                odds <- as.character(c(fishy$estimate[1]))
                fishConfidence <- c(fishConfidence, cfLB)
                fishConfidenceUB <- c(fishConfidenceUB, cfUB)
                fishOddsRatio <- c(fishOddsRatio, odds)
                
                
                fishResultNum <-
                  rbind(fishResult, as.numeric(c(fishy$p.value)))
                fishResult <-
                  rbind(fishResult, as.character(c(fishy$p.value)))
                
                x <- x + 1
              }
              
              
              
              #Run and add our p-adjustment
              pAdj <-
                p.adjust(fishResult,
                         method = input$pAdj,
                         n = length(fishResult))
              colnames(fishResult) <- c("p-value")
              colnames(fishResultNum) <- c("p-value")
              fishResult2I <-
                data.frame(
                  Image.Name = wwda2$Image.Name,
                  ConditionAndExperiment = wwda2$ConditionAndExperiment,
                  fishResult,
                  conf.interval.from = fishConfidence,
                  conf.interval.to = fishConfidenceUB,
                  p.adjusted = as.numeric(pAdj),
                  p.adjustment.method = input$pAdj,
                  odds.ratio = fishOddsRatio
                )
              output$siggy <-
                renderTable(
                  fishResult2I,
                  hover = TRUE,
                  border = TRUE,
                  spacing = "s",
                  digits = 5,
                  align = "l"
                )
              fishResult2 <-
                data.frame(
                  Image.Name = wwda2$Image.Name,
                  ConditionAndExperiment = wwda2$ConditionAndExperiment,
                  fishResultNum
                )
              Experimentname <-
                unique(fishResult2$ConditionAndExperiment)
              
              significantCount <- c()
              nonSignificant <- c()
              
              for (j in Experimentname) {
                fishData <- fishResult2 %>% filter(ConditionAndExperiment == j)
                total <- nrow(fishData)
                significant <- 0
                for (x in fishData$p.value) {
                  if (x < 0.05) {
                    significant <- significant + 1
                  }
                }
                significantCount <-
                  c(significantCount, significant)
                nonSignificant <-
                  c(nonSignificant, total - significant)
                
              }
              
              
              fishCounts <-
                data.frame(
                  ConditionAndExperiment = Experimentname,
                  Number.of.Significant.Samples = significantCount,
                  Number.of.Non.Significant.Samples = nonSignificant
                )
              output$fishyCounty <-
                renderTable(
                  fishCounts,
                  hover = TRUE,
                  border = TRUE,
                  spacing = "s",
                  digits = 5,
                  align = "l"
                )
              
            }
          },
          error = function(error_message) {
            message("Unable to prepare single disk fishers")
            return(NA)
          }
        )
        
        
        #Add the genotype to the headers
        headers <- colnames(wwda2)
        headers1 <- headers[1:4]
        headers2 <- headers[5:(length(headers) - 1)]
        headers3 <- headers[length(headers)]
        headers2 <- paste(headers2, genotypeNames[chal], sep = ".")
        headers <- c(headers1, headers2, headers3)
        

        colnames(wwda2) <- headers
        
        
        
        
        
        #Merge our genotype tables together
        if (chal == 1) {
          values$wwda2 <- data.frame(wwda2)
        }
        else {
          values$wwda2 <- merge(values$wwda2, wwda2)
        }
        
        wwda2 <- values$wwda2
        output$contents <-
          renderTable(
            wwda2,
            hover = TRUE,
            border = TRUE,
            spacing = "s",
            digits = 5,
            align = "l",
            width = "auto"
          )
        
        
        #Store these values for later use, and output data visually for user
        
        output$contents <-
          renderTable(
            wwda2,
            hover = TRUE,
            border = TRUE,
            spacing = "s",
            digits = 5,
            align = "l",
            width = "auto"
          )
        
        #Use the column headers to set which parameters the user can choose to analyze
        wwda2sub <-
          subset(
            values$wwda2,
            select = -c(
              File,
              Image.Name,
              Condition,
              Experiment,
              ConditionAndExperiment
            )
          )
        values$choices <- colnames(wwda2sub)
        values$rawChoices <- colnames(values$wwda2)
        values$control2 <- as.character(unique(wwda2$Condition))
        
        
        chal <- chal + 1
      }
      tester2 <- input$sepTimePoints == "Time-lapse analysis"
      tester3 <- input$sepTimePoints == "Z-axis analysis"
      
      if (isTRUE(tester2)) {
        wwda2$Image.Name <- sub(".tp.*", '', wwda2$Image.Name)
        
        
      }
      if (isTRUE(tester3)) {
        wwda2$Image.Name <- sub(".tp.*", '', wwda2$Image.Name)
        wwda2 <-
          wwda2[order(wwda2[, grepl("z.level." , names(wwda2))]),]
        wwda2 <-
          wwda2[order(wwda2[, grepl("Image.Name" , names(wwda2))]),]
        
      }
      values$wwda2 <- wwda2
      
      
    },
    
    error = function(error_message) {
      message("Unable to run wing disc analysis")
      return(NA)
    })
    
    # #This loop attempts to merge the data table made by the wing disc analysis, and merge it with the single cell counts
    tryCatch({
      validate(need(((input$sepTimePoints == "Complete single cell analysis") |
                       (
                         input$sepTimePoints == "Complete single cell analysis - timelapse"
                       )
      ), "Select complete single cell analysis"))
      
      
      #This is the second .csv upload button
      
      singleCellDf <-
        rbindlist(
          lapply(input$csvs2$datapath, read.csv),
          use.names = TRUE,
          fill = TRUE
        )
      
      
      
      
      
      singleCellDf$Image.Name <-
        as.character(singleCellDf$Image.Name)
      if (input$sepTimePoints == "Complete single cell analysis - timelapse") {
        ##place
        singleCellDf$Image.Name <-
          paste(singleCellDf$Image.Name,
                singleCellDf$TimePoint,
                sep = ", timepoint:")
      } else {
        singleCellDf$TimePoint <- NULL
      }
      
      
      #Retrieve the processed whole disc data table
      wwda2 <- values$wwda2
      
      values$regchoices <- colnames(singleCellDf)[-(1:2)]
      
      
      #Merge the data tables and update reactive values
      wwda2 <- merge(wwda2, singleCellDf, sort = TRUE)
      
      
      
      # #The merge step above does not work on csv files from older versions of the macro. This step catches and fixes that
      # if(length(wwda2$Image.Name) == 0){
      #   wwda2$Image.Name <- paste(wwda2$File, wwda2$Image.Name, sep="")
      #   wwda2 <- merge(wwda2, singleCellDf, sort=TRUE)
      #
      # }
      
      
      if (nrow(wwda2) == 0) {
        wwda2 <- values$wwda2
        wwda2 <- merge(wwda2, singleCellDf, sort = TRUE)
      }
      
      #  wwda2$Normalized.Z.Position <- wwda2$
      
      wwda2$Image.Name <-
        gsub(', timepoint:.*', "", wwda2$Image.Name)
      
      #Normalize z-levels to starting z-plane
      wwda2[, grep(pattern = "^Single.Cell.Z.Level.Genotype.", colnames(wwda2))] <-
        wwda2[, grep(pattern = "^Single.Cell.Z.Level.Genotype", colnames(wwda2))] - wwda2[, grep(pattern ="^Starting.Z.Plane", colnames(wwda2))]
      
      #Store these values for later use, and output data visually for user
      values$wwda2 <- wwda2
      
      output$contents <-
        renderTable(
          wwda2[input$startIdex:input$endIdex, ],
          hover = TRUE,
          border = TRUE,
          spacing = "s",
          digits = 5,
          align = "l",
          width = "auto"
        )
      
      #Use the column headers to set which parameters the user can choose to analyze
      
      values$choices <- colnames(values$wwda2)
      values$rawChoices <- colnames(values$wwda2)
      values$control2 <- as.character(unique(wwda2$Condition))

    },
    
    error = function(error_message) {
      message("Unable to perform complete single cell analysis")
      return(NA)
    })
    
  })
  
  
  #########################################################################################################
  ############################### This is where all the code for the data 'Transform Data!' button is######
  #########################################################################################################
  
  transformer <- observeEvent(input$transform, {
    tryCatch({
      #Get the column from the data table we want to transform
      dataToTransform <- input$dataToOperate
      newData <- values$wwda2[[dataToTransform]]
      
      #If the user specified to compare this data to another column, we get that column from the data table
      if (input$valueOrColumn == TRUE) {
        operatorColumn <- input$operatorColumn
        operatorData <- values$wwda2[[operatorColumn]]
        
        #Otherwise, we just get the operator value input by the user
      } else {
        operatorData <- input$operatorValue
      }
      
      
      #Get the operation specified by the user
      operation <- input$operation
      
      if (input$Excluder == FALSE) {
        if (operation == '+') {
          transformed <- newData + operatorData
        }
        if (operation == '-') {
          transformed <- newData - operatorData
        }
        if (operation == '*') {
          transformed <- newData * operatorData
        }
        if (operation == '/') {
          transformed <- newData / operatorData
        }
        if (operation == '^') {
          transformed <- newData ^ operatorData
        }
        if (operation == 'modulus') {
          transformed <- newData %% operatorData
        }
        if (operation == 'log') {
          transformed <- log(newData, base = operatorData)
        }
        
        
        
        
        #Here we make sure there are no duplicate row titles. If there are, we add a suffix
        colTitles <- colnames(values$wwda2)
        newTitle <- input$newTitle
        if (newTitle %in% colTitles) {
          if (newTitle == values$refString) {
            addCount <- values$addCount
          } else {
            addCount <- 0
          }
          values$refString <- newTitle
          values$addCount <- addCount + 1
          addCount <- toString(values$addCount)
          newTitle <- paste(newTitle, "(", addCount, ")")
        }
        newCol <- c(newTitle, colTitles)
        
        if ((input$sepTimePoints == "Complete single cell analysis") |
            (input$sepTimePoints == "Complete single cell analysis - timelapse")) {
          values$regchoices <- c(values$regchoices, newTitle)
        }
        
        #Add the new data to the output table
        wwda2 <- cbind(transformed, values$wwda2)
        colnames(wwda2) <- newCol
        
        #Store these values for later use, and output data visually for user
        values$wwda2 <- wwda2
      } else {
        wwda2 <- values$wwda2
        
        
        deleter <-
          paste("wwda2$",
                dataToTransform,
                operation,
                operatorData,
                sep = "")
        
        wwda2 <- wwda2[eval(parse(text = deleter)), ]
        values$wwda2 <- wwda2
        
        
      }
      
      output$contents <-
        renderTable(
          wwda2,
          hover = TRUE,
          border = TRUE,
          spacing = "s",
          digits = 5,
          align = "l",
          width = "auto"
        )
      
      #Use the column headers to set which parameters the user can choose to analyze
      
      values$rawChoices <- colnames(values$wwda2)
      values$choices <- colnames(values$wwda2)
      values$control2 <- as.character(unique(wwda2$Condition))
      tryCatch({
        wwda2sub <-
          subset(
            wwda2,
            select = -c(
              File,
              Image.Name,
              Condition,
              Experiment,
              ConditionAndExperiment
            )
          )
        values$choices <- colnames(wwda2sub)
      },
      
      error = function(error_message) {
        message("Unable to make wwdasub")
        return(NA)
      })
      
      values$rawChoices <- colnames(values$wwda2)
      
    },
    
    error = function(error_message) {
      message("Unable to transform data")
      return(NA)
    })
    
  })
  
  
  ###################################################################################################
  ############################### This is where all the code for the 'Regression' button is#############
  ###################################################################################################
  
  regressionTesting <- observeEvent(input$goReg, {
    output$regressionData <- renderTable(data.frame())
    output$chiSquaredHeader <- renderText(" ")
    output$NCVheader <- renderText(" ")
    output$residualsPlotHeader <- renderText(" ")
    output$NCVtable <- NULL
    output$DWTheader <- renderText("")
    output$DWTtable <- NULL
    output$residualsPlot <- NULL
    output$meanVarTable <- NULL
    
    output$regressionChiSquare <- renderTable(data.frame())
    output$predicted.data.plot <- renderPlot(ggplot())
    output$regressionSummary <- renderPrint("")
    output$Rsquared <- renderPrint("")
    
    tryCatch({
      # Attempt to run regression analysis
      nullVal <- input$regressionParameter1
      
      
      
      #Get all our data
      logRegData <- values$wwda2
      
      
      
      logRegData <- logRegData[order(logRegData$Condition, logRegData$Experiment),]
      
      
      
      
      if (isTRUE(input$excludecheckreg2 == TRUE)) {
        if (isTRUE(input$excludecheckreg == FALSE)) {
          excluder <-
            paste(
              "logRegData$",
              input$regressionParameterExclude,
              input$excludeOperatorReg,
              input$excludeValuereg,
              sep = ""
            )
        } else {
          excluder <-
            paste(
              "logRegData$",
              input$regressionParameterExclude,
              input$excludeOperatorReg,
              " logRegData$",
              input$regressionParameter2,
              sep = ""
            )
        }
        
        logRegData <- logRegData[eval(parse(text = excluder)), ]
        
      }
      
      #Subset our data based on user specified factors
      factors <- input$factorCheckboxes
      
      
      
      #Convert characters to factors and get rid of 1 level factors. This prevents an error being thrown
      logRegDataSub <-
        logRegData[, as.numeric(factors), drop = FALSE]
      logRegDataSub[sapply(logRegDataSub, is.ordered)] <-
        lapply(logRegDataSub[sapply(logRegDataSub, is.ordered)],
               as.character)
      logRegDataSub[sapply(logRegDataSub, is.character)] <-
        lapply(logRegDataSub[sapply(logRegDataSub, is.character)],
               as.factor)
      logRegDataSub <-
        logRegDataSub[, sapply(logRegDataSub, nlevels) != 1]
      
      values$newFactors <- colnames(logRegDataSub)
      
      
      if (isTRUE(input$linkFunction == "Logistic")) {
        #Run our logistic regression
        
        
        posVal <- input$regressionParameter2
        
        #This runs the logistic regression if we have two inputs: a number of positive values and a number of negative values
        if (isTRUE(posVal != "None / Not Applicable")) {
          dependent <- cbind(logRegData[[posVal]], logRegData[[nullVal]])
          logRegDataSub$dependent <- dependent
          regression <-
            glm(
              dependent ~ . ,
              family = binomial(link = "logit"),
              data = logRegDataSub,
              na.action = na.omit
            )
          #regression <- glm(cbind(logRegData[[posVal]], logRegData[[nullVal]])~. , family = binomial(link="logit"), data=logRegDataSub, na.action = na.omit)
          
        #This is where we run the logistic regression where each value is a success or a failure
        } else  {
          dependent <- logRegData[[nullVal]]
          logRegDataSub$dependent <- dependent
          
          regression <-
            glm(
              dependent ~ . ,
              family = binomial(link = "logit"),
              data = logRegDataSub,
              na.action = na.omit
            )
          
        }
        
        
        
        tryCatch({
          
          #Pull the coefficients from the results
          regOutput <- summary(regression)
          regOutput <- regOutput$coefficients
          
          
          #Adjust the p-values to control FWER
          regOutputNew <- data.frame(regOutput)
          pvalsReg <- regOutputNew$Pr...z..
          padjreg <-
            p.adjust(pvalsReg,
                     method = input$regAdjust ,
                     n = length(pvalsReg))
          
          
          counter <- 2
          logOddsVec <- c(" ", " ", " ")
          
          while (counter <= nrow(regOutput)) {
            logOddsCI <-
              exp(regOutput[counter, 1] + qnorm(c(0.5, 0.25, 0.975)) * regOutput[counter, 2])
            logOddsVec <- rbind(logOddsVec, logOddsCI)
            counter <- counter + 1
          }
          
          
          
          logOddsVec <- cbind(padjreg, logOddsVec)
          
          tipo = paste("Adjusted p-value(", input$regAdjust, ")", sep =  "")
          colnames(logOddsVec) <-
            c(tipo,
              "Odds Ratio",
              "Conf. Int. Lower",
              "Conf. Int. Upper")
          
          #We now have a table with all the parameters of interest we want to output to the user
          regOutput <- cbind(regOutput, logOddsVec)
          output$regressionData <-
            renderTable(
              regOutput,
              rownames = TRUE,
              hover = TRUE,
              border = TRUE,
              spacing = "s",
              digits = 5,
              align = "l"
            )
          
        },
        
        error = function(error_message) {
          message("Unable to generate regression table 1")
          return(NA)
        })
        
        #Attempt to run the pseudo R2 function
        tryCatch({
          r2e <- nagelkerke(regression, restrictNobs = TRUE)
          output$Rsquared <- renderPrint(r2e)
        },
        # Unable to run the pseudo r-squared
        error = function(error_message) {
          message("Unable to run r-squared")
          return(NA)
        })
        
        
        #Create a table of the actual values we fed into the analysis and the predicted values generated by the model
        #This will be used to create a diagnostic plot to help users see how their model performs
        
        predicted.data <- data.frame(probability.predicted = regression$fitted.values,
                                     if (isTRUE(typeof(posVal == "character")) &
                                         (isTRUE(posVal != "None / Not Applicable"))) {
                                       probability.observed = logRegData[[posVal]] / (logRegData[[nullVal]] + logRegData[[posVal]])
                                     } else {
                                       probability.observed = logRegData[[nullVal]]
                                     })
        
        
        #Generate the predicted vs. observed probability plots
        if ((isTRUE(
          (input$sepTimePoints == "Complete single cell analysis") |
          (
            input$sepTimePoints == "Complete single cell analysis - timelapse"
          )
        ))) {
          predicted.data.plot <-
            ggplot(data = predicted.data,
                   aes(x = probability.predicted, y = probability.observed)) + geom_point() +
            geom_smooth(method = 'glm', formula = y ~ x) + theme_tufte() + ggtitle("Predicted probability vs. observed probability")
          output$predicted.data.plot <-
            renderPlot(predicted.data.plot)
          
        } else {
          predicted.data.plot <-
            ggplot(data = predicted.data,
                   aes(x = probability.predicted, y = probability.observed)) + geom_point() +
            geom_smooth(method = 'lm', formula = y ~ x) + theme_tufte() + ggtitle("Predicted probability vs. observed probability")
          output$predicted.data.plot <-
            renderPlot(predicted.data.plot)
        }
        
        #Generate standard diagnostic plots
        output$residualsPlot <-
          renderPlot(glm.diag.plots(regression))
        output$residualsPlotHeader <-
          renderText("GLM Diagnostic Plots")
        
      #This is where we perform the Poisson regression
      } else if (isTRUE(input$linkFunction == "Poisson")) {
        
        #Here we generate a table showing mean and variance values, to see if Poisson assumptions are met
        meanVarTable <- data.frame(
          variable = nullVal,
          mean = mean(logRegData[[nullVal]]),
          variance = var(logRegData[[nullVal]])
        )
        
        output$meanVarTable <-
          renderTable(
            meanVarTable,
            rownames = TRUE,
            hover = TRUE,
            border = TRUE,
            spacing = "s",
            digits = 5,
            align = "l"
          )
        output$NCVheader <-
          renderText("For Poisson regression, the mean and variance should be approximately equal")
        
        
        #Run Poisson regression
        dependent <- logRegData[[nullVal]]
        logRegDataSub$dependent <- dependent
        regression <-
          glm(
            dependent ~ .,
            family = "poisson",
            data = logRegDataSub,
            na.action = na.omit
          )
        
        print ("Poisson regression successfully run")
        tryCatch({
          
          #Calculate a robust standard error after Cameron and Trivedi (2009)
          cov.regression <- vcovHC(regression, type = "HC0")
          std.err <- sqrt(diag(cov.regression))
          r.est <-
            cbind(
              Estimate = coef(regression),
              "Robust Std. Err." = std.err,
              "p.value" = 2 * pnorm(abs(coef(
                regression
              ) / std.err), lower.tail = FALSE),
              
              "Conf. Int. Lower" = coef(regression) - 1.96 * std.err,
              "Conf. Int. Upper" = coef(regression) + 1.96 * std.err
            )
          print("Robust standard error calculated")
          
          #Perform Chi-squared goodness of fit test
          chiSquareTest <-
            with(
              regression,
              cbind(
                residual.deviance = deviance,
                deg.of.freedom = df.residual,
                p.value = pchisq(deviance, df.residual, lower.tail =
                                   FALSE)
              )
            )
          
          
          
          output$chiSquaredHeader <-
            renderText("Goodness-of-fit chi squared test")
          output$regressionChiSquare <-
            renderTable(
              chiSquareTest,
              rownames = TRUE,
              hover = TRUE,
              border = TRUE,
              spacing = "s",
              digits = 5,
              align = "l"
            )
          
          
          print ("chi square test run")
          
          
          counter <- 1
          formList <- c()
          while (counter <= nrow(r.est)) {
            formList <-
              c(formList, as.formula(paste(
                " ~ exp(x", toString(counter), ")", sep = ""
              )))
            counter <- counter + 1
          }
          formList <- as.list(formList)
          
          
          s <- deltamethod(formList, coef(regression), cov.regression)
          
          print("Delta Method Run")
          
          
          ## exponentiate old estimates dropping the p values
          rexp.est <- exp(r.est[,-3])
          
   
          
          ## replace SEs with estimates for exponentiated coefficients
          rexp.est[, "Robust Std. Err."] <- s
          
          pvalsadj <- p.adjust(as.numeric(r.est[,3]), method = input$regAdjust , n = length(r.est[,3]))
          
          
          rexp.est <- cbind(pvalsadj, rexp.est)
          
          colnames(rexp.est) <-
            c(
              paste("Adjusted p-value(", input$regAdjust, ")", sep = ""),
              "Incident Rate Ratio",
              "IRR Robust Std. Err",
              "IRR Conf. Int. Lower",
              "IRR Conf. Int. Upper"
            )
          
          
          r.est <- cbind(r.est, rexp.est)
          
          print ("Poisson regression outputs prepared")
          
          output$regressionData <-
            renderTable(
              r.est,
              rownames = TRUE,
              hover = TRUE,
              border = TRUE,
              spacing = "s",
              digits = 5,
              align = "l"
            )
        },
        # Unable to generate regression plot
        error = function(error_message) {
          message("Unable to generate poisson regression table")
          return(NA)
        })
        
        #Attempt to run R-squared
        tryCatch({
          r2e <- nagelkerke(regression, restrictNobs = TRUE)
          output$Rsquared <- renderPrint(r2e)
        },
        # Unable to run r-squared
        error = function(error_message) {
          message("Unable to run r-squared")
          return(NA)
        })
        
        
        #Prepare predicted vs. observed values table, to help users see how well their model performed
        predicted.data <- data.frame(value.predicted = regression$fitted.values,
                                     value.observed = logRegData[[nullVal]])
        
        predicted.data.plot <-
          ggplot(data = predicted.data, aes(x = value.observed, y = value.predicted)) +
          geom_point() + geom_smooth(method = 'lm', formula = y ~ x) + theme_tufte() +
          ggtitle("Predicted probability vs. observed probability")
        output$predicted.data.plot <-
          renderPlot(predicted.data.plot)

        #Generate standard diagnostic plots
        output$residualsPlot <-
          renderPlot(glm.diag.plots(regression))
        output$residualsPlotHeader <-
          renderText("GLM Diagnostic Plots")
        
      #Here we run the negative binomial regression
      } else if (isTRUE(input$linkFunction == "Negative Binomial")) {
        
        #Prepare a table for users to see the mean and variance to see if negative binomial assumptions are met
        meanVarTable <- data.frame(
          variable = nullVal,
          mean = mean(logRegData[[nullVal]]),
          variance = var(logRegData[[nullVal]])
        )
        
        output$meanVarTable <-
          renderTable(
            meanVarTable,
            rownames = TRUE,
            hover = TRUE,
            border = TRUE,
            spacing = "s",
            digits = 5,
            align = "l"
          )
        output$NCVheader <-
          renderText("For Negative regression, the data should exhibit over-dispersion")
        dependent <- logRegData[[nullVal]]
        logRegDataSub$dependent <- dependent
        
        #Run the regression
        regression <-
          glm.nb(dependent ~ ., data = logRegDataSub, na.action = na.omit)
        
        #Perform a chi-squared goodness of fit test
        tryCatch({
          chiSquareTest <-
            with(
              regression,
              cbind(
                residual.deviance = deviance,
                deg.of.freedom = df.residual,
                p.value = pchisq(deviance, df.residual, lower.tail =
                                   FALSE)
              )
            )
          
          
          
          output$chiSquaredHeader <-
            renderText("Goodness-of-fit chi squared test")
          output$regressionChiSquare <-
            renderTable(
              chiSquareTest,
              rownames = TRUE,
              hover = TRUE,
              border = TRUE,
              spacing = "s",
              digits = 5,
              align = "l"
            )
          
        },
        # Unable to generate regression plot
        error = function(error_message) {
          message("Unable to run chi square")
          return(NA)
        })
        
        tryCatch({
          regOutput <- summary(regression)
          regOutput <- regOutput$coefficients
          
          ## exponentiate old estimates dropping the p values
          est <- regOutput[, 1]
          est <- exp(est)
          
          #Adjust the p-values to control FWER
          regOutputNew <- data.frame(regOutput)
          pvalsReg <- regOutputNew$Pr...z..
          padjreg <-
            p.adjust(pvalsReg,
                     method = input$regAdjust ,
                     n = length(pvalsReg))
          tipo <-
            paste("Adjusted p-value(", input$regAdjust, ")", sep = "")
          collos <- colnames(regOutput)
          collos <- c(collos, tipo)
          regOutput <- cbind(regOutput, padjreg)
          colnames(regOutput) <- collos
          
          regOutput <- cbind(regOutput, "Incident Rate Ratio" = est)
          
          output$regressionData <-
            renderTable(
              regOutput,
              rownames = TRUE,
              hover = TRUE,
              border = TRUE,
              spacing = "s",
              digits = 5,
              align = "l"
            )
        },
        # Unable to generate regression plot
        error = function(error_message) {
          message("Unable to generate regression table")
          return(NA)
        })
        
        #Attempt to run r-squared
        tryCatch({
          r2e <- nagelkerke(regression, restrictNobs = TRUE)
          output$Rsquared <- renderPrint(r2e)
        },
        # Unable to run r-squared
        error = function(error_message) {
          message("Unable to run r-squared")
          return(NA)
        })
        
        #Attempt to generate the predicted vs observed data plot
        tryCatch({
          predicted.data <- data.frame(value.predicted = regression$fitted.values,
                                       value.observed = logRegData[[nullVal]])
          
          predicted.data.plot <-
            ggplot(data = predicted.data, aes(x = value.observed, y = value.predicted)) +
            geom_point() + geom_smooth(method = 'lm', formula = y ~ x) + theme_tufte() +
            ggtitle("Predicted probability vs. observed probability")
          output$predicted.data.plot <-
            renderPlot(predicted.data.plot)
        },
        # Unable to run r-squared
        error = function(error_message) {
          message("Unable to run r-squared")
          return(NA)
        })
        
        
        #Generate standard diagnostic plots
        output$residualsPlot <-
          renderPlot(glm.diag.plots(regression))
        output$residualsPlotHeader <-
          renderText("GLM Diagnostic Plots")
        
      } else {
        
       
        #Here we run the linear regression analysis
        dependent <- logRegData[[nullVal]]
        logRegDataSub$dependent <- dependent
        regression <-
          lm(dependent ~ .,  data = logRegDataSub, na.action = na.omit)
        
        #Convert results to table output for display
        print ("Linear regression performed")
        
       # regOutput <- data.frame(Note = "Linear regression results are displayed in the Raw Outputs tab")
        regOutput <- summ(regression, confint = TRUE, digits = 4)
  
        regOutput <- regOutput$coeftable
    
        pvalstobe <- regOutput[,5]
    
        pvalsadj <-  p.adjust(pvalstobe,
                              method = input$regAdjust ,
                              n = length(pvalstobe))
        
      
        regOutput <- cbind(regOutput, pvalsadj)
      
        colnames(regOutput) <- c("Estimate", "Conf. Int. Upp.", "Conf. Int. Low.", "t-value", "p-value", paste("Adjusted p-value(", input$regAdjust, ")", sep = ""))
        
        output$regressionData <-
          renderTable(
            regOutput,
            rownames = TRUE,
            hover = TRUE,
            border = TRUE,
            spacing = "s",
            digits = 5,
            align = "l"
          )
        
        print ("Linear regression notice")
        
        #Run non-constant variances of error test
        tryCatch({
          NCV <- ncvTest(regression)
          output$NCVheader <-
            renderText("Non-constant variance of error test (NCV)")
          output$NCVtable <- renderPrint(NCV)
        },
        # Unable to NCV test
        error = function(error_message) {
          message("Unable to run NCV test")
          return(NA)
        })
        
        #Attempt to run Durbin-Watson test
        tryCatch({
          lagBase <- regression$coefficients
          
          lagBase <- length(lagBase) - 1
          
          dwt <-
            durbinWatsonTest(regression, max.lag = lagBase, reps = 1000)
          output$DWTheader <-
            renderText("Durbin-Watson test for auto-correlation of error terms")
          output$DWTtable <- renderPrint(dwt)
        },
        error = function(error_message) {
          message("Unable to run durbin-watson test test")
          return(NA)
        })
        
        #Generate predicted vs observed values plot
        tryCatch({
          predicted.data <- data.frame(value.predicted = regression$fitted.values,
                                       value.observed = logRegData[[nullVal]])
          
          predicted.data.plot <-
            ggplot(data = predicted.data, aes(x = value.observed, y = value.predicted)) +
            geom_point() + geom_smooth(method = 'lm', formula = y ~ x) + theme_tufte() +
            ggtitle("Predicted value vs. observed value")
          output$predicted.data.plot <-
            renderPlot(predicted.data.plot)
        },
        
        error = function(error_message) {
          message("Unable to make predictions plot")
          return(NA)
        })
        
        tryCatch({
          output$residualsPlot <- renderPlot({
            par(mfrow = c(2, 2))
            plot(regression, id.n = 3)
          })
          output$residualsPlotHeader <-
            renderText("Residuals plot of linear regression model")
        },
        
        error = function(error_message) {
          message("Unable to make predictions plot")
          return(NA)
        })
        
      }
      
      
      #Attempt to run VIF test
      tryCatch({
        vifOut <- car::vif(regression)
        output$vifHeader <-
          renderText("Variance inflation factors test (VIF) for multicollinear features")
        
        output$regressionVif <-
          renderTable(
            vifOut,
            include.colnames = TRUE,
            rownames = TRUE,
            hover = TRUE,
            border = TRUE,
            spacing = "s",
            digits = 5,
            align = "l"
          )
      },
      
      error = function(error_message) {
        message("Unable to run vif test")
        return(NA)
      })
      
      
      #Plot values to correlation matrix
      tryCatch({
        if (isTRUE(input$linkFunction == "Logistic") &
            isFALSE(input$sepTimePoints == "Complete single cell analysis") &
            isFALSE (input$sepTimePoints == "Complete single cell analysis - timelapse")) {
          Dependent.Variable <-
            logRegData[[posVal]] / (logRegData[[nullVal]] + logRegData[[posVal]])
          
        } else {
          Dependent.Variable <- logRegData[[nullVal]]
        }
        
        newLogReg <- logRegDataSub
        newLogReg <- cbind(Dependent.Variable, newLogReg)
        
        indx <- sapply(newLogReg, is.factor)
        newLogReg[indx] <-
          lapply(newLogReg[indx], function(x)
            as.integer(x))
        matrixCorr <- as.matrix(newLogReg)
        
        output$corrMat <-
          renderPlot(
            chart.Correlation(
              matrixCorr,
              histogram = TRUE,
              pch = 19,
              method = input$corrMatTest
            )
          )
        
      },
      # Unable to create correlation matrix
      error = function(error_message) {
        message("Unable to run correlation matrix")
        return(NA)
      })
      
      
      print ("Regression data processed")
      

      logSum <- summary(regression)
      output$regressionSummary <- renderPrint(logSum)
      
      
      print ("Regression raw data printed")
      
      #Make effects plot
      formulaP <- as.formula("~.")
      
      output$RegEffectsPlot <-
        renderPlot(plot(
          predictorEffects(regression, formulaP),
          main = "",
          lines = list(multiline = TRUE, z.var = "Condition"),
          type = "response"
        ))
      
      values$regression <- regression
      values$formulaP <- formulaP
      
      
      
      
      
      
    },
    # Unable to run regression
    error = function(error_message) {
      message("Unable to run regression")
      return(NA)
    })
    
  })
  
  
  #Adjust format of results plot
  analysis <- observeEvent(input$columnsReg, {
    cols <- input$columnsReg
    regression <- values$regression
    formulaP <- paste(cols, collapse = " + ")
    formulaP <- paste("~", formulaP, sep = " ")
    formulaP <- as.formula(formulaP)
    output$RegEffectsPlot <-
      renderPlot(plot(
        predictorEffects(regression, formulaP),
        main = "",
        lines = list(multipline = TRUE),
        type = "response"
      ))
    values$formulaP <- formulaP
    
  })
  
  # #Adjust format of results plot
  # analysis <- observeEvent(input$columnsReg2, {
  #
  #   formulaP <- values$formulaP
  #   regression <- values$regression
  #   newFactors <- values$newFactors
  #   zGroup<-input$columnsReg2
  #   zGroupIdex <- match(zGroup, newFactors )
  #   print(zGroupIdex)
  #   if (zGroup != "N/A"){
  #     output$RegEffectsPlot <- renderPlot( plot(predictorEffects( regression, formulaP , lines=list(multiline=TRUE, z.var=zGroupIdex))))
  #   } else {
  #     output$RegEffectsPlot <- renderPlot( plot(predictorEffects( regression, formulaP , se=FALSE), lines=list(multiline=TRUE)))
  #   }
  #   print(zGroupIdex)
  #   values$zGroupIdex <- zGroupIdex
  # })
  #
  
  
  analysis <- observeEvent(input$columns, {
    cols <- input$columns
    
    
    if (length(input$columns) == 1) {
      df <- data.frame(values$wwda2[, cols])
      names(df) <- names(values$wwda2[, cols])
      output$contents = renderTable(
        df[input$startIdex:input$endIdex, ],
        hover = TRUE,
        border = TRUE,
        spacing = "s",
        digits = 5,
        align = "l",
        width = "auto"
      )
      
    } else{
      output$contents = renderTable(
        values$wwda2[input$startIdex:input$endIdex, cols],
        hover = TRUE,
        border = TRUE,
        spacing = "s",
        digits = 5,
        align = "l",
        width = "auto"
      )
      values$cols <- cols
    }
  })
  
  analysis <- observeEvent(input$columnsGo, {
    cols <- values$cols
    
    
    #attempt to invert z-planes
    tryCatch({
      
      
      wwda2 <- values$wwda2
      wwda2$Image.Name <- sub(".tp.*", '', wwda2$Image.Name)
      cols2 <- input$columns2
      
      
      if (is.null(cols2) == FALSE) {
        removed <- wwda2[wwda2$Image.Name %in% cols2, ]
        
        
        
        if (((input$sepTimePoints == "Complete single cell analysis") |
             (
               input$sepTimePoints == "Complete single cell analysis - timelapse"
             )
        )) {
        
          removed[, grepl("Single.Cell.Z.Level" , names(removed))] <-
            abs(removed[, grepl("Single.Cell.Z.Level" , names(removed))] - removed[,grepl("Number.of.Z.Slices", names(removed))])
          
          
        } else{
          removed <-
            removed[order(removed[, grepl("z.level." , names(removed))]),]
          
          for (item in cols2) {
            removed[removed$Image.Name == item , grepl("z.level." , names(removed))] <-
              rev(removed[removed$Image.Name == item , grepl("z.level." , names(removed))])
          }
          
          
          
          #removed <- removed[order(removed$z.level.Losers),]
          # removed$z.level.Losers <- rev(removed$z.level.Losers)
          
        }
        redux <- wwda2[!(wwda2$Image.Name %in% cols2), ]
        sorted <- rbind(redux, removed)
        sorted <- sorted[order(sorted$Image.Name), ]
        values$wwda2 <- sorted
        
        output$contents <-
          renderTable(
            head(values$wwda2, 50)[, cols],
            hover = TRUE,
            border = TRUE,
            spacing = "s",
            digits = 5,
            align = "l",
            width = "auto"
          )
        
          print ("Selected values inverted")
        
        
        
        
        output$contents <-
          renderTable(
            head(values$wwda2, 50)[, cols],
            hover = TRUE,
            border = TRUE,
            spacing = "s",
            digits = 5,
            align = "l",
            width = "auto"
          )
      }
    },
   
    error = function(error_message) {
      message("Unable to invert z-planes")
      return(NA)
    })
  })
  
  
  
  analysis <- observeEvent(input$genPlot, {
    ############################################################
    ################# Specify statistics! #########################
    #############################################################
    
    
    #Reset all our outputs so we don't get confused
    output$statsTable <-
      renderTable(
        data.frame(),
        hover = TRUE,
        border = TRUE,
        spacing = "s",
        digits = 5,
        align = "l"
      )
    output$statsHeader <- renderText("")
    output$normality <-
      renderTable(
        data.frame(),
        hover = TRUE,
        border = TRUE,
        spacing = "s",
        digits = 5,
        align = "l"
      )
    output$normality2 <-
      renderTable(
        data.frame(),
        hover = TRUE,
        border = TRUE,
        spacing = "s",
        digits = 5,
        align = "l"
      )
    output$normalityHeader <- renderText("")
    output$normalityHeader2 <- renderText("")
    output$fligner <-
      renderTable(
        data.frame(),
        hover = TRUE,
        border = TRUE,
        spacing = "s",
        digits = 5,
        align = "l"
      )
    output$leveneHeader <- renderText("")
    output$plotso <-
      renderUI({
        img(src = "Logo.png",
            height = "65%",
            width = "65%")
      })
    output$fishBC <-
      renderTable(
        data.frame(),
        hover = TRUE,
        border = TRUE,
        spacing = "s",
        digits = 5,
        align = "l"
      )
    output$fishyPrint <- renderText("")
    output$fishyCounty <-
      renderTable(
        data.frame(),
        hover = TRUE,
        border = TRUE,
        spacing = "s",
        digits = 5,
        align = "l"
      )
    output$fishTable <-
      renderTable(
        data.frame(),
        hover = TRUE,
        border = TRUE,
        spacing = "s",
        digits = 5,
        align = "l"
      )
    print("Outputs reset")
    
    
    
    #This option sets the test we will use for stats.compare.means. Only basic means testing is available here.
    #All other statistical tests will be handled elsewhere
    values$method <- input$statsType
    
    
    

    
    #yarp is the data
    yarp <- values$wwda2
    
    #narp is the specified dataset for analysis
    narp <- input$plotter
    plottitle  <- toString(narp)
    
    
    
    #We convert these values to a formula that the ggplot algorithm can take
    plotformula <- paste(plottitle, "~ Condition", sep = " ")
    
    #Set up the comparisons variable for stat_compare_means
    comparisonVectors <- list()
    
    #This loop goes through and determines which comparisons to make for statistical tests and outputs on graphs
    comparisonsToBe <- unique(values$wwda2$Condition)
    comparisonsToBe2 <- comparisonsToBe[-1]
    if (isTRUE(input$refgroup != "All/Basemean")) {
      countington <- 1
      for (item in comparisonsToBe) {
        if (isTRUE(input$refgroup == "None")) {
          for (item2 in comparisonsToBe2) {
            comparisonVectors[[countington]] <- c(item, item2)
            countington <- countington + 1
          }
          comparisonsToBe2 <- comparisonsToBe2[-1]
          
        } else {
          if (isTRUE(item != input$refgroup)) {
            comparisonVectors[[countington]] <- c(input$refgroup, item)
            countington <- countington + 1
          }
        }
      }
    }
    
    
    #Extract just the column corresponding to the specified dataset to analyze
    values$idx <- grep(plottitle, colnames(values$wwda2))
    
    ################################################################################################################
    ############################# Here we prepare our paired box plot data #########################################
    ################################################################################################################
    
    
    
    if ((input$plotType == "Paired Box Plot") |
        (input$plotType == "Scatter Plot") |
        (input$plotType == "Scatter Plot w/ Regression Line") |
        (input$plotType == "Line Plot") |
        (((input$plotType == "Histogram") |
          (input$plotType == "Histogram w/ Density Plot")
        ) & isFALSE(input$plotterpaired == "N/A"))) {
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
      if (values$analysisType == "cloneTable") {
        xValID <- "CloneName"
      }
      if (values$analysisType == "cloneTable") {
        xValID <- "CloneID"
      }
      if (values$randoTable == TRUE) {
        xValID <- "ID"
      }
      
      
      #extract the data column from the second user-specified dataset
      values$plotData2 <- yarp[, c(as.character(zarp))]
      
      
      #Convert to formula for ggplot function to accept
      plotformula2 <-
        paste(xValP, "ConditionAndExperiment", sep = " ~ ")
      values$plotformula2 <- plotformula2
      
      #we extract the titles of all the columns we are interested/need to use for the paired plot
      idx <- grep(plottitle, colnames(values$wwda2))
      wwda2 <- values$wwda2
      
      idx11 <- grep(xValP, colnames(values$wwda2))
      idx2 <- grep(yValP, colnames(values$wwda2))
      idx3 <- grep(xValID, colnames(values$wwda2))
      idx4 <- grep("ConditionAndExperiment", colnames(values$wwda2))
      idx5 <- grep("Condition", colnames(values$wwda2))
      idx6 <- grep("Experiment", colnames(values$wwda2))
      
      #We use the column headers to make a new dataframe with just what we are interested in for the paired box plot and store it as the variable "pairing"
      wwda2 <- wwda2[, c(idx11, idx2, idx3, idx4, idx5, idx6)]
      pairing <- wwda2
      
      
      #extract our first dataset from the dataframe as a vector
      dataVector <- wwda2[xValP]
      names(dataVector) <- "xValues"
      
      #extract our second dataset from the dataframe as a vector
      dataVector2 <- wwda2[yValP]
      names(dataVector2) <- "xValues"
      nameVector <- wwda2["ConditionAndExperiment"]
      
      
      #Create a new table for our paired data for statistical analysis
      nameVector2 <- nameVector
      IDvector <-
        rep(xValP,
            times = 1,
            length.out = nrow(dataVector))
      IDvector2 <-
        rep(yValP,
            times = 1,
            length.out =  nrow(dataVector2))
      IDvector <- c(as.character(IDvector), as.character(IDvector2))
      names(IDvector) <- "Identification"
      names(IDvector2) <- "Identification"
      pairDataStat <- data.frame(
        xValues = rbind(dataVector, dataVector2),
        Identification = IDvector,
        ConditionAndExperiment = rbind(nameVector, nameVector2)
      )
      
      
      
      if ((xValP == "Percent.Foci.Coverage.of.Center") |
          (yValP == "Percent.Foci.Coverage.of.Center")) {
        tryCatch({
          # This is what I want to do...
          pairDataStat <- data.frame(
            xValues = rbind(dataVector, dataVector2),
            Identification = IDvector,
            ConditionAndExperiment = rbind(nameVector, nameVector2),
            CellsInCloneCenter = rbind(cval, cval2)
          )
          pairDataStat <-
            pairDataStat[pairDataStat$Cells.In.Clone.Center > 5, ]
        },
        # ... but if an error occurs, tell me what happened:
        error = function(error_message) {
          message("Unable to calculate cells in clone center")
          return(NA)
        })
      }
      
      pairDataStat <- na.omit(pairDataStat)
      values$pairDataStat <- pairDataStat
      
      #Output the processed data to reactive values
      wwda2 <- na.omit(wwda2)
      values$pairedwwda <- wwda2
      values$pairDataStat <- pairDataStat
      wwda2 <- values$wwda2
    }
    
    values$plotOutTitle <- plottitle
    if (input$Title != "") {
      values$plotOutTitle <- input$Title
    }
    
    
    dots <- input$dots
    
    ytitle <- toString(input$y)
    
    values$plotData <- yarp[, c(as.character(narp))]
    #######################################################################################################
    
    
    ##################################################################################################################################
    ############### Plot Generation ##################################################################################################
    ##################################################################################################################################
    
    
    values$backgroundFill <- input$backgroundFill
    values$backgroundColor <- input$backgroundColor
    values$titleSize <- input$titleSize
    values$axisFont <- input$axisFont
    
    
    
    if (input$plotType == "Paired Box Plot") {
      values$facetVal <- ". ~ ConditionAndExperiment"
    } else{
      values$facetVal <- ". ~ Experiment"
    }
    
    if (input$facet != "") {
      values$facetVal <- input$facet
    }
    tryCatch({
      values$facetVal <- as.formula(values$facetVal)
    },
    # ... but if an error occurs, tell me what happened:
    error = function(error_message) {
      message("Unable to generate facet formula")
      return(NA)
    })
    
    desfacito <- length(unique(values$wwda2$Experiment))
    if (input$plotType == "Paired Box Plot") {
      desfacito <- length(unique(values$wwda2$ConditionAndExperiment))
    }
    
    
    #############################################################################################################
    ##################################### Generate plot##########################################################
    #############################################################################################################
    
    
    
    tryCatch({
      #######################################
      #Set the correlation statistical method from user input
      if (input$statsType == "Kendall Tau Correlation") {
        values$corrMethod <- "kendall"
      } else if (input$statsType ==  "Pearson r Correlation") {
        values$corrMethod <- "pearson"
      } else {
        values$corrMethod <- "spearman"
      }
      
      ######################################
      # Set our colors based on user input
      
      if (is.null(input$cgDot)) {
        values$dotColor <- input$dotColor
      } else {
        if (input$cgDot == "No color grouping") {
          values$dotColor <- input$dotColor
        } else {
          items <- values$wwda2[, c(as.character(input$cgDot))]
          dict_names <- unique(items)
          values$dotColor <- match(items, dict_names) + 1
        }
      }
      
      
      if (is.null(input$cgDotFill)) {
        values$dotFill <- input$dotFill
      } else {
        if (input$cgDotFill == "No color grouping") {
          values$dotFill <- input$dotFill
        } else {
          items <- values$wwda2[, c(as.character(input$cgDotFill))]
          dict_names <- unique(items)
          values$dotFill <- match(items, dict_names) + 1
        }
      }
      
      
      xtitle <- input$xValue

      
      values$colorv <- input$color
      values$fillv <- input$fill
      if (is.null(input$cgColor)) {
        values$color <- input$color
      } else {
        if (input$cgColor == "No color grouping") {
          values$color <- input$color
        } else {
          
  
          items <- as.vector(values$wwda2[, c(as.character(input$cgColor))])
          dict_names <- unique(items)
          

          if (input$facetChoice == TRUE) {

            if (input$cgColor == "Condition") {
              values$color <-
                rep(2:(length(unique(values$wwda2$Condition))+1), length(unique(values$wwda2$Experiment)))
            } else if (input$cgColor == "Experiment") {
              values$color <-
                rep(2:(length(unique(values$wwda2$Condition))+1), each = length(unique(values$wwda2$Experiment)))
            } else {
              values$color <- 2:length(dict_names)
            }
    
          } else {
            if (input$cgColor == "Condition") {
              values$color <- 2:(length(unique(values$wwda2$Condition))+1)
            } else if (input$cgColor == "Experiment") {
              values$color <- 2
            } else {
              values$color <- 2:(length(dict_names)+1)
            }
            
          }
          
          
        }
      }
      #Set the plot fill colors
      if (is.null(input$cgColorFill)) {
        values$fill <- input$fill
      } else {
        if (input$cgColorFill == "No color grouping") {
          values$fill <- input$fill
        } else {
          items <-
            as.vector(values$wwda2[, c(as.character(input$cgColorFill))])
          dict_names <- unique(items)
          
          if (input$facetChoice == TRUE) {
            if (input$cgColorFill == "Condition") {
              values$fill <-
                rep(2:(length(unique(values$wwda2$Condition))+1), length(unique(values$wwda2$Experiment)))
            } else if (input$cgColorFill == "Experiment") {
              values$fill <-
                rep(2:(length(unique(values$wwda2$Condition))+1), each = length(unique(values$wwda2$Experiment)))
            } else {
              values$fill <- 2:(length(dict_names)+1)
            }
          } else {
            if (input$cgColorFill == "Condition") {
              values$fill <- 2:(length(unique(values$wwda2$Condition))+1)
            } else if (input$cgColorFill == "Experiment") {
              values$fill <- 2
            } else {
              values$fill <- 2:(length(dict_names)+1)
            }
          }
          
        }
      }
      
      print ("Plot colors assigned")
      
      #If specified by user, prepare paired box plot
      if (input$plotType == "Paired Box Plot") {
        # if ((xValP == "Percent.Foci.Coverage.of.Center") | (yValP == "Percent.Foci.Coverage.of.Center")){
        #
        #     pairing <- pairing[pairing$Cells.In.Clone.Center > 5,]
        #
        # }
        
        p1 <-
          ggpaired(
            pairing,
            cond1 = xValP,
            cond2 = yValP,
            id = xValID,
            line.color = input$pairedLineColor,
            line.size = input$pairedLineSize,
            palette = "jco",
            fill = values$fillv,
            color = values$colorv
            
          )
      
      
      if(input$stdev == TRUE){
        p1 <- p1 + stat_summary(fun = mean, fun.min = function(x) mean(x)-sd(x), fun.max = function(x) mean(x)+sd(x), colour = input$stColor, shape = input$stShape, size = input$stSize)
      }
          
        
        pairedvolo = TRUE
        #If specified by user, generate a facetted graph
        if ((desfacito > 1) & (input$facetChoice == TRUE)) {
          p1 <- p1  +  facet_grid(values$facetVal, scales = input$free_axes)
        }
        
        
      }
      
      #If specified by user, prepare histogram
      else if ((input$plotType == "Histogram") |
               (input$plotType == "Histogram w/ Density Plot")) {
        if (isTRUE(input$plotterpaired == "N/A")) {
          p1 <-
            ggplot(values$wwda2 ,
                   aes(
                     x = as.numeric(values$plotData),
                     color = Condition,
                     fill = Condition
                   ))
        } else {
          p1 <-
            ggplot(
              values$wwda2 ,
              aes(
                x = as.numeric(values$plotData),
                color = values$plotData2,
                fill = values$plotData2
              )
            )
          
        }
        
        #If specified by user, generate a facetted graph
        if ((desfacito > 1) & (input$facetChoice == TRUE)) {
          p1 <- p1  +  facet_grid(values$facetVal, scales = input$free_axes)
        }
        
        tryCatch({
          if (input$bins != 1) {
            p1 <- p1 + geom_histogram(alpha = 0.6)
            
          } else {
            p1 <- p1 + geom_histogram(binwidth = input$bins, alpha = 0.6)
          }
          
          
          
        },
        # Unable to make histogram
        error = function(error_message) {
          message("Unable to run r-squared")
          return(NA)
        })
        
        
        
        if (input$plotType == "Histogram w/ Density Plot") {
          if (isTRUE(input$plotterpaired == "N/A")) {
            p1 <-
              p1 + geom_density(
                alpha = 0.2,
                color = values$colorv,
                fill = values$fillv
              )
          } else {
            p1 <-
              p1 + geom_density(
                alpha = 0.2,
                color = values$plotData2,
                fill = values$fillv
              )
            
          }
        }
      }
      
      
      #If specified by user, prepare scatter plot
      else if ((input$plotType == "Scatter Plot") |
               (input$plotType == "Scatter Plot w/ Regression Line")) {
        dots <- input$dots + 2
        p1 <-
          ggplot(
            values$wwda2,
            aes(
              x = values$plotData,
              y = values$plotData2,
              color = values$wwda2$Condition,
              fill = values$wwda2$Condition,
              shape = values$wwda2$Condition
            )
          ) + geom_point(size = dots)
        pairedvolo <- FALSE
        if ((input$xValue == "") &
            (input$xDel == FALSE)) {
          xtitle <- xValP
        }
        if (input$y == "") {
          ytitle <- yValP
        }
        if (input$plotType == "Scatter Plot w/ Regression Line") {
          p1 <- p1 + geom_smooth(method = lm)
          
        }
        
        #If specified by user, generate a facetted graph
        if ((desfacito > 1) & (input$facetChoice == TRUE)) {
          p1 <- p1  +  facet_grid(values$facetVal, scales = input$free_axes)
        }
        
        
        #If specified by the user, prepare the line plots for z and t axis analysis
      } else if (input$plotType == "Line Plot") {
        if ((input$xValue == "") & (input$xDel == FALSE)) {
          xtitle <- xValP
        }
        if (input$y == "") {
          ytitle <- yValP
        }
        
        if (input$sepTimePoints == "Z-axis analysis") {
          test_data <-
            values$wwda2[, c("Image.Name",
                             xValP,
                             "ConditionAndExperiment",
                             input$plotterpaired)]
          test_data$Image.Name <-
            sub(".tp.*", '', test_data$Image.Name)
          names(test_data)[names(test_data) == input$plotterpaired] <-
            "y.axis.variable"
          p1 <-
            ggplot(
              test_data,
              aes(
                x = .data[[xValP]],
                y = y.axis.variable,
                color = ConditionAndExperiment,
                group = Image.Name
              )
            ) + geom_line()
        } else if (input$sepTimePoints == "Time-lapse analysis") {
          test_data <-
            values$wwda2[, c("Image.Name",
                             xValP,
                             "ConditionAndExperiment",
                             input$plotterpaired)]
          test_data$Image.Name <-
            sub(".tp.*", '', test_data$Image.Name)
          names(test_data)[names(test_data) == input$plotterpaired] <-
            "y.axis.variable"
          p1 <-
            ggplot(
              test_data,
              aes(
                x = .data[[xValP]],
                y = y.axis.variable,
                color = ConditionAndExperiment,
                group = Image.Name
              )
            ) + geom_line()
        }
        
        pairedvolo <- FALSE
        
        
      }
      
      
      #Otherwise, we prepare the box-plot with dot plot or violin plot with dot plot
      else {
        pairedvolo <- FALSE
        p1 <-
          ggplot(values$wwda2 ,
                 aes(x = values$wwda2$Condition  , y = values$plotData))
        
        #Prepare box plot with dot plot
        if (input$plotType == "Dot Plot w/ Box Plot") {
          p1 <-
            p1 + geom_boxplot(
              width = 0.4,
              color = values$color,
              fill = values$fill
            ) #unique(values$fill))
        }
        
        #Prepare violin plot
        else {
          p1 <- p1 + geom_violin(color = values$colorv, fill = values$fillv)
        }
        

        #Add dotplot to box and violin plots
        if (dots > 0) {
          
          if (input$jitterChoice == TRUE){
          p1 <-
            p1  + geom_dotplot(
              binaxis = 'y' ,
              dotsize = dots ,
              stackdir = 'center' ,
              stackratio = 1,
              color = values$dotColor,
              fill = values$dotFill,
              position = position_jitter(width=input$jitter, height = NULL)
              
            )
          } else {
            p1 <-
              p1  + geom_dotplot(
                binaxis = 'y' ,
                dotsize = dots ,
                stackdir = 'center' ,
                stackratio = 1,
                color = values$dotColor,
                fill = values$dotFill
                
              )
          }
        }
        
        if(input$stdev == TRUE){
          p1 <- p1 + stat_summary(fun = mean, fun.min = function(x) mean(x)-sd(x), fun.max = function(x) mean(x)+sd(x), colour = input$stColor, shape = input$stShape, size = input$stSize)
        }
        

        
        #If specified by user, generate a facetted graph
        if ((desfacito > 1) & (input$facetChoice == TRUE)) {
          p1 <- p1  +  facet_grid(values$facetVal, scales = input$free_axes)
        }
        
      }
      
      if ((input$plotType != "Scatter Plot") &
          (input$plotType != "Scatter Plot w/ Regression Line") &
          (input$plotType != "Line Plot")) {
        p1 <-  p1 + scale_x_discrete()
      }
      
      
      
      #add theme
      p1 <-
        p1 + ggtitle(toString(values$plotOutTitle)) + labs(x = xtitle, y = ytitle) + theme(
          plot.title = element_text(
            face = "bold",
            size = values$titleSize,
            
            hjust = 0
          ),
          panel.background = element_rect(
            fill = values$backgroundColor,
            colour = values$backgroundFill,
            size = 0.5,
            linetype = "solid"
          ),
          axis.text.x = element_text(
            angle = 45 ,
            hjust = 1 ,
            face = "bold" ,
            size = values$axisFont
          ),
          axis.text.y = element_text(
            angle = 45 ,
            hjust = 1 ,
            face = "bold" ,
            size = values$axisFont
          ),
          axis.title = element_text(size = values$axisFont, face = "bold"),
          legend.position = "none",
          panel.grid.major = element_line(
            size = input$majorGridSize,
            linetype = 'solid',
            colour = input$majorGridColor
          ),
          panel.grid.minor = element_line(
            size = input$minorGridsize,
            linetype = 'solid',
            colour = input$minorGridColor
          )
          
        )
      
      tryCatch(
        # Try to run compare means.
        {
          if ((input$plotType != "Scatter Plot") &
              (input$plotType != "Scatter Plot w/ Regression Line") &
              (input$plotType != "Line Plot")) {
            if (pairedvolo == FALSE) {
              plotformula <- as.formula(plotformula)
              if (input$refgroup == "All/Basemean") {
                values$statistics <-
                  compare_means(
                    plotformula,
                    data = values$wwda2,
                    ref.group = ".all.",
                    method = values$method,
                    p.adjust.method = input$pAdj,
                    group.by = "Experiment"
                  )
                
              } else if (input$refgroup == "None") {
                values$statistics <-
                  compare_means(
                    plotformula,
                    data = values$wwda2,
                    method = values$method,
                    p.adjust.method = input$pAdj,
                    group.by = "Experiment"
                  )
              } else if (input$refgroup == "N/A") {
                values$statistics <-
                  compare_means(
                    xValues ~ Identification,
                    data = values$pairDataStat,
                    method = values$method,
                    group.by = "ConditionAndExperiment",
                    paired = TRUE
                  )
              } else {
                values$statistics <-
                  compare_means(
                    plotformula,
                    data = values$wwda2,
                    method = values$method,
                    ref.group = input$refgroup,
                    p.adjust.method = input$pAdj,
                    group.by = "Experiment"
                  )
              }
              
              
              
              output$statsTable <-
                renderTable(
                  values$statistics,
                  hover = TRUE,
                  border = TRUE,
                  spacing = "s",
                  digits = 5,
                  align = "l"
                )
              output$statsHeader <-
                renderText("Statistical test:")
            } else {
              plotformula2 <- as.formula(values$plotformula2)
              
              if (input$refgroup != "None") {
                values$statistics <-
                  compare_means(
                    xValues ~ Identification,
                    data = values$pairDataStat,
                    method = values$method,
                    p.adjust.method = input$pAdj,
                    group.by = "ConditionAndExperiment",
                    paired = TRUE
                  )
              } else {
                values$statistics <-
                  compare_means(
                    xValues ~ Identification,
                    data = values$pairDataStat,
                    method = values$method,
                    p.adjust.method = input$pAdj,
                    group.by = "ConditionAndExperiment",
                    paired = TRUE
                  )
              }
              output$statsHeader <-
                renderText("Pair-wise statistical test:")
              output$statsTable <-
                renderTable(
                  values$statistics,
                  hover = TRUE,
                  border = TRUE,
                  spacing = "s",
                  digits = 5,
                  align = "l"
                )
            }
          }
          
        },
        # Error message for failing to run compare means
        error = function(error_message) {
          message("Could not run compare means")
          return(NA)
        }
      )
      
      output$effectSizeTable <-
        renderTable(
          data.frame(),
          hover = TRUE,
          border = TRUE,
          spacing = "s",
          digits = 5,
          align = "l"
        )
      output$effectSizeHeader <- renderText("")
      
      tryCatch(
        # Try statement for effect size
        {
          estimates <- c()
          confHigh <- c()
          confLow <- c()
          magnitudes <- c()
          effectConditions <- c()
          effectConditions2 <- c()
          plotformula <- as.formula(plotformula)
          if (input$effectSize == "Cohen's D - Pooled") {
            hedges <- FALSE
            pooler <- TRUE
            mode <- 1
          } else if (input$effectSize == "Cohen's D - Not Pooled") {
            hedges <- FALSE
            pooler <- FALSE
            mode <- 1
          } else if (input$effectSize == "Hedges' G - Pooled") {
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
          
          #Get the names and identifiers of all our datasets and datapoints
          if (input$plotType == "Paired Box Plot") {
            effectData <-
              values$wwda2[, c(xValP,
                               yValP,
                               "Condition",
                               "Experiment",
                               "ConditionAndExperiment")]
            effectData <- na.omit(effectData)
            
          } else {
            effectData <-
              values$wwda2[, c(plottitle,
                               "Condition",
                               "Experiment",
                               "ConditionAndExperiment")]
            effectData <- na.omit(effectData)
            
          }
          Experiments <- unique(values$wwda2$Experiment)
          if (input$plotType == "Paired Box Plot") {
            Experiments <- unique(values$wwda2$ConditionAndExperiment)
          }
          
          x <- 0
          lengthotron <- length(Experiments)
          
          
          # Loop through each experiment
          while (x < lengthotron) {
            Experimentname <- Experiments[x + 1]
            if (input$plotType == "Paired Box Plot") {
              ExperimentData <-
                effectData %>% filter(ConditionAndExperiment == Experimentname)
              genes <-
                unique(ExperimentData$ConditionAndExperiment)
              pairing = TRUE
              
              
            } else {
              ExperimentData <-
                effectData %>% filter(Experiment == Experimentname)
              genes <- unique(ExperimentData$Condition)
              pairing = FALSE
              
              if (input$refgroup == "All/Basemean") {
                baseData <- ExperimentData
                baseData$Condition <- "Basemean"
              }
            }
            
            # Loop through each experimental condition
            priorGenes <- c()
            
            
            count <- 0
            for (genename in genes) {
              if (input$plotType == "Paired Box Plot") {
                ExperimentData <-
                  effectData %>% filter(ConditionAndExperiment == genename)
                
                data1 <- cbind(1, ExperimentData[[xValP]])
                data2 <- cbind(2, ExperimentData[[yValP]])
                
                expData <- rbind (data1, data2)
                expData <- data.frame(expData)
                colnames(expData) <- c("x1", "x2")
                
                
                if (input$effectSize != 'N/A') {
                  if (mode == 1) {
                    effectSize <-
                      cohen.d(
                        x2 ~ x1,
                        data = expData,
                        pooled = pooler,
                        hedges.correction = hedges,
                        paired = TRUE,
                        conf.level = 0.95,
                        na.rm = TRUE
                      )
                  } else {
                    effectSize <-
                      cliff.delta(
                        x2 ~ x1,
                        data = expData,
                        conf.level = 0.95,
                        return.dm = FALSE,
                        use.unbiased = TRUE,
                        use.normal = FALSE,
                        na.rm = TRUE
                      )
                  }
                }
                
                estimates <- c(estimates, effectSize$estimate)
                mag <- as.numeric(effectSize$magnitude)
                
                if (mag == 1) {
                  mag = 'Negligible'
                }
                if (mag == 2) {
                  mag = 'Small'
                }
                if (mag == 3) {
                  mag = 'Medium'
                }
                if (mag == 4) {
                  mag = 'Large'
                }
                magnitudes <- c(magnitudes, mag)
                
                effectConditions <-
                  rbind(effectConditions, as.character(genename))
                effectConditions2 <-
                  rbind(effectConditions2, as.character(genename))
                conf <- effectSize$conf.int
                
                
              
                
                confLow <- c(confLow, effectSize$conf.int[1])
                confHigh <- c(confHigh, effectSize$conf.int[2])
                
              } else {
                if (genename != input$refgroup) {
                  if (input$refgroup != "None") {
                    if (input$refgroup != "All/Basemean") {
                      next
                    }
                  }
                  
                }
              }
              if (input$plotType != "Paired Box Plot") {
                Basemean <- FALSE
                
                for (genename2 in setdiff(genes, priorGenes)) {
                  if (input$refgroup == "All/Basemean") {
                    if (Basemean == TRUE) {
                      next
                    } else {
                      genename2 <- "lollapalooza"
                      Basemean <- TRUE
                      
                    }
                  }
                  
                  count <- count + 1
                  
                  if (genename != genename2) {
                    if (input$refgroup == "All/Basemean") {
                      newData <- ExperimentData[(ExperimentData$Condition == genename) ,]
                      
                      #Filter out dataset so it corresponds just to current experiments
                      
                      expData <-
                        newData %>% filter((Condition == genename))
                      
                      expData <- rbind(expData, baseData)
                      
                      
                      
                    } else {
                      newData <-
                        ExperimentData[(ExperimentData$Condition == genename) |
                                         (ExperimentData$Condition == genename2),]
                      
                      #Filter out dataset so it corresponds just to current experiments
                      
                      expData <-
                        newData %>% filter((Condition == genename) |
                                             (Condition == genename2))
                    }
                    
                    
                    
                    
                    
                    #Here, we calculate the effect sizes based on user input
                    
                    if (input$effectSize != 'N/A') {
                      if (input$refgroup == "All/Basemean") {
                        if (mode == 1) {
                          effectSize <-
                            cohen.d (
                              plotformula,
                              data = expData,
                              pooled = pooler,
                              hedges.correction = hedges,
                              paired = pairing,
                              conf.level = 0.95,
                              na.rm = TRUE
                            )
                        } else {
                          effectSize <-
                            cliff.delta(
                              plotformula,
                              data = expData,
                              conf.level = 0.95,
                              return.dm = FALSE,
                              use.unbiased = TRUE,
                              use.normal = FALSE,
                              na.rm = TRUE
                            )
                        }
                        effectConditions <-
                          rbind(effectConditions,
                                as.character(genename))
                        effectConditions2 <-
                          rbind(effectConditions2, "Basemean")
                      } else {
                        if (mode == 1) {
                          effectSize <-
                            cohen.d (
                              plotformula,
                              data = expData,
                              pooled = pooler,
                              hedges.correction = hedges,
                              paired = pairing,
                              conf.level = 0.95,
                              na.rm = TRUE
                            )
                        } else {
                          effectSize <-
                            cliff.delta(
                              plotformula,
                              data = expData,
                              conf.level = 0.95,
                              return.dm = FALSE,
                              use.unbiased = TRUE,
                              use.normal = FALSE,
                              na.rm = TRUE
                            )
                        }
                        effectConditions <-
                          rbind(effectConditions,
                                as.character(genename))
                        effectConditions2 <-
                          rbind(effectConditions2,
                                as.character(genename2))
                      }
                    }

                    
                    estimates <- c(estimates, effectSize$estimate)
                    mag <- as.numeric(effectSize$magnitude)
                    
                    if (mag == 1) {
                      mag = 'Negligible'
                    }
                    if (mag == 2) {
                      mag = 'Small'
                    }
                    if (mag == 3) {
                      mag = 'Medium'
                    }
                    if (mag == 4) {
                      mag = 'Large'
                    }
                    magnitudes <- c(magnitudes, mag)
                    
                    
                    conf <- effectSize$conf.int
                    confLow <- c(confLow, effectSize$conf.int[1])
                    confHigh <-
                      c(confLow, effectSize$conf.int[2])
                    confHigh <- confHigh[2]
                    
                  }
                  
                }
                
                
                
                priorGenes <- c(priorGenes, genename)
              }
              
            }
            x <- x + 1
          }
          
          
          
          effectSizeRes = data.frame(
            Group1 = effectConditions,
            Group2 = effectConditions2,
            method = input$effectSize,
            estimate = estimates,
            magnitude = magnitudes,
            conf.interval.low = confLow,
            conf.interval.high = confHigh
          )
          
          
          
          output$effectSizeTable <-
            renderTable(
              effectSizeRes,
              hover = TRUE,
              border = TRUE,
              spacing = "s",
              digits = 5,
              align = "l"
            )
          output$effectSizeHeader <-
            renderText(paste("Effect Size Estimates:", input$effectSize, sep = " "))
          
          values$effectSizeRes <- effectSizeRes
          
        },
        # Error message for when unable to run effect size
        error = function(error_message) {
          message("Unable to run effect size tests")
          return(NA)
        }
      )
      
      if (input$pDisplay == TRUE) {
        if ((input$plotType != "Histogram") &
            (input$plotType != "Histogram w/ Density Plot") &
            (input$plotType != "Scatter Plot") &
            (input$plotType != "Scatter Plot w/ Regression Line") &
            (input$plotType != "Line Plot")) {
          tryCatch({
            refgroup <- input$refgroup
            
            
            
            #ToBeAddedAsButtons
            step_size <- input$step_size
            y_offset <- input$y_offset
            ascending <- input$ascending
            tipL <- input$tipL
            mapSig <- FALSE
            vjustsig <- input$vjustSig
            barThickness <- input$barThickness
            textSizeSig <- input$textSizeSig
            sigColor <- input$sigColor
            adjustedP <- input$adjustedP
            
            
            
            effsize <- values$effectSizeRes
            anno_df <- values$statistics
            
            
            
            if (input$effectSize == "Cohen's D - Pooled") {
              effsymbol <- "d"
            } else if (input$effectSize == "Cohen's D - Not Pooled") {
              effsymbol <- "d"
            } else if (input$effectSize == "Hedges' G - Pooled") {
              effsymbol <- "g"
            } else if (input$effectSize == "Hedges' G - Not Pooled") {
              effsymbol <- "g"
            } else {
              effsymbol <- "delta"
            }
            
            
            fullLabel <-
              paste("p=", round(as.numeric(anno_df$p), 4), sep = "")
            
            
            
            
            if ((input$adjustedP == TRUE)) {
              fullLabel <-
                paste("p.adj=", round(as.numeric (anno_df$p.adj), 4), sep = "")
              
            }
            
            
            if (input$effSizeDisplay == TRUE) {
              fullLabel <-
                paste(fullLabel,
                      ", ",
                      effsymbol,
                      "=",
                      round(effsize$estimate, 4),
                      sep = "")
            }
            
            
              
            
            
          
            
            anno_df$fullLabel <- fullLabel
            
            
            
            
            if (input$plotType == "Paired Box Plot") {
              p1 <-
                p1 + stat_compare_means(paired = pairedvolo, method = values$method)
              
            } else {
              
              #This is where we workout how to position all of the pairwise comparisons on the graphs
              
              if (input$refgroup != "All/Basemean") {
                gb <- ggplot_build(p1)
                ymax = gb$layout$panel_params[[1]]$y.range[2]
                y_margin <- ymax * step_size
                
                y_pos <- c()
                
                val <- ymax - y_offset
                
                for (item in comparisonVectors) {
                  y_pos <- c(y_pos, val)
                  
                  
                  val <- val - y_margin
                  
                }
                
                if (ascending == TRUE) {
                  y_pos <- sort(y_pos, decreasing = FALSE)
                }
                
                print (1)
                print (fullLabel)
                print(textSizeSig)
                print (y_pos)
                
                #With facetted graphs, we need to duplicate the y_positions
                
                if ((length(y_pos) != 1) & (length(y_pos)<length(fullLabel)) & (length(unique(values$wwda2$Condition)) > 1  )){
                  
                  countLim <- ceiling( length (fullLabel) / length (y_pos))
        
                  counter <- 1
                  
                  y_posNew <- y_pos
                  y_posArch <- y_pos
                  
                  while (counter < countLim) {
                    y_posNew <- c(y_posNew, y_pos)
                    counter <- counter + 1
                  }
                  
                  y_pos <- y_posNew
                  
                  
                  
                  #If the user has given different sets of conditions per experiment, we need to omit excess positions
                  
                  print ("Excluding excess comparisons")
                  skipVector = c()
                  for (ExperimentL in (unique(values$wwda2$Experiment))) {
                    
                  
                    #Set up the comparisons variable for stat_compare_means
                    comparisonVectorsNew <- list()
                    
                   
                
                    #This loop goes through and determines which comparisons to make for statistical tests and outputs on graphs
                    newValues <- values$wwda2[values$wwda2$Experiment == ExperimentL, ]
                  
                    reffy <- input$refgroup
                    
                    
                    comparisonsToBe <- unique(newValues$Condition)
                    if (isFALSE (reffy %in% comparisonsToBe)){
                      y_pos <- c(y_pos, y_posArch)
                    }
                    
                    
                    comparisonsToBe2 <- comparisonsToBe[-1]
                    
                    
                    
                  
                    
                    
                  
                    if (isTRUE(input$refgroup != "All/Basemean")) {
                      countington <- 1
                      for (item in comparisonsToBe) {
                        if (isTRUE(input$refgroup == "None")) {
                          for (item2 in comparisonsToBe2) {
                            comparisonVectorsNew[[countington]] <- c(item, item2)
                            countington <- countington + 1
                          }
                          comparisonsToBe2 <- comparisonsToBe2[-1]
                          
                        } else {
                        
                          if ( (isTRUE(item != input$refgroup)) & (isTRUE(reffy %in% comparisonsToBe))) {
                            
                            comparisonVectorsNew[[countington]] <- c(input$refgroup, item)
                            countington <- countington + 1
                          } 
                            
                          
                          
                        }
                      }
                    }
                  
                    for (item in comparisonVectors){
                      
                    
                      if (list(item) %in% comparisonVectorsNew){
                        skipVector <- c(skipVector, 1)
                      } else {
                        print (c("item", item))
                        skipVector <- c(skipVector, NA)
                      }
                    }
                    
                  }
                  
                  print (skipVector)
                  print (y_pos)
                  
                  skipFrame <- data.frame(
                    col1 = y_pos,
                    col2 = skipVector
                  )
                  
                  print ("Excess comparisons omitted")
                  
                  skipFrame <- na.omit(skipFrame)
                  y_pos <- skipFrame$col1
                  
                  print (y_pos)
                }
                
                if (nrow(data.frame(anno_df)) >= 1) {
                  if (adjustedP == TRUE) {
                    p1 <- p1 +  ggsignif::geom_signif(
                      data = anno_df,
                      aes(
                        xmin = group1,
                        xmax = group2,
                        annotations = fullLabel,
                        y_position = y_pos
                      ),
                      map_signif_level = mapSig,
                      manual = TRUE,
                      tip_length = tipL ,
                      vjust = vjustsig,
                      size = barThickness,
                      textSize = textSizeSig,
                      color = sigColor
                      
                    )
                  } else{
                    p1 <- p1 +  ggsignif::geom_signif(
                      data = anno_df,
                      aes(
                        xmin = group1,
                        xmax = group2,
                        annotations = fullLabel,
                        y_position = y_pos
                      ),
                      map_signif_level = mapSig,
                      manual = TRUE,
                      tip_length = tipL ,
                      vjust = vjustsig,
                      size = barThickness,
                      textSize = textSizeSig,
                      color = sigColor
                      
                    )
                    
                  }
                }
              } else {
                gb <- ggplot_build(p1)
                ymax = gb$layout$panel_params[[1]]$y.range[2]
                
                val <- ymax - y_offset
                
                if ((nrow(data.frame(anno_df)) >= 1)) {
                  if (adjustedP == TRUE) {
                    p1 <-
                      p1 + geom_text(
                        data = anno_df,
                        aes(
                          y = val,
                          x = 1:nrow(anno_df)
                        ),
                        label = anno_df$fullLabel,
                        textSize = textSizeSig,
                        color = sigColor
                      )
                  }
                  else {
                    p1 <-
                      p1 + geom_text(
                        data = anno_df,
                        aes(
                          y = val,
                          x = 1:nrow(anno_df)
                        ),
                        label = anno_df$fullLabel,
                        textSize = textSizeSig,
                        color = sigColor
                      )
                  }
                }
              }
            }
            
            
            
          },
          # ... but if an error occurs, tell me what happened:
          error = function(error_message) {
            message("Unable to run plot stats")
            return(NA)
          })
        }
        
      }
      
      if (input$xDel == TRUE) {
        p1 <- p1 + theme(axis.text.x = element_blank())
      }
      
      
      if (input$limitx == TRUE) {
        p1 <- p1 + coord_cartesian(xlim = c(input$xMin, input$xMax))
      }
      
      if (input$limity == TRUE) {
        p1 <- p1 + coord_cartesian(ylim = c(input$yMin, input$yMax))
      }
      if (input$legend == TRUE) {
        p1 <-
          p1 + theme(legend.position = input$legendPosition) + labs(
            color = input$legendTitle,
            fill = input$legendTitle2,
            shape = input$legendTitle3
          )
        
      }
      
      
      
      
      #p1 <- p1 +stat_compare_means(method = values$method)
      
      
      
      output$plotso <- renderUI({
        output$plot_test <- renderPlot({
          p1
          
          
        })
        
        plotOutput("plot_test")
      })
      
      
      values$plotso <- p1
      
      
    },
    # ... but if an error occurs, tell me what happened:
    error = function(error_message) {
      message("Could not generate plot")
      return(NA)
    })
    
    
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
        if (input$runFishers == TRUE) {
          #Get the death table
          deathTable <- values$deathTable
          
          #Create a loop to run one fisher test per condition
          fishResult <- c()
          fishConfidence <- c()
          fishConfidenceUB <- c()
          fishOddsRatio <- c()
          x <- 0
          lengthotron <- length(deathTable$Genotype)
          while (x < lengthotron) {
            #Get death table and format it into a 2x2 contingency table
            gen <- deathTable[x + 1, 1]
            fishers <-
              matrix(c(deathTable[x + 1, 2], deathTable[x + 1, 3], deathTable[x + 1, 4], deathTable[x +
                                                                                                      1, 5]),
                     ncol = 2)
            colnames(fishers) <- c("Foci-", "Foci+")
            rownames(fishers) <- c("Border", "Center")
            fishers <- as.table(fishers)
            
            #Run fisher test and store results
            fishy <- fisher.test(fishers)
            
            
            cfLB <- as.character(c(fishy$conf.int[1]))
            cfUB <- as.character(c(fishy$conf.int[2]))
            odds <- as.character(c(fishy$estimate[1]))
            fishConfidence <- c(fishConfidence, cfLB)
            fishConfidenceUB <- c(fishConfidenceUB, cfUB)
            fishOddsRatio <- c(fishOddsRatio, odds)
            
            fishResult <-
              rbind(fishResult, as.character(c(fishy$p.value)))
            
            
            x <- x + 1
          }
          
          pAdj <-
            p.adjust(fishResult,
                     method = input$pAdj,
                     n = length(fishResult))
          colnames(fishResult) <- c("p-value")
          fishResultFinal = data.frame(
            Genotype = deathTable$Genotype,
            fishResult,
            conf.interval.from = fishConfidence,
            conf.interval.to = fishConfidenceUB,
            p.adjusted = as.character(pAdj),
            p.adjustment.method = input$pAdj,
            odds.ratio = fishOddsRatio
          )
          output$fishBC <-
            renderTable(
              fishResultFinal,
              hover = TRUE,
              border = TRUE,
              spacing = "s",
              digits = 5,
              align = "l"
            )
          output$fishBCHeader <-
            renderText("Fisher's Exact Test for Foci in the border vs. Foci in the center")
        }
      },
      # Error for unable to run fisher test
      error = function(error_message) {
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
    if ((input$plotType == "Scatter Plot") |
        (input$plotType == "Scatter Plot w/ Regression Line")) {
      if (input$statsType != "Hotellings T-Squared Test") {
        tryCatch(
          # Attempt to run correlations tests
          {
            #Get the names and identifiers of all our datasets and datapoints
            ConditionAndExperiments <-
              unique(values$wwda2$ConditionAndExperiment)
            
            #Initialize variables for keeping results and looping through datasets
            corrResult <- c()
            corrConditions <- c()
            corrConditions2 <- c()
            corrConditionAndExperiment <- c()
            pVals <- c()
            x <- 0
            lengthotron <- length(ConditionAndExperiments)
            
            
            # Loop through each experiment
            while (x < lengthotron) {
              #Get the name of the x and y variables, save to the Experimentname variables
              Experimentname <- ConditionAndExperiments[x + 1]
              Experimentname2 <- ConditionAndExperiments[x + 2]
              
              #Get just those columns from the dataframe
              ExperimentData <-
                values$wwda2 %>% filter(ConditionAndExperiment == Experimentname)
              
              #Check how many datapoints there are
              count <- nrow(ExperimentData)
              genes <- c(xValP, yValP)
              
              
              # Loop through each experimental condition
              
              priorGenes <- c()
              for (genename in genes) {
                for (genename2 in setdiff(genes, priorGenes)) {
                  if (genename != genename2) {
                    #Run the correlation test
                    corry <-
                      cor.test(
                        ExperimentData[, xValP],
                        y = ExperimentData[, yValP],
                        use = "pairwise.complete.obs",
                        method = values$corrMethod
                      )
                    if (values$corrMethod == "pearson") {
                      #Run the Z-score test for pearsons, if appropriate
                      zScore <- fisherz(corry$estimate)
                      corrResult <-
                        rbind(corrResult, as.character(
                          c(
                            corry$p.value,
                            corry$estimate,
                            values$corrMethod,
                            count,
                            zScore
                          )
                        ))
                    } else {
                      corrResult <-
                        rbind(corrResult, as.character(
                          c(
                            corry$p.value,
                            corry$estimate,
                            values$corrMethod
                          )
                        ))
                    }
                    
                    #Add the p value to a vector. We use this for the p adjustment
                    pVals <- rbind(pVals, corry$p.value)
                    
                    
                    #Store results to a dataframe
                    corrConditions <-
                      rbind(corrConditions, as.character(genename))
                    corrConditions2 <-
                      rbind(corrConditions2, as.character(genename2))
                    corrConditionAndExperiment <-
                      rbind(corrConditionAndExperiment,
                            as.character(Experimentname))
                    
                  }
                }
                priorGenes <- c(priorGenes, genename)
              }
              x <- x + 1
            }
            
            #Output our results
            if (values$corrMethod == "pearson") {
              colnames(corrResult) <-
                c("p-value",
                  "Correlation.Coefficient",
                  "Test",
                  "n",
                  "Z.Score")
            } else {
              colnames(corrResult) <-
                c("p-value", "Correlation.Coefficient", "Test")
            }
            
            #Run our p adjustment and add it to the output table
            pAdj <-
              p.adjust(pVals, method = input$pAdj, n = length(pVals))
            corrResult2 = data.frame(
              Group = corrConditionAndExperiment,
              X = corrConditions,
              Y = corrConditions2,
              corrResult,
              p.adjusted = as.numeric(pAdj),
              p.adjustment.method = input$pAdj
            )
            output$statsTable <-
              renderTable(
                corrResult2,
                hover = TRUE,
                border = TRUE,
                spacing = "s",
                digits = 5,
                align = "l"
              )
            
            pVals <- c()
            G1 <- c()
            G2 <- c()
            zVals <- c()
            priorGenes <- c()
            if (values$corrMethod == "pearson") {
              for (i in setdiff(corrConditionAndExperiment, priorGenes)) {
                newData <- corrResult2 %>% filter(Group == i)
                #output$contents <-renderTable(newData,  hover=TRUE, border=TRUE, spacing="s", digits=5, align ="l", width="auto")
                for (j in corrConditionAndExperiment) {
                  newData2 <- corrResult2 %>% filter(Group == j)
                  
                  if (i != j) {
                    z1 <- as.numeric(as.character(newData[1, "Correlation.Coefficient"]))
                    z2 <-
                      as.numeric(as.character(newData2[1, "Correlation.Coefficient"]))
                    n1 <- as.numeric(as.character(newData[1, "n"]))
                    n2 <- as.numeric(as.character(newData2[1, "n"]))
                    pairo <-
                      paired.r(z1, z2, NULL, n1, n2, twotailed = TRUE)
                    pVals <- rbind(pVals, pairo$p)
                    zVals <- rbind(zVals, pairo$z)
                    G1 <- rbind(G1, i)
                    G2 <- rbind(G2, j)
                    priorGenes <- c(priorGenes, i)
                    
                  }
                }
              }
              pAdj <-
                p.adjust(pVals,
                         method = input$pAdj,
                         n = length(pVals))
              corrValus <-
                data.frame(
                  Group1 = G1,
                  Group2 = G2,
                  Z.Value = zVals,
                  p.value = pVals,
                  p.adjusted = as.numeric(pAdj),
                  p.adjustment.method = input$pAdj
                )
              output$statsHeaderP <-
                renderText("Comparison of correlation between groups")
              output$statsTableP <-
                renderTable(
                  corrValus,
                  hover = TRUE,
                  border = TRUE,
                  spacing = "s",
                  digits = 5,
                  align = "l"
                )
            }
          },
          # Error fo if unable to run correlation
          error = function(error_message) {
            message("Unable to run correlationTest")
            return(NA)
          }
        )
        
      } else{
        tryCatch(
          # Run Hotellings t2
          {
            #Get the names and identifiers of all our datasets and datapoints
            ConditionAndExperiments <-
              unique(values$wwda2$ConditionAndExperiment)
            
            #Initialize variables for keeping results and looping through datasets
            corrResult <- c()
            corrConditions <- c()
            corrConditions2 <- c()
            corrConditionAndExperiment <- c()
            corrConditionAndExperiment2 <- c()
            pVals <- c()
            priorGenes <- c()
            newData <-
              cbind(values$wwda2[xValP], values$wwda2[yValP])
            newData <-
              cbind(values$wwda2["ConditionAndExperiment"], newData)
            
            
            # Loop through each experiment
            for (Experimentname in ConditionAndExperiments) {
              priorGenes <- cbind(priorGenes, Experimentname)
              for (Experimentname2 in setdiff(ConditionAndExperiments, priorGenes)) {
                if (Experimentname != Experimentname2) {
                  newData2 <-
                    newData %>% filter((ConditionAndExperiment == Experimentname) |
                                         (ConditionAndExperiment == Experimentname2)
                    )
                  output$contents <-
                    renderTable(
                      newData2,
                      hover = TRUE,
                      border = TRUE,
                      spacing = "s",
                      digits = 5,
                      align = "l",
                      width = "auto"
                    )
                  
                  newData.vars <-
                    newData2 %>% dplyr::select(-one_of("ConditionAndExperiment"))
                  
                  multiShap <-
                    mvnormtest::mshapiro.test(t(newData.vars))
                  varCoVar <- det(cov(newData.vars))
                  
                  if ((as.numeric(multiShap$p.value) > 0.05) &
                      (as.numeric(varCoVar) > 0)) {
                    assumptions <- "Assumptions met"
                  } else {
                    assumptions <- "Assumptions violated"
                  }
                  
                  
                  
                  hotellings <-
                    HotellingsT2(
                      filter(
                        newData2,
                        ConditionAndExperiment == Experimentname
                      )[, 2:3],
                      filter(
                        newData2,
                        ConditionAndExperiment == Experimentname2
                      )[, 2:3]
                    )
                  
                  corrResult <-
                    rbind(corrResult, as.character(
                      c(
                        multiShap$p.value,
                        varCoVar,
                        assumptions,
                        hotellings$p.value,
                        "Hotellings T-squared Test"
                      )
                    ))
                  corrConditions <-
                    rbind(corrConditions, as.character(xValP))
                  corrConditions2 <-
                    rbind(corrConditions2, as.character(yValP))
                  corrConditionAndExperiment <-
                    rbind(corrConditionAndExperiment,
                          as.character(Experimentname))
                  corrConditionAndExperiment2 <-
                    rbind(corrConditionAndExperiment2,
                          as.character(Experimentname2))
                  pVals <- cbind(pVals, hotellings$p.value)
                }
              }
            }
            
            #Output our results
            colnames(corrResult) <-
              c(
                "Multivariate shapiro p-value",
                "Determinant of Variance-Covariance Matrix",
                "Have test assumptions been met?",
                "Means comparison p-value",
                "Test"
              )#                        #Run p adjustment and add to table
            pAdj <-
              p.adjust(pVals, method = input$pAdj, n = length(pVals))
            corrResult2 = data.frame(
              Group1 = corrConditionAndExperiment,
              Group2 = corrConditionAndExperiment2,
              X = corrConditions,
              Y = corrConditions2,
              corrResult,
              p.adjusted = as.numeric(pAdj),
              p.adjustment.method = input$pAdj
            )
            
            output$statsTable <-
              renderTable(
                corrResult2,
                hover = TRUE,
                border = TRUE,
                spacing = "s",
                digits = 5,
                align = "l"
              )
            
          },
          # Error for if unable to run t2
          error = function(error_message) {
            message("Unable to run hotelling Test")
            return(NA)
          }
        )
      }
    }
    
    ###########################################################
    ##
    ##
    ## Equivalence of Variance test
    ##
    ##
    ###########################################################
    
    
    if ((input$plotType != "Scatter Plot") &
        (input$plotType != "Scatter Plot w/ Regression Line")) {
      tryCatch(
        # Try statement for equivalence of variance test
        {
          #Get the names and identifiers of all our datasets and datapoints
          flignerData <-
            values$wwda2[, c(plottitle,
                             "Condition",
                             "Experiment",
                             "ConditionAndExperiment")]
          flilgnerData <- na.omit(flignerData)
          Experiments <- unique(values$wwda2$Experiment)
          if (input$plotType == "Paired Box Plot") {
            Experiments <- unique(values$wwda2$ConditionAndExperiment)
          }
          
          #Initialize variables for keeping results and looping through datasets
          flignerResult <- c()
          fligConditions <- c()
          fligConditions2 <- c()
          x <- 0
          lengthotron <- length(Experiments)
          
          
          # Loop through each experiment
          while (x < lengthotron) {
            Experimentname <- Experiments[x + 1]
            if (input$plotType == "Paired Box Plot") {
              ExperimentData <-
                values$wwda2 %>% filter(ConditionAndExperiment == Experimentname)
              genes <- c(xValP, yValP)
            } else {
              ExperimentData <-
                flignerData %>% filter(Experiment == Experimentname)
              genes <- unique(ExperimentData$Condition)
            }
            
            # Loop through each experimental condition
            priorGenes <- c()
            for (genename in genes) {
              for (genename2 in setdiff(genes, priorGenes)) {
                if (genename != genename2) {
                  if (input$plotType == "Paired Box Plot") {
                    flignerVector1 <-
                      c(ExperimentData[, genename], ExperimentData[, genename2])
                    filler <- length(flignerVector1) / 2
                    flignerVector2 <-
                      c(rep(genename, filler),
                        rep(genename2, filler))
                    
                  } else {
                    geneData <-
                      ExperimentData %>% filter(Condition %in% c(genename, genename2))
                    flignerVector1 <- geneData[, plottitle]
                    flignerVector2 <-
                      as.factor(geneData[, "Condition"])
                  }
                  
                  fliggy <-
                    fligner.test(flignerVector1 ~ flignerVector2)
                  flignerResult <-
                    rbind(flignerResult, as.character(c(fliggy$p.value)))
                  fligConditions <-
                    rbind(fligConditions, as.character(genename))
                  fligConditions2 <-
                    rbind(fligConditions2, as.character(genename2))
                  
                }
                
              }
              
              
              priorGenes <- c(priorGenes, genename)
            }
            
            x <- x + 1
            
          }
          
          #Output our results
          colnames(flignerResult) <- c("p-value")
          fligResult2 = data.frame(Group1 = fligConditions, Group2 = fligConditions2, flignerResult)
          output$fligner <-
            renderTable(
              fligResult2,
              hover = TRUE,
              border = TRUE,
              spacing = "s",
              digits = 5,
              align = "l"
            )
          output$flignerHeader <- renderText(plottitle)
          
          
        },
        # Error message for when unable to run equivalence of variance test
        error = function(error_message) {
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
    if (input$statsType != "Hotellings T-Squared Test") {
      tryCatch(
        # Try statement for shapiro-wilks test
        {
          #Get the data from reactive values
          wwda2 <- values$wwda2
          
          #Run the shapiro test
          shapiroRes <-
            tapply(values$wwda2[, plottitle] ,
                   values$wwda2$ConditionAndExperiment ,
                   shapiro.test)
          shapiroP <- c()
          
          
          for (i in shapiroRes) {
            z <- 1
            for (a in i) {
              if (z == 2) {
                pval <- ifelse(nchar(a) > 6, paste0(strtrim(a, 6), '...'), a)
                shapiroP <- rbind(shapiroP, as.character(pval))
              }
              z <- z + 1
              
            }
          }
          
          ifelse(nchar(a) > 13, paste0(strtrim(a, 10), '...'), a)
          shapTable <- data.frame (
            ConditionAndExperiment = unique(values$wwda2$ConditionAndExperiment),
            p.value = shapiroP
          )
          output$normality <-
            renderTable(
              shapTable,
              hover = TRUE,
              border = TRUE,
              spacing = "s",
              digits = 5,
              align = "l"
            )
          output$normalityHeader <- renderText(plottitle)
          
          
          if ((input$plotType == "Paired Box Plot") |
              (input$plotType == "Scatter Plot") |
              (input$plotType == "Scatter Plot w/ Regression Line") |
              (input$plotType == "Line Plot")) {
            wwda2 <- values$wwda2
            
            shapiroRes <-
              tapply(values$wwda2[, input$plotterpaired] ,
                     values$wwda2$ConditionAndExperiment ,
                     shapiro.test)
            shapiroP <- c()
            
            for (i in shapiroRes) {
              z <- 1
              for (a in i) {
                if (z == 2) {
                  pval <- ifelse(nchar(a) > 6, paste0(strtrim(a, 6), '...'), a)
                  shapiroP <- rbind(shapiroP, as.character(pval))
                }
                z <- z + 1
                
              }
            }
            ifelse(nchar(a) > 13, paste0(strtrim(a, 10), '...'), a)
            shapTable <- data.frame (
              ConditionAndExperiment = unique(values$wwda2$ConditionAndExperiment),
              p.value = shapiroP
            )
            output$normality2 <-
              renderTable(
                shapTable,
                hover = TRUE,
                border = TRUE,
                spacing = "s",
                digits = 5,
                align = "l"
              )
            output$normalityHeader2 <-
              renderText(input$plotterpaired)
          }
        },
        # Error for unable to run shapiro wilks test
        error = function(error_message) {
          message("Unable to run Shapiro-Wilks test")
          return(NA)
        }
      )
    }
    
    
    
    
    
    
    
    
    
    
    
    
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
        if (input$runFishers == TRUE) {
          #Get the foci count data table
          deathTable <- values$deathTable
          
          #Initialize variables to hold our results
          fishResult <- c()
          fishConditions <- c()
          fishConditions2 <- c()
          fishConfidence <- c()
          fishConfidenceUB <- c()
          fishOddsRatio <- c()
          
          x <- 0
          
          #Loop through each row of the death table
          lengthotron <- length(deathTable$Genotype)
          while (x < lengthotron) {
            y <- x + 1
            while (y < lengthotron) {
              #Find genotype of the groups we are comparing
              gen <- deathTable[x + 1, 1]
              gen2 <- deathTable[y + 1, 1]
              
              #format these rows into a 2x2 contingency table and add headers
              fishers <-
                matrix(c(
                  deathTable[x + 1, 2],
                  deathTable[y + 1, 2],
                  deathTable[x + 1, 3],
                  deathTable[y + 1, 3]
                ),
                ncol = 2)
              colnames(fishers) <- c("Foci-", "Foci+")
              rownames(fishers) <- c("Condition1", "Condition2")
              fishers <- as.table(fishers)
              
              #Run fisher test
              fishy <- fisher.test(fishers)
              
              cfLB <- as.character(c(fishy$conf.int[1]))
              cfUB <- as.character(c(fishy$conf.int[2]))
              odds <- as.character(c(fishy$estimate[1]))
              fishConfidence <- c(fishConfidence, cfLB)
              fishConfidenceUB <- c(fishConfidenceUB, cfUB)
              fishOddsRatio <- c(fishOddsRatio, odds)
              
              #Store outputs
              fishResult <-
                rbind(fishResult, as.character(c(fishy$p.value)))
              fishConditions <-
                rbind(fishConditions, as.character(gen))
              fishConditions2 <-
                rbind(fishConditions2, as.character(gen2))
              
              y <- y + 1
              
            }
            
            x <- x + 1
          }
          
          
          
          #Run and add our p-adjustment
          pAdj <-
            p.adjust(fishResult,
                     method = input$pAdj,
                     n = length(fishResult))
          colnames(fishResult) <- c("p-value")
          fishResult2Z = data.frame(
            Group1 = fishConditions,
            Group2 = fishConditions2,
            fishResult,
            conf.interval.from = fishConfidence,
            conf.interval.to = fishConfidenceUB,
            p.adjusted = as.numeric(pAdj),
            p.adjustment.method = input$pAdj,
            odds.ratio = fishOddsRatio
          )
          output$fishTable <-
            renderTable(
              fishResult2Z,
              hover = TRUE,
              border = TRUE,
              spacing = "s",
              digits = 5,
              align = "l"
            )
          output$fishHeader <-
            renderText("Fisher's Exact Test Between Samples")
        }
        
      },
      # Error message for failing to run fisher test between groups
      error = function(error_message) {
        message("Could not run fisher test between groups")
        return(NA)
      }
    )
    
  })
  
  
}
