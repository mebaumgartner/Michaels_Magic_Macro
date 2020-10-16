list.of.packages <- c("shiny", "PerformanceAnalytics", "boot", "MASS", "car", "ggplot2", "RColorBrewer", "ggpubr", "markdown", "ggsignif", "rhandsontable", "msm", "dplyr", "car", "magrittr", "ICSNP", "mvnormtest", "psych", "corrplot", "rcompanion", "stringr", "effsize", "sandwich", "ggthemes", "rstatix")
library(data.table)
library(ggplot2)
library("RColorBrewer")
library(ggpubr)
library(markdown)
library(ggsignif)
library(rhandsontable)
library(dplyr)
library(car)
library(magrittr)
library(mvnormtest)
library(ICSNP)
library(psych)
library(corrplot)
library(rcompanion)
library(stringr)
library(effsize)
library(ggthemes)
library(sandwich)
library(msm)
library(car)
library(MASS)
library(boot)
library(PerformanceAnalytics)
library(rstatix)

# Define UI for application
shinyUI(
        
        
        #############################################################################################################################################################
        #############################################################################################################################################################
        #############################################################################################################################################################
        
        ## This is the ui, the part of the shiny app that is visually displayed
        
        #############################################################################################################################################################
        #############################################################################################################################################################
        #############################################################################################################################################################
            
        ui <- navbarPage("Wing Disc Analysis", 
                         
                         ###########################################################################################
                         
                         #Design for wing disc analysis panel
                         
                         ############################################################################################################################    
                         
                         
                         # Create a tab panel for the main data analysis
                         tabPanel("Wing Disc Analysis",
                                  
                                  # This sidebar panel holds most of the user input steps
                                  sidebarPanel(
                                      
                                        
                                      
                                      #Get the user to input the data csv files
                                      fileInput("csvs",
                                                label="Step 1: Upload your dataset(s) here",
                                                
                                                multiple = TRUE),
                                      h6("Select one or multiple wing disc analysis csv files"),
                                      
                                      
                                      #clicking this button extracts the unique image names from the input images and creates an interactive table for the user to put in the genotypes and experiment numbers
                                      h4("Step 2: Assign genotypes"),
                                      
                                      actionButton("saveBtn", "Assign genotypes"),
                                      h6("Use the Genotypes Assignment Table to specify experimental groupings in the second column and genotypes in the third column, then press 'Load Genotypes.' You can skip this step if you re-load data you already analyzed"),
                                      
                                      #clicking this button launches Remi's analysis for the relevant filetype and generates an output table of processed data
                                      h4("Step 3: Analyze your data"),
                                      
                                      actionButton("go", "Analyze!"),
                                      h6("The processed data table will output in the 'Analyzed Data Table' tab"),
                                      
                                      
                                      #This specifies which comparison tests are used, other than fisher's exact
                                      h4("Step 4: Determine plot style and data"),
                                      
                                      
                                      
                                      #Clicking these buttons generates the relevant plots
                                      selectInput("plotType", "Specify plot format:", choices=c("Dot Plot w/ Box Plot", "Violin Plot", "Paired Box Plot", "Histogram", "Histogram w/ Density Plot", "Scatter Plot", "Scatter Plot w/ Regression Line")),
                                      uiOutput('plotSelection'),
                                      uiOutput('plotPaired'),
                                      
                                      #Specify relevant statistical test
                                      uiOutput('stats'),
                                      uiOutput('controlGroup'),
                                      selectInput('effectSize', "Specify effect size output:", choices=c("Cliff's Delta", "Cohen's D - Pooled","Cohen's D - Not Pooled", "Hedges' G - Pooled", "Hedges' G - Not Pooled")),
                                      selectInput("pAdj", "Specify p-value adjustment for use during multiple comparisons:", choices=c("none", "fdr", "holm","hochberg","hommel","bonferroni", "BY")),
                                      
                                      #Give the user the chance to exclude data by other criteria
                                      checkboxInput("excluded", "Omit datapoints?", FALSE),
                                      h5("Retain data that satisfies the condition:"),
                                      uiOutput('exclusioninput1'),
                                      uiOutput('exclusioninput2'),
                                      uiOutput('exclusioninput3'),
                                      
                                      h4("Step 5: Create plot!"),
                                      h5("To adjust anything else about the plot, go to the 'Customize plot' panel. Once you have set your desired parameters, press the 'Generate plot!' button"),
                                      actionButton("genPlot", "Generate plot!"),
                                      h4("Step 6: Download data!"),
                                      h5("Once you download the analyzed dataset, you can load it into RStudio to do any further processing or plot generation that are not included in this module."),
                                      downloadButton("downloadData", "Download")
                                      
                                      
                                      
                                      
                                      
                                      
                                      
                                      
                                  ),
                                  
                                  #This panel holds the plot output
                                  mainPanel(
                                      h4("Plot:"),
                                      plotOutput("plotso")
                                      
                                  ),
                                  
                                  # This panel holds the stats output table and the genotype assignment table
                                  mainPanel(
                                      
                                      textOutput("statsHeader"),
                                      tableOutput("statsTable"),
                                      textOutput("statsHeaderP"),
                                      tableOutput("statsTableP"),
                                      textOutput("effectSizeHeader"),
                                      tableOutput("effectSizeTable"),
                                      h4("Genotype Assignment Table:"),
                                      rHandsontableOutput("table"),
                                      br(),
                                      
                                      #clicking this button loads the genotype assignments that the user has made
                                      actionButton("loadBtn","Load assignments")
                                  )),
                         #This tab panel puts out he anayzed data table
                         tabPanel("Analyzed Data Table", 
                                  sidebarPanel(h4("Data operations"),
                                               h5("Here, you can transform or perform operations on your data. The new data will be added as a new column to the table."),
                                               uiOutput('ColumnChoice'),
                                               selectInput("operation", "Specify operation to perform:", choices=c("+", "-", "*","/", "^", "modulus", "log")),
                                               h5('You can perform this operation against data in another column, or using a fixed value. In the case of log transformations, the value specified below will act as the base of the logarithm'),
                                               checkboxInput('valueOrColumn', "Compare to another column?", value=FALSE),
                                               uiOutput('columnOrFixed'),
                                               textInput("newTitle", "Add new data title", value="Enter title..."),
                                               actionButton("transform", "Transform data!"),
                                               ),
                                               
                                               
                                               
                                  mainPanel(tableOutput("contents"))
                                  ),
                         tabPanel("Customize plot",
                                  
                                  
                                  mainPanel(h3("Text options"),
                                            textInput("Title", "Change title to:", ""),
                                            numericInput("titleSize", "Adjust title font size: ", 20),
                                            textInput("y", "Add Y-axis label:", ""),
                                            checkboxInput("xDel", "Remove X-axis labels?", FALSE),
                                            textInput("xValue", "Add X-axis label:", ""),
                                            textInput("axisFont", "Adjust axis font size: ", 12),
                                            textInput("font", "Change font family to:", ""),
                                            checkboxInput("pDisplay", "Include p-value on plot?", TRUE),
                                            
                                            
                                            
                                            h3("Plot colors"),
                                            textInput("dotFill", "Set dot fill color:", "black"),
                                            textInput("dotColor", "Set dot color:", "white"),
                                            textInput("fill", "Set plot fill color:", "lightgrey"),
                                            textInput("color", "Set plot general color:", "black"),
                                            textInput("backgroundFill", "Set background fill color:", "white"),
                                            textInput("backgroundColor", "Set background color:", "white"),
                                            textInput("statsColor", "Set standard deviation marker color:", "red"),
                                            textInput("majorGridColor", "Set major grid line color:", "white"),
                                            textInput("minorGridColor", "Set minor grid line color:", "white"),
                                            textInput("pairedLineColor", "Set paired line color:", "grey"),
                                            
                                            h3("Color dots according to groupings?"),
                                            h4("Your experimental conditions and groupings must be integers to function correctly"),
                                            selectInput("cgDot", "Group Dot Color By:", choices=c("No color grouping", "GeneName", "week", "GeneWeek")),
                                            selectInput("cgDotFill", "Group Dot Fill Color By:", choices=c("No color grouping", "GeneName", "week", "GeneWeek")),
                                            selectInput("cgColor", "Group Plot Color By:", choices=c("No color grouping", "GeneName", "week", "GeneWeek")),
                                            selectInput("cgColorFill", "Group Plot Color Fill By:", choices=c("No color grouping", "GeneName", "week", "GeneWeek")),
                                            
                                            h3("Figure legend"),
                                            checkboxInput("legend", "Include legend?", TRUE),
                                            textInput("legendTitle", "Set legend title for color grouping:", "Genotype"),
                                            textInput("legendTitle2", "Set legend title for fill grouping:", "Genotype"),
                                            textInput("legendTitle3", "Set legend title for shape grouping:", "Genotype"),
                                            selectInput("legendPosition", "Legend position:", choices=c("right", "left", "top", "bottom")),
                                            
                                            h3("Gridlines and Icons, etc."),
                                            numericInput("dots", "Adjust dot size:", "1", min = 0, max = 20),
                                            numericInput("majorGridSize", "Adjust major gridline size", "0.5", min = 0),
                                            numericInput("minorGridSize", "Adjust minor gridline size", "0.25", min = 0),
                                            numericInput("pairedLineSize", "Adjust paired line size", "0.4", min = 0),
                                            numericInput("bins", "Set histogram binwidth = ", ".025"),
                                            
                                            checkboxInput("limitx", "Custom x-axis limits?", FALSE),
                                            numericInput("xMin", "Set x-axis minimum limit = ", "0"),
                                            numericInput("xMax", "Set x-axis maximum limit = ", "100"),
                                            checkboxInput("limity", "Custom y-axis limits?", FALSE),
                                            numericInput("yMin", "Set y-axis minimum limit = ", "0"),
                                            numericInput("yMax", "Set y-axis maximum limit = ", "100"),
                                            
                                            h3("Organize data"),
                                            
                                            checkboxInput("orderWeek", "Sort by week?", FALSE),
                                            checkboxInput("orderGene", "Sort by gene?", FALSE),
                                            checkboxInput("facetChoice", "Facet output plots?", TRUE),
                                            checkboxInput("lm", "lm regression line?", TRUE),
                                            textInput("facet", "Custom facet (format of 'rows ~ columns')", ""),
                                            selectInput("free_axes", "Free or fixed axes?", choices=c("free_x", "free_y", "fixed", "free")),
                                            h6("Input facet specification in the form of 'cols ~ rows' where cols and rows are either column names or '.' for none. "))                           
                                  
                         ),

                         
                         #Holds normality and variance tests
                         tabPanel("Parametric Assumptions Testing", 
                                  sidebarPanel(h4("Shapiro wilks normality test on: "),
                                               
                                               textOutput("normalityHeader"),
                                               tableOutput("normality"),
                                               
                                               textOutput("normalityHeader2"),
                                               tableOutput("normality2")
                                               
                                  ),
                                  mainPanel(h4("Fligner-Killeen test for equivalent variances"), textOutput("leveneHeader"),
                                            tableOutput("fligner"))),
                         
                         #This panel puts out the approximate cell counts made from approximating each cell as a cylinder of diameter 10 microns
                         tabPanel("Overall Fisher Tests", h4("Number of apoptotic and non-apoptotic cells in clone border vs clone center across all samples"), tableOutput("deathTable"),
                                  h4("Fisher test on number of apoptotic and non-apoptotic cells in clone border vs. clone center"), tableOutput("fishBC"),
                                  h4("Fisher test on number of apoptotic and non-apoptotic cells in clone border between experimental groups"), tableOutput("fishTable")
                         ),
                         tabPanel("Wing Disc by Wing Disc Fisher Tests", 
                                  mainPanel(h4("Counts of competing vs. non-competing wing discs as determined by Fisher Test of apoptotic and non-apoptotic cells in clone border vs. clone center per individual wing disc"),
                                            textOutput("fishyPrint"),
                                            tableOutput("fishyCounty"),
                                            h4("Fisher test on individual wing disc cell counts: "),
                                            tableOutput("siggy"))), 
                         
                         #This panel is used for setting up and running regression analysis
                         tabPanel("Regression Analysis",
                                  sidebarPanel(
                                      width = 4,
                                      h4("Specify your regression parameters:"),
                                      selectInput("linkFunction", "Select regression type:", choices = c("Logistic", "Linear", "Poisson", "Negative Binomial" )),
                                      uiOutput("regressionSelect1"),
                                      textOutput("LogRegressionText"),
                                      uiOutput("regressionSelect2"),
                                      uiOutput("factorCheckboxes"),
                                      selectInput("corrMatTest", "Select test for correlation matrix:", choices=c("pearson", "kendall", "spearman")),
                                      actionButton("goReg", "Run regression!")
                                        ),
                                  mainPanel(
                                          h4("Regression Results"),
                                          tableOutput("regressionData"),
                                          textOutput("vifHeader"),
                                          tableOutput("regressionVif"),
                                          textOutput("chiSquaredHeader"),
                                          tableOutput("regressionChiSquare"),
                                          textOutput("NCVheader"),
                                          tableOutput("meanVarTable"),
                                          verbatimTextOutput("NCVtable"),
                                          textOutput("DWTheader"),
                                          verbatimTextOutput("DWTtable"),
                                          plotOutput("predicted.data.plot"),
                                          textOutput("residualsPlotHeader"),
                                          plotOutput("residualsPlot"),
                                          h4("Summary of Regression Analysis:"),
                                          verbatimTextOutput("regressionSummary"),
                                          h4("Summary of R-squared (or pseudo R-squared) calculation:"),
                                          verbatimTextOutput("Rsquared")
                                         
                                  )
                                 
                                        
                                  ),
                         #Holds the correlation matrix information
                         tabPanel("Correlation Matrix",
                                  h4("Correlation Matrix"),
                                  h6("This panel displays a correlation matrix of all the variables used in regression analysis. Above the diagonal, the correlation value determined by the specified correlation test is shown, along with significance level ( '***' for p=0, '**' for p=0.01, '*' for p=0.05, '.' for p=1). A histogram showing the distribution of each variable is shown on the diagonal. Below the diagonal is a scatter plot of the two variables with a fitted line."),
                                  plotOutput("corrMat")
                                  
                         ),
                         
                         
                         
                         
                         
                         
                         tabPanel("Large Plot", plotOutput("plotso2"))
                         
                         
        )
)
