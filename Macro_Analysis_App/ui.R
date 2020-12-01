
#Here are the libraries we use
################################################################################
list.of.packages <- c("shiny", "PerformanceAnalytics", "boot", "MASS", "car", "ggplot2", "RColorBrewer", "ggpubr", "markdown", "ggsignif", "rhandsontable", "msm", "dplyr", "car", "magrittr", "ICSNP", "mvnormtest", "psych", "corrplot", "rcompanion", "stringr", "effsize", "sandwich", "ggthemes", "shinyBS", "shinythemes", "reshape2", "effects")
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
library(shinyBS)
library(reshape2)
library(shinythemes)
library(effects)


# Define UI for application

#Here is the shiny
##########################################################################################
shinyUI(
        
        
        #############################################################################################################################################################
        #############################################################################################################################################################
        #############################################################################################################################################################
        
        ## This is the ui, the part of the shiny app that is visually displayed
        
        #############################################################################################################################################################
        #############################################################################################################################################################
        #############################################################################################################################################################
            
        ui <- navbarPage("Wing Disc Analysis", 
                         theme = shinytheme("united"),
                         
                         
                         
                 #Design for wing disc analysis panel
                 
                 ############################################################################################################################    
                 
                
                # Create a tab panel for the main data analysis
                tabPanel("Load and Analyze Data",
                          
                # This sidebar panel holds most of the user input steps
                sidebarPanel(
                     
                      
                        #Get the user to input the data csv files
                        h4("Step 1: Upload your dataset(s) here"),
                        tipify(fileInput("csvs", label=NULL, multiple = TRUE),"Select one or multiple .csv files to upload. Data must be arranged in columns, and multiple files must be formatted identically.", placement="right" , trigger="hover" ),

                        #clicking this button extracts the unique image names from the input images and creates an interactive table for the user to put in the genotypes and experiment numbers
                        h4("Step 2: Assign genotypes"),
                        
                        tipify(actionButton("saveBtn", "Assign Experimental Conditions"), "Use the assignment table to group your data. The second column is used to split up experiments - such as separate replicates - for separate analysis (ignore if you have only one experiment). The third column specifies different conditions - groups within an experiment you wish to compare, such as controls, genotypes, drug treatments.", placement="right", trigger="hover" ),
                        
                        
                        #clicking this button launches Remi's analysis for the relevant filetype and generates an output table of processed data
                        h4("Step 3: Analyze your data"),
                        tipify(uiOutput("analysisType"), "If you have uploaded a Whole Disk Analysis CSV, specify what analysis you want to perform. If you choose Complete single cell analysis, you will also have to upload your single cell tracking .csv files in the prompt which will render below",trigger="hover", placement="top"),
                        tipify(uiOutput("secondCsv"), "Here, upload the single cell analysis .csvs in order to perform the complete single cell analysis", trigger="hover", placement = "right"),
                        tipify(checkboxInput("runFishers", "Run overall Fisher's exact test?", FALSE), "Tick this box to run a fishers exact test for all the dying cells across all samples in the border vs. center. Results will be displayed in the Overall Fisher Tests tab", trigger = "hover", placement = "right" ),
                        tipify(checkboxInput("runFishers2", "Run overall Fisher's exact test?", FALSE), "Tick this box to run a fishers exact test for all the dying cells the border vs. center for each sample individually. Results will be displayed in the Overall Fisher Tests tab", trigger = "hover", placement = "right" ),
                        tipify(actionButton("go", "Analyze!"), "Press this button to analyze your data. Results will show in the Analyzed Data Table tab"),
                        
                        
                        
                        #This specifies which comparison tests are used, other than fisher's exact
                        h4("Step 4: Determine plot style and data"),
                        
                        
                        
                        #Clicking these buttons generates the relevant plots
                        tipify(uiOutput("plotType"), "For data grouped into columns, choose either Dot Plot w/ Box Plot or Violin Plot. For paired data, use the Paired Box Plot. To look at a distribution, use the Histogram. For XY data, use the Scatter Plot.", placement="right", trigger="hover" ),
                        tipify(uiOutput('plotSelection'), "Select which dataset to show on the graph. For scatter plots, this specifies the x-axis variable. For paired box plots, this specifies the first variable.", placement="right", trigger="hover" ),
                        tipify(uiOutput('plotPaired'), "For scatter plots, this specifies the y-axis variable. For paired box plots, this specifies the second paired variable.", placement="right", trigger="hover" ),
                        
                        #Specify relevant statistical test
                        tipify(uiOutput('stats'), "Applicable statistical tests are determined based on the datasets provided. On paired box plots, pair-wise statistical tests are performed. To check if assumptions are met for parametric tests such as t-test and anove, check the Parametric Assumptions Testing tab. For details on the tests run, check the Details and Functions tab", placement="right", trigger="hover" ),
                        tipify(uiOutput('controlGroup'), "If you have a designated control group, select it here to specify it as the reference group for statistical testing. If you choose None, then all experimental groups will be compared against all others. If you select All/Basemean, then each group will be compared against the overall mean for all groups.", trigger = "hover"),
                        tipify(selectInput('effectSize', "Specify effect size output:", choices=c("Cliff's Delta", "Cohen's D - Pooled","Cohen's D - Not Pooled", "Hedges' G - Pooled", "Hedges' G - Not Pooled")),"This selects the effect size metric to be used in pairwise comparisons", placement="right", trigger="hover" ),
                        tipify(selectInput("pAdj", "Specify p-value adjustment for use during multiple comparisons:", choices=c("none", "fdr", "holm","hochberg","hommel","bonferroni", "BY")), "This specifies which p-adjustment method to use when performing multiple pairwise comparisons", placement="right", trigger="hover" ),
                        
      

                        
                        h4("Step 5: Create plot!"),
                        tipify(actionButton("genPlot", "Generate Plot!"), "Press this button once you have specified all of your analysis parameters to generate the plot. Go to the Customize Plot panel to adjust any other aspects of the displayed figure.", trigger="hover", placement="top")
                        
                        ),
                  
                  #This panel holds the plot output
                  mainPanel(
                      tipify(uiOutput("plotso"), "To customize the appearance of your plot and download it, go to the Customize and Export Plot tab.", placement="right", trigger="hover" ),
                      tabsetPanel(type="tabs", 
                                  tabPanel("Assign Experimental Conditions", 
                                           h4("Genotype Assignment Table:"),
                                           rHandsontableOutput("table")),
                                  tabPanel("Statistics Outputs",                                       
                                           textOutput("statsHeader"),
                                           tableOutput("statsTable"),
                                           textOutput("statsHeaderP"),
                                           tableOutput("statsTableP"),
                                           textOutput("effectSizeHeader"),
                                           tableOutput("effectSizeTable")),
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
                                  tabPanel("Customize and Export Plot",
                                           fluidRow(column(2,  
                                                           h3("Download plot"),
                                                           numericInput("plotheight", "Plot export height", "3"),
                                                           numericInput("plotwidth", "Plot export width", "3"),
                                                           selectInput("plotUnit", "Height and width units", choices=c("cm", "mm", "in", "px")),
                                                           numericInput("plotres", "Plot resolution", "300"),
                                                           downloadButton("plotDownload", "Download Plot"),  
                                                           hr(),
                                                           
                                                           h3("Text options"),
                                                           textInput("Title", "Change title to:", ""),
                                                           numericInput("titleSize", "Adjust title font size: ", 20),
                                                           textInput("y", "Add Y-axis label:", ""),
                                                           checkboxInput("xDel", "Remove X-axis labels?", FALSE),
                                                           textInput("xValue", "Add X-axis label:", ""),
                                                           textInput("axisFont", "Adjust axis font size: ", 12),
                                                           textInput("font", "Change font family to:", "")),
                                                    
                                                    
                                                    
                                                    column(2,
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
                                                           selectInput("cgDot", "Group Dot Color By:", choices=c("No color grouping", "GeneName", "week", "GeneWeek")),
                                                           selectInput("cgDotFill", "Group Dot Fill Color By:", choices=c("No color grouping", "GeneName", "week", "GeneWeek")),
                                                           selectInput("cgColor", "Group Plot Color By:", choices=c("No color grouping", "GeneName", "week", "GeneWeek")),
                                                           selectInput("cgColorFill", "Group Plot Color Fill By:", choices=c("No color grouping", "GeneName", "week", "GeneWeek"))),
                                                    
                                                    column(2, 
                                                           h3("Customize p-value display"),
                                                           checkboxInput("pDisplay", "Include p-value on plot?", TRUE),
                                                           checkboxInput("adjustedP", "Display adjusted p-value?", TRUE),
                                                           checkboxInput("effSizeDisplay", "Display effect size?", TRUE),
                                                           checkboxInput("ascending", "Display ascenting p-value placement?", TRUE),
                                                           checkboxInput("mapSig", "Display significance codes rather than p-values?", TRUE),
                                                           numericInput("y_offset", "Shift p-values down by factor of:", "0"),
                                                           numericInput("step_size", "Step size between adjacent bars", "0.1"),
                                                           numericInput("tipL", "Tip length", "0.03"),
                                                           numericInput("barThickness", "Set bar thickness", "1"),
                                                           numericInput("textSizeSig", "Set text size:", "8"),
                                                           numericInput("vjustSig", "Set p-value offset from bar", "0"),
                                                           textInput("sigColor", "Set color:", "black"),
                                                           hr(),
                                                           h3("Organize data"),
                                                           
                                                           checkboxInput("orderWeek", "Sort by week?", FALSE),
                                                           checkboxInput("orderGene", "Sort by gene?", FALSE),
                                                           checkboxInput("facetChoice", "Facet output plots?", TRUE),
                                                           checkboxInput("lm", "lm regression line?", TRUE),
                                                           tipify(textInput("facet", "Custom facet (format of 'rows ~ columns')", ""),"Input facet specification in the form of cols ~ rows where cols and rows are either column names or . for none. ", placement="right", trigger="hover" ),
                                                           selectInput("free_axes", "Free or fixed axes?", choices=c("free_x", "free_y", "fixed", "free")),
                                                    ),
                                                    
                                                    
                                                    column(2,                                        
                                                           h3("Figure legend"),
                                                           checkboxInput("legend", "Include legend?", TRUE),
                                                           textInput("legendTitle", "Set legend title for color grouping:", "Genotype"),
                                                           textInput("legendTitle2", "Set legend title for fill grouping:", "Genotype"),
                                                           textInput("legendTitle3", "Set legend title for shape grouping:", "Genotype"),
                                                           selectInput("legendPosition", "Legend position:", choices=c("right", "left", "top", "bottom")),
                                                           hr(),
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
                                                           numericInput("yMax", "Set y-axis maximum limit = ", "100"))
                                                    
                                                    
                                           )
                                  )
                      )
                 
                  
                  
                  
                  
                 )),
                 
         #This tab panel puts out he anayzed data table
         #################################################################################
         tabPanel("Analyzed Data Table",
                
                sidebarPanel(
                        
                        tipify(h4("Data operations:"),"Here, you can transform, delete, or perform operations on your data. The new data will be added as a new column to the table.", placement="right", trigger="hover" ),
                        checkboxInput("Excluder", "Delete values matching that fail to meet these criteria?", FALSE),
                        tipify(uiOutput('ColumnChoice'), "Select which variable you want to perform an operation or transformation on.", placement="right", trigger="hover" ),
                        tipify(selectInput("operation", "Specify operation to perform:", choices=c("+", "-", "*","/", "^", ">", ">=", "==", "<", "<=", "modulus", "log")), 'You can perform this operation against data in another column, or using a fixed value. In the case of log transformations, the value specified below will act as the base of the logarithm', placement="right", trigger="hover" ),
                        tipify(checkboxInput('valueOrColumn', "Compare to another column?", value=FALSE), "Tick this box to perform the operation using the value from a corresponding variable. Otherwise, specify a fixed value with which to perform the operation", placement="right", trigger="hover" ),
                        uiOutput('columnOrFixed'),
                        
                        textInput("newTitle", "Add new data title", value="Enter title..."),
                        actionButton("transform", "Transform data!"),
                        hr(),
                        tipify(uiOutput("checkInversion" ), "Use this selection box and button to invert the z-planes for any z-plane datasets. This is useful if you weren't fussy about where you started and finished imaging.", trigger="hover", placement = "right"),
                        actionButton("columnsGo", "Invert Z-planes!"),
                        tipify(downloadButton("downloadData", "Download Processed Data"), "Once you download the analyzed dataset, you can load it into RStudio to do any further processing or plot generation that are not included in this module."),
                        hr(),
                        numericInput("startIdex", "Display rows ranging from:", 1),
                        numericInput("endIdex", "Display rows ranging from:", 50),
                        uiOutput("checkChoices")

   
                ),
                mainPanel("Analyzed Data Table",
                        tableOutput("contents"),
                )
         ),


         

         

         
         #This panel is used for setting up and running regression analysis
         tabPanel("Regression Analysis",
                  sidebarPanel(
                      width = 4,
                      h4("Specify your regression parameters:"),
                      selectInput("linkFunction", "Select regression type:", choices = c("Logistic", "Linear", "Poisson", "Negative Binomial" )),

                      uiOutput("regressionSelect1"),
                      textOutput("LogRegressionText"),
                      uiOutput("exclusioninputreg4"),
                      uiOutput('regressionSelectExclude'),
                      uiOutput("exclusioninputreg"),
                      
                      uiOutput("exclusioninputreg3"),
                      uiOutput("exclusioninputreg2"),
                      tipify(uiOutput("regressionSelect2"), "In the case of logistic regression wherein you have your counts of successes and failures in separate columns, use this to specify your successes", placement="right", trigger="hover" ),
                      
                      
                       tipify(uiOutput("factorCheckboxes"), "Specify the terms/independent variables for regression analysis.", placement="right", trigger="hover" ),
                      selectInput("corrMatTest", "Select test for correlation matrix:", choices=c("pearson", "kendall", "spearman")),
                      actionButton("goReg", "Run regression!")
                        ),
                  mainPanel( 
                    tabsetPanel(type="tabs",
                      tabPanel("Regression Effects Plot",
                               plotOutput("RegEffectsPlot"),
                               uiOutput("checkChoicesReg")
                      ),
                      tabPanel("Correlation Matrix",         
                        tipify(plotOutput("corrMat"), "This panel displays a correlation matrix of all the variables used in regression analysis. Above the diagonal, the correlation value determined by the specified correlation test is shown, along with significance level ( '***' for p=0, '**' for p=0.01, '*' for p=0.05, '.' for p=1). A histogram showing the distribution of each variable is shown on the diagonal. Below the diagonal is a scatter plot of the two variables with a fitted line.", trigger="hover", placement="left")
                      )
                    ),
                     tabsetPanel(type="tabs", 
                        tabPanel("Regression Results",
                          tableOutput("regressionData")
                          ),
                        tabPanel("Assumptions Testing",
                          textOutput("vifHeader"),
                          tableOutput("regressionVif"),
                          textOutput("chiSquaredHeader"),
                          tableOutput("regressionChiSquare"),
                          textOutput("NCVheader"),
                          tableOutput("meanVarTable"),
                          verbatimTextOutput("NCVtable"),
                          textOutput("DWTheader"),
                          verbatimTextOutput("DWTtable")
                          ),
                        tabPanel("Diagnostic Plots",
                          plotOutput("predicted.data.plot"),
                          textOutput("residualsPlotHeader"),
                          plotOutput("residualsPlot")
                          ),
    
                        tabPanel("Raw Outputs and Summary",
                          h4("Summary of Regression Analysis:"),
                          verbatimTextOutput("regressionSummary"),
                          h4("Summary of R-squared (or pseudo R-squared) calculation:"),
                          verbatimTextOutput("Rsquared")
                         ),
                         tabPanel("Download Plot",
                          h3("Download plot"),
                          numericInput("plotheightreg", "Plot export height", "3"),
                          numericInput("plotwidthreg", "Plot export width", "3"),
                          selectInput("plotUnitreg", "Height and width units", choices=c("cm", "mm", "in", "px")),
                          numericInput("plotresreg", "Plot resolution", "300"),
                          downloadButton("plotDownloadReg", "Download Plot"),  
                          
                          )
                        
                  ))
                 
                        
                  ),

           tabPanel("Details and Formulas",
                    h3("On this panel, functions used for specific statistical tests are provided. This tool was made using R Shiny software, and all statistical tests were conducted using accepted R software functions."),
                    h4("(1) Wilcoxon, t-test, anova, and kruskal-wallis tests are conducted using the 'compare_means()' function. Pairing is set to TRUE in the case of a paired box plot"),
                    h4("(2) P-value adjustments in the case of multiple pairwise comparisons are done via the 'p.adjust.method' function"),
                    h4("(3) Fisher tests are conducted using the fisher.test() function on a 2x2 contingency table of dying vs. not-dying cells"),
                    h4("(4) For parametric assumptions testing, the shapiro-wilks test is conducted using the 'shapiro.test' function and the fligner-killeen test is conducted using the 'fligner.test' function"),
                    h4("(5) For effect size metrics, cohen's d and hedges' g are conducted using the 'cohen.d' function, and cliff's delta is conducted using the 'cliff.delta' function, both from the 'effsize' package."),
                    h4("(6) Spearman's Rho, Pearson's R, and Kendall's Tau are conducted using the 'cor.test' function"),
                    h4("(7) The Hotelling's T2 test is conducted using the 'HotellingsT2' function. The multivariate shapiro-wilks test is conducted using 'mvnormtest::mshapiro.test.'"),
                    h4("(8) The 'glm' function is used to conduct logistic and poisson regression. The 'lm' function is used to conduct linear regressio. The 'glm.nb' function is used to conduct negative binomial regression."),
                    h4("(9) The correlation matrix is generated using the PerformanceAnalytics package")
                    )       
         )

         
         
         
         
         
         
         
         
         
)

