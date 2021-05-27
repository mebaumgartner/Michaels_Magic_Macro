# Pipeline for Enhanced Clonal Analysis (PECAn)

## Introduction

> The PECAn pipeline aims to serve as a complete, integrated image and data analysis and processing pipeline. PECAn aims to allow researchers to take complex images (2D or 3D, multichannel, multi-genotype, still images or videos) and convert them into useful data along with performing statistical tests and graph generation, all with minimal user input. The pipeline consists of two parts - an image analysis plugin for ImageJ/FIJI and a data processing, statistical analysis, and graph generation application written in R shiny.

## How to Use

> To run the pipeline, put all of your images of a given type into an otherwise empty folder. Then launch FIJI, and select 'PECAn_1.3' from the plugins dropdown menu. You will then be prompted to select the folder in which your images have been saved. A user interface will then allow you to specify which analysis to perform. As the program runs, it will generate output images and .csv files. The output images are a visual representation of the analysis performed, and are essential for evaluating whether or not the analysis was performed correctly. The csv files can then be uploaded into the analysis app for processing. Step-by-step instructions are provided in the user interface.

## Installation

> The two parts of the pipeline must be installed separately. For the imagej/FIJI plugin, there are two options: (1) manual installation and (2) using the FIJI update site manager. To manually install the plugin, go to the Fiji Plugin folder of the folder downloaded from Github. Then, find where on your computer FIJI has been installed. Copy and paste the 'PECAn_1.3.py' file into 'Fiji.app\plugins,' and copy and paste the 'Lib' folder into 'Fiji.app\jars.' This plugin also requires several additional plugins: MorphoLibJ, BioVoxxel, and IJ-plugins toolkit. The update site is currently not live, but will be provided once the software is published.

> For the R shiny app, there are also two options: (1) run from the web and (2) run locally. To access from the web, copy and paste the following url into any conventional web browser: https://michaelbaumgartner.shinyapps.io/Macro_Analysis_App/ To rub the app locally, copy and paste the 'Macro_Analysis_App' folder onto your computer, and open the ui.R and app.R files in R studio. Then, press the 'run App' button in the top righthand corner. Several packages will need to be installed, though R studio should automatically guide you through the process. If not, the libaries are listed at the top of the ui.R file.