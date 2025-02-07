##########################################################################################################################################################################################
##########################################################################################################################################################################################
#
#
#  PECAn - this is a script for comprehensive image analysis and quantification of confocal images
#
#
#
#
##########################################################################################################################################################################################
##########################################################################################################################################################################################


# * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * #
# * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * #
##############		THIS SCRIPT DEPENDS UPON A PACKAGE ENTITLED JSF_package. JSF PACKAGE WILL BE IN THE Jars\Lib FOLDER!!! EACH RELEVANT MODULE CAN BE FOUND THEREIN!!!		#################
# * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * #
# * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * #







##########################################################################################################################################################################################
##########################################################################################################################################################################################
#
#
#  Here is the primary script
#
#
##########################################################################################################################################################################################
##########################################################################################################################################################################################

def run_script():

	print ("Script launched")
	#Default imageJ and pyton imports that should all be installed with you basic FIJI
	from java.awt import Color, Rectangle
	from ij import IJ, WindowManager, ImagePlus
	import ij
	import os
	import gc
	from ij.plugin.frame import RoiManager
	from ij.measure import ResultsTable
	from ij.gui import ShapeRoi
	from random import randrange
	from ij.plugin import ZProjector, Concatenator

	from JSF_package import Initialize
	Initialize.initializer()

	############################################
	# First, we close any excess open windows and reset the ROI manager
	# This keeps windows from piling up and possibly interfering with the macro

	#Here we set out background color to black
	IJ.setBackgroundColor(0, 0, 0)

	#Delete any old macro tables that are still open from previous runs
	closeStrings = ["summary table", "Disc Analysis", "clone tracking analysis", "whole disc analysis", "Clone Analysis", "Summary Table"]

	#Close all open images
	windows = WindowManager.getAllNonImageWindows()
	for item in windows:
		item = item.getTitle()
		for sub in closeStrings:
			if item.find(sub) != -1:
				x =WindowManager.getWindow(item)
				WindowManager.setWindow(x)
				IJ.run("Close")

	#Set parameters and initialize roiManager, clear excess ROIs
	rt = ResultsTable.getResultsTable()
	rm = RoiManager().getInstance()
	rm.reset()
	IJ.run("Line Width...", "line=4")
	IJ.run("Options...", "iterations=1 count=1 do=Nothing")
	IJ.run("Set Measurements...", "area modal mean standard min centroid perimeter fit shape feret's integrated nan redirect=None decimal=3")


	##########################
	# This bit of code launches the GUI from which the user specifies what kind of analysis to run

	#Get all user parameters, exit the script if the window is cancelled
	from JSF_package.user_inputs_GUI import Open_GUI
	try:
		#Call user input GUI function
		ok = Open_GUI()
		if ok == 0:
			IJ.error("Process cancelled! \n \n"+ "So long for now, partner!")
			return
		customAnalysis = 0
	except:
		IJ.error("Could not get parameters!")
		return
	del Open_GUI
	

	#Once the user inputs have been specified, we pull all the user-specified parameters as variables
	from JSF_package.configBasic import singleCellMethod, dcp1Counting, dcp1Deluxe, cellCountDeluxe, restoreROI, cellDeathSegMethod, zMethod
	from JSF_package.configSave import generateImage, csvSaver
	from JSF_package.configRoi import minRoiSize
	
	import JSF_package
	from JSF_package import  _misc_, caspase_analysis, cell_tracking, clone_analysis, configBasic, configCellTrack, configCloneSeg, configDeathSeg, configDeathTrack, configRoi, configSave, seeded_region_growing, speckles_analysis, start_up, tracking_and_outputs, user_inputs_GUI
	from JSF_package._misc_ import mask_confirmer, channel_open, selection_confirmer, channel_organize_and_open, decode_channels
	
	# the variable 'deluxeCell' determines whether or not we launch the detailed single cell analysis (i.e. z-position, distance to border, etc.).
	# True means 'yes, run the fancy single cell analysis'
	if (dcp1Deluxe == True and dcp1Counting == True and cellDeathSegMethod != "Disabled") or (cellCountDeluxe == True and singleCellMethod != "Disabled"):
		deluxeCell = True
	else:
		deluxeCell = False

	roiMask = 0 # placeholder variable for seeded Region growing on roi mask. Don't worry about this variable now. It will be explained in the seeded region growing step
	IJ.log("Analysis parameters successfully specified by user") #The IJ.log function just prints a string to the log for measuring progress

	#This is a placeholder variable for the display panel. If the user opts to display this image for troubleshooting, we will show it once per loop
	impVis = ImagePlus()

	#Prompt the user to specify the desired input folder. Exit script if none specified
	try:
		dir1, filelist, dir2, dir1name = JSF_package.start_up.folder_selection()
		IJ.runMacro("run('Close All')")
	except:
		IJ.error("No appropriate folder selected. Please try again.")
		return

	IJ.log("User specified that analysis should be run on all image files in the directory: "+str(dir1))

	#The user has to tell the program how many distinct genotypes to look for in each image.
	#This information is held in the 'numGenotypes' variable
	numGenotypes = JSF_package.configBasic.numGenotypes

	#The 'halfHalfNC' variable will be true if the user opted to analyze two ROI's in an image.
	#It is called this was originally developed to analyze wing discs that are half wildtype and half minute and do not compete, thus we call them 'Non-competing half and half' experiments
	#A lot of the variable names are derived from their relevance to minute competition experiments, so I will provide context to hopefully help you keep them straight.

	#The software treats all genotypes in each ROI separately, so we have to double the numGenotypes variable
	if JSF_package.configRoi.halfHalfNC == True:
		numGenotypes = 2*numGenotypes

	loop_counter = 0
	fullCloneROIArray = [] #This array holds the raw roi for the clone channel without overlap with pouch. We use this to calculate single cell distances to the border

	#We need one list per genotype. We prepare this variable to have empty lists for each genotype, which we will add to later
	while loop_counter < numGenotypes:
		fullCloneROIArray = fullCloneROIArray + []
		loop_counter += 1

	#The macro will output the names of the genotypes, to help the user keep things straight. These are kept in the 'genotypeNames' variable
	#These are the generic default names. IF the user uses a WEKA classifier, the names will be whatever they named the classes.
	genotypeNames = ["Genotype 1", "Genotype 2", "Genotype 3", "Genotype 4", "Genotype 5"]

	#For two ROI analyses, we tag each genotype name with which ROI it corresponds to
	if JSF_package.configRoi.halfHalfNC == True:
		genotypeNamesNew = []
		for name in genotypeNames:
			genotypeNamesNew = genotypeNamesNew + [	"ROI_1_"+name, "ROI_2_"+name]
		genotypeNames = genotypeNamesNew




	#Here we prompt the user to specify ROIs for analysis
	#If restore ROIs option is not ticked, prompt the user to input the ROIs for analysis
	if restoreROI != True:
		try:
			selections = JSF_package.start_up.ROI_selection(filelist, dir1, rm)
			if selections == 0:
				IJ.error("No images were marked for processing! \n \n"+ "Either you skipped all the images in the file, or you did not select any ROIs. \n"+"Make sure there is an ROI on the image when you click the button. \n" +"You can draw an ROI with any selection tool. \n \n"+"Better luck next time!")
				return
		except:
			IJ.error("Could not process ROI's! An unexpected error happened in the ROI selection step. \n"+"This shouldn't happen. If this keeps happening, make sure the plugin is installed properly - along with its dependencies - and the images are of the correct format.")
			return
		IJ.log("New ROIs successfully obtained from user inputs")



	errCount = 0 #This variable counts how many images could not be processed (images that threw an error in the try/except blocks)
	errStr = "" #This variable tracks the names of the images that could not be processed. This string is printed and in a file to let users know which images were not handled well

	#We save all our variable choices to a metadata text file. Useful if we want to come back and remember some settings we used.
	########################################

	metaPath = os.path.join(dir1, "MetaData.txt")
	outFile2 = open(metaPath, 'w')
	output2 = ""
	pathMet = os.getcwd()
	dirMet = os.path.join(pathMet, "jars", "Lib", "JSF_package")

	from inspect import getmembers
	

	dataNew = ""
	varNames = [item for item in dir(configBasic) if not item.startswith("__")]
	for item in getmembers(configCellTrack):
		if item[0] in varNames:
			dataNew = dataNew + str(item[0]) + "=" + str(item[1]) + "\n"

	IJ.log(dataNew)
	output2 = output2 + dataNew
	

	dataNew = ""
	varNames = [item for item in dir(configCellTrack) if not item.startswith("__")]
	for item in getmembers(configCellTrack):
		if item[0] in varNames:
			dataNew = dataNew + str(item[0]) + "=" + str(item[1]) + "\n"

	IJ.log(dataNew)
	output2 = output2 + dataNew



	dataNew = ""
	varNames = [item for item in dir(configCloneSeg) if not item.startswith("__")]
	for item in getmembers(configCellTrack):
		if item[0] in varNames:
			dataNew = dataNew + str(item[0]) + "=" + str(item[1]) + "\n"

	IJ.log(dataNew)
	output2 = output2 + dataNew


	dataNew = ""
	varNames = [item for item in dir(configDeathSeg) if not item.startswith("__")]
	for item in getmembers(configCellTrack):
		if item[0] in varNames:
			dataNew = dataNew + str(item[0]) + "=" + str(item[1]) + "\n"

	IJ.log(dataNew)
	output2 = output2 + dataNew
	del getmembers

	outFile2.write(output2)
	outFile2.close()
	################################################


	anyImages = False #This variable determines if any images are in the specified folder. It is false until proven otherwise
	validSelections = False #This variable determines if the user input valid ROI selections. It is false until proven otherwise

	#####################################   Loop each file in the specified folder one-by-one for analysis       #####################################################
	for names in filelist:

		

		#This try/except block covers if entire files throw an error
		#To debug the script, comment out this try/except block along with the one corresponding to individual images
		try:


			print ("New file selected")
			csvCount = 1 #This variable tracks how many .csv files we have put out and is used to number the files. We save and restart each analysis table (other than the summary table) after each ten discs. This helps prevent running out of memory
			goodImage = 0 #This variable tracks the number of images successfully analyzed. This tells the macro when to save csv files.
			rtD = ResultsTable() #This is the whole disc analysis table variable
			rtC = ResultsTable() #This is the individual clones analysis table
			rtS = ResultsTable()	#This is the summary table
			rtDeluxe = ResultsTable() #This is the deluxe cell counting results table

			#We open any files in the folder if they aren't any of the following genotypes
			#The macro will attempt to open anything, as images can come in all sorts of types compatible with the bioformats importer
			#If it bumps into a weird filetype it can't handle, the try/except block will just throw an error, and the script will start on the next file
			tcheck = names.find(".txt")
			mcheck = names.find(".model")
			zcheck = names.find(".zip")
			pcheck = names.find(".py")

			if (tcheck == -1) and (mcheck == -1) and (zcheck == -1) and (pcheck == -1):

				anyImages = True
				IJ.log("Analysis started on file: "+ str(names))

				#Get rid of any excess or lingering images
				IJ.runMacro("run('Close All')")

				#Restore selections from input files
				#The users ROIs and z-plane specifications will be saved in the input folder.
				#Here, we just reload them for the relevant file.
				skipString, numImages, startEndSlice, lifVariable = JSF_package.start_up.selection_retrieval(names, dir1, rm)

				if skipString != "Cancel":
					validSelections = True

				#skipstring is a record of the namess of all images and files that the user chose to skip. Here, we just add a log of these to the error output
				if skipString == "Cancel":
					errCount = errCount + 1
					errStr = errStr + "[file (skipped):" + names + "]"
					continue


				#Track all the images we tried to process regardless of if they throw an error or not. We use this to extract information from the saved parameters.
				fullLoopCounter = 0

				#Designate active filepath and track number of images opened
				inPath = dir1 + names
				lifVal = 0

				#ROIarchive is just the number of ROIs loaded into the manager from the user specifications
				#At the end of processing each image, we will delete all ROIs in the roi manager with an index greater than this.
				#This just prevents the ROI manager from getting bogged down and eating up loads of memory
				ROIarchive = rm.getCount()



				#####################################   Loop through all images in the file        #####################################################
				for indices in lifVariable:

					


					#This try/except block covers individual images, in case they throw an error. If they do, we make a note of which images could not be processed
					#For debugging, comment out this try/except block as well as the one for overall files
					try:

						IJ.log("Analysis started on image: "+str(indices))
						print("Analysis started on image: "+str(indices))

						#Reset the ROI manager by deleting all ROIs other than the user-specified ones
						currentNumRois = rm.getCount()
						while currentNumRois > ROIarchive:
							rm.select(currentNumRois - 1)
							rm.runCommand("Delete")
							currentNumRois = rm.getCount()


						loop_counter = 1
						fullCloneROIArray = [[]] #This array holds the raw roi for the clone channel without overlap with pouch. We use this to calculate single cell distances to the border
						while loop_counter < numGenotypes:
							fullCloneROIArray = fullCloneROIArray+[[]]
							loop_counter += 1



						#Placeholder variables for timelapse analysis.
						timepoint = 1
						timeFinish = 1


						#Initialize a variable to hold all timelapse images across all timepoints to create a final timelapse composite image, if applicable
						timeLapseImageArray = list()


						#This loop runs through each timepoint of the image. If the image is not a timelapse, it just runs once.
						while timepoint <= timeFinish:

							print ("Timeloop initiated")

							#Set the starting and ending Z-slices from user input
							#startEndSlice holds the starting and ending z-planes specified by the users

							#the variable 'stackno' (short for stack number) references which z-plane is currently being analyzed
							#we set it initally to whatever the user specified as the starting z-plane
							stackno = startEndSlice[fullLoopCounter*2]
							stackend = startEndSlice[(fullLoopCounter*2)+1] #This variable marks the final z-plane the user specified for analysis
							ref1 = stackno.find(".")
							ref2 = stackend.find(".")
							if ref1 != -1:
								stackno = int(stackno[:ref1])
							else:
								stackno = int(stackno)
							if ref2 != -1:
								stackend = int(stackend[:ref2])
							else:
								stackend = int(stackend)

							IJ.log("Timepoint "+str(timepoint)+" of "+str(timeFinish)+" started")

							#Exit the macro if the user pressed the escape key at any intervening time.
							if IJ.escapePressed() == 1:
								IJ.log("Process aborted by user")
								IJ.error("Process cancelled! \n \n"+ "So long for now, partner!")
								return

							#if the user wants to have the image panel displayed during processing, we show it
							#'visualize=1' means 'yes, display the output image.' The variable 'generateImage' references whether or not the user opted to generate the output panel
							if JSF_package.configSave.visualize == 1 and generateImage == True:
								impVis.show()


							#We only all the channels from the image we are interested in at the correct timepoint and z-level and store them in an array.
							pullArray, timeFinish, colorsOfChannels = channel_organize_and_open(indices, inPath, timeFinish, timepoint )
							if pullArray == "Error":
								IJ.log("Unable to load image channels! \n \n"+"This likely means that you told the macro to analyze a channel that does not exist. \n" + "For example, if your image has channels, you can't analyze channel 3. \n" + "The macro will try again on the next image. \n"+ "If this keeps happening, abort the macro by pressing 'Escape.' \n \n"+ "Sorry about all this!")
								break

							#Z-project down to a 2D-image, if specified by user
							if zMethod != "Disabled":

								#Create a loop to run through all open image stacks
								zCounter = 0
								while zCounter < len(pullArray):

									#Extract the images from the pullArray
									toBeProjected = pullArray[zCounter]
									toBeProjectedImp = toBeProjected[-1]

									#If the image exists, we Z-project it
									if toBeProjectedImp != 0:
										savTitle = toBeProjectedImp.getTitle()
										projectedImp = ZProjector.run(toBeProjectedImp, zMethod, stackno, stackend)

										#We reset the title and put the image back in the array for storage
										projectedImp.setTitle(savTitle)
										toBeProjected[-1] = projectedImp
										pullArray[zCounter] = toBeProjected

									zCounter += 1


								#As we now have only one z-plane, we set our stackend variable equal to stackno so the loop does not repeat
								stackend = stackno

							#We extract the images from the array. The 'decode channels' function also concatenates and merges multiple channels, if the user specified multiple channels for a given analysis
							IDs, colorsOfChannels = decode_channels(JSF_package.configBasic.cloneChannel, pullArray, colorsOfChannels, 1)
							

							#If the user opted to use a 3D weka classifier, we just run it once now and then store the resulting segmented stack
							#When we loop through each z-plane, we just pull each z-plane from this segmented stack to use as our clones image
							segmentedIDs, genotypeNames = JSF_package._misc_.weka3D(IDs, JSF_package.configBasic.cloneSeg, genotypeNames)

							#We re-assign the genotype names based on the WEKA classifier
							if segmentedIDs != 0:
								genotypeNames = genotypeNames[1:numGenotypes+1]

								if JSF_package.configRoi.halfHalfNC == True:
									genotypeNamesNew = []
									for name in genotypeNames:
										genotypeNamesNew = genotypeNamesNew + [	"ROI_1_"+name, "ROI_2_"+name]

									genotypeNames = genotypeNamesNew

							#Store some variables for safe keeping and get some housekeeping parameters so we don't lose them
							Title = IDs.getTitle() #IDs is just the clone image (stands for 'Image of Discs')
							calibration = IDs.getCalibration() #This gets the pixel calibration (i.e. pixels to microns or whatever)

							#The name 'pouch' corresponds to the overall ROIs specified by the user.
							#This name is because in wing disc competition experiments, we are interested in analyzing the 'pouch' of the wing discs
							#Therefore, I just called the ROI the pouch, because my ROIs are always just the pouch

							#We group all the user specified ROIs together (the user can specify several for different Z-planes)
							pouchArray = [] #This holds all the ROIs corresponding to the first user specified ROI.
							pouch2Array =[] #This holds all the ROIs corresponding to the second user specified ROI.

							IJ.log("Image Name: "+ str( Title))


							#Get the foci channel from the original image

							#Throughout the script, you will see references to 'cell death' and 'dcp1' and 'caspase.'
							#This is because I originally developed the foci segmentation tool to identify dying cells based on
							#cleaved caspase/dcp1 stainings. This is just the foci segmentation.
							if JSF_package.configBasic.dcp1Choice == 1:
								IDs2, colorsOfChannels = decode_channels(JSF_package.configBasic.dcp1Channel, pullArray, colorsOfChannels, 1)
								Title2 = IDs2.getTitle()
								segmentedIDs2, blank = JSF_package._misc_.weka3D(IDs2, JSF_package.configBasic.cellDeathSegMethod, genotypeNames)


							#Get the fluorescence channel from the original image
							IDs3 = 0
							if JSF_package.configBasic.fluoChoice == 1:
								if JSF_package.configBasic.preProcess == True:
									option = 1
								else:
									option = 0
								IDs3, colorsOfChannels= decode_channels(JSF_package.configBasic.fluoChannel, pullArray, colorsOfChannels, option)
								Title3 = IDs3.getTitle()


							#Get the speckle channel
							IDs4 = 0
							if JSF_package.configBasic.speckle == 1:
								IDs4, colorsOfChannels = decode_channels(JSF_package.configBasic.speckleChannel, pullArray, colorsOfChannels, 0)
								Title4 = IDs4.getTitle()

							#Get the channels for seeded region growing. We can do seeded region growing in several different analyses, so we do this several times
							############################################

							if JSF_package.configRoi.seedChoice == True:
								from JSF_package.configRoi import seedChoice, seedChannel, minSeedSize, invertSeed
		 						seedIDroi, colorsOfChannels= decode_channels(seedChannel, pullArray, colorsOfChannels, 0)
							else:
								seedIDroi = 0
							if JSF_package.configCloneSeg.seedChoiceClones == True and JSF_package.configCloneSeg.seedChannelClones != "ROI Mask":
								seedIDclone, colorsOfChannels = decode_channels(JSF_package.configCloneSeg.seedChannelClones, pullArray, colorsOfChannels, 0)
	 						else:
	 							seedIDclone = 0
	 						if	JSF_package.configCellTrack.seedChoiceCell == True:
	 							seedIDcell, colorsOfChannels = decode_channels(JSF_package.configCellTrack.seedChannelCell, pullArray, colorsOfChannels, 0)
	 						else:
	 							seedIDcell = 0
	 						if JSF_package.configDeathSeg.seedChoiceCas == True:
		 						seedIDcas, colorsOfChannels = decode_channels(JSF_package.configDeathSeg.seedChannelCas, pullArray, colorsOfChannels, 0)
							else:
								seedIDcas = 0

							##################################################################

	 						IJ.log("Substacks/channels obtained and ready to be analysed")



							#We Identify the appropriate ROI by searching for its name in the ROI list.
							excludinator = 0
							multiPouch = []

							#We don't need this step if the user is going to extract the ROIs with a script
							if (JSF_package.configBasic.ROIseg == "Manual Selections" ) or (JSF_package.configBasic.ROIseg == "Analyze Entire Image"):

								#We loop through each Roi in the list, and check it's name. If the name is correct (Title+pouch) and at the right zlevel, we pull it and save it.
								customROI = False
								count1 = 0
								while count1 < rm.getCount():

									rm.select(count1)
									rName = rm.getName(count1)
									rName = str(rName)
									if str(Title + "-Po") in rName:

										pouch = ShapeRoi(rm.getRoi(count1))

										zVals = rName.find("Pouch[z")
										if zVals != -1:
											extractor = rName[zVals+7:-2]
											split = extractor.find("-")
											zValsStart = extractor[:split]
											zValsEnd = extractor[split+1:]
											zValsStart = float(zValsStart)
											zValsEnd = float(zValsEnd)
											zValsStart = int(zValsStart)
											zValsEnd = int(zValsEnd)
											ROIslices = range(zValsStart, zValsEnd+1)
											multiPouch = multiPouch + [pouch, ROIslices]

										#We get the second roi region, if specified by the user
										try:
											if JSF_package.configRoi.halfHalf == True or JSF_package.configRoi.halfHalfNC == True:
												excludinator = ShapeRoi(rm.getRoi(count1+1))
												if zVals != -1:
													multiPouch = multiPouch + [excludinator]
										except:
											IJ.error("Oh no! You attempted to analyze two ROIs in the image, but the macro can only find one!\nThis means you either attempted to 'restoreROIs' on a folder that only had one ROI saved per image, or, if you are using weka, your weka classifier was set up incorrectly\nThe first class of objects must be background, and there must be two additional classifiers for your two ROIs.\nIf you don't want to analyze two rois, switch off that feature in the advanced ROI settings.")
											exit()
									count1 = count1 + 1
								IJ.run("Clear Results")
								rm.runCommand("Deselect")

							else:

								#If the user chooses to get the ROIs with a script, we pull the specifed channel for segmentation and store it
								IDs6, colorsOfChannels = decode_channels(JSF_package.configBasic.ROIchannel, pullArray, colorsOfChannels, 1)
								segmentedIDs6, blank = JSF_package._misc_.weka3D(IDs6, JSF_package.configBasic.ROIseg, genotypeNames)
								customROI = True

								#Get the extra ROI if specified by the user
								if JSF_package.configRoi.halfHalf == True or JSF_package.configRoi.halfHalfNC:
									count1 = 0
									while count1 < rm.getCount():

										rm.select(count1)
										rName = rm.getName(count1)

										if str(Title + "-Non-Clone") in str(rName):
											excludinator = ShapeRoi(rm.getRoi(count1))
											zVals = rName.find("Clone[z")
											extractor = rName[zVals+7:]
											split = extractor.find("-")
											zValsStart = extractor[:split]
											zValsEnd = extractor[split+1:]
											zValsStart = float(zValsStart)
											zValsEnd = float(zValsEnd)
											zValsStart = int(zValsStart)
											zValsEnd = int(zValsEnd)
											ROIslices = range(int(zValsStart), int(zValsEnd)+1)
											multiPouch = multiPouch + [excludinator, ROIslices]
										count1 = count1 + 1
									rm.runCommand("Deselect")



							cellCountChannel = JSF_package.configBasic.cellCountChannel


							#We get the DAPI roi channel, which we will use for filtering our individual cells, and the actual single cell ROI, if appropriate
							if JSF_package.configBasic.cellCount == 1:
								if JSF_package.configBasic.DAPIchannel != 'Disabled':

									IDs5, colorsOfChannels= decode_channels(JSF_package.configBasic.DAPIchannel, pullArray, colorsOfChannels, 0)
									Title5 = IDs5.getTitle()


									#Mean Z-project our Dapi image, get the mean and stdev of the pixel values
									projection = ZProjector.run(IDs5, "avg", stackno, stackend)
									DF  = projection.getRawStatistics()
									impStdDev = DF.stdDev
									impMean = DF.mean
									DAPIfilter = impMean - (impStdDev * configCellTrack.dapiFilterWeight)
									print "DAPIfilter =", DAPIfilter

									



								else:
									IDs5 = 0
									DAPIfilter = 0

								IDsCellCount, colorsOfChannels= decode_channels(JSF_package.configBasic.cellCountChannel, pullArray, colorsOfChannels, 1)
								segmentedIDsCC, blank = JSF_package._misc_.weka3D(IDsCellCount, JSF_package.configBasic.singleCellMethod, genotypeNames)

							else:
								IDsCellCount =  IDs
								cellCountChannel = JSF_package.configBasic.cloneChannel
							if JSF_package.configBasic.singleCellMethod == "Default":
								IDsCellCount =  IDs
								cellCountChannel = JSF_package.configBasic.cloneChannel

							#Figure out the dimensions of the image
							iHeight = ImagePlus.getHeight(IDs)
							iHeightArchive = iHeight
							iWidth = ImagePlus.getWidth(IDs)
							sliceROIs = list() #This list keeps track of all the non-single clones ROIs. We will use this in our measurement step
							cloneROIs = [[]] #This list keeps track of all the single clone ROIs
							genCount = 1
							while genCount < numGenotypes:
								cloneROIs = cloneROIs + [[]]
								genCount += 1
							blank = 0

							caspaseSegment = list() #This list keeps track of all the single Foci ROIs
							cellROIs = list() #This list keeps track of all the single cell ROIs

							#This list keeps track of all the borderROIs
							borderArray = [[]]
							counter = 1
							while counter < numGenotypes:
								counter += 1
								borderArray = borderArray + [[]]

							casRefArray = list()



							if JSF_package.configRoi.halfHalfNC == True and (JSF_package.configBasic.cloneSeg != "Membrane-tagged GFP" and JSF_package.configBasic.cloneSeg != "Cytosolic GFP" and JSF_package.configBasic.cloneSeg != "Use ROIs As Clones"):
								numGenotypes = numGenotypes / 2

							#These arrays keep track of all our masks and images for generating the output image
							cloneMaskArray = list()
							borderMaskArray = list()
							casMaskArray = list()
							cloneImpArray = list()
							fluoImpArray = list()
							casImpArray = list()
							cloneBorderArray = list()
							casCasArray = list()
							cloneTrackingArray = list()
							refOutArray = list()
							refStandArray = list()
							refBaseArray = list()

							#Original starting slice
							zStart = stackno
							pouchHeight = stackend - stackno

							zValsN = [-1]
							pouchCount = 0

							#We create an array of unique colors. These are used to identify distinct genotypes in the output panel
							colorArray = [[0,255,255], [255,105,0]]
							colores = 0
							while colores < 100:
								colores += 1
								color = [round(randrange(0,255)),round(randrange(0,255)), round(randrange(0,255))]
								color = [int(num) for num in color]
								colorArray = colorArray+[color]



							#####################################   Here we start a loop through each desired z-slice of the stack and begin our analysis         #####################################################
							while stackno <= stackend:

								print ("Next z-plane selected")
								

								IJ.log("Z-plane "+str(stackno)+" of "+str(stackend)+" started")

								#Aborts if user presses escape
								if IJ.escapePressed() == 1:
									IJ.log("Process aborted by user")
									IJ.error("Process cancelled! \n \n"+ "So long for now, partner!")
									exit()


								#If ROIs were manually added, the ROI names are annotated with the z-levels they apply to. We loop through all the available ROIs to check if the current Z-level in range
								correctROI = False
								pouchCount = 0
								while len(multiPouch) > 1 and correctROI == False:

									if customROI == False:
										pouch = multiPouch[pouchCount]
										zValsN = multiPouch[pouchCount + 1]
										if JSF_package.configRoi.halfHalf == True or JSF_package.configRoi.halfHalfNC == True:
											excludinator = multiPouch[pouchCount+2]
											pouchCount += 3
										else:
											pouchCount += 2
									elif JSF_package.configRoi.halfHalf == True or JSF_package.configRoi.halfHalfNC == True:
										excludinator = multiPouch[pouchCount]
										zValsN = multiPouch[pouchCount + 1]
										pouchCount += 2

									for item in zValsN:
										if stackno == item:
											correctROI = True

								#If the user put in two ROIs that overlap with each other, we want to prevent double Counting
								#The second ROI takes precedence, and the first ROI becomes whichever part of the first ROI don't overlap with the second ROI
								if JSF_package.configRoi.halfHalfNC == True or JSF_package.configRoi.halfHalf == True:
									pouch.not(excludinator)
									pouch2Array = pouch2Array + [excludinator]

								#If the user does not use manually drawn ROIs but instead creates them from a script or a weka classifier, we launch this block of code
								if customROI == True:

									#IDs6 is the channel the user specified for segmenting the clones
									IDs6.setSlice(stackno)
									ROIimp = IDs6.crop()
									cal = ROIimp.getCalibration()
									caliber = cal.pixelHeight

									#if the uesr specified segmentation (kept in the variable 'ROIseg') ends with .model, it is a WEKA classifier
									#We therefore launch WEKA segmentation
									if JSF_package.configBasic.ROIseg.endswith(".model"):

										#If it's a 2D weka classifier, we run it once per Z-plane
										if JSF_package.configBasic.ROIseg.find("_3D_.")==-1:

											from trainableSegmentation import WekaSegmentation

											segmentator = WekaSegmentation( ROIimp )
											classifierPath = os.path.join(os.getcwd(), "jars", "Lib", "JSF_package", "Weka_Models", JSF_package.configBasic.ROIseg )
											segmentator.loadClassifier(classifierPath)
											resultant = segmentator.applyClassifier(ROIimp, 0, False)
											
											del segmentator

										#If it is a 3D weka classifier, we just pull the appropriate z-plane from the segmented stack we generated earlier
										else:
											segmentedIDs6.setSlice(stackno)
											resultant = segmentedIDs6.crop()

									#If the user specified to run their own custom script, we execute their code here
									else:
										from JSF_End_User_Made_Code._executor import user_made_code
										resultant = user_made_code(JSF_package.configBasic.ROIseg, ROIimp, IDs6, rm, stackno, 0, 0)

									#'seedChoice' will be true if the user specified they want to do a seeded region growing step in the ROI segmentation
									if JSF_package.configRoi.seedChoice == True:

										#seedIDroi is the channel onto which the seeds, generated by the prior step, are applied
										seedIDroi.setSlice(stackno)
										seedIDroi2 = seedIDroi.crop()

										#Run a gaussian blur on the image we apply the seeds to, if specified by user
										gaussVal = JSF_package.configRoi.blurRoi

										if gaussVal != 0:
											IJ.run(seedIDroi2, "Gaussian Blur...", "sigma="+str(gaussVal))

										#Run the seeded region growing step
										animationStack, regionMask = JSF_package.seeded_region_growing.seededRegionGrowing(seedIDroi2, resultant, 1, iHeight, rm, minSeedSize, invertSeed)
										resultant = regionMask.duplicate()

									#This is the optional 'post-processing' step that can be specified by the users
									if JSF_package.configRoi.roiPost != "None" and JSF_package.configBasic.ROIseg.endswith(".model"):

										if JSF_package.configRoi.roiPost == "Despeckle":
											IJ.run(resultant, "Despeckle", "")
										elif JSF_package.configBasic.cloneSeg.find("_3D_.") == -1:
											from JSF_End_User_Made_Code._executor import user_made_code
											resultant = user_made_code(JSF_package.configRoi.roiPost, resultant, IDs, rm, stackno, pouch, excludinator)
										else:
											from JSF_End_User_Made_Code._executor import user_made_code
											resultant = user_made_code(JSF_package.configRoi.roiPost, resultant, segmentedIDs6, rm, stackno, pouch, excludinator)
											

									startW = 1


									#So here, we get a mask of the segmented ROIs. This allows us to apply seeds to them for seeded region growing.
									#This may be a bit confusing, so this is what we used in example 3 to use the GFP nuclei to identify the whole region as a given genotype
									if JSF_package.configCloneSeg.seedChannelClones == "ROI Mask" and JSF_package.configCloneSeg.seedChoiceClones == True:
										roiMask = resultant.duplicate()
										roiMask.getProcessor().setThreshold(1,255,0)
										IJ.run(roiMask,"Convert to Mask", "")
										IJ.run(roiMask, "Canvas Size...", "width="+str(iWidth)+" height="+str(iHeight+100)+" position=Top-Left zero")
										mask_confirmer(iHeight + 20, roiMask)
										IJ.run(roiMask, "Select None", "")
										IJ.run(roiMask, "Canvas Size...", "width="+str(iWidth)+" height="+str(iHeight)+" position=Top-Left zero")

										if JSF_package.configRoi.roiPost == "Despeckle":
											IJ.run(roiMask, "Erode", "")
											IJ.run(roiMask, "Erode", "")
											IJ.run(roiMask, "Despeckle", "")
											IJ.run(roiMask, "Dilate", "")
											IJ.run(roiMask, "Dilate", "")

										from ij.plugin import RGBStackMerge

										concat = RGBStackMerge()
										newImpMask = [IDs, IDs6]
										seedIDclone = concat.mergeChannels(newImpMask, True)
										IJ.run(seedIDclone, "RGB Color", "slices")

									if JSF_package.configRoi.halfHalfNC == True:
										startW = 2
										excludinateToBe = resultant.duplicate()
										excludinateToBe.getProcessor().setThreshold(1,1,0)
										IJ.run(excludinateToBe,"Convert to Mask", "")
										IJ.run(excludinateToBe, "Canvas Size...", "width="+str(iWidth)+" height="+str(iHeight+100)+" position=Top-Left zero")
										mask_confirmer(iHeight + 20, excludinateToBe)
										IJ.run(excludinateToBe, "Create Selection", "")
										excludinator = ShapeRoi(excludinateToBe.getRoi())


									try:
										resultant.getProcessor().setThreshold(startW, 255, 0)
										IJ.run(resultant,"Convert to Mask", "")
										IJ.run(resultant, "Canvas Size...", "width="+str(iWidth)+" height="+str(iHeight+100)+" position=Top-Left zero")
										mask_confirmer(iHeight + 20, resultant)
										

										
										###test
										ROIholeDiameter = 1100
										if ROIholeDiameter > 0:
									
											IJ.run(resultant, "Canvas Size...", "width="+str(iWidth)+" height="+str(iHeight)+" position=Top-Left zero")
									
											#Here we use analyze particles to get rid of extraneous roi objects below a certain size
											IJ.run(resultant, "Select None", "")
											#Convert pixel to micron units
											cal = resultant.getCalibration()
											caliber = cal.pixelHeight
											
											#Here we use analyze particles to get rid of extraneous roi objects below a certain size
											IJ.run(resultant, "Select None", "")
											#IJ.run(resultant, "Invert", "")
											manager = RoiManager(True)
											
											from ij.plugin.filter import ParticleAnalyzer
											from ij.measure import ResultsTable, Measurements
													
											#This step is a further hole filling step. We invert the image, run analyze particles to identify all holes in the mask.	
											IJ.run(resultant, "Invert", "")
											manager = RoiManager(True)
											ParticleAnalyzer.setRoiManager(manager) 
											pa = ParticleAnalyzer(ParticleAnalyzer.ADD_TO_MANAGER | ParticleAnalyzer.SHOW_NONE,Measurements.AREA,ResultsTable(), 0, ROIholeDiameter/(caliber*caliber), 0.0, 1.0)
											pa.setHideOutputImage(True)
											pa.analyze(resultant)
											rm.runCommand(resultant,"Show None")
											IJ.run(resultant, "Select None", "")
											IJ.run(resultant, "Invert", "")	
										
											#We take all these ROIs and clear them, essentially deleting the holes because the image is an inverting LUT.
											manager.runCommand("Deselect")
											
											IJ.log("Analyze particles step to clear holes")
											aROIs = manager.getSelectedRoisAsArray()
											manager.reset()
											if aROIs:
												op = ShapeRoi(aROIs[0])
												for rp in aROIs:
													rp = ShapeRoi(rp)
													op.or(rp)
												resultant.setRoi(op)
												IJ.run(resultant, "Clear", "")
												
											rm.getInstance()
									
										
										minRoiSize = float(minRoiSize)
										if minRoiSize > 0:
											IJ.run(resultant, "Canvas Size...", "width="+str(iWidth)+" height="+str(iHeight)+" position=Top-Left zero")
											
											
											#Here we use analyze particles to get rid of extraneous roi objects below a certain size
											IJ.run(resultant, "Select None", "")
											#IJ.run(resultant, "Invert", "")
											manager = RoiManager(True)
											
											from ij.plugin.filter import ParticleAnalyzer
											from ij.measure import ResultsTable, Measurements
	
											ParticleAnalyzer.setRoiManager(manager) 
											pa = ParticleAnalyzer(ParticleAnalyzer.ADD_TO_MANAGER | ParticleAnalyzer.SHOW_NONE,Measurements.AREA,ResultsTable(), 0, minRoiSize/(caliber*caliber), 0.0, 1.0)
											pa.setHideOutputImage(True)
											pa.analyze(resultant)
											rm.runCommand(resultant,"Show None")
											IJ.run(resultant, "Select None", "")
											IJ.run(resultant, "Invert", "")	
										
											#We take all these ROIs and clear them, essentially deleting the holes because the image is an inverting LUT.
											manager.runCommand("Deselect")
											
											IJ.log("Analyze particles step to clear holes")
											aROIs = manager.getSelectedRoisAsArray()
											manager.reset()
											del manager
											rm = RoiManager().getInstance()
											
											if aROIs:
												op = ShapeRoi(aROIs[0])
												for rp in aROIs:
													rp = ShapeRoi(rp)
													op.or(rp)
												resultant.setRoi(op)
												IJ.run(resultant, "Clear", "")
											
											IJ.run(resultant, "Select None", "")
											IJ.run(resultant, "Invert", "")
											IJ.run(resultant, "Canvas Size...", "width="+str(iWidth)+" height="+str(iHeight+100)+" position=Top-Left zero")
										
										
										
										IJ.run(resultant, "Create Selection", "")
										pouch = ShapeRoi(resultant.getRoi())
										
	
										
									except:
										pouch = ShapeRoi(1,1, Rectangle(0,0,1,1))
										
								
								resultant = None
								pouchArray = pouchArray + [pouch]
								
								#Run clone analysis. For a full accounting of these variables, see the clone_analysis function in the clone_analysis.py file
								#Roido is the composite clones ROI for a given z plane for use in single cell tracking
								print ("Starting clone analysis")
								
								Title, genotypesImpArray, noClones, caliber, sliceROIs, borderArray, cloneMask, borderMask, cloneImp, cloneBorderImp, Roido, genotypeNames, fullCloneROIArray, rm, wekaSkipImage = JSF_package.clone_analysis.clone_analysis(IDs, Title, pouch, excludinator, stackno, iHeight, rm, sliceROIs, borderArray, IDs3, cellROIs, numGenotypes, timepoint, colorArray, seedIDclone, segmentedIDs, genotypeNames, roiMask, iWidth, deluxeCell, fullCloneROIArray)
								print ("Clone analysis run")
								
								#Store all the images we created for the output panel in arrays
								cloneMaskArray.extend([cloneImp.duplicate()])
								cloneImpArray.extend([cloneImp.duplicate()])
								del cloneImp
								
								borderMaskArray.extend([borderMask.duplicate()])
								del borderMask
			
								cloneBorderArray.extend([cloneBorderImp.duplicate()])
								del cloneBorderImp
								


								IJ.log("Clone analysis completed successfully")


								#Get the fluorescence channel if prompted by the user
								if JSF_package.configBasic.fluoChoice == 1:
									IDs3.setSlice(stackno)
									fluoImp = IDs3.crop()
									IJ.run(fluoImp, "RGB Color", "")
									fluoImpArray.extend([fluoImp])

								#Run the individual clone analysis if prompted by user, and store the images
								if JSF_package.configBasic.cloneTracking == 1 and noClones == 0:

									IJ.log("Clone segmentation initiated")
									print("Clone segmentation initiated")
									#cloneROIs is just a two dimensional list containing the indices of all the segmented single clone ROIs that have been added to the ROI manager.
									#It takes the format of [ [indices of all individual ROIs at one Z-level], [inidices of all the individual ROIs at the next Z-level], [etc.] ]
									cloneROIs, rm = JSF_package.clone_analysis.clone_segmentor(genotypesImpArray, rm, Title, numImages, stackno, pouch, caliber, cloneROIs, timepoint, excludinator)
									cloneTrackingImp = IJ.createImage("CloneTracking_Z:"+str(stackno), "RGB black", iWidth, iHeight, 1)
									cloneTrackingImp.setRoi(pouch)
									IJ.setForegroundColor(255,255,255)
									IJ.run("Line Width...", "line=4")
									IJ.run(cloneTrackingImp, "Draw", "")
									if JSF_package.configRoi.halfHalfNC == True:
										IJ.setForegroundColor(255,255,0)
										cloneTrackingImp.setRoi(excludinator)
										IJ.run(cloneTrackingImp, "Draw", "")

									IJ.log("Clone segmentation completed successfully")
									print("Clone segmentation completed successfully")
									
									#Add headers/legend to output image
									cloneTrackingImp.getProcessor().drawString("Clones are color coded, fill = border, outline = center",20, 20, Color.blue)
									if JSF_package.configBasic.dcp1Choice == 1:
										cloneTrackingImp.getProcessor().drawString("Caspase in clones. Halo identifies clone assignment",20, 40, Color.white)
										cloneTrackingImp.getProcessor().drawString("Green Halo = caspase in center",20, 60, Color.green)
										cloneTrackingImp.getProcessor().drawString("Red Halo = caspase in border",20, 80, Color.red)
									cloneTrackingArray.extend([cloneTrackingImp])

								#Here, we run the cell segmentation and tracking if prompted by the user. This analysis only works with membrane-tagged GFP
								if (JSF_package.configBasic.cloneSeg != "Cytosolic GFP") and (JSF_package.configCloneSeg.winwo == 0) and (JSF_package.configBasic.singleCellMethod != "Disabled"):

									#cellROIs takes the exact same format as cloneROIs, only it has the indices corresponding to the centroids of single cell segmented ROIs
									IJ.log("Single-cell segmentation initiated")
									print("Single-cell segmentation initiated")
									
									cellROIs = JSF_package.cell_tracking.cell_tracking(cellROIs, IDs, stackno, Roido, rm, iHeight, rt, IDs5, DAPIfilter, seedIDcell, pouch, excludinator, segmentedIDsCC, wekaSkipImage)
									wekaSkipImage = None

									IJ.log("Single-cell segmentation completed successfully")
									print("Single-cell segmentation completed successfully")

								#Run the foci analysis if prompted by user, and store the images
								if JSF_package.configBasic.dcp1Choice == 1:

									IJ.log("Apoptosis analysis initiated")
									print("Apoptosis analysis initiated")

									sliceROIs, casMask, casImp, casCasImp, casRefArray = JSF_package.caspase_analysis.dcp1_analysis(pouch, IDs2, Title, stackno, iHeight, rm, sliceROIs, casRefArray, numGenotypes, timepoint, caspaseSegment, excludinator, seedIDcas, segmentedIDs2, iWidth)
									casMaskArray.extend([casMask])
									casImpArray.extend([casImp])
									casCasArray.extend([casCasImp])
									
									IJ.log("Apoptosis analysis completed successfully")
									print("Apoptosis analysis completed successfully")


								#Run the speckle analysis, store the images
								if JSF_package.configBasic.speckle == 1:

									IJ.log("Speckle analysis initiated")
									print("Speckle analysis initiated")

									#For a full accounting of these variables, see the speckleAnalysis function in the speckles_analysis.py file
									refOutImp, refStandImp, sliceROIs = JSF_package.speckles_analysis.speckleAnalysis(IDs4, stackno, iHeight, rm, sliceROIs, pouch, timepoint, numGenotypes, excludinator)

									refOutArray.extend([refOutImp])
									refBaseArray.extend([refStandImp])
									refoImp = refStandImp.duplicate()
									refStandArray.extend([refoImp])

									IJ.log("Speckle analysis completed successfully")
									print("Speckle analysis completed successfully")


								#Quit the script if the user has pressed escape at any point in the loop
								if IJ.escapePressed() == 1:
									IJ.log("Process aborted by user")
									IJ.error("Process cancelled! \n \n"+ "So long for now, partner!")
									return


								#Move up to the next Z-slice for the next loop!
								stackno = stackno + 1

							#####################################   We have now exited the loop for the individual Z-plane!         #####################################################



							trackingArray = list() #This array will hold all of our tracking arrays for dcp1, clones, and individual cells. If we do not do a particular tracking, that place will simply be a string that says 'Skip'

							# Here we run through our various individual caspase ROIs and link them together into individual caspase positive foci based on centroid-tracking
							if JSF_package.configBasic.dcp1Choice == 1 and JSF_package.configBasic.dcp1Counting == 1:

								#caspaseSegment is a 2D array like cellROIs and cloneROIs.
								#We convert this into totalCaspaseAssignments, which is now another 2D array of the format [ [ Indices of all rois in the manager corresponding to an individual caspase foci ], [ next caspase foci ] ]

								IJ.log("Foci tracking initiated")
								print("Foci tracking initiated")

								genCounter = 1
								caspaseSegment = [caspaseSegment]
								count_limit = numGenotypes

								if JSF_package.configBasic.cloneSeg != "Membrane-tagged GFP" and JSF_package.configBasic.cloneSeg != "Cytosolic GFP" and JSF_package.configBasic.cloneSeg != 'Use ROIs As Clones' and JSF_package.configRoi.halfHalfNC == True:
									count_limit = count_limit + 1

								while genCounter < count_limit:
									genCounter += 1
									caspaseSegment = caspaseSegment + caspaseSegment


								totalCaspaseAssignments = JSF_package.tracking_and_outputs.tracker(caspaseSegment, rm, 2)
								trackingArray.extend([totalCaspaseAssignments])



								IJ.log("Foci tracking completed successfully")
								print("Foci tracking completed successfully")


							else:
								trackingArray.extend(["Skip"])

							#clone tracking
							if JSF_package.configBasic.cloneTracking == 1:

								IJ.log("Clone tracking initiated")
								print("Clone tracking initiated")

								#totalCloneAssignments is an array like totalCaspaseAssignments
								supersetCloneAssignments = JSF_package.tracking_and_outputs.tracker(cloneROIs, rm, 1)
								trackingArray.extend([supersetCloneAssignments])

								IJ.log("Clone tracking completed successfully")
								print("Clone tracking completed successfully")

							else:
								trackingArray.extend(["Skip"])

							if JSF_package.configBasic.cellCount == 1:

								IJ.log("Single-cell tracking initiated")
								print("Single-cell tracking initiated")

								#totalCellAssignments is an array like totalCaspaseAssignments
								cellROIs = [cellROIs]
								genCounter = 1
								while genCounter < numGenotypes:
									genCounter += 1
									cellROIs = cellROIs + cellROIs
								totalCellAssignments = JSF_package.tracking_and_outputs.tracker(cellROIs, rm, 3)

								trackingArray.extend([totalCellAssignments])

								IJ.log("Single-cell tracking completed successfully")
								print("Single-cell tracking completed successfully")

							else:
								trackingArray.extend(["Skip"])


							#we do our Measurements in this function. It's complicated, so check the file in JSF_package in Jars\Lib

							iHeight = iHeightArchive

							IJ.log("Whole disc measurements initiated")
							print("Whole disc measurements initiated")
							LoLa, LoLb = JSF_package.tracking_and_outputs.whole_disc_measurements(IDs, IDs3, numGenotypes, pouch, sliceROIs, casCasArray, casMaskArray, refStandArray, refOutArray, rm, rt, iHeight, zStart, names, Title, pouchArray, pouch2Array, colorArray, genotypeNames, excludinator, iWidth, IDs4)
							IJ.log("Whole disc measurements completed successfully. Tracking measurements initiated")
							print("Whole disc measurements completed successfully. Tracking measurements initiated")
							outImp, outClImp, cloneMaskStack, cloneLoLa, cloneLoLb, LoLc, rtDeluxe, outImpSC = JSF_package.tracking_and_outputs.tracking_measurements(IDs, IDs3,trackingArray, casMaskArray, cloneMaskArray, iHeight, numGenotypes, borderArray, rm, zStart, cloneTrackingArray, names, Title, rt, pouchHeight, casRefArray, cloneImpArray, colorArray, pouchArray, pouch2Array, genotypeNames, pouch, excludinator, iWidth, timepoint, rtDeluxe, deluxeCell, fullCloneROIArray)
							IJ.log("Tracking measurements completed successfully. Whole disc results table generation initiated")
							print("Tracking measurements completed successfully. Whole disc results table generation initiated")
							rtD = JSF_package.tracking_and_outputs.create_whole_disc_table(rtD, LoLa, LoLb, LoLc, timepoint)
							IJ.log("Whole disc results table generation completed successfully")
							print("Whole disc results table generation completed successfully")
							rtD.show("Disc Analysis")
							rtS = JSF_package.tracking_and_outputs.create_summary_table(rtS, LoLa, LoLb, LoLc, timepoint)

							#flush big variables
							LoLa = LoLb = LoLc = sliceROIs = None



							IJ.log("Whole disc results table generation completed successfully")
							print("Whole disc results table generation completed successfully")
							rtS.show("Summary Table")
							if JSF_package.configBasic.cloneTracking == 1:
								IJ.log("Clone tracking results table generation initiated")
								print("Clone tracking results table generation initiated")
								rtC = JSF_package.tracking_and_outputs.create_clone_table(rtC, cloneLoLa, cloneLoLb, timepoint)
								rtC.show("Clone Analysis")

								#Flush big variables
								cloneLoLa = cloneLoLb = None
								IJ.log("Clone tracking results table generation completed successfully")
								print("Clone tracking results table generation completed successfully")
							timepoint = timepoint + 1


							#make our composite image
							IJ.log("Composite output image generation initiated")
							print("Composite output image generation initiated")
							#imp is just the imagePlus of our display panel
							if generateImage == True:
								imp = JSF_package.tracking_and_outputs.image_generator(cloneMaskStack, borderMaskArray, casMaskArray, cloneImpArray, fluoImpArray, casImpArray, iHeight, casCasArray, cloneBorderArray, cloneTrackingArray, outImp, outClImp, refBaseArray, refOutArray, refStandArray, iWidth, outImpSC, deluxeCell, calibration)


								if JSF_package.configBasic.timelapse == True:

									timeLapseImageArray.extend([imp])

								IJ.log("Composite output image generation completed successfully")
								print("Composite output image generation completed successfully")

							#Flush image stacks now that they've been used
							cloneMaskStack = borderMaskArray = casMaskArray = cloneImpArray = fluoImpArray = casImpArray = casCasArray = cloneBorderArray = cloneTrackingArray = outImp = outClImp = refBaseArray = refOutArray = refStandArray = None


							#Prepare the destination in which we save the image
							savName = names.replace("/", '')


							path = os.path.join(dir2,savName)
							savTitle = Title.replace("/", "")
							path = path + str(savTitle)+"--Processed Stack"



							#Here we save the output images
							if JSF_package.configSave.saveChoice == 1 and JSF_package.configBasic.timelapse == False and generateImage == True:
								if int(JSF_package.configSave.imageSampler) > 1:
									if (int(goodImage) % int(JSF_package.configSave.imageSampler)) == 0:
										IJ.saveAs(imp, "Tiff", path)
								else:
									IJ.saveAs(imp, "Tiff", path)


							#Dump our composite image

							#display panel if prompted by user
							if generateImage == True:
								if JSF_package.configSave.visualize == 1:
									impVis.hide()
									impVis = imp
								elif JSF_package.configBasic.timelapse == True:
									print 1
								else:
									imp.flush()

							#Here we save our csv's after each 10 discs. This keeps our memory from crashing
							isSaved = False

							if (goodImage > 0) and (goodImage%csvSaver == 0):

								pathR = dir2
								namePath = dir1name
								pathR = pathR.replace(".tif", "")
								pathR = pathR.replace(".lif", "")

								savTitle  = Title.replace("/", "")

								suffixD =  "whole disc analysis-- "+str(savTitle)+"_"+str(csvCount)+".csv"
								suffixC = "clone tracking analysis-- "+str(savTitle)+"_"+str(csvCount)+".csv"
								suffixDeluxe = "single cell tracking analysis-- "+str(savTitle)+"_"+str(csvCount)+".csv"
								deluxeTable = os.path.join(pathR, suffixDeluxe)
								discTable = os.path.join(pathR, suffixD)
								cloneTable = os.path.join(pathR, suffixC)
								x = WindowManager.getWindow("Disc Analysis")
								WindowManager.setWindow(x)

								saveLooper = True
								counter = 1
								while saveLooper == True and counter <= 5:
									saveLooper = False
									try:
										IJ.saveAs("Results", discTable)
									except:
										x = WindowManager.getWindow(suffixD)
										WindowManager.setWindow(x)
										suffixD =  "whole disc analysis-- "+str(savTitle)+"_"+str(csvCount)+"("+str(counter)+")"+".csv"
										discTable = os.path.join(pathR, suffixD)
										saveLooper = True
										if (counter == 5):
											IJ.log("Could not save wing disc results table: "+ str(discTable))
									counter += 1
								if deluxeCell==True:
									x =WindowManager.getWindow("Deluxe Cell Counting Results")
									WindowManager.setWindow(x)

									saveLooper = True
									counter = 1


									while saveLooper == True and counter <= 5:
										saveLooper = False
										try:
											x =WindowManager.getWindow("Deluxe Cell Counting Results")
											WindowManager.setWindow(x)
											IJ.saveAs("Results", deluxeTable)
										except:
											x = WindowManager.getWindow(suffixDeluxe)
											WindowManager.setWindow(x)
											suffixDeluxe = "single cell tracking analysis-- "+str(savTitle)+"_"+str(csvCount)+".csv"
											deluxeTable = os.path.join(pathR, suffixDeluxe)

											saveLooper = True
											if (counter == 5):
												IJ.log("Could not save single cell results table: "+ str(cloneTable))
										counter += 1
									IJ.selectWindow(suffixDeluxe)
									IJ.run("Close")

								if JSF_package.configBasic.cloneTracking == 1:
									x =WindowManager.getWindow("Clone Analysis")
									WindowManager.setWindow(x)

									saveLooper = True
									counter = 1
									while saveLooper == True and counter <= 5:
										saveLooper = False
										try:
											x =WindowManager.getWindow("Clone Analysis")
											WindowManager.setWindow(x)
											IJ.saveAs("Results", cloneTable)
										except:
											x = WindowManager.getWindow(suffixC)
											WindowManager.setWindow(x)
											suffixC = "clone tracking analysis-- "+str(savTitle)+"_"+str(csvCount)+"("+str(counter)+")"+".csv"
											cloneTable = os.path.join(pathR, suffixC)

											saveLooper = True
											if (counter == 5):
												IJ.log("Could not save wing disc results table: "+ str(cloneTable))
										counter += 1

									IJ.selectWindow(suffixC)
									IJ.run("Close")


								IJ.selectWindow(suffixD)
								IJ.run("Close")
								csvCount = csvCount + 1
								IJ.log("Batch of results from Z-stacks saved")
								print("Batch of results from Z-stacks saved")
								isSaved = True

							selections = 1
							goodImage = goodImage +1
							
							gc.collect()
							IJ.run("Collect Garbage")



						#Add 1 to full loop counter once all the analysis of a given image is done. This is used to pick the starting and ending slice from the user selections
						fullLoopCounter = fullLoopCounter + 1

						#Here we save the output images
						if JSF_package.configSave.saveChoice == 1 and JSF_package.configBasic.timelapse == True and generateImage == True:

							IJ.log("Timelapse composite image generation started")
							print("Timelapse composite image generation started")

							#Combine all images in time-lapse into one 4d stack
							impTL = timeLapseImageArray[0]

							idex = 1
							while idex < len(timeLapseImageArray):
								impTL2 = timeLapseImageArray[idex]
								impTL = Concatenator.run(impTL, impTL2)
								idex += 1

							IJ.log("Timelapse composite image generation completed successfully")
							print("Timelapse composite image generation completed successfully")

							if int(JSF_package.configSave.imageSampler) > 1:
								if (int(goodImage) % int(JSF_package.configSave.imageSampler)) == 0:
									IJ.saveAs(impTL, "Tiff", path)

							else:
								IJ.saveAs(impTL, "Tiff", path)


					except:
						try:
							Title = str(Title)
						except:
							Title = "No Title"
						Title = str(Title)
						if Title.endswith(".txt"):
							IJ.log("text file skipped: "+ Title)
						elif Title.endswith(".zip"):
							IJ.log("zip file skipped: "+ Title)
						elif Title.endswith(".py"):
							IJ.log("python file skipped: "+ Title)
						elif Title.endswith(".model"):
							IJ.log("weka model file skipped: "+ Title)
						else:
							errCount = errCount + 1
							errStr = errStr + "[image:" + Title +"]"
						continue

			else:
				continue

			#Here we save our results, if they have not been saved already

			#Check if there are any unsaved results
			windows =  ij.WindowManager.getNonImageTitles()
			openning = False
			for item in windows:
				if item == "Disc Analysis":
					openning = True

			#If yes, save them to the output directory
			if openning == True:
				pathR = dir2
				namePath = dir1name
				pathR = pathR.replace(".tif", "")
				pathR = pathR.replace(".lif", "")
				savTitle = Title.replace("/", "")
				suffixD =  "whole disc analysis-- "+str(savTitle)+"_"+str(csvCount)+".csv"
				suffixC = "clone tracking analysis-- "+str(savTitle)+"_"+str(csvCount)+".csv"
				suffixDeluxe = "single cell tracking analysis-- "+str(savTitle)+"_"+str(csvCount)+".csv"
				discTable = os.path.join(pathR, suffixD)
				cloneTable = os.path.join(pathR, suffixC)
				deluxeTable = os.path.join(pathR, suffixDeluxe)
				x = WindowManager.getWindow("Disc Analysis")
				WindowManager.setWindow(x)
				IJ.saveAs("Results", discTable)
				x = WindowManager.getWindow(suffixD)
				WindowManager.setWindow(x)
				IJ.run("Close")


				if JSF_package.configBasic.cloneTracking == 1:
					x =WindowManager.getWindow("Clone Analysis")
					WindowManager.setWindow(x)
					IJ.saveAs("Results", cloneTable)
					x = WindowManager.getWindow(suffixC)
					WindowManager.setWindow(x)
					IJ.run("Close")

				if deluxeCell==True:
					x =WindowManager.getWindow("Deluxe Cell Counting Results")
					WindowManager.setWindow(x)
					IJ.saveAs("Results", deluxeTable)
					x = WindowManager.getWindow(suffixDeluxe)
					WindowManager.setWindow(x)
					IJ.run("Close")


				IJ.log("Final batch of results saved")
				print("Final batch of results saved")


		except:

			try:
				Title = str(Title)
			except:
				Title = "No Title"
			if Title.endswith(".txt"):
				IJ.log("text file skipped: "+ Title)
			elif Title.endswith(".zip"):
				IJ.log("zip file skipped: "+ Title)
			elif Title.endswith(".py"):
				IJ.log("python file skipped: "+ Title)
			elif Title.endswith(".model"):
				IJ.log("weka model file skipped: "+ Title)
			continue

		# show display panel if prompted
		if JSF_package.configSave.visualize == 1 and generateImage == True:
			impVis.show()


	#Here, we abort the analysis if there are no good images in the specified folder for analysis
	if anyImages == False:
		IJ.log("No images of correct format in specified folder. Analysis aborted")
		IJ.error("There are no images suitable for anlaysis in this folder. \n \n"+ "Did you accidentally select the wrong folder? \n \n" + "Try again, but make sure you select a folder that contains .lif or .tif images in it!")
		return

	#If there are no user-specified ROIs in the folder, we abort the analysis
	if validSelections == False:
		IJ.log("No ROI selections were found in this folder. Analysis aborted")
		IJ.error("There are no ROI selections in this folder. \n\n"+ "Either you haven't made selections before, or your '--ROIs.zip' and '--Selections.txt' files have been misplaced. \n \n" +"Try re-doing the analysis, but leave the 'Restore ROIs' checkbox unticked.")
		return

	#We save the summary output table
	if goodImage > 0:
		pathR = dir2
		sumTable = os.path.join(pathR, "summary table.csv")
		x =WindowManager.getWindow("Summary Table")
		WindowManager.setWindow(x)
		suffixD = "summary table.csv"

		saveLooper = True
		counter = 1
		while saveLooper == True and counter <= 5:
			saveLooper = False
			try:
				x = WindowManager.getWindow("Summary Table")
				WindowManager.setWindow(x)
				IJ.saveAs("Results", sumTable)

			except:
				x = WindowManager.getWindow(suffixD)
				WindowManager.setWindow(x)
				suffixD =  "summary table"+"("+str(counter)+")"+".csv"
				sumTable = os.path.join(pathR, suffixD)
				saveLooper = True
				if (counter == 5):
					IJ.log("Could not save summary results table: "+ str(sumTable))
			counter += 1

	# if there were no errors, save everything
	if errCount == 0:
		IJ.showMessage("All done! No errors detected.")
		IJ.log("Analysis complete. No errors detected.")
	else:
		IJ.showMessage("All done! " + str(errCount) + " errors were detected. The following could not be processed: " + errStr+ ".")
		IJ.log("Analysis complete. "+ str(errCount) + " errors were detected. The following could not be processed: " + errStr+ ".")
		savTitle = Title.replace("/", "")
		txtPath = os.path.join(dir2, savTitle + "--Unprocessed.txt")
		outFile = open(txtPath, 'w')
		output = "_"+errStr
		outFile.write(output)
		outFile.close()
	return


#Actually run the script
if __name__ in ['__builtin__', '__main__']:
    run_script()
