#
#
#
# This module contains all the functions for getting the macro going, like finding the parameters, making ROI selections, and retrieving old ROI selections
#
#
#



########
# This prompts the user to select an input folder and defines the output folder. If one does not exist, it creates one
def folder_selection():

	import os
	from ij import IJ
	
	#Prompt user to specify correct file
	dir1 = IJ.getDirectory("Choose folder with lif files ")

	#Extract a list of all files in the specified directory
	filelist = os.listdir(dir1)

	#Identify the output folder based on the input folder
	dir1parent = os.path.dirname(os.path.dirname(dir1))
	dir1name = os.path.basename(os.path.dirname(dir1))
	dir2 = os.path.join(dir1parent, dir1name + "--Processed Results")

	#If the output folder does not exist, create it
	if os.path.exists(dir2) == False:
		os.makedirs(dir2)

	return dir1, filelist, dir2, dir1name

#############
# This function loops through all images and prompts the user to select the pouch ROI. A should equal the list of names in the file, b should be the directory
def ROI_selection(filelist, dir1, rm):

	import ij
	from ij import IJ
	from loci.plugins import BF
	from loci.plugins.in import ImporterOptions
	from JSF_package import configBasic as cfg
	from JSF_package import configCloneSeg as cfgCS
	import JSF_package
	from JSF_package import _misc_, caspase_analysis, cell_tracking, clone_analysis, configBasic, configCellTrack, configCloneSeg, configDeathSeg, configDeathTrack, configRoi, configSave, seeded_region_growing, speckles_analysis, start_up, tracking_and_outputs, user_inputs_GUI
	

	selections = 0
	selectionsHoldover = 0

	# We loop through all files in the folder
	for names in filelist:
		
		#Set a path specific to this file
		path = dir1 + names
		
		numImages = 0 #This variable counts the number of images we actually want to process
		skipString = "" #This tracks all the images we want to skip
		sliceTracker = "" #This tracks the starting and ending slice and outputs it so we can restore it if we want to
		lifVariable = "" #We use this varible for tracking where an image is in the .lif file
		lifT = 0

		#Open each file if its a lif of tif
		if path.endswith(".zip") == False and path.endswith(".txt") == False:
			options = ImporterOptions()
			options.setColorMode(ImporterOptions.COLOR_MODE_DEFAULT)
			options.setOpenAllSeries(True)
			options.setId(path)
			imps = BF.openImagePlus(options)
	
		else:
			continue

		#Loop through all images in the file

		for imp in imps:

			#Check if the file can be converted to 8bit
			
			try:
				impType = imp.getBitDepth()
				if impType != 8:
					IJ.run(imp, "8-bit", "")
			except:
				IJ.error("Invalid Input: image "+str(imp.getTitle())+" cannot be coerced into an 8-bit image.")


			if IJ.escapePressed() == 1:
				IJ.error("Process Cancelled")
				return

			#Select the active window, determine how many z-slices in the stack
			maxZ = imp.getNSlices() - cfgCS.rollingZ
			lifT = lifT+1

			#If too few slices to process, skip this image
			minSlices = 1+cfgCS.rollingZ+cfgCS.rollingZ
			if imp.getNSlices() < minSlices:
				skipString = skipString + str(imp.getTitle())
				imp.hide()
				imp.flush()
				continue

			#Generate dialog window for user to set slices and ROIs
			looper = 1
			sliceArrayComp = []
			imp.show()
			while looper > 0:

				if IJ.escapePressed() == 1:
					IJ.error("Process Cancelled")
					return
			
				looper = 0

				#Here we make things a bit easier for people who want to analyze the whole image, and select everything for them early to add as an ROI.
				#We clip off a few pixels at the edges to minimize border effects
				if JSF_package.configBasic.ROIseg == "Analyze Entire Image":
					IJ.run(imp, "Select All", "")
					IJ.run(imp, "Enlarge...", "enlarge=-10 pixel")

				#This is a dialog window to guide people through selecting their ROIs and Z-planes.
				gd = ij.gui.NonBlockingGenericDialog('Clone-O-Matic 3000')

				#Here, we modify the text a bit to suit the input parameters, but it's not all that important
				if JSF_package.configBasic.ROIseg != "Analyze Entire Image" and JSF_package.configBasic.ROIseg != "Manual Selections":
					gd.addMessage("Specify Z-planes to analyze")
				else:
					gd.addMessage("Specify ROI and Z-planes to analyze")
					gd.enableYesNoCancel("Finished", "Add ROI for other Z-planes")	
					gd.addMessage("Z-planes already assigned an ROI:"+str(sliceArrayComp))

					if JSF_package.configRoi.halfHalf == True:
						gd.addMessage("Specify region for clonal analysis")
						gd.enableYesNoCancel("Finished", "Add ROI for other Z-planes")
						
					elif JSF_package.configRoi.halfHalfNC == True:
						gd.addMessage("Specify first region for clonal analysis")
						gd.enableYesNoCancel("Finished", "Add ROI for other Z-planes")	

				#Set up the default start and end slices. If the user has the rolling z option, we have to clip off a few slices from analysis
				gd.addNumericField("Starting Slice: ", 1+cfgCS.rollingZ,0)
				gd.addNumericField("Ending Slice: ", maxZ,0)

				gd.setCancelLabel('SKIP')
				gd.showDialog()

				

				if gd.wasOKed() == False and gd.wasCanceled() == False:
					looper = 1
				sliceInitial = gd.getNextNumber() 
				sliceFinal = gd.getNextNumber() 
	
				#Correct for if the sliceInitial or sliceFinal are invalid or out of order
				if sliceInitial > sliceFinal:
					x = sliceFinal
					sliceFinal = sliceInitial
					sliceInitial = x
				if sliceInitial < 1+cfgCS.rollingZ:
					sliceInitial = 1+cfgCS.rollingZ
				if sliceFinal > maxZ:
					sliceFinal = maxZ

				#The user can enter multiple pouch ROIs, but we have to make sure everything is compatible
				#There cannot be overlap in the ROIs and z-planes assigned
				totalZ = range(int(1+cfgCS.rollingZ), int(maxZ)+1)
				checker = range(int(sliceInitial), int(sliceFinal+1))
				breaker = 0
				for item in checker:
					if item in sliceArrayComp:
						IJ.error("You've cannot assign two ROI's to a Z-plane. Try again!")
						looper = 1
						breaker = 1
						break
				if breaker == 1:
					continue

				#Here, we are making sure there are no gaps in the entered z-slices - we cannot have a slice without a pouch ROI!
				sacRestore = sliceArrayComp		
				sliceArrayComp = sliceArrayComp + checker
				sliceArrayComp.sort()
				if gd.wasOKed():
					strstr = ""
					startCheck = sliceArrayComp[0]
					endCheck = sliceArrayComp[-1] + 1
					for item in range(startCheck, endCheck):
						
						check = 0
						for item2 in sliceArrayComp:
							if item == item2:
								check=1
					
						if check == 0:
							strstr = strstr + str(item)
						
					if strstr != "":
						IJ.error("You have gaps in your Z-plane ROI assignments! Please assign an ROI for the Z-planes "+ strstr)
						looper = 1
						sliceArrayComp = sacRestore
						continue
							
					
					
	
				#If the user selected an area and pressed okay, we add this image/roi to the list to be proccessed. Else we skip the image
				x = imp.getRoi()
				if JSF_package.configBasic.ROIseg == "Analyze Entire Image" or JSF_package.configBasic.ROIseg == "Manual Selections":
					if x != None and gd.wasCanceled() == False and JSF_package.configRoi.halfHalf == False and JSF_package.configRoi.halfHalfNC == False:
						rm.runCommand('Add')
						rm.rename((rm.getCount() - 1), imp.getTitle()+"-Pouch[z"+str(sliceInitial)+"-"+str(sliceFinal)+"z]")
					elif x != None and gd.wasCanceled() == False and (JSF_package.configRoi.halfHalf == True or JSF_package.configRoi.halfHalfNC == True):
					
						#Here we prompt the user to select the non-clone compartment if this is a half and half clones experiment
						if JSF_package.configRoi.halfHalf == True or JSF_package.configRoi.halfHalfNC == True:
							IJ.setForegroundColor(255,255,255)
							IJ.run("Line Width...", "line=4")
							IJ.run(imp, "Draw", "stack")
							IJ.setForegroundColor(255,0,0)
							IJ.run("Line Width...", "line=2")
							IJ.run(imp, "Draw", "stack")
							gd2 = ij.gui.NonBlockingGenericDialog('Clone-O-Matic 3000')
							gd2.setCancelLabel('SKIP')
							if JSF_package.configRoi.halfHalf == True:
								gd2.addMessage("Select non-clone reference compartment")
							else:
								gd2.addMessage("Select second region for analysis")
							gd2.showDialog()
							nonClone = imp.getRoi()
							if nonClone != None and gd2.wasOKed() == True:
								rm.addRoi(x)
								rm.addRoi(nonClone)						
								rm.rename((rm.getCount() - 2), imp.getTitle()+"-Pouch[z"+str(sliceInitial)+"-"+str(sliceFinal)+"z]")
								rm.rename((rm.getCount() - 1), imp.getTitle()+"-Non-Clone[z"+str(sliceInitial)+"-"+str(sliceFinal)+"z]")
				
				
				
				#Save the user inputs if they hit 'OK'
				if gd.wasOKed():
					numImages = numImages + 1
					sliceTracker = sliceTracker + str(sliceArrayComp[0]) + "<" + str(sliceArrayComp[-1]) + "<"
					lifVariable = lifVariable + str(lifT) + "<"

				#Save user inputs if they pressed 'add another slice' but have used up all the available z-planes
				elif totalZ == sliceArrayComp and gd.wasCanceled == False:
					looper = 0
					numImages = numImages + 1
					sliceTracker = sliceTracker + str(sliceArrayComp[0]) + "<" + str(sliceArrayComp[-1]) + "<"
					lifVariable = lifVariable + str(lifT) + "<"

				#Record that which images have been 'skipped' so we do not analyze them
				elif gd.wasCanceled == True:
					skipString = skipString + imp.getTitle()	
	
								
								
					
			#Close image as we use them
			imp.hide()
			imp.flush()

		#Save ROIs and skipstring etc. so we only have to have user input once
		if rm.getCount > 0:
			selections = 1
			
			if JSF_package.configBasic.ROIseg == "Analyze Entire Image" or JSF_package.configBasic.ROIseg == "Manual Selections":
				if rm.getCount() > 0:
					roiPath = path[:-4] + "--ROIs.zip"
					rm.runCommand("Select All")
					rm.runCommand("save selected", roiPath)
					rm.reset()
				else:
					selections = 0
			if numImages != 0 and sliceTracker != "" and lifVariable != "":
				txtPath = path[:-4] + "--Selections.txt"
				outFile = open(txtPath, 'w')
				output = "_"+skipString+"#"+str(numImages)+"#"+sliceTracker[:-1] + "#" + lifVariable[:-1]
				outFile.write(output)
				outFile.close()
			else:
				selections = 0

			if selections == 1:
				selectionsHoldover = 1

		selections = selectionsHoldover
			

	return selections
	
##################
# This function retrieves the saved ROI data. a should equal the active file, b should be the directory	
def selection_retrieval(a, b, rm):	

	import os, JSF_package
	from JSF_package import _misc_, caspase_analysis, cell_tracking, clone_analysis, configBasic, configCellTrack, configCloneSeg, configDeathSeg, configDeathTrack, configRoi, configSave, seeded_region_growing, speckles_analysis, start_up, tracking_and_outputs, user_inputs_GUI
	
	inPath = b + a

	#Import rois from zip file to roiManager
	if JSF_package.configBasic.ROIseg == "Manual Selections" or JSF_package.configBasic.ROIseg =="Analyze Entire Image" :
		rm.reset()
		roiPath = inPath[:-4] + "--ROIs.zip"
		if os.path.isfile(roiPath):
			rm.runCommand("Open", roiPath)
		else:
			return "Cancel", "Cancel", "Cancel", "Cancel"
	
	#Import skipstring and slicenumbers from textfile
	txtPath = inPath[:-4] + "--Selections.txt"
	if os.path.isfile(txtPath):
		with open(txtPath, 'r') as myfile:
			data = myfile.read()
		skipString, numImages, slicor, lifT =  data.split("#")
		startEndSlice = slicor.split("<")
		lifVariable = lifT.split("<")
	else:
		return "Cancel", "Cancel", "Cancel", "Cancel"

	return skipString, numImages, startEndSlice, lifVariable