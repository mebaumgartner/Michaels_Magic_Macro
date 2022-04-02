#
#
#
# These are miscellaneous functions that are used in the PECAn software
#
#
#

##############
#This makes sure the binary mask was made in the correct format.
def mask_confirmer(a, b):

	from ij import IJ
	from ij.measure import ResultsTable

	print "Clearing results"
	#Create an ROI in a blank region of the image (created by resizing the canvas)
	IJ.run("Clear Results")
	b.setRoi(1, a, 1, 1)
	print "test ROI set"
	
	#Measure the area and get the maximum pixel value

	IJ.run(b, "Measure", "")
	print "Measuring black value"
	IJ.run(b, "Select None", "")
	print "Clearing selection"
	rt = ResultsTable.getResultsTable()
	print "Extracting results table"
	peepers = rt.getStringValue("Max",0)
	IJ.run("Clear Results")
	print "Results cleared"


	#Since this is a binary image with known parameters, if the Max value is 255, the image needs to be inverted
	if float(peepers)==255:
		IJ.run(b, "Invert", "")

	print "Mask confirmation complete"


###############
#This function determines if an ROI is empty or not. If it is, we replace it with a little placeholder ROI
def selection_confirmer(ROI, iHeight, imp):

	from ij.gui import Plot, ShapeRoi

	#Initialize variables and make sure our ROI is in the correct format
	if not ROI:
		#If the ROI is empty, we replace it with a placeholder ROI and return an "<empty>" string that we can use to later identify it
		imp.setRoi(5,iHeight+20,1,1)
		ROI = imp.getRoi()
		ROI = ShapeRoi(ROI)
		emptyRoi = "<empty>"

		return ROI, emptyRoi


	if ROI:

		emptyRoi = ""
		ROI = ShapeRoi(ROI)

		#Determinining if an ROI is empty is actually tricky. How I do it is by getting the string (which has the general details), and finding
		#What it's width is. If the width of the ROI is zero, it is empty for our purposes
		x = str(ROI).find("width")
		x = str(ROI)[x+6:x+7]
		if x == "0":

			#If the ROI is empty, we replace it with a placeholder ROI and return an "<empty>" string that we can use to later identify it
			imp.setRoi(5,iHeight+20,1,1)
			ROI = imp.getRoi()
			ROI = ShapeRoi(ROI)
			emptyRoi = "<empty>"

	else:

			#If the ROI is empty, we replace it with a placeholder ROI and return an "<empty>" string that we can use to later identify it
			imp.setRoi(5,iHeight+20,1,1)
			ROI = imp.getRoi()
			ROI = ShapeRoi(ROI)
			emptyRoi = "<empty>"
	return ROI, emptyRoi

############
#This function opens just  the channel of the series we want and keeps it as a stack.
def channel_open(file_path, s, c):

	from loci.plugins import BF
	from loci.plugins.in import ImporterOptions
	from ij import IJ

	s = s-1
	c = c-1
	options = ImporterOptions()
	options.setId(file_path)
	options.setColorMode(ImporterOptions.COLOR_MODE_GRAYSCALE)
	options.setCBegin(s, c)
	options.setCEnd(s,c)
	options.setSeriesOn(s,True)
	imps = BF.openImagePlus(options)
	for imp in imps:


		return imp

###########
#This function is used to open all the channels we need without creating duplicates

def channel_organize_and_open(indices, inPath, timeFinish, timepoint ):

	from JSF_package._misc_ import channel_open
	import JSF_package
	from JSF_package import _misc_, caspase_analysis, cell_tracking, clone_analysis, configBasic, configCellTrack, configCloneSeg, configDeathSeg, configDeathTrack, configRoi, configSave, seeded_region_growing, speckles_analysis, start_up, tracking_and_outputs, user_inputs_GUI
	from JSF_package.configBasic import ROIchannel, preProcess, cloneChannel, timelapse, speckleChannel, DAPIchannel, cellCountChannel, dcp1Channel, fluoChannel, fluoChoice, speckle, ROIseg, singleCellMethod, cloneSeg, cellDeathSegMethod
	from JSF_package.configCloneSeg import seedChannelClones, seedChoiceClones
	from JSF_package.configCellTrack import seedChannelCell, seedChoiceCell
	from JSF_package.configDeathSeg import seedChoiceCas, seedChannelCas
	from JSF_package.configRoi import seedChannel, seedChoice
	from ij import ImageStack, ImagePlus, IJ

	primaryChannels = []
	secondaryChannels = []



	#Load the clone channel
	primaryChannels = primaryChannels + [cloneChannel]
	if seedChoiceClones == True and seedChannelClones != "ROI Mask":
		secondaryChannels = secondaryChannels  + [int(seedChannelClones)]

	#Load the cell death segmentation channel
	if cellDeathSegMethod != "Disabled":
		primaryChannels = primaryChannels + [dcp1Channel]
		if seedChoiceCas == True:
			secondaryChannels = secondaryChannels  + [int(seedChannelCas)]

	#Load the speckle channel
	if speckle == True:
		secondaryChannels = secondaryChannels + [int(speckleChannel)]

	#Load the fluorescence channel
	if fluoChoice == True:
#		if preProcess == "None":
#			secondaryChannels = secondaryChannels + [int(fluoChannel)]
#		else:
		primaryChannels = primaryChannels + [fluoChannel]

	#Load the ROI channel
	if ROIseg != "Manual Selections" and ROIseg != "Analyze Entire Image":
		primaryChannels = primaryChannels + [ROIchannel]
		if seedChoice == True:
			secondaryChannels = secondaryChannels + [int(seedChannel)]

	#Load the single cell segmentation channel
	if singleCellMethod != "Disabled":
		primaryChannels = primaryChannels + [cellCountChannel]
		if seedChoiceCell == True:
			secondaryChannels = secondaryChannels  + [int(seedChannelCell)]
		if DAPIchannel != "Disabled":
			secondaryChannels = secondaryChannels + [int(DAPIchannel)]


	colorsOfChannels = []
	channelsToOpen = []

	#We loop through all the primary channels. If the user specified what color they should be in the output, we decode that here
	for item in primaryChannels:
		item.replace(" ", "")
		item.strip(",")
		x = item.split(",")

		posColors = ["rR", "gG", "bB", "cC", "mM", "yY"]
		colorIdex = []
		for s in x:
			try:
				int(s)
				domColor = 0
			except:
				colPicker = s[-1]
				if colPicker in posColors[0]:
					domColor = "Red"
				elif colPicker in posColors[1]:
					domColor = "Green"
				elif colPicker in posColors[2]:
					domColor = "Blue"
				elif colPicker in posColors[3]:
					domColor = "Cyan"
				elif colPicker in posColors[4]:
					domColor = "Magenta"
				elif colPicker in posColors[5]:
					domColor = "Yellow"
				else:
					domColor = 0
			colorIdex = colorIdex + [domColor]

		fullColorNames = ["Red", "Green", "Blue", "Cyan", "Magenta", "Yellow"]
		unclaimedColors = [Zz for Zz in fullColorNames if Zz not in colorIdex]
		new = 0
		while new < len(colorIdex):
			checkCol = colorIdex[new]
			if checkCol == 0:
				colorIdex[new] = unclaimedColors[0]
				unclaimedColors = unclaimedColors[1:]
			new += 1

		import re
		new = 0
		while new < len(x):
			a = x[new]
			x[new] = re.sub('[^0-9]','', a)
			new += 1


		x = [ int(y) for y in x ]

		channelsToOpen = channelsToOpen + x
		colorsOfChannels = colorsOfChannels +[colorIdex]


	channelsToOpen = channelsToOpen + secondaryChannels
	channelsToOpen = sorted(list(set(channelsToOpen)))
	channelRange = range(1, channelsToOpen[-1]+1)

	try:

		count = 1
		pullArray = []
		for item in channelRange:
			if item in channelsToOpen:
				IDs = channel_open(inPath, int(indices), item)

				#If not 8-bit, coerce to 8-bit
				impType = IDs.getBitDepth()
				if impType != 8:
					IJ.run(IDs, "8-bit", "")
					IJ.log("Image "+str(IDs.getTitle())+" coerced into 8-bit image")


				Title = IDs.getTitle()
				calibration = IDs.getCalibration()
				if timepoint == 1:
					dimensions = IDs.getDimensions()
					timeFinish =  dimensions[4]
				stack = IDs.getImageStack()
				ch = ImageStack(IDs.width, IDs.height)
				for i in range(1, IDs.getNSlices() + 1):
				  	index = IDs.getStackIndex(int(item), i, timepoint)
					ch.addSlice(str(i), stack.getProcessor(index))
				IDs = ImagePlus("Channel " + str(item), ch)
				IDs.setCalibration(calibration)
				IDs.setTitle(Title)
			else:
				IDs = 0
			pullArray = pullArray + [[count, IDs]]
			count += 1

	except:
		pullArray = "Error"

	return pullArray, timeFinish, colorsOfChannels

############################################################
#This function returns the channel we want to work on. If the user specified multiple channels for analysis, we open and merge them all
def decode_channels(idSearch, pullArray, colorsOfChannels, multipleC):

	from ij.plugin import RGBStackMerge
	from ij import IJ
	import re



	#This is the list of characters the user can input to specify channel colors
	acceptableStrings = ['r', 'R', 'g', 'G', 'b','B', 'c', 'C', 'm', 'M', 'y', 'Y']

	#If there are multiple channels, we need to pull the colorSet variable from the colorsOfChannels array
	if multipleC == 1:
		colorSet = colorsOfChannels[0]

	#If idSearch is not an integer, we need to unpack all the information that there is in it.
	if type(idSearch) != int:


		res1 = " ".join(re.split("[^a-zA-Z]*", idSearch))

		#Get rid of white spaces and split at commas
		idSearch.replace(" ", "")
		idSearch.strip(",")
		idSearch = idSearch.split(",")

		count = 0
		while count < len(idSearch):
			idSearch[count] = re.sub('[^0-9]','', idSearch[count])
			count +=1
		idSearch = [ int(newChannelZ) for newChannelZ in idSearch ]
	else:
		idSearch = [idSearch]

	#Initialize a variable to track which images we are going to assign to each channel
	impsToPull = []


	for item in pullArray:

		if int(item[0]) in idSearch:
			impsToPull = impsToPull + [item[1]]
			Title = item[1].getTitle()
	if len(impsToPull) == 1:
		baseImp = impsToPull[0]
		if multipleC == 1:
			IJ.run(baseImp, colorSet[0], "")
	else:
		concat = RGBStackMerge()
		if multipleC == 1:
			count = 0
			while count < len(impsToPull):
				IJ.run(impsToPull[count], colorSet[count], "")
				count +=1
		baseImp = concat.mergeChannels(impsToPull, True)

		rgbSwitch = False
		rgbCount = 0
		while rgbCount < len(res1):
			check = res1[rgbCount]


#Recently changed. be suspicious here if issues arise
			if check in acceptableStrings:
				rgbSwitch = True

			rgbCount+=1

	
		if rgbSwitch == True:
			print "whoopx3"
			IJ.run(baseImp, "RGB Color", "slices")
		


	baseImp.setTitle(Title)


	if multipleC==True:
		colorsOfChannels = colorsOfChannels[1:]
	return baseImp, colorsOfChannels

########################################################################################
# Run weka segmentation in 3d mode
def weka3D(IDs, method, genotypeNames):

	#Here, we make sure the correct type of classifier has been specified
	#If not, we exit the analysis
	if method.endswith(".model") == False:
		return 0, genotypeNames
	if method.find("_3D_.") == -1:
		return 0, genotypeNames

	#Import the weka segmenter
	from trainableSegmentation import WekaSegmentation
	import os
	segmentator = WekaSegmentation( True )

	#Pull the user-created classifier from the JSF_package
	classifierPath = os.path.join(os.getcwd(), "jars", "Lib", "JSF_package", "Weka_Models", method )
	segmentator.loadClassifier(classifierPath)

	#IDs is the raw image, segmentedIDs is the mask created by WEKA
	segmentedIDs = segmentator.applyClassifier(IDs, 0, False)

	#Get the class names from the WEKA classifier for use as our genotype names
	genotypeNames = segmentator.getClassLabels()

	#Calibrate the image
	segmentedIDs.setCalibration(IDs.getCalibration())

	return segmentedIDs, genotypeNames

##################################################
# This is used to find the minimum distance between a given point and all points on the perimeter of an ROI
def find_minimum_distance(roi, x, y):

	from ij.gui import ShapeRoi
	from math import sqrt

	roi = ShapeRoi(roi) #roi is whatever roi we want to get the minimum distance from. We coerce it into a shape roi, which is a format that has useful properties

	#Initialize starter values. Any distance between the roi and the designated point which is less than 'minimum' will become the new minimum.
	#The coordinates will then be stored in xmin and ymin
	minimum = 999999999999999999999999999999999999999999999999
	xmin = 0
	ymin=0

	#A shape roi is a composite of many other rois. We want to measure the distance against all rois for all the sub-shapes
	rA = roi.getRois()

	#We loop through each of the sub-rois, interpolate them into a polygon, and then extract one-by-one each pixel on the outline.
	#We then measure the distance from each such point
	for roi in rA:
		count = 0
		poly = roi.getInterpolatedPolygon(2, True)

		dx = x-xmin
		dy = y-ymin

		#get each pixel from the polygon
		while count < poly.npoints:
			dx = x-poly.xpoints[count]
			dy = y-poly.ypoints[count]

			#measure the distance using the pythagorean theorem
			distance = sqrt((dx*dx)+(dy*dy))

			#If we have a new minimum distance, overwrite the previous minimum
			if distance < minimum:
				minimum = distance
				xmin = poly.xpoints[count]
				ymin = poly.ypoints[count]

			count += 1

	#return the coordinates of the nearest point on the ROI perimeter
	return xmin, ymin
	
####################################################
# This is the DBSCAN clustering tool
def DBSCANtron (resultant, rm, iHeight):

		####pholder
		from ij import IJ
		import os
		from JSF_package.configCloneSeg import DBSCANChoice, DBSCANdistOE, DBSCANminDensityOE, DBSCANminCellsInCluster
		from ij.plugin.frame import RoiManager
		from ij.gui import ShapeRoi
			
		#The plugin will delete our ROIs. We save them, so we can retrieve them after we run the plugin
		roiPath = os.path.join(os.getcwd(),"Jars", "Lib", "JSF_package", "workingROIs.zip")
		rm.runCommand("Select All")
		rm.runCommand("save selected", roiPath)
		rm.reset()
		rm.getInstance()
		

		#Run the algorithm on a duplicate image
		resultant2 = resultant.duplicate()
		

		IJ.run(resultant2, "Select None", "")
		IJ.run(resultant2, "SSIDC Cluster Indicator", "distance=" + str(DBSCANdistOE)+" mindensity="+str(DBSCANminDensityOE))
		

		#Pull all of the resulting cluster ROIs from the ROI manager, store as a variable, then reload our previous ROIs
		rm = RoiManager.getInstance()
		rm.runCommand(resultant ,"Show None")
		rm.runCommand("Select All")
		
		#allClusters is our DBSCAN ROIs
		allClusters = rm.getSelectedRoisAsArray()
		rm.reset()

		#Resultant 2 is going to be a blank image onto which we will draw our cluster ROIs.
		#By doing this, we can bin together overlapping clusters to get a 'region of engraftment'
		resultant2 = resultant.duplicate()
		IJ.run(resultant2, "Create Selection", "")
		IJ.run(resultant2, "Fill", "")
		resultant3 = resultant2.duplicate()

		#In this step, we merge together all of our cluster ROIs into one big ROI, and draw that ROI on the image
		for item in allClusters:
			sumRoi = item
		sumRoi = ShapeRoi(sumRoi)
		for clusterRoi in allClusters:	
			clusterRoi = ShapeRoi(clusterRoi)
			sumRoi.or(clusterRoi)	
		resultant2.setRoi(sumRoi)
		IJ.run(resultant2, "Clear", "")
		IJ.run(resultant2, "Select None", "")
		
		
		#We now run an analyze particles step to get each 'cluster of clusters'
		IJ.run(resultant2, "Analyze Particles...", "size=0.00-1.000E19 circularity=0.00-1.00 add")
		
		#Pull all of the resulting cluster ROIs from the ROI manager, store as a variable, then reload our previous ROIs
		rm = RoiManager.getInstance()
		rm.runCommand(resultant ,"Show None")
		rm.runCommand("Select All")
		
		#clusters2 is our DBSCAN ROIs, binned for overlap
		clusters2 = rm.getSelectedRoisAsArray()
		rm.reset()
		
		#We now run through each of these cluster of clusters to check how many cells they contain. 
		#This is a filtering step where we get rid of excess clusters
		resultant.setRoi(1, iHeight+20, 1, 1)
		sumRoi = resultant.getRoi()
		sumRoi = ShapeRoi(sumRoi)
		
		
		filteredClusters = []
		for item in clusters2:
			resultant.setRoi(item)
			
			#run analyze particles step to figure out how many cells there are
			IJ.run(resultant, "Analyze Particles...", "size=0.00-1.000E19 circularity=0.00-1.00 exclude add")
			rm = RoiManager.getInstance()
			rm.runCommand(resultant ,"Show None")
			rm.runCommand("Select All")
			
			#clusters2 is our DBSCAN ROIs, binned for overlap
			cells = rm.getSelectedRoisAsArray()
			rm.reset()
			if len(cells) >= DBSCANminCellsInCluster:
				
				filteredClusters = filteredClusters + [item]
				sumRoi = item
			

			
		#Now we bin together our filtered clusters, which we will use to count the number of total cells	
		sumRoi = ShapeRoi(sumRoi)
		for newRoi in filteredClusters:	
			newRoi = ShapeRoi(newRoi)
			sumRoi.or(newRoi)	
			
			
		#Here is our decision point: treat the individual rois that have been clustered together as clones, use the DBSCAN search areas, or use the DBSCAN search areas + fill holes
		if DBSCANChoice != "Cluster Without Flooding":
			resultant3.setRoi(sumRoi)
			IJ.run(resultant3, "Clear", "")
			IJ.run(resultant3, "Select None", "")
			
			if DBSCANChoice == "Flood + Fill Foles":
				IJ.run(resultant3, "Fill Holes", "")


		else:
		
			
			resultant.setRoi(sumRoi)
			IJ.run(resultant, "Analyze Particles...", "size=0.00-1.000E19 circularity=0.00-1.00 exclude add")
			
			rm = RoiManager.getInstance()
			rm.runCommand(resultant ,"Show None")
			rm.runCommand("Select All")
	
			
			#validCells is the cells identified in our clusters
			validCells = rm.getSelectedRoisAsArray()
			
			for item in validCells:
				sumRoi = item
			sumRoi = ShapeRoi(sumRoi)
			for clusterRoi in validCells:	
				clusterRoi = ShapeRoi(clusterRoi)
				sumRoi.or(clusterRoi)	
			resultant3.setRoi(sumRoi)
			IJ.run(resultant3, "Clear", "")
			IJ.run(resultant3, "Select None", "")
			

		
		
		rm.reset()
		rm.getInstance()
		rm.runCommand("Open", roiPath)
		IJ.run(resultant, "Select None", "")
		
#			resultant2.setRoi(sumRoi)
#			IJ.run(resultant2, "Make Inverse", "")			
#			IJ.setForegroundColor(255, 255, 255)
#			IJ.run(resultant2, "Fill", "")
#			
#
#			
#			#if DBSCANChoice == "Convex hull":
#			IJ.run(resultant2, "Select None", "")
#			IJ.run(resultant2, "Create Selection", "")
#			IJ.run(resultant2, "Convex Hull", "")
#			#IJ.run(resultant2, "Fill Holes", "")
#			
#			resultant2.show()
#			exit()

		
		resultant = resultant3.duplicate()
		
		return resultant, rm
