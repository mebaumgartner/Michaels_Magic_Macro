#
#
#
# This module contains only the speckle analysis function
#
#
#

################
def speckleAnalysis(IDs4, stackno, iHeight, rm, sliceROIs, pouch, timepoint, numGenotypes, excludinator):

	from JSF_package.configBasic import speckleNoise, dcp1Choice, speckleMethod
	from JSF_package.configRoi import halfHalf, halfHalfNC
	import JSF_package


	#Import needed functions and modulees
	from ij.measure import ResultsTable, Measurements
	from ij import IJ, ImagePlus
	from ij.plugin.filter import ParticleAnalyzer, MaximumFinder
	from ij.plugin import ImageCalculator
	from ij.process import ImageProcessor
	from JSF_package._misc_ import mask_confirmer, selection_confirmer
	from ij.gui import ShapeRoi, OvalRoi
	
	rt = ResultsTable.getResultsTable()
	#Isolate reflux channel image from active z slice
	IDs4.setSlice(stackno)
	refluxImp = IDs4.crop()
	refluxSeed = refluxImp.duplicate()
	

	#Duplicate the image for the output image
	refOutImp = refluxImp.clone()
	IJ.run(refOutImp, "Orange Hot", "")
	IJ.run(refOutImp, "RGB Color", "")
	refStandImp = IDs4.crop()
	IJ.run(refStandImp, "RGB Color", "")

	#Resize the reflux image to give us some dead space to work with and to match its dimensions to the other images
	IJ.run(refluxImp, "Canvas Size...", "width="+str(refluxImp.getWidth())+" height="+str(iHeight+100)+" position=Top-Left zero")

	#We copy the image and run a remove outliers step to get rid of any reflux speckles. This gives us just the background
	impAutoBackground = refluxImp.duplicate()
	IJ.run(impAutoBackground, "Remove Outliers...", "radius=25 threshold=50 which=Bright stack")

	#Subtract the background image from the original image, This gives us cleaner reflux signal
	refluxImp = ImageCalculator().run("Subtract create", refluxImp, impAutoBackground)

	#We extract the non-zero mode grey value. We interpret this to be background and subtract this value from all pixels in the image.
	refluxImp.getProcessor().setThreshold(1, 255, 0)
	IJ.run(refluxImp, "Select All", "")
	IJ.run("Clear Results")
	IJ.run(refluxImp, "Measure", "")
	modMod = rt.getValue("Mode", 0)
	IJ.run(refluxImp, "Subtract...", "value="+str(modMod+1))
	

	#We run a find maxima step and we convert the output to an image with dots for all the found maxima.
	mf = MaximumFinder()
	maxip = mf.findMaxima(refluxImp.getProcessor(), speckleNoise, ImageProcessor.NO_THRESHOLD, MaximumFinder.SINGLE_POINTS, True, False)
	maxip = ImagePlus("Found maxima", maxip)
	maxip.setCalibration(refluxImp.getCalibration())

	#We confirm the image was made in the right way, then create a selection for use in making the output image. 
	mask_confirmer(iHeight + 20, maxip)
	IJ.run(maxip, "Select None", "")
	IJ.run(maxip, "Invert", "")

	#We create an ROI for the speckles in the image
	IJ.run(maxip, "Create Selection", "")
	refluxRoi = ShapeRoi(maxip.getRoi())



	#If the user specifies speckle area and fluorescence, we do a seeded region growing step on the speckles using the 'found maxima' as seeds
	if speckleMethod != "Disabled":

		#Create a new image formatted for seeded region growing analysis
		maxipNew = IJ.createImage("SRG_Mask:"+str(stackno), "8-bit black", maxip.getWidth(), iHeight, 1)
		maxipNew.setCalibration(maxip.getCalibration())
		maxipNew.setRoi(refluxRoi)
		IJ.run(maxipNew, "Set Pixels ...", "value=1")		

		#Run a gaussian blur on the image we apply the seeds to, if specified by user
		gaussVal = speckleMethod.find("=")
		if gaussVal != -1:
			gaussVal = speckleMethod[gaussVal+1:]
			gaussVal = int(gaussVal)
			IJ.run(refluxSeed, "Gaussian Blur...", "sigma="+str(gaussVal))
	
		#Run the seeded region growing
		animationStack, maxip2 = JSF_package.seeded_region_growing.seededRegionGrowing(refluxSeed, maxipNew, 1, iHeight, rm, True, 50)

		#Reformat the seeded region growing image back into a binary image of the correct format
		maxip2.getProcessor().setThreshold(1, 255, 0)
		IJ.run(maxip2,"Convert to Mask", "")
		IJ.run(maxip2, "Canvas Size...", "width="+str(maxip.getWidth())+" height="+str(iHeight+100)+" position=Top-Left zero")
		mask_confirmer(iHeight + 20, maxip2)
		maxip = maxip2

		#We create an ROI for the speckles in the image
		IJ.run(maxip, "Create Selection", "")
		refluxRoi = ShapeRoi(maxip.getRoi())



	#We go through the ROI manager and extract the ROIs corresponding to the areas we are interested in. 
	a = len(sliceROIs)
	dcp1Corrector = 0
	if dcp1Choice == 1:
		dcp1Corrector = 5
		dcp1Corrector = dcp1Corrector + (4*(numGenotypes-1))
	if halfHalf == True:
		dcp1Corrector = numGenotypes
	dcp1Corrector = dcp1Corrector + (4*(numGenotypes-1))

	loopCounter = 0
	while loopCounter < numGenotypes:

		if loopCounter != 0 and JSF_package.configRoi.halfHalfNC == True:
			pouch = excludinator.clone()
	
		clones = ShapeRoi(rm.getRoi(sliceROIs[a-4-dcp1Corrector+(4*loopCounter)]))
		center = ShapeRoi(rm.getRoi(sliceROIs[a-2-dcp1Corrector+(4*loopCounter)]))
		border = ShapeRoi(rm.getRoi(sliceROIs[a-3-dcp1Corrector+(4*loopCounter)]))
		nc = ShapeRoi(rm.getRoi(sliceROIs[a-1-dcp1Corrector+(4*loopCounter)]))
	

	
		#Get the intersection of the speckes with the clones
		refluxDup = refluxRoi.clone()
		cloneSpeckles = clones.clone()
		cloneSpeckles = cloneSpeckles.and(refluxDup)
		cloneSpeckles, isClones = selection_confirmer(cloneSpeckles, iHeight, refluxImp)
		rm.addRoi(cloneSpeckles)
	
		#Get the intersection of the speckles with the border
		refluxDup = refluxRoi.clone()
		borderSpeckles = border.clone()
		borderSpeckles = borderSpeckles.and(refluxDup)
		borderSpeckles, isBorder = selection_confirmer(borderSpeckles, iHeight, refluxImp)
		rm.addRoi(borderSpeckles)
	
		#Get the intersection of the speckles with the center
		refluxDup = refluxRoi.clone()
		centerSpeckles = center.clone()
		centerSpeckles = centerSpeckles.and(refluxDup)
	
		centerSpeckles, isCenter = selection_confirmer(centerSpeckles, iHeight, refluxImp)
		rm.addRoi(centerSpeckles)
	
		#Get the intersection of the speckles with the not-clones
		refluxDup = refluxRoi.clone()
		ncSpeckles = nc.clone()
		ncSpeckles = ncSpeckles.and(refluxDup)
		ncSpeckles, isNc = selection_confirmer(ncSpeckles, iHeight, refluxImp)
		rm.addRoi(ncSpeckles)
	
		#Get the intersection of the speckles with the non-competing half and half region
		if halfHalf == True:
			refluxDup = refluxRoi.clone()
			hh = rm.getRoi(pouch+1)
			hhSpeckles = hh.clone()
			hhSpeckles = ShapeRoi(hhSpeckles)
			hhSpeckles = hhSpeckles.and(refluxDup)
			hhSpeckles, isHh = selection_confirmer(hhSpeckles, iHeight, refluxImp)
			rm.addRoi(hhSpeckles)	
	
		#Here we rename each ROI appropriately
		x = rm.getCount()
		if halfHalf == True:
			x = x -1
		rm.select(x-4)
		rm.runCommand("Rename", "SpecklesClones_ROI_Z:"+str(stackno)+isClones+"_genotype"+str(loopCounter+1)+"_timepoint"+str(timepoint))
		rm.select(x - 3)
		rm.runCommand("Rename", "SpecklesCenter_ROI_Z:"+str(stackno)+isCenter+"_genotype"+str(loopCounter+1)+"_timepoint"+str(timepoint))
		rm.select(x - 2)
		rm.runCommand("Rename", "SpecklesBorder_ROI_Z:"+str(stackno)+isBorder+"_genotype"+str(loopCounter+1)+"_timepoint"+str(timepoint))
		rm.select(x - 1)
		rm.runCommand("Rename", "SpecklesNotClones_ROI_Z:"+str(stackno)+isNc+"_genotype"+str(loopCounter+1)+"_timepoint"+str(timepoint))
		if halfHalf == True:
			rm.select(x)
			rm.runCommand("Rename", "SpecklesNonCompeting_ROI_Z:"+str(stackno)+isHh+"_genotype"+str(loopCounter+1)+"_timepoint"+str(timepoint))
			sliceROIs.extend([x-4, x-3, x-2, x-1, x])		
		else:
			sliceROIs.extend([x-4, x-3, x-2, x-1])

		loopCounter += 1

	return refOutImp, refStandImp, sliceROIs


###################################################################################################################
#Extract the speckle counts from the speckle ROIs
def speckle_counter(activeRoi, iHeight, iWidth):

	from ij import IJ
	from ij.measure import ResultsTable, Measurements
	from ij.plugin.filter import ParticleAnalyzer

	#get the mask of the relevant roi
	antMask = IJ.createImage("speckleMask", "8-bit white", iWidth, iHeight, 1)
	antMask.setRoi(activeRoi)
	IJ.setForegroundColor(0, 0, 0)
	IJ.run(antMask, "Fill", "slice")

	#Run analyze particles on it to get the count of speckles
	refluxTable = ResultsTable()
	pa = ParticleAnalyzer(0, Measurements.AREA, refluxTable, 0, 99999999999999999, 0.0, 1.0)
	pa.setHideOutputImage(True)
	pa.analyze(antMask)
	refluxCount = refluxTable.getCounter()
	refluxTable.reset()

	return refluxCount