###############################################################################
#This implements the seeded region growing tool by Jarek Sacha

def seededRegionGrowing(impToBeProcessed, seedMask, numFrames, iHeight, rm, invertSeedClones, minSeedSizeClones):
	
	from net.sf.ij_plugins.im3d.grow import SRG
	from ij.process import ByteProcessor, ImageProcessor
	from ij import ImagePlus, IJ
	from ij.plugin.filter import MaximumFinder, ParticleAnalyzer
	from ij.plugin.frame import RoiManager
	from JSF_package._misc_ import mask_confirmer
	from ij.gui import ShapeRoi, OvalRoi
	from ij.measure import ResultsTable, Measurements

	IJ.run(seedMask, "Select None", "")
	IJ.run(impToBeProcessed, "Select None", "")

	IJ.run(seedMask, "Canvas Size...", "width="+str(seedMask.getWidth())+" height="+str(iHeight)+" position=Top-Left zero")
	maskDupRes = impToBeProcessed.duplicate()

	#The impToBeProcessed can be any imageplus. seedMask must be a mask image with 0-255 values, where 0 is background.
	#Here we get everything in the correct format to be run
	impToBeProcessed = impToBeProcessed.duplicate()
	cal = impToBeProcessed.getCalibration()
	caliber = cal.pixelHeight

	
	if invertSeedClones == True:
		IJ.run(impToBeProcessed, "Invert", "")
	seedMask = seedMask.duplicate()
	IJ.run(seedMask, "Add...", "value=1")
	impToBeProcessed.duplicate()

	
	#Find regions to be our background seeds. This step is tricky, but the best solution I found is just to find the points furthest from the seeds and use those.
	#We do this by taking the background, converting it to binary, running a watershed, and finding the centroids
	seedMaskDup = seedMask.duplicate()
	seedMaskDup.getProcessor().setThreshold(1, 1, 0)
	IJ.run(seedMaskDup,"Convert to Mask", "")
	IJ.run(seedMaskDup, "Canvas Size...", "width="+str(seedMaskDup.getWidth())+" height="+str(iHeight+100)+" position=Top-Left zero")
	mask_confirmer(iHeight + 20, seedMaskDup)
	IJ.run(seedMaskDup, "Create Selection", "")
	toSub = ShapeRoi(seedMaskDup.getRoi())
	IJ.run(seedMaskDup, "Select None", "")
	IJ.run(seedMaskDup, "Watershed", "")


	manager = RoiManager(True)
	ParticleAnalyzer.setRoiManager(manager) 
	pa = ParticleAnalyzer(ParticleAnalyzer.ADD_TO_MANAGER | ParticleAnalyzer.SHOW_NONE,Measurements.AREA,ResultsTable(), float(minSeedSizeClones)/(caliber*caliber), 9999999999, 0.0, 1.0)
	pa.setHideOutputImage(True)
	pa.analyze(seedMaskDup)
	manager.runCommand("Deselect")
	
	#We now get all the resulting ROIs from the roi manager as an array
	aROIs = manager.getSelectedRoisAsArray()
	manager.reset()
	
	y = 1 #y is just a counter we use for tracking indexes through the loop

	#Set roi to original image and draw
	seedMask.setRoi(toSub)
	IJ.run(seedMask, "Set Pixels ...", "value=0")
	
	#the variable rp ('Roi Presumptive') is an roi from the analyze particles step.
	for rp in aROIs:
		cent = rp.getContourCentroid()
		oO = OvalRoi(cent[0], cent[1], 1, 1)
		seedMask.setRoi(oO)
		IJ.run(seedMask, "Set Pixels ...", "value=1")

	
	IJ.run(seedMask, "Select None", "")
	seedMask = seedMask.getProcessor()
	seedMask = seedMask.convertToByteProcessor()
	
	try:
		#Run the seeded region growing
		srg = SRG()
		srg.setImage(impToBeProcessed.getProcessor())
		srg.setSeeds(seedMask)
		srg.setNumberOfAnimationFrames(numFrames)
		srg.run()

		IJ.log("Seeded Region Growing Completed Successfully.")

		#Get the output images and 
		regionMask = srg.getRegionMarkers()
		animationStack = srg.getAnimationStack()
		animationStack = ImagePlus("SRG ANIMATION STACK", animationStack)
		regionMask = ImagePlus("SRG MASK", regionMask)
		IJ.run(regionMask, "Subtract...", "value=1")
		regionMask.setCalibration(cal)
		animationStack.setCalibration(cal)

	except:
	
		IJ.log("Seeded Region Growing Unsuccessful. Returning Seeds")
		animationStack  = maskDupRes.duplicate()
		regionMask = maskDupRes 

	return animationStack, regionMask
