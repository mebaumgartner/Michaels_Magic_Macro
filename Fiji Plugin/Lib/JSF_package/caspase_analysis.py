#################################################################
#
#
#  All of these functions are used for Foci segmentation in PECAn
#
#
###################################################################

###################################
# This is the foci segmentation function


def caspase_segmentor (cal, imp, rm, Title, stackno, pouch, caspaseSegment, dcp1Radius, caliber, minCasSize, timepoint, excludinator, iHeight):



	#Import the functions we need
	from ij.plugin.filter import MaximumFinder, ParticleAnalyzer
	from ij.process import ImageProcessor
	from ij.gui import ShapeRoi
	from ij import IJ, ImagePlus
	from ij.plugin.frame import RoiManager
	from ij.measure import ResultsTable, Measurements
	from JSF_package._misc_ import mask_confirmer
	from inra.ijpb.morphology import Strel, Morphology, MinimaAndMaxima
	from inra.ijpb.binary import BinaryImages
	from ij.plugin import ImageCalculator
	from ij.gui.Roi import getContourCentroid
	from java.awt import Color, Rectangle

	import JSF_package
	from JSF_package._misc_ import mask_confirmer, selection_confirmer
	from JSF_package import _misc_, caspase_analysis, cell_tracking, clone_analysis, configBasic, configCellTrack, configCloneSeg, configDeathSeg, configDeathTrack, configRoi, configSave, seeded_region_growing, speckles_analysis, start_up, tracking_and_outputs, user_inputs_GUI


	rt = ResultsTable.getResultsTable()
	IJ.run("Clear Results")
	IJ.run(imp, "Select None", "")
	IJ.run(imp, "Measure", "")
	Max = rt.getValue("Max", 0)

	maskImp = IJ.createImage("woop", "8-bit black", imp.getWidth(), (iHeight+100), 1)
	maskImp.setCalibration(cal)

	if JSF_package.configRoi.halfHalfNC == True:
		pouch = pouch.clone()
		pouch = pouch.or(excludinator)

	y = 0
	newCasRois= list()
	import time
	while 1 <= Max:

		#time.sleep(0.0001)
		print "Caspase segmentor initiated"
		
		IJ.run("Clear Results")
		IJ.run(imp, "Select None", "")
		IJ.run(imp, "Measure", "")
		Max = rt.getValue("Max", 0)

		if Max == 0:
			break
		
		print "Duplicating caspase masks"
		imp2 = imp.duplicate()

		print "Applying threshold
		imp2.getProcessor().setThreshold(Max, Max, 0)

		print "Converting to mask"


		IJ.run(imp2,"Convert to Mask", "")

		print "Resizing canvas"

		IJ.run(imp2, "Canvas Size...", "width="+str(imp2.getWidth())+" height="+str(iHeight+100)+" position=Top-Left zero")

		print "Canvas resized"


		mask_confirmer(iHeight + 20, imp2)

		print "Creating selection"
		
		#Make sure no selections are lingering on the images
		IJ.run(imp2, "Create Selection", "")
		clearRoi = ShapeRoi(imp2.getRoi())
		imp.setRoi(clearRoi)
		IJ.run(imp, "Clear" , "")


		print "Apply and measure"
		maskImp.setRoi(clearRoi)
		IJ.run("Clear Results")
		IJ.run(maskImp, "Measure", "")
		Area = rt.getValue("Area", 0)

		print "Confirming selection"
		clearRoi, isZ = selection_confirmer(clearRoi, iHeight, maskImp)
		checker1 = pouch.clone()
		checker2 = clearRoi.clone()
		checker1 = checker1.and(checker2)
		checker1 = str(checker1)


		print "Selection parameters"
		checker1 = str(clearRoi)
		xVal = checker1.find("x=")
		xVal = xVal + 2
		yVal = checker1.find("y=")
		yVal = yVal + 2
		wVal = checker1.find("width=")
		wVal = wVal+6
		hVal = checker1.find("height=")
		hVal = hVal+7

		print "Skip step initiating"
		if checker1[xVal] == "0" and checker1[yVal] == "0" and checker1[wVal] == "0" and checker1[hVal] == "0":
			skipper = True
		else:
			skipper = False

		if skipper == False:

			if Area >= minCasSize:
				y += 1

				xCoord, yCoord = getContourCentroid(clearRoi)
				newRoi = ShapeRoi(int(xCoord), int(yCoord), Rectangle(0,0,1,1))
				maskImp.setRoi(newRoi)
				IJ.run(maskImp, "Enlarge...", "enlarge="+str(dcp1Radius)+ " scaled")
				enlargedRoi = ShapeRoi(maskImp.getRoi())
				rm.addRoi(enlargedRoi)
				T = rm.getCount() - 1
				rm.select(T)

				rm.runCommand("Rename", "Caspase_Z:"+str(stackno)+"_No."+str(y)+"_timepoint"+str(timepoint))

				newCasRois.extend([T])


	#Now we just run back to the parent clone analysis function
	caspaseSegment.append(newCasRois)

	print "Caspase segmentation ROIs added"
	return caspaseSegment



################################
#This is our caspase analysis function
def dcp1_analysis( pouch, IDs2, Title, stackno, iHeight, rm, sliceROIs, casRefArray, numGenotypes, timepoint, caspaseSegment, excludinator, seedIDcas, segmentedIDs2, iWidth):

	import JSF_package
	from JSF_package.configDeathSeg import dcp1Booster, lowPass, highPass, multVal, outVal, seedChoiceCas, seedChannelCas, minSeedSizeCas, invertSeedCas
	from JSF_package.configRoi import halfHalf
	from JSF_package.configDeathTrack import minCasSize, dcp1Gauss, dcp1Radius
	from JSF_package import _misc_, caspase_analysis, cell_tracking, clone_analysis, configBasic, configCellTrack, configCloneSeg, configDeathSeg, configDeathTrack, configRoi, configSave, seeded_region_growing, speckles_analysis, start_up, tracking_and_outputs, user_inputs_GUI

	from ij import IJ
	from ij.plugin import ImageCalculator
	from ij.plugin.frame import RoiManager
	from ij.plugin.filter import ParticleAnalyzer
	from ij.measure import ResultsTable, Measurements
	from ij.gui import ShapeRoi
	from JSF_package._misc_ import mask_confirmer, selection_confirmer
	from ij import IJ, ImagePlus


	IJ.redirectErrorMessages(True)

	IDs2.setSlice(stackno)
	imp = IDs2.crop()

	cal = imp.getCalibration()
	Caliber = cal.pixelHeight
	casImp = imp.duplicate()
	countImp = imp.duplicate()


	if (JSF_package.configBasic.cellDeathSegMethod == "High Background") or (JSF_package.configBasic.cellDeathSegMethod == "Default"):

		print ("Standard background caspase")


		### Super filtering for v. noisy images
		if JSF_package.configBasic.cellDeathSegMethod == "High Background":

			#This is an extra background subtraction step.
			impAutoBackground = imp.duplicate()

			#we threshold a duplicate image, with a high threshold cutoff (mean + 2x stdev) and run an analyze particles for anything larger than our remove outliers filter
			maskOfDestiny = imp.duplicate()
			thresholding  = maskOfDestiny.getRawStatistics()
			impStdDev = thresholding.stdDev
			impMean = thresholding.mean
			maskOfDestiny.getProcessor().setThreshold((impMean + impStdDev + impStdDev), 255, 0)
			IJ.run(maskOfDestiny, "Convert to Mask", "")
			manager = RoiManager(True)
			ParticleAnalyzer.setRoiManager(manager)
			pa = ParticleAnalyzer(ParticleAnalyzer.ADD_TO_MANAGER | ParticleAnalyzer.SHOW_NONE,Measurements.AREA,ResultsTable(), 625, 999999999999999, 0.0, 1.0)
			pa.setHideOutputImage(True)
			pa.analyze(maskOfDestiny)
			rm.runCommand(maskOfDestiny,"Show None")
			manager.runCommand("Deselect")
			aROIs = manager.getSelectedRoisAsArray()
			IJ.run(impAutoBackground, "Remove Outliers...", "radius=25 threshold=50 which=Bright stack")
			if aROIs:
				op = ShapeRoi(aROIs[0])
				for rp in aROIs:
					rp = ShapeRoi(rp)
					op.or(rp)
				impAutoBackground.setRoi(op)
				IJ.run(impAutoBackground, "Clear", "")
			IJ.run(imp,"Salt and Pepper", "stack")
			imp = ImageCalculator().run("Subtract create", imp, impAutoBackground)

		#Here we multiply the whole dcp1 pixel intensity specified by the user, if the user elects to do so. This step is optional, but helps dim images
		if str(dcp1Booster) != '1.0':
			IJ.run(imp, "Multiply...", "value="+str(dcp1Booster)+" stack")



		#Run our high-pass and low-pass filters
		highPassImp = imp.duplicate()
		lowPassImp = imp.duplicate()
		IJ.run(lowPassImp, "Bandpass Filter...", "filter_large="+str(lowPass/Caliber)+" filter_small=0 suppress=None tolerance=5")
		IJ.run(highPassImp, "Bandpass Filter...", "filter_large=99999999999999999999999999 filter_small="+str(highPass/Caliber)+" suppress=None tolerance=5")
		if int(multVal) != 1:
			IJ.run(highPassImp, "Multiply...", "value="+str(multVal)+" stack")


		print ("highpass/lowpass filter performed")
		
		#Subtract lowpass from the original image, then add highpass to the resultant
		imp = ImageCalculator().run("Subtract create", imp, lowPassImp)
		imp = ImageCalculator().run("Add create", imp, highPassImp)

		#Clear the images we don't need anymore
		lowPassImp.flush()
		highPassImp.flush()

		#This is an extra background subtraction step.
		impAutoBackground = imp.duplicate()

		#we threshold a duplicate image, with a high threshold cutoff (mean + 2x stdev) and run an analyze particles for anything larger than our remove outliers filter
		maskOfDestiny = imp.duplicate()
		thresholding  = maskOfDestiny.getRawStatistics()
		impStdDev = thresholding.stdDev
		impMean = thresholding.mean
		maskOfDestiny.getProcessor().setThreshold((impMean + impStdDev + impStdDev), 255, 0)
		IJ.run(maskOfDestiny, "Convert to Mask", "")
		manager = RoiManager(True)
		ParticleAnalyzer.setRoiManager(manager)
		pa = ParticleAnalyzer(ParticleAnalyzer.ADD_TO_MANAGER | ParticleAnalyzer.SHOW_NONE,Measurements.AREA,ResultsTable(), 625, 999999999999999, 0.0, 1.0)
		pa.setHideOutputImage(True)
		pa.analyze(maskOfDestiny)
		rm.runCommand(maskOfDestiny,"Show None")
		manager.runCommand("Deselect")
		aROIs = manager.getSelectedRoisAsArray()
		IJ.run(impAutoBackground, "Remove Outliers...", "radius=25 threshold=50 which=Bright stack")
		if aROIs:
			op = ShapeRoi(aROIs[0])
			for rp in aROIs:
				rp = ShapeRoi(rp)
				op.or(rp)
			impAutoBackground.setRoi(op)
			IJ.run(impAutoBackground, "Clear", "")
		imp = ImageCalculator().run("Subtract create", imp, impAutoBackground)


		print ("Outliers removed")

		#Now we make the actual mask
		IJ.run(imp, "Auto Threshold", "method=RenyiEntropy white")
		IJ.run(imp,"Convert to Mask", "")



		#Ensure mask was made correctly
		mask_confirmer(iHeight + 20, imp)

		#Small objects of a size decided by the user are removed
		IJ.run(imp, "Remove Outliers...", "radius="+str(outVal/Caliber)+" threshold=50 which=Dark")
		IJ.run(imp, "Fill Holes", "stack")


	#Run our custom script options
	else:

		import os

		IDs2.setSlice(stackno)
		cloneImp = IDs2.crop()
		cal = cloneImp.getCalibration()
		caliber = cal.pixelHeight

		#Run the weka analysis, if prompted
		if JSF_package.configBasic.cellDeathSegMethod.endswith(".model"):



			if JSF_package.configBasic.cloneSeg.find("_3D_.") == -1:

				from trainableSegmentation import WekaSegmentation

				segmentator = WekaSegmentation( cloneImp )
				classifierPath = os.path.join(os.getcwd(), "jars", "Lib", "JSF_package", "Weka_Models", JSF_package.configBasic.cellDeathSegMethod)
				segmentator.loadClassifier(classifierPath)
				resultant = segmentator.applyClassifier(cloneImp, 0, False)

			else:
				segmentedIDs2.setSlice(stackno)
				resultant = segmentedIDs2.crop()

		else:
			from JSF_End_User_Made_Code._executor import user_made_code
			resultant = user_made_code(JSF_package.configBasic.cellDeathSegMethod, cloneImp, IDs2, rm, stackno, pouch, excludinator)

		if seedChoiceCas == True:


			seedIDcas.setSlice(stackno)
			seedIDcas2 = seedIDcas.crop()

			#Run a gaussian blur on the image we apply the seeds to, if specified by user
			gaussVal = JSF_package.configDeathSeg.blurCas

			if gaussVal != 0:
				IJ.run(seedIDcas2, "Gaussian Blur...", "sigma="+str(gaussVal))

			animationStack, regionMask = JSF_package.seeded_region_growing.seededRegionGrowing(seedIDcas2, resultant, 1, iHeight, rm, minSeedSizeCas, invertSeedCas)
			resultant = regionMask.duplicate()

		if JSF_package.configDeathSeg.casPost != "None" and JSF_package.configBasic.cellDeathSegMethod.endswith(".model"):


			if JSF_package.configDeathSeg.casPost == "Despeckle":
				IJ.run(resultant, "Despeckle", "")
			elif JSF_package.configBasic.cellDeathSegMethod.find("_3D_.") == -1:
				from JSF_End_User_Made_Code._executor import user_made_code
				resultant = user_made_code(JSF_package.configDeathSeg.casPost, resultant, IDs, rm, stackno, pouch, excludinator)
			else:
				from JSF_End_User_Made_Code._executor import user_made_code
				resultant = user_made_code(JSF_package.configDeathSeg.casPost, resultant, segmentedIDs, rm, stackno, pouch, excludinator)


		resultant.getProcessor().setThreshold(1, 255, 0)
		IJ.run(resultant,"Convert to Mask", "")
		IJ.run(resultant, "Canvas Size...", "width="+str(resultant.getWidth())+" height="+str(iHeight+100)+" position=Top-Left zero")
		mask_confirmer(iHeight + 20, resultant)
		imp = resultant


	
	#Caspase watershed for counting step
	casMask2 = imp.duplicate()
	iWidth = casMask2.getWidth()
	pHo = IJ.run(imp, "Create Selection", "")
	pHo = imp.getRoi()

	

	if JSF_package.configBasic.dcp1Counting == 1:

		print ("Watershed initiated")

		from inra.ijpb.watershed import Watershed


		#we get our images and clean them up a little bit in preparation for the watershed
		IJ.run(casMask2, "Invert", "")
		IJ.run(countImp, "Invert", "")
		IJ.run(countImp, "Gaussian Blur...", "sigma="+str(dcp1Gauss)+" scaled")
		IJ.run(casMask2, "Invert", "")


		#Run the watershed
		newImp = Watershed.computeWatershed(countImp,  casMask2, 4, 0, 255)
		forCounts = newImp.duplicate()


		#Now we get the watershed-ed image, and put into a properly formatted binary image
		IJ.setThreshold(newImp, -1000000000000000000000000000000.000000000, 0.000000000)
		IJ.run(newImp, "Make Binary", "")
		IJ.run(newImp, "Canvas Size...", "width="+str(newImp.getWidth())+" height="+str(iHeight+100)+" position=Top-Left zero")
		mask_confirmer(iHeight+20, newImp)
		IJ.run(newImp, "Canvas Size...", "width="+str(newImp.getWidth())+" height="+str(iHeight)+" position=Top-Left zero")
		IJ.run(newImp, "Invert", "")

		
		#get the rois and add them to the manager
		IJ.run(newImp, "Create Selection", "")
		IJ.run(newImp, "Make Inverse", "")
		IJ.run(newImp, "Add to Manager", "")
		x = rm.getCount()

		#scrap the images we aren't using anymore
		countImp.hide()
		casMask2.hide()
		countImp.flush()
		casMask2.flush()
		IJ.runMacro("run('Close All')")

		#We then feed this image into the caspase_segmentor function
		IJ.setForegroundColor(255, 255, 255)
		imp = IJ.createImage("woop", "8-bit black", iWidth, iHeight, 1)
		yeet = ShapeRoi(rm.getRoi(x-1))
		rm.select(x-1)
		rm.runCommand("Delete")
		imp.setRoi(yeet)
		IJ.run(imp, "Fill", "")
		cal = casImp.getCalibration()
		caliber = cal.pixelHeight



		caspaseSegment = caspase_segmentor (cal, forCounts, rm, Title, stackno, pouch, caspaseSegment,  dcp1Radius, caliber, minCasSize, timepoint, excludinator, iHeight)

		print ("Caspase events segmented")

	#We create a selection from the mask and extract the ROI

	x = pHo

	IJ.run(imp, "Canvas Size...", "width="+str(iWidth)+" height="+str(iHeight+100)+" position=Top-Left zero")


	#If the ROI exists, we find its intersection with the pouch and make a new ROI
	print ("Finding caspase in ROI")
	if x:
		x = ShapeRoi(x)
		y = pouch.clone()

		if JSF_package.configRoi.halfHalfNC == True:
			exc = excludinator.clone()
			y = y.or(exc)
		checker1 = y.clone()
		z= x.and(y)
		z = ShapeRoi(z)

		#We check if this new, resulting ROI exists or not
		z, isZ = selection_confirmer(z, iHeight, imp)
		checker2 = z.clone()
		checker1 = checker1.xor(checker2)
		checker1 = str(checker1)
		xVal = checker1.find("x=")
		xVal = xVal + 2
		yVal = checker1.find("y=")
		yVal = yVal + 2
		wVal = checker1.find("width=")
		wVal = wVal+6
		hVal = checker1.find("height=")
		hVal = hVal+7
		if checker1[xVal] == "0" and checker1[yVal] == "0" and checker1[wVal] == "0" and checker1[hVal] == "0":
			imp.setRoi(5,iHeight+20,1,1)
			z = imp.getRoi()
			z = ShapeRoi(z)
			isZ = "<empty>"

	#If the ROI does not exist, we create a placeholder ROI and mark it as empty
	else:
		imp.setRoi(5,iHeight+20,1,1)
		z = imp.getRoi()
		z = ShapeRoi(z)
		isZ = "<empty>"

	#Add the ROI to the manager and log the number of ROIs therein. We rename the caspase ROI as we go
	peeps = rm.getCount()
	rm.addRoi(z)
	x = rm.getCount() - 1
	rm.select(x)
	sliceROIs.extend([x])
	rm.runCommand("Rename", "Caspase_ROI_Z:"+str(stackno)+isZ+"_genotype1_timepoint"+str(timepoint))

	print ("ROI added to manager")

	#Here we create our caspase mask
	IJ.setForegroundColor(255, 255, 255)
	cas = ShapeRoi(rm.getRoi(x))

	

	#Set the red color for the caspase image, convert to RGB
	if seedChoiceCas == True:
		seedIDcas.setSlice(stackno)
		casImp = seedIDcas.crop()
	IJ.run(casImp, "RGB Color", "")
	casCasImp = casImp.duplicate()
	borderM = pouch.clone()
	casCasImp.setRoi(borderM)
	IJ.run("Line Width...", "line=4")
	IJ.run(casCasImp, "Draw", "")
	if JSF_package.configRoi.halfHalfNC == True:
		casCasImp.setRoi(excludinator)
		IJ.setForegroundColor(255, 255, 0)
		IJ.run(casCasImp, "Draw", "")
	casMask = casCasImp.duplicate()
	casCasImp.setRoi(cas)
	IJ.run(casCasImp, "Fill", "")

	print ("Caspase image produced")

	#We go through the ROI manager and extract the ROIs
	a = len(sliceROIs)
	cas = ShapeRoi(rm.getRoi(x))



	loopCounter = 0
	while loopCounter < numGenotypes:

		if loopCounter != 0 and JSF_package.configRoi.halfHalfNC == True:
			pouch = excludinator.clone()


		loopCounter += 1

		corrector = a - 5 + ((loopCounter - numGenotypes)*4)
		clones = ShapeRoi(rm.getRoi(sliceROIs[corrector]))
		center = ShapeRoi(rm.getRoi(sliceROIs[corrector+1]))
		border = ShapeRoi(rm.getRoi(sliceROIs[corrector+2]))
		nc = ShapeRoi(rm.getRoi(sliceROIs[corrector+3]))

		#Here we reformat these values to make sure they are not overwritten
		cas = cas.clone()
		clones = clones.clone()
		center = center.clone()
		border = border.clone()
		nc = nc.clone()

		#Here we find  the intersection between the caspase ROI and the various regions. This is the death in each region
		clones = cas.clone().and(clones)
		clones, isClones = selection_confirmer(clones, iHeight, imp)
		rm.addRoi(clones)

		center = cas.clone().and(center)
		center, isCenter = selection_confirmer(center, iHeight, imp)
		rm.addRoi(center)

		border = cas.clone().and(border)
		border, isBorder = selection_confirmer(border, iHeight, imp)
		rm.addRoi(border)

		nc = cas.clone().and(nc)
		nc, isNc = selection_confirmer(nc, iHeight, imp)
		rm.addRoi(nc)

		#Here we rename each ROI appropriately
		x = rm.getCount()
		rm.select(x - 4)
		casRefArray = casRefArray + [x-5]
		rm.runCommand("Rename", "CaspaseClones_ROI_Z:"+str(stackno)+isClones+"_genotype"+str(loopCounter)+"_timepoint"+str(timepoint))
		rm.select(x - 3)
		rm.runCommand("Rename", "CaspaseCenter_ROI_Z:"+str(stackno)+isCenter+"_genotype"+str(loopCounter)+"_timepoint"+str(timepoint))
		rm.select(x - 2)
		rm.runCommand("Rename", "CaspaseBorder_ROI_Z:"+str(stackno)+isBorder+"_genotype"+str(loopCounter)+"_timepoint"+str(timepoint))
		rm.select(x - 1)
		rm.runCommand("Rename", "CaspaseNotClones_ROI_Z:"+str(stackno)+isNc+"_genotype"+str(loopCounter)+"_timepoint"+str(timepoint))

		if halfHalf == True:
			#Half half caspase
			nonCompeting = excludinator.clone()
			nonCompeting = cas.clone().and(nonCompeting)
			nonCompeting, isNonCompeting = selection_confirmer(nonCompeting, iHeight, imp)
			rm.addRoi(nonCompeting)
			rm.select(x)
			rm.runCommand("Rename", "CaspaseNonCompeting_ROI_Z:"+str(stackno)+isNonCompeting+"_genotype"+str(loopCounter)+"_timepoint"+str(timepoint))
			sliceROIs.extend([x-4, x-3, x-2, x-1, x])
		else:
			sliceROIs.extend([x-4, x-3, x-2, x-1])

		print ("Overlap established")

	return sliceROIs, casMask, casImp, casCasImp, casRefArray
