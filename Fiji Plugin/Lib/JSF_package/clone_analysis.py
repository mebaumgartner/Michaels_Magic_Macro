#
#
#
# This module contains two big functions: clone_analysis, and clone_segmentor
#
#
#


#######################			
# This function is where we perform our overall clone analysis
def clone_analysis(IDs, Title, pouch, excludinator, stackno, iHeight, rm, sliceROIs, borderArray, IDs3, cellROIs, numGenotypes, timepoint, colorArray, seedIDclone, segmentedIDs, genotypeNames, roiMask, iWidth,deluxeCell, fullCloneROIArray):

	#Import user defined variables
	import JSF_package
	from JSF_package.configBasic import fluoChoice, cloneSeg
	from JSF_package import _misc_, caspase_analysis, cell_tracking, clone_analysis, configBasic, configCellTrack, configCloneSeg, configDeathSeg, configDeathTrack, configRoi, configSave, seeded_region_growing, speckles_analysis, start_up, tracking_and_outputs, user_inputs_GUI
	
	from JSF_package.configCloneSeg import subAdd, borderMargin, holeDiameter, rollingZ, winwo, seedChoiceClones, invertSeedClones, minSeedSizeClones
	from JSF_package.configRoi import halfHalf
	
	
	#Import needed modules and functions
	from java.awt import Color, Rectangle
	from ij.plugin import ZProjector
	from ij import IJ
	from JSF_package._misc_ import mask_confirmer, selection_confirmer
	from ij.plugin import ImageCalculator
	from ij.plugin.frame import RoiManager
	from ij.plugin.filter import ParticleAnalyzer
	from ij.measure import ResultsTable, Measurements
	from ij.gui import ShapeRoi
	from random import randrange
	import os

	IJ.redirectErrorMessages(True)
	
	noClones = 0
	Roido = ShapeRoi(1,1, Rectangle(0,0,1,1))
	roidoArchive = ShapeRoi(1,1, Rectangle(0,0,1,1))

	rt = ResultsTable.getResultsTable()
	genotypesImpArray = list()


	#Max Z-project our image
	projection = ZProjector.run(IDs, "MAX", stackno-int(rollingZ), stackno+int(rollingZ))
	
	# use the subAdd variable to do some background cleanup
	IJ.run(projection, "Subtract...", "value="+str(subAdd))
	IJ.run(projection, "Add...", "value="+str(subAdd))

	#Get the right image and calibrations
	IDs.setSlice(stackno)
	cloneImp = IDs.crop()
	cal = cloneImp.getCalibration()
	caliber = cal.pixelHeight

	#Here, if prompted by the user, we create a mask of our pouch/ROIs and treat them as clones
	if JSF_package.configBasic.cloneSeg == "Use ROIs As Clones":

		IJ.setForegroundColor(255, 255, 255)
		cloneImp = IJ.createImage("Resultant", "8-bit black", cloneImp.getWidth(), iHeight+100, 1)
		cloneImp.setCalibration(cal)
		cloneImp.setRoi(pouch)
		IJ.run(cloneImp, "Fill", "")
		IJ.run(cloneImp,"Convert to Mask", "")
		mask_confirmer(iHeight + 20, cloneImp)			
		genotypesImpArray.extend([cloneImp])

		#Change the genotype names accordingly
		genotypeNames = ["Compartment 1", "Compartment 2"]

		#Do the same thing if there is a second compartment
		if JSF_package.configRoi.halfHalfNC == True:

			cloneImp = IJ.createImage("Resultant", "8-bit black", cloneImp.getWidth(), iHeight+100, 1)
			cloneImp.setCalibration(cal)
			cloneImp.setRoi(excludinator)
			IJ.run(cloneImp, "Fill", "")
			IJ.run(cloneImp,"Convert to Mask", "")
			mask_confirmer(iHeight + 20, cloneImp)			
			genotypesImpArray.extend([cloneImp])
				

	#Launch our custom analysis options, if prompted by user
	elif JSF_package.configBasic.cloneSeg != "Membrane-tagged GFP" and JSF_package.configBasic.cloneSeg != "Cytosolic GFP":
		
		#Call the weka classifier and have it segment the images
		if JSF_package.configBasic.cloneSeg.endswith(".model"):
			if JSF_package.configBasic.cloneSeg.find("_3D_.") == -1:
				
			
				
				from trainableSegmentation import WekaSegmentation

				segmentator = WekaSegmentation( cloneImp )
				classifierPath = os.path.join(os.getcwd(), "jars", "Lib", "JSF_package", "Weka_Models", JSF_package.configBasic.cloneSeg )
				segmentator.loadClassifier(classifierPath)
				resultant = segmentator.applyClassifier(cloneImp, 0, False)
				genotypeNames = segmentator.getClassLabels()
				genotypeNames = genotypeNames[1:numGenotypes+1]
				if JSF_package.configRoi.halfHalfNC == True:
			
					genotypeNamesNew = []
					for name in genotypeNames:
						genotypeNamesNew = genotypeNamesNew + [	"ROI_1_"+name, "ROI_2_"+name]
					genotypeNames = genotypeNamesNew

				
	
			else:
				segmentedIDs.setSlice(stackno)
				resultant = segmentedIDs.crop()

		else:
			from JSF_End_User_Made_Code._executor import user_made_code
			resultant = user_made_code(JSF_package.configBasic.cloneSeg, cloneImp, IDs, rm, stackno, pouch, excludinator)
			
		#Call our seeded region growing function, if selected
		if seedChoiceClones == True:

			seedIDclone.setSlice(stackno)

			if JSF_package.configCloneSeg.seedChannelClones == "ROI Mask":
				seedIDclone2 = roiMask
				
			else:
				
				seedIDclone2 = seedIDclone.crop()
				
				#Run a gaussian blur on the image we apply the seeds to, if specified by user
				gaussVal = JSF_package.configCloneSeg.blurClones
				
				if gaussVal != 0:
					IJ.run(seedIDclone2, "Gaussian Blur...", "sigma="+str(gaussVal))

				
			animationStack, regionMask = JSF_package.seeded_region_growing.seededRegionGrowing(seedIDclone2, resultant, 1, iHeight, rm, invertSeedClones, minSeedSizeClones)			
			resultant = regionMask.duplicate()

		#Add optional post-processing step
		if JSF_package.configCloneSeg.clonesPost != "None" and JSF_package.configBasic.cloneSeg.endswith(".model"):
			

			if JSF_package.configCloneSeg.clonesPost == "Despeckle":
				IJ.run(resultant, "Despeckle", "")
			elif JSF_package.configBasic.cloneSeg.find("_3D_.") == -1:
				from JSF_End_User_Made_Code._executor import user_made_code
				resultant = user_made_code(JSF_package.configCloneSeg.clonesPost, resultant, IDs, rm, stackno, pouch, excludinator)
			else:
				from JSF_End_User_Made_Code._executor import user_made_code
				resultant = user_made_code(JSF_package.configCloneSeg.clonesPost, resultant, segmentedIDs, rm, stackno, pouch, excludinator)

			
		activeGenotype = 0
#		if JSF_package.configRoi.halfHalfNC == True:
#			numGenotypes = numGenotypes/2
		
		while activeGenotype <= numGenotypes:
			resultantDup = resultant.duplicate()
		
			#Convert WEKA/custom images to mask			
			if JSF_package.configBasic.cloneSeg != "Membrane-tagged GFP" and JSF_package.configBasic.cloneSeg != "Cytosolic GFP":
				IJ.run(resultantDup, "Select None", "")
		
				
				from ij.process.ImageProcessor import BLACK_AND_WHITE_LUT
				from ij import ImagePlus

				calibration = resultantDup.getCalibration()
				impByte = resultantDup.getProcessor().convertToByte(0)
				impByte.setThreshold(activeGenotype, activeGenotype, BLACK_AND_WHITE_LUT)
				impByte = impByte.createMask()
				resultantDup = ImagePlus("imp", impByte)
				IJ.run(resultantDup, "Canvas Size...", "width="+str(resultantDup.getWidth())+" height="+str(iHeight+100)+" position=Top-Left zero")
				IJ.run(resultantDup, "Select None", "")	
				IJ.run(resultantDup, "Invert", "")
				
				resultantDup.setCalibration(calibration)

				
			
		
				
			else:
				IJ.run(resultantDup,"Convert to Mask", "")
				IJ.run(resultantDup, "Canvas Size...", "width="+str(resultantDup.getWidth())+" height="+str(iHeight+100)+" position=Top-Left zero")
				mask_confirmer(iHeight + 20, resultantDup)


			
			if activeGenotype != 0:

				#This step is a hole filling step. We invert the image, run analyze particles to identify all holes in the mask.			
				IJ.run(resultantDup, "Select None", "")	
				IJ.run(resultantDup, "Invert", "")

		
				
				manager = RoiManager(True)
				ParticleAnalyzer.setRoiManager(manager) 
				pa = ParticleAnalyzer(ParticleAnalyzer.ADD_TO_MANAGER | ParticleAnalyzer.SHOW_NONE,Measurements.AREA,ResultsTable(), 0, holeDiameter/(caliber*caliber), 0.0, 1.0)
				pa.setHideOutputImage(True)
				pa.analyze(resultantDup)
				rm.runCommand(resultantDup,"Show None")
				IJ.run(resultantDup, "Select None", "")
				IJ.run(resultantDup, "Invert", "")	

				manager.runCommand("Deselect")
				aROIs = manager.getSelectedRoisAsArray()
				manager.reset()
				if aROIs:
					op = ShapeRoi(aROIs[0])
					for rp in aROIs:
						rp = ShapeRoi(rp)
						op.or(rp)
					resultantDup.setRoi(op)
					IJ.run(resultantDup, "Clear", "")

			


				if JSF_package.configCloneSeg.clonesPost == "Despeckle":

	
					IJ.run(resultantDup, "Select None", "")
					pa.analyze(resultantDup)
					rm.runCommand(resultantDup,"Show None")
					IJ.run(resultantDup, "Select None", "")
	
					manager.runCommand("Deselect")
					aROIs = manager.getSelectedRoisAsArray()
					manager.reset()
					if aROIs:
						op = ShapeRoi(aROIs[0])
						for rp in aROIs:
							rp = ShapeRoi(rp)
							op.or(rp)
						resultantDup.setRoi(op)
						IJ.setForegroundColor(255, 255, 255)
						resultantDup.setColor(Color.white)
						IJ.run(resultantDup, "Fill", "")
						IJ.run(resultantDup, "Select None", "")
				


		

				genotypesImpArray.extend([resultantDup.duplicate()])
				if JSF_package.configRoi.halfHalfNC == True:
					
					genotypesImpArray.extend([resultantDup.duplicate()])
			else:
				notClonesImp = resultantDup
			activeGenotype += 1

	else:

		numGenotypes = 1
		genotypeName = ["Genotype Names"]
		if JSF_package.configCloneSeg.winwo == True:
			genotypeName = genotypeName + ["Winners"]
		else:
			genotypeName = genotypeName + ["Losers"]
		if JSF_package.configRoi.halfHalfNC == True:
			genotypeName = ["Genotype Names", "Losers", "Winners"]
			numGenotypes = 2
		

	
		#Duplicate the image for the clone analysis
		projection2 = projection.duplicate()

	
		#Process one of the images to be the less detailed hole filler. This mask will preserve less detail, but contain fewer internal holes.
		IJ.run(projection, "Despeckle", "")
	
		#This is the step where we treat the images differently depending on GFP type
		if JSF_package.configBasic.cloneSeg == 'Membrane-tagged GFP':
			IJ.run(projection,"Subtract Background...", "rolling=50 stack")
			IJ.run(projection,"Subtract Background...", "rolling=50 stack")
		else: 
			IJ.run(projection,"Subtract Background...", "rolling=50 sliding stack")
			IJ.run(projection,"Subtract Background...", "rolling=50 sliding stack")
			
		IJ.run(projection,"Median...", "radius=7")
		IJ.run(projection, "Canvas Size...", "width="+str(projection.getWidth())+" height="+str(iHeight+100)+" position=Top-Left zero")
		IJ.run(projection,"Convert to Mask", "")
		mask_confirmer(iHeight + 20, projection)
		IJ.run(projection, "Erode", "")
	
		#Process the other duplicate to preserve more details like the clone contours and what not, but this one has more internal holes
		IJ.run(projection2, "Auto Threshold", "method=Huang dark")
		IJ.run(projection2, "Invert", "")
		IJ.run(projection2, "Canvas Size...", "width="+str(projection2.getWidth())+" height="+str(iHeight+100)+" position=Top-Left zero")
		IJ.run(projection2, "Convert to Mask", "")
		mask_confirmer(iHeight + 20, projection2)
		IJ.run(projection2, "Remove Outliers...", "radius=5 threshold=50 which=Bright")
		IJ.run(projection2, "Remove Outliers...", "radius=2 threshold=50 which=Dark")
		IJ.run(projection2, "Despeckle", "")
	
		#We sum the two images together to preserve detail while also filling holes
		resultant = ImageCalculator().run("Add create", projection, projection2)
	
		#This closes nuclear holes on the edge of the roi
		TNTpresentsTheCloser = resultant.duplicate()
		IJ.run(TNTpresentsTheCloser, "Find Edges", "")
		IJ.run(TNTpresentsTheCloser, "Dilate", "")
		if iHeight > 512:
			IJ.run(TNTpresentsTheCloser, "Dilate", "")
		IJ.run(TNTpresentsTheCloser, "Close-", "")
		if iHeight > 512:
			IJ.run(TNTpresentsTheCloser, "Erode", "")
		resultant = ImageCalculator().run("Add create", resultant, TNTpresentsTheCloser)
		projection.flush()
		projection2.flush()

		#Convert pixel to micron units
		cal = resultant.getCalibration()
		caliber = cal.pixelHeight
		
	
		#This step is a further hole filling step. We invert the image, run analyze particles to identify all holes in the mask.	
		IJ.run(resultant, "Invert", "")
		manager = RoiManager(True)
		ParticleAnalyzer.setRoiManager(manager) 
		pa = ParticleAnalyzer(ParticleAnalyzer.ADD_TO_MANAGER | ParticleAnalyzer.SHOW_NONE,Measurements.AREA,ResultsTable(), 0, holeDiameter/(caliber*caliber), 0.0, 1.0)
		pa.setHideOutputImage(True)
		pa.analyze(resultant)
		rm.runCommand(resultant,"Show None")
		IJ.run(resultant, "Select None", "")
		IJ.run(resultant, "Invert", "")	
	
		#We take all these ROIs and clear them, essentially deleting the holes because the image is an inverting LUT.
		manager.runCommand("Deselect")
		aROIs = manager.getSelectedRoisAsArray()
		manager.reset()
		if aROIs:
			op = ShapeRoi(aROIs[0])
			for rp in aROIs:
				rp = ShapeRoi(rp)
				op.or(rp)
			resultant.setRoi(op)
			IJ.run(resultant, "Clear", "")

		if seedChoiceClones == True:
			seedIDclone.setSlice(stackno)
			seedIDclone2 = seedIDclone.crop()
			IJ.run(resultant, "Grays", "")
			IJ.run(resultant, "Select None", "")
			IJ.run(resultant, "Create Selection", "")
			IJ.run(resultant, "Make Inverse", "")
			IJ.run(resultant, "Set Pixels ...", "value=1")
			IJ.run(resultant, "Canvas Size...", "width="+str(resultant.getWidth())+" height="+str(iHeight)+" position=Top-Left zero")
			animationStack, regionMask = JSF_package.seeded_region_growing.seededRegionGrowing(seedIDclone2, resultant, 1, iHeight, rm, invertSeedClones, minSeedSizeClones)			
			resultant = regionMask.duplicate()
			resultant.getProcessor().setThreshold(1, 1, 0)
			IJ.run(resultant,"Convert to Mask", "")
			IJ.run(resultant, "Canvas Size...", "width="+str(resultant.getWidth())+" height="+str(iHeight+100)+" position=Top-Left zero")
			mask_confirmer(iHeight + 20, resultant)			

		genotypesImpArray.extend([resultant])

		if JSF_package.configRoi.halfHalfNC == True:
			adder = resultant.duplicate()
			genotypesImpArray.extend([adder])

	#We initialize the output images onto which we will draw our clone measurements
	cloneMask = IJ.createImage("cloneMask Z:"+str(stackno), "RGB black", iWidth, iHeight, 1)

	#cloneImp is just the base image we processed
	if seedChoiceClones == True:
		seedIDclone.setSlice(stackno)
		cloneImp = seedIDclone.crop()
	else:
		IDs.setSlice(stackno)
		cloneImp = IDs.crop()
	IJ.run(cloneImp, "RGB Color", "")
	borderM = pouch.clone()
	cloneImp.setRoi(borderM)
	IJ.setForegroundColor(255, 255, 255)
	IJ.run("Line Width...", "line=4")
	IJ.run(cloneImp, "Draw", "")
	
	
	#borderMask is a duplicate of that, though is we do fluorescence analysis, we replace it with the fluorescence channel image
	borderMask = cloneImp.duplicate()	
	if fluoChoice == 1:
		IDs3.setSlice(stackno)
		borderMask = IDs3.crop()
		IJ.run(borderMask, "RGB Color", "")
		borderM = pouch.clone()
		borderMask.setRoi(borderM)
		IJ.setForegroundColor(255, 255, 255)
		IJ.run("Line Width...", "line=4")
		IJ.run(borderMask, "Draw", "")
	if halfHalf == True or JSF_package.configRoi.halfHalfNC == True:
		borderM = excludinator.clone()
		cloneImp.setRoi(borderM)
		IJ.setForegroundColor(255, 255, 0)
		IJ.run(cloneImp, "Draw", "")	

		if fluoChoice == 1:
			borderM = excludinator.clone()
			borderMask.setRoi(borderM)
			IJ.setForegroundColor(255, 255, 0)
			IJ.run(cloneImp, "Draw", "")	
			
	cloneBorderImp = borderMask.duplicate()
	


	loopNC = 0

	genotypeChannel = 1	

	
	for resultant in genotypesImpArray:

	
		
		color = colorArray[genotypeChannel-1]
		
		IJ.run(resultant, 'Make Binary', "")
		
		#We create a selection from the mask
		IJ.run(resultant, "Create Selection", "")
	

		if "Custom" not in JSF_package.configBasic.cloneSeg:
			#If we want ot analyze the unlabelled cells, we just invert this selection if prompted by the user
			if winwo == True:
				IJ.run(resultant, "Make Inverse", "")	
		
		#We do some roi math to just get the intersection of the ROI we get from the mask and the pouch ROI
		#This try/except corrects for if there are no clones at all
		try:
			x = resultant.getRoi()
			x = ShapeRoi(x)
			if(deluxeCell == True):
				fullCloneROIArray[genotypeChannel-1]=fullCloneROIArray[genotypeChannel-1] +[x.clone()]
#				impBordMask = IJ.createImage("Untitled", "8-bit white", iWidth, iHeight, 1)
#				impBordMask.setRoi(x)
#				IJ.setForegroundColor(0, 0, 0)
#				IJ.run("Line Width...", "line=0")
#				IJ.run(impBordMask, "Draw", "")
#				fullCloneROIArray[genotypeChannel-1]=fullCloneROIArray[genotypeChannel-1] +[impBordMask]
		
			pholder = resultant.getRoi()
			pholder = ShapeRoi(pholder)
		except:
			resultant.setRoi(5,iHeight+20,1,1)
			x = resultant.getRoi()
			x = ShapeRoi(x)
			if deluxeCell == True:
				fullCloneROIArray[genotypeChannel-1] = fullCloneROIArray[genotypeChannel-1] +["<empty>"]
				
			pholder = x.clone()
	
		
		y = pouch.clone()
		if JSF_package.configRoi.halfHalfNC == True:
			if genotypeChannel % 2 == 1:
				exc = excludinator.clone()
				y = y.not(exc)
			else:
				y = excludinator.clone()

			
		z= x.and(y)
		peeps = rm.getCount()
		z, isClone = selection_confirmer(z, iHeight, resultant)
		
		
			
		loopNC = 1
		#Here we check if a selection was created. If it was not, there are no clones, we create placeholder clones, and start the the loop again for the next level up.
		if isClone == "<empty>":
	
			#Create placeholder ROIs.
			resultant.setRoi(5,iHeight+20,1,1)
			rm.addRoi(resultant.getRoi())
			rm.addRoi(resultant.getRoi())
			rm.addRoi(resultant.getRoi())
			IJ.run(resultant, "Make Inverse", "")
			rm.addRoi(resultant.getRoi())
			Roido = resultant.getRoi()
			roidoArchive = roidoArchive.or(Roido)
			
			#We give them their appropriate names	
			rm.select(peeps)
			rm.runCommand("Rename", "Clones_ROI_Z:"+str(stackno)+isClone+"_genotype"+str(genotypeChannel)+"_timepoint"+str(timepoint) )
			rm.select(peeps+1)
			rm.runCommand("Rename", "Center_ROI_Z:"+str(stackno)+isClone+"_genotype"+str(genotypeChannel)+"_timepoint"+str(timepoint))
			rm.select(peeps+2)
			rm.runCommand("Rename", "Border_ROI_Z:"+str(stackno)+isClone+"_genotype"+str(genotypeChannel)+"_timepoint"+str(timepoint))
			rm.select(peeps+3)
			rm.runCommand("Rename", "Not_Clones_ROI_Z:"+str(stackno)+isClone+"_genotype"+str(genotypeChannel)+"_timepoint"+str(timepoint))	
			noClones = 1
		else:
		
			if halfHalf == False:
				rm.addRoi(z)
			else:
				toBeExcluded = z.clone()
				toBeExcluded = ShapeRoi(toBeExcluded)
				
				z = toBeExcluded.not(excludinator)
				rm.addRoi(z)
			
			#We now get the shrunken clones to get the clone center ROI. The amount of reduction is determined by the user
			resultant.setRoi(pholder)
			IJ.run(resultant, "Enlarge...", "enlarge=-"+str(borderMargin))
			center = ShapeRoi(resultant.getRoi())
			Roido = z.clone()
			roidoArchive = roidoArchive.or(Roido)
			z1 = z.clone()
			center = z1.and(center)
			center, isCenter = selection_confirmer(center, iHeight, resultant)
	
			#We run an XOR function on the clones and clone center. This is the border region
			border = z.clone()
			center1 = center.clone()
			border = border.xor(center1)
	
			#This is another correction step. If the clones are so large that the enlarge step would eliminate them entirely, theROI manager does nothing
			border, isBorder = selection_confirmer(border, iHeight, resultant)
			if isBorder == "<empty>":
				rm.addRoi(border)	
				rm.addRoi(z)
				isCenter = isBorder
				isBorder = ""
			else:
				rm.addRoi(center)
				rm.addRoi(border)
	
			#rename all of our ROIs	
			rm.select(peeps)
			rm.runCommand("Rename", "Clones_ROI_Z:"+str(stackno)+isClone+"_genotype"+str(genotypeChannel)+"_timepoint"+str(timepoint))
	
			#This is our center ROI
			rm.select(peeps+1)
			rm.runCommand("Rename", "Center_ROI_Z:"+str(stackno)+isCenter+"_genotype"+str(genotypeChannel)+"_timepoint"+str(timepoint))
	
			#This is our border ROI
			rm.select(peeps+2)
			rm.runCommand("Rename", "Border_ROI_Z:"+str(stackno)+isBorder+"_genotype"+str(genotypeChannel)+"_timepoint"+str(timepoint))
			
			borderArray[genotypeChannel-1] = borderArray[genotypeChannel-1] + [peeps+2]

		
			if JSF_package.configBasic.cloneSeg == 'Membrane-tagged GFP' or JSF_package.configBasic.cloneSeg == "Cytosolic GFP" or JSF_package.configBasic.cloneSeg == "Use ROIs As Clones":
			
				#This is our ROI for everything that isnt clones
				if cloneSeg != 'Use ROIs As Clones':
					IJ.run(resultant, "Create Selection", "")
					whatItDo = pouch.clone()
					if JSF_package.configRoi.halfHalfNC == True and genotypeChannel != 1:
						whatItDo = excludinator.clone()
					whatItDo.xor(z)	
	 	
				else:
					whatItDo = None	
				whatItDo, itDoNot = selection_confirmer(whatItDo, iHeight, resultant)


					
				if itDoNot == "<empty>":
					notClones = pouch.clone()
					if JSF_package.configRoi.halfHalfNC == True and genotypeChannel != 1:
						notClones = excludinator.clone()
				
						
				else:
					if winwo == False:
						IJ.run(resultant, "Make Inverse", "")
					notClones = ShapeRoi(resultant.getRoi())
					p = pouch.clone()
					if JSF_package.configRoi.halfHalfNC == True and genotypeChannel != 1:
						p = excludinator.clone()
					notClones = notClones.and(p)
					
			else:
		
				IJ.run(notClonesImp, "Create Selection", "")
				notClones = ShapeRoi(notClonesImp.getRoi())
				notClones, ncCheck = selection_confirmer(notClones, iHeight, notClonesImp)
				if ncCheck == "<empty>":
					resultant.setRoi(5,iHeight+20,1,1)
					rm.addRoi(resultant.getRoi())			

				
			if halfHalf == True:
			
				toBeExcluded = excludinator.clone()
				toBeExcluded = toBeExcluded.clone()
				toBeExcluded = ShapeRoi(toBeExcluded)
				notClones = notClones.not(toBeExcluded)
			rm.addRoi(notClones)
			
				
			rm.select(peeps+3)
			rm.runCommand("Rename", "Not_Clones_ROI_Z:"+str(stackno)+"_genotype"+str(genotypeChannel)+"_timepoint"+str(timepoint))


				
			if halfHalf == True:
				plippity = excludinator.clone()
				rm.addRoi(plippity)
				rm.select(peeps+4)
				rm.runCommand("Rename", "NonCompeting_ROI_Z:"+str(stackno)+"_genotype"+str(genotypeChannel)+"_timepoint"+str(timepoint))


			color = colorArray[genotypeChannel-1]
			#Here we produce our mask images 
			IJ.setForegroundColor(color[0], color[1], color[2])
			cloneMask.setColor(Color(color[0], color[1], color[2]))
			x = rm.getRoi(peeps)
			cloneMask.setRoi(x)
			IJ.run(cloneMask, "Fill", "")

	
			x = rm.getRoi(peeps+1)
			borderMask.setRoi(x)
			xRedux = x
			IJ.setForegroundColor(255, 0, 0)
			if fluoChoice != 1:
				IJ.run(borderMask, "Fill", "")
			IJ.setForegroundColor(color[0], color[1], color[2])
			borderMask.setColor(Color(color[0], color[1], color[2]))
			IJ.run("Line Width...", "line=3")
			IJ.run(borderMask, "Draw", "")
			IJ.setForegroundColor(255, 255, 255)
			cloneMask.setColor(Color.white)
			IJ.run("Line Width...", "line=1")
			IJ.run(borderMask, "Draw", "")
			flo = rm.getRoi(peeps+2)
			borderMask.setRoi(flo)
			IJ.setForegroundColor(color[0], color[1], color[2])
			borderMask.setColor(Color(color[0], color[1], color[2]))
			IJ.run("Line Width...", "line=3")
			IJ.run(borderMask, "Draw", "")
			IJ.setForegroundColor(255, 255, 255)
			cloneMask.setColor(Color.white)
			IJ.run("Line Width...", "line=1")
			IJ.run(borderMask, "Draw", "")
	
			
		
			
			x = rm.getRoi(peeps)
			cloneBorderImp.setRoi(x)
			IJ.setForegroundColor(color[0], color[1], color[2])
			cloneBorderImp.setColor(Color(color[0], color[1], color[2]))
			IJ.run(cloneBorderImp, "Fill", "")
			
			
			IJ.run(cloneBorderImp, "Draw", "")
			IJ.setForegroundColor(255, 255, 255)
			cloneBorderImp.setColor(Color.white)
			IJ.run("Line Width...", "line=1")
			IJ.run(cloneBorderImp, "Draw", "")
			borderM = pouch.clone()
			cloneBorderImp.setRoi(borderM)
			IJ.run("Line Width...", "line=4")
			IJ.run(cloneBorderImp, "Draw", "")

	
		if halfHalf == True:
			sliceROIs.extend([peeps, peeps+1, peeps+2, peeps+3, peeps+4])
		else:
			sliceROIs.extend([peeps, peeps+1, peeps+2, peeps+3])

		genotypeChannel += 1

	if JSF_package.configRoi.halfHalfNC == True:
		Roido = roidoArchive
		
	return Title, genotypesImpArray, noClones, caliber, sliceROIs, borderArray, cloneMask, borderMask, cloneImp, cloneBorderImp, Roido, genotypeNames, fullCloneROIArray
		
##################################
# This is the clone segmentation function
def clone_segmentor (genotypesImpArray, rm, Title, numImages, stackno, pouch, caliber, cloneROIs, timepoint, excludinator):

	from JSF_package.configBasic import minCloneSize
	from JSF_package import _misc_, caspase_analysis, cell_tracking, clone_analysis, configBasic, configCellTrack, configCloneSeg, configDeathSeg, configDeathTrack, configRoi, configSave, seeded_region_growing, speckles_analysis, start_up, tracking_and_outputs, user_inputs_GUI
	
	import JSF_package
	from JSF_package.configCloneSeg import 	DBSCANdist,	DBSCANminDensity, DBSCANmode
	from ij.plugin.frame import RoiManager
	from ij.plugin.filter import ParticleAnalyzer
	from ij.measure import ResultsTable, Measurements
	from ij.gui import ShapeRoi
	import os

	from ij import IJ


	#loop through the clones of each genotype
	count = 0
	corrector = 4*(len(genotypesImpArray)-1)
	a = rm.getCount()
	for resultant in genotypesImpArray:
				

		border = a - 2 - corrector
		corrector = corrector - 4
		pouchRoi = pouch.clone()
		if JSF_package.configRoi.halfHalfNC == True and count > 0:
			pouchRoi = excludinator.clone()

		aROIs = []

		#Here we run the DBSCAN clustering algorithm with biovoxxel's SSIDC plugin
		if DBSCANmode != "Disabled":

			

			#The plugin will delete our ROIs. We save them, so we can retrieve them after we run the plugin
			roiPath = os.path.join(os.getcwd(),"Jars", "Lib", "JSF_package", "workingROIs.zip")
			rm.runCommand("Select All")
			rm.runCommand("save selected", roiPath)
			rm.reset()
			rm.getInstance()

			#Run the algorithm on a duplicate image
			resultant2 = resultant.duplicate()
			IJ.run(resultant2, "Select None", "")
			IJ.run(resultant2, "SSIDC Cluster Indicator", "distance=" + str(DBSCANdist)+" mindensity="+str(DBSCANminDensity))
			resultant2.flush()



			#Pull all of the resulting cluster ROIs from the ROI manager, store as a variable, then reload our previous ROIs
			rm = RoiManager.getInstance()
			rm.runCommand(resultant ,"Show None")
			rm.runCommand("Select All")
			allClusters = rm.getSelectedRoisAsArray()
			rm.reset()
			rm.getInstance()
			rm.runCommand("Open", roiPath)
			IJ.run(resultant, "Select None", "")

			#Now we decide how to handle the clusters. If the user specified flooding, we just fill the cluster ROI on the binary image. 
			#They will then be picked up in the subsequent analyze particles step
			for clusterRoi in allClusters:			
				resultant.setRoi(clusterRoi)

				
				if DBSCANmode != "Cluster Without Flooding":
					IJ.run(resultant, "Clear", "")

				#Otherwise, we run an analyze particles step within these rois, adding the separate areas to the ROI manager as one item
				
				else:

					#Get the intersection with the pouch and ensure that there is an ROI
					rCount = rm.getCount()
					pouchRoiDup = ShapeRoi(pouch.clone())
					clusterRoi = ShapeRoi(clusterRoi)
					clusterRoi = clusterRoi.and(pouchRoiDup)
					b = str(clusterRoi).find("width")
					b = str(clusterRoi)[b+6:b+7]
					if b == "0":
						continue
					yarp = clusterRoi.getLength()
					if yarp < 5:
						continue
					
					resultant2 = resultant.duplicate()
					resultant2.setRoi(clusterRoi)
					IJ.run(resultant2, "Analyze Particles...", "size="+str(minCloneSize/(caliber*caliber))+"-9999999999999999 circularity=0.00-1.00 add")
					resultant2.flush()
					cROIs = []
					while rm.getCount() > rCount:
						newRoi = ShapeRoi(rm.getRoi(rm.getCount() - 1))
						resultant.setRoi(newRoi)
						cROIs = cROIs+[newRoi]
						rm.select(rm.getCount() - 1)
						rm.runCommand("Delete")	
					

					for clusterROI2 in cROIs:
						clusterROI2 = ShapeRoi(clusterROI2)
						resultant.setRoi(clusterROI2)
						IJ.run(resultant, "Fill", "")
					aROIs = aROIs + cROIs

			
		
		#Here we run an analyze particles step. This allows us to identify each clone individually and add it to the roimanager
		#We also keep track of how many ROIs there are before and after this step
		resultant.setRoi(pouchRoi)


		#Create and set up our roiManager
		manager = RoiManager(True)
		ParticleAnalyzer.setRoiManager(manager) 
		pa = ParticleAnalyzer(ParticleAnalyzer.ADD_TO_MANAGER | ParticleAnalyzer.SHOW_NONE,Measurements.AREA,ResultsTable(), minCloneSize/(caliber*caliber), 99999999999999999, 0.0, 1.0)
		pa.setHideOutputImage(True)
		
		pa.analyze(resultant)
		manager.runCommand("Deselect")
		
	
		#We now get all the resulting ROIs from the roi manager as an array
		bROIs = manager.getSelectedRoisAsArray()
		for newRoi in bROIs:
			aROIs = aROIs+[newRoi]
		manager.reset()

		#get the border roi form the general ROI manager
		cloneFullRoi = ShapeRoi(rm.getRoi(border-2))
		
		resultant.setRoi(cloneFullRoi)
		
		y = 1 #y is just a counter we use for tracking indexes through the loop
		newAssignments = list() # newAssignments is a list of all the new ROIs at a given z-level
		#the variable rp ('Roi Presumptive') is an roi from the analyze particles step.
		for rp in aROIs:

			#check for overlap with border ROI to see if it falls in our measurements region
			cloneFullRoiDup = cloneFullRoi.clone()
			rp = ShapeRoi(rp)
			rp = rp.and(cloneFullRoiDup)
			b = str(rp).find("width")
			b = str(rp)[b+6:b+7]
			if b == "0":
				continue
			yarp = rp.getLength()
			if yarp < 5:
				continue
			rm.addRoi(rp)

			#rename new ROI and add it to our assignments list
			T = rm.getCount() - 1
			rm.select(T)
			rm.runCommand("Rename", "SingleClone_Z:"+str(stackno)+"_No."+str(y)+"_genotype"+str(count+1)+"_timepoint"+str(timepoint))
			T = rm.getSelectedIndex()
			newAssignments.extend([T])
			y = y+1


		collate = cloneROIs[count]
		collate= collate + [newAssignments]
		cloneROIs[count] = collate

		count += 1
		
	return cloneROIs, rm
