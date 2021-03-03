#
#
#
# This module contains the function for tracking ROIs by overlap, the image generator, and the measurments and data table outputs
#
#
#


################
#This is where we do our clone assignments. We also use the same algorithm for caspase and cell tracking.
def tracker(cloneROIs, rm, choice):

	from ij.gui import ShapeRoi, OvalRoi
	from ij.plugin.frame import RoiManager

	from ij import IJ
	IJ.redirectErrorMessages(True)



	#This is the total list of all clone assignments. It is a 3d array
	supersetCloneAssignments = list()

	
	#loop through each genotype for all available clones
	for cloneGenotype in cloneROIs:

		
		#cloneROIs is now a 2d list, with each object being a list of all the individual clones in a given z slice
		#This variable counts what z-slice we are on
		zCounter = 0

		totalCloneAssignments = list()

		#Here we loop through the various z-slices
		for zClones in cloneGenotype:
		

			#This gets us just the clones in a given z-slice
			for indClone in zClones:
				
	
				#Skip this round if the roi has already been assigned to a clone
				switcheroo = 0
				for test in totalCloneAssignments:
					if indClone in test:
						switcheroo = 1
				if switcheroo == 1:
					continue
	
				#get the roi from the roi manager
				cl = rm.getRoi(indClone)
	
				#This list logs all the rois attributed to one clone
				fullList = list([indClone])
	
				#This variable tracks if there is overlap between clones
				#Intoduce a loop for all z-levels above the active one
				levelsAbove = cloneGenotype[zCounter + 1:]
				for Zls in levelsAbove:
	
					#This list tracks all ROIs in a single z-level that belong to one clone
					cloneList = list()
	
					#Introduce a loop just for the specific z-level
					for nextZ in Zls:
	
						#get the roi from the roi manager
						cl2 = rm.getRoi(nextZ)
	
						#Now we compare the two clones for overlap
						a = ShapeRoi(cl.clone())
						b = ShapeRoi(cl2.clone())
						c = a.and(b)
						c = str(c)
						d = c.find("width=")
						c = c[d+6:d+7]
	
						#If they do overlap we add the index to our clonelist
						if c != "0":
							cloneList.extend([nextZ])
	
					#We add this list of rois in a given level to a 2d array
					fullList.extend(cloneList)
	
					#If no clone was found, the clone is done and we start over on the next iteration
					if len(cloneList) == 0:
						break
	
					#we reset the reference roi to be the one found in the next level up
					cl = rm.getRoi(cloneList[0])
	
					#If multiple clones are present in this z-slice, the reference clone incorporates all of them
					if len(cloneList) > 0:
						pH = ShapeRoi(cl.clone())
						for i in cloneList:
							roi = ShapeRoi(rm.getRoi(i))
							pH.or(roi)
						cl = pH
	
				#Before we add the clone assignments to the list, we need to make sure these clones did not merge/overlap with any other existing clones.
				if choice != -1:
					merge = 0
					mergeRef = list()
					newList = fullList
		
					#We go through each assignment in the new clone and each assignment in all previously assigned clones and look for an intersection
					for checkeroo in fullList:
						countZ = 0
						for asg in totalCloneAssignments:
							for iZ in asg:
								if checkeroo == iZ:
									mergeRef = mergeRef + [countZ]
									merge = 1
									newList = newList + asg
							countZ = countZ + 1
		
					#If there was no overlap of clones, we add the new clone to the list
					if merge != 1:
						totalCloneAssignments.append(fullList)	
		
					#Otherwise, we take the two overlapping clones, remove duplicates, sort, and add to the clone assignments
					else: 
						newList = set(newList)
						newList = list(newList)
						newList = sorted(newList)
						mergeRef = set(mergeRef)
						mergeRef = list(mergeRef)
						mergeRef = sorted(mergeRef)
						totalCloneAssignments[mergeRef[0]] = newList
						merger = sorted(mergeRef[1:], reverse = True)
						for mR in merger:
							del totalCloneAssignments[mR]
				else:
					totalCloneAssignments.append(fullList)
					
			zCounter = zCounter + 1
		supersetCloneAssignments.append(totalCloneAssignments)

	
	return supersetCloneAssignments


####################################################################################################################
# This does extracts the actual measurements for the whole disc analysis based on the ROIs we have extracted
def whole_disc_measurements(IDs, IDs3, numGenotypes, pouch, sliceROIs, casCasArray, casMaskArray, refStandArray, refOutArray, rm, rt, iHeight, zStart, names, Title, pouchArray, pouch2Array, colorArray, genotypeNames, excludinator, iWidth, IDs4):	

	from JSF_package.configBasic import cloneChannel, fluoChannel, dcp1Choice, fluoChoice, speckle, speckleMethod, dcp1Counting, singleCellMethod
	import JSF_package
	from JSF_package import _misc_, caspase_analysis, cell_tracking, clone_analysis, configBasic, configCellTrack, configCloneSeg, configDeathSeg, configDeathTrack, configRoi, configSave, seeded_region_growing, speckles_analysis, start_up, tracking_and_outputs, user_inputs_GUI
	


	from random import randrange
	from ij import IJ, WindowManager, ImagePlus, ImageStack
	from ij.measure import ResultsTable, Measurements
	from java.awt import Color
	from ij.gui import ShapeRoi
	from ij.gui.Roi import getContourCentroid, isArea
	from JSF_package._misc_ import selection_confirmer
	from ij.plugin.filter import ParticleAnalyzer, MaximumFinder
	from ij.process import ImageProcessor as IPr

	from time import sleep
	IJ.redirectErrorMessages(True)

	#Reset the measurements table and prepare our output headings
	IJ.run("Clear Results")
	rt = ResultsTable.getResultsTable()


	if JSF_package.configRoi.halfHalfNC == True and (JSF_package.configBasic.cloneSeg != "Membrane-tagged GFP" and JSF_package.configBasic.cloneSeg != "Cytosolic GFP" and JSF_package.configBasic.cloneSeg != "Use ROIs As Clones"):
	
		numGenotypes = numGenotypes*2

	#These lists are used for storing the results from the results table
	totalCaspaseList = ["Caspase Area"] # caspase area
	notClonesAreaList = ["Not Clones Area"]
	notClonesCasList = ["Not Clones Caspase Area"]
	notClonesFluoList = ["Not Clone Fluorescence IntDen"]
	fileNamesArray = ["File"]
	imageNamesArray = ["Image Name"]
	zLevelArray = ["Z level"]
	



	#These lists are for holding the measurements for specific clone genotypes. For these, we make an array of arrays, with each sub-array corresponding to a specific clone genotype
	pouchAreaList = ["Pouch Area"]
	pouchFluoList = ["Pouch Fluorescence IntDen"]	
	cloneAreaList = ["Clone Area"] #clone area
	borderAreaList = ["Border Area"] #border area
	centerAreaList = ["Center Area"] #center area
	casBorderList = ["Border Caspase"] # caspase in the border area
	casCenterList = ["Center Caspase"] # caspase in the center area
	cloneCasList = ["Clone Caspase"]
	cloneFluoList = ["Clone Fluorescence IntDen"] # clone fluorescence
	borderFluoList = ["Border Fluorescence IntDen"] #border fluorescence
	centerFluoList= ["Center Fluorescence IntDen"] #clone center fluorescence area
	speckleClonesList = ["#Speckles in clones"]
	speckleBorderList = ["#Speckles in border"]
	speckleCenterList = ["#Speckles in Center"]
	speckleNotClonesList = ["#Speckles in Not Clones"]


	#Add speckle fluorescence and area, if prompted by the user
	if speckleMethod != "Count" and speckle == True:
		speckleClonesFluo = ["Speckle Fluorescence in Clones IntDen"]
		speckleClonesArea = ["Speckle Area in Clones IntDen"]
		speckleCenterFluo = ["Speckle Fluorescence in Center IntDen"]
		speckleCenterArea = ["Speckle Area in Center IntDen"]
		speckleBorderFluo = ["Speckle Fluorescence in Border IntDen"]
		speckleBorderArea = ["Speckle Area in Border IntDen"]
		speckleNotClonesFluo = ["Speckle Fluorescence in Not Clones IntDen"]
		speckleNotClonesArea = ["Speckle Area in Not Clones IntDen"]
		speckleNonCompFluo = ["Speckle Fluorescence Non Competing IntDen"]
		speckleNonCompArea = ["Speckle Area Non Competing IntDen"]

	#Add sub-lists for each genotype
	activeGenotype = 1
	
	while activeGenotype <= numGenotypes:
		genotypeString = "<"+str(genotypeNames[activeGenotype-1])+"_[Genotype:"+str(activeGenotype)+"]>"
		pouchAreaList = pouchAreaList + [[genotypeString]]
		pouchFluoList = pouchFluoList + [[genotypeString]]
		cloneAreaList = cloneAreaList + [[genotypeString]]
		borderAreaList = borderAreaList + [[genotypeString]]
		centerAreaList = centerAreaList + [[genotypeString]]
		cloneCasList = cloneCasList + [[genotypeString]]
		casBorderList = casBorderList + [[genotypeString]]
		casCenterList = casCenterList + [[genotypeString]]
		cloneFluoList = cloneFluoList + [[genotypeString]]
		borderFluoList = borderFluoList + [[genotypeString]]
		centerFluoList = centerFluoList + [[genotypeString]]
		speckleClonesList = speckleClonesList  + [[genotypeString]]
		speckleBorderList = speckleBorderList + [[genotypeString]]
		speckleCenterList = speckleCenterList + [[genotypeString]]
		speckleNotClonesList = speckleNotClonesList + [[genotypeString]]


	
		#Add speckle fluorescence and area, if prompted by the user
		if speckleMethod != "Count" and speckle == True:
			speckleClonesFluo = speckleClonesFluo + [[genotypeString]]
			speckleClonesArea = speckleClonesArea + [[genotypeString]]
			speckleCenterFluo = speckleCenterFluo + [[genotypeString]]
			speckleCenterArea = speckleCenterArea + [[genotypeString]]
			speckleBorderFluo = speckleBorderFluo + [[genotypeString]]
			speckleBorderArea = speckleBorderArea + [[genotypeString]]
		
		activeGenotype += 1

	
	if JSF_package.configRoi.halfHalfNC == True and (JSF_package.configBasic.cloneSeg != "Membrane-tagged GFP" and JSF_package.configBasic.cloneSeg != "Cytosolic GFP" and JSF_package.configBasic.cloneSeg != "Use ROIs As Clones"):
		numGenotypes = numGenotypes/2
	
	#Placeholders for various images and stacks
	cloneMaskStack = 0
	outImp = 0
	outClImp = 0

	#We do all our measurements off of the fluorescence channel, making it easier to get fluorescence outputs
	imp = IDs	
	if (fluoChoice == 1):
		imp = IDs3
	if speckle == True and speckleMethod != "Count":
		impSpeckle = IDs4

		#Here we can do our pre-processing of the fluoresence channel
		if JSF_package.configBasic.preProcess != "None":
			from JSF_End_User_Made_Code._executor import user_made_code
			imp = user_made_code(JSF_package.configBasic.preProcess, imp, IDs3, rm, zStart, pouch, excludinator)	

					
	IJ.run(imp, "Canvas Size...", "width="+str(iWidth)+" height="+str(iHeight+100)+" position=Top-Left zero")


	#We use the sliceROIs variable we've been using to track whole disc analysis ROI indexes to extract all the relevant ROIs as an array.
	rm.setSelectedIndexes(sliceROIs)
	roiArray = rm.getSelectedRoisAsArray()
	
	##########################################################################################################################################
	#We loop through all sliceROIs - these are all the ROIs that are not from the individual clone tracking

	#activeRoi = y, indexCount = z
	zlevelActive = 0
	zLevs = 0
	activeGenotype = 0
	
	indexCount = 0 #this index counts the number of times we have run through this loop
	for activeRoi in roiArray:

		#We extract the Roi title from the active ROI
		roiName = rm.getName(sliceROIs[indexCount])

		#The roi name has all the classification information we need. Here, we extract the z-level the roi is from and store it in the variable 'zLevelRef'
		zLevelRef = roiName
		startIndex = zLevelRef.find("_Z:")

		#We have added a tag to the end of each ROI name - '<empty>' - if the ROI was empty or had no area. We then search for the index of this tag and put it in the variable zEnd.
		#if zEnd is not equal to -1, it means the tag exists and the ROI is empty. If it is equal to -1, then the ROI is not empty. We will use this to know which regions to add measurements for.
		zEnd = zLevelRef.find("<")
		zEndArchive = zEnd
		if zEnd == -1:
			zEnd = zLevelRef.find("_genotype")
		zLevelRef = int(zLevelRef[startIndex+3:zEnd])
		zEnd = zEndArchive


		#Here we extract the genotype of the ROI from the ROI name
		ROIgenotype = roiName.find("genotype")
		endIndex = roiName.find("_timepoint")
		ROIgenotype = int(roiName[ROIgenotype+8:endIndex])
		color = colorArray[ROIgenotype-1]

		#Set the correct slice and roi on the reference image
		imp.setSlice(zLevelRef)
		imp.setRoi(activeRoi)

		if speckle==True and speckleMethod != "Count":
			impSpeckle.setSlice(zLevelRef)
			impSpeckle.setRoi(activeRoi)

		#Here we run our measurements on the image. The measurements are all now in the measurements table
		IJ.run("Clear Results")
		IJ.run(imp, "Measure", "")

		#Ma is the measured area and Mf is the measured fluorescence
		Ma = rt.getValue("Area", 0)
		Mf = rt.getValue("IntDen", 0)

		if speckle==True and speckleMethod != "Count":
			IJ.run(impSpeckle, "Measure", "")
			MfS = rt.getValue("IntDen", 1)

		

		#If we determined that the ROI is empty, we set Ma and Mf to zero
		if zEnd != -1:
			Ma = 0
			Mf = 0

		#We select the relevant dcp1 masks from the image arrays. we are going to draw interesting ROIs on it for the output image
		if dcp1Choice == 1:
			casCasImp = casCasArray[zLevelRef - zStart]
			casCasImp.setRoi(activeRoi)
			casMaskImp = casMaskArray[zLevelRef - zStart]
			casMaskImp.setRoi(activeRoi)

		#We select the relevant speckle analysis image from the image array. We are going to draw interesting ROIs on it for the output image
		if speckle == 1:
			speckleMaskImp = refStandArray[zLevelRef - zStart]
			speckleOverlayImp = refOutArray[zLevelRef - zStart]

		#We now check the name of the ROI to determine what it is we just measured. We then add these values to the array that tracks our output data
		


		#We do the same thing, this time for the ROI corresponding to the clone center		
		if roiName.startswith("Center_") == True:

			#for measurements that are genotype specific, we add it to the appropriate sub-array
			centerAreaList[ROIgenotype] = centerAreaList[ROIgenotype] + [Ma]
			centerFluoList[ROIgenotype] = centerFluoList[ROIgenotype] + [Mf]
			
			if dcp1Choice == 1:
				casCasImp.getProcessor().drawString("Clone Center", 10, 120, Color.orange)
				IJ.setForegroundColor(0,0,0)
				IJ.run("Line Width...", "line=4")
				IJ.run(casCasImp, "Draw", "")
				IJ.setForegroundColor(255, 105, 0)
				IJ.run("Line Width...", "line=2")
				IJ.run(casCasImp, "Draw", "")
				IJ.run(casMaskImp, "Draw", "")

		#Again for the border
		if roiName.startswith("Border_") == True:

			borderAreaList[ROIgenotype] = borderAreaList[ROIgenotype] + [Ma]
			borderFluoList[ROIgenotype] = borderFluoList[ROIgenotype] + [Mf]

		#Again for the non-clones area
		if "Not_Clones_" in roiName and ROIgenotype == 1:

			notClonesFluoList = notClonesFluoList + [Mf]
			notClonesAreaList = notClonesAreaList + [Ma]

		#Now we start measuring for the caspase areas. We are not interested in the fluorescence for these ROIs, so we just measure area.
		#We also draw the ROIs to the output image, depending on the ROI
		#Total caspase measurement
		if "Caspase_" in roiName and ROIgenotype == 1:

			totalCaspaseList = totalCaspaseList + [Ma]	
			if dcp1Choice == 1:

				IJ.setForegroundColor(0, 255, 255)
				casCasImp.setColor(Color.cyan)
				IJ.run(casCasImp, "Fill", "")
				IJ.setForegroundColor(0, 0, 0)
				casCasImp.setColor(Color.black)
				IJ.run("Line Width...", "line=2")
				IJ.run(casCasImp, "Draw", "")
				
				
		#Caspase area in the clones
		if "CaspaseClones" in roiName:

			cloneCasList[ROIgenotype] = cloneCasList[ROIgenotype] + [Ma]

			#Draw yellow border around caspase in clones
			if dcp1Choice == 1:
				casCasImp.getProcessor().drawString("Caspase in Clones", 10, 40, Color.yellow)

				IJ.setForegroundColor(0, 0, 0)
				casCasImp.setColor(Color.black)
				IJ.run("Line Width...", "line=4")
				IJ.run(casCasImp, "Draw", "")
				IJ.setForegroundColor(255, 255, 0)
				casCasImp.setColor(Color.yellow)
				IJ.run("Line Width...", "line=2")
				IJ.run(casCasImp, "Draw", "")

		#Caspase area in the clones
		if "CaspaseCenter" in roiName:

			casCenterList[ROIgenotype] = casCenterList[ROIgenotype] + [Ma]
			if dcp1Choice == 1:
				casCasImp.getProcessor().drawString("Center", 10, 60, Color.blue)

				IJ.setForegroundColor(0, 0, 255)
				casCasImp.setColor(Color.blue)
				IJ.run(casCasImp, "Fill", "")

		#Caspase in the border
		if "CaspaseBorder" in roiName:

			casBorderList[ROIgenotype] = casBorderList[ROIgenotype] + [Ma]
			if dcp1Choice == 1:
				casCasImp.getProcessor().drawString("Border", 10, 80, Color.red)
				casCasImp.setColor(Color.red)
				IJ.run(casCasImp, "Fill", "")

		#Caspase in the non-competing region
		if "CaspaseNotClones" in roiName and ROIgenotype == 1:
			notClonesCasList = notClonesCasList + [Ma]
			
			if dcp1Choice == 1:
				IJ.setForegroundColor(0, 255, 255)
				casCasImp.setColor(Color.cyan)
				IJ.run(casCasImp, "Fill", "")		

		#Now we do the speckles measurements
		if "SpecklesClones" in roiName:

			#Add speckle fluorescence and area, if prompted by the user
			if speckleMethod != "Count":
				speckleClonesFluo[ROIgenotype] = speckleClonesFluo[ROIgenotype] + [MfS]
				speckleClonesArea[ROIgenotype] = speckleClonesArea[ROIgenotype] + [Ma]

	

			#get the speckle count
			refluxCount = JSF_package.speckles_analysis.speckle_counter(activeRoi, iHeight, iWidth)
			speckleClonesList[ROIgenotype] = speckleClonesList[ROIgenotype] + [refluxCount]

			if refluxCount != 0:
				#Draw ROI to speckle imp mask
				IJ.run("Line Width...", "line=1")
				speckleMaskImp.setRoi(activeRoi)
				IJ.run(speckleMaskImp, "Enlarge...", "enlarge=1 pixel")
				speckleMaskImp.setColor(Color(color[0], color[1], color[2]))
				IJ.setForegroundColor(color[0], color[1], color[2])
				IJ.run(speckleMaskImp, "Fill", "slice")
				IJ.setForegroundColor(0,0,0)
				speckleMaskImp.setColor(Color.black)
	
				#Draw ROI to overlay image
				speckleOverlayImp.setRoi(activeRoi)


				IJ.run("Line Width...", "line=1")
				IJ.setForegroundColor(0,255,0)
				speckleOverlayImp.setColor(Color.green)
				IJ.run(speckleOverlayImp, "Enlarge...", "enlarge=2 pixel")
				IJ.run(speckleOverlayImp, "Draw", "slice")
				IJ.setForegroundColor(0,0,0)
				speckleOverlayImp.setColor(Color.black)

				

			speckleOverlayImp.getProcessor().drawString("Clones", 10, 20, Color.green)


		if "SpecklesCenter" in roiName:

			#Add speckle fluorescence and area, if prompted by the user
			if speckleMethod != "Count":
				speckleCenterFluo[ROIgenotype] = speckleCenterFluo[ROIgenotype] + [MfS]
				speckleCenterArea[ROIgenotype] = speckleCenterArea[ROIgenotype] + [Ma]
	
	
		
			#get the speckle count 
			refluxCount = JSF_package.speckles_analysis.speckle_counter(activeRoi, iHeight, iWidth)
			speckleCenterList[ROIgenotype] = speckleCenterList[ROIgenotype] + [refluxCount]

			if refluxCount != 0:

				#Draw ROI to speckle imp mask
				IJ.run("Line Width...", "line=1")
				speckleMaskImp.setRoi(activeRoi)
				speckleMaskImp.setColor(Color.blue)
				IJ.setForegroundColor(0,0,255)
				IJ.run(speckleMaskImp, "Fill", "slice")
				IJ.setForegroundColor(0,0,0)
				speckleMaskImp.setColor(Color.black)
				speckleMaskImp.getProcessor().drawString("Clone Center", 10, 40, Color.blue)
	
	
				speckleOverlayImp.setRoi(activeRoi)
				IJ.run("Line Width...", "line=1")
				IJ.setForegroundColor(0,0,255)
				speckleOverlayImp.setColor(Color.blue)
				IJ.run(speckleOverlayImp, "Enlarge...", "enlarge=2 pixel")
				IJ.run(speckleOverlayImp, "Draw", "slice")
				IJ.setForegroundColor(0,0,0)
				speckleOverlayImp.setColor(Color.black)

			IJ.setForegroundColor(0,0,0)
			speckleOverlayImp.getProcessor().drawString("Clone Center", 10, 40, Color.blue)

		if "SpecklesBorder" in roiName:

			#Add speckle fluorescence and area, if prompted by the user
			if speckleMethod != "Count":
				speckleBorderFluo[ROIgenotype] = speckleBorderFluo[ROIgenotype] + [MfS]
				speckleBorderArea[ROIgenotype] = speckleBorderArea[ROIgenotype] + [Ma]

	
			#get the speckle count 
			refluxCount = JSF_package.speckles_analysis.speckle_counter(activeRoi, iHeight, iWidth)
			speckleBorderList[ROIgenotype] = speckleBorderList[ROIgenotype] + [refluxCount]

			if refluxCount != 0:

				#Draw ROI to speckle imp mask
				IJ.run("Line Width...", "line=1")
				speckleMaskImp.setRoi(activeRoi)
				IJ.setForegroundColor(255,0,0)
				speckleMaskImp.setColor(Color.red)
				IJ.run(speckleMaskImp, "Fill", "slice")
				IJ.setForegroundColor(0,0,0)
				speckleMaskImp.setColor(Color.black)
				speckleMaskImp.getProcessor().drawString("Clone Border", 10, 60, Color.red)

				#Draw ROI to overlay image

				speckleOverlayImp.setRoi(activeRoi)
				IJ.run("Line Width...", "line=1")
				IJ.setForegroundColor(255,0,0)
				speckleOverlayImp.setColor(Color.red)
				IJ.run(speckleOverlayImp, "Enlarge...", "enlarge=2 pixel")
				IJ.run(speckleOverlayImp, "Draw", "slice")
				IJ.setForegroundColor(0,0,0)
				speckleOverlayImp.setColor(Color.black)

			
			IJ.setForegroundColor(0,0,0)
			speckleOverlayImp.getProcessor().drawString("Clone Border", 10, 60, Color.red)
		if "SpecklesNotClones" in roiName:
					
			#Add speckle fluorescence and area, if prompted by the user
			if speckleMethod != "Count":
				speckleNotClonesFluo = speckleNotClonesFluo + [MfS]
				speckleNotClonesArea = speckleNotClonesArea + [Ma]

		
			#get the speckle count 
			refluxCount = JSF_package.speckles_analysis.speckle_counter(activeRoi, iHeight, iWidth)
			speckleNotClonesList[ROIgenotype] = speckleNotClonesList[ROIgenotype] + [refluxCount]	

			if refluxCount != 0:

				#Draw ROI to speckle imp mask
				IJ.run("Line Width...", "line=1")
				speckleMaskImp.setRoi(activeRoi)
				speckleMaskImp.setColor(Color.white)
				IJ.setForegroundColor(255,255,255)
				IJ.run(speckleMaskImp, "Fill", "slice")
				IJ.setForegroundColor(0,0,0)
				speckleMaskImp.setColor(Color.black)
	
				#Draw ROI to overlay image
				speckleOverlayImp.setRoi(activeRoi)
				IJ.run("Line Width...", "line=1")
				IJ.setForegroundColor(255,255,255)
				speckleOverlayImp.setColor(Color.white)
				IJ.run(speckleOverlayImp, "Enlarge...", "enlarge=2 pixel")
				IJ.run(speckleOverlayImp, "Draw", "slice")
				IJ.setForegroundColor(0,0,0)
				speckleOverlayImp.setColor(Color.black)

				
			IJ.setForegroundColor(0,0,0)
			speckleMaskImp.getProcessor().drawString("Not Clones", 10, 80, Color.white)
			speckleOverlayImp.getProcessor().drawString("Not Clones", 10, 80, Color.white)


			
		if roiName.startswith("Clones_") == True:

			cloneAreaList[ROIgenotype] = cloneAreaList[ROIgenotype] + [Ma]
			cloneFluoList[ROIgenotype] = cloneFluoList[ROIgenotype] + [Mf]

			#Draw the clones in the caspase mask
			if dcp1Choice == 1:
				IJ.setForegroundColor(color[0], color[1], color[2])
				casCasImp.getProcessor().drawString("Clones", 10, 20, Color.green)
				casCasImp.getProcessor().drawString("Caspase not in clones", 10, 100, Color.cyan)
				IJ.run("Line Width...", "line=4")
				IJ.run(casCasImp, "Draw", "")
				IJ.run(casMaskImp, "Draw", "")
				IJ.setForegroundColor(0, 255, 0)
				IJ.run("Line Width...", "line=2")
				IJ.run(casCasImp, "Draw", "")
				IJ.run(casMaskImp, "Draw", "")

			#Draw the clones in the speckle masks
			if speckle == 1:
				speckleMaskImp.setRoi(activeRoi)
				speckleOverlayImp.setRoi(activeRoi)
				speckleOverlayImp.setColor(Color.black)
				speckleMaskImp.setColor(Color.black)
				IJ.setForegroundColor(0, 0, 0)
				IJ.run("Line Width...", "line=4")
				IJ.run(speckleMaskImp, "Draw", "")
				IJ.run(speckleOverlayImp, "Draw", "")
				speckleOverlayImp.setColor(Color(color[0], color[1], color[2]))
				speckleMaskImp.setColor(Color(color[0], color[1], color[2]))
				IJ.setForegroundColor(color[0], color[1], color[2])
				IJ.run("Line Width...", "line=2")
				IJ.run(speckleMaskImp, "Draw", "")
				IJ.run(speckleOverlayImp, "Draw", "")

		

		if zLevelRef > zlevelActive:
			pouchActive = pouchArray[zLevs]


			#Set the correct slice and roi on the reference image
			imp.setSlice(zLevelRef)
			imp.setRoi(pouchActive)
	
			#Here we run our measurements on the image. The measurements are all now in the measurements table
			IJ.run("Clear Results")
			IJ.run(imp, "Measure", "")
	
			#Ma is the measured area and Mf is the measured fluorescence
			Ma = rt.getValue("Area", 0)
			Mf = rt.getValue("IntDen", 0)	

			pouchAreaList[1] = pouchAreaList[1]+[Ma]
			pouchFluoList[1] = pouchFluoList[1]+[Mf]


			if JSF_package.configRoi.halfHalfNC == True:
				pouch2Active = pouch2Array[zLevs]
				imp.setRoi(pouch2Active)
		
				#Here we run our measurements on the image. The measurements are all now in the measurements table
				IJ.run("Clear Results")
				IJ.run(imp, "Measure", "")
		
				#Ma is the measured area and Mf is the measured fluorescence
				Ma = rt.getValue("Area", 0)
				Mf = rt.getValue("IntDen", 0)

				pouchAreaList[2] = pouchAreaList[2]+[Ma]
				pouchFluoList[2] = pouchFluoList[2]+[Mf]

			zLevs += 1
			zlevelActive = zLevelRef

		indexCount += 1

	#We make our output table starting here. First thing we do is get all the file names, image names, zLevels, and the pouch area, and put them in arrays
	
	numMeasurements = len(notClonesAreaList)
	loopCounterT = 0
	while loopCounterT < numMeasurements:
		zLevelArray = zLevelArray + [loopCounterT + zStart]
		fileNamesArray = fileNamesArray + [names]
		if names != Title:
			imageNamesArray = imageNamesArray + [Title.replace(names, '')]
		else:
			imageNamesArray = imageNamesArray + [Title]
	
		loopCounterT += 1

	#We make two lists in which we store all the lists of measurements we have made - these are stored in variables LoLa (list of lists a) and LoLb (list of lists b). 
	#LoLa holds all the measurements that don't depend on genotype. LoLb holds all the measurements that do depend on genotype
	LoLa = [fileNamesArray, imageNamesArray, zLevelArray, notClonesAreaList ]
	LoLb = [pouchAreaList,  cloneAreaList, borderAreaList, centerAreaList ]

	if dcp1Choice == 1:
		LoLa = LoLa + [totalCaspaseList, notClonesCasList]
		LoLb = LoLb + [casBorderList, casCenterList, cloneCasList]



	if fluoChoice == 1:
		LoLa = LoLa + [notClonesFluoList]
		LoLb = LoLb + [pouchFluoList, cloneFluoList, borderFluoList, centerFluoList]

	if speckle == True:
		LoLb = LoLb + [speckleClonesList, speckleNotClonesList, speckleBorderList, speckleCenterList]

	if speckleMethod != "Count" and speckle == True:

		LoLb = LoLb + [speckleClonesFluo, speckleClonesArea, speckleCenterFluo, speckleCenterArea, speckleBorderFluo, speckleBorderArea]
		LoLa = LoLa + [speckleNotClonesFluo, speckleNotClonesArea]
	return LoLa, LoLb


###################################################################################################################################################################
#Generate summary table
def create_summary_table(rtS, LoLa, LoLb, LoLc, timepoint):

	from JSF_package import _misc_, caspase_analysis, cell_tracking, clone_analysis, configBasic, configCellTrack, configCloneSeg, configDeathSeg, configDeathTrack, configRoi, configSave, seeded_region_growing, speckles_analysis, start_up, tracking_and_outputs, user_inputs_GUI
	
	from JSF_package.configBasic import timelapse
	from ij import IJ
	IJ.redirectErrorMessages(True)

	#create a superset of LoLa and LoLb, we keep track of where the switch between the two lists occurred by getting the length of LoLa
	superset = LoLa + LoLb 

	switch = len(LoLa)
	switch2 = len(superset)

	superset = LoLa+LoLb+list(LoLc)
	if timelapse == True:
		superset = superset + ["TimePoint"]
	
	#Get the numnber of measurements by taking the file list and getting the count
	numMeasurements = len(LoLa[0]) - 1
	
	#Add the headings from LoLa. The first value in each list is the header string. We only add the header if measurements were added to that list (length of the list is greater than 1)

	
	count = 0
	rtS.incrementCounter()
	for item in superset:
		if item == "TimePoint":
			rtS.addValue("TimePoint", timepoint)
			continue

		if count <= switch:
			if len(item) > 1:
				header = item[0]
				if header == "File" or header == "Image Name":
					itemToAdd = item[1]
					count += 1
				elif header == "Z level":
					continue		
				else:
					itemToAdd = 0
					for obj in item[1:]:
						itemToAdd += obj
				rtS.addValue(header, itemToAdd)
		else:
			baseName = item[0]
			subItems = item[1:]
			
			for sub in subItems:
				
				if len(sub) > 1:
					genotype = sub[0]
					header = str(baseName + genotype)
					if count < switch2:
						itemToAdd = 0
						for obj in sub[1:]:
							
							itemToAdd += obj
					else:
						itemToAdd = sub[1]
					rtS.addValue(header, itemToAdd)
		count += 1

				
	return rtS
####################################################################################################################################################################
# Genrate whole disc table from LoLa and LoLb measurement tables
def create_whole_disc_table(rtD, LoLa, LoLb, LoLc, timepoint):

	from JSF_package.configBasic import timelapse
	from ij import IJ
	IJ.redirectErrorMessages(True)
	from JSF_package import _misc_, caspase_analysis, cell_tracking, clone_analysis, configBasic, configCellTrack, configCloneSeg, configDeathSeg, configDeathTrack, configRoi, configSave, seeded_region_growing, speckles_analysis, start_up, tracking_and_outputs, user_inputs_GUI
	
	#create a superset of LoLa and LoLb, we keep track of where the switch between the two lists occurred by getting the length of LoLa
	superset = LoLa + LoLb 

	switch = len(LoLa)
	switch2 = len(superset)

	superset = LoLa+LoLb+list(LoLc)
	if timelapse == True:
		superset = superset + ["TimePoint"]
	
	#Get the numnber of measurements by taking the file list and getting the count
	numMeasurements = len(LoLa[0]) - 1
	
	#Add the headings from LoLa. The first value in each list is the header string. We only add the header if measurements were added to that list (length of the list is greater than 1)
	measurementCount = 1
	while measurementCount < numMeasurements:
		
		count = 0
		rtD.incrementCounter()
		for item in superset:
			if item == "TimePoint":
				rtD.addValue("TimePoint", timepoint)
				continue

			if count < switch:
				if len(item) > 1:
					header = item[0]
					itemToAdd = item[measurementCount]
					rtD.addValue(header, itemToAdd)
			else:
				baseName = item[0]
				subItems = item[1:]
				for sub in subItems:
					if len(sub) > 1:
						genotype = sub[0]
						header = str(baseName + genotype)
						if count < switch2:
							itemToAdd = sub[measurementCount]
						else:
							itemToAdd = sub[1]
						rtD.addValue(header, itemToAdd)
			count += 1

				
		measurementCount += 1
	return rtD

###########################################################################################################################################################################
def tracking_measurements(IDs, IDs3,trackingArray, casMaskArray, cloneMaskArray, iHeight, numGenotypes, borderArray, rm, zStart, cloneTrackingArray, names, Title, rt, pouchHeight, casRefArray, cloneImpArray, colorArray, pouchArray, pouch2Array, genotypeNames, pouch, excludinator, iWidth, timepoint, rtDeluxe, deluxeCell, fullCloneROIArray):

	from JSF_package.configBasic import cloneChannel, fluoChannel, fluoChoice, dcp1Choice, cloneTracking
	from random import randrange
	from ij import IJ, WindowManager, ImagePlus, ImageStack
	from ij.measure import ResultsTable, Measurements
	from java.awt import Color
	from ij.gui import ShapeRoi, Line
	from ij.gui.Roi import getContourCentroid, isArea
	from JSF_package._misc_ import selection_confirmer
	from ij.plugin.filter import ParticleAnalyzer, MaximumFinder
	import JSF_package
	from JSF_package import _misc_, caspase_analysis, cell_tracking, clone_analysis, configBasic, configCellTrack, configCloneSeg, configDeathSeg, configDeathTrack, configRoi, configSave, seeded_region_growing, speckles_analysis, start_up, tracking_and_outputs, user_inputs_GUI
	

	IJ.redirectErrorMessages(True)

	#ToBeRemoved <- True



	
	#placeholder variables
	outImp = 0
	outClImp = 0
	cloneMaskStack = 0
	cloneLoLa = 0
	cloneLoLb = 0
	skipper=False
	

	#We do all our measurements off of the fluorescence channel, making it easier to get fluorescence outputs
	imp = IDs	
	if (fluoChoice == 1):
		imp = IDs3


		#Here we can do our pre-processing of the fluoresence channel
		if JSF_package.configBasic.preProcess != "None":
			from JSF_End_User_Made_Code._executor import user_made_code
			imp = user_made_code(JSF_package.configBasic.preProcess, imp, IDs3, rm, zStart, pouch, excludinator)	
		
	IJ.run(imp, "Canvas Size...", "width="+str(iWidth)+" height="+str(iHeight+100)+" position=Top-Left zero")
	


	#Initialize caspase and cell tracking variables
	totalCloneCount = ["Total Number of Clones"]
	totalCasCount = ["Total Caspase Cells"]
	borderCasCount = ["Caspase Count in Border"]
	centerCasCount = ["Caspase Count in Center"]
	totalCellCount = ["Total Cell count"]
	borderCount = ["Cell Count in Border"]
	centerCount = ["Cell Count in Center"]

	if JSF_package.configRoi.halfHalfNC == True and (JSF_package.configBasic.cloneSeg != "Membrane-tagged GFP" and JSF_package.configBasic.cloneSeg != "Cytosolic GFP" and JSF_package.configBasic.cloneSeg != "Use ROIs As Clones"):
		numGenotypes = numGenotypes*2

	#Deluxe cell tracking variables
	if deluxeCell == True:


		imageNameListCell = ["Image Name"]
		genotypeListCell =["Single Cell Genotype"]
		cellIDList = ["Single Cell ID"]
		isBorderList = ["Is in Border"] #True for border cell, false for center cell
		isDCP1positiveList = ["Is DCP1 positive"] #True for Caspase positive cell, false for non-dying cell
		cellZLevel = ["Single Cell Z Level"] #This is the middle z-level (as it will be segmented in 3D. If there is a tie, winner is whichever z-level has a greater area.
		distanceToCentroid = ["Distance from pouch centroid"] #This measures the distance of the single cell from the center of the ROI
		distanceToPerimeter = ["Distance from clone perimeter"] #This measures the distance of the single cell to the clone perimeter

		if fluoChoice == True:
			centroidFluorescence = ["Fluorescence at single cell centroid disc"]
	
	#headings is a list of all the column headings. This first set of headings are universal.
	
	imageNameList = ["Image Name"]
	zLevelList = ["Z-level"]
	pouchAreaList = ["Pouch Area"]
		
	pouchHeightList = ["Pouch Height"]
	
	#These are the headings  that are repeated once per genotype of clones
	cloneIdList = ["Clone ID"]
	cloneAreaList = ["Clone Area"]
	borderAreaList = ["Border Area"]
	centerAreaList = ["Center Area"]
	casList = ["Caspase Coverage"]
	casBorderList = ["Caspase Border"]
	casCenterList = ["Caspase Center"]
	feretMaxList = ["FeretMax"]
	feretMinList = ["FeretMin"]
	cloneFluoList = ["Clone Fluorescence IntDen"]
	borderFluoList = ["Border Fluorescence IntDen"]
	centerFluoList = ["Center Fluorescence IntDen"]
	genotypeList = ["Clone Genotype"]
	
	activeGenotype = 0
	while activeGenotype < numGenotypes:
		genotypeString = "<"+str(genotypeNames[activeGenotype-1])+"_[Genotype:"+str(activeGenotype)+"]>"
		totalCasCount = totalCasCount + [0]
		borderCasCount = borderCasCount + [0]
		centerCasCount = centerCasCount + [0]
		totalCellCount = totalCellCount+[0]
		borderCount = borderCount + [0]
		centerCount = centerCount + [0]
		totalCloneCount = totalCloneCount+[0]
		activeGenotype+=1

#	if deluxeCell == True:
#		activeGenotype = 0
#		while activeGenotype < numGenotypes:
#			cellIDList = cellIDList + [[genotypeString]]
#			isBorderList = isBorderList + [[genotypeString]]
#			isDCP1positiveList = isDCP1positiveList + [[genotypeString]]
#			cellZLevel = cellZLevel + [[genotypeString]]
#			distanceToCentroid = distanceToCentroid + [[genotypeString]]
#			distanceToPerimeter = distanceToPerimeter + [[genotypeString]]
#			if fluoChoice == True:
#				centroidFluorescence = centroidFluorescence + [[genotypeString]]
#			activeGenotype+=1
	
	if JSF_package.configRoi.halfHalfNC == True and (JSF_package.configBasic.cloneSeg != "Membrane-tagged GFP" and JSF_package.configBasic.cloneSeg != "Cytosolic GFP" and JSF_package.configBasic.cloneSeg != "Use ROIs As Clones"):
		numGenotypes = numGenotypes/2

	#the trackCount variable tracks which loop/ analysis we are doing. it starts at 2, doing the caspase tracking. At 1, it does the individual clone tracking. At 0, it does the individual cell counting.
	trackCount = 2 

	#This is a placeholder for the clone tracking output image
	outImpCT = 0

	#This is a placeholder for the dcp1 trackign output image
	outImpDcp1 = 0

	#This is a placeholder for the singleCell tracking output 
	outImpSC = 0

	cloneMaskStack = ImageStack(iWidth, iHeight)
	cellID = 1
	#Here we loop through the tracking array, which is a list of 3 lists. Each list will consist of ROI indices, or it will say 'Skip' if that particular analysis is not being performed
	for assignments in trackingArray: 

		#If the analysis is not to be performed, we restart the loop for the next analysis
		if assignments == "Skip":
			trackCount = trackCount - 1
			continue

		#Here we create our output image stacks. These vary based on which output is being made
		outImp = ImageStack(iWidth, iHeight)

		x = 0
		if trackCount == 1:
			while x < len(cloneTrackingArray):
				outImp.addSlice(cloneTrackingArray[x].getTitle(), cloneTrackingArray[x].getProcessor())
				cloneMaskStack.addSlice(cloneMaskArray[x].getTitle(), cloneMaskArray[x].getProcessor())
				x = x + 1
		elif trackCount == 2:
			while x < len(casMaskArray):
				top = casMaskArray[x]
				outImp.addSlice(casMaskArray[x].getTitle(), casMaskArray[x].getProcessor())
				x = x + 1
		else:
			while x < len(cloneImpArray):
				outImp.addSlice(cloneImpArray[x].getTitle(), cloneImpArray[x].getProcessor())
				x = x + 1
		outImp = ImagePlus("outImp", outImp)
		#if (deluxeCell == True) and trackCount == 0:
		outImpSC = outImp.duplicate()
		
		#ti stands for tracking index. It keeps track of which object we are tracking. This is used for labelling each object in the output images
		ti = 1
		

		#This is all for the individual clone tracking
		if trackCount == 1:
	

			#Create an image stack to which we weill write the output clone tracking masks
			clDup = cloneMaskStack.duplicate()
			outClImp = ImagePlus("outClImp", clDup)

			
		
		skipcellcount = 0
		
		#Pull the clone arrays from the 2d array. We are now looping through each group of ROIs batched together as a clone or cell
		count = 0

	

		ROIgenotype=0
		for genotype in assignments:

			
			ROIgenotype += 1
			totalCount = 0 #This variable tracks our total number of cells/clones/cell death events

			
			for clone in genotype:

				#When we are assigning cells or capsase cells to either the border or the center, we run into a problem. What if a cell object is in the border at one z-level and the center at another z-level?
				#The solution here is to say that if the cell object is in the border at more z-levels than its in the center, we count it as a border cell. 
				#Uhold counts the number of times an object is marked as border and Chold counts the number of times its marked as center
				Uhold = 0
				Chold = 0
				Jhold = 0
				skipCell = False
				
	
				#set a random color to uniquely identify each cell/clone/caspase event in the output images
				color = colorArray[(ROIgenotype-1)%100]
				rVal = int(randrange(0,255))
				gVal = int(randrange(0,255))
				bVal = int(randrange(0,255))
				outImp.setColor(Color(rVal, gVal, bVal))
				IJ.setForegroundColor(rVal, gVal, bVal)
				
					


				#Get each ROI per clone
				#Add to total count?
				totalCountAdd = False


				
				if (JSF_package.configBasic.dcp1Deluxe == True and deluxeCell == True and trackCount != 1) or (JSF_package.configBasic.cellCountDeluxe == True and deluxeCell == True and trackCount != 1):

					
	
					#Find the z-level of each individual cell
					zList = list()
					switch = 1
					for c in clone:
					

						#Get the pouch ROI
						rdr=ShapeRoi(rm.getRoi(c))
						bName = rm.getName(c)
						zFinder = bName.find("_Z:")
						zEnd = bName.find("_No.")
						bName = bName[zFinder+3:zEnd]
					
						zList = zList +[int(bName)]
						
						if (switch==1):
							cumulative = rdr
						else:
							cumulative = cumulative.or(rdr)
						switch=0

					#Get the centroid of the cell
					xCoordCell, yCoordCell = cumulative.getContourCentroid()

					
					
	
					#For deluxe cell counting, we get the median Z-plane value
					zList = sorted(zList)
					medianZ =  zList[int(len(zList)/2)]
			

				
				for c in clone:
		
					#Get the border ROIs for comparison

					
					for border in borderArray[ROIgenotype-1]:
	
						#Determine if this border is at the correct z-level
					
						if c > border:
							zBorder = border
						if c < border:
							break
	
					#Get the total caspase ROI for comparison. We will use this for determing caspase coverage of individual clones
					if (dcp1Choice == 1):
						zCas = casRefArray[0]
						casO = 0
						for casR in casRefArray:
							if c < casR and c > casO:
								zCas = casR
							casO = casR
						



					#We extract the ROIs from these indices
					cloneROI = rm.getRoi(c)
					cloneROI = cloneROI.clone()
					cloneROI = ShapeRoi(cloneROI)
					borderROI = rm.getRoi(zBorder)
					borderROI = borderROI.clone()
					borderROI = ShapeRoi(borderROI)


					#Get the pouch ROI
					bName = rm.getName(c)
					zFinder = bName.find("_Z:")
					zEnd = bName.find("_No.")
					bName = bName[zFinder+3:zEnd]
					idexPouch = int(bName) - zStart
					pouchROI = pouchArray[idexPouch]


					if JSF_package.configRoi.halfHalfNC == True and ROIgenotype % 2 != 1:
						pouchROI = pouch2Array[idexPouch]

					pouchROI = pouch.clone()
					pouchROI = ShapeRoi(pouchROI)

					#If we do deluxe cell tracking, we get the centroid of the pouch ROI for measuring distance to centroid
					if (JSF_package.configBasic.dcp1Deluxe == True and deluxeCell == True and trackCount != 1) or (JSF_package.configBasic.cellCountDeluxe == True and deluxeCell == True and trackCount != 1):
					
						
						if int(bName) == medianZ:
							
							xCoordPouch, yCoordPouch = pouchROI.getContourCentroid()
							
							if JSF_package.configRoi.halfHalfNC == True:
								pouchCombo1 = ShapeRoi(pouchArray[idexPouch])
								pouchCombo2 = ShapeRoi(pouch2Array[idexPouch])
								pouchCombo = ShapeRoi(pouchCombo1.or(pouchCombo2))
								xCoordPouch, yCoordPouch = pouchCombo.getContourCentroid()

								
							#Get the x and y coordinates of the closest point on the clone perimeter
							perimeterROI = fullCloneROIArray[ROIgenotype-1]
							perimeterROI=perimeterROI[int(bName) -zStart]
							xCoordClone, yCoordClone = JSF_package._misc_.find_minimum_distance(perimeterROI, xCoordCell, yCoordCell)
					
							
							if (fluoChoice == True):
								imp.setSlice(int(bName))
								imp.setRoi(c)
								
								IJ.run(imp, "Measure", "")
								count = count + 1
								centroidFluorescence[[genotypeString]] = centroidFluorescence[[genotypeString]] + rt.getValue("Mean", count)


					#Check if the ROI overlaps with the Pouch region at all 
					pouchChecker = cloneROI.clone()
					overlapROI = pouchChecker.and(pouchROI)
					overlapROI, isOverlap = selection_confirmer(overlapROI, iHeight, imp)
					if isOverlap != '<empty>':
						pouchOverlap = overlapROI.size()
						totAreaP = cloneROI.size()
						if pouchOverlap > totAreaP/4:
							totalCountAdd = True

					#get the center ROI
					centerROI = rm.getRoi(zBorder - 1)
					centerROI = centerROI.clone()
					centerROI = ShapeRoi(centerROI)

					#For the individual cell and cell death tracking
					if trackCount != 1:

						#get not clone ROI
						notCloneROI = rm.getRoi(zBorder + 1)
						notCloneROI = notCloneROI.clone()
						notCloneROI = ShapeRoi(notCloneROI)
					
						#For dcp1 tracking and cell tracking, cloneROI is a bit of a misnomer. Here, it is the cellROI. We want to check for overlap of the cell ROI with the border
						borderChecker = cloneROI.clone()
						
						#Get the area of the ROI. this is how we check if it is more in one area or the other.
						totArea = borderChecker.size()
						borderOverlap = 0
						centerOverlap = 0
						notClonesOverlap = 0
	
						#Get the overlap between the cell and the border ROIs
						borderDuplicate = borderROI.clone()
						borderChecker.and(borderDuplicate)
	
						#Here we check to see if there is any overlap. If x, y, width, and height are zero, then there is no ROI
						#Otherwise, there is some level of overlap, and we set U to 1. If U is one, there is border overlap, if U is zero, there is not
						bobo = str(borderChecker)
						U = 0
						if bobo.find("x=0") == -1:
							U = 1
						if bobo.find("y=0") == -1:
							U = 1
						if bobo.find("width=0") == -1:
							U = 1
						if bobo.find("height=0") == -1:
							U = 1
						if U == 1:
							borderOverlap = borderChecker.size()
	
						#For dcp1 tracking, we check for center overlap. This is the exact same setup as for the border
						#The only difference is that C is the variable instead of U.
						centerChecker = cloneROI.clone()
						centerDuplicate = centerROI.clone()
						centerChecker.and(centerDuplicate)
						bobo = str(centerChecker)
						C = 0
						if bobo.find("x=0") == -1:
							C = 1
						if bobo.find("y=0") == -1:
							C = 1
						if bobo.find("width=0") == -1:
							C = 1
						if bobo.find("height=0") == -1:
							C = 1
						if C == 1:
							centerOverlap = centerChecker.size()

						if JSF_package.configRoi.halfHalfNC == True or (ROIgenotype == 1):
							notClonesChecker = cloneROI.clone()
							notClonesDuplicate = notCloneROI.clone()
							notClonesChecker.and(notClonesDuplicate)
							bobo = str(notClonesChecker)
							J = 0
							if bobo.find("x=0") == -1:
								J = 1
							if bobo.find("y=0") == -1:
								J = 1
							if bobo.find("width=0") == -1:
								J = 1
							if bobo.find("height=0") == -1:
								J = 1
							if J == 1:
								notClonesOverlap = notClonesChecker.size()
	
						#We don't want to include cells or caspase cells that are just barely in the clone region, as it is tough to say these are definitively in agiven clone region. We therefore set a filter called 'overlapFilt'
						#This sets the fraction of a cell object that must be in a given region for it to be counted
						overlapFilt = 0.8
						if trackCount == 2:
							overlapFilt = 0.5
	
						#The total overlap for a cell, that is the fraction that is in the border + the fraction that is in the center, must be greater than overlapFilt to be counted and added to Chold or Uhold
						if (centerOverlap + borderOverlap) >= (totArea * overlapFilt):
	
							#Here, we figure out if the cell object is more in one area than the other. This is a tiebreaker for cell objects that are partially in both the clone border and the clone center
							if (centerOverlap > (totArea/4)) and (borderOverlap > (totArea/4)):
								if centerOverlap > borderOverlap:
									Chold = Chold + 1
								else:
									Uhold = Uhold + 1
							elif centerOverlap > (totArea/4):
								Chold = Chold + 1
							elif borderOverlap > (totArea/4):
								Uhold = Uhold + 1
							elif notClonesOverlap > (totArea/4):
								Jhold = Jhold + 1
						elif notClonesOverlap > (totArea/4):
									Jhold = Jhold + 1
	
						#Extract the caspase ROI, if applicable.
						if (dcp1Choice == 1):
							caspaseROI = rm.getRoi(zCas)
							caspaseROI = caspaseROI.clone()
							caspaseROI = ShapeRoi(caspaseROI)
	
							if (trackCount == 0):
								casChecker = cloneROI.clone()
								casTotArea = casChecker.size()
								casDuplicate = caspaseROI.clone()
								casChecker.and(casDuplicate)
			
								#Here we check to see if there is any overlap. If x, y, width, and height are zero, then there is no ROI
								#Otherwise, there is some level of overlap, and we set U to 1. If U is one, there is border overlap, if U is zero, there is not
								bobo = str(casChecker)
								Ctest = 0
								if bobo.find("x=0") == -1:
									Ctest = 1
								if bobo.find("y=0") == -1:
									Ctest = 1
								if bobo.find("width=0") == -1:
									Ctest = 1
								if bobo.find("height=0") == -1:
									Ctest = 1
								if Ctest == 1:
									casOverlap = casChecker.size()
								
									if casOverlap > (casTotArea/2):
										skipCell = True
										skipcellcount += 1
										xEx, yEx = cloneROI.getContourCentroid()
										outImp.getProcessor().drawString(str("e"+str(skipcellcount)),int(xEx), int(yEx), Color.black)
								

	
					#Get the border and center ROIs
					if trackCount == 1:
						#Get the clone border
						borderInd = cloneROI.clone()
						bROI = borderROI.clone()
						borderInd.and(bROI)
	
						#Get the clone center
						centerInd = cloneROI.clone()
						cROI = centerROI.clone()
						centerInd.and(cROI)

											
						
					#Find the caspase overlap in the center and border
					if (dcp1Choice == 1) and (trackCount == 1):
					
						#Get caspase in clone
						caspaseROI = rm.getRoi(zCas)
						caspaseROI = ShapeRoi(caspaseROI)
						casClone = cloneROI.clone()
						casRef = caspaseROI.clone()
						casClone.and(casRef)
		
						#Get caspase in border
						casBorder = borderInd.clone()
						casRef = caspaseROI.clone()
						casBorder.and(casRef)
		
						#Get caspase in center
						casCenter = centerInd.clone()
						casRef = caspaseROI.clone()
						casCenter.and(casRef)
	
					#Set the rois on the correct image and z-level
					bName = rm.getName(zBorder)
					zFinder = bName.find("_Z:")
					zEnd = bName.find("<")
					if zEnd == -1:
						if bName.startswith("Casp") == True:
							zEnd = len(bName)
						else:
							zEnd = bName.find("_genotype")
					bName = bName[zFinder+3:zEnd]
	
					#These are all the clone tracking-specific measurements
					if trackCount == 1:
						if JSF_package.configRoi.halfHalfNC == True:
							if ROIgenotype == 1:
							
								idexPouch = int(bName) - zStart
								pouchActive = pouchArray[idexPouch]
						
						
								#Set the correct slice and roi on the reference image
								imp.setSlice(int(bName))
								imp.setRoi(pouchActive)
						
								#Here we run our measurements on the image. The measurements are all now in the measurements table
								IJ.run(imp, "Measure", "")
								count = count + 1
						
								#Ma is the measured area and Mf is the measured fluorescence
								Ma = rt.getValue("Area", count)
					
								pouchAreaList = pouchAreaList+[Ma]
			
							else:
						
								pouch2Active = pouch2Array[idexPouch]
								imp.setRoi(pouch2Active)
						
								#Here we run our measurements on the image. The measurements are all now in the measurements table
								
								IJ.run(imp, "Measure", "")
								count = count + 1
						
								#Ma is the measured area and Mf is the measured fluorescence
								Ma = rt.getValue("Area", count)
				
								pouchAreaList = pouchAreaList+[Ma]
	
						
						imp.setSlice(int(bName))
						
						#Add values to array 
						imageNameList = imageNameList + [names + Title]
						genotypeList = genotypeList + [ROIgenotype]
						cloneIdList = cloneIdList + [ti]
						zLevelList = zLevelList + [bName]
						
	
						#Measure the clone
						imp.setRoi(cloneROI) 
						IJ.run(imp, "Measure", "")
						Ma = rt.getValue("Area", count)
						Mmax = rt.getValue("Feret", count)
						Mmin = rt.getValue("MinFeret", count)
						Mf = rt.getValue("IntDen", count)
						
						
						cloneAreaList = cloneAreaList + [Ma]
						pouchHeightList = pouchHeightList + [pouchHeight]
						cloneFluoList = cloneFluoList + [Mf]
						count = count + 1
	
						#Measure the border
						imp.setRoi(borderInd)
						borderInd, isBorder = selection_confirmer(borderInd, iHeight, imp)
						if isBorder != "<empty>":
							IJ.run(imp, "Measure", "")
							Ma = rt.getValue("Area", count)
							Mf = rt.getValue("IntDen", count)
							count = count + 1
							borderAreaList= borderAreaList + [Ma]
							borderFluoList = borderFluoList + [Mf]
						else:
							borderAreaList= borderAreaList + [0]
							borderFluoList = borderFluoList + [0]
		
						#Measure the center
						imp.setRoi(centerInd)
						centerInd, isCenter = selection_confirmer(centerInd, iHeight, imp)
						if isCenter != "<empty>":
							IJ.run(imp, "Measure", "")
							Ma = rt.getValue("Area", count)
							Mf = rt.getValue("IntDen", count)
							centerAreaList = centerAreaList + [Ma]
							centerFluoList = centerFluoList + [Mf]
							count = count + 1
						else:
							centerAreaList = centerAreaList + [0]
							centerFluoList = centerFluoList + [0]	
											
					if (dcp1Choice == 1) and (trackCount == 1):
	
						#Measure the caspase area
						imp.setRoi(casClone)
						casClone, isCas = selection_confirmer(casClone, iHeight, imp)
						if isCas != "<empty>":
							IJ.run(imp, "Measure", "")
							Ma = rt.getValue("Area", count)
							casList = casList + [Ma]
							count = count + 1
						else: 
							casList = casList + [0]
	
						#Measure the caspase area
						imp.setRoi(casBorder)
						casBorder, isCasB = selection_confirmer(casBorder, iHeight, imp)
						if isCasB != "<empty>":
							IJ.run(imp, "Measure", "")
							Ma = rt.getValue("Area", count)
							casBorderList = casBorderList + [Ma]
							count = count + 1
						else:
							casBorderList = casBorderList + [0]
					
						#Measure the caspase area
						imp.setRoi(casCenter)
						casCenter, isCasC = selection_confirmer(casCenter, iHeight, imp)
						if isCasC != "<empty>":
							IJ.run(imp, "Measure", "")
							Ma = rt.getValue("Area", count)
							count = count + 1
							casCenterList = casCenterList   + [Ma]	
						else:
							casCenterList   = casCenterList  + [0]	
	
					#Draw the output image
					xCoord, yCoord = getContourCentroid(cloneROI)
					outImp.setSlice(int(bName)-int(zStart) + 1)
	
					if trackCount == 1:
						if isBorder != "<empty>":
							IJ.setForegroundColor(rVal, gVal, bVal)
							outImp.setRoi(borderInd)			
							IJ.run(outImp, "Fill", "slice")
							if isCenter != "<empty>":
								outImp.setRoi(centerInd)
								IJ.run("Line Width...", "line=3")
								outImp.setColor(Color.black)
								IJ.setForegroundColor(0, 0, 0)
								IJ.run(outImp, "Draw", "slice")
								IJ.run("Line Width...", "line=1")
								outImp.setColor(Color(rVal, gVal, bVal))
								IJ.setForegroundColor(rVal, gVal, bVal)
								IJ.run(outImp, "Draw", "slice")
	
	

	
					#Draw clone tracking output image
					if trackCount == 1:
						outClImp.setColor(Color(rVal, gVal, bVal))
						outClImp.setColor(Color(rVal, gVal, bVal))
						IJ.setForegroundColor(rVal, gVal, bVal)
						outImp.getProcessor().drawString(str(ti),int(xCoord), int(yCoord), Color.black)
						outClImp.setRoi(cloneROI)
						outClImp.setSlice(int(bName)-int(zStart) + 1)
						IJ.run("Line Width...", "line=3")
						IJ.run(outClImp, "Draw", "slice")
						outClImp.getProcessor().drawString(str(ti),int(xCoord), int(yCoord), Color.black)


						if dcp1Choice == 1:
							if isCas != "<empty>":	
								outImp.setRoi(casClone)
								outImp.setColor(Color.white)
								IJ.run(outImp, "Fill", "slice")
								if isCasB != "<empty>":
									outImp.setRoi(casBorder)
									IJ.run("Line Width...", "line=3")
									outImp.setColor(Color.red)
									IJ.setForegroundColor(255, 0, 0)
									IJ.run(outImp, "Draw", "slice")
									IJ.run("Line Width...", "line=1")
									outImp.setColor(Color(rVal, gVal, bVal))
									IJ.setForegroundColor(rVal, gVal, bVal)
									IJ.run(outImp, "Draw", "slice")
		
								if isCasC != "<empty>":
									outImp.setRoi(casCenter)
									IJ.run("Line Width...", "line=3")
									outImp.setColor(Color.green)
									IJ.setForegroundColor(0, 255, 0)
									IJ.run(outImp, "Draw", "slice")
									IJ.run("Line Width...", "line=1")
									outImp.setColor(Color(rVal, gVal, bVal))
									IJ.setForegroundColor(rVal, gVal, bVal)
									IJ.run(outImp, "Draw", "slice")						
	
					#Draw cell tracking output image
					elif trackCount == 0:
						outImp.setRoi(cloneROI)
						outImp.setSlice(int(bName)-int(zStart) + 1)
						outImp.setColor(Color(color[0], color[1], color[2]))
						IJ.setForegroundColor(color[0], color[1], color[2])
						outImp.setColor(Color(color[0], color[1], color[2]))
						IJ.run("Line Width...", "line=3")
						if (Uhold >= Chold) and (Uhold > 0):
							IJ.run(outImp, "Draw", "slice")
							outImp.setColor(Color(rVal, gVal, bVal))
							IJ.setForegroundColor(rVal, gVal, bVal)
							IJ.run(outImp, "Fill", "slice")
							outImp.setColor(Color(255,0,0))
							outImp.setColor(Color.blue)
							IJ.setForegroundColor(255,0,0)
							IJ.run("Line Width...", "line=1")
							IJ.run(outImp, "Draw", "slice")
						elif Chold > Uhold:
							IJ.run(outImp, "Draw", "slice")
							outImp.setColor(Color(rVal, gVal, bVal))
							IJ.setForegroundColor(rVal, gVal, bVal)
							IJ.run(outImp, "Fill", "slice")
							outImp.setColor(Color(0,0,255))
							outImp.setColor(Color.blue)
							IJ.setForegroundColor(0,0, 255)
							IJ.run("Line Width...", "line=1")
							IJ.run(outImp, "Draw", "slice")
						elif Jhold > 0:
							IJ.run(outImp, "Fill", "slice")
							outImp.setColor(Color(rVal, gVal, bVal))
							IJ.setForegroundColor(rVal, gVal, bVal)
							IJ.run(outImp, "Draw", "slice")
							IJ.setForegroundColor(255,255,255)
							IJ.run("Line Width...", "line=1")
							IJ.run(outImp, "Draw", "slice")
							
						
	
					#Draw caspase tracking output image
					else:
					
						outImp.setRoi(cloneROI)
						outImp.setSlice(int(bName)-int(zStart) + 1)
						IJ.setForegroundColor(color[0], color[1], color[2])
						IJ.setForegroundColor(color[0], color[1], color[2])
						IJ.run("Line Width...", "line=3")
						
						if (Uhold >= Chold) and (Uhold > 0):
							IJ.run(outImp, "Draw", "slice")
							outImp.setColor(Color.red)
							IJ.setForegroundColor(255,0,0)
							IJ.run("Line Width...", "line=1")
							IJ.run(outImp, "Draw", "slice")
							outImp.getProcessor().drawString(str(borderCasCount[ROIgenotype] + 1),int(xCoord), int(yCoord), Color.black)
						elif Chold > Uhold:
							IJ.run(outImp, "Draw", "slice")
							outImp.setColor(Color.blue)
							IJ.setForegroundColor(0,0,255)
							IJ.run("Line Width...", "line=1")
							IJ.run(outImp, "Draw", "slice")
							outImp.getProcessor().drawString(str(centerCasCount[ROIgenotype] + 1),int(xCoord), int(yCoord), Color.black)
						elif Jhold > 0:
							IJ.run(outImp, "Draw", "slice")
							IJ.setForegroundColor(255,255,0)
							IJ.setForegroundColor(255,255,0)
							IJ.run("Line Width...", "line=1")
							IJ.run(outImp, "Draw", "slice")
							
						

				if totalCountAdd == True:
					totalCount += 1
				if (skipper == False) or (trackCount == 1):
					ti = ti+1

			
	
				#Add our border/center individual counts
				skipper = True
				if trackCount == 2:
					if (Uhold >= Chold) and (Uhold > 0):
						borderCasCount[ROIgenotype] = borderCasCount[ROIgenotype] + 1
						skipper = False
						if skipCell == False:
							cellID += 1
					elif Chold > Uhold :
						centerCasCount[ROIgenotype] = centerCasCount[ROIgenotype] + 1
						skipper = False
						if skipCell == False:
							cellID += 1
				if trackCount == 0:
					if (Uhold >= Chold) and (Uhold > 0):
						borderCount[ROIgenotype] = borderCount[ROIgenotype] + 1
						skipper = False
						if skipCell == False:
							cellID += 1
					elif Chold > Uhold :
						centerCount[ROIgenotype] = centerCount[ROIgenotype] + 1
						skipper = False
						if skipCell == False:
							cellID += 1

				#Add our deluxeCell counts if applicable
				if ((JSF_package.configBasic.dcp1Deluxe == True and deluxeCell == True and trackCount != 1) or (JSF_package.configBasic.cellCountDeluxe == True and deluxeCell == True and trackCount != 1)) and skipper==False and skipCell==False:

				
						
						imageNameListCell = imageNameListCell + [Title]
			
						cellIDList = cellIDList + [cellID]
						genotypeListCell = genotypeListCell + ["<"+str(genotypeNames[ROIgenotype-1])+"_[Genotype:"+str(ROIgenotype)+"]>"]
			
						cellZLevel = cellZLevel + [int(medianZ)-int(zStart)]

						if (Uhold >= Chold) and (Uhold > 0):
							isBorderList = isBorderList +[True]
							outImp.setColor(Color.red)
						else:
							isBorderList = isBorderList +[False]
							outImp.setColor(Color.blue)
							IJ.setForegroundColor(0, 0, 255)
						if trackCount == 0:
							isDCP1positiveList = isDCP1positiveList + [0]
						else:
							isDCP1positiveList = isDCP1positiveList + [1]

						distance = Line(xCoordCell, yCoordCell, xCoordPouch, yCoordPouch)
						imp.setRoi(distance)
					
						
						distance2 = imp.getRoi()
						distanceVal = distance2.getLength()
						if xCoordClone != "<empty>":
							distanceBord = Line(xCoordCell, yCoordCell, xCoordClone, yCoordClone)
						else:
							distanceBord = float("NaN")
						imp.setRoi(distanceBord)
						distanceBord2 = imp.getRoi()
						distanceBordVal = distanceBord2.getLength()

				
						outImp.setSlice(int(medianZ)-int(zStart)+1)
						outImp.setRoi(distanceBord)
						IJ.setForegroundColor(255,255,0)
						IJ.run(outImp, "Draw", "slice")
						IJ.setForegroundColor(255,255,255)
						if trackCount == 0:
							outImpSC.setSlice(int(medianZ)-int(zStart) + 1)
							outImpSC.setRoi(distance)
						
							IJ.run(outImpSC, "Draw", "slice")

						else:
							outImp.setRoi(distance)
							IJ.run(outImp, "Draw", "slice")


	


						distanceToCentroid = distanceToCentroid + [distanceVal]
						distanceToPerimeter = distanceToPerimeter + [distanceBordVal]

			#Add our overall counts, regardless of border/center
			if trackCount == 2:
				totalCasCount[ROIgenotype] = totalCount
			elif trackCount == 0:
				totalCellCount[ROIgenotype] = totalCount
			
			else:
				totalCloneCount[ROIgenotype] = totalCount

						
		if trackCount == 1:
			outImpCT = outImp
		elif trackCount == 2:
			outImpDcp1 = outImp	
		else:
			outImp = outImpCT
	
		trackCount = trackCount - 1

	count = 0
	for item in borderCasCount:
		if count != 0:
			labelled = ["Genotype_"+str(count)]+[item]
			borderCasCount[count] = labelled
		count += 1
	count = 0
	for item in centerCasCount:
		if count != 0:
			labelled = ["Genotype_"+str(count)]+[item]
			centerCasCount[count] = labelled
		count += 1
	count = 0
	for item in borderCount:
		if count != 0:
			labelled = ["Genotype_"+str(count)]+[item]
			borderCount[count] = labelled
		count += 1
	count = 0
	for item in centerCount:
		if count != 0:
			labelled = ["Genotype_"+str(count)]+[item]
			centerCount[count] = labelled
		count += 1

	count = 0
	for item in totalCasCount:
		if count != 0:
			labelled = ["Genotype_"+str(count)]+[item]
			totalCasCount[count] = labelled
		count += 1

	count = 0
	for item in totalCellCount:
		if count != 0:
			labelled = ["Genotype_"+str(count)]+[item]
			totalCellCount[count] = labelled
		count += 1

	count = 0
	for item in totalCloneCount:
		if count != 0:
			labelled = ["Genotype_"+str(count)]+[item]
			totalCloneCount[count] = labelled
		count += 1
		


	LoLc = totalCloneCount, totalCasCount, borderCasCount, centerCasCount, totalCellCount, borderCount, centerCount
	
 	if cloneTracking == 1:
		cloneLoLa = [imageNameList, zLevelList, pouchAreaList, pouchHeightList, genotypeList]
		cloneLoLb = [cloneIdList, cloneAreaList, borderAreaList, centerAreaList, casList, casBorderList, casCenterList, feretMaxList, feretMinList, cloneFluoList, centerFluoList, borderFluoList]
	cellLoLa = 0
	cellLoLb = 0
	if deluxeCell ==True:
		if fluoChoice == 1:
			cellLoL= ["Title", "TimePoint", cellIDList , genotypeListCell, cellZLevel , isBorderList , isDCP1positiveList , distanceToCentroid, distanceToPerimeter, centroidFluorescence ]
		else:
			cellLoL= ["Title", "TimePoint", cellIDList , genotypeListCell, cellZLevel , isBorderList , isDCP1positiveList , distanceToCentroid, distanceToPerimeter]
	
		

		

		for item in cellLoL:
			print "item= ", item

			
		measurementCount = 1
		while measurementCount < len(cellIDList):
			count = 0
			rtDeluxe.incrementCounter()
			

			for item in cellLoL:
				if item == "TimePoint":
					rtDeluxe.addValue("TimePoint", timepoint)
					continue
				if item == "Title":
					rtDeluxe.addValue("Image Name", Title)
					continue
					
				if len(item) > 1:
				
					header = item[0]
					itemToAdd = item[measurementCount]
					rtDeluxe.addValue(header, itemToAdd)

			measurementCount += 1

	
	
		rtDeluxe.show("Deluxe Cell Counting Results")


	
	return outImp, outClImp, cloneMaskStack, cloneLoLa, cloneLoLb, LoLc, rtDeluxe, outImpSC
	

###########################################
#This script makes the composite image
def image_generator(cloneMaskStack, borderMaskArray, casMaskArray, cloneImpArray, fluoImpArray, casImpArray, iHeight, casCasArray, cloneBorderArray, cloneTrackingArray, outImp, outClImp, refBaseArray, refOutArray, refStandArray, iWidth, outImpSC, deluxeCell):

	from JSF_package.configBasic import fluoChoice, dcp1Choice, speckle, cloneTracking
	from JSF_package import _misc_, caspase_analysis, cell_tracking, clone_analysis, configBasic, configCellTrack, configCloneSeg, configDeathSeg, configDeathTrack, configRoi, configSave, seeded_region_growing, speckles_analysis, start_up, tracking_and_outputs, user_inputs_GUI
	
	from ij import IJ, ImagePlus, ImageStack
	from ij.gui import ShapeRoi, TextRoi, OvalRoi
	from ij.plugin import StackCombiner

	IJ.redirectErrorMessages(True)

	
	
	#Create variables for the stacks we will add images to
	cloneStack = ImageStack(iWidth, iHeight)
	if fluoChoice == 1:
		fluoStack = ImageStack(iWidth, iHeight)
	cloneBorderStack = ImageStack(iWidth, iHeight)
	borderStack = ImageStack(iWidth, iHeight)
	if dcp1Choice == 1:
		casStack = ImageStack(iWidth, iHeight)
		casMaskStack = ImageStack(iWidth, iHeight)
		casCasStack = ImageStack(iWidth, iHeight)
	if speckle == 1:
		speckleStack = ImageStack(iWidth, iHeight)
		speckleOverlayStack = ImageStack(iWidth, iHeight)
		speckleMaskStack = ImageStack(iWidth, iHeight) 
	
	
	x = 0
	while x < len(cloneImpArray):
		cloneStack.addSlice(cloneImpArray[x].getTitle(), cloneImpArray[x].getProcessor())
		if fluoChoice == 1:
			fluoStack.addSlice(fluoImpArray[x].getTitle(), fluoImpArray[x].getProcessor())
		borderStack.addSlice(borderMaskArray[x].getTitle(), borderMaskArray[x].getProcessor())
		if (deluxeCell == False):
			cloneBorderStack.addSlice(cloneBorderArray[x].getTitle(), cloneBorderArray[x].getProcessor())

		if dcp1Choice == 1:
			casStack.addSlice(casImpArray[x].getTitle(), casImpArray[x].getProcessor())
			casMaskStack.addSlice(casMaskArray[x].getTitle(), casMaskArray[x].getProcessor())
			casCasStack.addSlice(casCasArray[x].getTitle(), casCasArray[x].getProcessor())
		if speckle == 1:
			speckleStack.addSlice(refBaseArray[x].getTitle(), refBaseArray[x].getProcessor())
			speckleOverlayStack.addSlice(refOutArray[x].getTitle(), refOutArray[x].getProcessor())
			speckleMaskStack.addSlice(refStandArray[x].getTitle(), refStandArray[x].getProcessor())
			
		x = x + 1

	cloneOutStack = StackCombiner().combineHorizontally(cloneStack, borderStack)
	if deluxeCell == True:
		outImpSC=outImpSC.getStack()
		cloneOutStack = StackCombiner().combineHorizontally(cloneOutStack, outImpSC)
	else:
		cloneOutStack = StackCombiner().combineHorizontally(cloneOutStack, cloneBorderStack)

	if cloneTracking == 1:
		outImp = outImp.getStack()
		outClImp = outClImp.getStack()

		try:
			cloneMaskStack = cloneMaskStack.getStack()
		except:
			IJ.log( "Could not retrieve clone mask stack")
		if fluoChoice == 1:
			ctOutStack = StackCombiner().combineHorizontally(fluoStack, outImp)
		else:
			ctOutStack = StackCombiner().combineHorizontally(cloneMaskStack, outImp)
		ctOutStack = StackCombiner().combineHorizontally(ctOutStack, outClImp)
		cloneOutStack = StackCombiner().combineVertically(cloneOutStack, ctOutStack)

	if dcp1Choice == 1:
		casOutStack = StackCombiner().combineHorizontally(casStack, casMaskStack)
		casOutStack = StackCombiner().combineHorizontally(casOutStack, casCasStack)
		cloneOutStack = StackCombiner().combineVertically(cloneOutStack, casOutStack)

	if speckle == 1:
		speckleOutStack = StackCombiner().combineHorizontally(speckleStack, speckleMaskStack)
		speckleOutStack = StackCombiner().combineHorizontally(speckleOutStack, speckleOverlayStack)
		cloneOutStack = StackCombiner().combineVertically(cloneOutStack, speckleOutStack)

	imp = ImagePlus("Display Panel", cloneOutStack)

	return imp

##################################################################
# This function takes the clone measurements and produces an output table
def create_clone_table(rtC, cloneLoLa, cloneLoLb, timepoint):

	from JSF_package.configBasic import timelapse
	from JSF_package import _misc_, caspase_analysis, cell_tracking, clone_analysis, configBasic, configCellTrack, configCloneSeg, configDeathSeg, configDeathTrack, configRoi, configSave, seeded_region_growing, speckles_analysis, start_up, tracking_and_outputs, user_inputs_GUI
	
	from ij import IJ
	IJ.redirectErrorMessages(True)
	#create a superset of LoLa and LoLb, we keep track of where the switch between the two lists occurred by getting the length of LoLa

	superset = cloneLoLa + cloneLoLb 


	if timelapse == True:
		superset = superset + ["TimePoint"]
	
	#Get the numnber of measurements by taking the file list and getting the count
	numMeasurements = len(cloneLoLa[0]) - 1
	
	#Add the headings from LoLa. The first value in each list is the header string. We only add the header if measurements were added to that list (length of the list is greater than 1)
	measurementCount = 1
	while measurementCount < numMeasurements:
		
		count = 0
		rtC.incrementCounter()
		for item in superset:
			if item == "TimePoint":
				rtC.addValue("TimePoint", timepoint)
				continue
			if len(item) > 1:
				header = item[0]
				itemToAdd = item[measurementCount]
				rtC.addValue(header, itemToAdd)

			count += 1

				
		measurementCount += 1
	return rtC	



				
				
				
		

	

	