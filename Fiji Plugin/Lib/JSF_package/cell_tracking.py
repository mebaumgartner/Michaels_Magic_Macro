#
#
#
# This function is where we segment all the individual cells within CD8-GFP marked clones
#
#
#


def cell_tracking(cellROIs, IDs, stackno, Roido, rm, iHeight, rt, IDs5, DAPIfilter, seedIDcell, pouch, excludinator, segmentedIDsCC, wekaSkipImage):
#c filterWeight customSegmentationCells ):

	from JSF_package.configCellTrack import minCellSize, cellCountRadius, morphoSeg, filterWeight, seedChoiceCell, seedChannelCell, maxCellSize, minCircularity, maxCircularity
	import JSF_package
	from JSF_package import _misc_, caspase_analysis, cell_tracking, clone_analysis, configBasic, configCellTrack, configCloneSeg, configDeathSeg, configDeathTrack, configRoi, configSave, seeded_region_growing, speckles_analysis, start_up, tracking_and_outputs, user_inputs_GUI
	from JSF_package.configBasic import cloneSeg, singleCellMethod, cloneChannel, cellCountChannel
	
	#Import necessary packages
	from inra.ijpb.morphology import Strel, Morphology, MinimaAndMaxima
	from inra.ijpb.binary import BinaryImages
	from inra.ijpb.watershed import Watershed
	from ij import IJ, ImagePlus
	from ij.plugin import ImageCalculator
	from ij.plugin.filter import ParticleAnalyzer
	from ij.measure import ResultsTable, Measurements
	from JSF_package._misc_ import mask_confirmer
	from ij.gui import ShapeRoi, OvalRoi
	from ij.plugin.frame import RoiManager
	import os

	IJ.redirectErrorMessages(True)
	#Get the clone channel image from the correct z-plane
	IDs.setSlice(stackno)
	ccImp = IDs.crop()
	ccImpRaw = ccImp.duplicate()
	cal = ccImp.getCalibration()
	caliber = cal.pixelHeight
	cellCountRadius = cellCountRadius/caliber


	if JSF_package.configBasic.DAPIchannel != "Disabled":
		#Get the DAPI channel image (we will use this for filtering our cell ROIs)
		IDs5.setSlice(stackno)
		nImp = IDs5.crop()

	if JSF_package.configBasic.singleCellMethod == "Default":


	
		#Reset the results panel. We will use it again shortly
		IJ.run("Clear Results")
	
		#These two steps reduce background and enhance the membane GFP prominence
		IJ.run(ccImp, "Despeckle", "")
		IJ.run(ccImp, "Unsharp Mask...", "radius=2 mask=0.60")
	
		#We run a bottom hat operation. This gives us a pretty good image of the unlabelled GFP parts of the cells. 
		IJ.run(ccImp, "Gray Scale Attribute Filtering", "operation=[Bottom Hat] attribute=Area minimum=250 connectivity=4")
		ccImp3 = IJ.getProcessor()
		IJ.runMacro("run('Close All')")
		ccImp3 = ImagePlus("bottomHat", ccImp3)
	
		#We subtract the bottom hat image from our regular image. The resulting image shohuld have very low signal in the cell nuclei, allowing for a better watershed
		ccImp = ImageCalculator().run("Subtract create", ccImp, ccImp3)
		ccImp2 = ccImp.duplicate()
	
		#This step helps us fill in regions that have GFP signal but no clear cells
		IJ.run(ccImp2, "Subtract Background...", "rolling=3 create sliding")
		ccImp = ImageCalculator().run("Add create", ccImp, ccImp2)
	
		#Smooth the image to prevent over-segmentation with the watershed
		IJ.run(ccImp, "Gaussian Blur...", "sigma=.2 scaled")
	
		#We use the closing function from MorphoLibJ to fill in spots of low signal in the GFP that are too small to be cells.
		strel = Strel.Shape.OCTAGON.fromRadius( 1 )
		ip = Morphology.closing( ccImp.getProcessor(), strel )
		ccImp = ImagePlus("imp", ip )
	
		#The watershed algorithm works pretty well, but we still get some ROIs on high-GFP regions of the image.
		#To solve this, we employ a fluorescence intensity filter. Any ROI with too high of fluorescence is deemed not to be a real cell
		#We take our filtered image, measure the average mean pixel value over the clones and get the standard deviation
		#The filter is the mean minus half the standard deviation. The user can scale up or down this filter in the advanced settings panel (the variable is 'filterWeight')
		
		ccImpRaw.setRoi(Roido)
		IJ.run(ccImpRaw, "Measure", "")
		threshFilter = (rt.getValue("Mean", 0) - (rt.getValue("StdDev", 0) * 0.5))*filterWeight 

		#We now run our extended minima and watershed function from MorphoLibJ
		regionalMinima = MinimaAndMaxima.extendedMinima( ccImp.getProcessor(), morphoSeg, 4 )
		imposedMinima = MinimaAndMaxima.imposeMinima(ccImp.getProcessor(), regionalMinima, 4)
		labeledMinima = BinaryImages.componentsLabeling( regionalMinima, 4, 32 )
		ip_segmented = Watershed.computeWatershed( imposedMinima, labeledMinima, None, 4, True)
		ccImp = ImagePlus("imp_MorphSegmented",ip_segmented)	

	
	
		#This function gives our an image with the segmented regions, but its in a weird format, so we play around with it until we have a nice 8-bit binary image

		IJ.run(ccImp, "Multiply...", "value=9999999999999")		
		IJ.run(ccImp, "8-bit", "")
		IJ.run(ccImp, "Make Binary", "")
		mask_confirmer(iHeight + 20, ccImp)	
		IJ.run(ccImp, "Invert", "")
		IJ.run(ccImp, "Erode", "")	
		
	

	else:
		threshFilter = 999999999999
		

		if JSF_package.configBasic.singleCellMethod.endswith(".model"):
			
		
			
			if JSF_package.configBasic.singleCellMethod.find("_3D_.") == -1:
			
				
#				ccImp.show()
#				exit()
				if (cloneSeg == singleCellMethod) and (cloneChannel == cellCountChannel):
					resultant = wekaSkipImage.duplicate()
					wekaSkipImage = None
					
				else:
				
					from trainableSegmentation import WekaSegmentation
					
					wekaCal = ccImp.getCalibration()
					segmentator = WekaSegmentation( ccImp )
					classifierPath = os.path.join(os.getcwd(), "jars", "Lib", "JSF_package", "Weka_Models", JSF_package.configBasic.singleCellMethod )
					segmentator.loadClassifier(classifierPath)
					resultant = segmentator.applyClassifier(ccImp, 0, False)
					del segmentator
					resultant.setCalibration(wekaCal)
				
#				resultant.show()
#				exit()
				
			else:
				
				segmentedIDsCC.setSlice(stackno)
				resultant = segmentedIDsCC.crop()

		else:
			
			
			
			from JSF_End_User_Made_Code._executor import user_made_code
			resultant = user_made_code(JSF_package.configBasic.singleCellMethod, ccImp, IDs, rm, stackno, pouch, excludinator)

			
			
		if seedChoiceCell == True:
			seedIDcell.setSlice(stackno)
			seedIDcell2 = seedIDcell.crop()


			#Run a gaussian blur on the image we apply the seeds to, if specified by user
			gaussVal = JSF_package.configCellTrack.blurCell
			if gaussVal != 0:
				IJ.run(seedIDcell2, "Gaussian Blur...", "sigma="+str(gaussVal))
			
			animationStack, regionMask = JSF_package.seeded_region_growing.seededRegionGrowing(seedIDcell2, resultant, 1, iHeight, rm, minSeedSizeCell, invertSeedClonesCell)			
			resultant = regionMask.duplicate()

		if JSF_package.configCellTrack.cellPost != "None" and JSF_package.configBasic.singleCellMethod.endswith(".model"):
			
			

			if JSF_package.configCellTrack.cellPost == "Despeckle" or JSF_package.configCellTrack.cellPost == "Despeckle and Watershed":
				IJ.run(resultant, "Despeckle", "")
			elif JSF_package.configCloneSeg.clonesPost == 'Dilate, close, erode':
				print "Dilate, close, erode selected"	
			elif JSF_package.configBasic.singleCellMethod.find("_3D_.") == -1:
				from JSF_End_User_Made_Code._executor import user_made_code
				resultant = user_made_code(JSF_package.configCellTrack.cellPost, resultant, IDs, rm, stackno, pouch, excludinator)
			else:
				from JSF_End_User_Made_Code._executor import user_made_code
				resultant = user_made_code(JSF_package.configCellTrack.cellPost, resultant, segmentedIDs, rm, stackno, pouch, excludinator)
			
			
		
		resultant.getProcessor().setThreshold(1, 1, 0)
		IJ.run(resultant,"Convert to Mask", "")
		
		
		if JSF_package.configCellTrack.cellPost == 'Dilate, close, erode':
			IJ.run(resultant, "Dilate", "")
			IJ.run(resultant, "Dilate", "")
			IJ.run(resultant, "Close-", "")
			IJ.run(resultant, "Erode", "")
	
		
		if JSF_package.configCellTrack.cellPost == "Despeckle and Watershed":
			IJ.run(resultant, "Remove Outliers...", "radius=2 threshold=50 which=Dark")
			IJ.run(resultant, "Watershed", "")
		
		ccImp = resultant

	#Make sure the mask was made correctly
	iWidth = ccImp.getWidth()
	IJ.run(ccImp, "Canvas Size...", "width="+str(iWidth)+" height="+str(iHeight+100)+" position=Top-Left zero")
	mask_confirmer(iHeight+20, ccImp)

	#Run an analyze particles step to add each segmented region to the ROI manager
	pl = rm.getCount()
	ccImp.setCalibration( cal )
	IJ.run(ccImp, "Analyze Particles...", "size="+str(minCellSize)+"-"+str(maxCellSize)+" circularity="+str(minCircularity)+"-"+str(maxCircularity)+" add")
	ccImp = IJ.createImage("ccImp", "8-bit white", iWidth, iHeight, 1)
	while pl < rm.getCount():
		cent = rm.getRoi(pl)
		ccImp.setRoi(cent)
		IJ.run(ccImp, "Clear", "")
		rm.select(pl)
		rm.runCommand("Delete")

	#Clear everything outside the pouch so we do not measure it
	ccImp.setRoi(Roido)
	IJ.run(ccImp, "Make Inverse", "")
	IJ.run(ccImp, "Clear", "")
	IJ.run(ccImp, "Invert", "")
	IJ.run(ccImp, "Select None", "")


	#Run an analyze particles step to add each segmented region to the ROI manager
	pl = rm.getCount()
	pl2 = pl
	ccImp.setCalibration( cal )
	IJ.run(ccImp, "Analyze Particles...", "size="+str(minCellSize)+"-"+str(maxCellSize)+" circularity="+str(minCircularity)+"-"+str(maxCircularity)+" add")
	y = rm.getCount()
	blorp1 = list()

	#We now run through each ROI from the analyze particles step and convert it to a centroid with a radius determined by the user ('cellCountRadius')
	while pl < y:
		cent = rm.getRoi(pl2)
		
		#Make sure there is some DAPI signal in our ROI, if specified by user
		if JSF_package.configBasic.DAPIchannel != "Disabled":
			nImp.setRoi(cent)
			IJ.run("Clear Results")
			IJ.run(nImp, "Measure", "")
			dapiMean = rt.getValue("Mean", 0)
		else:
			dapiMean = 1
		
		cent = cent.getContourCentroid()

		#Convert pixel to micron units and scale our unit, as this function does not
	
		
	
		oO = OvalRoi(cent[0] - cellCountRadius/2, cent[1] - cellCountRadius/2, cellCountRadius, cellCountRadius)
		ccImpRaw.setRoi(oO)

		
		
		#We measure the mean pixel value over this new centroid ROI. If it is below our intensity filter threshold, we deem it a cell, keep it in the ROI manager, and rename it.
		IJ.run(ccImpRaw, "Measure", "")
		cellMean = rt.getValue("Mean", 0)
		if (cellMean < threshFilter) and (dapiMean > DAPIfilter):
			rm.addRoi(oO)
			rm.select(rm.getCount()-1)
			rm.runCommand("Rename", "SingleCell_Z:"+str(stackno)+"_No."+str(pl-pl2))
			blorp1.extend([pl])
			pl = pl+1
		else:
			y = y-1
		rm.select(pl2)
		rm.runCommand("Delete")

		
	cellROIs.append(blorp1)
	IJ.run("Clear Results")

	
	
	#return our array of individual cells.
	return cellROIs