
#We generate the dialog boxes to get user inputs. These variables are all saved in and loaded from various config files.

############################################################################################################
#Here is the main user input GUI function
from fiji.util.gui import GenericDialogPlus
from java.awt.event   import ActionListener
from java.awt import Font
import os
import JSF_package
from ij import IJ
from shutil import copy
from JSF_package import _misc_, caspase_analysis, cell_tracking, clone_analysis, configBasic, configCellTrack, configCloneSeg, configDeathSeg, configDeathTrack, configRoi, configSave, seeded_region_growing, speckles_analysis, start_up, tracking_and_outputs, user_inputs_GUI
	

#We import the necessary plugins from IJPB/MorphoLibJ. If MorphoLibJ is not installed, we skip these steps.

try:
	from inra.ijpb.morphology import Strel, Morphology, MinimaAndMaxima
	from inra.ijpb.binary import BinaryImages
	from inra.ijpb.watershed import Watershed
	morphoInstalled = 1
except:
	morphoInstalled = 0


#Do the same for SRG


try:
	from net.sf.ij_plugins.im3d.grow import SRG
	
	seedInstalled = 1
	
except:
	seedInstalled = 0
	


#All user-specified variables are pulled from, written to, and saved in config files.

## Here we create the functions for when the user clicks one of the advanced settings buttons
class ButtonClic(ActionListener):
	
    def actionPerformed(self, event): # self (or this in Java) to state that the method will be associated to the class instances

		
		output = "" #Empty string to which our outputs that we will write to the config file will be attached

		#gd2 is the advanced generic dialog
		gd2 = GenericDialogPlus("Advanced settings")
		gd2.setFont(Font("Sanserif", Font.PLAIN, 12))

		Numbers = range(0,1500)
		Numbers = map(str, Numbers)
		
		newNumbers = range(3,1500)
		newNumbers = map(str, Numbers)

		postChoices = ["None", "Despeckle"]

		#Check for custom post-processing files - i.e. files made by the user to process weka files
		mPath = os.path.join(os.getcwd(), "jars", "Lib", "JSF_End_User_Made_Code")
		macrofiles = [f for f in os.listdir(mPath) if os.path.isfile(os.path.join(mPath, f))]

		for item in macrofiles:
			if item.endswith("__init__.py") == False and item.endswith(".class") == False and item.endswith("_executor.py") == False and item.endswith("_Template.py") == False:
				if item not in postChoices :
					segChoices = postChoices  + [item]

		postChoices = postChoices + ["Make Your Own New Script!"]
        
        # Check from where comes the event 
		Source = event.getSource() # returns the Button object
 
        # Do an action depending on the button clicked
		if Source.label == "Set advanced clone segmentation parameters":

			
			#Import config file relevant to this panel
			from JSF_package import configCloneSeg as cfgCS

			#Create generic dialog options

			gd2.setInsets(0,0,0)
			gd2.addMessage("Advanced clone analysis parameters", Font("Sanserif", Font.BOLD, 12))
			gd2.setInsets(0,0,0)
			gd2.addNumericField("Subtract pixel intensity from clone channel (useful for getting rid of bleed through):", cfgCS.subAdd, 0)
			gd2.setInsets(0,0,0)
			gd2.addNumericField("Set clone border thickness (scaled units):", cfgCS.borderMargin,1)
			gd2.setInsets(0,0,0)
			gd2.addNumericField("Fill holes in clone mask with area <= (scaled units):", cfgCS.holeDiameter,3)
			gd2.setInsets(0,0,0)
			gd2.addChoice("Max Z-projection of 'X' slices above and below current plane:", Numbers, str(cfgCS.rollingZ))
			gd2.setInsets(0,0,0)
			gd2.addChoice("Set post-processing of Weka-segmented images (Weka segmentation only):", postChoices, cfgCS.clonesPost)

			
			
			gd2.setInsets(0,0,0)
			gd2.addMessage("Clone tracking clustering with DBSCAN - requires BioVoxxel plugin", Font("Sanserif", Font.BOLD, 12))
			gd2.setInsets(0,0,0)
			gd2.addChoice("Select clone tracking clustering method:", ["Disabled", 'Cluster Without Flooding', "Cluster With Flooding"], str(cfgCS.DBSCANmode))
			gd2.setInsets(0,0,0)
			gd2.addChoice("Minimum particle density:", newNumbers, str(cfgCS.DBSCANminDensity))
			gd2.setInsets(0,0,0)
			gd2.addNumericField("Distance (Epsilon):", cfgCS.DBSCANdist, 0)
			gd2.setInsets(0,0,0)
			gd2.addMessage("Seeded region growing options - requires IJ-plugins toolkit", Font("Sanserif", Font.BOLD, 12))
			gd2.setInsets(0,0,0)
			gd2.addCheckbox("Add a seeded region growing step?", cfgCS.seedChoiceClones)
			gd2.addToSameRow()
			gd2.addChoice("Apply seeds to image channel:", (["ROI Mask"]+Numbers), str(cfgCS.seedChannelClones))
			gd2.setInsets(0,0,0)
			gd2.addCheckbox("Seeded region growing on dark background?", cfgCS.invertSeedClones)
			gd2.addToSameRow()
			gd2.addNumericField("Seeded region growing gaussian blur radius:", cfgCS.blurClones, 1)
			gd2.setInsets(0,0,0)
			gd2.addNumericField("Seeded region growing filter background seeds:", cfgCS.minSeedSizeClones, 1)

	
			
			gd2.showDialog()

	
			
			if gd2.wasOKed():
				#If user pressed okay, get the user specified variables

				
				
				subAdd = gd2.getNextNumber()
				borderMargin = gd2.getNextNumber()
				holeDiameter = gd2.getNextNumber()
				rollingZ = gd2.getNextChoice()
				clonesPost = gd2.getNextChoice()
				winwo = False
				DBSCANmode = gd2.getNextChoice()
				DBSCANminDensity = gd2.getNextChoice()
				DBSCANdist = gd2.getNextNumber()
				seedChoiceClones = gd2.getNextBoolean()
				seedChannelClones = gd2.getNextChoice()
				invertSeedClones = gd2.getNextBoolean()
				blurClones = gd2.getNextNumber()
				minSeedSizeClones = gd2.getNextNumber()

				
				
				if seedInstalled == 0 and seedChoiceClones == True:
				
					seedChoiceClones = False
					IJ.log("To perform seeded region growing, please install the IJ-plugins toolkit")
				

				#Write these variables to a string
				output = output + "seedChoiceClones="+ str(seedChoiceClones)+"\n"
				output = output + "seedChannelClones='"+ str(seedChannelClones)+"'\n"
				output = output + "invertSeedClones="+ str(invertSeedClones)+"\n"
				output = output + "minSeedSizeClones="+ str(minSeedSizeClones)+"\n"
				output = output + "subAdd="+ str(subAdd)+"\n"
				output = output + "borderMargin="+ str(borderMargin)+"\n"
				output = output + "holeDiameter="+str(holeDiameter) +"\n"
				output = output + "rollingZ="+str(rollingZ) +"\n"
				output = output + "winwo="+str(winwo) +"\n"
				output = output + "DBSCANmode='"+str(DBSCANmode) +"'\n"
				output = output + "DBSCANminDensity="+str(DBSCANminDensity) +"\n"
				output = output + "DBSCANdist="+str(DBSCANdist) +"\n"
				output = output + "clonesPost='"+str(clonesPost) +"'\n"
				output = output + "blurClones="+str(blurClones) +"\n"

				

				#Save this string as a .py file. We can now pull these variables from the config file to any module
				path = os.getcwd()
				dir2 = os.path.join(path, "jars", "Lib", "JSF_package")
				txtPath = os.path.join(dir2, "configCloneSeg.py")
				if os.path.exists(dir2) == False:
					os.makedirs(dir2)
				outFile = open(txtPath, 'w')
				outFile.write(output)
				outFile.close()
				reload(JSF_package.configCloneSeg)
	
				
		
		elif Source.label == "Set advanced single cell tracking parameters":

			from JSF_package import configCellTrack as cfgCT
		
			gd2.addMessage("Advanced Single Cell Tracking Parameters", Font("Sanserif", Font.BOLD, 12))
			gd2.addNumericField("Track individual cell ROIs with area >= (scaled units):", cfgCT.minCellSize, 3)
			gd2.addNumericField("Single cell centroid tracking: set centroid radius(scaled units):", cfgCT.cellCountRadius, 3)
			gd2.addNumericField("Watershed function tolerance:", cfgCT.morphoSeg, 0)
			gd2.addNumericField("Nuclear stain intensity threshold weighting:", cfgCT.filterWeight, 1)
			gd2.addChoice("Set post-processing of Weka-segmented images (Weka segmentation only):", postChoices, cfgCT.cellPost)
			gd2.addMessage("Seeded region growing options - requires IJ-plugins toolkit", Font("Sanserif", Font.BOLD, 12))
			gd2.addCheckbox("Add a seeded region growing step?", cfgCT.seedChoiceCell)
			gd2.addToSameRow()
			gd2.addChoice("Apply seeds to image channel:", Numbers, str(cfgCT.seedChannelCell))
			gd2.addCheckbox("Seeded region growing on dark background?", cfgCT.invertSeedCell)
			gd2.addToSameRow()
			gd2.addNumericField("Seeded region growing gaussian blur radius:", cfgCT.blurCell, 1)
			gd2.addNumericField("Seeded region growing filter background seeds:", cfgCT.minSeedSizeCell, 1)
			
			
			gd2.showDialog()

			if gd2.wasOKed():
				minCellSize = gd2.getNextNumber()
				cellCountRadius = gd2.getNextNumber()
				morphoSeg = gd2.getNextNumber()
				filterWeight = gd2.getNextNumber()
				cellPost = gd2.getNextChoice()
				seedChoiceCell = gd2.getNextBoolean()
				seedChannelCell = gd2.getNextChoice()
				invertSeedCell = gd2.getNextBoolean()
				blurCell = gd2.getNextNumber()
				minSeedSizeCell = gd2.getNextNumber()
				if seedInstalled == 0 and seedChoiceCell == 1:
					seedChoiceCell == False
					IJ.error("To perform seeded region growing, please install the IJ-plugins toolkit")
				

				output = output + "minCellSize="+str(minCellSize) +"\n"
				output = output + "cellCountRadius="+ str(cellCountRadius)+"\n"
				output = output + "morphoSeg="+ str(morphoSeg)+"\n"
				output = output + "filterWeight="+ str(filterWeight)+"\n"
				output = output + "seedChoiceCell="+ str(seedChoiceCell)+"\n"
				output = output + "seedChannelCell='"+ str(seedChannelCell)+"'\n"
				output = output + "invertSeedCell="+ str(invertSeedCell)+"\n"
				output = output + "minSeedSizeCell="+ str(minSeedSizeCell)+"\n"
				output = output + "blurCell="+ str(blurCell)+"\n"
				output = output +"cellPost='"+ str(cellPost)+"'\n"

				path = os.getcwd()
				dir2 = os.path.join(path, "jars", "Lib", "JSF_package")
				txtPath = os.path.join(dir2, "configCellTrack.py")
				if os.path.exists(dir2) == False:
					os.makedirs(dir2)
				outFile = open(txtPath, 'w')
				outFile.write(output)
				outFile.close()
				reload(JSF_package.configCellTrack)
		

		
		elif Source.label == "Set advanced cell death segmentation parameters":

			from JSF_package import configDeathSeg as cfgDS
		
			gd2.addMessage("Advanced Cell Death Segmentation Parameters", Font("Sanserif", Font.BOLD, 12))

			gd2.addNumericField("Pre-processing: multiply pixel values by (useful for dim images):", cfgDS.dcp1Booster, 1)
			gd2.addNumericField("Low-pass filter upper bound (scaled units, Default and high background processing only):", cfgDS.lowPass, 3)
			gd2.addNumericField("High-pass filter lower bound (scaled units, Default and high background processing only):", cfgDS.highPass, 3)
			gd2.addNumericField("Filter weight (Default and high background processing only):", cfgDS.multVal, 1)
			gd2.addNumericField("Remove outliers with area >= (scaled units, Default and high background processing only)):", cfgDS.outVal, 3)
			gd2.addChoice("Set post-processing of Weka-segmented images (Weka segmentation only):", postChoices, cfgDS.casPost)
			gd2.addMessage("Seeded region growing options - requires IJ-plugins toolkit", Font("Sanserif", Font.BOLD, 12))
			gd2.addCheckbox("Add a seeded region growing step?", cfgDS.seedChoiceCas)
			gd2.addToSameRow()
			gd2.addChoice("Apply seeds to image channel:", Numbers, str(cfgDS.seedChannelCas))
			gd2.addCheckbox("Seeded region growing on dark background?", cfgDS.invertSeedCas)
			gd2.addToSameRow()
			gd2.addNumericField("Seeded region growing gaussian blur radius:", cfgDS.blurCas, 1)
			gd2.addNumericField("Seeded region growing filter background seeds:", cfgDS.minSeedSizeCas, 1)			

		
			
			gd2.showDialog()

			if gd2.wasOKed():
				
				
				dcp1Booster = gd2.getNextNumber()
				lowPass = gd2.getNextNumber()
				highPass = gd2.getNextNumber()
				multVal = gd2.getNextNumber()
				outVal = gd2.getNextNumber()
				casPost = gd2.getNextChoice()
				seedChoiceCas = gd2.getNextBoolean()
				seedChannelCas = gd2.getNextChoice()
				invertSeedCas = gd2.getNextBoolean()
				blurCas = gd2.getNextNumber()
				minSeedSizeCas = gd2.getNextNumber()
		
				if seedInstalled == 0 and seedChoiceCas == 1:
					seedChoiceCas == False
					IJ.error("To perform seeded region growing, please install the IJ-plugins toolkit")
			
				output = output + "seedChoiceCas="+str(seedChoiceCas)+"\n"
				output = output + "seedChannelCas='"+str(seedChannelCas)+"'\n"
				output = output + "dcp1Booster="+str(dcp1Booster) +"\n"
				output = output + "lowPass="+ str(lowPass)+"\n"
				output = output + "highPass="+str(highPass) +"\n"
				output = output + "multVal="+ str(multVal)+"\n"
				output = output + "outVal="+str(outVal) +"\n"
				output = output + "minSeedSizeCas="+str(minSeedSizeCas) +"\n"
				output = output + "invertSeedCas="+str(invertSeedCas) +"\n"
				output = output + "casPost='"+str(casPost)+"'\n"
				output = output + "blurCas="+str(blurCas) +"\n"
		
				path = os.getcwd()
				dir2 = os.path.join(path, "jars", "Lib", "JSF_package")
				txtPath = os.path.join(dir2, "configDeathSeg.py")
				if os.path.exists(dir2) == False:
					os.makedirs(dir2)
				outFile = open(txtPath, 'w')
				outFile.write(output)
				outFile.close()
				reload(JSF_package.configDeathSeg)
		

			
		elif Source.label == "Set advanced cell death event counting parameters":

			from JSF_package import configDeathTrack as cfgDT
			gd2.addMessage("Advanced Cell Death Event Counting Parameters", Font("Sanserif", Font.BOLD, 12))
			gd2.addNumericField("Track individual cell death ROIs with area >= (scaled units):", cfgDT.minCasSize, 3)
			gd2.addNumericField("Cell death event watershed: set gaussian blur radius (scaled units):", cfgDT.dcp1Gauss, 3)
			gd2.addNumericField("Cell death event centroid tracking: set centroid radius (scaled units):", cfgDT.dcp1Radius, 3)
			
			gd2.showDialog()
			

			if gd2.wasOKed():
			
				minCasSize = gd2.getNextNumber()
				dcp1Gauss = gd2.getNextNumber()
				dcp1Radius = gd2.getNextNumber()


				output = output + "minCasSize="+ str(minCasSize)+"\n"
				output = output + "dcp1Gauss="+str(dcp1Gauss) +"\n"
				output = output + "dcp1Radius="+str(dcp1Radius) +"\n"


				path = os.getcwd()
				dir2 = os.path.join(path, "jars", "Lib", "JSF_package")
				txtPath = os.path.join(dir2, "configDeathTrack.py")
				if os.path.exists(dir2) == False:
					os.makedirs(dir2)
				outFile = open(txtPath, 'w')
				outFile.write(output)
				outFile.close()
				reload(JSF_package.configDeathTrack)
		
									 
		elif Source.label == "Save and display options":
			from JSF_package import configSave as cfgS
			gd2.addMessage("Save and Display Options", Font("Sanserif", Font.BOLD, 12))
			gd2.addCheckbox("Generate output image panel?", cfgS.generateImage)
			gd2.addCheckbox("Display output panel?", cfgS.visualize)
			gd2.addCheckbox("Save output images?", cfgS.saveChoice)
			gd2.addChoice("Save every Nth output image?", Numbers, str(cfgS.imageSampler))
			gd2.addChoice("Save output CSV after every Nth image?", Numbers, str(cfgS.csvSaver))
						
			gd2.showDialog()
			
			if gd2.wasOKed():

				generateImage = gd2.getNextBoolean()
				visualize = gd2.getNextBoolean()
				saveChoice = gd2.getNextBoolean()
				imageSampler = gd2.getNextChoice()
				
				csvSaver =gd2.getNextChoice()
				
				
				output = output + "visualize="+ str(visualize)+"\n"
				output = output + "saveChoice="+ str(saveChoice)+"\n"
				output = output + "imageSampler="+str(imageSampler) +"\n"
				output = output + "csvSaver="+ str(csvSaver)+"\n"
				output = output + "generateImage="+str(generateImage) +"\n"

				path = os.getcwd()
				dir2 = os.path.join(path, "jars", "Lib", "JSF_package")
				txtPath = os.path.join(dir2, "configSave.py")
				if os.path.exists(dir2) == False:
					os.makedirs(dir2)
				outFile = open(txtPath, 'w')
				outFile.write(output)
				outFile.close()
				reload(JSF_package.configSave)

		elif Source.label == "Set advanced ROI segmentation parameters":
			from JSF_package import configRoi as cfgR

			gd2.addMessage("ROI Segmentation and Options", Font("Sanserif", Font.BOLD, 12))
			gd2.addCheckbox("Compare two regions in an image?", cfgR.halfHalfNC)
			gd2.addChoice("Set post-processing of Weka-segmented images (Weka segmentation only):", postChoices, cfgR.roiPost)
			
			gd2.addMessage("Seeded region growing options - requires IJ-plugins toolkit", Font("Sanserif", Font.BOLD, 12))
			gd2.addCheckbox("Add a seeded region growing step?", cfgR.seedChoice)
			gd2.addToSameRow()
			gd2.addChoice("Apply seeds to image channel:", Numbers, str(cfgR.seedChannel))
			gd2.addCheckbox("Seeded region growing on dark background?", cfgR.invertSeed)
			gd2.addToSameRow()
			gd2.addNumericField("Seeded region growing gaussian blur radius:", cfgR.blurRoi, 1)
			gd2.addNumericField("Seeded region growing filter background seeds:", cfgR.minSeedSize, 1)
			
			

			gd2.showDialog()
			
			if gd2.wasOKed():
				halfHalf = False
				halfHalfNC = gd2.getNextBoolean()
				roiPost = gd2.getNextChoice()
				seedChoice = gd2.getNextBoolean()
				seedChannel = gd2.getNextChoice()
				invertSeed = gd2.getNextBoolean()
				blurRoi = gd2.getNextNumber()
				minSeedSize = gd2.getNextNumber()
				if seedInstalled == 0 and seedChoice == 1:
					seedChoice == False
					IJ.error("To perform seeded region growing, please install the IJ-plugins toolkit")
				

				if halfHalfNC == True:
					halfHalf = False
					
				output = output+"seedChoice="+str(seedChoice)+"\n"
				output = output+"seedChannel='"+str(seedChannel)+"'\n"			
				output = output+"invertSeed="+str(invertSeed)+"\n"
				output = output+"minSeedSize="+str(minSeedSize)+"\n"
				output = output+"roiPost='"+str(roiPost)+"'\n"	
				output = output + "halfHalf="+str(halfHalf) +"\n"
				output = output + "blurRoi="+str(blurRoi) +"\n"
				output = output + "halfHalfNC="+str(halfHalfNC) +"\n"

				path = os.getcwd()
				dir2 = os.path.join(path, "jars", "Lib", "JSF_package")
				txtPath = os.path.join(dir2, "configRoi.py")
				if os.path.exists(dir2) == False:
					os.makedirs(dir2)
				outFile = open(txtPath, 'w')
				outFile.write(output)
				outFile.close()
				reload(JSF_package.configRoi)

def Open_GUI():

	#Import our base config file
	from JSF_package import configBasic as cfg
	import JSF_package

	gdLoop = 1
	while gdLoop == 1:
		gdLoop = 0

		#Get the directory of our package in which we store the custom scripts
		path = os.getcwd()
		dir2 = os.path.join(path, "jars", "Lib", "JSF_package")
		wPath = os.path.join(path, "jars", "Lib", "JSF_package", "Weka_Models")
		mPath = os.path.join(path, "jars", "Lib", "JSF_End_User_Made_Code")

		options = ["Select option", "Save current settings"]
		segChoices = []
		preProcess = ["None"] 

		#Add our custom scripts as selectable options
		wekafiles = [f for f in os.listdir(wPath) if os.path.isfile(os.path.join(wPath, f))]
		fijifiles = [f for f in os.listdir(dir2) if os.path.isfile(os.path.join(dir2, f))]
		macrofiles = [f for f in os.listdir(mPath) if os.path.isfile(os.path.join(mPath, f))]


		for item in wekafiles:
			if item.endswith(".model"):
				if item not in segChoices :
					segChoices  = segChoices +[item]


		for item in macrofiles:
			if item.endswith("__init__.py") == False and item.endswith(".class") == False and item.endswith("_executor.py") == False:
				if item not in segChoices :
					segChoices = segChoices  + [item]
				if item not in preProcess:
					preProcess = preProcess + [item]
		
			
		#Here, we are checking for any settings that the user has saved previously. If they exist, we add it as an option that the user can select to overwrite the current settings
		
		for item in fijifiles:
			if item.startswith("_"):
				item = item[1:]
				subscr = item.find("_userSettingFile_.py")
				if subscr != -1:
					item = item[:subscr]
					options = options + [item]

		

		preProcess = preProcess + ["Make Your Own New Script!"]
		segChoices = segChoices + ["Add_Custom_Weka_Classifier", "Add_Custom_3D_Weka_Classifier", "Make Your Own New Script!"]
		#Set up out generic dialog panel to get user input
		gd = GenericDialogPlus("Set up your analysis!")

		
		Numbers = range(1,253)
		Numbers = map(str, Numbers)

		#Add z-projection options
		zOptions = ["Disabled", "max", "min", "sum", "avg", "median", "sd"]

		#Add speckle analysis options
		speckleOptions = ["Disabled", "Count", "Count, Area, Fluorescence"]
		sc = 0
		while sc<150:
			sc+=1
			speckleOptions = speckleOptions+["Count, Area, Fluorescence + Gauss="+str(sc)]
			
		
	
		#Add all out options to the generic dialog, pull all default variables from the config file
		gd.setFont(Font("Sanserif", Font.PLAIN, 12))
		gd.setInsets(0,0,0)
		gd.addChoice("Z project stack (converts 3D stacks to 2D projections)?", zOptions, str(cfg.zMethod))
		gd.setInsets(0,0,0)
		gd.addMessage("Region of Interest (ROI) Selection Settings", Font("Sanserif", Font.BOLD, 12))
		gd.setInsets(0,0,0)
		gd.addButton("Set advanced ROI segmentation parameters", ButtonClic())
		items = ["Manual Selections", "Analyze Entire Image"]+segChoices
		gd.setInsets(0,0,0)
		gd.addCheckbox("Restore saved ROI selections?", cfg.restoreROI)
		gd.setInsets(0,0,0)
		gd.addChoice("How would you like to segment your ROI's?", items, cfg.ROIseg)
		gd.addToSameRow()
		gd.addStringField("Specify ROI selection channel (separate multiple channels with commas):", str(cfg.ROIchannel))
		
		gd.setInsets(0,0,0)
		gd.addMessage("Clone/Genotype Segmentation Settings", Font("Sanserif", Font.BOLD, 12))
		gd.setInsets(0,0,0)
		gd.addButton("Set advanced clone segmentation parameters", ButtonClic())
		items = ["Membrane-tagged GFP", "Cytosolic GFP", "Use ROIs As Clones"]+segChoices
		gd.setInsets(0,0,0)
		gd.addChoice("How should clones be segmented?", items, cfg.cloneSeg)
		gd.addToSameRow()
		gd.addStringField("Specify clone segmentation channel (separate multiple channels with commas):", str(cfg.cloneChannel))
		gd.setInsets(0,0,0)
		gd.addChoice("Specify number of clone genotypes:", Numbers, str(cfg.numGenotypes))
		gd.setInsets(0,0,0)
		gd.addCheckbox("Analyze each clone individually?", cfg.cloneTracking)
		gd.addToSameRow()
		gd.addNumericField("Track individual clone ROIs with area >= (scaled units):", cfg.minCloneSize, 1)

		gd.setInsets(0,0,0)
		gd.addMessage("Single Cell Tracking Settings",  Font("Sanserif", Font.BOLD, 12))
		gd.setInsets(0,0,0)
		gd.addButton("Set advanced single cell tracking parameters", ButtonClic())
		items = ["Disabled", "Default"]+segChoices
		gd.setInsets(0,0,0)
		gd.addChoice("How should single cells be segmented?", items, cfg.singleCellMethod)
		gd.setInsets(0,0,0)
		gd.addChoice("Specify nuclear stain channel for single cell filtering", ["Disabled"] + Numbers, str(cfg.DAPIchannel))
		gd.addToSameRow()
		gd.addStringField("Specify single cell segmentation channel (separate multiple channels with commas):", str(cfg.cellCountChannel))
		gd.setInsets(0,0,0)
		gd.addCheckbox("Analyze single cells for spatial and fluorescence properties?", cfg.cellCountDeluxe)

		gd.setInsets(0,0,0)
		gd.addMessage("Cell Death Segmentation Settings", Font("Sanserif", Font.BOLD, 12))
		gd.setInsets(0,0,0)
		gd.addButton("Set advanced cell death segmentation parameters", ButtonClic())
		items = ["Disabled", "Default", "High Background"]+segChoices
		gd.setInsets(0,0,0)
		gd.addChoice("How should cell death events be segmented?", items, cfg.cellDeathSegMethod)
		gd.addToSameRow()
		gd.addStringField("Specify cell death segmentation channel (separate multiple channels with commas):", str(cfg.dcp1Channel))
		gd.setInsets(0,0,0)
		gd.addCheckbox("Count number of individual cell death events?", cfg.dcp1Counting)
		gd.addToSameRow()
		gd.addCheckbox("Analyze single cells for spatial and fluorescence properties?", cfg.dcp1Deluxe)
		gd.addButton("Set advanced cell death event counting parameters", ButtonClic())

		gd.setInsets(0,0,0)
		gd.addMessage("Fluorescence and Speckles Settings", Font("Sanserif", Font.BOLD, 12))
		gd.setInsets(0,0,0)
		gd.addChoice("Pre-Process Fluorescence Channel Method:", preProcess, str(cfg.preProcess))
		gd.setInsets(0,0,0)
		gd.addCheckbox("Analyze clones for mean fluorescence intensity?", cfg.fluoChoice)
		gd.addToSameRow()
		gd.addStringField("Specify fluorescence channel (For pre-processing with multiple channels, separate with commas)", str(cfg.fluoChannel))
		gd.setInsets(0,0,0)
		gd.addChoice("Analyze clones for speckles?", speckleOptions, cfg.speckleMethod)
		gd.addToSameRow()
		gd.addChoice("Specify speckles channel", Numbers, str(cfg.speckleChannel))
		gd.setInsets(0,0,0)
		gd.addNumericField("Speckle detection noise tolerance:", cfg.speckleNoise, 0)
		gd.setInsets(0,0,0)
		gd.addCheckbox("Analyze timelapse images?", cfg.timelapse)
		gd.addToSameRow()
		gd.addChoice("Set timelapse frame interval", Numbers, str(cfg.frame_interval))
	
		

		gd.setInsets(10,0,0)
		gd.addButton("Save and display options", ButtonClic())
		gd.addToSameRow()
		gd.addChoice("Save or load settings?", options, "Select option" )
		
		#Show dialog window. Extract variables if user presses 'OK'
		gd.showDialog()
		ok = 0
		if gd.wasOKed():
	
			ok = 1
			
			#Create an empty string. We will add things to this to write to out defaults file, if prompted by user
			output = ""

			zMethod = gd.getNextChoice()
			output = output+"zMethod='"+str(zMethod)+"'\n"
		
			#Extract variables from dropdown menus
			ROIseg = gd.getNextChoice()
			#Add this variable to the defaults output
			output = output+"ROIseg='"+str(ROIseg)+"'\n"

			restoreROI = gd.getNextBoolean()
			output = output+"restoreROI="+str(restoreROI)+"\n"
			
			ROIchannel = gd.getNextString()
			output = output+"ROIchannel='"+str(ROIchannel)+"'\n"
			
			cloneSeg = gd.getNextChoice()
			output = output+"cloneSeg='"+str(cloneSeg)+"'\n"

			numGenotypes = gd.getNextChoice()
			output = output+"numGenotypes="+str(numGenotypes)+"\n"
			
			cloneChannel = gd.getNextString()
			output = output+"cloneChannel='"+str(cloneChannel)+"'\n"
			
			singleCellMethod = gd.getNextChoice()
			output = output+"singleCellMethod='"+str(singleCellMethod)+"'\n"
			if morphoInstalled == 0 and singleCellMethod == "Default":
				singleCellMethod = "Disabled"
				IJ.error("To perform the default single cell tracking, please install MorphoLibJ")
	
			if singleCellMethod != "Disabled":
				cellCount = True
			else: 
				cellCount = False
			output = output+"cellCount="+str(cellCount)+"\n"
			
			DAPIchannel = gd.getNextChoice()
			if DAPIchannel == "Disabled":
				DAPIchannel = "'Disabled'"
			output = output+"DAPIchannel="+str(DAPIchannel)+"\n"
	
			cellCountChannel = gd.getNextString()
			output = output+"cellCountChannel='"+str(cellCountChannel)+"'\n"
			
			cellDeathSegMethod = gd.getNextChoice()
			output = output+"cellDeathSegMethod='"+str(cellDeathSegMethod)+"'\n"
			if morphoInstalled == 0 and cellDeathSegMethod == "Default":
				cellDeathSegMethod = "Disabled"
				IJ.error("To perform the default cell death tracking, please install MorphoLibJ")
	
			if cellDeathSegMethod != "Disabled":
				dcp1Choice = True
			else:
				dcp1Choice = False
			output = output+"dcp1Choice="+str(dcp1Choice)+"\n"


			preProcess = gd.getNextChoice()
			output = output+"preProcess='"+str(preProcess)+"'\n"
	
			dcp1Channel = gd.getNextString()
			output = output+"dcp1Channel='"+str(dcp1Channel)+"'\n"


			
			fluoChannel = gd.getNextString()
			output = output+"fluoChannel='"+str(fluoChannel)+"'\n"

			speckleMethod = gd.getNextChoice()

			if seedInstalled == 0 and speckleMethod != "Disabled":
				speckleMethod = "Disabled"
				IJ.error("To perform speckle area and fluorescence measurements, please install the IJ-plugins toolkit")
			output = output+"speckleMethod='"+str(speckleMethod)+"'\n"
			
			speckleChannel = gd.getNextChoice()
			output = output+"speckleChannel="+str(speckleChannel)+"\n"
	
			frame_interval = gd.getNextChoice()
			output = output+"frame_interval="+str(frame_interval)+"\n"

			saveLoad = gd.getNextChoice()
	
			#Extract variables from checkboxes
			cloneTracking = gd.getNextBoolean()
			output = output+"cloneTracking="+str(cloneTracking)+"\n"
			
			cellCountDeluxe = gd.getNextBoolean()
			output=output+"cellCountDeluxe="+str(cellCountDeluxe)+"\n"

			dcp1Counting = gd.getNextBoolean()
			output = output+"dcp1Counting="+str(dcp1Counting)+"\n"

			dcp1Deluxe = gd.getNextBoolean()
			output=output+"dcp1Deluxe="+str(dcp1Deluxe)+"\n"
			
			fluoChoice = gd.getNextBoolean()
			output = output+"fluoChoice="+str(fluoChoice)+"\n"
			
			speckle = True
			if speckleMethod == "Disabled":
				speckle = False
			output = output+"speckle="+str(speckle)+"\n"
	
			timelapse = gd.getNextBoolean()
			output = output+"timelapse="+str(timelapse)+"\n"


			
			
	
			#Extract variables from numeric fields
			minCloneSize = gd.getNextNumber()
			output = output+"minCloneSize="+str(minCloneSize)+"\n"
			
			speckleNoise = gd.getNextNumber()
			output = output+"speckleNoise="+str(speckleNoise)+"\n"

			channelChecker = [dcp1Channel, ROIchannel, cloneChannel, cellCountChannel, fluoChannel]
			counting = 1
			error = ""
			digits = True
			acceptableStrings = ['r', 'R', 'g', 'G', 'b','B', 'c', 'C', 'm', 'M', 'y', 'Y']
			for item in channelChecker:
				item.replace(" ", "")
				item.strip(",")
				newCheck = item.split(",")
				if len (newCheck) > 1:
					
					if counting == 1 and cellDeathSegMethod == "Default":
						error = error + "Cell Death Chanel and"
					elif counting == 3 and (cloneSeg == "Manual Selections" or cloneSeg == "Analyze Entire Image"):
						error = error + "Clone Channel and"
					elif counting == 4 and singleCellMethod == "Default":
						error = error + "Single Cell Channel and"
					elif counting == 5 and preProcess == "None":
						error = error + "Fluorescence Channel"
				counting = counting + 1
				for item2 in newCheck:
					try:
						int(item2)
					except:
						digits = False
						try:
							if item2[-1] in acceptableStrings:
								if int(item2[:-1]):
									digits=True
						except:
							digits = False
								
			if digits == False:
				IJ.error("Invalid channel input! Channel inputs must be integers separated by commas!")
				gdLoop = 1
				continue
			if error != "":
				error.strip(" and")
				IJ.error("Error with "+ error+": you cannot specify multiple channels for Default analyses")
				gdLoop = 1
				continue
	
			if saveLoad == "Select option":
	
				#Write out the user inputs to the config file
				txtPath = os.path.join(dir2, "configBasic.py")
				if os.path.exists(dir2) == False:
					os.makedirs(dir2)
				outFile = open(txtPath, 'w')
				outFile.write(output)
				outFile.close()
	

			elif saveLoad == "Save current settings":
				SLloop = 1
				gdLoop = 1
				while SLloop == 1:
					SLloop = 0
					gdSL = GenericDialogPlus("Save your current settings")
					gdSL.addStringField("Name your settings file", "New_Settings")
					gdSL.showDialog()
	
					if gdSL.wasOKed():
						setName = gdSL.getNextString()
						setName = "_"+setName+"_userSettingFile_.py"

	
						
						if setName not in fijifiles:

							output = output+"seedChoice="+str(JSF_package.configRoi.seedChoice)+"\n"
							output = output+"seedChannel='"+str(JSF_package.configRoi.seedChannel)+"'\n"
							output = output+"invertSeed="+str(JSF_package.configRoi.invertSeed)+"\n"
							output = output+"minSeedSize="+str(JSF_package.configRoi.minSeedSize)+"\n"
							output = output+"halfHalf="+str(JSF_package.configRoi.halfHalf)+"\n"
							output = output+"halfHalfNC="+str(JSF_package.configRoi.halfHalfNC)+"\n"
							output = output+"roiPost='"+str(JSF_package.configRoi.roiPost)+"'\n"
							output = output+"blurRoi="+str(JSF_package.configRoi.blurRoi)+"\n"

							output = output+"visualize="+str(JSF_package.configSave.visualize)+"\n"
							output = output+"saveChoice="+str(JSF_package.configSave.saveChoice)+"\n"
							output = output+"imageSampler="+str(JSF_package.configSave.imageSampler)+"\n"
							output = output+"csvSaver="+str(JSF_package.configSave.csvSaver)+"\n"
							output = output+"generateImage="+str(JSF_package.configSave.generateImage)+"\n"

							
							output = output+"minCasSize="+str(JSF_package.configDeathTrack.minCasSize)+"\n"
							output = output+"dcp1Gauss="+str(JSF_package.configDeathTrack.dcp1Gauss)+"\n"
							output = output+"dcp1Radius="+str(JSF_package.configDeathTrack.dcp1Radius)+"\n"

							output = output+"seedChoiceCas="+str(JSF_package.configDeathSeg.seedChoiceCas)+"\n"
							output = output+"seedChannelCas='"+str(JSF_package.configDeathSeg.seedChannelCas)+"'\n"
							output = output+"minSeedSizeCas="+str(JSF_package.configDeathSeg.minSeedSizeCas)+"\n"
							output = output+"invertSeedCas="+str(JSF_package.configDeathSeg.invertSeedCas)+"\n"
							output = output+"dcp1Booster="+str(JSF_package.configDeathSeg.dcp1Booster)+"\n"
							output = output+"lowPass="+str(JSF_package.configDeathSeg.lowPass)+"\n"
							output = output+"highPass="+str(JSF_package.configDeathSeg.highPass)+"\n"
							output = output+"multVal="+str(JSF_package.configDeathSeg.multVal)+"\n"
							output = output+"outVal="+str(JSF_package.configDeathSeg.outVal)+"\n"
							output = output+"casPost='"+str(JSF_package.configDeathSeg.casPost)+"'\n"
							output = output+"blurCas="+str(JSF_package.configDeathSeg.blurCas)+"\n"
	
							output = output+"subAdd="+str(JSF_package.configCloneSeg.subAdd)+"\n"
							output = output+"borderMargin="+str(JSF_package.configCloneSeg.borderMargin)+"\n"
							output = output+"holeDiameter="+str(JSF_package.configCloneSeg.holeDiameter)+"\n"
							output = output+"rollingZ="+str(JSF_package.configCloneSeg.rollingZ)+"\n"
							output = output+"winwo="+str(JSF_package.configCloneSeg.winwo)+"\n"
							
							output = output+"seedChoiceClones="+str(JSF_package.configCloneSeg.seedChoiceClones)+"\n"
							output = output+"seedChannelClones='"+str(JSF_package.configCloneSeg.seedChannelClones)+"'\n"
							output = output+"minSeedSizeClones="+str(JSF_package.configCloneSeg.minSeedSizeClones)+"\n"
							output = output+"invertSeedClones="+str(JSF_package.configCloneSeg.invertSeedClones)+"\n"
							output = output + "DBSCANmode='"+str(JSF_package.configCloneSeg.DBSCANmode) +"'\n"
							output = output + "DBSCANminDensity="+str(JSF_package.configCloneSeg.DBSCANminDensity) +"\n"
							output = output + "DBSCANdist="+str(JSF_package.configCloneSeg.DBSCANdist) +"\n"
							output = output+"clonesPost='"+str(JSF_package.configCloneSeg.clonesPost)+"'\n"
							output = output + "blurClones="+str(JSF_package.configCloneSeg.blurClones) +"\n"

							output = output+"seedChoiceCell="+str(JSF_package.configCellTrack.seedChoiceCell)+"\n"
							output = output+"seedChannelCell='"+str(JSF_package.configCellTrack.seedChannelCell)+"'\n"
							output = output+"invertSeedCell="+str(JSF_package.configCellTrack.invertSeedCell)+"\n"
							output = output+"minSeedSizeCell="+str(JSF_package.configCellTrack.minSeedSizeCell)+"\n"
							output = output+"minCellSize="+str(JSF_package.configCellTrack.minCellSize)+"\n"
							output = output+"cellCountRadius="+str(JSF_package.configCellTrack.cellCountRadius)+"\n"
							output = output+"morphoSeg="+str(JSF_package.configCellTrack.morphoSeg)+"\n"
							output = output+"filterWeight="+str(JSF_package.configCellTrack.filterWeight)+"\n"
							output = output+"cellPost='"+str(JSF_package.configCellTrack.cellPost)+"'\n"
							output = output+"blurCell="+str(JSF_package.configCellTrack.blurCell)+"\n"


							#Write out the user inputs to the config file
							txtPath = os.path.join(dir2, setName)
							if os.path.exists(dir2) == False:
								os.makedirs(dir2)
							try:
								outFile = open(txtPath, 'w')
								outFile.write(output)
								outFile.close() 
							except:
								SLloop = 1
								IJ.error("There is a problem with that name. Please choose another.")

						else:
							SLloop = 1
							IJ.error("That name is already taken! Please choose another name.")
				
			else:
				gdLoop = 1
				
				loader = "_"+saveLoad+"_userSettingFile_.py"

				sourcePath = os.path.join(dir2, loader)
				
				destPath = os.path.join(dir2, "configBasic.py")
				copy(sourcePath, destPath)

				destPath = os.path.join(dir2, "configCellTrack.py")
				copy(sourcePath, destPath)

				destPath = os.path.join(dir2, "configCloneSeg.py")
				copy(sourcePath, destPath)

				destPath = os.path.join(dir2, "configDeathSeg.py")
				copy(sourcePath, destPath)

				destPath = os.path.join(dir2, "configDeathTrack.py")
				copy(sourcePath, destPath)

				destPath = os.path.join(dir2, "configSave.py")
				copy(sourcePath, destPath)

				destPath = os.path.join(dir2, "configRoi.py")
				copy(sourcePath, destPath)
				
			#Reload config files, since we have edited them. The macro will use previously saved variables otherwise.
			reload (JSF_package)
			reload(JSF_package.configBasic)
			reload(JSF_package.configCloneSeg)
			reload(JSF_package.configCellTrack)
			reload(JSF_package.configDeathSeg)
			reload(JSF_package.configDeathTrack)
			reload(JSF_package.configSave)
			reload(JSF_package.configRoi)

			if ("Custom" in cloneSeg) or ("Custom" in singleCellMethod) or ("Custom" in cellDeathSegMethod) or ("Custom" in ROIseg):
				looper = 1
				gdLoop = 1
				while looper == 1:
					looper = 0
					gdCS = GenericDialogPlus("Choose your custom segmentation methods")
				
					if "Custom" in ROIseg:
						gdCS.addMessage("ROI Segmentation", Font("Sanserif", Font.BOLD, 14))
						gdCS.addStringField("Add a name for your file!", "New_"+ROIseg[11:])
						gdCS.addFileField("ROI segmentation: select your custom "+ROIseg[11:]+" segmentation file:", os.getcwd())
				
					if "Custom" in cloneSeg:
						gdCS.addMessage("Clone Segmentation", Font("Sanserif", Font.BOLD, 14))
						gdCS.addStringField("Add a name for your file!", "New_"+cloneSeg[11:])
						gdCS.addFileField("Select your custom "+cloneSeg[11:]+" segmentation file:", os.getcwd())
						
					if "Custom" in singleCellMethod:
						gdCS.addMessage("Single Cell Segmentation", Font("Sanserif", Font.BOLD, 14))
						gdCS.addStringField("Add a name for your file!", "New_"+singleCellMethod[11:])
						gdCS.addFileField("Select your custom "+singleCellMethod[11:]+" segmentation file:", os.getcwd())	
							
					if "Custom" in cellDeathSegMethod:
						gdCS.addMessage("Cell Death Segmentation", Font("Sanserif", Font.BOLD, 14))
						gdCS.addStringField("Add a name for your file!", "New_"+cellDeathSegMethod[11:])
						gdCS.addFileField("Select your custom "+cellDeathSegMethod[11:]+" segmentation file:", os.getcwd())
				
					gdCS.showDialog()
					if gdCS.wasOKed():

						error = 0
						weka = 0
						
						if "Custom" in ROIseg:
							ROIname = gdCS.getNextString() 
							sourcePath = gdCS.getNextString()
							if "Weka" not in ROIseg:
								destPath = "_"+ROIname+"_"+ROIseg[11:]+"_customSegment_.py"
							else:
								if "_3D_" in ROIseg:
									destPath = ROIname+"_3D_.model"
								else:
									destPath = ROIname+".model"
								weka = 1
							if (destPath not in fijifiles) and weka == 0:
								destPath = os.path.join(dir2, destPath)
								copy(sourcePath, destPath)
							elif (destPath not in fijifiles) and weka == 1:
								destPath = os.path.join(dir2, "Weka_Models", destPath)
								copy(sourcePath, destPath)
							else:
								error = 1		

						weka = 0
						if "Custom" in cloneSeg:
							cloneName = gdCS.getNextString()
							sourcePath = gdCS.getNextString()
							if "Weka" not in cloneSeg:
								destPath = "_"+cloneName+"_"+cloneSeg[11:]+"_customSegment_.py"
							else:
								if "_3D_" in cloneSeg:
									destPath = cloneName+"_3D_.model"
								else:
									destPath = cloneName+".model"
								weka = 1
							if (destPath not in fijifiles) and weka == 0:
								destPath = os.path.join(dir2, destPath)
								copy(sourcePath, destPath)
							elif (destPath not in fijifiles) and weka == 1:
								destPath = os.path.join(dir2, "Weka_Models", destPath)
								copy(sourcePath, destPath)
							else:
								error = 1
									
						weka = 0	
						if "Custom" in singleCellMethod:
							cellName = gdCS.getNextString()
							sourcePath = gdCS.getNextString()
							if "Weka" not in singleCellMethod:
								destPath = "_"+cellName+"_"+singleCellMethod[11:]+"_customSegment_.py"
							else:
								if "_3D_" in cellName:
									destPath = cellName+"_3D_.model"
								else:
									destPath = cellName+".model"
								weka = 1
							if (destPath not in fijifiles) and weka == 0:
								destPath = os.path.join(dir2, destPath)
								copy(sourcePath, destPath)
							elif (destPath not in fijifiles) and weka == 1:
								destPath = os.path.join(dir2, "Weka_Models", destPath)
								copy(sourcePath, destPath)
							else:
								error = 1	
								
						weka = 0		
						if "Custom" in cellDeathSegMethod:
							deathName = gdCS.getNextString()
							sourcePath = gdCS.getNextString()
							if "Weka" not in cellDeathSegMethod:
								destPath = "_"+deathName+"_"+cellDeathSegMethod[11:]+"_customSegment_.py"
							else:
								if "_3D_" in deathName:
									destPath = deathName+"_3D_.model"
								else:
									destPath = deathName+".model"
								weka = 1
							if (destPath not in fijifiles) and weka == 0:
								destPath = os.path.join(dir2, destPath)
								copy(sourcePath, destPath)
							elif (destPath not in fijifiles) and weka == 1:
								destPath = os.path.join(dir2, "Weka_Models", destPath)
								copy(sourcePath, destPath)
							else:
								error = 1	
						if error == 1:
							IJ.error("One of your names is already taken! Please enter a new name.")
							looper = 1
					else:
						IJ.error("No segmentation scheme was added!")	
						
		
		
			
	
	
	return ok