#########################################################################################################################################################
#
#
# Jython macro template!
#
#
#########################################################################################################################################################

# Hi there!
#
# This is the template for a script that you can customise and incorporate into Michael's Magic Macro!
# This default script is set to Jython, but all imageJ-compatible languages should work.
#
# There are a few rules, which are as follows:
#
# (1) You must save this script to the 'JSF_End_User_Made_Code' folder. The macro can find it from there. 
# (2) Your code must all be written within a function named 'main'. 
# (3) The main function accepts 6 arguments:
#
#		imp - this is the active image slice for analysis. As in, if the macro is analyzing channel 2, Z-plane 2, timepoint 1, imp will just be the single image corresponding to those indices
#		stack - this is a stack (or a hyperstack, if you specified multiple channels for analysis) of all the Z-planes of the channel you are analyzing. As in, all the clone channel images, for example
#		stackno - this refers to the z-plane that we are actively analyzing. As in, if we are analyzing plane 14 of 23, stackno will be 14
#		rm - this refers to the ROI manager. Everything stored in the roi manager right now is available to you for use in analysis. 
#			 Feel free to pull anything from the roi manager, but ease do not add things to or delete things from the ROI manager from here, as it can mess things up.
#		pouchROI - this variable holds a ShapeRoi for the region of the image we are analyzing (unless you are writing this script to define ROIs, in which case it will be 0).
#		pouchROI2 - this variable holds the second pouch region, if it exists. This is the ROI specified if the user chooses to analyze two regions of an image separately.
#
# (4) The main function returns 1 argument, which must be a single slice ImagePlus which we will use to create ROIs. This ImagePlus must be segmented in the following manner:
#		All regions you do not wish to be analyzed - background or non-relevant parts of the image - must have a pixel value of 0.
#		All other regions must have a pixel value between 1 and 253. 
#		For Clone Segmentation, every pixel with a given value will be grouped together into a genotype for analysis. (i.e. all pixels with a value of 1 will be genotype 1, all with a value of 2 will be genotype 2)
#		For Cell Death analysis and Single Cell analysis, all pixels with a value >= 1 will be lumped into one big group
#		For ROI segmentation, pixel values greater than 1 will all be binned together, unless the user is looking at two separate regions. Then they are binned by pixel values = 1 and pixel values = 2
#
#(5) The exception is fluorescence pre-processing.
#		Here, you get the entire fluorescence channel(s) image stack, and you must return the entire image stack.
#		Just run your pre-processing on it (like a flat field correction or a noise reduction), and return that as the outImp
#
# Everything else is up to you!
#



# Here we define the 'main' function. All your code to be executed must be written in here!
def main(imp, stack, rm, stackno, pouchROI, pouchROI2):

	################################################################
	#****************** Write your code here! *********************#
	################################################################

	
	#This is sample code you can delete. This is what loaded this file and displayed the images

	#Display our 'imp' variable
	imp.setTitle("imp")
	imp.show()

	#Set the pouchROI to 'imp', if it exists
	print pouchROI
	if pouchROI != 0:
		imp.setRoi(pouchROI)

	#Display our 'stack' variable
	stack.setTitle("stack")
	stack.show()

	#Set the pouchROI2 to 'stack', if the pouchROI2 exists
	if pouchROI2 != 0:
		stack.setRoi(pouchROI2)

	#Here, we are copying a this template and writing it to a new file.
	#We then open this new file in the text editor, and now you can work on it without overwriting my beautiful template.
	
	#here we import some modules
	from ij import IJ #If you're new to jython programming, you're almost always gonna want to import IJ. It allows you to access all of ImageJ's plugins
	import os #os is useful for file manipulation
	from shutil import copy #This is copy/paste utility for files

	#You cannot just type out the file directory, as python does not like those. Instead, we use the path.join() function to create a directory
	# os.getcwd() gets the current working directory. For FIJI, this defaults to the Fiji.app folder.
	templatePath = os.path.join(os.getcwd(),"jars","Lib","JSF_End_User_Made_Code","_Template.py")
	destPath = os.path.join(os.getcwd(),"jars","Lib","JSF_End_User_Made_Code","_NewCustomScript.py")

	#We make sure that there isn't already a file called '_NewCustomScript.py' in the 'JSF_End_User_Made_Code' file. 
	pathCount = 1
	start = True
	while start == True:
		if os.path.isfile(destPath):
			destPath = os.path.join(os.getcwd(),"jars","Lib","JSF_End_User_Made_Code","_NewCustomScript("+str(pathCount)+").py")
			pathCount +=1
		else:
			start = False

	copy(templatePath, destPath)

	#This is an example of why importing IJ is so useful. You can run almost any IJ commands with it.
	#If you can't figure out how to do something, just launch the 'Recorder' from from the 'Plugins>Macros>Record...' menu option. 
	#Set the 'Record:' option to Java, and do what you need to do. The recorder will convert your actions to Java code, which can be readily run in Jython. The trailing ';' isn't needed though
	temp = IJ.open(destPath)

	#Print a message to let the user know what's up
	IJ.error("The template for making a new jython script is now open in the text editor! \n \n"+"The instructions are in the comments: \n\nThe images displayed in front of you are the variables 'imp' and 'stack.' \n")
	exit()

	
	################################################################
	#********************* Stop writing here! *********************#
	################################################################


	#Here we return our outImp. This line can't be removed
	return outImp
	
