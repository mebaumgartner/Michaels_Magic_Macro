def user_made_code(mode, imp, stack, rm, stackno, pouchROI, pouchROI2):

	import os
	from ij.gui import ShapeRoi
	
	if mode == "Make Your Own New Script!":
		mode = "_Template.py"

	mode = os.path.splitext(mode)[0]

	exec("from JSF_End_User_Made_Code."+str(mode)+" import main")

	imp = imp.duplicate()
	stack = stack.duplicate()
	if pouchROI != 0:
		pouchROI = ShapeRoi(pouchROI.clone())
	if pouchROI2 != 0:	
		pouchROI2 = ShapeRoi(pouchROI2.clone())
	stackno = int(stackno)

	outImp = main(imp, stack, rm, stackno, pouchROI, pouchROI2)
	
	return outImp