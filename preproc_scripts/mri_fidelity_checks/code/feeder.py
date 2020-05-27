# PURPOSE
# This file feeds all the relevant scan data found from the specified directory
# down to compare.py. It then organizes the resulting output and writes the
# results of the fidelity checks to output files.
# Author: Austin Marcus (axm6053@psu.edu)

import os
import sys
from functools import cmp_to_key
import helper
import compare

OUTPUT_DIR = "fidelity_output_data" 
WRITE_OK = False

# compares two file paths
# returns a number specifying the numerical order of the subject id numbers contained in the filename
# for use with a sorting function
def comparePathsBySubject(path1, path2):
	id1 = helper.getSubjectIdOfPath(path1)
	id2 = helper.getSubjectIdOfPath(path2)
	if id1 > id2:
		return 1
	elif id1 < id2:
		return -1
	else:
		return 0

# compares two file paths
# returns a number specifying the alphabetical order of the tasks contained in the filename
# for use with a sorting function
def comparePathsByTask(path1, path2):
	task1 = helper.getTaskFromFilename(path1)
	task2 = helper.getTaskFromFilename(path2)

	if task1 == None or task2 == None:
		return 0

	if task1 > task2:
		return 1
	elif task1 < task2:
		return -1
	else:
		return 0

# INPUT: relative paths to fidelity files in a list
# OUTPUT: a list of relative paths to fidelity files. list is 2D. inner lists are grouped by scan
def sortPathsByScan(paths):
	sortedFlat = sorted(paths, key=cmp_to_key(comparePathsByTask))

	sortedFolded = []
	lastPath = sortedFlat[0]
	fold = [lastPath]
	for i in range(1, len(sortedFlat)):
		currPath = sortedFlat[i]
		if comparePathsByTask(lastPath, currPath) == 0:
			fold.append(currPath)
		else:
			sortedFolded.append(fold)
			fold = [currPath]

		lastPath = currPath

	if len(fold) > 0:
		sortedFolded.append(fold)

	return sortedFolded

# Asks the user if it is ok to write output data to the gloabally specified directory
def prepOutputDir():
	global WRITE_OK
	if os.path.exists(OUTPUT_DIR):
		pass
		#if not WRITE_OK:
		#	# ask user if ok to overwrite
		#	print("Ok to use " + OUTPUT_DIR + " as output directory? (y/n): ", end="")
		#	if input() != "y":
		#		return -1
		#	else:
		#		WRITE_OK = True
	else:
		os.mkdir(OUTPUT_DIR)

	return 0;

# INPUT:
#	A task name
# OUTPUT:
# 	The tsv filename for output data corresponding to the specified task
def getTaskFilename(task):
	return OUTPUT_DIR + "/" + task + ".tsv"

# INPUT:
#	A task name
# OUTPUT:
#	True if the corresponding output task file already exists, False otherwise
def checkIfTaskFileExists(task):
	filename = getTaskFilename(task)
	newfile = True
	if os.path.exists(filename):
		newfile = False

	return not newfile

# INPUT:
#	col: a collated pair of fileAnalysis objects
#	suf: text, a file suffix. either "json" or "nii"
# OUTPUT:
#	the fileAnalysis object in col corresponding to the file suffix
#	None if that is not in col
def getSuffixCol(col, suf):
	for i in col:
		if i != None:
			if i.fileSuffix == suf:
				return i
	return None

# INPUT:
#	fd is a file descriptor: assuming it references a clean file
# 	data is a list of collated fileAnalysis objects
# OUTPUT:
#	writes the headers of the task to the file
def write_headers(fd, data):
	fd.write("UpperPath\tSubId\tType\t")
	fileSuffixes = ["json", "nifti"]
	fieldCount = [0, 0]
	for i in range(len(fileSuffixes)):
		suffix = fileSuffixes[i]
		example = getSuffixCol(data[0], suffix)
		if example == None:
			continue
		for entry in example.fidelityChecks:
			fieldCount[i] += 1
			field = entry.getName()
			fd.write(field + "\t")

	fd.write("\n")

# INPUT: data is a collated list of fileAnalysis objects
# writes the data to the file corresponding to the task in subject order
# will write robustly: if the same line (same path, subject and scanType), will replace it with the newer version
# will determine if corresponding file existed already or not and update accordingly.
def write_data(task, data):
	newfile = not checkIfTaskFileExists(task)
	
	# write headers
	if newfile == True:
		fd = open(getTaskFilename(task), "w")
		write_headers(fd, data)
		text = [] 
	else:
		fd = open(getTaskFilename(task), "r")
		text = fd.read().split("\n")[:-1]
		fd.close()
		# add newline back in
		for i in range(len(text)):
			text[i] = text[i] + "\n"

	# get number of json headers (to align text properly)
	num_json_headers = len(getSuffixCol(data[0], "json").fidelityChecks)

	# write each line by subject
	for collate in data: 
		# each collate should have the same scanType and subID
		cur_line = ""

		cur_line += str(helper.getUpperPathFromPath(collate[0].filename)) + "\t" + str(collate[0].subID) + "\t" + collate[0].scanType + "\t"

		# write json data
		cur_line, wrote = write_data_if(cur_line, "json", collate)
		if wrote == False:
			cur_line = write_blanks(cur_line, num_json_headers)
		# write nifti data
		cur_line, wrote = write_data_if(cur_line, "nifti", collate)

		cur_line += "\n"

		# special insertion is only necessary when dealing with a file that is not new
		# casing done for optimization
		if newfile == False:
			organizeLines(text, cur_line)
		else:
			text.append(cur_line)
	
	text = "".join(text)
	
	if newfile:
		fd.write(text)
		fd.close()
	else:
		fd = open(getTaskFilename(task), "w")
		fd.write(text)
		fd.close()

# INPUT: line is a string, a single line from a fidelity checks .tsv file
# OUTPUT: returns a 3-tuple of the first 3 attributes on a line: file path, subject id, scan type (bold or sbref)
def getLineAttrs(line):
	parts = line.split("\t")
	return (parts[0], eval(parts[1]), parts[2])

# INPUT: 
#	lines is a list of strings which are terminated by a newline
#	new_line is the current line being conisdered for placement in this list
# OUTPUT:
#	modifies lines with new_line inserted into the appropriate place
# This function decides whether or not a new line should replace an old line in a file and, if not, where to place it
def organizeLines(lines, new_line):
	
	
	newAttrs = getLineAttrs(new_line)
	offset = 1
	for i in range(offset, len(lines[offset:]) + offset): # don't process the header line
		curAttrs = getLineAttrs(lines[i])

		if newAttrs[1] < curAttrs[1]: # compare subjects
			
			# insert before
			lines.insert(i, new_line)
			return
		# replacement should occur if the first 3 fields are the same (file, id, scan type)
		elif newAttrs[1] == curAttrs[1] and newAttrs[0] == curAttrs[0] and newAttrs[2] == curAttrs[2]: # determine if new should replace the current line
			lines.insert(i, new_line)
			lines.pop(i+1)
			return

	# insert after "-" at the end of the file
	lines.append(new_line)

# INPUT:
#	text: the string to add text to
#	suf: the file suffix
#	collate: a collate
# OUTPUT: 
#	True if the fileAnalysis object with the given suffix existed in the given collate, false otherwise
#	returns the modified string, text
def write_data_if(text, suf, collate):
	suf_col = getSuffixCol(collate, suf)
	if suf_col != None:
		for entry in suf_col.fidelityChecks:
			text += entry.printNoName() + "\t"
		return (text, True)
	return (text, False)

# Writes num tabs to string text
def write_blanks(text, num):
	for i in range(num):
		text += "\t"
	return text

# INPUT: fidelData is an array of fileAnalysis objects sorted by subject id
# OUTPUT: a list of pairs of fileAnalysis objects. Nifti and json files with the same scan type, task name and subject id are paired together 
# done to make it easier to write data in the correct order to output files
def collateFileAnalyses(fidelData):
	collated = []
	i = 0

	# iterate through all possible pairs. if a pair meets the criteria for
	# being a collate, add that collate to the return value and remove those
	# objects from the list so they are not used again.

	while i < len(fidelData):
		foundPair = False
		j = i + 1
		while j < len(fidelData):
			if fidelData[i].sameScan(fidelData[j]):
				collated.append( [fidelData[i],fidelData[j]] )
				foundPair = True
				del fidelData[j]
				del fidelData[i]
				i-=1
				break
			j+=1
		
		if not foundPair:
			collated.append( [fidelData[i], None] )
			del fidelData[i]
			i-=1

		i+=1

	return collated
		

# INPUT: fidelData is an array of fileAnalysis objects
# writes all the fidelity data for a scan to a file
def writeScanCheck(fidelData):
	#if prepOutputDir() < 0:
	#	print("intended output directory was vetoed by user: aborting write")
	#	exit()
	prepOutputDir()

	task = fidelData[0].task
	fidelData = collateFileAnalyses(fidelData)
	write_data(task, fidelData)

# INPUT: 
#	sub_id_list is a list of subject ids in string format
# OUTPUT:
#	returns a list of regexs that will match the names of data files for those subjects
def generateRegexs(sub_id_list):
	fileTypes = ["\.json", "\.nii\.gz"]
	subPrefix = "sub-"

	regex_list = []
	for sub_id in sub_id_list:
		for file_type in fileTypes:
			r = ".*" + subPrefix + "0*" + sub_id + "[^0-9].*" + file_type
			regex_list.append(r)

	# if no subjects passed, find all
	if len(sub_id_list) == 0:
		for file_type in fileTypes:
			r = ".*" + subPrefix + "[0-9]*.*" + file_type
			regex_list.append(r)

	return regex_list
	
# INPUT:
#	templatePath is the path to the json file containing the desired fidelity check values
#	dataDir is the path to the BIDS formatted directory containing the subject mri data
#	subIDs is a list of subject ids (in numerical format)
# OUTPUT:
#	gets all relevant files of the subjects listed beneath dataDir, compares their fidelity values against templatePath, and outputs the results by writing to a file
def checkFiles(templatePath, dataDir, subIDs):

	# filter out files we don't need. if not done, may interfere with the sorting process because some files aren't orderable
	files = []
	regexs = generateRegexs(subIDs)
	for f in helper.getFilepaths(dataDir, regexs):
		if helper.getTaskFromFilename(f) != None:
			files.append(f)
	
	if len(files) == 0:
		print("feeder.py: WARNING: No matching files were found. Aborting...")
		sys.exit(-1)


	# sort files by their corresponding scan
	filesByScan = sortPathsByScan(files)

	# initialize comparator
	comparator = compare.fidelityTemplate(templatePath)
	
	# operate on one sub-list (all files corresponding to a particular scan) at a time
	for filesOfScan in filesByScan:

		aggOut= []
		for fidelityFile in filesOfScan:
			# process each fidelityFile individually (comparing fidelityFile against corresponding template in templatePath fidelityFile)
			fidelityOut = comparator.compareToFile(fidelityFile)

			# aggregate output results of verification by incrementally constructing data structure
			if fidelityOut != None:
				aggOut.append(fidelityOut)


		if len(aggOut) > 0:
			# sort scan output by subject
			aggOut = sorted(aggOut, key=lambda l:l.subID)
			# write to an output file
			writeScanCheck(aggOut)

# parse command line arguments and feed to main function
OUTPUT_DIR = sys.argv[3] + "/" + OUTPUT_DIR
nums = helper.readNumsOrRangesFromCmdLine(4)
nums = list(str(i) for i in nums)
checkFiles(sys.argv[1], sys.argv[2], nums)
