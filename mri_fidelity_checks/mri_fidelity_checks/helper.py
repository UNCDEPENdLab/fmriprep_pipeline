# helper functions for use in feeder.py and compare.py
# Author: Austin Marcus (axm6053@psu.edu)

import os
import sys
from enum import Enum, auto

class fidel_result(Enum):
	PASS = auto()
	FAIL = auto()
	
class failure_reason(Enum):
	NOT_PRESENT = auto()
	NOT_EQUAL = auto()

# This class is used to encode/decode the raw text saved in the fidelity output tsv files
class fidelity_check:

	diff_delim = "/"
	delim = ":"
	addr_char = "@"

	def __init__(self, field, status=None, diff=None):
		self.field = field
		self.status = status
		self.diff = diff

	def result(self):
		if self.status == fidel_result.PASS:
			return (True, None)
		else:
			return (False, self.diff)

	def passCheck(self):
		self.status = fidel_result.PASS

	def failCheck(self, diff):
		self.status = fidel_result.FAIL
		self.diff = diff

	def getName(self):
		return self.field

	def str_diff(self, diff):
		if diff == failure_reason.NOT_PRESENT:
			return "not present"
		else:
			return str(diff)

	def printNoName(self):
		passed, diff = self.result()
		if passed:
			return "pass"
		else:
			return "fail" + fidelity_check.delim + self.str_diff(diff)

	def print(self):
		passed, diff = self.result()
		if passed:
			return self.getName() + fidelity_check.delim + "pass"
		else:
			return self.getName() + fidelity_check.delim + "fail" + fidelity_check.delim + self.str_diff(diff)

	@staticmethod
	def parseNoName(raw):
		fields = raw.split(fidelity_check.delim)
		if fields[0] == "pass":
			return (True, None)
		elif fields[0] == "fail":
			expected = []
			actual = []
			index = []
			for comparison in fields[1:]:
				data = comparison
				if fidelity_check.addr_char in data:
					x = comparison.split(fidelity_check.addr_char)
					data = x[0]
					index.append(x[1])

				data = data.split(fidelity_check.diff_delim)

				expected.append(data[0])
				actual.append(data[1])

			return (False, expected, actual, index)
		
# INPUT:
#	start_index is the first index of sys.argv that is a number of range value
# OUTPUT:
#	a list of numbers in numerical form. all ranges have been expanded inclusively
def readNumsOrRangesFromCmdLine(start_index):
	nums = []
	for range_or_num in sys.argv[start_index:]:
		getNums(nums, range_or_num)

	return nums

# INPUT: either a number in string format "4" or a range in string format "5-90"
# OUTPUT: expands the ranges and adds each number to the num_list
def getNums(num_list, spec):
	if "-" in spec:
		spec_split = spec.split("-")
		a = int(spec_split[0])
		b = int(spec_split[1])
		for i in range(a,b+1): # INCLUSIVE range
			num_list.append(i)
	else:
		num_list.append(int(spec))

# INPUT:
#	path is a path to a directory
#	regex_list is a list of regex searches
# OUPUT: returns a list of all the files found using the given regexs 
def getFilepaths(path, regex_list):

	raw = ""
	for regex in regex_list:
		raw += os.popen("find " + path + " -regex " + regex).read()
	
	paths = raw.split("\n")
	if path[-1] != "/":
		path = path + "/"
	i = 0
	while i < len(paths):
		if paths[i] == "":
			del paths[i]
			continue

		i+=1

	return paths

# returns the whether dealing with a json or a nifti file based on the actual file suffix
def getFileSuffix(filename):
	filename = getFilenameFromPath(filename)

	fileSuffix = filename[filename.find("."):]

	if fileSuffix == ".json":
		return "json"
	elif fileSuffix == ".nii.gz":
		return "nifti"
	else:
		return -1

# returns the type of scan (e.g. bold or sbref)
def getScanType(filename):
	filename = getFilenameFromPath(filename)

	scanType = filename.split(".")[0].split("_")[-1]

	return scanType

# returns the subject number (as a number) by parsing the filename
# returns -1 if filename 
def getSubjectIdOfPath(path):
	filename = getFilenameFromPath(path)

	if "sub" not in filename:
		print("ERROR: " + filename + " does not conform to naming conventions.")
		return None

	return int(filename.split("_")[0].split("-")[1])


def getTaskFromTsvFilename(tsv_filename):
	filename = getFilenameFromPath(tsv_filename)

	return filename[:filename.find(".")]

# returns the task name
# filename is the name of a .json or .nii.gz file in the bids directory
def getTaskFromFilename(filename):
	filename = getFilenameFromPath(filename)

	# split name on "_"
	filename_split = filename.split("_")

	if "task" not in filename_split[1]: # if scan type not specified as "task-<scan>"
		if "T1w" in filename_split[1]:
			return "T1w"
		else:
			#print("ERROR: " + filename + " does not specify a task.")
			return None

	scan = filename_split[1].split("-")[1]
	# check if part of the scan name had a "_" in it
	if "run" not in filename_split[2]:
		scan += "_" + filename_split[2]

	return scan

def getFilenameFromPath(path):

	if path == None:
		return -1
	elif "/" not in path:
		filename = path
	else:
		filename = path[path.rfind("/")+1:]

	return filename

def getUpperPathFromPath(path):
	if path == None:
		return -1
	elif "/" not in path:
		return None
	else:
		return path[:path.rfind("/")+1]

def ensureTrailingSlash(string):
	if string[-1] != "/":
		return string + "/"
	else:
		return string

# extract run number from filename to perform fidelity checks on each run of a task
def getRunNumberFromFilename(filename):
	filename = getFilenameFromPath(filename)
	filename_split = filename.split("_")
	runNum = 1 # if filename doesn't contain "run", default value set to 1
	for part in filename_split:
		if "run" in part:
			runNum = int(part.split("-")[1])
			break

	return runNum
 
 
