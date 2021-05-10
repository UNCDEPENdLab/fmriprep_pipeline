# PURPOSE
# This file reads a json template file and constructs a dictionary of
# field-value pairs. When passed a relevant fidelity file, it compares the
# empirical field values in it against this dictionary and returns which tests
# it passed and which it failed
# Author: Austin Marcus (axm6053@psu.edu)

# Assumptions being made:
##Questionable:
## The target json sub-elemnt name should exactly match oart of the file name
#	# however, this may be inconsistent. Best match is with the SeriesDescription element
## a "-" in json fields is equivalent to a "_" --> should remove this assumption. made because of mismatch with SeriesDescription.
#
##Valid:
## values from json template that are a single element vector (e.g. [5]) are equivalent to the single element (data from experimental json files is not bracketed) --> valid
## if a list (dim or pixdim) is longer in the experimental data than the list given in the template json, only look at up till the length of the template list

import json
import os
import sys
import helper

TOL = .0001
manual_verify = False

# this class organizes the data associated with a single file analysis: a comparison of a single file against the json template file
class fileAnalysis:

	def __init__(self, filename, fileSuffix, task, scanType, subID, fidelityChecks):
		self.filename = filename # the filename, not the path
		self.fileSuffix = fileSuffix # the suffix of the file (json or nii)
		self.task = task # the task done during the scan. (rest_post, ...)
		self.scanType = scanType # the type of scan done. (bold or sbref)
		self.subID = subID
		self.fidelityChecks = fidelityChecks # array of fidelity_check objects
	
	# INPUT:
	#	other: a fileAnalysis object
	# OUTPUT:
	#	True if the only thing different between this fileAnalysis and the other is their file suffix. that is, they represent the same scan, just one is the nifti file and the other is the json file
	#	False otherwise
	def sameScan(self, other):
		if self.task == other.task and self.scanType == other.scanType and self.subID == other.subID and helper.getUpperPathFromPath(self.filename) == helper.getUpperPathFromPath(other.filename):
			return True
		return False

	def print(self):
		print("Scan type: " + self.task + "\nFile type: " + self.scanType + "\nFile suffix: " + self.fileSuffix + "\nSubject Id: " + str(self.subID))
		print("\t" + str(self.fidelityChecks))
		print()

# fed an experimental json file and compares against values
class fidelityTemplate:
	
	def __init__(self, jsonFileName):
		f = open(jsonFileName, "r")

		# convert raw json to dictionary
		self.data = json.load(f)
		for dict1 in self.data:
			for key1 in self.data[dict1]:
				for key2 in self.data[dict1][key1]:
					self.data[dict1][key1][key2] = self.interpret(self.data[dict1][key1][key2])
	
	# takes as input a string, and attempts to convert it to a number
	# if given a list with one element, will break it out of the list as well
	def interpret(self, data):

		if type(data) == list and len(data) == 1: # breaks out single element list
			data = data[0]
		
		if type(data) == list:
			# iterate on each
			for i in range(len(data)):
				data[i] = self._interpret(data[i])

		else: # just do it once
			data = self._interpret(data)

		return data

	def _interpret(self, data):
		try: # tries to convert to number
			data = float(data)
		except: 
			#data = data.replace("-","_") # assuming it must be a string at this point if not a number
			pass

		return data
		
	# decides how to get data from given file
	def getExData(self, exFileName, fileSuffix):
		# select wether comparing to json file or nifti file 
		if fileSuffix == "json":
			
			f = open(exFileName, "r")
			exData = json.load(f)

		elif fileSuffix == "nifti": 
			exData = {}
			raw = os.popen("fslhd " + str(exFileName)).read()
			for line in raw.split("\n"):
				line = line.split("\t")
				lineParts = list(filter(lambda x: x != "\t" and x != "", line))
				if len(lineParts) != 2:
					continue
	
				lineParts[1] = self.interpret(lineParts[1])
		
				# consider aggregating specific fields into array to match format of desired json
				specs = ["dim", "pixdim"]
				matched = False
				for i in specs:
					if i == lineParts[0][:len(i)]: #match
						matched = True
						# if first one, initialize
						if exData.get(i) == None:
							exData[i] = []
						exData[i].append(lineParts[1])
							
				if not matched:
					exData[lineParts[0]] = lineParts[1]

		return exData

	# returns the scan type (rest_pre, rest_post) by extraction from filename
	# returns file type (json or nifti) by checking file suffix
	# returns subject ID number
	def parseFileName(self, filename):
		subID = helper.getSubjectIdOfPath(filename)
		task = helper.getTaskFromFilename(filename)
		if task == "T1w":
			scanType = "---"
		else:
			scanType = helper.getScanType(filename)
		fileSuffix = helper.getFileSuffix(filename)
		
		# add "_sbref" in task name if scanType is sbref to match task names in neuromap_validation.json (template)
	
		return task, fileSuffix, subID, scanType

	def guessIsKey(self, key, guess):
		key_mod = key.replace('_', '').replace('-', '').lower()
		guess = guess.replace('_', '').replace('-', '').lower()

		return key_mod == guess

	def isTaskInTemplate(self, task):
		# check if task is specified in template file
		try:
			self.data[task]
			return True
		except:
			for key in list(self.data.keys()):
				if self.guessIsKey(key, task):
					return key
			return False

	# pass file name to compare object. 
	# checks that target fields match fields in the data file
	# returns a 3-tuple: (task, subID, output)
	# 		output: array of 2-tuples: (field, [0,1])
	def compareToFile(self, exFileName):
		# parse filename to get scan and file type
		task, fileSuffix, subID, scanType = self.parseFileName(exFileName)
		if task == None:
			return None

		ret = self.isTaskInTemplate(task)
		if ret == False:
			return None
		elif ret != True:
			task = ret
	
		# task name in template file has <taskName>_sbref for sbref files
		if scanType == "sbref":
			taskTemp = task + "_sbref"
		else:
			taskTemp = task

		# get the data from the file
		exData = self.getExData(exFileName, fileSuffix)
		# select which set of fidelity checks applies to this file
		checks = self.data[taskTemp][fileSuffix]
		
		output = []
		if (manual_verify):
			print("file: " + exFileName)
		for key in checks: # attempting to check only based on fields present in template file; assumption that data files should have a superset
			if (manual_verify):
				print("key: " + key + "\n\ttemplt: " + str(checks[key]) + "\n\tactual: ", end="")
			cur_check = helper.fidelity_check(key)
			# check if key in data file
			try:
				exData[key]
				if (manual_verify):
					print(exData[key], end="\n")
			except:
				if (manual_verify):
					print("not present", end="\n")
				cur_check.failCheck(str(checks[key]) + helper.fidelity_check.diff_delim + str("not present"))
				output.append(cur_check)
				continue

			equal, indicies = self.compareObjects(exData[key], checks[key])
			if equal:
				cur_check.passCheck()
				if (manual_verify):
					print("\t0", end="")
			else:
				# if it differed by an element in a list, pass the index
				if indicies != None:
					# build string: "<expected>/<actual>@<index>:..."
					result = ""
					for i in indicies:
						result += ("%s%s%s%s%d" % (str(checks[key][i]), helper.fidelity_check.diff_delim, str(exData[key][i]), helper.fidelity_check.addr_char, i))
						result += ":"
					result = result[:-1]

					cur_check.failCheck(result)
				else:
					cur_check.failCheck(str(checks[key]) + helper.fidelity_check.diff_delim + str(exData[key]))
				if (manual_verify):
					print("\t1", end="")

			if (manual_verify):
				print()
			output.append(cur_check)

		if (manual_verify):
			print()

		# sort output by key name to ensure consistency
		output = sorted(output, key=lambda x: x.getName()) 

		return fileAnalysis(exFileName, fileSuffix, task, scanType, subID, output)

	# assuming objects have been interpreted at this point
	# takes two objects, that is, a piece of text, a number, or an array of either and tests their equality
	# these objects are the values of the fields from the template json and the experimental data
	def compareObjects(self, exOb, templateOb):

		# compare each element of a list
		if type(exOb) == list and type(templateOb) == list:
			indicies = []
			for i in range(len(templateOb)):
				if self._compareObjects(exOb[i], templateOb[i]) == False:
					indicies.append(i)
		
			if len(indicies) == 0:
				return (True, None)
			else:
				return (False, indicies)
			
		elif type(exOb) == list or type(templateOb) == list:
			return (False, None)
		else:
			return (self._compareObjects(exOb, templateOb), None)

	def _num_in_tol(self, num1, num2):
		return num1 < num2 + TOL and num1 > num2 - TOL

	def _compareObjects(self, exOb, templateOb):

		if type(templateOb) == str and type(exOb) != str:
			# get number in template object
			num = float(templateOb[:-1])
			if templateOb[-1] == "+":
				if self._num_in_tol(num, exOb) or num < exOb:
					return True
				else:
					return False
		elif type(templateOb) == str and type(exOb) == str:
			if exOb == templateOb:
				return True
			else:
				return exOb.replace("-","_") == templateOb.replace("-","_")
		else:
			return self._num_in_tol(exOb, templateOb)
				
