# PURPOSE
# This file reads the output files of feeder.py and turns this information to
# be more user-friendly. It will output, by subject, which fields failed to
# match the correct value to stdout.
# Author: Austin Marcus (axm6053@psu.edu)

from random import choice
import helper
import sys

# core objects
# subject. maintains a list of checks it failed
# failed check. name, scan task and type

class subject:

	def __init__(self, subID):
		self.failedChecks = []
		self.subID = subID

	def addFailedCheck(self, check):
		self.failedChecks.append(check)

	def printStatus(self):
		print("Subject #" + str(self.subID))
		if len(self.failedChecks) == 0:
			print("\tNone")
		else:
			for check in self.failedChecks:
				print("\t", end="")
				check.print()

	def matches_id(self, ID):
		if self.subID == ID:
			return True
		else:
			return False

class check:
	
	def __init__(self, fieldName, task, scanType, expected, actual, index=None):
		self.fieldName = fieldName
		self.task = task
		self.scanType = scanType
		self.expected = expected
		self.actual = actual
		self.index = index

	def print(self):
		if len(self.index) == 0:
			print("%s %s %s\n\t\tExpected: %s\n\t\tActual: %s" % (self.task, self.scanType, self.fieldName, self.expected[0], self.actual[0]))
		else:
			print("%s %s %s" % (self.task, self.scanType, self.fieldName))
			for i in range(len(self.expected)):
				print("\t\tIndex: %s\tExpected: %s\tActual: %s" % (self.index[i], self.expected[i].ljust(12, " "), self.actual[i].ljust(12, " ")))

# INPUT:
# 	subs is a list of subject objects
#	ID is an int
# OUTPUT:
#	the subject object that has the matching ID
#	None, if there is no such subject
def getSub(subs, ID):
	for s in subs:
		if s.matches_id(ID):
			return s
	return None

# INPUT:
# 	the filename of a .tsv file with fidelity checks
#	subs is a list of subject objects
# OUTPUT:
# 	extracts the failed checks from the file and, if the corresponding subject is in subs, adds those checks to it
def extractChecksIntoSubs(filename, subs):
	fd = open(filename, "r")
	task = helper.getTaskFromTsvFilename(filename)

	all_lines = fd.read().split("\n")

	delim = "\t"
	#headers = fd.readline().split(delim)
	headers = all_lines[0].split("\t")[:-1]

	#line = fd.readline().split(delim)[:-1]
	#while line:
	for line in all_lines[1:][:-1]:
		line = line.split("\t")[:-1]
		checks_from_line = []

		subID = int(line[1])
		scanType = line[2]

		# get the subject object if in the list of subs
		cur_sub = getSub(subs, subID)
		if cur_sub == None: # if subject is not in list, don't bother checking
			continue

		# look at each check entry in the row
		offset = 3
		for i in range(offset, len(line[offset:])+offset):
			cur_check = line[i]
			field_name = headers[i]
			if cur_check == "": # skip over empty check values
				continue

			x = helper.fidelity_check.parseNoName(cur_check)
			if x[0] == False: # check was "fail"
				cur_sub.addFailedCheck(check(field_name, task, scanType, x[1], x[2], index=x[3]))

		#line = fd.readline().split(delim)[:-1]

def main():
	dir_path = sys.argv[1]
	# read command line input and prep desired subjects
	nums = helper.readNumsOrRangesFromCmdLine(2)

	# prep subjects
	nums.sort()
	subs = []
	for i in nums:
		subs.append(subject(i))

	# extract on each file
	search = ["\".*\.tsv\""]
	files = helper.getFilepaths(dir_path, search)
	for f in files:
		extractChecksIntoSubs(f, subs)

	# output subjects
	for i in subs:
		i.failedChecks = sorted(i.failedChecks, key=lambda x: x.fieldName.lower())
		i.failedChecks = sorted(i.failedChecks, key=lambda x: x.scanType)
		i.failedChecks = sorted(i.failedChecks, key=lambda x: x.task)
		i.printStatus()

main()
