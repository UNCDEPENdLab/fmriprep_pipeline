# This script internalizes the output of aci jobs, allowing for easy selection and manipulation of their contents. Currently, it is used to generate an error report and a fidelity checks report.

# needs: location to look for output files, location to write report
# process: for each file, parse into error object, keep track of error objects, write report

#NOTE: only output files that have their job ids listed in the expectationFile will be compiled

import re
import sys
import os
from enum import Enum, auto
import pipeline_status_helper as helper

def getFilenameFromPath(path):

	if path == None:
		return -1
	elif "/" not in path:
		filename = path
	else:
		filename = path[path.rfind("/")+1:]

	return filename

class jobOutput:

	class outputType(Enum):
		stdout = auto()
		stderr = auto()

		def __str__(self):
			if self == jobOutput.outputType.stdout:
				return "stdout"
			elif self == jobOutput.outputType.stderr:
				return "stderr"
	
	fileToRegex = {outputType.stderr: "^(?P<script>[a-z0-9_]*)\.e(?P<jobId>[0-9]*)$", outputType.stdout: "^(?P<script>[a-z0-9_]*)\.o(?P<jobId>[0-9]*)$"}

	# filepath: a *.[eo]* aci output file
	def __init__(self, filepath, jobIdToSubId):
		# get needed information
		info = jobOutput.parseFilename(getFilenameFromPath(filepath))
		if info == None:
			print("%s: WARNING: %s did not match expected filename pattern" % (sys.argv[0], filepath), file=sys.stderr)
			self._error_on_init()
			return

		self.script = info[0].group("script")
		self.jobId = int(info[0].group("jobId"))
		self.filetype = info[1]

		# lookup subId with jobId: where to get this information? parse files beforehand and pass mapping in
		try:
			self.subId = jobIdToSubId[self.jobId]
		except:
			self._error_on_init()
			return

		# read and interpret contents of file
		self.content = open(filepath, "r").read()
		if len(self.content) > 0 and self.filetype == jobOutput.outputType.stderr:
			self.error = True
		else:
			self.error = False

	def _error_on_init(self):
		self.script = None
		self.subId = None
		self.jobId = None
		self.error = False
		self.content = None

	def __str__(self):
		return "%s %s %s %s\n#### BEGIN CONTENT ####\n%s#### END CONTENT ####\n" % (self.subId, self.script, self.jobId, self.filetype, self.content)
		
	@staticmethod
	def parseFilename(filename):
		for fileType in jobOutput.fileToRegex:
			match = re.search(jobOutput.fileToRegex[fileType], filename)
			if match:
				return match, fileType
		return None

	def jobDidError(self):
		return self.error

def parseJobsToSub(filepath):
	fileContent = open(filepath, "r").read()
	jobsToSub = {}
	for line in fileContent.split("\n"):
		if line == "":
			continue

		sub = int(line.split("\t")[0])
		for job in line.split("\t")[1].split(","):
			if job == "":
				continue
			jobsToSub[int(job)] = sub
	
	return jobsToSub

def getOutputFiles(sourceDir):
	raw = os.popen("find %s -type f -regex \"%s\"" % (sourceDir, ".*/[0-9a-z_]*\.[oe][0-9]*")).read()
	paths = [ i for i in raw.split("\n") if i != ""]
	return paths

def compileReport(jobs, header):
	if len(jobs) == 0:
		return "%s\n<nothing to report>\n\n" % header
		
	text = "%s" % header
	for job in jobs:
		text += str(job) + "\n"
	return text

def driver(expectationFile, outputFileDir):
	# get jobid to subject mapping
	jobsToSub = parseJobsToSub(expectationFile)

	# find all job output files
	outputFilePaths = getOutputFiles(outputFileDir)

	# construct jobOutput objects, one for each file
	joboutputs = [ jobOutput(path, jobsToSub) for path in outputFilePaths ]
	joboutputs = [ i for i in joboutputs if i.script != None ]
	joboutputs = sorted(joboutputs, key=lambda x: x.subId)
	
	# compile error report
	errorReport = compileReport([j for j in joboutputs if j.jobDidError()], "######## ERROR REPORT ########\n\n")
	# compile fidelity report
	fidelityReport = compileReport([j for j in joboutputs if j.script == "fidelity_checks" and j.filetype == jobOutput.outputType.stdout], "######## FIDELITY REPORT ########\n\n")

	return errorReport, fidelityReport
