# this script will parse the NeuroMap output directories for a given list of subjects, looking for ".complete" files for each data modality. it then compiles this information to produce a succinct report on the status of the pipeline.

# Author: Austin Marcus (axm6053@psu.edu)

# PROGRAM INPUT OUTPUT
# INPUT:
# 	1) a file with a list of subjects that have raw data: means that these subjects are expected to have been fully processed by the pipeline
#	2) a mapping of locations in which to look for which kind of complete files.
#	3) the base location of the output from NeuroMap
# OUTPUT:
#	a file with a succinct report on which data modalities for which subjects completed or not

from enum import Enum, auto
import sys
import os
import pipeline_status_helper as helper
import compile_output

# represents data modality
class modality_t(Enum):
	MRIQC = auto()
	HEUDICONV = auto()
	FMRIPREP = auto()
	FIDELITY = auto()

	def __str__(self):

		if self == modality_t.MRIQC:
			return "MRIQC Checks"
		elif self == modality_t.HEUDICONV:
			return "BIDS Data"
		elif self == modality_t.FMRIPREP:
			return "FRMIPREP Processed"
		elif self == modality_t.FIDELITY:
			return "Fidelity Checks"

# organizes and maintains information about which processes/modalities ran to completion for a specific subject
class subject:

	colspace = 20 # format output spacing
	
	# INPUT:
	#	idNum: the subject id number
	#	mapping: a dictionary mapping data modality to location and names
	def __init__(self, idNum, mapping):
		self.idNum = int(idNum)
		# generate dict: tracks which are complete: None means error or incomplete. if successful, replaced with date of completion
		self.modal_status = {i: None for i in mapping}
		self.complete = True

	def changeStatus(self, mode, newVal):
		self.modal_status[mode] = newVal

	# generates an output string specifying the status of a particular modality for this subject
	def _getStatusString(self, mode):
		status = self.modal_status[mode]
		if status == None:
			string = "INCOMPLETE"
		else:
			string = status

		return string.rjust(subject.colspace, " ")

	# generate output string header
	def genHeader(self):
		text = "sub "
		for mode in self.modal_status:
			text += "\t%s" % str(mode).rjust(subject.colspace, " ")

		return text

	# generate output in tabulated manner
	def __str__(self):
		text = "%03d:" % self.idNum
		for mode in self.modal_status:
			text += "\t%s" % self._getStatusString(mode)

		return text

	# check that each expected file exists
	def checkFiles(self, modeMapping):
		self.complete = True
		for mode in modeMapping:
			expectedFile = helper.ensureTrailingSlash(modeMapping[mode]["location"]) + ("sub-%03d/" % self.idNum) + modeMapping[mode]["name"]
			if os.path.exists(expectedFile):
				info = open(expectedFile, "r").read()
				info = "".join(i for i in info if i != "\n")
				self.changeStatus(mode, info)
			else:
				self.complete = False

# uses classes defined above to handle and process information
# INPUT:
#	expectationFilepath: the path to a file mapping subject ids to ACI jobids that are processing that subject's data
#	baseLoc: the NeuroMap output root directory
def driver(expectationFilepath, baseLoc):

	# TODO: convert to YAML encoding
	modeMapping = { 
		modality_t.MRIQC: {"location": "mriqc_IQMs", "name": ".complete"},
		modality_t.HEUDICONV: {"location": "bids", "name": ".heudiconv.complete"},
		modality_t.FMRIPREP: {"location": "MR_Proc/fmriprep", "name": ".complete"},
		modality_t.FIDELITY: {"location": "bids", "name": ".fidelity.complete"} 
	}

	# parse expectationFilename to get list of subject numbers
	fd = open(expectationFilepath, "r")
	subjects = [subject(i.split("\t")[0], modeMapping) for i in fd.read().split("\n") if i != ""]
	fd.close()

	if len(subjects) <= 0:
		print("%s: ABORTING: No subjects were identified" % sys.argv[0], file=sys.stderr)
		sys.exit(-1)

	# calculate locations of .complete files
	for mode in modeMapping:
		modeMapping[mode]["location"] = helper.ensureTrailingSlash(baseLoc) + modeMapping[mode]["location"]

	# check each subject has .complete files
	for sub in subjects:
		sub.checkFiles(modeMapping)

	subjects = sorted(subjects, key=lambda x: x.idNum)

	# find which subjects are incomplete
	incompleteSubs = ["%03d" % i.idNum for i in subjects if not i.complete]

	# generate report
	text = subjects[0].genHeader() + "\n"
	text += "\n".join(str(sub) for sub in subjects)

	if len(incompleteSubs) > 0:
		text += "\n" + "Incomplete Subjects: %s" % ", ".join(incompleteSubs)
	else:
		text += "\n" + "All subjects are complete"

	return text

# compiles 3 reports together:
#	1) the status of the entire pipeline
#	2) stderr of processes run on the current call to preprocess.sh
#	3) the fidelity checks run on the current call to preprocess.sh
def getPipelineStatus():
	expectationFilepath = sys.argv[1]
	baseLoc = sys.argv[2]
	outputFileDir = sys.argv[3]

	report = ""
	report += driver(expectationFilepath, baseLoc)
	report += "\n\n"
	for i in compile_output.driver(expectationFilepath, outputFileDir):
		report += i + "\n"

	return report
	
print(getPipelineStatus())
