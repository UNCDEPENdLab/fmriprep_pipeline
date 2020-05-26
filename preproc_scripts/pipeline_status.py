# this script will parse the output directories of a given list of subjects (built up from running the pipeline, which ones should be completed), looking for .complete files for each data modality. it thencompiles this information to produce a succinct report on the status of the pipeline.

# where to put .complete files?
#	all in the BIDS dir? or in their respective data modalities?
# might be better to put them in their respective modality's directory so that if one is poking around in the files, they can easily see if their current directory has been completed

# INPUT:
# 	a file with a list of subjects with raw data: means that these subjects are expected to have been fully processed by the pipeline
#	a mapping of locations in which to look for which kind of complete files
#	the base location of the output
# OUTPUT:
#	a file with a succinct report on which data modalities for which subjects completed or not
#		also include date completed

from enum import Enum, auto
import sys
import os

class modality_t(Enum):
	MRIQC = auto()
	HEUDICONV = auto()
	FMRIPREP = auto()
	FIDELITY = auto()

# organizes and maintains information about which processes ran to completion for a specific subject
class subject:

	colspace = 20
	
	def __init__(self, idNum, mapping):
		self.idNum = int(idNum)
		# tracks which are complete: None means error or incomplete. if successful, replaced with date of completion
		#self.modal_status = {modality_t.MRIQC: None, modality_t.HEUDICONV: None, modality_t.FMRIPREP: None, modality_t.FIDELITY: None}
		# generate dict
		self.modal_status = {i: None for i in mapping}

	def changeStatus(self, mode, newVal):
		self.modal_status[mode] = newVal

	def _getStatusString(self, mode):
		status = self.modal_status[mode]
		if status == None:
			string = "INCOMPLETE"
		else:
			string = status

		return string.rjust(subject.colspace, " ")

	def genHeader(self):
		text = "sub "
		for mode in self.modal_status:
			text += "\t%s" % str(mode).rjust(subject.colspace, " ")

		return text

	# output in tabulated manner
	def __str__(self):
		text = "%03d:" % self.idNum
		for mode in self.modal_status:
			text += "\t%s" % self._getStatusString(mode)

		return text

	def checkFiles(self, modeMapping):
		# check that each expected file exists
		for mode in modeMapping:
			expectedFile = ensureTrailingSlash(modeMapping[mode]["location"]) + ("sub-%03d/" % self.idNum) + modeMapping[mode]["name"]
			if os.path.exists(expectedFile):
				info = open(expectedFile, "r").read()
				self.changeStatus(mode, info)

def ensureTrailingSlash(string):
	if string[-1] != "/":
		return string + "/"
	else:
		return string

def driver():
	# parse cmd line
	expectationFilepath = sys.argv[1]
	baseLoc = sys.argv[2]

	modeMapping = { 
		modality_t.MRIQC: {"location": "mriqc_IQMs", "name": ".complete"},
		modality_t.HEUDICONV: {"location": "bids", "name": ".heudiconv.complete"},
		modality_t.FMRIPREP: {"location": "MR_Proc/fmriprep", "name": ".complete"},
		modality_t.FIDELITY: {"location": "bids", "name": ".complete"} 
	}

	#modeMapping = {
	#	"Type 1": {"location": "thing1", "name": "complete"},
	#	"Type 2": {"location": "thing2", "name": "all_done"}
	#}

	# parse expectationFilename to get list of subject numbers
	fd = open(expectationFilepath, "r")
	subjects = [subject(i, modeMapping) for i in fd.read().split("\n") if i != ""]
	fd.close()

	if len(subjects) <= 0:
		print("%s: ABORTING: No subjects were identified" % sys.argv[0])

	# calculate locations
	for mode in modeMapping:
		modeMapping[mode]["location"] = ensureTrailingSlash(baseLoc) + modeMapping[mode]["location"]

	# check each subject has files
	for sub in subjects:
		sub.checkFiles(modeMapping)

	# generate report
	text = subjects[0].genHeader() + "\n"
	text += "\n".join(str(sub) for sub in sorted(subjects, key=lambda x: x.idNum))

	print(text)

driver()
	
