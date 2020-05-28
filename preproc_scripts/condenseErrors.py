# this script reads the output of the qsub jobs, finds those that threw an error, and compiles them into a concise report that can be emailed
# things to get:
#	traceback --> make my own scripts put error messages on stderr!
#		read the .e file
#	subject number
#		either have every job write its sub number to its stdout or write the job numbers when writing expected subjects in preprocess.sh
#	script that was running
#		in the name of the output file
#	job id
#		in the name of the output file
#	time of error
