
def ensureTrailingSlash(string):
	if string[-1] != "/":
		return string + "/"
	else:
		return string

