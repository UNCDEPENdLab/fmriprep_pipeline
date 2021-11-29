from setuptools import setup, find_packages

DIST_NAME = 'mri-fidelity-checks'
AUTHOR = 'Austin Marcus'
VERSION = '1.0'
DESCRIPTION = 'Used to automate the verification of mri scan data fidelity.'
URL = 'https://github.com/UNCDEPENdLab/fmriprep_pipeline'
CLASSIFIERS = [ 'Intended Audience :: Researchers',
                'Programming Language :: Python',
                'Operating System :: Linux']
setup(name=DIST_NAME,
      author=AUTHOR,
      version=VERSION,
      description=DESCRIPTION,
      url=URL,
      classifiers=CLASSIFIERS,
	packages=find_packages())
