#!/usr/bin/python

#####
# Simple regression testing system.
#
# By: Andy Stone (aistone@gmail.com)
#
# This script will iterate through all .test files in the directory and
# subdirectories this script is invoked from.  Each test file is a python
# script that will store a value in the global variable 'passed'.  This
# variable should be set to false if the test has failed.
#
# The functions in this script can be invoked from within a .test file
# (for example the runProg() function is commonly called).
#
# NOTE: This script will only work with Python 2.6 (and presumably higher).
#####

import os
import time
import sys
from subprocess import *
import filecmp
import threading

passed = None
numPassed = 0
numFailed = 0

processes = {}
proccount = 0
servers = []

# Run 'make' to rebuild the project, if the project fails to build set
# passed to false.
def rebuild():
    global passed

    output = open("build.log", 'w')

    proc = Popen("make clean; make", shell=True, stdout=output, stderr=STDOUT)
    proc.wait()

    passed = (proc.returncode == 0)

# Run the program prog, check output against chkFile
def runProg(prog, chkfile=None):
    if chkfile is not None:
        output = open(chkfile + '.tmp', 'w')
    else:
        output = open('tmp.tmp', 'w')

    args = []
    args.append(prog)

    # start the process (the thread will hang here until the process
    # terminates).
    proc = Popen(" ".join(args), shell=True, stdout=output, stderr=STDOUT, stdin=PIPE)
    proc.wait()

    output.close()

    # if a check file was specified check against it
    if chkfile is not None:
        checkGood(chkfile)

# compare the output in a .tmp file against the contents of a .good file.  If
# there's a difference flag that the test failed.
def checkGood(filename):
    global passed

    if os.path.exists(filename + '.good'):
        if filecmp.cmp(filename + '.tmp', filename + '.good'):
            os.remove(filename + '.tmp')
        else:
            passed = False
    else:
        passed = False

def runTest(testFile, directory):
    global passed, numPassed, numFailed, server

    print '%25s \033[37m%25s\033[0m  ' % (directory, testname),

    # start out assuming it passed, set to false if there was an issue
    passed = True
    server = []

    # run the test
    file = open(testFile)
    exec file

    # wait for all threads to finish
    while threading.activeCount() != 1:
        time.sleep(0.1)

    # check if it passed
    if passed:
        numPassed += 1
        print '\033[32mpassed\033[0m'
    elif not passed:
        print '\033[31mFAILED! (in dir %s)\033[0m' % directory
        numFailed += 1

    # if the test passed remove the temporary output file
    if passed:
        try:
            os.remove('tmp.tmp')
        except OSError:
            # Will throw exception if the file does not exist (which isn't
            # something we really need to worry about)
            pass

    # remove all resident java instances
    for server in servers:
        os.system("ssh " + server + " killall java > /dev/null 2>&1")
        

# iterate through each .test file in the directory.  os.walk returns
# tuples: (dirpath, dirnames, filenames).
for dir in os.walk("."):
    for file in dir[2]:
        if file.endswith('test'):
            testname = file[:-(len('test')+1)]
            cwd = os.getcwd();
            os.chdir(dir[0])
            runTest(file, dir[0])
            os.chdir(cwd)

print 60*'='
print str(numPassed) + " test(s) passed"
print str(numFailed) + " test(s) failed"

