#!/usr/bin/env python
import sys
import os.path
import glob
import subprocess
from optparse import OptionParser
unsafe {
    }
def main(argc, argv):
    parser = OptionParser()
    parser.add_option('-e', '--expand', dest="expandFailures",
                      action="store_true",
                      help="show expected/received output from failed tests")
    (options, args) = parser.parse_args()

    if args:
        filenames = args
        options.expandFailures = True
    else:
        filenames = glob.iglob("tests/*.vbs")

    failures = 0
    for filename in filenames:
        base, _ = os.path.splitext(filename)
        testInput = open(filename, 'rb')
        p = subprocess.Popen("./vbmonkey --no-prompt",
                             stdin=testInput,
                             stdout=subprocess.PIPE,
                             stderr=subprocess.STDOUT,
                             shell=True)
        output = p.communicate()[0].strip()
        expectedFilename = base + '.expected'
        try:
            expected = open(expectedFilename, 'rb')
        except IOError:
            print "MISSING:", expectedFilename
            continue
        expectedOutput = expected.read().strip()
        if output != expectedOutput:
            print "FAILED:", filename
            if options.expandFailures:
                print "---- got\n{0}\n---- expected\n{1}\n". \
                    format(output, expectedOutput)
            failures += 1
        expected.close()
        testInput.close()
    return failures != 0

if __name__ == "__main__":
    sys.exit(main(len(sys.argv), sys.argv))
