#!/usr/bin/python

import sys
import time

while 1:
    line = sys.stdin.readline()
    if not line: break

    # Send back the received line
    print "Received", line,

    # Lets send back the current time just so we have more than one
    # line of output.
    print "Current time is", time.ctime()

    # Now send our terminating string to end the transaction.
    print "OK"

    # And finally, lets flush stdout because pipes are normally fully
    # buffered.
    sys.stdout.flush()

