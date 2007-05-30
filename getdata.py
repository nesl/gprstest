#!/usr/bin/python

import SOAPpy
import os, sys, md5
import urllib
import commands
import sys
from SOAPpy import WSDL

#print "Command Input"
#for cmdInput in sys.argv:
#       print cmdInput

def getstuff(user_name, iteration=0, length=500):
    soapClient = SOAPpy.SOAPProxy("http://sensorbase.org/soap/dataGet.php")
    value = soapClient.getData('august@cs.ucla.edu', 'audio', \
                               "image,time_stamp,user_name", \
                               "p_48_Images", \
                               "1 AND user_name='%s' ORDER BY time_stamp ASC" % user_name, \
                               iteration * length, length, "csv")
    rows = value.split('\n')
    allrows = []
    for row in rows:
        if len(row) == 0:
            continue
        s = row.split(',')
        allrows.append(s)
    count = 1
    length = len(allrows)
    for row in allrows:
        #    commands.getstatusoutput("wget 'http://www.sensorbase.org/%s' -O %04i.jpg" % (row[0], count))
        print "[%03i/%05i/%05i] http://www.sensorbase.org/%s '%s/%s %s.jpg'" % (iteration, count, length, row[0], user_name, row[1], row[2])
        commands.getstatusoutput("wget 'http://www.sensorbase.org/%s' -O '%s/%s %s'.jpg" % (row[0], user_name, row[1], row[2]))
        count += 1
    return length



def main():
    iteration = 0
    user_name = sys.argv[1]
    print "user_name is: " + user_name
    while(True):
        if (getstuff(user_name, iteration) == 0):
            break
        iteration += 1

if __name__ == "__main__":
    main()
