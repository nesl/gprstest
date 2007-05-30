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

def getstuff(table_name, iteration=0, length=500):
    soapClient = SOAPpy.SOAPProxy("http://sensorbase.org/soap/dataGet.php")
    value = soapClient.getData('adparker@gmail.com', 'ecopda', \
                               "file_size_bytes,response_reason,response_status,signal_bars,signal_dbm,time_data,time_download,time_epoch", \
                               "p_153_%s"%table_name, \
                               "ORDER BY time_epoch ASC", \
                               iteration * length, length,
                               "csv")
    print value
    # rows = value.split('\n')
#     allrows = []
#     for row in rows:
#         if len(row) == 0:
#             continue
#         s = row.split(',')
#         allrows.append(s)
#     count = 1
#     length = len(allrows)
#     for row in allrows:
#         #    commands.getstatusoutput("wget 'http://www.sensorbase.org/%s' -O %04i.jpg" % (row[0], count))
#         print "[%03i/%05i/%05i] http://www.sensorbase.org/%s '%s/%s %s.jpg'" % (iteration, count, length, row[0], user_name, row[1], row[2])
#         commands.getstatusoutput("wget 'http://www.sensorbase.org/%s' -O '%s/%s %s'.jpg" % (row[0], user_name, row[1], row[2]))
#         count += 1
#     return length



def main():
    iteration = 0
    table_name = sys.argv[1]
    print "table_name is: " + table_name
    while(True):
        if (getstuff(table_name, iteration) == 0):
            break
        iteration += 1

if __name__ == "__main__":
    main()
