#import appuifw
import time
import httplib
import urllib
#import sysinfo
#import location

project_id_str = u'153'
g_table_name = u'AndrewLaptop'
files_to_test = [u"1k", u"10k", u"50k", u"100k"]

class Measurement:
    def __init__(self):
        self.time_date = None
        self.time_epoch = None
        self.time_download = None
        self.response_status = None
        self.response_reason = None
        self.file_size_bytes = None
        self.signal_dbm = None
        self.signal_bars = None
##
## PUBLIC
##
    def measure(self, fname):
        fsize = 0
        self.signal_bars = 0
        self.signal_dbm = 0
        c1 = time.clock()
        (r1, fsize) = self.download(fname)
        c2 = time.clock()
        cdiff = c2 - c1
        t2 = time.time()
        self.update_state(cdiff, t2, r1, fsize)

##
## PUBLIC
##
    def uploadToSensorBase(self, project_id, table_name):
        params = urllib.urlencode({'email'       : 'adparker@gmail.com',
                                   'pw'          : 'ecopda',
                                   'data_string' : self.sensorbase_str(),
                                   'type'        : 'xml',
                                   'project_id'  : project_id,
                                   'tableName'   : table_name })
        headers = {'Content-type' : 'application/x-www-form-urlencoded',
                   'Accept'       : 'text/plain'}
        conn = httplib.HTTPConnection("sensorbase.org")
        conn.request("POST", "/alpha/upload.php", params, headers)
        response = conn.getresponse()
        responseText = response.read()
        conn.close()
        return responseText

##
## HELPER FUNCTIONS
##
    def download(self, fname):
        conn = httplib.HTTPConnection("www.lecs.cs.ucla.edu")
        conn.request("GET","/~adparker/test/"+fname)
        r1 = conn.getresponse()
        print r1.status, r1.reason
        data1 = r1.read()
        bytes_read = len(data1)
        conn.close()
        return (r1,bytes_read)
        
    """
    t1: start time;
    t2: end time;
    r1: response time;
    """
    def update_state(self,cdiff,t2,r1,fsize):
        # Convert t2 into the following format and store into self.time_date:
        # YYYY-MM-DD HH:MM:SS
        gmt      = time.localtime(t2)
        dtf_str  = "%(year)d-%(month)d-%(day)d %(hour)d:%(minute)d:%(second)d"
        dtf_dict = {"year":gmt[0], "month":gmt[1], "day":gmt[2],
                    "hour":gmt[3], "minute":gmt[4], "second":gmt[5]}
        self.time_date       = unicode(dtf_str % dtf_dict)
        self.response_status = r1.status
        self.response_reason = r1.reason
        self.time_download   = cdiff
        print u"time_download " + str(self.time_download)
        self.time_epoch      = t2
        self.file_size_bytes = fsize

    """
    Returns a string that can be posted to sensorbase
    String looks like:
    <table>
       <row>
         <field />
       </row>
       <row> ... </row>
    </table>
    """
    def sensorbase_str(self):
        output = u"<table><row>\n"
        output += self.field_str("time_date",       self.time_date)
        output += self.field_str("time_download",   self.time_download)
        output += self.field_str("time_epoch",      self.time_epoch)
        output += self.field_str("response_status", self.response_status)
        output += self.field_str("response_reason", self.response_reason)
        output += self.field_str("file_size_bytes", self.file_size_bytes)
        #output += self.field_str("signal_bars", self.signal_bars)
        #output += self.field_str("signal_dbm", self.signal_dbm)
        
        output += '</row></table>\n'
        return output

    """
    name:string - the field name.
    item - the value of the field. This function will call str() on it.
    returns - a string of the form
            <field name="NAME">ITEM_STR</field>\n
    """
    def field_str(self, name, item):
        output = u'<field name="'+name+'">'
        output += str(item)
        output += '</field>\n'
        return output


def do_tests():
    first_f = files_to_test[0]
    print "Fetching a file, the timing of which will be ignored..."
    measurement = Measurement()
    print  u"File: " + first_f
    measurement.measure(first_f)
    print "Done."
    exceptions = 0
    iteration = 0    
    while(1):
        iteration += 1
        print u"*********\nITERATION: " + str(iteration)
        for f in files_to_test:
            try:
                time.sleep(5)
                print unicode(time.ctime())
                print u'exceptions: ' + str(exceptions)
                #print u'signal_bars():' + str(sysinfo.signal_bars())
                #print u'signal_dbm():' + str(sysinfo.signal_dbm())
                #print u'location:' + str(location.gsm_location())
                measurement = Measurement()
                print  u"File: " + f
                measurement.measure(f)
                responseText = measurement.uploadToSensorBase(project_id_str,
                                                              g_table_name)
                print g_table_name +":" + responseText
            except Exception, e:
                print e
                exceptions = exceptions + 1
                

do_tests()
