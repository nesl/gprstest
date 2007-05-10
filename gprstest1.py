import appuifw
import time
import httplib
import urllib

project_id_str = u'153'
table_name_prefix = u'CENS Lab test '
files_to_test = ["1k", "10k", "50k", "100k"]

class Measurement:
    def __init__(self):
        self.time_date = None
        self.time_epoch = None
        self.time_download = None
        self.response_status = None
        self.response_reason = None


##
## PUBLIC
##
    def measure(self, fname):
        t1 = time.time()
        r1 = self.download(fname)
        t2 = time.time()
        self.update_state(t1, t2, r1)

##
## PUBLIC
##
    def upload(self, project_id, table_name):
        params = {}
        params['email'] = 'adparker@gmail.com'
        params['pw'] = 'ecopda'
        params['data_string'] = self.sensorbase_str()
        params['type'] = 'xml'
        params['project_id'] = project_id
        params['tableName'] = table_name
        params = urllib.urlencode(params)
        headers = {}
        headers['Content-type']='application/x-www-form-urlencoded'
        headers['Accept']='text/plain'
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
        conn.close()
        return r1
        
    """
    t1: start time;
    t2: end time;
    r1: response time;
    """
    def update_state(self,t1,t2,r1):
        # Convert t2 into the following format and store into self.time_date:
        # YYYY-MM-DD HH:MM:SS
        gmt = time.gmtime(t2)
        dtf_str = "%(year)-%(month)-%(day) %(hour):%(minute):%(second)"
        dtf_dict = {"year":gmt[0], "month":gmt[1], "day":gmt[2],
                    "hour":gmt[3], "minute":gmt[4], "second":gmt[5]}
        self.time_date = unicode(dtf_str % dtf_dict)
        self.response_status = r1.status
        self.response_reason = r1.reason
        self.time_download = t2 - t1
        self.time_epoch = t2

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
        output += self.field_str("time_date", self.time_date)
        output += self.field_str("time_download",self.time_download)
        output += self.field_str("time_epoch",self.time_epoch)
        output += self.field_str("response_status",self.response_status)
        output += self.field_str("response_reason",self.response_reason)
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
    for f in files_to_test:
        measurement = Measurement()
        print "Fetching file " + f
        measurement.measure(f)
        table_name = table_name_prefix + f
        print "Uploading to tableName: " + table_name
        responseText = measurement.upload(project_id_str, )
        print "SB response: " + responseText

do_tests()
