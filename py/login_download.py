import urllib2
import cookielib
import sys

username = sys.argv[1]
password = sys.argv[2]
url = sys.argv[3]
download_file_split = url.split("/")
download_file_name = download_file_split[len(download_file_split)-1]

#print username
#print password
#print url


# store authenticated cookies
SERVER = 'https://transfer.pgatourhq.com/statsarchive'

authinfo = urllib2.HTTPPasswordMgrWithDefaultRealm()
authinfo.add_password(None, SERVER, username, password)
handler = urllib2.HTTPBasicAuthHandler(authinfo)

cj = cookielib.CookieJar()
opener = urllib2.build_opener(handler, urllib2.HTTPCookieProcessor(cj))
urllib2.install_opener(opener)


try:
	f = urllib2.urlopen(url)
	print f.getcode()
	data = f.read()

	with open(download_file_name, "wb") as code:
	    code.write(data)
	
	code.close()
	print "data written successfully"
except urllib2.HTTPError:
	print "unauthorized request"

