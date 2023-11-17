#!/usr/bin/python

# (c) K.Rege 10.5.2019 created
# 1.10.2019 rege add wasm support
# 10.10.2020 rege add python 3.8 support

try:
        # python 2.7 until 3.7
        from BaseHTTPServer import BaseHTTPRequestHandler,HTTPServer
except ImportError:
        # for python 3.8        
        from http.server import BaseHTTPRequestHandler,HTTPServer
        
# or use python3 -m httpserver

from os import curdir, sep
try:
	from urlparse import urlparse
except ImportError:
	from urllib.parse import urlparse
import cgi, io, traceback, socket, errno, os

PORT_NUMBER = (80,8080,8081)
FILETYPES = {
	".html" :'text/html',
	".htm"  :'text/html',
	".css"  :'text/css',
	".xml"  :'text/xml',
	".jpg"  :'image/jpg',
	".jpeg" :'image/jpg',
	".png"  :'image/png',
	".ico"  :'image/x-icon',
	".gif"  :'image/gif',
	".pdf"  :'application/pdf',
	".js"   :'application/javascript',
	".json" :'application/json',
	".wasm" :'application/wasm',
	".zip"  :'application/zip'
	}


def checkPort(port):
	s = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
	try:
		s.bind(("127.0.0.1", port))
		s.close()
		return True
	except socket.error as e:
		if e.errno == errno.EADDRINUSE:
			return False
		# print("Port "+str(port)+" is already in use")
		else:
		# something else raised the socket.error exception
			print(e)
	s.close()
	return False


#This class will handles any incoming request from
#the browser 
self = None
form = None
class myHandler(BaseHTTPRequestHandler):
	#log handler
	def log_message(self, format, *args):
		print ("[%s] %s" % (self.log_date_time_string(),format%args))

	#Handler for the GET requests
	def do_GET(_self):
		global self,form
		self = _self
		if self.path=="/":
			self.path="/index.html"
		if os.path.isdir(self.path[1:]):
 			self.path=self.path+"/index.html"                       
		try:
			#Check the file extension required and
			#set the right mime type
			sendReply = False
			for ext in FILETYPES.keys() :
				if self.path.endswith(ext):
					mimetype=FILETYPES[ext]
					sendReply = True
			if self.path.split("?")[0].endswith(".py"):	
				try: 
					form = {}
					if "?" in self.path:
						args = cgi.parse_qs(self.path.split("?")[1], True)
						for key in args:
							form[key] = args[key][0]
					exec(io.open(curdir + sep + self.path.split("?")[0], "r", encoding="utf-8", errors="ignore").read(),  globals())
				except:
					self.send_error(500,'Internal Server Error On: %s' % self.path)
					traceback.print_exc()
				return
			if sendReply:
				#Open the static file requested and send it
				#if "text" in mimetype:
				#	f = io.open(curdir + sep + self.path, "r", encoding="utf-8", errors="ignore")
				#else:
				f = open(curdir + sep + self.path, "rb") 
				self.send_response(200)
				self.send_header('Content-type',mimetype)
				self.end_headers()
				self.wfile.write(f.read())
				f.close()
			else: self.send_error(406,'File Type Not Supported: %s' % self.path)
			return
		except IOError:
			self.send_error(404,'File Not Found: %s' % self.path)

	#Handler for the POST requests
	def do_POST(_self):
		global self,form
		self = _self
		if self.path.endswith(".py"):
			_form = cgi.FieldStorage(
				fp=self.rfile, 
				headers=self.headers,
				environ={'REQUEST_METHOD':'POST',
						 'CONTENT_TYPE':self.headers['Content-Type'],
			})
		form = {}	 
		for k in _form:
			form[k] = _form[k].value
		try: exec(io.open(curdir + sep + self.path, "r", encoding="utf-8", errors="ignore").read(),	 globals())
		except:
			self.send_error(500,'Internal Server Error On: %s' % self.path)
			traceback.print_exc()
			
			
try:
	#Create a web server and define the handler to manage the
	#incoming request
	for port in PORT_NUMBER:
		if checkPort(port):
			server = HTTPServer(('', port), myHandler)
			print ('Started httpserver on port '+ str(port))
			server.serve_forever()
	print ('ERROR: all ports in use ' + str(PORT_NUMBER))

except KeyboardInterrupt:
	print ('^C received, shutting down the web server')
	server.socket.close()
