##
## Beispiel zum Testen
## ACHTUNG: quick&dirty, enthaelt noch keinerlei Fehlerbehandlung
##

import urllib.request
import html
import re


def doload(url):
    contents = urllib.request.urlopen(url).read()
    contents = contents.decode(encoding="utf-8")
    data = html.unescape(contents)
    withouttags = re.sub(r"<(.|\s)*?>", " ", data)
    return re.sub(r"\s+", " ", withouttags).split()
