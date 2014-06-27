#!/usr/bin/env python
# encoding: utf-8

## WordCount mapper for Hadoop streaming example in Python

import re
import sys
import argparse

parser = argparse.ArgumentParser()
parser.add_argument("filename")

args = parser.parse_args()
filepath = "../lang/"
filename = args.filename
filex = filepath + filename

lang_id = filename.split(".")[0]

with open(filex) as f:
    content = f.readlines()

pat = re.compile("^[\-\_]+$")

for line in content:
    for word in re.sub('[^a-z0-9\-\_]', ' ', line.strip().lower()).split(" "):
        if not re.search(pat, word) and len(word) > 0:
            print "\t".join([word, lang_id, "1"])
            
            