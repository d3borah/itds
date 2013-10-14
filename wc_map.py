#!/usr/bin/env python
# encoding: utf-8

## WordCount mapper for Hadoop streaming example in Python

import re
import sys
import argparse

parser = argparse.ArgumentParser()

parser.add_argument("lang")
args = parser.parse_args()
lang_id = args.lang

pat = re.compile("^[\-\_]+$")

for line in sys.stdin:
    for word in re.sub('[^a-z0-9\-\_]', ' ', line.strip().lower()).split(" "):
        if not re.search(pat, word) and len(word) > 0:
            print "\t".join([word, lang_id, "1"])
            