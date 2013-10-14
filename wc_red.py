#!/usr/bin/env python
# encoding: utf-8

## WordCount reducer for Hadoop streaming example in Python

import sys
import operator

word_list = {}

## collect (key,val) pairs from sort phase

for line in sys.stdin:
    try:
        word, lang_id, count = line.strip().split("\t", 3)
        word_lang = word + "," + lang_id #gonna cheat by sticking with the same dictionary data structure, as I don't know python yet. 
        
        if word_lang not in word_list:
            word_list[word_lang] = int(count)
        else:
            word_list[word_lang] += int(count)

    except ValueError, err:
        sys.stderr.write("Value ERROR: %(err)s\n%(data)s\n" % {"err": str(err), "data": line})


#open output file
f = open(lang_id + '.wordcount.txt', 'w')

#emit results
for w in sorted(word_list, key=word_list.get, reverse=True):
    my_word_lang = w
    my_count =  word_list[w]
    f.write(",".join( [my_word_lang, str(my_count)] ) + "\n") #realize there is an extra newline at eof.. deal with it later
