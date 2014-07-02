#!/usr/bin/env python
# encoding: utf-8

## WordCount reducer for Hadoop streaming example in Python

import sys
import operator

words = {}

## collect emissions from mapper and count. didn't make sense to do an in-mapper combiner in the wordcount example.
for line in sys.stdin:
    try:
        word, lang, count = line.strip().split("\t", 3)
        word_lang = word + "," + lang
        
        if word_lang not in words:
            words[word_lang] = int(count)
        else:
            words[word_lang] += int(count)

    except ValueError, err:
        sys.stderr.write("Value ERROR: %(err)s\n%(data)s\n" % {"err": str(err), "data": line})

#emit results
for word_lang, count in words.items(): 
    print ",".join([word_lang, str(count)])
    
    
#open output file
#f = open(lang + '.wordcount.txt', 'w')

#emit results
#for w in sorted(words, key=word_list.get, reverse=True):
#    my_word_lang = w
#    my_count =  words[w]
#    f.write(",".join( [my_word_lang, str(my_count)] ) + "\n") #realize there is an extra newline at eof.. deal with it later
