from collections import defaultdict
import csv
import numpy as np
import re

acd_data = csv.reader(open('acd_merged_reflex_aligned.tsv','r'),delimiter='\t')
acd_data = list(acd_data)

cons = ['B', 'C', 'D', 'G', 'H', 'K', 'L', 'M', 'N', 'P', 'R', 'S', 'T', 'V', 'W', 'X', 'Z', 'b', 'c', 'd', 'f', 'g', 'h', 'j', 'k', 'l', 'm', 'n', 'p', 'q', 'r', 's', 't', 'v', 'w', 'x', 'y', 'z', 'ð', 'ñ', 'č', 'đ', 'ń', 'ņ', 'ŋ', 'ŕ', 'š', 'ž', 'ƀ', 'ƚ', 'ǥ', 'ǵ', 'Ɂ', 'ɖ', 'ɗ', 'ɣ', 'ɫ', 'ɬ', 'ɭ', 'ɮ', 'ɸ', 'ʃ', 'ʄ', 'ʈ', 'ʔ', 'ʰ', 'ʷ', 'ʸ', 'β', 'γ', 'θ', 'ϕ', 'ᵇ', 'ᵏ', 'ᵐ', 'ᵬ', 'ḍ', 'ḏ', 'ḱ', 'ḷ', 'ḿ', 'ṁ', 'ṃ', 'ṇ', 'ṕ', 'ṛ', 'ṭ']

acd_data[0].append('IDCC_word')
for i in range(1,len(acd_data)):
    l = acd_data[i]
    counter = 0
    seglist = []
    w = l[10]
    w = re.sub('<|>|-','',w) #get rid of infix and affix boundaries
    w_ = list(w)
    for j in range(1,len(w_))[::-1]:
        if w_[j] == w_[j-1]:
            w_.pop(j)
    w__ = []
    for s in w_:
        if s in cons:
            w__.append(s)
    for j in range(1,len(w__)):
        if w__[j-1] == w__[j]:
            seglist.append(w__[j-1])
            counter += 1
            print(w__[j-1],end=' ')
    if counter > 0:
        counter = 1
        print(w)
    acd_data[i].append(str(counter))

acd_data[0].append('IDCC_morph')
for i in range(1,len(acd_data)):
    l = acd_data[i]
    counter = 0
    seglist = []
    w = l[11]
    w = re.sub('<.*>','',w) #get rid of infixation
    w_ = list(w)
    for j in range(1,len(w_))[::-1]:
        if w_[j] == w_[j-1]:
            w_.pop(j)
    w__ = []
    for s in w_:
        if s in cons:
            w__.append(s)
    for j in range(1,len(w__)):
        if w__[j-1] == w__[j]:
            seglist.append(w__[j-1])
            counter += 1
            print(w__[j-1],end=' ')
    if counter > 0:
        counter = 1
        print(w)
    acd_data[i].append(str(counter))
        
f = open('acd_merged_w_IDCC.tsv','w')
for l in acd_data:
    print('\t'.join(l),file=f)

f.close()