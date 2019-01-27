#!/usr/bin/env python
########################################################################
# 
# Copyright (c) 2018 Baidu.com, Inc. All Rights Reserved
# 
########################################################################
 
'''
File: add.py
Author: work(work@baidu.com)
Date: 2018/05/15 13:48:38
'''
import sys

f = open('final_data','w')
for line in open('test.csv'):
    line = line.strip()
    lines = line.split('\015')
    for l in lines:
        info = ''
        items = l.split(',')
        for each in items:
            info = info+'\t'+each
        if info.strip() != '':
            print >>f,info.strip()
f.close()
