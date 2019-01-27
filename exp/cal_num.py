#!/usr/bin/env python
#! encoding=utf-8
########################################################################
# 
# Copyright (c) 2018 Baidu.com, Inc. All Rights Reserved
# 
########################################################################
 
'''
File: cal_num.py
Author: work(work@baidu.com)
Date: 2018/05/15 13:58:00
'''
import sys

ret={}
for line in open('final_data'):
    line = line.strip()
    items = line.split('\t')
    L = len(items)
    for i in range(2, L):
        key = items[i]
        ret.setdefault(key, 0)
        ret[key] += 1

func_list = sorted(ret.items(), key=lambda d:d[1], reverse=True)
func_f = open('feature_list.csv','w')
# 输出每个用途出现次数
#for k, v in ret.items():
#    func_item =  k+'\t'+str(v)
#    print >> func_f, func_item.strip()

for each in func_list:
    func_name, func_val = each
#    if func_name == '':
#	continue
    func_item = func_name + '\t' + str(func_val)
    print >> func_f, func_item.strip()
func_f.close()

result_f = open('result.csv','w')
for line in open('final_data'):
    line = line.strip()
    items = line.split('\t')
    L = len(items)
    try:
        base_info = '\t'.join([items[0],items[1]])
    except:
        print line
        continue
    rr = {}
    for i in range(2, L):
        key = items[i]
        v = ret[key]
        rr[key] = v
    ll = sorted(rr.items(), key=lambda d: d[1], reverse=True)
    ll_n = len(ll)
    base_info = base_info + '\t' + str(ll_n)
    for each in ll:
        key, num = each
        base_info = base_info+'\t'+key+'\t'+str(num)

#    print base_info.strip()
    print >> result_f, base_info.strip()

result_f.close()

