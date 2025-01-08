#!/usr/bin/env python3

import sys

with open(sys.argv[1], "r") as f:
  lines = f.readlines()
  
  
starts = []
ranges = []
for i in range(len(lines)):
  if lines[i] == "bfa-c: [DEBUG] SMT:SEND: (push 1)\n":
    starts.append(i)
    continue
  if lines[i] == "bfa-c: [DEBUG] SMT:SEND: (pop 1)\n":
    ranges.append((starts.pop(), i))
    continue
  

## This is so not optimised...  
def in_list(ranges, i):
  for r in ranges:
    if i >= r[0] and i <= r[1]:
      return True
  return False

for i in range(len(lines)):
  if in_list(ranges, i):
    continue
  print(lines[i], end="")
