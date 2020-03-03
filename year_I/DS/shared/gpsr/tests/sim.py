#!/usr/bin/env python2

import sys
import random
import time
from TOSSIM import *

MOTES = int(sys.argv[2])

t = Tossim([])

r = t.radio()
f = open(sys.argv[1], "r")

for line in f:
  s = line.split()
  if s:
    print (" ", s[0], " ", s[1], " ", s[2])
    r.add(int(s[0]), int(s[1]), float(s[2]))
    r.add(int(s[1]), int(s[0]), float(s[2]))

recv_f = open("recv_msgs.txt", "w")
t.addChannel("DEBUG", sys.stdout);
t.addChannel("SimpleReadingsStore", recv_f);

noise = open("meyer-heavy.txt", "r")
for line in noise:
    str1 = line.strip()
    if str1:
        val = int(str1)
        for i in range(MOTES):
            t.getNode(i).addNoiseTraceReading(val)

for i in range(MOTES):
    print("Creating noise model for ",i)
    t.getNode(i).createNoiseModel()


for i in range(MOTES):
    t.getNode(i).bootAtTime(random.randint(100000, 500000));



for i in range(MOTES * 1200):
    t.runNextEvent()
