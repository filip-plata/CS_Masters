import sys
import random
import time
from TOSSIM import *
import Radio

t = Tossim([])

r = t.radio()
f = open("topo.txt", "r")

for line in f:
  s = line.split()
  if s:
    print " ", s[0], " ", s[1], " ", s[2];
    r.add(int(s[0]), int(s[1]), float(s[2]))

#t.addChannel("Boot", sys.stdout);
#t.addChannel("DEBUG", sys.stdout);
send_f = open("send_msgs.txt", "w")
t.addChannel("AppS", send_f);
t.addChannel("AppR", sys.stdout);

noise = open("meyer-heavy.txt", "r")
for line in noise:
    str1 = line.strip()
    if str1:
        val = int(str1)
        for i in range(1, 3):
            t.getNode(i).addNoiseTraceReading(val)
        t.getNode(0).addNoiseTraceReading(-100)

for i in range(3):
    print "Creating noise model for ",i;
    t.getNode(i).createNoiseModel()


t.getNode(0).bootAtTime(100001);
t.getNode(1).bootAtTime(800008);
t.getNode(2).bootAtTime(1800009);



for i in range(10000):
    t.runNextEvent()
