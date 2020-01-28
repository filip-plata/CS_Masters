import csv
import pims
import numpy as np
import os.path
from tqdm.auto import tqdm

# Parse metadata
symbols = []
for metadata_fie in ["uta_handshapes_dana.csv", "uta_handshapes_liz.csv", "uta_handshapes_tyler.csv", "uta_handshapes_lana.csv", "uta_handshapes_naomi.csv"]:
with open("old_data/annotations/" + metadata_fie) as csv_file:
csv_reader = csv.reader(csv_file, delimiter=',')
line_count = 0
for row in csv_reader:
if len(row[7].split('/')) >= 2:
obj = {}
obj["symbol"] = row[2]
obj["path"] = row[7].split('/')[-2] + "/" + row[7].split('/')[-1]
obj["beg"] = int(row[8])
obj["end"] = int(row[9])
symbols += [obj]

line_count += 1

# Get word statistics
counts = {}
stats = {}
for s in symbols:
if s["symbol"] in stats:
stats[s["symbol"]] += 1
else:
stats[s["symbol"]] = 1
counts[s["symbol"]] = 0

# Try to load clips for each symbol
filepaths, labels = [], []
for s in tqdm(symbols):
if stats[s["symbol"]] < 4:
continue

old_filename = "old_data/" + s["path"]
new_filedir = "new_data/" + s["symbol"]
new_filename = new_filedir + "/" + str(counts[s["symbol"]]) + ".npz"
counts[s["symbol"]] += 1
if not os.path.isdir(new_filedir):
os.mkdir(new_filedir)

if os.path.isfile(old_filename):
# Get frames
v = pims.Video(old_filename)
clip = np.zeros((0,480,640,3))
for frame_id in range(s["beg"], s["end"]):
clip = np.append(clip, [np.copy(v.get_frame(frame_id))], axis=0)
v.close()
del v

# Remove symbols and change to [0, 1]
clip = clip/256
for i in range(len(clip)): 
for j in range(100): 
for k in range(250): 
clip[i, j, k] = 0 

# Save parsed data
with open(new_filename, "wb+") as outfile:
np.savez_compressed(outfile, clip=clip)
filepaths = np.append(filepaths, [new_filename])
labels = np.append(labels, [s["symbol"]])
else:
print(old_filename)

Save metadata
with open("meta_data.npz", "wb+") as outfile:
np.savez_compressed(outfile, filepaths=filepaths, labels=labels)

# Load metadata
filepaths, labels = [], []
with open("meta_data.npz", "rb") as outfile:
obj = np.load(outfile, allow_pickle=True)
filepaths = obj["filepaths"]
labels = obj["labels"]



# Tmp snippets
clip = [] 
filename = filepaths[420]
with open(filename, "rb") as outfile: 
obj = np.load(outfile, allow_pickle=True) 
clip = obj["clip"]

import matplotlib.pyplot as plt
import matplotlib.image as mpimg

imgplot = plt.imshow(clip[10])
plt.show()

# mins = [0, 0, 0, 0, 0]
# for s in stats:
# if stats[s] > 5:
# mins[4] += 1
# else:
# mins[stats[s]-1] += 1


# max_len, ss = 0, ""
# for s in tqdm(symbols):
# if stats[s["symbol"]] < 4:
# continue
# if len(s["symbol"]) > max_len:
# ss = s["symbol"]
# max_len = len(s["symbol"])
