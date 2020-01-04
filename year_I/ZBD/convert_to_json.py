import json_lines
import json


file1 = 'clashroyale_stable_battles_20180924.jsonl'
count = 0

with open(file1, 'rb') as f: # opening file in binary(rb) mode
    with open(file1[:-1], 'w+') as handle:
       for item in json_lines.reader(f):
           print(json.dumps(item, indent=4, sort_keys=True), file=handle)
           count += 1
           if count > 5000:
               break
