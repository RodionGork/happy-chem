import re
import random

with open('molecules.txt') as f:
    content = f.readlines()
moleculeIds = [int(re.sub(r'^([0-9\-]+).*', r'\1', x)) for x in content]

with open('catalysts.txt') as f:
    content = f.readlines()
catalystIds = [int(re.sub(r'^([0-9\-]+).*', r'\1', x)) for x in content]

with open('reactions.txt') as f:
    content = f.readlines()
reactionIds = [int(re.sub(r'^([0-9\-]+).*', r'\1', x)) for x in content]

#print("molecule ids: ", [str(x) for x in moleculeIds])
#print("catalyst ids: ", [str(x) for x in catalystIds])
#print("reaction ids: ", [str(x) for x in reactionIds])

for rid in reactionIds:
    nInputs = random.randint(3, 6) // 3
    inputs = random.sample(moleculeIds, nInputs)
    for mid in inputs:
        print("REAGENT_IN %s %s %s" % (rid, mid, random.randint(2, 10) / 10))
    nOutputs = random.randint(2, 4) // 2
    outputs = random.sample(moleculeIds, nOutputs)
    for mid in inputs:
        print("PRODUCT_FROM %s %s %s" % (rid, mid, random.randint(2, 10) / 10))
    temperature = random.randint(25, 70) * 10
    pressure = 101.3 * 2 ** (random.randint(1, 14) / 2)
    print("ACCELERATE %s %s %.1f" % (random.choice(catalystIds), temperature, pressure))

