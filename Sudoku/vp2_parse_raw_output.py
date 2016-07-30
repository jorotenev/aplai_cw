import re,sys,os
logDir = sys.argv[1] + '/'
file_prefix='output_'
def readFile(file):
	with open(logDir+file, 'r') as logFile:
	    data=logFile.readlines()
	    lines = [s.strip() for s in data if s.startswith("bucket(")]
	    lines = [getBucketAndCoords(l) for l in lines]
	    lines = [item for sublist in lines for item in sublist]
	    lines.sort()
	    return lines

def getBucketAndCoords(line):
	"bucket(2,[1-1,2-7,3-4,4-6,5-3,6-9,7-5,8-2,9-8])"
	p = re.compile('(\d-\d)')

	coords = re.findall(p, line)

	pp = re.compile('bucket\((\d)')
	number = line[7]
	result = [(c,number) for c in coords]
	return result

p = re.compile('.+(\d)\)')
def parse(line):
	a = re.match(p, line).groups()[0]
	return a

 

files = [f for f in os.listdir(logDir) if f.startswith(file_prefix)]

for file in files:
	content = readFile(file)
	
	
	res = [ch[1] for ch in content]
	res = ''.join(res)
	saveName = file.replace('output','answer')
	fileName = logDir+'answers/'+saveName
	os.makedirs(os.path.dirname(fileName), exist_ok=True)
	with open(fileName, 'w') as saveFile:
		print(res,file=saveFile)
