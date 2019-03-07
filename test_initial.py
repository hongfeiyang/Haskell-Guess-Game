#python script to find best initial

import itertools
import subprocess

i = ["A1","A2","A3","B1","B2","B3","C1","C2","C3","D1","D2","D3","E1","E2","E3","F1","F2","F3","G1","G2","G3"]



for x in itertools.combinations(i,3):
	with open('Proj1.hs', 'r') as file:
		data = file.readlines()

	# Change the index to the line number (-1) and new initial guess string
	data[] = ""
	with open('Proj1.hs', 'w') as file:
		file.writelines(data)

	subprocess.run("ghc -O2 Proj1Test", check=True)

	out = subprocess.getoutput("python average.py")

	# Make a csv file called cases first!!!!!!!
	with open("cases.csv", "a") as myfile:
		myfile.write("{0}, {1}\n".format(x,out))





