def classify(x):
	"""classify with random forest"""
	from sklearn.ensemble import RandomForestClassifier
	import numpy as np
	
	rf = RandomForestClassifier()
	mod = rf.fit(x[:, 0:4], x[:, 4])
	
	return(mod)
