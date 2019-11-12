def word_count(strings):
	counts = dict()
	words = strings.split()
	
	for word in words:
		if word in counts:
			counts[word] += 1
		else:
			counts[word] =1
	return counts
	
	