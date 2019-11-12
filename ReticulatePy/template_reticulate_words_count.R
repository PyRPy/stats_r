
# # word count in python --------------------------------------------------

# load packages and python envir
library(reticulate)
use_condaenv("deeplearning")

# run python script - use dictionary to record number of words for each word
source_python('word_count.py')

# run the words count function
words <- "today is a very good day, and it is shining this morning and now"
word_count(words)

# convet to data.frame
data.frame(word_count(words))


# python ------------------------------------------------------------------

# def word_count(strings):
#   counts = dict()
#   words = strings.split()
# 
#   for word in words:
#     if word in counts:
#     counts[word] += 1
#   else:
#     counts[word] =1
#   return counts
