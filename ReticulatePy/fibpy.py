def fibPy(n):
  if n < 2:
  	return n
  return fibPy(n-2) + fibPy(n-1)