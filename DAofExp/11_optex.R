# optex.r, generating a BIBD, Table 11.20, p383

# install.packages("ibd")
library(ibd)
bibd(v = 7, r=3, b = 7, k = 3, lambda = 1)
