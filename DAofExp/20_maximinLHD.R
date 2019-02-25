# maximinLHD.r, Section 20.7.1, p784

# install.packages("lhs") # Latin Hypercube Sample Package
library(lhs)
# Generate a good n x k LHD
Best <- maximinLHS(n=30, k=3, dup=5)
# "dup" is an integer tuning parameter that determines the number of 
# candidate points considered. Larger values should inprove results 
# but require more computational resources.

# Display the LHD
Best
