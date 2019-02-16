# randomize.r, Section 3.9, pages 59-60

trtmt <- c(1,1,1,2,2,2)  # Create column trtmt = (1,1,1,2,2,2)
ranno <- runif(length(trtmt))  # Create column of 6 unif(0,1) RVs
design <- data.frame(trtmt,ranno)  # Create data.frame "design""
design  # Display the data.frame design

order(ranno)  # Display positions of ordered RVs, for illustration only
design <- design[order(ranno),]  # Sort rows by RVs, save
design  # Display design with rows sorted, for illustration only
design$EU = c(1:6)  # Add col EU = (1,2,3,4,5,6) to design
design  # Display the results of the randomization
