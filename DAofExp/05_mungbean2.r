# Transformation ----------------------------------------------------------

# mungbean2.r, mung bean experiment, Table 5.12 p128

# Table 5.11, page 126
mung.data = read.table("Data/mung.bean.txt", header=T) 

# Compute sample means and variances and their natural logs by trtmt
MeanLnth = by(mung.data$Length, mung.data$Trtmt, mean) # Col of means (by trtmt)
VarLnth = by(mung.data$Length, mung.data$Trtmt, var)  # Col of sample variances
LnMean = log(MeanLnth)  # Column of ln sample means
LnVar = log(VarLnth)  # Column of ln sample variances
Trtmt = c(1:6)  # Column of trtmt levels
stats = cbind(Trtmt, MeanLnth, VarLnth, LnMean, LnVar)  # Column bind 
stats  # Display the stats data

# Plot log sample variance versus log sample mean
plot(LnVar ~ LnMean, las=1)
