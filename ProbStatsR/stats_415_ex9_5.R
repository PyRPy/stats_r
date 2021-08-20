
# 9.4 - Comparing Two Proportions -----------------------------------------

n1 = 605
n2 = 195
y1 = 351
y2 = 41
p1hat = y1/n1
p2hat = y2/n2

# Null hypothesis, Ho : p1 = p2 
# Alternative hypothesis: Ha : p1 =/= p2
phat = (y1 + y2) / (n1 + n2)
Z = ((p1hat - p2hat) - 0)/sqrt(phat * (1 - phat) * (1/n1 + 1/n2))
# Z = 8.985901

alpha = 0.05
qnorm(c(alpha/2, 1 - alpha/2))
# -1.959964  1.959964

Z > abs(qnorm(c(alpha/2, 1 - alpha/2))) # reject Ho
