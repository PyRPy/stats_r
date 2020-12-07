GamesHowell = function(y, T, alpha=0.05){
# y is a data column, T the corresp column of trtmt levels.
# For the y-values corresponding to each level in T, compute:
r  = tapply(y, T, length)  # Column of reps r_i
ybar =  tapply(y, T, mean)  # Column of trtmt sample means ybar_i
s2 =  tapply(y, T, var)  # Column of trtmt sample variances s^2_i
v = length(r)  # v = number of treatments (length of column r)
combos = combn(v,2)  # 2 by v-choose-2, cols being combos (i,s)
i = combos[1,]  # Save row 1, i.e. the i's, as the column i
s = combos[2,]  # Save row 2, i.e. the s's, as the column s
# For each combo (i,s), compute est of tau_i - tau_s, stde, etc.
estimate = combn(v, 2, function(is) -diff(ybar[is]) )  # est's
stde = combn(v, 2, function(is) sqrt(sum(s2[is]/r[is])) )  # stde's
t = estimate/stde  # t-statistics
df = combn(v, 2, function(is)
     (sum(s2[is]/r[is]))^2/(sum((s2[is]/r[is])^2/(r[is]-1))) )  # df's
p = ptukey(abs(t)*sqrt(2), v, df, lower.tail=F)  # p-values
p = round(p, digits=5)  # Keep at most 5 decimal places
w = qtukey(0.05,v,df,lower.tail=F)/sqrt(2)  # Critical coefficients
msd = w*stde  # msd's
lcl = estimate - msd  # Lower confidence limits
ucl = estimate + msd  # Upper confidence limits
results = cbind(i, s, estimate, stde, df, t, p, msd, lcl, ucl)
results = signif(results, digits=5)  # Keep 5 significant digits
results =  results[rev(order(estimate)),]  # Sort by estimates
rownames(results) = seq(1:nrow(results))  # Name rows 1,2,...,nrows
header=paste("Games-Howell method of MCP for tau_i-tau_s",
             "with alpha =",alpha)
return(list(header,results))
}  # end function