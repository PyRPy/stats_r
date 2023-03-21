
# contingency table -------------------------------------------------------

# from Ex 9.2-1
M <- as.table(rbind(c(8, 13, 16, 10, 3), c(4, 9, 14, 16, 7)))
dimnames(M) <- list(group = c("Group I", "Group II"),
                    Grade = c("A", "B", "C", "D", "F"))
M

(res_chisq <- chisq.test(M))
res_chisq$statistic    # 5.18
res_chisq$observed
res_chisq$expected

