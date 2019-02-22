# RSMdesigns.r, generating RSM designs, Table 16.22, p607

library(rsm)

# Paint experiment
cube(basis = ~ A+B+C, generators = c(D ~ B*C, E ~ A*D, F ~ A*B), 
     n0=0, reps=4)

# Paint experiment: same design, different notation for factors
cube(3, generators = c(x4 ~ x2*x3, x5 ~ x1*x4, x6 ~ x1*x2), 
     n0=0, reps=4)

# Acid copper pattern plating experiment: rotatable CCD
# Create design in 2 parts, then join the parts
dsgn1 <- cube(2, n0=2, reps=1,
             coding=list(x1 ~ (A - 10.5)/1, x2 ~ (B - 36)/5))
dsgn2 <- star(dsgn1, n0=0, alpha="rotatable", reps=1)
dsgn12 <- djoin(dsgn1, dsgn2)
dsgn12 # Show design in randomized order
stdorder(dsgn12) # Show design in standard order
# Create the design all at once: defaults is factorial and axial blocks
dsgn <- ccd(2, n0=c(2,0), alpha="rotatable", oneblock=T, randomize=F,
                 coding=list(x1 ~ (A - 10.5)/1, x2 ~ (B - 36)/5))
varfcn(dsgn, ~ SO(x1, x2), contour=T) # Contour of scaled variances
# Add data to the coded data set containing the design
dsgn$s <- c(5.60, 4.84, 6.45, 5.19, 4.32, 4.25, 5.76, 4.42, 5.46, 5.81)
# Data analysis
model2 <- rsm(s ~ SO(x1, x2), data=dsgn)
summary(model2)
contour(model2, ~ x1 + x2, at=round(xs(model2),3), image=T, las=1)

# Flour experiment: orthogonal CCD
ccd(basis = ~ B+C+D, n0=2, alpha=1.2872, randomize=F)

# PAH experiment: rotatable CCD, with orthogonal blocking
ccd(4, alpha="rotatable", n0=2, blocks = ~ x1*x2*x3*x4, randomize=F)

# Flour experiment: noise array only
cube(basis = ~ G+J, generators = c(K ~ G*J), n0=0, randomize=F)

# Box-Behnken example design: 4 factors, 1 ctr pt per block
bbd(4, n0=3, block=F, randomize=F)
# Same design, except 3 blocks
bbd(4, n0=1, block=T, randomize=F)


