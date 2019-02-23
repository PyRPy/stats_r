# icecream.r, ice cream experiment, Table 17.18, p665

# Create a user-defined function for sample size calculations
compute.v.given.r <- function(r=5,alpha=0.05,power=0.95){
    # Initialize variables
    ratio = (2*r+1)/(r+1);  
    result="Power <"
    v=1;
    while((result=="Power <")&(v<201)){
        v=v+1;
        df1=v-1; df2=v*(r-1);
        F1 = qf(1-alpha,df1,df2) # Compute F(df1,df2,alpha)
        F2 = qf(power,df2,df1)   # Compute F(df2,df1,1-power)
        product=F1*F2
        if (product<ratio) {result="Power ="} 
    } # End while loop, either finding v or power less than target 
    data.frame(r,v,result,power,alpha,F1,F2,product,ratio,df1,df2)
} # end function

compute.v.given.r(r=11,alpha=0.05,power=0.95) # Call the function 

compute.v.given.r(r=3,alpha=0.05,power=0.95)  # Call the function 