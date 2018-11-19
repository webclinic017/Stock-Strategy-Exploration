# Optimizing spline fit for maximizing return on stock investments

library(optimization)
if (Combined_Results != Combined_Results){
load(file = "C:/Users/plfullen/Desktop/NASDAQ Historical.RDATA")}

DF <<- Combined_Results %>%
  group_by(Stock) %>%
  filter(Stock == "NFLX")

#Running the optimization
OptSplineParameter = optimize(PR_Cost_Function, c(0,1), maximum = TRUE,tol = 0.0001)
OptSplineParameter$maximum
OptSplineParameter$objective


#Plot check
n=seq(0,1, by=0.01)
a=n
cnt=1
for (val in n) {
a[cnt] =  PR_Cost_Function(val)
cnt=cnt+1
  }
plot(a)
