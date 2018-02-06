temp=scan()
-.4 
-.37 
-.43 
-.47 
-.72 
-.54 
-.47 
-.54 
-.39 
-.19 
-.4 
-.44 
-.44 
-.49 
-.38 
-.41 
-.27 
-.18 
-.38 
-.22 
-.03 
-.09 
-.28 
-.36 
-.49 
-.25 
-.17 
-.45 
-.32 
-.33 
-.32 
-.29 
-.32 
-.25 
-.05 
-.12 
-.26 
-.48 
-.37 
-.2 
-.15 
-.08 
-.14 
-.13 
-.12 
-.1 
 .13 
-.01 
 .06 
-.17 
-.01 
 .09 
 .05 
-.16 
 .05 
-.02 
 .04 
 .17 
 .19 
 .05 
 .15 
 .13 
 .09 
 .04 
 .11 
-.03 
 .03 
 .15 
 .04 
-.02 
-.13 
 .02 
 .07 
 .2 
-.03 
-.07 
-.19 
 .09 
 .11 
 .06 
 .01 
 .08 
 .02 
 .02 
-.27 
-.18 
-.09 
-.02 
-.13 
 .02 
 .03 
-.12 
-.08 
 .17 
-.09 
-.04 
-.24 
 .16 
 .09 
 .12 
 .27 
 .42 
 .02 
 .3 
 .09 
 .05 
 .17 
 .33 

times=1:length(temp)

#TEST FOR AUTOCORRELATION
lsres=lm(temp~times)$resid
pdf('../report/figs/problem_8/temp_acf.pdf')
acf(lsres)
Box.test(lsres,lag=10,fitdf=0)

# Plot raw data
pdf('../report/figs/problem_8/temp_ar1_quad.pdf')
plot(times,temp)
lines(times,temp) #connect the points

#Fit a quadratic trend with AR(1) errors
X=cbind(times,times^2)
fit=arima(temp,order=c(1,0,0),xreg=X)
lines(times,temp-fit$resid,col='blue')
fit # we find that the t^2 term is not significant

pdf('../report/figs/problem_8/temp_ar1_lin.pdf')
plot(times,temp)
lines(times,temp) #connect the points
#Fit linear trend with AR(1) errors
fit2=arima(temp,order=c(1,0,0),xreg=times)
fit2
lines(times,temp-fit2$resid,col="red")  #overly the predicted points

# Based on our fitted model, we estimate with 95% confidence that
# the average rate of change of the gobal temperature is 
# increasing by at least (0.0056-(1.645*.0007)) = 0.0044485 degrees per year
# or 0.44485 degrees per century. 
