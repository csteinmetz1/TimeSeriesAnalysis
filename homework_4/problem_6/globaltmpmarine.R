temp=scan()
-.27 
-.24 
-.25 
-.31 
-.37 
-.32 
-.23 
-.33 
-.26 
-.15 
-.35 
-.3 
-.34 
-.37 
-.33 
-.28 
-.12 
-.09 
-.26 
-.16 
-.07 
-.17 
-.3 
-.41 
-.45 
-.32 
-.24 
-.42 
-.44 
-.37 
-.35 
-.38 
-.31 
-.34 
-.18 
-.07 
-.31 
-.47 
-.38 
-.27 
-.23 
-.22 
-.31 
-.27 
-.29 
-.21 
-.04 
-.13 
-.16 
-.3 
-.12 
-.04 
-.11 
-.18 
-.11 
-.13 
-.09 
 .03 
 .07 
-.04 
-.03 
 .06 
 .04 
 .02 
 .18 
 .09 
-.08 
-.07 
-.08 
-.07 
-.12 
-.01 
 .07 
 .11 
-.12 
-.14 
-.19 
 .09 
 .13 
 .07 
 .03 
 .09 
 .07 
 .09 
-.16 
-.14 
-.02 
-.03 
-.06 
 .1 
 .03 
-.14 
 .02 
 .11 
-.11 
-.11 
-.18 
 .08 
 .01 
 .11 
 .15 
 .17 
 .12 
 .27 
 .11 
 .08 
 .14 
 .29 

n=length(temp)
times=1:n

# Plot the raw data alone
pdf('../report/figs/problem_6/temp_data.pdf')
plot(times,temp)
lines(times,temp)

# Plot raw data and linear fit
pdf('../report/figs/problem_6/temp_ols.pdf')
plot(times,temp)
lines(times,temp)
ols=lm(temp~times)
lines(times,temp-ols$resid,col='blue')

# Plot ACF and PACF of residuals
olsres=lm(temp~times)$resid
pdf('../report/figs/problem_6/temp_acf.pdf')
acf(olsres)
pdf('../report/figs/problem_6/temp_pacf.pdf')
pacf(olsres)

# Fit linear trend with AR(1) errors
ar1fit=arima(temp,order=c(1,0,0), xreg=times)
ar1fit 
BIC(ar1fit)
Box.test(ar1fit$resid, lag=24, fitdf=1, type="Ljung")

pdf('../report/figs/problem_6/temp_ar1_lin.pdf')
plot(times,temp)
lines(times,temp)
lines(times,temp-ar1fit$resid,col="red")  #overly the predicted points

# Fit linear trend with AR(4) errors
ar4fit=arima(temp,order=c(4,0,0), xreg=times)
ar4fit 
BIC(ar4fit)
Box.test(ar4fit$resid, lag=24, fitdf=1, type="Ljung")

pdf('../report/figs/problem_6/temp_ar4_lin.pdf')
plot(times,temp)
lines(times,temp)
lines(times,temp-ar4fit$resid,col="red")  #overly the predicted points

# First order differencing with AR(1) errors
arimafit=arima(temp,order=c(1,1,0))
arimafit
BIC(arimafit)
Box.test(arimafit$resid, lag=24, fitdf=1, type="Ljung")

# First order differencing with AR(4) errors
arimafit2=arima(temp,order=c(4,1,0))
arimafit2
BIC(arimafit2)
Box.test(arimafit2$resid, lag=24, fitdf=1, type="Ljung")

# First order differencing with AR(4) errors
arimafit3=arima(temp,order=c(4,1,1))
arimafit3
BIC(arimafit3)
Box.test(arimafit3$resid, lag=24, fitdf=1, type="Ljung")

# Linear trend with AR(4) errors
arimafit4=arima(temp,order=c(4,0,4), xreg=times)
arimafit4
BIC(arimafit4)
Box.test(arimafit4$resid, lag=24, fitdf=1, type="Ljung")

# Linear trend with AR(24) errors
arimafit4=arima(temp,order=c(4,0,1), xreg=times)
arimafit4
BIC(arimafit4)
Box.test(arimafit4$resid, lag=24, fitdf=1, type="Ljung")

# Forecast three timesteps ahead
temp.fore=predict(ar1fit,n.ahead=3,newxreg=c(n+1,n+2,n+3))

pdf('../report/figs/problem_6/temp_arima_pred.pdf')

U=temp.fore$pred+1.96*temp.fore$se
L=temp.fore$pred-1.96*temp.fore$se
min1=min(temp,L)
max1=max(temp,U)
ts.plot(ts(temp),temp.fore$pred,col=1:2,ylim=c(min1,max1))
lines(U,col=3)
lines(L,col=3)


#pred1=arfit$coef[2]+(n+1)*arfit$coef[3]+arfit$coef[1]*(temp[n]-arfit$coef[2]-arfit$coef[3]*n)
#pred1
#sqrt(arfit$sigma)
#pred2=arfit$coef[2]+(n+2)*arfit$coef[3]+arfit$coef[1]^2*(temp[n]-arfit$coef[2]-arfit$coef[3]*n)
#pred2
#sqrt(arfit$sigma*(1+arfit$coef[1]^2))
