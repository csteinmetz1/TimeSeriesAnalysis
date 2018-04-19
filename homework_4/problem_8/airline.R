airline=scan()
112
118
132
129
121
135
148
148
136
119
104
118
115
126
141
135
125
149
170
170
158
133
114
140
145
150
178
163
172
178
199
199
184
162
146
166
171
180
193
181
183
218
230
242
209
191
172
194
196
196
236
235
229
243
264
272
237
211
180
201
204
188
235
227
234
264
302
293
259
229
203
229
242
233
267
269
270
315
364
347
312
274
237
278
284
277
317
313
318
374
413
405
355
306
271
306
315
301
356
348
355
422
465
467
404
347
305
336
340
318
362
348
363
435
491
505
404
359
310
337
360
342
406
396
420
472
548
559
463
407
362
405

y=log(airline[1:120])
pdf('../report/figs/problem_8/airline.pdf')
par(mfrow=c(1,1))
plot(1:length(airline[1:120]),airline[1:120])
lines(1:length(airline[1:120]),airline[1:120])
#Time series regression
M1=rep(c(1,0,0,0,0,0,0,0,0,0,0,0),10)
M2=rep(c(0,1,0,0,0,0,0,0,0,0,0,0),10)
M3=rep(c(0,0,1,0,0,0,0,0,0,0,0,0),10)
M4=rep(c(0,0,0,1,0,0,0,0,0,0,0,0),10)
M5=rep(c(0,0,0,0,1,0,0,0,0,0,0,0),10)
M6=rep(c(0,0,0,0,0,1,0,0,0,0,0,0),10)
M7=rep(c(0,0,0,0,0,0,1,0,0,0,0,0),10)
M8=rep(c(0,0,0,0,0,0,0,1,0,0,0,0),10)
M9=rep(c(0,0,0,0,0,0,0,0,1,0,0,0),10)
M10=rep(c(0,0,0,0,0,0,0,0,0,1,0,0),10)
M11=rep(c(0,0,0,0,0,0,0,0,0,0,1,0),10)
times=1:120
tsqr=times^2
X=cbind(times,M1,M2,M3,M4,M5,M6,M7,M8,M9,M10,M11)
lsfit=lm(y~X)
e=lsfit$resid
pdf('../report/figs/problem_8/acf_and_pacf.pdf')
par(mfrow=c(2,1))
acf(e,lag.max=25)
pacf(e,lag.max=25)
#tsregfit=arima(y,order=c(1,0,0),seasonal=list(period=12,order=c(1,0,0)),xreg=X)
#tsregfit=arima(y,order=c(1,0,0),xreg=X)
#Box.test(tsregfit$resid,lag=13,fitdf=1,type="Ljung")
#tsregfit
#BIC(tsregfit)

# in addition attempt to fit an AR(2)
tsregfit=arima(y,order=c(2,0,0),xreg=X)
tsregfit
#Box.test(tsregfit$resid,lag=13,fitdf=2,type="Ljung")
#tsregfit
#BIC(tsregfit)


newX=cbind(121:132,
c(1,0,0,0,0,0,0,0,0,0,0,0),
c(0,1,0,0,0,0,0,0,0,0,0,0),
c(0,0,1,0,0,0,0,0,0,0,0,0),
c(0,0,0,1,0,0,0,0,0,0,0,0),
c(0,0,0,0,1,0,0,0,0,0,0,0),
c(0,0,0,0,0,1,0,0,0,0,0,0),
c(0,0,0,0,0,0,1,0,0,0,0,0),
c(0,0,0,0,0,0,0,1,0,0,0,0),
c(0,0,0,0,0,0,0,0,1,0,0,0),
c(0,0,0,0,0,0,0,0,0,1,0,0),
c(0,0,0,0,0,0,0,0,0,0,1,0))
#predict next year 
logfore=predict(tsregfit,n.ahead=12,newxreg=newX)
up=exp(logfore$pred+1.96*logfore$se)
low=exp(logfore$pred-1.96*logfore$se)
pdf('../report/figs/problem_8/reg_pred.pdf')
par(mfrow=c(1,1))
plot(1:132,airline,xlim=c(1,145),ylim=c(90,700))
lines(1:132,airline)
lines(121:132,exp(logfore$pred),col=2)
lines(121:132,up,col=3)
lines(121:132,low,col=3)

actual = airline[121:132]
pred = exp(logfore$pred)

#sum(actual - pred)^2/12

#NOW TRY SARIMA
z=diff(y,lag=12)
par(mfrow=c(2,1))
acf(z,lag.max=25)
pacf(z,lag.max=25)
#COULD PROBABLY BEAT THE MODEL BELOW
#sarimafit=arima(y,order=c(3,0,0),seasonal=list(period=12,order=c(1,1,0)),xreg=times)
#sarimafit
#BIC(sarimafit)
#Box.test(sarimafit$resid,lag=13,fitdf=4,type="Ljung")
#OUR BETTER MODEL
sarimafit=arima(y,order=c(1,0,1),seasonal=list(period=12,order=c(1,1,0)),xreg=times)
sarimafit
#sarimafit
#BIC(sarimafit)
#Box.test(sarimafit$resid,lag=13,fitdf=4,type="Ljung")
logfore=predict(sarimafit,n.ahead=12,newxreg=121:132)
up=exp(logfore$pred+1.96*logfore$se)
low=exp(logfore$pred-1.96*logfore$se)
pdf('../report/figs/problem_8/sarima_pred.pdf')
par(mfrow=c(1,1))
plot(1:132,airline,xlim=c(1,145),ylim=c(90,700))
lines(1:132,airline)
lines(121:132,exp(logfore$pred),col=2)
lines(121:132,up,col=3)
lines(121:132,low,col=3)

actual = airline[121:132]
pred = exp(logfore$pred)

#sum(actual - pred)^2/12

#red1=sarimafit$coef[2]+(n+1)*sarimafit$coef[3]+sarimafit$coef[1]*(temp[n]-sarimafit$coef[2]-sarimafit$coef[3]*n)
#pred1




