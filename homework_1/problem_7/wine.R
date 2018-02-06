wine=scan()
.46400E+03
.67500E+03
.70300E+03
.88700E+03
.11390E+04
.10770E+04
.13180E+04
.12600E+04
.11200E+04
.96300E+03
.99600E+03
.96000E+03
.53000E+03
.88300E+03
.89400E+03
.10450E+04
.11990E+04
.12870E+04
.15650E+04
.15770E+04
.10760E+04
.91800E+03
.10080E+04
.10630E+04
.54400E+03
.63500E+03
.80400E+03
.98000E+03
.10180E+04
.10640E+04
.14040E+04
.12860E+04
.11040E+04
.99900E+03
.99600E+03
.10150E+04
.61500E+03
.72200E+03
.83200E+03
.97700E+03
.12700E+04
.14370E+04
.15200E+04
.17080E+04
.11510E+04
.93400E+03
.11590E+04
.12090E+04
.69900E+03
.83000E+03
.99600E+03
.11240E+04
.14580E+04
.12700E+04
.17530E+04
.22580E+04
.12080E+04
.12410E+04
.12650E+04
.18280E+04
.80900E+03
.99700E+03
.11640E+04
.12050E+04
.15380E+04
.15130E+04
.13780E+04
.20830E+04
.13570E+04
.15360E+04
.15260E+04
.13760E+04
.77900E+03
.10050E+04
.11930E+04
.15220E+04
.15390E+04
.15460E+04
.21160E+04
.23260E+04
.15960E+04
.13560E+04
.15530E+04
.16130E+04
.81400E+03
.11500E+04
.12250E+04
.16910E+04
.17590E+04
.17540E+04
.21000E+04
.20620E+04
.20120E+04
.18970E+04
.19640E+04
.21860E+04
.96600E+03
.15490E+04
.15380E+04
.16120E+04
.20780E+04
.21370E+04
.29070E+04
.22490E+04
.18830E+04
.17390E+04
.18280E+04
.18680E+04
.11380E+04
.14300E+04
.18090E+04
.17630E+04
.22000E+04
.20670E+04
.25030E+04
.21410E+04
.21030E+04
.19720E+04
.21810E+04
.23440E+04
.97000E+03
.11990E+04
.17180E+04
.16830E+04
.20250E+04
.20510E+04
.24390E+04
.23530E+04
.22300E+04
.18520E+04
.21470E+04
.22860E+04
.10070E+04
.16650E+04
.16420E+04
.15250E+04
.18380E+04
.18920E+04
.29200E+04
.25720E+04
.26170E+04
.20470E+04

y=log(wine)
times=1:142
tsq=times^2  #If we wanted quadratic trend we'd add this column to X matrix

Jan=rep(c(1,0,0,0,0,0,0,0,0,0,0,0),12)[1:142]
Feb=rep(c(0,1,0,0,0,0,0,0,0,0,0,0),12)[1:142]
Mar=rep(c(0,0,1,0,0,0,0,0,0,0,0,0),12)[1:142]
Apr=rep(c(0,0,0,1,0,0,0,0,0,0,0,0),12)[1:142]
May=rep(c(0,0,0,0,1,0,0,0,0,0,0,0),12)[1:142]
Jun=rep(c(0,0,0,0,0,1,0,0,0,0,0,0),12)[1:142]
Jul=rep(c(0,0,0,0,0,0,1,0,0,0,0,0),12)[1:142]
Aug=rep(c(0,0,0,0,0,0,0,1,0,0,0,0),12)[1:142]
Sep=rep(c(0,0,0,0,0,0,0,0,1,0,0,0),12)[1:142]
Oct=rep(c(0,0,0,0,0,0,0,0,0,1,0,0),12)[1:142]
Nov=rep(c(0,0,0,0,0,0,0,0,0,0,1,0),12)[1:142]
Dec=rep(c(0,0,0,0,0,0,0,0,0,0,0,1),12)[1:142]
sint=sin(2*pi*times/12)
cost=cos(2*pi*times/12)
X=cbind(times,Jan,Feb,Mar,Apr,May,Jun,Jul,Aug,Sep,Oct,Nov,Dec)  #sin and cos and constant for Jan; 
X_jan=cbind(times,sint,cost)  #sin and cos and constant for Jan; 

pdf('../report/figs/problem_7/monthly_model.pdf')
lsfit=lm(y~X) #you remove sin/cos and do all months
lsfit_jan=lm(y~X_jan) #you remove sin/cos and do all months
plot(times,wine) #plot on original scale
lines(times,wine) #add lines to existing plot
lines(times,exp(lsfit$fitted),col="red") #undo log for fitted model
#lines(times,exp(lsfit_jan$fitted),col="blue") #undo log for fitted model

pdf('../report/figs/problem_7/all_months_acf.pdf')
acf(lsfit$res,lag.max=15)  #significant correlation
Box.test(lsfit$res,lag=15,fitdf=0,type="Ljung")   

pdf('../report/figs/problem_7/jan_acf.pdf')
acf(lsfit_jan$res,lag.max=15)  #significant correlation
Box.test(lsfit_jan$res,lag=15,fitdf=0,type="Ljung")   
#pvalue indicates significant autocorrelation 

