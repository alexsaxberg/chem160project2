alpham<-0.000006 #(person-1 day-1)
alphaf<-0.0000009
gammam<-0.05 #(day-1)
gammaf<-0.007
Sm<-14000
Sf<-9000
Im<-1000
If<-1000
Sm.hist<-c()
Sf.hist<-c()
Im.hist<-c()
If.hist<-c()
for (day in 1:2000) {
	Sm.hist[day]<-Sm
	Sf.hist[day]<-Sf
	Im.hist[day]<-Im
    	If.hist[day]<-If
	delta.Sm<-(gammam*Im*-alpham*Sm*If)
	delta.Im<-(alpham*Sm*If-gammam*Im)
    	delta.Sf<-(gammaf*If-alphaf*Sf*Im)
    	delta.If<-(alphaf*Sf*Im-gammaf*If)
	Sm<-Sm+delta.Sm
    	Sf<-Sf+delta.Sf
	Im<-Im+delta.Im
    	If<-If+delta.If
	# Ensure S,I,R > 0
	Sm<-max(Sm,0)
    	Sf<-max(Sf,0)  
	Im<-max(Im,0)
    	If<-max(If,0)
}
plot(Im.hist,type="l")
lines(If.hist,col=2)
