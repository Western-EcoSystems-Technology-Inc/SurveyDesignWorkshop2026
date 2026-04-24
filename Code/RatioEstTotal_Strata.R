RatioEstTotal_Strata<-function(Sample, Frame, varname) {

# Calculate ratio estimator of total by stratum, then combine.
# lahs 2026.03.11

H <- length(unique(Sample$stratum))
Sample$StrataCode <- as.numeric(as.factor(Sample$stratum))

ans<-data.frame(matrix(NA, H+1,2))
Sample<-Sample[!is.na(Sample[,varname]),]		# omit inaccessible sites
StratumKey<-unique(data.frame(stratum=Frame$stratum, StrataCode=Frame$StrataCode))
StratumKey<-StratumKey[order(StratumKey[,2]),]
Strata<-c(as.character(StratumKey[,1]),"ALL")
for(h in 1:H) {
	Frame.h<-Frame[Frame$StrataCode==h,]
	Sample.h<-Sample[Sample$StrataCode==h,]
      Y.h <- Sample.h[,varname]
	N.h<- length(unique(Frame.h$Reach))
	n.h<- length(unique(Sample.h$Reach))
	tx.h<- mean(Sample.h$length_mi)
	ty.h<- mean(Y.h)
      B.h <- ty.h/tx.h
      tx <- sum(Frame.h$length_mi)
      t_hat_yr.h <- B.h*tx
      s2e.h <- var(Y.h - B.h*Sample.h$length_mi)
      Var.t_hat_yr.h <- (N.h^2)*(1-(n.h/N.h))*s2e.h/n.h
      ans[h,]<-c(t_hat_yr.h, Var.t_hat_yr.h)
       }
# Incorporate Stratum level estimates
ans[H+1,]<-colSums(ans[1:H,])
strat = unique(Sample[,c("StrataCode","stratum")])
strat= strat[order(strat[,1]),]
strat = data.frame(rbind(strat,
   data.frame(StrataCode="All",stratum="All")))
ans<-data.frame(ans, sqrt(ans[,2]), ans[,1]-1.96*sqrt(ans[,2]), ans[,1]+1.96*sqrt(ans[,2]))

ans<-data.frame(cbind(strat, ans))
ans = ans[order(ans[,1]),]
names(ans)<-c("StratumCode","Stratum","EstTotal", "Var", "SE", "CILow", "CIHigh")
ans[,4:7]<-round(ans[,4:7],2)
return(ans)
}

