## Gabungan function, dpcca dan pcsuf versi 1

cbind.fill<-function(...){
    nm <- list(...) 
    nm<-lapply(nm, as.matrix)
    n <- max(sapply(nm, nrow)) 
    do.call(cbind, lapply(nm, function (x) 
    rbind(x, matrix(, n-nrow(x), ncol(x))))) 
}

DPCCA_CC=function(x,y,k){
    xx<- cumsum(x - mean(x))
    yy<- cumsum(y - mean(y))
  slide_win_xx = mat_sliding_window(xx,k)
  slide_win_yy = mat_sliding_window(yy,k)
  x_hat = t(apply(slide_win_xx,1,function(n) (lm(n~seq(1:length(n)))$fitted.values)))
  y_hat = t(apply(slide_win_yy,1,function(n) (lm(n~seq(1:length(n)))$fitted.values)))
  F2_dfa_x = c()
  F2_dfa_y = c()
  for(i in 1:nrow(x_hat)){
 ## Equation 4
    F2_dfa_x = c(F2_dfa_x,mean((xx[i:(i+k-1)]-x_hat[i,])^2))
  }
  
  mat_sliding_window = function(xx,k){
## Function to generate boxes given dataset(xx) and box size (k)
  slide_mat=c()
  for (i in 1:(length(xx)-k+1)){
    slide_mat = rbind(slide_mat,xx[i:(i+k-1)] )
  }
  return(slide_mat)
 }
 
  for(i in 1:nrow(y_hat)){
## Equation 4
    F2_dfa_y = c(F2_dfa_y,mean((yy[i:(i+k-1)]-y_hat[i,])^2))
  }
  ## Average detrend variance over all boxes to obtain fluctuation
  F2_dfa_x = mean(F2_dfa_x) ## Equation 3
  F2_dfa_y = mean(F2_dfa_y) ## Equation 3

  ## Get detrended covariance of two profile
  F2_dcca = c()
  for(i in 1:nrow(x_hat)){
  ## Equation 5
    F2_dcca = c(F2_dcca,mean((xx[i:(i+k-1)]-x_hat[i,]) * (yy[i:(i+k-1)]-y_hat[i,])))
  }

## Equation 6
  F2_dcca = mean(F2_dcca)
 

## Calculate correlation coefficient partial
  rho = F2_dcca / sqrt(F2_dfa_x * F2_dfa_y) ## Equation 1
   	rM <- matrix(c(1, rho, rho, 1), nrow=2, ncol=2, byrow = TRUE)
	inv_rM <- solve(rM)
	rho_partial = -inv_rM[1,2]/((sqrt(inv_rM[1,1])* sqrt(inv_rM[2,2])))
	return(rho_partial)
}

shift<-function(x,shift_by){
    stopifnot(is.numeric(shift_by))
    stopifnot(is.numeric(x))
 
    if (length(shift_by)>1)
        return(sapply(shift_by,shift, x=x))
 
    out<-NULL
    abs_shift_by=abs(shift_by)
    if (shift_by > 0 )
        out<-c(tail(x,-abs_shift_by),rep(NA,abs_shift_by))
    else if (shift_by < 0 )
        out<-c(rep(NA,abs_shift_by), head(x,-abs_shift_by))
    else
        out<-x
    out
}

matrix.index <- function(a, value) {
  idx <- which(data.frame(a)==value)
  col.num <- ceiling(idx/nrow(a))
  row.num <- idx - (col.num-1) * nrow(a)
  return(c(row.num, col.num))
}

mat_sliding_window = function(xx,k){
## Function to generate boxes given dataset(xx) and box size (k)
  slide_mat=c()
  for (i in 1:(length(xx)-k+1)){
    slide_mat = rbind(slide_mat,xx[i:(i+k-1)] )
  }
  return(slide_mat)
 }
 
 trace<-function(A)
{return(ifelse(is.square(A),sum (diag(A)),NA));
}

is.square<-function(A)
{return(is.matrix(A)&&(nrow(A)== ncol(A)));
}

## PERHITUNGAN DPPCA BERDASARKAN PASANGAN VAR DAN TIME-LAGS LOKASI JUANDA dan PERAK1 serta antar rainfall
## Ini yang sudah benar
juanda <- read.csv("juanda.csv", header=FALSE)
juanda_M<-as.matrix(juanda)
perak1 <- read.csv("perak1.csv", header=FALSE)
perak1_M<-as.matrix(perak1)
perak2 <- read.csv("perak2.csv", header=FALSE)
perak2_M<-as.matrix(perak2)
gab_3lok <- array( c( juanda_M , perak1_M, perak2_M) , dim = c( nrow(juanda_M) , ncol(juanda_M), 3 ) )

juanda <- data.frame(juanda[-1,])
perak1 <- data.frame(perak1[-1,])
perak2 <- data.frame(perak2[-1,])

head(juanda)
head(perak1)
head(perak2)
head(gab_3lok)

for (k in 6:7)
{
rho_M <- array(data = NA, dim=c(13, 12, 3, 3), dimnames = NULL)
for(lok1 in 1:3) {
Rainfall <- gab_3lok[,1,lok1]
for(col in 2:ncol(gab_3lok))
{
for(lok2 in 1:3) {
{
	Prediktor <- gab_3lok[ ,col,lok2]
	x <- Prediktor
	y <- Rainfall
	rho_M[1, col, lok1, lok2] <- DPCCA_CC(x,y,k)
	
	for(t in 1:12)
	{
	x <- Prediktor[-(((length(Prediktor)+1)-t):length(Prediktor))]
	y <- Rainfall[-(1:t)]
	rho_M[t+1,col, lok1, lok2] <- DPCCA_CC(x,y,k)
	}
	
## Perhitungan dpcca untuk Pasangan Rainfall
	Prediktor <- gab_3lok[,1,lok2]
	x <- Prediktor
	y <- Rainfall
	if(identical(x,y)==TRUE) {rho_M[1, 1, lok1, lok2] <-1}
	else {rho_M[1, 1, lok1, lok2] <- DPCCA_CC(x,y,k)}
	
	for(t in 1:12)
	{
	x <- Prediktor[-(((length(Prediktor)+1)-t):length(Prediktor))]
	y <- Rainfall[-(1:t)]
	rho_M[t+1,1,lok1, lok2] <- DPCCA_CC(x,y,k)
	}
rownames(rho_M) <- paste('lag ke', 1:13) 
colnames(rho_M) <- paste('var', 1:12) 
}
}
}
}
print(k)
print(rho_M)
rho_M.status <- ifelse(abs(rho_M[ , , , ])< 0.04811, 0, 1)
print(rho_M.status)
}


##PEMBENTUKAN FEATURE TIME-LAGS-BERDASAR DPCCA
r <- array(data=NA, dim=c(nrow=156 , ncol=156,3, 3),dimnames=NULL)
antar.lok<-array(data=NA, dim=c(nrow(gab_3lok) , ncol=156,3, 3),dimnames=NULL)
lokasi<-array(data=NA, dim=c(nrow(gab_3lok) , ncol=156,3),dimnames=NULL)
	for (lok2 in 1:3)
   {
	for (lok1 in 1:3)
        {
    y <- NULL
        for(col in 1:12)
        {
        count_t<-13
        x_lag<-matrix(data=NA,nrow(gab_3lok),count_t,dimnames=NULL)
        t <- 1
        repeat {
		if(t==1){x_lag[ ,t]<- as.matrix(gab_3lok[  ,col,lok1])
		if(rho_M.status[t,col,lok1,lok2]==0){x_lag[ ,t]<- NA}}
		else{
		lag <- (t-1)
        x_lag[ ,t]<- as.matrix(shift(gab_3lok[ ,col,lok1], -lag))
        if(rho_M.status[t,col,lok1,lok2]==0){x_lag[ ,t]<- NA}}
        colnames(x_lag) <- (0:(count_t-1))
        colnames(x_lag) <- paste(col,"~t-",colnames(x_lag),sep="")
        t= t+1
        if (t > 13){
        break
        }        
        }
        y[[col]] <- x_lag
        }
        z <- do.call(cbind, y)
		lokasi[ , , lok1] <- z
        }
		antar.lok[ , ,  , lok2] <- lokasi
	}


##Perhitungan MERIT-VALUE
num_merit <- matrix(data=NA,nrow=3, ncol=3, dimnames=NULL)
denum_merit<- matrix(data=NA,nrow=3, ncol=3, dimnames=NULL)
merit.value <- matrix(data=NA,nrow=3, ncol=3, dimnames=NULL)
rho_M.status.opt<-rho_M.

for (lok2 in 1:3)
{
	for (lok1 in 1:3)
	{
	jum_pred <- length(which(rho_M.status[ , ,lok1,lok2]==1))+length(which(rho_M.status[ , ,lok1,lok2] == 2))-1
	merit.SU[is.na(merit.SU)]=0
		for (kol2 in 2:ncol(merit.SU))
			{	merit.SU[1,1,lok1,lok2]<-abs(merit.SU[1,1,lok1,lok2])+abs(merit.SU[1,kol2,lok1,lok2])
				numerator <- as.numeric(merit.SU[1,1,lok1,lok2])
				num_merit[lok1,lok2] <- as.numeric(numerator)
				
				for (kol1 in kol2:nrow(merit.SU))
				{
				merit.SU[2,2,lok1,lok2]<-merit.SU[2,2,lok1,lok2]+merit.SU[kol1,kol2,lok1,lok2]
				}
				denumerator <- as.numeric(merit.SU[2, 2, lok1, lok2])
				denum_merit[lok1, lok2] <- as.numeric(denumerator)	
			}
				merit.value[lok1, lok2]<-num_merit[lok1,lok2]/(sqrt(jum_pred+jum_pred*(jum_pred-1)*(denumerator/jum_pred)))
				rownames(rho_M.status.opt) <- paste('lag ke', 1:13) 
				colnames(rho_M.status.opt) <- paste('var', 1:12) 
	}
	trace.merit.value <-trace(merit.value)
	trace.merit.value <-abs(trace.merit.value)
}

print(merit.value)
print(rho_M.status.opt)
print(trace.merit.value)
print(merit1.value)
print(alpha.opt)
print(beta.opt)
print(PC_SU.status.opt)
print(jum_pred)


## Generate data berdasar rho_M.status.opt
antar.lok.dpcca.opt<-array(data=NA, dim=c(nrow(gab_3lok) , ncol=156,3, 3),dimnames=NULL)
		for (lok2 in 1:3) 
		{
			for (lok1 in 1:3)
			{
			y <- NULL
				for(col in 1:12)
				{
				count_t<-13
				x_lag<-matrix(data=NA,nrow(gab_3lok),count_t,dimnames=NULL)
				t <- 1
				repeat {
				if(t==1){x_lag[ ,t]<-as.matrix(gab_3lok[ ,col,lok1])
				if(rho_M.status.opt[t,col,lok1,lok2]==0)
				{x_lag[ ,t]<- NA}}
				else{
				lag <-(t-1)
				x_lag[ ,t]<- as.matrix(shift(gab_3lok[ ,col,lok1], -lag))
				if(rho_M.status.opt[t,col,lok1,lok2]==0){x_lag[ ,t]<- NA}}
				colnames(x_lag) <- (0:(count_t-1))
				colnames(x_lag) <- paste(col,"~t-",colnames(x_lag),sep="")
				t= t+1
				if (t > 13){
				break
				}        
				}
				y[[col]] <- x_lag
				}
				z <- do.call(cbind, y)
				lokasi[ , ,lok1] <- z
				}
				antar.lok.dpcca.opt[ , , ,lok2] <-lokasi
		} 

## Mendefinisikan data untuk input RNN
antar_lok11 <- antar.lok.dpcca.opt[ , ,1,1]
antar_lok12 <- antar.lok.dpcca.opt[ , ,1,2]
antar_lok13 <- antar.lok.dpcca.opt[ , ,1,3]

antar_lok21 <- antar.lok.dpcca.opt[ , ,2,1]
antar_lok22 <- antar.lok.dpcca.opt[ , ,2,2]
antar_lok23 <- antar.lok.dpcca.opt[ , ,2,3]

antar_lok31 <- antar.lok.dpcca.opt[ , ,3,1]
antar_lok32 <- antar.lok.dpcca.opt[ , ,3,2]
antar_lok33 <- antar.lok.dpcca.opt[ , ,3,3]

antar_lok11 <- antar_lok11[, apply(antar_lok11, 2, function(x) !all(is.na(x)))] 
antar_lok12 <- antar_lok12[, apply(antar_lok12, 2, function(x) !all(is.na(x)))] 
antar_lok13 <- antar_lok13[, apply(antar_lok13, 2, function(x) !all(is.na(x)))] 

antar_lok21 <- antar_lok21[, apply(antar_lok21, 2, function(x) !all(is.na(x)))] 
antar_lok22 <- antar_lok22[, apply(antar_lok22, 2, function(x) !all(is.na(x)))]
antar_lok23 <- antar_lok23[, apply(antar_lok32, 2, function(x) !all(is.na(x)))]

antar_lok31 <- antar_lok31[, apply(antar_lok31, 2, function(x) !all(is.na(x)))] 
antar_lok32 <- antar_lok32[, apply(antar_lok32, 2, function(x) !all(is.na(x)))]
antar_lok33 <- antar_lok33[, apply(antar_lok33, 2, function(x) !all(is.na(x)))]

opt.3juanda.dpcca <- data.frame(cbind(antar_lok11, antar_lok21, antar_lok31))
opt.3perak1.dpcca <- data.frame(cbind(antar_lok12, antar_lok22, antar_lok32))
opt.3perak2.dpcca <- data.frame(cbind(antar_lok13, antar_lok23, antar_lok33))

write.csv(opt.3juanda.dpcca, file="opt.3juanda-linier.csv")
write.csv(opt.3perak1.dpcca, file="opt.3perak1-linier.csv")
write.csv(opt.3perak2.dpcca, file="opt.3perak2-linier.csv")


 