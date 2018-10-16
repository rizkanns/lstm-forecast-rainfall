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
juanda <- read.csv("juanda_lok.csv", header=TRUE)
juanda_M<-as.matrix(juanda)
perak1 <- read.csv("perak1_lok.csv", header=TRUE)
perak1_M<-as.matrix(perak1)
perak2 <- read.csv("perak2_lok.csv", header=TRUE)
perak2_M<-as.matrix(perak2)
gab_3lok <- array( c( juanda_M , perak1_M, perak2_M) , dim = c( nrow(juanda_M) , ncol(juanda_M), 3 ) )
for (k in 6:7)
{
rho_M <- array(data = NA, dim=c(13, 8, 3, 3), dimnames = NULL)
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
colnames(rho_M) <- paste('var', 1:8) 
}
}
}
}
print(k)
print(rho_M)
rho_M.status <- ifelse(abs(rho_M[ , , , ])< 0.04811, 0, 1)
print(rho_M.status)
}

## Memilih fitur relevan linier dan non linier

PC_M <- array(data = NA, dim=c(13, 8, 3, 3), dimnames = NULL)

##menghitung korelasi (PC) untuk seleksi Relevan
for(lok1 in 1:3) {
Rainfall <- gab_3lok[,1,lok1]
for(col in 2:ncol(gab_3lok))
{
for(lok2 in 1:3) {
{
	Prediktor <- gab_3lok[ ,col,lok2]
	if (rho_M.status[1,col, lok1, lok2]==0) {PC_M[1,col,lok1,lok2] <- 0}
	else {
	x <- Prediktor
	y <- Rainfall
	PC_M[1,col,lok1,lok2] <- cor(x,y)
	}
	
	for(t in 1:12)
	{
	if (rho_M.status[t+1, col, lok1, lok2]==0) {PC_M[t+1, col, lok1, lok2] <-0}
	else
	{
	x <- Prediktor[-(((length(Prediktor)+1)-t):length(Prediktor))]
	y <- Rainfall[-(1:t)]
	PC_M[t+1,col, lok1, lok2] <- cor(x,y)
	}
	}
}
}

##Perhitungan PC untuk Pasangan Rainfall
for(lok2 in 1:3) {
	if(rho_M.status[1, 1, lok1, lok2]==0){PC_M[1, 8, lok1, lok2] <- 0}
	else{
	Prediktor <- gab_3lok[,1,lok2]
	x <- Prediktor
	y <- Rainfall
	{PC_M[1, 1, lok1, lok2] <- cor(x, y)}}
	
	for(t in 1:12)
	{
	x <- Prediktor[-(((length(Prediktor)+1)-t):length(Prediktor))]
	y <- Rainfall[-(1:t)]
	PC_M[t+1,1,lok1, lok2] <- cor(x, y)
	}
}
}
}
print(PC_M)
PC_Status <- ifelse(abs(PC_M[ , , , ])< 0.113, 0, 1)
print(PC_Status)


## SU dan Status_SU untuk memilih relevan non linier 
library(FSelector)

SU_M <- array(data = NA, dim=c(13, 8, 3, 3), dimnames = NULL)

##menghitung Symmetrical Unc. (SU) untuk seleksi Relevan
for(lok1 in 1:3) {
Rainfall <- gab_3lok[,1,lok1]
for(col in 2:ncol(gab_3lok))
{
for(lok2 in 1:3) {
{
	Prediktor <- gab_3lok[ ,col,lok2]
	if (rho_M.status[1,col, lok1, lok2]==0) {SU_M[1,col, lok1, lok2] <- 0}
	else {
	x <- Prediktor
	y <- Rainfall
	x <- data.frame(x)
	y <- data.frame(y)
	z <- data.frame(c(x, y))
	colnames(z)<-c("x", "y")
	SU_M[1,col, lok1, lok2] <- as.numeric(symmetrical.uncertainty(x~., z))
	}
	
	for(t in 1:12)
	{
	if (rho_M.status[t+1, col, lok1, lok2]==0) {SU_M[t+1, col, lok1, lok2] <-0}
	else
	{
	x <- Prediktor[-(((length(Prediktor)+1)-t):length(Prediktor))]
	y <- Rainfall[-(1:t)]
	x <- data.frame(x)
	y <- data.frame(y)
	z <- data.frame(c(x, y))
	colnames(z)<-c("x", "y")
	SU_M[t+1,col, lok1, lok2] <- as.numeric(symmetrical.uncertainty(x~., z))
	}
	}
}
}
##Perhitungan PC untuk Pasangan Rainfall
for(lok2 in 1:3) {
	if(rho_M.status[1, 1, lok1, lok2]==0){SU_M[1, 1, lok1, lok2] <- 0}
	else{
	Prediktor <- gab_3lok[,1,lok2]
	x <- Prediktor
	y <- Rainfall
	x <- data.frame(x)
	y <- data.frame(y)
	z <- data.frame(c(x, y))
	SU_M[1, 1, lok1, lok2] <- as.numeric(symmetrical.uncertainty(x~., z))
	
	for(t in 1:12)
	{
	x <- Prediktor[-(((length(Prediktor)+1)-t):length(Prediktor))]
	y <- Rainfall[-(1:t)]
	x <- data.frame(x)
	y <- data.frame(y)
	z <- data.frame(c(x, y))
	SU_M[t+1,1,lok1, lok2] <- as.numeric(symmetrical.uncertainty(x~., z))
	}
}
}
}
rownames(rho_M.status) <- paste('lag ke', 1:13) 
colnames(rho_M.status) <- paste('var', 1:8)
}
print(SU_M)
SU_Status <- ifelse(abs(SU_M[ , , , ])< 0.1, 0, 1)
print(SU_Status)


##PEMBENTUKAN FEATURE TIME-LAGS-PC dan SU
## Time-lags linier terlebih dulu
r <- array(data=NA, dim=c(nrow=104 , ncol=104,3, 3),dimnames=NULL)
antar.lok.PC<-array(data=NA, dim=c(nrow(gab_3lok) , ncol=104,3, 3),dimnames=NULL)
lokasi<-array(data=NA, dim=c(nrow(gab_3lok) , ncol=104,3),dimnames=NULL)
	for (lok2 in 1:3)
   {
	for (lok1 in 1:3)
        {
    y <- NULL
        for(col in 1:8)
        {
        count_t<-13
        x_lag<-matrix(data=NA,nrow(gab_3lok),count_t,dimnames=NULL)
        t <- 1
        repeat {
		if(t==1){x_lag[ ,t]<- as.matrix(gab_3lok[  ,col,lok1])
		if(PC_Status[t,col,lok1,lok2]==0){x_lag[ ,t]<- NA}}
		else{
		lag <- (t-1)
        x_lag[ ,t]<- as.matrix(shift(gab_3lok[ ,col,lok1], -lag))
        if(PC_Status[t,col,lok1,lok2]==0){x_lag[ ,t]<- NA}}
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
		antar.lok.PC[ , ,  , lok2] <- lokasi
	}

## create fitur time lags non linier
antar.lok.SU<-array(data=NA, dim=c(nrow(gab_3lok),ncol=104, 3, 3),dimnames=NULL)
lokasi<-array(data=NA, dim=c(nrow(gab_3lok) , ncol=104,3),dimnames=NULL)
matriks.SU <- array(data=NA, dim=c(nrow=104, ncol=104, 3, 3),dimnames=NULL)

	for (lok2 in 1:3)
   {
	for (lok1 in 1:3)
        {
    y <- NULL
        for (col in 1:8)
        {
        count_t<-13
        x_lag<-matrix(data=NA,nrow(gab_3lok),count_t,dimnames=NULL)
        t <- 1
        repeat {
		if(t==1){x_lag[ ,t]<- as.matrix(gab_3lok[  ,col,lok1])
		if(SU_Status[t,col,lok1,lok2]==0){x_lag[ ,t]<- NA}}
		else{
		lag <- (t-1)
        x_lag[ ,t]<- as.matrix(shift(gab_3lok[ ,col, lok1], -lag))
        if(SU_Status[t,col,lok1,lok2]==0){x_lag[ ,t]<- NA}}
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
		antar.lok.SU[ , ,  , lok2] <- lokasi
	}
	
	
##Hitung matriks korelasi PC var prediktor
	for (lok2 in 1:3)
	{
	for (lok1 in 1:3)
	{
		r[ , ,lok1, lok2] <- cor(antar.lok.PC[ , , lok1, lok2],use='pairwise.complete.obs')
		round(r, digits=3)
	}
	}
	
## Menghitung SU var prediktor
	for (lok2 in 1:3)
	{
	for (lok1 in 1:3)
	{
	for (kol1 in 1:104)
	{
	Var1 <-antar.lok.SU[ ,kol1,lok1,lok2]
	for (kol2 in 1:104)
	{
	x <- as.data.frame(antar.lok.SU[ ,kol2,lok1,lok2])
	y <- as.data.frame(Var1)
	z <-as.data.frame(c(x, y))
	colnames(z)<-c("x", "y")
	ifelse((all(is.na(z$x)))||(all(is.na(z$y))),next,
	matriks.SU[kol1,kol2,lok1,lok2]<-as.numeric(symmetrical.uncertainty(x~., z))) 
	}
	}
	}	
	}
	
## Ranking PC_M
maks_rankPC <- 80
PC2.rank<-array(data=NA, dim=c(nrow(PC_M), ncol(PC_M),3),dimnames=NULL) 
PC.rank<-array(data=NA, dim=c(nrow(PC_M), ncol(PC_M),3,3),dimnames=NULL)
for (lok2 in 1:3)
{
for (lok1 in 1:3)
{
d.rank <- matrix(rank(-(abs(PC_M[ , ,lok1,lok2]))), nrow = 13, ncol = 8)
d.rank[d.rank[ , ]>maks_rankPC]=0
PC2.rank[ , ,lok1] <- d.rank
}
PC.rank[ , , , lok2] <- PC2.rank
}

# Ranking SU_M
maks_rankSU <-40
SU2.rank<-array(data=NA, dim=c(nrow(SU_M), ncol(SU_M),3),dimnames=NULL) 
SU.rank<-array(data=NA, dim=c(nrow(SU_M), ncol(SU_M),3,3),dimnames=NULL)
for (lok2 in 1:3)
{
for (lok1 in 1:3)
{
d.rank <- matrix(rank(-(abs(SU_M[ , ,lok1,lok2]))), nrow = 13, ncol = 8)
d.rank[d.rank[ , ]>maks_rankSU]=0
SU2.rank[ , ,lok1] <- d.rank
}
SU.rank[ , , , lok2] <- SU2.rank
}

## Memberi indeks prediktor relevan berdasar PC
PC.indeks <- array(data=NA, dim=c(maks_rankPC,2,3,3),dimnames=NULL)
PC2.indeks <- array(data=NA, dim=c(maks_rankPC,2,3),dimnames=NULL)
for (lok2 in 1:3)
{
for (lok1 in 1:3)
{
ind <- NULL
## maks <- sum(PC_Status[ , ,lok1, lok2]==1)
for (rank in 1:maks_rankPC) ## harusnya maks s/d total PC_Status=1
{
indeks <- as.matrix(matrix.index(PC.rank[ , ,lok1, lok2], rank))
indeks <- t(indeks)
ind[[rank]] <- indeks
}
ind2 <- do.call(rbind, ind)
PC2.indeks[ , , lok1] <- ind2
}
PC.indeks[ , , , lok2] <- PC2.indeks
}

## Memberi indeks prediktor relevan berdasar SU
SU.indeks <- array(data=NA, dim=c(maks_rankSU,2,3,3),dimnames=NULL)
SU2.indeks <- array(data=NA, dim=c(maks_rankSU,2,3),dimnames=NULL)
for (lok2 in 1:3)
{
for (lok1 in 1:3)
{
ind <- NULL
## maks <- sum(SU_Status[ , ,lok1, lok2]==1)
for (rank in 1:maks_rankSU) ## harusnya maks s/d total PC_Status=1
{
indeks <- as.matrix(matrix.index(SU.rank[ , ,lok1, lok2], rank))
indeks <- t(indeks)
ind[[rank]] <- indeks
}
ind2 <- do.call(rbind, ind)
SU2.indeks[ , ,lok1] <- ind2
}
SU.indeks[ , , ,lok2] <- SU2.indeks
}

##--------------------------------------------------------
## Membandingkan matriks korelasi, SU dengan alpha-beta
trace1.merit.value <-0
alpha <- 0.2
beta <- 0.2
delta <- 0.05
jum.pred.opt <- 0
while (alpha < 0.5)
{
while (beta <0.5)
{
##remove PC.rank
for (lok2 in 1:3)
{
	for(lok1 in 1:3)
	{
	for (rank in 1:(maks_rankPC-1))
	{
	x1_bar <- PC.indeks[rank,1,lok1,lok2]
	x1_kol <- PC.indeks[rank,2,lok1,lok2]
	x1 <-x1_bar+((x1_kol-1)*13)
	for (rank2 in 2:maks_rankPC)
	{x2_bar <- PC.indeks[rank2, 1, lok1, lok2]
	x2_kol <- PC.indeks[rank2, 2, lok1, lok2]
	x2 <-x2_bar+((x2_kol-1)*13)
	ifelse(abs(r[x1,x2,lok1,lok2])> alpha,PC.rank[x2_bar,x2_kol,lok1,lok2]<-NA,PC.rank[x2_bar,x2_kol,lok1,lok2]<-1)
	}
	}
	}
}

	## remove SU.rank
for (lok2 in 1:3)
{
	for (lok1 in 1:3)
	{
	for (rank in 1:(maks_rankSU-1))
	{
	x1_bar <- SU.indeks[rank,1,lok1,lok2]
	x1_kol <- SU.indeks[rank,2,lok1,lok2]
	x1 <-x1_bar+((x1_kol-1)*13)
	for (rank2 in 2:maks_rankSU)
	{x2_bar <- SU.indeks[rank2, 1, lok1, lok2]
	x2_kol <- SU.indeks[rank2, 2, lok1, lok2]
	x2 <-x2_bar+((x2_kol-1)*13)
	ifelse(abs(matriks.SU[x1,x2,lok1,lok2])> beta,SU.rank[x2_bar,x2_kol,lok1,lok2]<-NA,SU.rank[x2_bar,x2_kol,lok1,lok2]<-1)
	}
	}
	}
}

## Mengubah kode PC_SU.status
PC.rank1 <-ifelse(PC.rank!=0,1,0)
SU.rank1 <-ifelse(SU.rank!=0,1,0)
PC.rank1[is.na(PC.rank1)]=0
SU.rank1[is.na(SU.rank1)]=0

PC_SU.status <-PC.rank1+SU.rank1

## buat array time lags kembali untuk antar.lok.PC_SU
antar.lok.PC_SU<-array(data=NA, dim=c(nrow(gab_3lok) , ncol=104,3, 3),dimnames=NULL)
		for (lok2 in 1:3) 
		{
			for (lok1 in 1:3)
			{
			y <- NULL
				for(col in 1:8)
				{
				count_t<-13
				x_lag<-matrix(data=NA,nrow(gab_3lok),count_t,dimnames=NULL)
				t <- 1
				repeat {
				if(t==1){x_lag[ ,t]<-as.matrix(gab_3lok[ ,col,lok1])
				if(PC_SU.status[t,col,lok1,lok2]==0)
				{x_lag[ ,t]<- NA}}
				else{
				lag <-(t-1)
				x_lag[ ,t]<- as.matrix(shift(gab_3lok[ ,col,lok1], -lag))
				if(PC_SU.status[t,col,lok1,lok2]==0){x_lag[ ,t]<- NA}}
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
				antar.lok.PC_SU[ , , ,lok2] <-lokasi
		}
			
merit.SU <- array(data=NA, dim=c(nrow=104 , ncol=104,3, 3),dimnames=NULL)
			for (lok2 in 1:3)
			{
			for (lok1 in 1:3)
			{
			for (kol1 in 1:104)
			{
			Var1 <-antar.lok.PC_SU[ ,kol1,lok1,lok2]
			for (kol2 in 1:104)
			{
			x <- as.data.frame(antar.lok.PC_SU[ ,kol2,lok1,lok2])
			y <- as.data.frame(Var1)
			z <-as.data.frame(c(x, y))
			colnames(z)<-c("x", "y")
			ifelse((all(is.na(z$x)))||(all(is.na(z$y))),next,
			merit.SU[kol1,kol2,lok1,lok2]<-as.numeric(symmetrical.uncertainty(x~., z))) 
			}
			}
			}	
			}

##Perhitungan MERIT-VALUE
num_merit <- matrix(data=NA,nrow=3, ncol=3, dimnames=NULL)
denum_merit<- matrix(data=NA,nrow=3, ncol=3, dimnames=NULL)
merit.value <- matrix(data=NA,nrow=3, ncol=3, dimnames=NULL)
PC_SU.status.opt<-PC_SU.status

for (lok2 in 1:3)
{
	for (lok1 in 1:3)
	{
	jum_pred <- length(which(PC_SU.status[ , ,lok1,lok2]==1))+length(which(PC_SU.status[ , ,lok1,lok2] == 2))-1
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
	}
	trace.merit.value <-trace(merit.value)
	trace.merit.value <-abs(trace.merit.value)
	if (trace1.merit.value < trace.merit.value)
	{
	trace1.merit.value<-trace.merit.value
	PC_SU.status.opt<-PC_SU.status
	jum.pred.opt <- jum_pred
	} else {
	trace1.merit.value
	PC_SU.status.opt
	jum.pred.opt
	}
}
beta<-beta+delta
}
alpha<-alpha+delta
}


print(merit.value)
print(PC_SU.status.opt)
print(jum.pred.opt)
print(trace1.merit.value)

## Generate data berdasar PC_SU.status.opt
antar.lok.PC_SU.opt<-array(data=NA, dim=c(nrow(gab_3lok) , ncol=104,3, 3),dimnames=NULL)
		for (lok2 in 1:3) 
		{
			for (lok1 in 1:3)
			{
			y <- NULL
				for(col in 1:8)
				{
				count_t<-13
				x_lag<-matrix(data=NA,nrow(gab_3lok),count_t,dimnames=NULL)
				t <- 1
				repeat {
				if(t==1){x_lag[ ,t]<-as.matrix(gab_3lok[ ,col,lok1])
				if(PC_SU.status.opt[t,col,lok1,lok2]==0)
				{x_lag[ ,t]<- NA}}
				else{
				lag <-(t-1)
				x_lag[ ,t]<- as.matrix(shift(gab_3lok[ ,col,lok1], -lag))
				if(PC_SU.status.opt[t,col,lok1,lok2]==0){x_lag[ ,t]<- NA}}
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
				antar.lok.PC_SU.opt[ , , ,lok2] <-lokasi
		} 

## Mendefinisikan data untuk input RNN
antar_lok11 <- antar.lok.PC_SU.opt[ , ,1,1]
antar_lok12 <- antar.lok.PC_SU.opt[ , ,1,2]
antar_lok13 <- antar.lok.PC_SU.opt[ , ,1,3]

antar_lok21 <- antar.lok.PC_SU.opt[ , ,2,1]
antar_lok22 <- antar.lok.PC_SU.opt[ , ,2,2]
antar_lok23 <- antar.lok.PC_SU.opt[ , ,2,3]

antar_lok31 <- antar.lok.PC_SU.opt[ , ,3,1]
antar_lok32 <- antar.lok.PC_SU.opt[ , ,3,2]
antar_lok33 <- antar.lok.PC_SU.opt[ , ,3,3]

antar_lok11 <- antar_lok11[, apply(antar_lok11, 2, function(x) !all(is.na(x)))] 
antar_lok12 <- antar_lok12[, apply(antar_lok12, 2, function(x) !all(is.na(x)))] 
antar_lok13 <- antar_lok13[, apply(antar_lok13, 2, function(x) !all(is.na(x)))] 

antar_lok21 <- antar_lok21[, apply(antar_lok21, 2, function(x) !all(is.na(x)))] 
antar_lok22 <- antar_lok22[, apply(antar_lok22, 2, function(x) !all(is.na(x)))]
antar_lok23 <- antar_lok23[, apply(antar_lok32, 2, function(x) !all(is.na(x)))]

antar_lok31 <- antar_lok31[, apply(antar_lok31, 2, function(x) !all(is.na(x)))] 
antar_lok32 <- antar_lok32[, apply(antar_lok32, 2, function(x) !all(is.na(x)))]
antar_lok33 <- antar_lok33[, apply(antar_lok33, 2, function(x) !all(is.na(x)))]

opt.3juanda <- data.frame(cbind(antar_lok11, antar_lok21, antar_lok31))
opt.3perak1 <- data.frame(cbind(antar_lok12, antar_lok22, antar_lok32))
opt.3perak2 <- data.frame(cbind(antar_lok13, antar_lok23, antar_lok33))

write.csv(opt.3juanda, file="opt.3juanda_lok.csv")
write.csv(opt.3perak1, file="opt.3perak1_lok.csv")
write.csv(opt.3perak2, file="opt.3perak2_lok.csv")


