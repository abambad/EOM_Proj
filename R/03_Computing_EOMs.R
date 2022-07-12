# EOM ---------------------------------------------------------------------
h0 <- db.in@dx[1]
dh <- h0/2

# Computing the different MAF elements
MAF.operators <- maf.calc(db.in, h0 = h0, dh = dh, dirvect=0, tolang=90, verbose = F)

Ps <- MAF.operators@pcaz2f
Qs <- MAF.operators@pcaf2z

# Re-ordering EOM by % OF explained CORRELATION
cor.expl <- apply(Qs^2,1,"sum")/tr(var(Zs))
reorder <- order(cor.expl,decreasing = T)

# re-ordering elements of the MAF object
EOM.operators <- MAF.operators
EOM.operators@eigen <- MAF.operators@eigen[reorder]
EOM.operators@pcaf2z <- MAF.operators@pcaf2z[reorder,] # new.Qs 
EOM.operators@pcaz2f <- solve(EOM.operators@pcaf2z) #new.Ps

# Computing MAFs and EOMs
MAFs <- Zs %*% Ps 
EOMs <- MAFs[,reorder]

cumul.cor.expl <- NULL
for(i in 1:T){
  hat.Zsr <- as.matrix(EOMs[,1:i]) %*% EOM.operators@pcaf2z[1:i,]
  cumul.cor.expl[i] <- tr(var(hat.Zsr))/tr(var(Zs))
}

# Building EOM in the RGeostats format
db.EOM <- pca.z2f(db.in, EOM.operators,radix="EOM")
db.EOM <- db.locate(db.EOM, 67:127,"NA")

# Cross variograms of the selected EOM
n <- 10 #61
db.EOM <- db.locate(db.EOM, 67:(66+n),"z")

nlag <- 10
h.intervals <- seq(from=dh,by=h0,length=nlag+1)
h.centres <- (h.intervals[-1]+h.intervals[-(nlag+1)])/2

vario <- vario.calc(db.EOM, 
                    breaks=h.intervals,
                    dirvect=0,tolang=90,
                    calcul="vg")
EOM.lab <- NULL
for(i in 1:n) EOM.lab[i] <- paste0("EOM.",i)
h.lab <- as.character(h.centres)
cross.vario <- array(NA,dim=c(n,n,nlag),
                     dimnames=list(EOM.lab,EOM.lab,h.lab)) 
for(i in 1:n){
  for(j in 1:n){
    if(j != i) cross.vario[i,j,] <- vario.extract(vario,ivar=i,jvar=j,idir=1,flag.dist=1)$vario
  }
}

cross.vario.n.n <- as.data.frame.table(cross.vario[1:n,1:n,])

# EOM2  --------------
# Using a large reference interval distance for building EOM

h0.2 <- 0.5 
dh.2 <- 0.5

MAF.2.operators <- maf.calc(db.in, h0 = h0.2, dh = dh.2, dirvect=0, tolang=90, verbose = F)

Ps.2 <- MAF.2.operators@pcaz2f
Qs.2 <- MAF.2.operators@pcaf2z

# Re-ordering EOM by % OF explained CORRELATION
cor.expl.2 <- apply(Qs.2^2,1,"sum")/tr(var(Zs))
reorder.2 <- order(cor.expl.2,decreasing = T)

# re-ordering elements of the EOM.operators objects
EOM.2.operators <- MAF.2.operators
EOM.2.operators@eigen <- MAF.2.operators@eigen[reorder.2]
EOM.2.operators@pcaf2z <- MAF.2.operators@pcaf2z[reorder.2,]  
EOM.2.operators@pcaz2f <- solve(EOM.2.operators@pcaf2z)

MAFs.2 <- Zs %*% Ps.2
EOMs.2 <- MAFs.2[,reorder.2]
cumul.cor.expl.2 <- NULL
for(i in 1:T){
  hat.Zsr <- as.matrix(EOMs.2[,1:i]) %*% EOM.2.operators@pcaf2z[1:i,]
  cumul.cor.expl.2[i] <- tr(var(hat.Zsr))/tr(var(Zs))
}

# Building EOM in the RGeostats format
db.EOM.2 <- pca.z2f(db.in, EOM.2.operators,radix="EOM")
db.EOM.2 <- db.locate(db.EOM.2, (67+n):127,"NA")

# Cross variograms of the selected EOM2
# For the crossvariogram the lags are the same as before
vario.2 <- vario.calc(db.EOM.2, 
                      breaks=h.intervals,
                      dirvect=0,tolang=90,
                      calcul="vg")
cross.vario.2 <- array(NA,dim=c(n,n,nlag),
                       dimnames=list(EOM.lab,EOM.lab,h.lab)) 
for(i in 1:n){
  for(j in 1:n){
    if(j !=i ) cross.vario.2[i,j,] <- vario.extract(vario.2,ivar=i,jvar=j,idir=1,flag.dist=1)$vario
  }
}

cross.vario.2.n.n <- as.data.frame.table(cross.vario.2[1:n,1:n,])
