
load('Data/db.in.RData')
source("Data/survey.03.2015.R")

# Create working variables ------------------------------------------------

survey.list <- names(db.in[,5:65])
T  <-  61
x.lab = substr(db.getname(db.in, rank  = c(5:(T+4))), start = 3, stop = 6)
my_month <- as.numeric(substr(x.lab, 3,4))
x.lab = ifelse(as.numeric(substr(x.lab, 1,2))>81, paste0('19', x.lab), paste0('20', x.lab))
x.lab2 = as.numeric(substr(x.lab, 1,4)) - (as.numeric(substr(x.lab, 5,6)) + 12)/12
x.lab2 = as.numeric(substr(x.lab, 1,4)) + ( as.numeric(substr(x.lab, 5,6)) -1)/12

s2 <- as.vector(db.stat(db.in, fun="var", 5:65, flag.mono=T))
m <- as.vector(db.stat(db.in, fun="mean", 5:65, flag.mono=T))

x.cruise = substr(db.getname(db.in, rank  = c(5:(T+4))), start = 1, stop = 7)

sel.hot <- my_month >= 6 & my_month <= 10
sel.cold <- !sel.hot

# Matrices notations
Z <- as.matrix(db.extract(db.in,names=5:65))
# centred
m <- apply(Z,2,"mean")
Z <- Z - outer(rep(1,NROW(Z)),m)
# Standardisation
Zs <- Z %*% diag(1/sqrt(s2))

# Figure 1 ----------------------------------------------------------------
plot(x=c(-19,-14),y=c(16,21),type="n",xlab="Longitude",ylab="Latitude",las=1)
sel <- survey.03.2015[,3] > 0
points(x=survey.03.2015[!sel,1],y=survey.03.2015[!sel,2],pch="+")
symbols(x=survey.03.2015[,1],y=survey.03.2015[,2],circles=sqrt(survey.03.2015[,3]),inches=0.1,add=T)
map("worldHires", add=T)
text(-14.5,19.5,"MAURITANIA")
points(-16,18.1,pch=20)
text(-15.5,18.1,"Nouakchott")
legend("bottomleft",legend=c("0","1","44"),pch=c(3,1,1),pt.cex=c(0.5,0.2,2))
dev.print(device = png, file = "Res/survey.png",width=600,height=600)

map("worldHires", xlim=c(-30,53),ylim=c(-40,50),interior = F)
dev.print(device = png, file = "Res/world.png",width=450,height=450)

map("worldHires", xlim=c(-30,53),ylim=c(-40,50))
dev.print(device = png, file = "Res/world2.png",width=450,height=450)