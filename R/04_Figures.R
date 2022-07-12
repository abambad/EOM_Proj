
# Figure 3a ---------------------------------------------------------------
boxplot(Freq~Var3,data=cross.vario.n.n,
        xlab="Distance",ylab="Cross-variogram",las=1,ylim=c(-0.4,0.3))
abline(h=0,lwd=2)
dev.print(device = png, file = "Res/crossvario.1.png",width=450,height=450)

# Figure 3b ---------------------------------------------------------------
boxplot(Freq~Var3,data=cross.vario.2.n.n,
        xlab="Distance",ylab="Cross-variogram",las=1,ylim=c(-0.4,0.3),na.rm=T)
abline(h=0,lwd=2)
dev.print(device = png, file = "Res/crossvario.2.png",width=450,height=450)


# Figure 2 ----------------------------------------------------------------
plot(c(0,0.95),c(-0.15,1.4),type="n",yaxs="i",xaxs="i",las=1,
     xlab="Distance",
     ylab="Variogram at distance 0.1°= eigen values/2",)
points(0.1+0.3*cumul.cor.expl,EOM.operators@eigen/2,type='l',pch=".")
points(0.1+0.3*cumul.cor.expl[1:n],EOM.operators@eigen[1:n]/2,type='b',pch=20)
points(0.5+0.3*cumul.cor.expl.2,EOM.2.operators@eigen/2,type='l',col=2,pch=".")
points(0.5+0.3*cumul.cor.expl.2[1:n],EOM.2.operators@eigen[1:n]/2,type='b',col=2,pch=20)
arrows(0.1,0,0.1,1.35,length=0.1,col=1)
arrows(0.5,0,0.5,1.35,length=0.1,col=2)
#abline(v=c(0.1,0.5),col=c(1,2),lty=2)
abline(h=1,lty=2)
arrows(0.1,0,0.45,0,lty=1,col=1,length=0.1)
arrows(0.5,0,0.85,0,lty=1,col=2,length=0.1)
text(0.55/2,-0.075,"% of explained correlation")
text(1.35/2,-0.075,"% of explained correlation",col=2)
segments(0.1+0.3*c(0.5,1),c(0,0),0.1+0.3*c(0.5,1),c(1.35,1.35),lty=3)
segments(0.5+0.3*c(0.5,1),c(0,0),0.5+0.3*c(0.5,1),c(1.35,1.35),lty=3,col=2)
text(0.1+0.3*c(0.5,1),c(-0.025,-0.025),c("50%","100%"))
text(0.5+0.3*c(0.5,1),c(-0.025,-0.025),c("50%","100%"),col=2)
points(rep(0.1,n),EOM.operators@eigen[1:n]/2,pch=20)
points(rep(0.105,T-n),EOM.operators@eigen[-(1:n)]/2,pch="-")
points(rep(0.505,T-n),EOM.2.operators@eigen[-(1:n)]/2,pch="-",col=2)
points(rep(0.5,n),EOM.2.operators@eigen[1:n]/2,pch=20,col=2)
#points(0.1,-0.1,pch=20,cex=2)
#points(0.5,-0.1,pch=20,cex=2,col=2)
polygon(x=c(0.05,0.15,0.15,0.05),y=c(-0.13,-0.13,-0.11,-0.11),col=rgb(0,0,0,0.5))
polygon(x=c(0.01,0.99,0.99,0.01),y=c(-0.13,-0.13,-0.15,-0.15),col=rgb(1,0,0,0.5))
arrows(0.1,-0.11,0.1,-0.05,length=0.1,col=1,lwd=2)
arrows(0.5,-0.13,0.5,-0.05,length=0.1,col=2,lwd=2)
dev.print(device = png, file = "Res/corexpl_x_eigenvalue.png",width=450,height=450)

# Figure 5 --------------------------------------------------------------
Qs <- EOM.operators@pcaf2z[1:n,]
d10.w <- t(Qs[1:n,]) %>% dist() %>% hclust(method="ward.D2") %>% as.dendrogram()
dend <- d10.w
dend %>%
  #set("labels_col", value = c("red", "blue"), k=2) %>%
  set("labels_col", value = 0) %>%
  set("branches_k_color", value = c("red", "blue"), k = 2) %>%
  set("leaves_pch", 19)  %>% 
  set("nodes_cex", 0.7) %>% 
  plot(axes=T,las=1)

season.colors.4 <- c(rep(rgb(0,0,1),5),rgb(0,0,1,0.5),rgb(1,0,0,0.5),rep(rgb(1,0,0),3),rgb(1,0,0,0.5),rgb(0,0,1,0.5))
colors_month <- season.colors.4[my_month]
colored_bars(colors = colors_month, dend = dend, rowLabels = c("month"))
dev.print(device = png, file = "Res/dendrogram.png",width=650,height=450)



# Figure 6 ----------------------------------------------------------------
# create a data frame with the first n MAFs
cluster <- cutree(dend,k=2) # 1 == "Hot" ; 2 == "Cold"
cluster.HC <- as.vector(ifelse(cluster==1,"Hot","Cold"))
sel.Hot.cluster <- as.vector(cluster==1)
cluster.col <- as.vector(ifelse(cluster==1,"red","blue"))

values <- as.vector(Qs[1:n,])
tmp.survey.type <- rep(cluster.HC,each=n)
coef.lab <- NULL
for(i in 1:n){
  if (i < 10) coef.lab[i] <- paste0('C0',i)
  if (i >= 10) coef.lab[i] <- paste0('C',i)
}  

tmp.coefs <- rep(coef.lab,T)
tmp.coefs <- rep(EOM.lab,T)

df <- data.frame(EOM=tmp.coefs, 
                 Seasons=tmp.survey.type, 
                 Scores=values)
pvalue <- NULL
for(i in 1:n) pvalue[i] <- anova(lm(Scores[EOM==EOM.lab[i]]~Seasons[EOM==EOM.lab[i]],data=df))$`Pr(>F)`[1]

# grouped boxplot
# put transparency proportional to the pvalue of the statistical different between Hot and Cold
ggplot(df, aes(x=EOM, y=Scores, fill=Seasons)) +
  scale_x_discrete(limits=EOM.lab)+
  geom_boxplot(alpha=rep(1-sqrt(pvalue),each=2))+
  scale_fill_manual(values=c("skyblue","red"))+
  geom_hline(yintercept=0,linetype="dashed")+
  theme_bw()
#scale_fill_discrete(type=rep(c("skyblue","red"),10))
dev.print(device = png, file = "Res/mean.coefficients.png",width=450,height=450)



# Figure 4 ----------------------------------------------------------------
Hot.mean <- apply(Qs[1:n,sel.Hot.cluster],1,"mean")
Cold.mean <- apply(Qs[1:n,!sel.Hot.cluster],1,"mean")

Hot.mean.est.standardised.map <- as.vector(EOMs[,1:n]%*%Hot.mean)
Cold.mean.est.standardised.map <- EOMs[,1:n]%*%Cold.mean

#Upload mean distribution in DB
sel.df2db <- db.in$Polygon
tmp <- rep(NA, length(sel.df2db))
tmp[sel.df2db] <- Hot.mean.est.standardised.map
db.in <- db.add(db.in,"Hot.mean.est.standardised.map"= tmp)
tmp[sel.df2db] <- Cold.mean.est.standardised.map
db.in <- db.add(db.in,"Cold.mean.est.standardised.map"= tmp)
tmp[sel.df2db] <- EOMs[,1]
db.in <- db.add(db.in,"EOM.1"= tmp)
tmp[sel.df2db] <- EOMs[,2]
db.in <- db.add(db.in,"EOM.2"= tmp)

plot(db.in,name.image="Hot.mean.est.standardised.map",asp=1,
     bty="n",xaxt="n",yaxt="n",title="")
map("worldHires",add=T, fill = T,col="white")
dev.print(device = png, file = "Res/Hot.mean.png",width=450,height=450)

plot(db.in,name.image="Cold.mean.est.standardised.map",asp=1,
     bty="n",xaxt="n",yaxt="n",title="")
map("worldHires",add=T, fill = T,col="white")
dev.print(device = png, file = "Res/Cold.mean.png",width=450,height=450)

plot(db.in,name.image="EOM.1",asp=1,
     bty="n",xaxt="n",yaxt="n",title="")
map("worldHires",add=T, fill = T,col="white")
dev.print(device = png, file = "Res/EOM.1.png",width=450,height=450)

plot(db.in,name.image="EOM.2",asp=1,
     bty="n",xaxt="n",yaxt="n",title="")
map("worldHires",add=T, fill = T,col="white")
dev.print(device = png, file = "Res/EOM.2.png",width=450,height=450)

p1 <- round(cumul.cor.expl[1]*100,1)
p2 <- round(diff(cumul.cor.expl[c(1,2)])*100,1)
plot(Qs[1,],Qs[2,],
     col=cluster.col,
     pch=ifelse(sel.hot,yes="H",no="C"),
     xlab=paste0("EOM.1 (",p1,"%)"),ylab=paste0("EOM.2 (",p2,"%)"),
     las=1,asp=1)
abline(h=0,v=0,lty=2,col="grey")
dev.print(device = png, file = "Res/plan.MAFactoriel.png",width=450,height=450)
