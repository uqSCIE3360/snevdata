### Figure 3 in Yamamichi et al. 
### A shady phytoplankton paradox: When phytoplankton increases under low light

type <- c("202L", "219L", "204M", "218M", "203H", "217H") # Low light = black, mid = blue, 
cols <- c("black", "black", "blue", "blue", "red", "red") # Low light = black, mid = blue, control = red
pchs <- c(19, 15, 19, 15, 19, 15) # Road side: circle (19), canal side: square (15)
percentage <- c(0.246, 0.246, 0.435, 0.435, 1, 1)*100;

## Fig. 3b
# Chlorophyll a vs. light intensity
dev.new(width=8,height=6); 
par(mar=c(5, 5.5, 4, 2) + 0.1)

read.csv("chla.csv",header=TRUE)->dat0
dat<-t(dat0[,-1])

x<-percentage[1]; y<-mean(dat[-1,1]); z<-sd(dat[-1,1])/sqrt(nrow(dat)-1);
plot(x,y,xlim=c(0,110),ylim=c(0,15),xlab="Percent surface light transmission",ylab=expression(paste("Mean Chl ", italic(a), " (", mu, "g ", L^{-1}, ")")),pch=pchs[1],col=cols[1],cex=2,cex.lab=1.7)
arrows(x, y+z, x, y-z, angle = 90, length = 0.1, col = cols[1])
arrows(x, y-z, x, y+z, angle = 90, length = 0.1, col = cols[1])

for(i in 2:6){
	x<-percentage[i]; y<-mean(dat[-1,i]); z<-sd(dat[-1,i])/sqrt(nrow(dat)-1);
	lines(x,y,pch=pchs[i],col=cols[i],type="p",cex=2)
	arrows(x, y+z, x, y-z, angle = 90, length = 0.1, col = cols[i])
	arrows(x, y-z, x, y+z, angle = 90, length = 0.1, col = cols[i])
}
legend("topleft", legend = type, pch = pchs, col = cols)


## Fig. 3d
# Macrophyte biomass vs. light intensity
macro<-c(0, 0.221, 75.2, 31.5, 0, 94.3)
dev.new(width=8,height=6); 
par(mar=c(5, 5.5, 4, 2) + 0.1)
x<-percentage[1]; y<-macro[1];
plot(x,y,xlim=c(0,110),ylim=c(0,100),xlab="Percent surface light transmission",ylab=expression(paste("Macrophyte biomass (g DW ", m^{-2}, ")")),pch=pchs[1],col=cols[1],cex=2,cex.lab=1.7)

for(i in 2:6){
	x<-percentage[i]; y<-macro[i];
	lines(x,y,pch=pchs[i],col=cols[i],type="p",cex=2)
}
legend("topleft", legend = type, pch = pchs, col = cols)