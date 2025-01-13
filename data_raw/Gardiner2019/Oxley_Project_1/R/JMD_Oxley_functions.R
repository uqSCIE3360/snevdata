get.binomial.means.func<-function(binary.var)(summary(glm(binary.var~1, family=binomial))$coef[1,1]) 
get.binomial.se.func<-function(binary.var)(summary(glm(binary.var~1, family=binomial))$coef[1,2]) 

## Logistic plot function
logistic.plot<-function(x,y,n.bins, colour, point.type){
    ocx<-order(x)
    ox<-x[ocx]
    oy<-y[ocx]
    ncuts<-n.bins
    reps<-ceiling(length(ocx)/ncuts)
    clst<-gl(ncuts,reps,length=length(ocx))
    logit.y<-as.numeric(sapply(split(oy,clst),get.binomial.means.func))
    logit.se.y<-as.numeric(sapply(split(oy,clst),get.binomial.se.func))
	 mean.y<-plogis(logit.y)
	 upper.y<-plogis(logit.y + logit.se.y)
	 lower.y<-plogis(logit.y - logit.se.y) 
	 mean.x<-as.numeric(sapply(split(ox,clst),mean,na.rm=T))
    points(mean.x,mean.y, col = colour, pch=point.type)
    arrows(mean.x, lower.y, mean.x, upper.y, length = 0, angle = 0, code = 2, col=colour)
}  


## create nice evenly space variables for plotting
seq.func<-function(x)(seq(min(x, na.rm=T), max(x, na.rm=T), length.out=100))

### plot with 95% CI using lmer  or lme objects
lmer.predict<-function(mod, newdat, se.mult, binom=NULL, poisson=NULL){
pvar1 <- diag(as.matrix(newdat) %*% tcrossprod(vcov(mod),as.matrix(newdat)))
newdat$y<- as.matrix(newdat) %*% fixef(mod)  
newdat <- data.frame(newdat, plo = newdat$y-(se.mult*sqrt(pvar1)), phi = newdat$y+(se.mult*sqrt(pvar1)))

## if you have used binomial errors then this will back transform logits to the probability scale
if(binom==T) {
		newdat$y<-plogis(newdat$y); newdat$plo<-plogis(newdat$plo); newdat$phi<-plogis(newdat$phi)
		} else 

## if you have used poisson errors or have log-transformed your response, then this will back transform to the original scale (e.g. abundance)
if(poisson==T) {
		newdat$y<-exp(newdat$y); newdat$plo<-exp(newdat$plo); newdat$phi<-exp(newdat$phi)
		} 
return(with(newdat, data.frame(y, phi, plo)))
}

plot.CI.func<- function(x.for.plot, pred, upper, lower, env.colour, env.trans=NA, line.colour, line.weight, line.type){
colour.rgb<-col2rgb(col=env.colour)  
polygon.coords<-data.frame(rbind(cbind(x.for.plot[1], lower[1]), 
							cbind(x.for.plot, upper), 
							cbind(x.for.plot, lower)[rev(order(x.for.plot)),]))
names(polygon.coords)<-c("x", "y")							
polygon(polygon.coords$x, polygon.coords$y, col=rgb(red=colour.rgb["red",],blue=colour.rgb["blue",], green=colour.rgb["green",] , alpha=env.trans, maxColorValue = 255), border=NA)
lines(x.for.plot, pred, col=line.colour, lwd=line.weight, lty=line.type)         
} 

## function to put histogram in pairs plot diagonals
panel.hist <- function(x, ...)
{
    usr <- par("usr"); on.exit(par(usr))
    par(usr = c(usr[1:2], 0, 1.5) )
    h <- hist(x, plot = FALSE)
    breaks <- h$breaks; nB <- length(breaks)
    y <- h$counts; y <- y/max(y)
    rect(breaks[-nB], 0, breaks[-1], y, col = "cyan", ...)
}

## function to unstandardise a variable
un.std.func<-function(std.x, mean.x, sd.x) ((std.x*sd.x) + mean.x)
