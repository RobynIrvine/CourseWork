# this is a comment!
graphics.off()
rm(list=ls())

head(ToothGrowth)
gp<-ggplot(data=ToothGrowth, aes(x=dose, y=len2, colour=supp))
gp <- gp + scale_size_continuous(name="Sleep (hr)")
gp <- gp + scale_color_discrete(name="Diet")
gp<- gp+ geom_jitter(position=position_jitter(width=0.1)) + xlab("Dose (mg)") + ylab("Length (mm)")
gp<- gp+ expand_limits(x=c(0,2.5))
quartz(width=4, height=4)
gp

gp <- ggplot(data=msleep,aes(x=bodywt,y=brainwt,size=sleep_total))
gp <- gp + facet_grid(.~vore)
gp <- gp + geom_point()
gp <- gp + scale_x_continuous(name="Body Weight (kg)",trans="log10")
gp <- gp + scale_y_continuous(name="Brain Weight (kg)",trans="log10")
gp <- gp + scale_size_continuous(name="Sleep (hr)")
quartz()
print(gp)


gp<-ggplot(data=ToothGrowth, aes(x=dose, y=len2))
gp <- gp + facet_grid(.~supp)
gp <- gp + scale_size_continuous(name="Sleep (hr)")
gp <- gp + scale_color_discrete(name="Diet")
gp<- gp+ geom_jitter(position=position_jitter(width=0.1)) + xlab("Dose (mg)") + ylab("Length (mm)")
gp<- gp+ expand_limits(x=c(0,2.5))
quartz(width=4, height=4)
gp

theme_set(theme_bw())

gp<-ggplot(data=ToothGrowth, aes(x=dose, y=len2))
gp <- gp + facet_grid(.~supp)
gp <- gp + scale_size_continuous(name="Sleep (hr)")
gp <- gp + scale_color_discrete(name="Diet")
gp <- gp + theme(panel.grid.major=element_line(),panel.grid.minor=element_line())
gp<- gp+ geom_jitter(position=position_jitter(width=0.1)) + xlab("Dose (mg)") + ylab("Length (mm)")
gp<- gp+ expand_limits(x=c(0,2.5))
quartz(width=4, height=4)
gp

theme_set(theme_bw())
gp<-ggplot(data=ToothGrowth, aes(x=dose, y=len2))
gp <- gp + facet_grid(.~supp)
gp <- gp + scale_size_continuous(name="Sleep (hr)")
gp <- gp + scale_color_discrete(name="Diet")
gp <- gp + theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank())
gp<- gp+ geom_jitter(position=position_jitter(width=0.1)) + xlab("Dose (mg)") + ylab("Length (mm)")
gp<- gp+ expand_limits(x=c(0,2.5))
quartz(width=4, height=4)
gp

#linear regression
mod<-lm(Volume~Girth,data=trees)
summary(mod)


library(datalist)
newdata <- datalist::new_data(trees, "Girth")
newdata$Volume <- predict(mod,newdata=newdata)

gp <- ggplot(data=trees, aes(x=Girth,y=Volume))
gp <- gp + geom_point()
gp <- gp + geom_line(data=newdata)
print(gp)

predc <- data.frame(predict(mod,newdata,interval="conf"))
predp <- data.frame(predict(mod,newdata,interval="pred"))
newdatac <- cbind(newdata,predc)
newdatap<-cbind (newdata, predp)
gp <- gp + geom_line(data=newdatac,aes(y=lwr),linetype='dashed')
gp <- gp + geom_line(data=newdatac,aes(y=upr),linetype='dashed')
gp <- gp + geom_line(data=newdatap,aes(y=lwr),linetype='dashed', colour="red")
gp <- gp + geom_line(data=newdatap,aes(y=upr),linetype='dashed', colour="red")
print(gp)

qplot(.fitted, .stdresid, data = mod) + geom_hline(yintercept = 0) + geom_smooth(se = TRUE)

#allometric 

#linear regression
mod<-lm(log(Volume)~log(Girth),data=trees)
summary(mod)

qplot(.fitted, .stdresid, data = mod) + geom_hline(yintercept = 0) + geom_smooth(se = TRUE)

#ANCOVA with toothgrowth
mod <- lm(len ~ supp + dose, data = ToothGrowth)
summary(mod)

qplot(.fitted, .stdresid, data = mod) + geom_hline(yintercept = 0) + geom_smooth(se = TRUE)

newdata <- datalist::new_data(ToothGrowth, c("dose", "supp"))
newdata$len <- predict(mod, newdata=newdata)

gp <- ggplot(data=ToothGrowth,aes(x=dose,y=len,colour=supp))
gp <- gp + geom_point(position=position_jitter(width=0.1))
gp <- gp + geom_line(data=newdata,aes(group=supp))
print(gp)

mod2 <- lm(len ~ supp, data = ToothGrowth)
mod3 <- lm(len ~ dose, data = ToothGrowth)
mod4 <- lm(len ~ dose * supp, data = ToothGrowth)

#old section 8 onwards
# SEction 8 onwards
#july 29-09  
graphics.off()
rm(list=ls())

mod1<-lm(Volume~Girth, data=trees)
summary(mod1)
confint(mod1)

windows()
plot(Volume~Girth, data=trees)
newdata<-data.frame(Girth=seq(from=min(trees$Girth), to = max(trees$Girth), length.out=30))
newdata$fit<-predict (mod1,newdata)
lines(fit~Girth, data=newdata)

#add confidence intervals
mat<-predict(mod1, newdata, interval="confidence")
newdata$LowerCI<-mat[,2]
newdata$UpperCI<-mat[,3]
lines(LowerCI~Girth, data=newdata, lty="dashed")
lines(UpperCI~Girth, data=newdata, lty="dashed")
print(mat)

#add prediction intervals
mat.prediction<-predict(mod1, newdata, interval="prediction")
newdata$LowerPI<-mat.prediction[,2]
newdata$UpperPI<-mat.prediction[,3]
lines(LowerPI~Girth, data=newdata, lty="dashed",col="red")
lines(UpperPI~Girth, data=newdata, lty="dashed",col="red")

windows()
par(mfrow=c(2,2))
plot(mod1)

trees$logG<-log(trees$Girth)
trees$logV<-log(trees$Volume)
print(trees)

modlog<-lm(logV~logG, data=trees)
summary(modlog)
windows()
par(mfrow=c(2,2))
plot(modlog)

#exercise 8.6
data(ToothGrowth)
print(ToothGrowth)
windows()
plot(len~dose, data=ToothGrowth, type="n", xlab="Dose(mg)", ylab="Length (mm)", xlim=c(0,2.5))
points(len~dose, data=ToothGrowth[ToothGrowth$supp=="OJ",],pch=4, col="red")
points(len~dose, data=ToothGrowth[ToothGrowth$supp=="VC",],pch=5, col="blue")

tooth.mod<-lm(len~supp+dose,data=ToothGrowth)
#summary(tooth.mod)
#windows()
#par(mfrow=c(2,2))
#plot(tooth.mod)

newdata2<-expand.grid(dose=c(0.5,1,2),supp=c("OJ","VC"))
newdata2$fit<-predict(tooth.mod,newdata2)
lines(fit~dose, data=newdata2[newdata2$supp=="OJ",],col="red")
lines(fit~dose,data=newdata2[newdata2$supp=="VC",],col="blue")

tooth.mod2<-lm(len~supp, data=ToothGrowth)
tooth.mod3<-lm(len~dose, data=ToothGrowth)
tooth.mod4<-lm(len~dose+supp, data=ToothGrowth)

windows()


