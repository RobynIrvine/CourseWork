# this is a comment!
graphics.off()
rm(list=ls())

summary(datasets::trees)
F<-45
C<-(F-32)*(5/9)
print(C)

Fstr<-c(0,32,64,100)
C<-(Fstr-32)*(5/9)
print(C)

farenheit_to_kelvin <- function (farenheit) {
  kelvin <- ((farenheit - 32) * 5 / 9) + 273.15
  return (kelvin)
}
farenheit_to_kelvin(c(0, 32))

install.packages("tidyr")
library(tidyr)

yrcount <- data.frame( day = 1:3, year1 = seq(-2,3, length.out = 3), year2 = 3:5, year3 = 7:9)
yrcount
yrcount <- gather(yrcount, year, count, -day)
yrcount

head(spread(yrcount, year, count))

library(ggplot2)
gp <- ggplot(data = trees, aes(x = Girth, y = Volume))
gp <- gp + geom_point()
print(gp)

qplot(Girth, Volume, data = trees)

library(ggplot2)
gp <- ggplot(data = trees, aes(x = Girth, y = Volume))
gp <- gp + geom_line()
print(gp)

library(ggplot2)
gp <- ggplot(data = trees, aes(x = Girth, y = Volume))
gp <- gp + geom_line()
gp <- gp+ geom_point()
print(gp)

gp <- ggplot(data=msleep,aes(x=bodywt,y=brainwt,size=sleep_total,colour=vore))
gp <- gp + geom_point()
gp <- gp + scale_x_continuous(name="Body Weight (kg)",trans="log10")
gp <- gp + scale_y_continuous(name="Brain Weight (kg)",trans="log10")
gp <- gp + scale_size_continuous(name="Sleep (hr)")
gp <- gp + scale_color_discrete(name="Diet")
quartz()
gp
ggsave(filename="msleep.png",plot=gp, width=7,height=7, dpi=600)
