library(ggplot2)

rs2017 <- readRDS("regSeason2017.Rda")
calledStrikes <- rs2017[rs2017$des=="Called Strike",]
strikezone <- data.frame(x1=-0.83083,x2=0.83083,y1=1.5,y2=3.5)
cs <- calledStrikes[1:1000,]

normedz <- 2.0*(calledStrikes$pz-calledStrikes$sz_top)/(calledStrikes$sz_top-calledStrikes$sz_bot)+3.5
normedCalledStrikes <- calledStrikes
normedCalledStrikes$pz <- normedz
ns <- normedCalledStrikes[1:1000,]
ggplot() + geom_point(data=normedCalledStrikes, aes(x=px, y=pz), alpha=0.02, color="red3", size= 3, stroke = 1) +
          geom_rect(data=strikezone, aes(xmin=x1, xmax=x2, ymin=y1, ymax=y2), color="black", alpha=0) +  
          coord_fixed() + scale_x_continuous(limits = c(-1.5,1.5)) + scale_y_continuous(limits = c(0.5,4)) +
          theme_bw()
calledBalls <- rs2017[rs2017$des=="Ball",]
normedzb <- 2.0*(calledBalls$pz-calledBalls$sz_top)/(calledBalls$sz_top-calledBalls$sz_bot)+3.5
normedCalledBalls <- calledBalls
normedCalledBalls$pz <- normedzb
nb <- normedCalledBalls[1:1000,]
ggplot() + geom_point(data=normedCalledBalls, aes(x=px, y=pz), alpha=0.02, color="green3", size= 3, stroke = 1) +
          geom_rect(data=strikezone, aes(xmin=x1, xmax=x2, ymin=y1, ymax=y2), color="black", alpha=0) +  
          coord_fixed() + scale_x_continuous(limits = c(-1.5,1.5)) + scale_y_continuous(limits = c(0.5,4)) +
          theme_bw()

ggplot() + geom_point(data=normedCalledBalls, aes(x=px, y=pz), alpha=0.02, color="blue3", size= 3, stroke = 1) +
  geom_point(data=normedCalledStrikes, aes(x=px, y=pz), alpha=0.02, color="yellow", size= 3, stroke = 1) +
          geom_rect(data=strikezone, aes(xmin=x1, xmax=x2, ymin=y1, ymax=y2), color="black", alpha=0) +  
          coord_fixed() + coord_cartesian(xlim = c(-1.5,1.5), ylim=c(0.5,4)) +
          theme_bw()

ggplot() + 
  geom_point(data=normedCalledStrikes, aes(x=px, y=pz), alpha=0.02, color="yellow", size= 3, stroke = 1) +
  geom_point(data=normedCalledBalls, aes(x=px, y=pz), alpha=0.02, color="blue3", size= 3, stroke = 1) +
  geom_rect(data=strikezone, aes(xmin=x1, xmax=x2, ymin=y1, ymax=y2), color="black", alpha=0) +  
  coord_fixed(xlim = c(-1.5,1.5), ylim=c(0.5,4)) +
  theme_bw()
