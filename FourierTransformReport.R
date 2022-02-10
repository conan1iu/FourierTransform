## ----eval = T, echo = F------------------------------------------------------------------

library(ggpmisc)
library(ggplot2)
library(dplyr)
library(ggthemr)
ggthemr('fresh')
library(xtable)
library(latex2exp)
library(datasets)
air <- data.frame(AirPassengers)
air <- data.frame(cbind(seq(1, nrow(air), 1)), air)
air$Time <- air$cbind.seq.1..nrow.air...1..
air <- data.frame(air[,2:3])


## ----eval = T, echo = F, fig = T, height = 3, png=TRUE-----------------------------------
ggplot(air, aes(Time, AirPassengers)) +
  geom_path(aes(color = "lightblue")) +
  xlab("Time (months)") +
  ylab("Number of passengers") +
  theme(legend.position = "none")


## ----eval = T, echo = F, fig = T, width = 8, height = 3, png=TRUE------------------------
fsine <- function(x) (sin((6*pi)*x + pi/2)+1)
xx <- seq(0, 4.5, by = 0.001)
xxx <- as.data.frame(xx)
sin <- as.data.frame(fsine(xx))
sin <- cbind(xxx, sin)
sin <- as.data.frame(sin)

ggplot(sin, aes(xx, fsine(xx))) + 
  geom_path(aes(color = "lightblue")) +
  xlab("Time (s)") +
  ylab("f(Time)") +
  scale_x_continuous(breaks = seq(0, 4.5, by = 1)) +
  geom_hline(yintercept = 0, color = "black", alpha = 0.3)  +
  geom_vline(xintercept = 0, color = "black", alpha = 0.3) +
  scale_x_continuous(expand = c(0, 0)) + 
  scale_y_continuous(expand = c(0, 0)) +
  theme(legend.position = "none") +
  theme(legend.position = "none")


## ----eval = T, echo = F, fig = T, height = 2.5, png=TRUE---------------------------------
get_circle_coords <- function(r = 1, ...) {
  data_frame(theta = seq(0, 2 * pi, ...),
             x     = cos(theta) * r,
             y     = sin(theta) * r)
}

circ <- get_circle_coords(length.out = 200)
circ <- as.data.frame(circ)

library(dplyr)

circ$color <- ifelse(circ$y>=0.0, ifelse(circ$x > 0.5, "blue", "black"), "black")

ggplot(circ, aes(x = x, y = y)) +
  geom_path(aes(color = "lightblue")) +
  coord_fixed() +
  theme(legend.position = "none", axis.text.x = element_blank(), axis.text.y = element_blank())


## ----eval = T, echo = F, fig = T, height = 3, png=TRUE-----------------------------------
library(egg)
one <- ggplot(circ, aes(x = x, y = y)) +
  geom_path(aes(color = "blue")) +
  coord_fixed() +
  theme(legend.position = "none", axis.text.x = element_blank(), axis.text.y = element_blank()) +
  geom_segment(
    x = 0, y = 0,
    xend = 0, yend = 1,
    lineend = "round", # See available arrow types in example above
    linejoin = "round",
    size = 1, 
    arrow = arrow(length = unit(0.3, "inches")),
    colour = "skyblue" # Also accepts "red", "blue' etc
  ) 

two <- ggplot(circ, aes(x = x, y = y)) +
  geom_path(aes(color = "blue")) +
  coord_fixed() +
  theme(legend.position = "none", axis.text.x = element_blank(), axis.text.y = element_blank()) +
  geom_segment(
    x = 0, y = 0,
    xend = 1, yend = 0,
    lineend = "round", # See available arrow types in example above
    linejoin = "round",
    size = 1, 
    arrow = arrow(length = unit(0.3, "inches")),
    colour = "tomato" # Also accepts "red", "blue' etc
  ) 

three <- ggplot(circ, aes(x = x, y = y)) +
  geom_path(aes(color = "blue")) +
  coord_fixed() +
  theme(legend.position = "none", axis.text.x = element_blank(), axis.text.y = element_blank()) +
  geom_segment(
    x = 0, y = 0,
    xend = 0, yend = -1,
    lineend = "round", # See available arrow types in example above
    linejoin = "round",
    size = 1, 
    arrow = arrow(length = unit(0.3, "inches")),
    colour = "pink" # Also accepts "red", "blue' etc
  ) 

four <- ggplot(circ, aes(x = x, y = y)) +
  geom_path(aes(color = "blue")) +
  coord_fixed() +
  theme(legend.position = "none", axis.text.x = element_blank(), axis.text.y = element_blank()) +
  geom_segment(
    x = 0, y = 0,
    xend = -1, yend = 0,
    lineend = "round", # See available arrow types in example above
    linejoin = "round",
    size = 1, 
    arrow = arrow(length = unit(0.3, "inches")),
    colour = "palegreen" # Also accepts "red", "blue' etc
  ) 

ggarrange(two, three, four, one, nrow = 1, ncol = 4)


## ----eval = T, echo = F, hide = T--------------------------------------------------------
fperfect <- function(x) (sin((6*pi)*x + pi/2)+1)*exp(-2*pi*(1/2)*x*(0+1i))
xx <- seq(0, 4.5, by=0.0001)

new <- fperfect(xx)

df <- data.frame(Re(new), Im(new))

df$Re <- df$Re.new. 
df$Re.new. <- NULL

df$Im <- df$Im.new.
df$Im.new. <- NULL

library(ggthemr)
library(ggpmisc)
ggthemr('fresh')


## ----eval = T, echo = F, fig = T, height = 2.5, png=TRUE---------------------------------
flower <- ggplot(df, aes(Re, Im)) + 
  geom_path(size = 0.5, alpha = 0.3, color = "steelblue") +
  xlab("") +
  ylab("") +
  scale_x_continuous(limits = symmetric_limits) +
  scale_y_continuous(limits = symmetric_limits) +
  geom_hline(yintercept = 0, color = "black", alpha = 0.3)  +
  geom_vline(xintercept = 0, color = "black", alpha = 0.3) +
  coord_fixed(ratio = 1) +
    theme(legend.position = "none", axis.text.x = element_blank(), axis.text.y = element_blank()) +
  geom_segment(
    x = 0, y = 0,
    xend = 2, yend = 0,
    lineend = "round", # See available arrow types in example above
    linejoin = "round",
    size = 1, 
    arrow = arrow(length = unit(0.3, "inches")),
    colour = "tomato" # Also accepts "red", "blue' etc
  ) +
  theme(legend.position = "none")

flower


## ----eval = T, echo = F, fig = T, height = 3.5, png=TRUE---------------------------------
sinnn <- ggplot(sin, aes(xx, fsine(xx))) + 
  geom_path(aes(color = "steelblue")) +
  xlab("Time (s)") +
  ylab("f(Time)") +
  scale_x_continuous(breaks = seq(0, 4.5, by = 1)) +
  geom_hline(yintercept = 0, color = "black", alpha = 0.3)  +
  geom_vline(xintercept = 0, color = "black", alpha = 0.3) +
  scale_x_continuous(expand = c(0, 0)) + 
  scale_y_continuous(expand = c(0, 0)) +
  theme(legend.position = "none")

ggarrange(flower, sinnn, ncol = 2, nrow = 1)


## ----eval = T, echo = F, fig = T, height = 3, png=TRUE-----------------------------------
sinnnn <- ggplot(sin[sin$xx <= 2,], aes(xx, fsine(xx))) + 
  geom_path(aes(color = "lightblue")) +
  xlab("Time (s)") +
  ylab("f(Time)") +
  scale_x_continuous(breaks = seq(0, 2, by = 1)) +
  geom_hline(yintercept = 0, color = "black", alpha = 0.3)  +
  geom_vline(xintercept = 0, color = "black", alpha = 0.3) +
  scale_x_continuous(expand = c(0, 0)) + 
  scale_y_continuous(expand = c(0, 0)) +
  theme(legend.position = "none")

ggarrange(flower, sinnnn, ncol = 2, nrow = 1)


## ----eval = T, echo = F, hide = T--------------------------------------------------------
f2.7 <- function(x) (sin((6*pi)*x + pi/2)+1)*exp(-2*pi*(2.72)*x*(0+1i))
f1 <- function(x) (sin((6*pi)*x + pi/2)+1)*exp(-2*pi*(1.56)*x*(0+1i))
f0.3 <- function(x) (sin((6*pi)*x + pi/2)+1)*exp(-2*pi*(0.21)*x*(0+1i))

fff <- seq(0, 4.5, by=0.0001)

F2 <- f2.7(fff)
F1 <- f1(fff)
FF <- f0.3(fff)

F22 <- data.frame(Re(F2), Im(F2))
F11 <- data.frame(Re(F1), Im(F1))
FFF <- data.frame(Re(FF), Im(FF))

G2 <- ggplot(F22, aes(Re.F2., Im.F2.)) + 
  geom_path(size = 0.5, alpha = 0.3) +
  xlab("") +
  ylab("") +
  scale_x_continuous(limits = symmetric_limits) +
  scale_y_continuous(limits = symmetric_limits) +
  geom_hline(yintercept = 0, color = "black", alpha = 0.3)  +
  geom_vline(xintercept = 0, color = "black", alpha = 0.3) +
  coord_fixed(ratio = 1) 

G1 <- ggplot(F11, aes(Re.F1., Im.F1.)) + 
  geom_path(size = 0.5, alpha = 0.3) +
  xlab("") +
  ylab("") +
  scale_x_continuous(limits = symmetric_limits) +
  scale_y_continuous(limits = symmetric_limits) +
  geom_hline(yintercept = 0, color = "black", alpha = 0.3)  +
  geom_vline(xintercept = 0, color = "black", alpha = 0.3) +
  coord_fixed(ratio = 1) 

G0 <- ggplot(FFF, aes(Re.FF., Im.FF.)) + 
  geom_path(size = 0.5, alpha = 0.3) +
  xlab("") +
  ylab("") +
  scale_x_continuous(limits = symmetric_limits) +
  scale_y_continuous(limits = symmetric_limits) +
  geom_hline(yintercept = 0, color = "black", alpha = 0.3)  +
  geom_vline(xintercept = 0, color = "black", alpha = 0.3) +
  coord_fixed(ratio = 1) 


## ----eval = T, echo = F, fig = T, height = 3, png=TRUE-----------------------------------
ggarrange(G0, G1, G2, ncol = 3, nrow = 1)


## ----eval = T, echo = F, hide = T--------------------------------------------------------
f2.8 <- function(x) (sin((6*pi)*x + pi/2)+1)*exp(-2*pi*(2.9)*x*(0+1i))
f2.9 <- function(x) (sin((6*pi)*x + pi/2)+1)*exp(-2*pi*(2.95)*x*(0+1i))
f3 <- function(x) (sin((6*pi)*x + pi/2)+1)*exp(-2*pi*(3)*x*(0+1i))

fff <- seq(0, 4.5, by=0.0001)

FF28 <- f2.8(fff)
FF29 <- f2.9(fff)
FF3 <- f3(fff)

F28 <- data.frame(Re(FF28), Im(FF28))
F29 <- data.frame(Re(FF29), Im(FF29))
F33 <- data.frame(Re(FF3), Im(FF3))

G28 <- ggplot(F28, aes(Re.FF28., Im.FF28.)) + 
  geom_path(size = 0.5, alpha = 0.3) +
  xlab("") +
  ylab("") +
  scale_x_continuous(limits = symmetric_limits) +
  scale_y_continuous(limits = symmetric_limits) +
  geom_hline(yintercept = 0, color = "black", alpha = 0.3)  +
  geom_vline(xintercept = 0, color = "black", alpha = 0.3) +
  coord_fixed(ratio = 1) 

G29 <- ggplot(F29, aes(Re.FF29., Im.FF29.)) + 
  geom_path(size = 0.5, alpha = 0.3) +
  xlab("") +
  ylab("") +
  scale_x_continuous(limits = symmetric_limits) +
  scale_y_continuous(limits = symmetric_limits) +
  geom_hline(yintercept = 0, color = "black", alpha = 0.3)  +
  geom_vline(xintercept = 0, color = "black", alpha = 0.3) +
  coord_fixed(ratio = 1) 

G30 <- ggplot(F33, aes(Re.FF3., Im.FF3.)) + 
  geom_path(size = 0.5, alpha = 0.3) +
  xlab("") +
  ylab("") +
  scale_x_continuous(limits = symmetric_limits) +
  scale_y_continuous(limits = symmetric_limits) +
  geom_hline(yintercept = 0, color = "black", alpha = 0.3)  +
  geom_vline(xintercept = 0, color = "black", alpha = 0.3) +
  coord_fixed(ratio = 1) 


## ----eval = T, echo = F, fig = T, height = 3,png=TRUE------------------------------------
ggarrange(G28, G29, G30, ncol = 3, nrow = 1)


## ----eval = T, echo = F, fig = T, height = 4,png=TRUE------------------------------------
ggarrange(G30, sinnnn, ncol = 2, nrow = 1)


## ----eval = T, echo = F, hide = T--------------------------------------------------------
f2.7 <- function(x) (sin((6*pi)*x + pi/2)+1)*exp(-2*pi*(2.72)*x*(0+1i))
f1 <- function(x) (sin((6*pi)*x + pi/2)+1)*exp(-2*pi*(1.56)*x*(0+1i))
f0.3 <- function(x) (sin((6*pi)*x + pi/2)+1)*exp(-2*pi*(0.21)*x*(0+1i))

fff <- seq(0, 4.5, by=0.0001)

F2 <- f2.7(fff)
F1 <- f1(fff)
FF <- f0.3(fff)

F22 <- data.frame(Re(F2), Im(F2))
F11 <- data.frame(Re(F1), Im(F1))
FFF <- data.frame(Re(FF), Im(FF))

G2 <- ggplot(F22, aes(Re.F2., Im.F2.)) + 
  geom_path(size = 0.5, alpha = 0.3) +
  geom_point(aes(x = mean(Re(F2)), y = 0), col = "red") +
  xlab("") +
  ylab("") +
  scale_x_continuous(limits = symmetric_limits) +
  scale_y_continuous(limits = symmetric_limits) +
  geom_hline(yintercept = 0, color = "black", alpha = 0.3)  +
  geom_vline(xintercept = 0, color = "black", alpha = 0.3) +
  coord_fixed(ratio = 1) 

G1 <- ggplot(F11, aes(Re.F1., Im.F1.)) + 
  geom_path(size = 0.5, alpha = 0.3) +
  geom_point(aes(x = mean(Re(F1)), y = 0), col = "red") +
  xlab("") +
  ylab("") +
  scale_x_continuous(limits = symmetric_limits) +
  scale_y_continuous(limits = symmetric_limits) +
  geom_hline(yintercept = 0, color = "black", alpha = 0.3)  +
  geom_vline(xintercept = 0, color = "black", alpha = 0.3) +
  coord_fixed(ratio = 1) 

G0 <- ggplot(FFF, aes(Re.FF., Im.FF.)) + 
  geom_path(size = 0.5, alpha = 0.3) +
  geom_point(aes(x = mean(Re(FF)), y = 0), col = "red") +
  xlab("") +
  ylab("") +
  scale_x_continuous(limits = symmetric_limits) +
  scale_y_continuous(limits = symmetric_limits) +
  geom_hline(yintercept = 0, color = "black", alpha = 0.3)  +
  geom_vline(xintercept = 0, color = "black", alpha = 0.3) +
  coord_fixed(ratio = 1) 


## ----eval = T, echo = F, fig = T, height = 4,png=TRUE------------------------------------
ggarrange(G0, G1, G2, ncol = 3, nrow = 1)


## ----eval = T, echo = F, hide = T--------------------------------------------------------
G28 <- ggplot(F28, aes(Re.FF28., Im.FF28.)) + 
  geom_path(size = 0.5, alpha = 0.3) +
  geom_point(aes(x = mean(Re(FF28)), y = 0), col = "red") +
  xlab("") +
  ylab("") +
  scale_x_continuous(limits = symmetric_limits) +
  scale_y_continuous(limits = symmetric_limits) +
  geom_hline(yintercept = 0, color = "black", alpha = 0.3)  +
  geom_vline(xintercept = 0, color = "black", alpha = 0.3) +
  coord_fixed(ratio = 1) 

G29 <- ggplot(F29, aes(Re.FF29., Im.FF29.)) + 
  geom_path(size = 0.5, alpha = 0.3) +
  geom_point(aes(x = mean(Re(FF29)), y = 0), col = "red") +
  xlab("") +
  ylab("") +
  scale_x_continuous(limits = symmetric_limits) +
  scale_y_continuous(limits = symmetric_limits) +
  geom_hline(yintercept = 0, color = "black", alpha = 0.3)  +
  geom_vline(xintercept = 0, color = "black", alpha = 0.3) +
  coord_fixed(ratio = 1) 

G30 <- ggplot(F33, aes(Re.FF3., Im.FF3.)) + 
  geom_path(size = 0.5, alpha = 0.3) +
  geom_point(aes(x = mean(Re(FF3)), y = 0), col = "red") +
  xlab("") +
  ylab("") +
  scale_x_continuous(limits = symmetric_limits) +
  scale_y_continuous(limits = symmetric_limits) +
  geom_hline(yintercept = 0, color = "black", alpha = 0.3)  +
  geom_vline(xintercept = 0, color = "black", alpha = 0.3) +
  coord_fixed(ratio = 1) 



## ----eval = T, echo = F, fig = T, height = 4,png=TRUE------------------------------------
ggarrange(G28, G29, G30, ncol = 3, nrow = 1)


## ----eval = T, echo = F------------------------------------------------------------------
frequencies <- seq(0, 4.5, by = 0.01)
time <- seq(0, 4.5, by = 0.001)

freq <- function(x, y) (sin((6*pi)*x+pi/2)+1)*exp(-2*pi*(y)*x*(0+1i))

list <- lst()
for(i in seq(from = 0, to = 4.5, by = 0.01)) {list[100*i] <- mean(Re(freq(frequencies, i)))}

testing <- do.call(rbind.data.frame, list)

testing$x <- testing[,1]
testing[,1]<- NULL

testing <- as.data.frame(testing)

test <- cbind(seq(1, nrow(testing), by = 1), as.data.frame(testing$x))


## ----eval = T, echo = F, fig = T, height = 4,png=TRUE------------------------------------
ggplot(test,  aes(x = (seq(1, nrow(testing), by = 1)/100)+0.065, y = 2*testing$x)) + 
  geom_path(aes(color = "lightblue")) +
  xlab("Frequency") +
  ylab("Center of mass (x coordinate)") +
  geom_hline(yintercept = 0, color = "black", alpha = 0.3)  +
  geom_vline(xintercept = 0, color = "black", alpha = 0.3) +
  theme(legend.position = "none")


## ----eval = T, echo = F, hide = T--------------------------------------------------------
get_circle_coords <- function(r = 1, ...) {
  data_frame(theta = seq(0, 2 * pi, ...),
             x     = cos(theta) * r,
             y     = sin(theta) * r)
}

circ <- get_circle_coords(length.out = 200)
circ <- as.data.frame(circ)

library(dplyr)

circ$color <- ifelse(circ$y >= 0.0, ifelse(circ$x >= 0.5, "blue", "black"), "black")


## ----eval = T, echo = F, fig = T, height = 4, width = 4,png=TRUE-------------------------
ggplot(circ, aes(x = x, y = y)) +
  geom_path(aes(color = color)) +
  coord_fixed() +
  theme(legend.position = "none") +
  geom_segment(x = 0.5, y = 0, xend = 0.5, yend = sqrt(1-0.5^2), colour = "tomato") +
  geom_segment(x = 0, y = 0, xend = 0.5, yend = 0, colour = "tomato") +
  geom_segment(x = 0, y = 0, xend = 0.5, yend = sqrt(1-0.5^2), colour = "tomato") +
   geom_curve(aes(x = 1, y = 0, xend = 0.5, yend = sqrt(1-0.5^2), colour = "curve"), data = circ, curvature = 0.28) +
  geom_label(
    label=expression("sin"(theta)), 
    x=0.7,
    y=0.3,
    label.size = 0.1,
    fill="white"
  ) +
  geom_label(
    label=expression("cos"(theta)), 
    x=0.25,
    y=-0.15,
    label.size = 0.1,
    fill="white"
  ) +
  geom_hline(yintercept = 0, color = "black", alpha = 0.3)  +
  geom_vline(xintercept = 0, color = "black", alpha = 0.3) +
    geom_point(aes(x =0.5, y = sqrt(1-0.5^2)), col = "tomato", size = 3) +
  geom_point(aes(x =1, y = 0), col = "tomato", size = 3) +
  xlab("Re") +
  ylab("Im") 


## ----echo = F, eval = T------------------------------------------------------------------
set.seed(102)
acq.freq <- 1
time     <- 200
w        <- 2*pi/time
xx       <- seq(0,time,acq.freq)
trajectory <- 0.3*rnorm(201) + 3*sin(2*w*xx) + 1*sin(5*w*xx) + 2*sin(10*w*xx) 
T <- data.frame(trajectory)
TT <- data.frame(cbind(Time = seq(1, nrow(T), 1), Rate = trajectory))


## ----eval = T, echo = F, fig = T, height = 3,png=TRUE------------------------------------
ggplot(TT, aes(Time, Rate)) +
  geom_path(aes(color = "lightblue")) +
  geom_hline(yintercept = 0, color = "black", alpha = 0.3)  +
  geom_vline(xintercept = 0, color = "black", alpha = 0.3) +
  ylab("Change (% of opening price)") +
  xlab("Time (days)") + theme(legend.position = 'none         A')


## ----echo = F, eval = T------------------------------------------------------------------
df <- data.frame(Time = c(0, 1, 2, 3, 4, 5), Rate = data.frame(head(trajectory, n = 6)))
df$Change <- df$head.trajectory..n...6.
df$head.trajectory..n...6. <- NULL
df <- data.frame(df)


## ----first_table, echo=FALSE,results=tex, echo = F, figure = T,png=TRUE------------------
library(xtable)

print(xtable(df,
             caption="First six observations",
             label="table:df"),
      include.rownames=F,
      caption.placement="bottom"
  )


## ----eval = T, echo = F, hide = T--------------------------------------------------------
set.seed(102)
acq.freq <- 200
time     <- 1
w        <- 2*pi/time
xx       <- seq(0,time,1/acq.freq)
trajectory <- 0.2*rnorm(201) + 3*sin(2*w*xx) + 1*sin(5*w*xx) + 2*sin(10*w*xx) 
X.k <- fft(trajectory)

data123 <- data.frame(cbind(seq(1,nrow(data.frame(X.k)), 1),abs(X.k)/100))



## ----eval = T, echo = F, fig = T, height = 3,png=TRUE------------------------------------
ggplot(data123[2:201,], aes(X1-1, X2)) +
  geom_point(aes(color = "lightblue")) + 
  geom_segment(aes(x=X1-1, xend=X1-1, y=0, yend=X2)) +
  xlab("Frequency") +
  ylab("Amplitude") +
  theme(legend.position = "none")


## ----eval = T, echo = F, fig = T, height = 3,png=TRUE------------------------------------
ggplot(data123[2:100,], aes(X1-1, X2)) +
  geom_point(aes(color = "lightblue")) + 
  geom_segment(aes(x=X1-1, xend=X1-1, y=0, yend=X2)) +
  xlab("Frequency") +
  ylab("Amplitude") +
  theme(legend.position = "none")


## ----echo = F, eval = T------------------------------------------------------------------
set.seed(102)
acq.freq <- 200
time     <- 1
w        <- 2*pi/time
xx       <- seq(0,time,1/acq.freq)
YY <- 3*sin(2*w*xx) + 1*sin(5*w*xx) + 2*sin(10*w*xx) 
YY <- data.frame(YY)
Y <- data.frame(cbind(seq(1,nrow(YY),1),YY))

a <- ggplot(TT, aes(Time, Rate)) +
  geom_path(aes(color = "lightblue")) +
  geom_hline(yintercept = 0, color = "black", alpha = 0.3)  +
  geom_vline(xintercept = 0, color = "black", alpha = 0.3) +
  ylab("Change (% of opening price)") +
  xlab("Time (days)") +
  theme(legend.position = 'none')

b <- ggplot(Y, aes(seq.1..nrow.YY...1., YY)) +
  geom_path(aes(color = "lightblue")) +
  xlab("Time (days)") +
  ylab("'Change' (% of opening price)") +
  theme(legend.position = 'none')


## ----eval = T, echo = F, fig = T, height = 3,png=TRUE------------------------------------
ggarrange(b, a, ncol = 2, nrow = 1)

