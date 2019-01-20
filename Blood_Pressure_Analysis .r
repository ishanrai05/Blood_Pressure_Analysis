
# install.packages("cosinor")
# install.packages("ggplot2")
# install.packages("readxl")
# install.packages("psych")

library(cosinor)
library(ggplot2)
library(readxl)
library(psych)

my_data <- read_excel("Project.xlsx")

summary(my_data)

head(my_data)

my_data[,1] -> time
my_data[,2] -> time_
my_data[,3] -> Average_SBP
my_data[,4] -> Average_DBP
my_data[,5] -> Average_PulseR

unlist(time, use.names=FALSE) -> time
unlist(Average_SBP, use.names=FALSE) -> Average_SBP
unlist(Average_DBP, use.names=FALSE) -> Average_DBP
unlist(Average_PulseR, use.names=FALSE) -> Average_PulseR

summary (Average_SBP)
summary (Average_DBP)
summary (Average_PulseR)

p = ggplot(my_data , aes(x=Time_hours)) +  geom_line(aes( y=Average_SBP), color = "blue", size = 0.8) + geom_line(aes( y=Average_DBP), color = "blue", size = 0.8) + geom_line(aes( y=Average_PulseR), color = "blue", size = 0.8)
# p1 = p + geom_point(aes( y=Average_DBP), color = "red") + geom_point(aes(y = Average_SBP), color="blue")
p2 = p + scale_x_continuous(breaks=seq(0,30,1)) + scale_y_continuous(breaks=seq(0,150,15)) + expand_limits(x=10, y=0)
p3=p2 + coord_fixed(ratio=1/18)
p3 + geom_segment(aes(x=Time_hours, xend=Time_hours, y=Average_SBP, yend=Average_DBP), 
        size=0.5, data=my_data, colour="blue")




timeTemp2 <- data.frame(time,Average_SBP)
summary(timeTemp2)

ggplot(my_data , aes(x=Time_hours)) + geom_line(aes( y=Average_SBP), color = "blue") + geom_line(aes( y=Average_DBP), color = "grey") + geom_point(aes( y=Average_DBP), color = "grey") 

ggplot(my_data , aes(x=Average_SBP)) + geom_histogram()

ggplot(my_data , aes(x=Average_SBP)) + geom_density( color = "RED")

# fitting a linear model
ggplot(my_data , aes(x=Time_hours, y=Average_SBP)) + geom_point() + geom_smooth( se = TRUE, method = "lm") + ggtitle("Systolic Blood Pressure") + xlab("Time in hours")


#predict(fit)

timeTemp3 <- data.frame(time,Average_DBP)
summary(timeTemp3)

ggplot(my_data , aes(x=Time_hours, y=Average_DBP)) + geom_line()

ggplot(my_data , aes(x=Average_DBP)) + geom_histogram()

ggplot(my_data , aes(x=Average_DBP)) + geom_density( color = "BLUE")

# fitting a linear model
ggplot(my_data , aes(x=Time_hours, y=Average_DBP)) + geom_point() + geom_smooth( se = TRUE, method = "lm") + ggtitle("Dialstolic Blood Pressure") + xlab("Time in hours")


timeTemp3 <- data.frame(time,Average_PulseR)
summary(timeTemp3)

ggplot(my_data , aes(x=Time_hours, y=Average_PulseR)) + geom_line()

ggplot(my_data , aes(x=Average_PulseR)) + geom_histogram()
ggplot(my_data , aes(x=Time_hours, y=Average_DBP)) + geom_point() + geom_smooth( se = TRUE, method = "lm") + ggtitle("Pulse Rate") + xlab("Time in hours")


# Correlation between DBP and SBP
ggplot(my_data , aes(x=Average_DBP, y=Average_SBP)) + geom_point() + geom_smooth( se = TRUE, method = "lm")

ggplot(my_data , aes(x=Average_SBP, y=Average_DBP)) + geom_point() + geom_smooth( se = TRUE, method = "lm")

df<-data.frame(time,Average_SBP,Average_DBP)
tail(df)

fit <- cosinor.lm(time ~ time(time) + Average_SBP + amp.acro(Average_SBP), data = df)
summary(fit)
test_cosinor(fit, "Average_SBP", param = "amp")
summary(predict(fit))
# ggplot.cosinor.lm(fit, x_str = "Average_SBP") + coord_fixed(ratio=1/25)
# ggplot(df , aes(x=time, y=Average_SBP)) + geom_point()

# fitting cosinor curve to average dystolic blood pressure data
fit <- cosinor.lm( Average_SBP ~ time(time), period=15.4, data = df)
a = ggplot(fit) + geom_point(data=df , aes(x=time, y=Average_SBP), size = 0.7)
a = a + scale_x_continuous(breaks=seq(0,15,1)) + scale_y_continuous(breaks=seq(0,150,15))
a = a + expand_limits(x=10, y=0) + coord_fixed(ratio=1/20)
a



# fitting cosinor curve to average dystolic blood pressure data
fit2 <- cosinor.lm(Average_DBP ~ time(time), period=15.4, data = df)
b = ggplot(fit2) 
b = b + coord_fixed(ratio=1/20)
b = b + scale_x_continuous(breaks=seq(0,15,1)) + scale_y_continuous(breaks=seq(0,150,15))
b = b + geom_point(data=df , aes(x=time, y=Average_DBP), size = 0.7)
b + expand_limits(x=10, y=0)
