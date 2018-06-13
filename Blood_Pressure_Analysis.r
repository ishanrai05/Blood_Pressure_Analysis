
library(cosinor)
library(ggplot2)
library(readxl)
library(psych)

my_data <- read_excel("E:/Arun/New folder/Project.xlsx")

summary(my_data)

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

#Time=c(1,2,3,4,24)     #add more times 
#Rectal=c(33.8,37.6,37.1,35.5,38.2)  #add more temperatures 
#cosinor(Time,Rectal)
#timeTemp <- data.frame(Time,Rectal)
#cosinor.plot("Time","Rectal",data=timeTemp)

#str(vitamind)
#cosinor.plot("time","X",data=timeTemp2,)
#fit <- cosinor.lm(Time_hours ~ time(Time_hours) + Average_SBP+ amp.acro(Average_SBP), data = my_data)
#cosinor_analyzer(data = vitamind)
#str(vitamind)

ggplot(my_data , aes(x=Time_hours, y=Average_SBP)) + geom_line()

ggplot(my_data , aes(x=Time_hours, y=Average_SBP)) + geom_point() + geom_smooth( se = TRUE, method = "lm") + ggtitle("Systolic Blood Pressure") + xlab("Time in hours")

ggplot(my_data , aes(x=Average_SBP)) + geom_histogram()

ggplot(my_data , aes(x=Average_SBP)) + geom_density( color = "RED")

cosinor(time,Average_SBP)

timeTemp2 <- data.frame(time,Average_SBP)
summary(timeTemp2)

cosinor.plot("time","Average_SBP",data=timeTemp2)


#library(cosinor)
#fit <- cosinor.lm(Y ~ time(time) + X + amp.acro(X), data = vitamind, period = 12)
#summary(fit)
#test_cosinor(fit, "X", param = "amp")
#summary(vitamind$Y)
#summary(predict(fit))
#library(ggplot2)
#summary(vitamind)
#ggplot.cosinor.lm(fit, x_str = "X") 
#(ggplot(vitamind , aes(x=X, y=time)) + geom_point())


#predict(fit)

ggplot(my_data , aes(x=Time_hours, y=Average_DBP)) + geom_line()

ggplot(my_data , aes(x=Time_hours, y=Average_DBP)) + geom_point() + geom_smooth( se = TRUE, method = "lm") + ggtitle("Dialstolic Blood Pressure") + xlab("Time in hours")

ggplot(my_data , aes(x=Average_DBP)) + geom_histogram()

ggplot(my_data , aes(x=Average_DBP)) + geom_density( color = "BLUE")

cosinor(time,Average_DBP)

timeTemp3 <- data.frame(time,Average_DBP)
summary(timeTemp3)

cosinor.plot("time","Average_DBP",data=timeTemp3)

ggplot(my_data , aes(x=Time_hours, y=Average_PulseR)) + geom_line()

ggplot(my_data , aes(x=Time_hours, y=Average_DBP)) + geom_point() + geom_smooth( se = TRUE, method = "lm") + ggtitle("Pulse Rate") + xlab("Time in hours")

ggplot(my_data , aes(x=Average_PulseR)) + geom_histogram()

cosinor(time,Average_PulseR)

timeTemp3 <- data.frame(time,Average_PulseR)
summary(timeTemp3)

cosinor.plot("time","Average_PulseR",data=timeTemp3)

#Correlation
ggplot(my_data , aes(x=Average_DBP, y=Average_SBP)) + geom_point() + geom_smooth( se = TRUE, method = "lm")

ggplot(my_data , aes(x=Average_SBP, y=Average_DBP)) + geom_point() + geom_smooth( se = TRUE, method = "lm")

str(time)
str(Average_SBP)
#data frame
#time<- c(2, 6, 10, 18, 22, 26, 30, 34, 38)
#resp<- c(2.54, 0.13, -0.38, -0.57,  0.11, -0.20, -0.26, -0.62, -0.73)
df<-data.frame(time,Average_SBP)
head(df)
#summary(my_data$Time)

# data fits both linear and cosinor models (but not very well)
#cos.model <- cosinor.lm(time ~ time(time), period=24, data=df)
#fit <- cosinor.lm(time ~ time(time) + X + amp.acro(Average_SBP), data = df)

#summary(cos.model)
#l.model <- lm(resp ~ time, data=df)
#summary(l.model)

#plot with a loess smoother looks like a combination of the models 
#plot to see fit

#ggplot(df, aes(x = time, y = resp))+
#  geom_point()+
#  geom_smooth(method = "lm", se = FALSE)

#fit <- cosinor.lm(time ~ time(time) + X + amp.acro(Average_SBP), data = df, period = 12)
#summary(fit)
#test_cosinor(fit, "Average_SBP", param = "amp")
#summary(vitamind$Y)
#summary(predict(fit))
#ggplot.cosinor.lm(fit, x_str = "X") 
#(ggplot(vitamind , aes(x=X, y=time)) + geom_point())

