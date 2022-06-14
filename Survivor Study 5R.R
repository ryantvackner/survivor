##*
##*Survivor Study 5
##*Ryan Vackner 
##*October 26th, 2021
##*
library(readr)
library(stargazer)
library(jtools)
library(ggplot2)

#my data
mydat = read.csv("C:/Users/ryanv/Desktop/Survivor Study/Survivor Study 5.csv", header = TRUE)

#descriptive statistics using stargazer
stargazer(mydat, type = 'text')

#estimate regressions
reg1 = lm(Finish ~ Sex + Age, data = mydat)
reg2 = lm(Finish ~ InRCW + InICW + TRCW + TICW + Idol + factor(Season), data = mydat)
reg3 = lm(Finish ~ Sex + Age + InRCW + InICW + TRCW + TICW + Idol + factor(Season), data = mydat)

summ(reg1, digits = getOption("jtools-digits", 4))
summ(reg2, digits = getOption("jtools-digits", 4))
summ(reg3, digits = getOption("jtools-digits", 4))

#plot the data: Days on the vertical; Age on the horizontal
plot(x=mydat$Age, y=mydat$Finish, xlab="Age", ylab="Days",
     main="Scatter Plot of Player Age and Days on Survivor", pch=21, bg="blue")

#plot the data: Days on the vertical; InRCW on the horizontal
plot(x=mydat$InRCW, y=mydat$Finish, xlab="Ind Reward Challenge Win", ylab="Days",
     main="Scatter Plot of Ind Reward Challenge Wins and Days on Survivor", pch=21, bg="red")

#plot the data: Days on the vertical; InICW on the horizontal
plot(x=mydat$InICW, y=mydat$Finish, xlab="Ind Immunity Challenge Win", ylab="Days",
     main="Scatter Plot of Ind Immunity Challenge Wins and Days on Survivor", pch=21, bg="green")

#plot the data: Days on the vertical; TRCW on the horizontal
plot(x=mydat$TRCW, y=mydat$Finish, xlab="Tribal Reward Challenge Win", ylab="Days",
     main="Scatter Plot of Tribal Reward Challenge Wins and Days on Survivor", pch=21, bg="cyan")

#plot the data: Days on the vertical; TICW on the horizontal
plot(x=mydat$TICW, y=mydat$Finish, xlab="Tribal Immunity Challenge Win", ylab="Days",
     main="Scatter Plot of Tribal Immunity Challenge Wins and Days on Survivor", pch=21, bg="orange")

#plot the data: Days on the vertical; Idols on the horizontal
plot(x=mydat$Idol, y=mydat$Finish, xlab="Idols", ylab="Days",
     main="Scatter Plot of Idols and Days on Survivor", pch=21, bg="blue")