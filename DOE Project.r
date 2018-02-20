twolevel3 <- read.csv(file="doe.csv", header=TRUE, sep=",")


# Use regression
head(twolevel3)
twolevel3.reg.model = lm(Inventory ~ Batch*Reorder* Target ,data=twolevel3)
m1 <- lm(Inventory ~ Batch+Reorder+Target+Batch*Reorder+Batch*Target + Reorder*Target + Batch*Target*Reorder,data = twolevel3)
summary(twolevel3.reg.model)
summary(m1)
plot(twolevel3.reg.model)

anova(twolevel3.reg.model)

# Use FrF2
install.packages("FrF2")
library(FrF2)
d <- read.csv("doe1.csv")
plan32design <- FrF2(8,3,factor.names = list(Batch = "",Reorder = "",Target = "")) 
plan32design

plan32design.resp= add.response(plan32design,d$Inventory)
plan32design.resp.lm = lm('d$Inventory~ d$Batch*d$Reorder*d$Target',plan32design.resp)
summary(plan32design.resp.lm)
anova(plan32design.resp.lm)
# Effect plot
MEPlot(plan32design.resp)
IAPlot(plan32design.resp)
# You can also use half-normal plot
DanielPlot(plan32design.resp, code = TRUE, half = TRUE) 
# Result slightly different from previous regression/ANOVA approach


#Creating regression model
library(ggplot2)
library(corrplot)
data <- read.csv("doe.csv")
colnames(data)
m1 <- lm(Inventory ~ Batch + Reorder + Target, data=data )
summary(m1)
plot(data$Batch,data$Inventory,data= data)
plot(data$Reorder,data$Inventory)
plot(data$Target,data$Inventory)
null <- lm(Inventory~1,data=data)
full <- lm(Inventory~ . ,data = data)
m2<-step(null,scope = list(lower=null,upper=full),direction = "forward")
summary(m2)
m3 <-step(full, data=data, direction="backward")
summary(m3)
m4<-step(null, scope = list(upper=full), data=data, direction="both")
summary(m4)
m5 <- lm(Inventory ~ Reorder + Target + Reorder*Target,data = data)
summary(m5)
Inven <- regsubsets(Inventory ~ Batch + Reorder + Target ,data=data,nbest = 10)
plot(Inven, scale="adjr2")
plot(Inven, scale="bic")
plot(m4$residuals,m4$fitted.values)

