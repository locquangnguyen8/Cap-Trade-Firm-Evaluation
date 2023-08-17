# Load data and package
library("readxl")
library(bayesvl)
setwd("~/Desktop/ClimateChange-Business")
dat <- read_excel("SP500.xlsx")
colnames(dat) 
keeps <- c("AverageStockPrice","LogNetIncome", "EmissionsTrading","LogCO2")
dat <- dat[keeps]
dat<-na.omit(dat)
data_num <- as.data.frame(apply(dat, 2, as.numeric))
sapply(data_num, class)
attach(data_num)
library(bayesvl)
library(cowplot)
library(ggplot2)

# Model construction:
model2<-bayesvl()
model2<-bvl_addNode(model2,"AverageStockPrice","norm")
model2<-bvl_addNode(model2,"LogNetIncome","norm")
model2<-bvl_addNode(model2,"EmissionsTrading","binom")
model2 <- bvl_addNode(model2, "LogCO2", "norm")

model2<-bvl_addNode(model2,"Income_EmissionsTrading","trans")
model2<-bvl_addNode(model2,"Income_CO2_EmissionsTrading","trans")
model2<-bvl_addNode(model2,"CO2_EmissionsTrading","trans")
model2<-bvl_addNode(model2,"Income_CO2","trans")

model2<-bvl_addArc(model2,"LogNetIncome","Income_CO2","*")
model2<-bvl_addArc(model2,"LogCO2","Income_CO2","*")

model2<-bvl_addArc(model2,"LogNetIncome","Income_EmissionsTrading","*")
model2<-bvl_addArc(model2,"EmissionsTrading","Income_EmissionsTrading","*")

model2<-bvl_addArc(model2,"LogNetIncome","Income_CO2_EmissionsTrading","*")
model2<-bvl_addArc(model2,"LogCO2","Income_CO2_EmissionsTrading","*")
model2<-bvl_addArc(model2,"EmissionsTrading","Income_CO2_EmissionsTrading","*")

model2<-bvl_addArc(model2,"LogCO2","CO2_EmissionsTrading","*")
model2<-bvl_addArc(model2,"EmissionsTrading","CO2_EmissionsTrading","*")

model2<-bvl_addArc(model2,"LogNetIncome","AverageStockPrice","slope")
model2<-bvl_addArc(model2,"Income_EmissionsTrading","AverageStockPrice","slope")
model2<-bvl_addArc(model2,"EmissionsTrading","AverageStockPrice","slope")
model2<-bvl_addArc(model2,"Income_CO2_EmissionsTrading","AverageStockPrice","slope")
model2<-bvl_addArc(model2,"CO2_EmissionsTrading","AverageStockPrice","slope")
model2<-bvl_addArc(model2,"LogCO2","AverageStockPrice","slope")
model2<-bvl_addArc(model2,"Income_CO2","AverageStockPrice","slope")
bvl_bnPlot(model2)

# Generate Stan code
model_string2<- bvl_model2Stan(model2)
cat(model_string2) 

# Model Fit
model2<-bvl_modelFit(model2, data_num, warmup = 2000, iter = 5000, chains = 4,cores = 4)
summary(model2)
bvl_plotIntervals(model2,c("b_Income_CO2_AverageStockPrice","b_EmissionsTrading_AverageStockPrice","b_LogCO2_AverageStockPrice","b_CO2_EmissionsTrading_AverageStockPrice","b_Income_CO2_EmissionsTrading_AverageStockPrice","b_LogNetIncome_AverageStockPrice","b_Income_EmissionsTrading_AverageStockPrice"
                           ))+theme_bw()+geom_vline(xintercept = 0,color = "red", size=1)
# To plot diagnostic plots
bvl_plotTrace(model2)
bvl_plotGelman(model2)
bvl_plotAcfs(model2,3,3,param=NULL)
loo2<-bvl_stanLoo(model2)
plot(loo2)

# To plot posterior distribution plots
bvl_plotDensity(model2)+theme_bw()
bvl_plotParams(model2,3,3,credMass = 0.89,params = NULL)
bvl_plotDensity2d(model2,"b_CO2_EmissionsTrading_AverageStockPrice","b_Income_EmissionsTrading_AverageStockPrice")+theme_bw()

#Model 1
model1<-bayesvl()
model1<-bvl_addNode(model1,"LogNetIncome","norm")
model1<-bvl_addNode(model1,"EmissionsTrading","binom")
model1<-bvl_addArc(model1,"EmissionsTrading","LogNetIncome","slope")
bvl_bnPlot(model1)

# Generate Stan code
model_string1<- bvl_model2Stan(model1)
cat(model_string1) 

# Model Fit
model1<-bvl_modelFit(model1, data_num, warmup = 2000, iter = 5000, chains = 4,cores = 4)
summary(model1)
bvl_plotIntervals(model1,c("b_EmissionsTrading_LogNetIncome"))+theme_bw()+geom_vline(xintercept = 0,color = "red", size=1)
# To plot diagnostic plots
bvl_plotTrace(model1)
bvl_plotGelman(model1)
bvl_plotAcfs(model1,param=NULL)
loo1<-bvl_stanLoo(model1)
plot(loo1)

