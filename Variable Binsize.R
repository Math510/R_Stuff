############################################################################
# Netscape Valuation for IPO
############################################################################

#RevGrowth <- 0.65
#RandD <- 0.368
#RiskPrem <- 0.075

require(scales)
require(grid)
require(triangle)

Netscape <- function(RevG, ResDev, RiskP){
  
  TermGrowth <- 0.04
  CostSales <- 0.104
  TaxRate <- 0.34
  
  OperExp <- cbind(0.4, 0.35, 0.35, 0.35, 0.25, 0.25, 0.2, 0.2, 0.2, 0.2)
  CapExp <- cbind(0.25, 0.21, 0.21, 0.2, 0.1, 0.1, 0.1, 0.1, 0.1, 0.1)
  
  Dep <- 0.055
  NWC <- 0.00
  Beta <- 1.5
  Riskless <- 0.0671
  EquityCost <- Riskless + RiskP*Beta
  Shares <- 38000
  
  Revenues <- rep(33250, 10)
  CostofGoods <- rep(3472, 10)
  RandDExp <- rep(12230, 10)
  Deprec <- rep(1836, 10)
  OtherOperExp <- rep(26898, 10)
  ProfitBTaxes <- rep(-11186, 10)
  Taxes <- rep(-3803, 10)
  NetIncome <- rep(-7383, 10)
  
  CapitalExp <- rep(15236, 10)
  
  FreeCashFlow <- rep(-20783, 10)
  PVCash <- rep(-20783, 10)
  CumulCash <- rep(-20783, 10)
  
  for(i in 2:10){
    
    Revenues[i] <- (1 + RevG)*Revenues[i-1]
    CostofGoods[i] <- CostSales*Revenues[i]
    RandDExp[i] <- ResDev*Revenues[i]
    Deprec[i] <- Dep*Revenues[i]
    OtherOperExp[i] <- OperExp[i-1]*Revenues[i]
    ProfitBTaxes[i] <- Revenues[i] - CostofGoods[i] - RandDExp[i] - Deprec[i] - OtherOperExp[i]
    Taxes[i] <- TaxRate*ProfitBTaxes[i]
    NetIncome[i] <- ProfitBTaxes[i] - Taxes[i]
    
    CapitalExp[i] <- CapExp[i-1]*Revenues[i]
    
    FreeCashFlow[i] <- NetIncome[i] + Deprec[i] - CapitalExp[i]
    PVCash[i] <- FreeCashFlow[i]/((1 + EquityCost)^(i-1))
    CumulCash[i] <- CumulCash[i-1] + FreeCashFlow[i]
    
  }
  
  TermValue <- FreeCashFlow[10]*(1 + TermGrowth)/(EquityCost - TermGrowth)
  PVCashFlow <- sum(PVCash)
  PVTerminal <- TermValue/((1 + EquityCost)^10)
  TotalPV <- PVCashFlow + PVTerminal
  
  TotalNPV <- TotalPV
  Ratio <- PVTerminal/TotalPV
  YearPos <- ifelse(CumulCash > 0, 0, 1)
  Year <- sum(YearPos)
  MaxLoss <- min(CumulCash)
  SharePrice <- TotalPV/Shares
  
  results <- rbind(TotalNPV, Ratio, Year, MaxLoss, SharePrice)
  
  return(results)}

#Results2 <- Netscape(RevGrowth, RandD, RiskPrem)
#Results2

TotalNPV <- NULL
Ratio <- NULL
Year <- NULL
MaxLoss <- NULL
SharePrice <- NULL

for(i in 1:10000){
  
  set.seed(i)
  
  Results2 <- Netscape(rnorm(1,0.65,0.05), rtriangle(1,0.32,0.42,0.37), runif(1,0.05,0.1))
  TotalNPV <- data.frame(rbind(TotalNPV, Results2[[1]]))
  Ratio <- data.frame(rbind(Ratio, Results2[[2]]))
  Year <- data.frame(rbind(Year, Results2[[3]]))
  MaxLoss <- data.frame(rbind(MaxLoss, Results2[[4]]))
  SharePrice <- data.frame(rbind(SharePrice, Results2[[5]]))
  
}

MNPV <- mean(TotalNPV[,1])
MNPVLabel = paste("$",format(round(MNPV, digits=0), 
                             big.mark=",", 
                             big.interval=3))

hist_TotalNPV <- ggplot(TotalNPV, aes(x=TotalNPV[,1]))
hist_TotalNPV <- hist_TotalNPV + 
  geom_histogram(colour = 'blue',
                 fill = 'blue',
                 binwidth=50000) +
  labs(x="TotalNPV", y=NULL) +
  scale_x_continuous(labels = dollar) +
  geom_vline(xintercept=MNPV, colour='red') +
  annotate("text",x=MNPV,y=10,label=MNPVLabel,hjust=0, colour='white')
print(hist_TotalNPV)

MRatio <- mean(Ratio[,1])
MRLabel = sprintf("%3.2f ", MRatio)
hist_Ratio <- ggplot(Ratio, aes(x=Ratio[,1]))
hist_Ratio <- hist_Ratio + 
  geom_histogram(colour = 'gray',
                 fill = 'gray',
                 binwidth=.005
  ) +
  labs(x="Ratio", y=NULL) +
  geom_vline(xintercept=MRatio, colour='red') +
  annotate("text",x=MRatio,y=10,label=MRLabel,hjust=0) 
print(hist_Ratio)

hist_Year <- ggplot(Year, aes(x=Year[,1]))
hist_Year <- hist_Year + 
  geom_histogram(colour = 'black',
                 fill = 'black',
                 binwidth =.05
  ) +
  labs(x="Year", y=NULL) 
print(hist_Year)

MML <- mean(MaxLoss[,1])
MMLLabel = paste("$",format(round(MML, digits=0), 
                            big.mark=",", 
                            big.interval=3))

hist_MaxLoss <- ggplot(MaxLoss, aes(x=MaxLoss[,1]))
hist_MaxLoss <- hist_MaxLoss + 
  geom_histogram(colour = 'red',
                 fill = 'red',
                 binwidth=2000
  ) +
  labs(x="MaxLoss", y=NULL) + 
  scale_x_continuous(labels = dollar) +
  geom_vline(xintercept=MML, colour='black') +
  annotate("text",x=MML,y=10,label=MMLLabel,hjust=0)
print(hist_MaxLoss)

MSP <- mean(SharePrice[,1])
MSPLabel = sprintf("$%3.2f ", MSP)
hist_SharePrice <- ggplot(SharePrice, aes(x=SharePrice[,1]))
hist_SharePrice <- hist_SharePrice + 
  geom_histogram(colour = 'green',
                 fill = 'green',
                 binwidth=2.0
  ) +
  labs(x="SharePrice", y=NULL) +
  scale_x_continuous(labels = dollar) +
  geom_vline(xintercept=MSP, colour='red') +
  annotate("text",x=MSP,y=10,label=MSPLabel,hjust=0)
print(hist_SharePrice)


########################################################################
########################################################################
###Variable Binwidth###
########################################################################
########################################################################
MNPV <- mean(TotalNPV[,1])
bin20 <- (max(TotalNPV[,1])-min(TotalNPV[,1]))/20
bin50 <- (max(TotalNPV[,1])-min(TotalNPV[,1]))/50
bin100 <- (max(TotalNPV[,1])-min(TotalNPV[,1]))/100
MNPVLabel = paste("$",format(round(MNPV, digits=0), 
                             big.mark=",", 
                             big.interval=3))

hist_TotalNPV <- ggplot(TotalNPV, aes(x=TotalNPV[,1]))
hist_TotalNPV <- hist_TotalNPV + 
  geom_histogram(colour = 'blue',
                 fill = 'blue',
                 binwidth=50000) +
  labs(x="TotalNPV", y=NULL) +
  scale_x_continuous(labels = dollar) +
  geom_vline(xintercept=MNPV, colour='red') +
  annotate("text",x=MNPV,y=10,label=MNPVLabel,hjust=0, colour='white')
print(hist_TotalNPV)

hist_TotalNPV <- ggplot(TotalNPV, aes(x=TotalNPV[,1]))
hist_TotalNPV <- hist_TotalNPV + 
  geom_histogram(colour = 'blue',
                 fill = 'blue',
                 binwidth=bin20) +
  labs(x="TotalNPV", y=NULL) +
  scale_x_continuous(labels = dollar) +
  geom_vline(xintercept=MNPV, colour='red') +
  annotate("text",x=MNPV,y=10,label=MNPVLabel,hjust=0, colour='white')
print(hist_TotalNPV)

hist_TotalNPV <- ggplot(TotalNPV, aes(x=TotalNPV[,1]))
hist_TotalNPV <- hist_TotalNPV + 
  geom_histogram(colour = 'blue',
                 fill = 'blue',
                 binwidth=bin50) +
  labs(x="TotalNPV", y=NULL) +
  scale_x_continuous(labels = dollar) +
  geom_vline(xintercept=MNPV, colour='red') +
  annotate("text",x=MNPV,y=10,label=MNPVLabel,hjust=0, colour='white')
print(hist_TotalNPV)

hist_TotalNPV <- ggplot(TotalNPV, aes(x=TotalNPV[,1]))
hist_TotalNPV <- hist_TotalNPV + 
  geom_histogram(colour = 'blue',
                 fill = 'blue',
                 binwidth=bin100) +
  labs(x="TotalNPV", y=NULL) +
  scale_x_continuous(labels = dollar) +
  geom_vline(xintercept=MNPV, colour='red') +
  annotate("text",x=MNPV,y=10,label=MNPVLabel,hjust=0, colour='white')
print(hist_TotalNPV)
##################################################

MRatio <- mean(Ratio[,1])
MRLabel = sprintf("%3.2f ", MRatio)
hist_Ratio <- ggplot(Ratio, aes(x=Ratio[,1]))
hist_Ratio <- hist_Ratio + 
  geom_histogram(colour = 'gray',
                 fill = 'gray',
                 binwidth=.005
  ) +
  labs(x="Ratio", y=NULL) +
  geom_vline(xintercept=MRatio, colour='red') +
  annotate("text",x=MRatio,y=10,label=MRLabel,hjust=0) 
print(hist_Ratio)

hist_Year <- ggplot(Year, aes(x=Year[,1]))
hist_Year <- hist_Year + 
  geom_histogram(colour = 'black',
                 fill = 'black',
                 binwidth =.05
  ) +
  labs(x="Year", y=NULL) 
print(hist_Year)

MML <- mean(MaxLoss[,1])
MMLLabel = paste("$",format(round(MML, digits=0), 
                            big.mark=",", 
                            big.interval=3))

hist_MaxLoss <- ggplot(MaxLoss, aes(x=MaxLoss[,1]))
hist_MaxLoss <- hist_MaxLoss + 
  geom_histogram(colour = 'red',
                 fill = 'red',
                 binwidth=2000
  ) +
  labs(x="MaxLoss", y=NULL) + 
  scale_x_continuous(labels = dollar) +
  geom_vline(xintercept=MML, colour='black') +
  annotate("text",x=MML,y=10,label=MMLLabel,hjust=0)
print(hist_MaxLoss)


########################################################################
########################################################################
######LOOP with changing Bin Size##############################
########################################################################
########################################################################
binsizes <- c(3, 5, 20, 50, 100)
for(i in 1:length(binsizes)){
  bins <- (max(SharePrice[,1])-min(SharePrice[,1]))/binsizes[i]
  print(bins)
  MSP <- mean(SharePrice[,1])
  MSPLabel = sprintf("$%3.2f ", MSP)
  
  hist_SharePrice <- ggplot(SharePrice, aes(x=SharePrice[,1]))
  hist_SharePrice <- hist_SharePrice + 
    geom_histogram(colour = 'green',
                   fill = 'green',
                   binwidth=bins
    ) +
    labs(x="SharePrice", y=NULL) +
    scale_x_continuous(labels = dollar) +
    geom_vline(xintercept=MSP, colour='red') +
    annotate("text",x=MSP,y=10,label=MSPLabel,hjust=0)
  print(hist_SharePrice)
  
}

bin20 <- (max(SharePrice[,1])-min(SharePrice[,1]))/20
bin50 <- (max(SharePrice[,1])-min(SharePrice[,1]))/50
bin100 <- (max(SharePrice[,1])-min(SharePrice[,1]))/100
MSP <- mean(SharePrice[,1])
MSPLabel = sprintf("$%3.2f ", MSP)

hist_SharePrice <- ggplot(SharePrice, aes(x=SharePrice[,1]))
hist_SharePrice <- hist_SharePrice + 
  geom_histogram(colour = 'green',
                 fill = 'green',
                 binwidth=2.0
  ) +
  labs(x="SharePrice", y=NULL) +
  scale_x_continuous(labels = dollar) +
  geom_vline(xintercept=MSP, colour='red') +
  annotate("text",x=MSP,y=10,label=MSPLabel,hjust=0)
print(hist_SharePrice)

hist_SharePrice <- ggplot(SharePrice, aes(x=SharePrice[,1]))
hist_SharePrice <- hist_SharePrice + 
  geom_histogram(colour = 'green',
                 fill = 'green',
                 binwidth=bin20
  ) +
  labs(x="SharePrice", y=NULL) +
  scale_x_continuous(labels = dollar) +
  geom_vline(xintercept=MSP, colour='red') +
  annotate("text",x=MSP,y=10,label=MSPLabel,hjust=0)
print(hist_SharePrice)

hist_SharePrice <- ggplot(SharePrice, aes(x=SharePrice[,1]))
hist_SharePrice <- hist_SharePrice + 
  geom_histogram(colour = 'green',
                 fill = 'green',
                 binwidth=bin50
  ) +
  labs(x="SharePrice", y=NULL) +
  scale_x_continuous(labels = dollar) +
  geom_vline(xintercept=MSP, colour='red') +
  annotate("text",x=MSP,y=10,label=MSPLabel,hjust=0)
print(hist_SharePrice)

hist_SharePrice <- ggplot(SharePrice, aes(x=SharePrice[,1]))
hist_SharePrice <- hist_SharePrice + 
  geom_histogram(colour = 'green',
                 fill = 'green',
                 binwidth=bin100
  ) +
  labs(x="SharePrice", y=NULL) +
  scale_x_continuous(labels = dollar) +
  geom_vline(xintercept=MSP, colour='red') +
  annotate("text",x=MSP,y=10,label=MSPLabel,hjust=0)
print(hist_SharePrice)