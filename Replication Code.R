

# Replication code for:

#The impact of increased proximity from an asylum centre on the Swiss 
#anti-immigration party's voting share


#### CONTENTS 

#### --------------------

### 2) Institutional Background
## 2.2) Asylum centres 
### 5) Data and descriptive statistics
## 5.2) Descriptive statistics 
### 6) Main results and discussion 
## 6.1) Common trend assumption 
## 6.2) Main Results 
## 6.3) Robustness check
## 6.4) Heterogeneous effect


### 2) Intstitutional Background

## 2.1 Asylum centres
# ------------------
library(foreign)
library(ggplot2)
library(dplyr)
ddpath  <- "C:/..."
wdpath  <- "C:/..."
setwd(wdpath)
wdf <- read.csv2(file(paste(ddpath, "Asylum Centres.csv",sep=""), 
                      encoding="latin1"), 
                 header=T, sep=",", dec=".", na.strings="NA")

# Replace NA with 0
wdf[is.na(wdf)] <- 0

wdf1 <- c("Canton", "Nb_Centres", "Year", "Nb_Centres_m", "Month")
wdf1 <- wdf[wdf1]
wdf1 <- wdf1[which(wdf1$Year > 0),]


# Fig 1: links. Number of asylum centres opened year by year
ggplot(wdf1, aes(fill= Canton, y=Nb_Centres, x=Year)) + 
    geom_bar(position="stack", stat="identity", width =0.6) +
  scale_x_continuous(name = "Year",
                     breaks = seq(2010, 2020, 1),
                     limits=c(2009, 2021),
                     labels=c("2010" = "23.10.11", "2011" = "31.12.11" , "2012" = "31.12.12" , "2013" = "31.12.13" , "2014" = "31.12.14" , "2015" = "18.10.15" , "2016" = "31.12.15" , "2017" = "31.12.16" , "2018" = "31.12.17" , "2019" = "31.12.18" , "2020" = "31.12.19")) +
   scale_y_continuous(name = "Asylum centres",
                      breaks = seq(0, 300, 50)
                      ) +
   scale_fill_manual(values=c("pink", "aquamarine", "bisque", "brown", "burlywood", "cadetblue", "chartreuse", "chocolate", "coral", "cornsilk", "cyan", "darkcyan", "darkgoldenrod1", "darkgray", "darkkhaki", "darkolivegreen1", "darkorange", "darkorchid", "darksalmon", "darkseagreen", "darkslategray1", "deeppink3", "deepskyblue1", "dodgerblue3", "firebrick3", "goldenrod2"))+
   ggtitle("Number of asylum centres per year") +
  geom_vline(xintercept = 2015, colour ="black") +
  geom_vline(xintercept = 2010) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle=60, vjust=1, hjust = 1)) 



# Fig. A5: Cantonal  share  of  asylum  centres.
ggplot(wdf1, aes(fill= Canton, y=Nb_Centres, x=Year)) + 
  geom_bar(position="fill", stat="identity", width =0.6) +
  scale_x_continuous(name = "Year",
                     breaks = seq(2010, 2020, 1),
                     limits=c(2009, 2021),
                     labels=c("2010" = "23.10.2011", "2011" = "31.12.2011" , "2012" = "31.12.2012" , "2013" = "31.12.2013" , "2014" = "31.12.2014" , "2015" = "18.10.2015" , "2016" = "31.12.2015" , "2017" = "31.12.2016" , "2018" = "31.12.2017" , "2019" = "31.12.2018" , "2020" = "31.12.2019")) +
  scale_y_continuous(name = "% Number Centres",
                     breaks = seq(0, 5, 0.1)                     

  ) +
  scale_fill_manual(values=c("pink", "aquamarine", "bisque", "brown", "burlywood", "cadetblue", "chartreuse", "chocolate", "coral", "cornsilk", "cyan", "darkcyan", "darkgoldenrod1", "darkgray", "darkkhaki", "darkolivegreen1", "darkorange", "darkorchid", "darksalmon", "darkseagreen", "darkslategray1", "deeppink3", "deepskyblue1", "dodgerblue3", "firebrick3", "goldenrod2"))+
  ggtitle("% Number Centres per Year") +
  coord_flip() +
  geom_vline(xintercept = 2015, colour ="black") +
  geom_vline(xintercept = 2010) + 
  theme_minimal()


# Fig 1: right. Number of asylum centres opened month by month
ggplot(wdf1, aes(fill= Canton, y=Nb_Centres_m, x=Month)) + 
  geom_bar(position="stack", stat="identity", width =0.6) +
  scale_x_continuous(name = "Month",
                     breaks = seq(2010, 2026, 1),
                     limits=c(2009, 2027),
                     labels=c("2010" = "01.15", "2011" = "02.15" , "2012" = "03.15" , "2013" = "04.15" , "2014" = "05.15" , "2015" = "06.15" , "2016" = "07.15" , "2017" = "08.15" , "2018" = "09.15" , "2019" = "10.15" , "2020" = "11.15" , "2021" = "12.15" , "2022" = "01.16" , "2023" = "02.16", "2024" = "03.16", "2025" = "04.16", "2026" = "05.16")) +
  scale_y_continuous(name = "Asylum centres",
                     breaks = seq(-20, 30, 1)
  ) +
  scale_fill_manual(values=c("pink", "aquamarine", "bisque", "brown", "burlywood", "cadetblue", "chartreuse", "chocolate", "coral", "cornsilk", "cyan", "darkcyan", "darkgoldenrod1", "darkgray", "darkkhaki", "darkolivegreen1", "darkorange", "darkorchid", "darksalmon", "darkseagreen", "darkslategray1", "deeppink3", "deepskyblue1", "dodgerblue3", "firebrick3", "goldenrod2"))+
  ggtitle("Number of asylum centres per month") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle=60, vjust=1, hjust = 1)) 




### 5) Data and Descriptive statistics 

## 5.2) Descriptive statistics 

library(doBy)
library(xtable)

wdf <- read.csv2(file(paste(ddpath, "Data Municipalities Distance, SVP, Covariates.csv", sep=""), 
                       encoding="latin1"), 
                  header=T, sep=",", dec=".", na.strings="NA")

# Data panel
wdf <-subset(wdf, Year=="2011" | Year=="2015")
wdf <-subset(wdf, Year=="2011")


# Dummy variables for Typology and Language
wdf$Rural = ifelse(wdf$Typology == "Rural", 1, 0)
wdf$Urban = ifelse(wdf$Typology == "Urbain", 1, 0)
wdf$Intermediate = ifelse(wdf$Typology == "Intermediaire", 1, 0)

wdf$German = ifelse(wdf$Language == "de", 1 ,0) 
wdf$French = ifelse(wdf$Language == "fr", 1 ,0) 
wdf$Italian = ifelse(wdf$Language == "it", 1 ,0) 
wdf$Romansh = ifelse(wdf$Language == "rm", 1 ,0) 
wdf$Distance = (wdf$Distance)/1000
wdf$time = ifelse(wdf$Year == 2015, 1, 0)
wdf$treated = ifelse(wdf$Diffdistance > 0.000000001, 1, 0)


# Table 1: Descriptive statistics

wdf1 <-subset(wdf[,c("Distance" , "treated", "SVP", "Population", "Density", "GDPcap",  
                     "Age20.64" , "Foreigners" , "TertiaryDegree" , "Unemployment" ,  
                     "Rural", "Intermediate" , "Urban" ,  "Surface" , "German" , "French" , "Italian" , "Romansh"  
                     )]) #  "time" #

# Choice between Switzerland as a whole, German-speaking, French-speaking, Italian-speaking 
wdf2 <-subset(wdf1, German =="1") # French, Italian

a<- summaryBy(. ~ treated, data=wdf2, FUN=c(mean), keep.names=TRUE) # na.rm=TRUE, ,  # or wdf1
t(a)
xtable(t(a)) # each part is assembled together in Latex 

wdf_cov <- c("Distance" , "SVP", "Population", "Density", "GDPcap",  
             "Age20.64" , "Foreigners" , "TertiaryDegree" , "Unemployment" ,  
             "Rural", "Intermediate" , "Urban" ,  "Surface") #, "German" , "French" , "Italian" , "Romansh")

wdf2 %>% # or wdf1
  group_by(treated) %>%
  select(one_of(wdf_cov)) %>%
  summarise_all(list(~ mean(., na.rm = TRUE)))
#  summarise_all(funs(mean(., na.rm = T)))

# T-tests to evaluate whether these means are statistically distinguishable
# the smaller the p-value, the stronger the evidence is that the two populations have different means.
lapply(wdf_cov, function(v) {
t.test(wdf2[, v] ~ wdf2[, "treated"])
})

# Switzerland
# p-value < 2.2e-16 0.01535 0.7526 0.5699 0.1128 0.7724 0.01021 0.004992 0.1638 0.9265 0.3898 0.3976 0.1504 9.108e-06 2.229e-08  0.7597 0.01217

# german-speaking Switzerland 
# p-value 2.2e-16 0.9623 0.627 0.9021 0.3991 0.3512 0.252 0.08214 0.1105 0.3749 0.002739 0.03379 0.09045

# french-speaking Switzerland 
# p-value 2.754e-14 0.07162 0.3905 0.02036 0.0005341 0.4506 6.312e-06 8.723e-07 0.5968 0.01142 0.01413 0.6035 0.7404

# italian-speaking Switzerland
# p-value 3.093e-11 0.5684 0.8353 0.02039 0.1223 0.8368 0.0524 0.0003444 0.01022 0.02538 0.3619 0.000364 0.05743 


# Switzerland
# p-value <0.01 0.015 0.752 0.57 0.113 0.772 0.01 <0.01 0.164 0.927 0.39 0.398 0.15 <0.01 <0.01  0.76 0.012

# german-speaking Switzerland 
# p-value <0.01 0.962 0.627 0.902 0.34 0.351 0.252 0.082 0.111 0.375 <0.01 0.034 0.09

# french-speaking Switzerland 
# p-value <0.01 0.072 0.39 0.02 <0.01 0.45 <0.01 <0.01 0.597 0.011 0.014 0.603 0.74

# italian-speaking Switzerland
# p-value <0.01 0.568 0.835 0.02 0.122 0.837 0.052 <0.01 0.01 0.025 0.362 <0.01 0.057 


### 6) Results and Discussion         # I assemble the output for each code in Latex

## 6.1) Common trend assumption 
library(ggplot2)

# CTA Graph
wdfS1 <- read.csv2(file(paste(ddpath, "Data Municipalities Distance, SVP, Covariates.csv", sep=""), 
                        encoding="latin1"), 
                   header=T, sep=",", dec=".", na.strings="NA")

# Treatment
wdfS1$treated = ifelse(wdfS1$Diffdistance > 0.0000001, 1, 0) # wdf$Diffdistance < - 0.0000001 | 

# OPTIONS: language
# Switzerland as a whole (), German (de), French (fr) or Italian (it)
wdfS1$French = ifelse(wdfS1$Language == "it", 1, 0)
wdfS <-subset(wdfS1, French ==  1)

# 1999
wdf99 <-subset(wdfS, Year=="1999")
TP1 <- lm(SVP ~ treated, 
          data = wdf99)

# 2003
wdf03 <-subset(wdfS, Year=="2003")
TP2 <- lm(SVP ~ treated, 
          data = wdf03)

# 2007
wdf07 <-subset(wdfS, Year=="2007")
TP3 <- lm(SVP ~ treated, 
          data = wdf07)

# 2011
wdf11 <-subset(wdfS, Year=="2011")
TP4 <- lm(SVP ~ treated, 
          data = wdf11)

# 2015
wdf15 <-subset(wdfS, Year=="2015")
TP5 <- lm(SVP ~ treated, 
          data = wdf15)

# 2019
wdf19 <-subset(wdfS, Year=="2019")
TP6 <- lm(SVP ~ treated, 
          data = wdf19)

# Results 
coeftest(TP1,  vcov = vcovHC, type = "HC1", cluster="group")
coeftest(TP2,  vcov = vcovHC, type = "HC1", cluster="group")
coeftest(TP3,  vcov = vcovHC, type = "HC1", cluster="group")
coeftest(TP4,  vcov = vcovHC, type = "HC1", cluster="group")
coeftest(TP5,  vcov = vcovHC, type = "HC1", cluster="group")
coeftest(TP6,  vcov = vcovHC, type = "HC1", cluster="group")


### Fig. 3. CTA Difference between treatment and control
Share_SVP <- c(-2.1689, -2.26153, -2.62313  , -1.43021 ,  -2.20168 , 0.13269 ,
               0.95142 , 0.054456  , -0.070968  , -0.03171, 0.33804 , -1.4697e-01,
               -0.53965 , -1.51419 , -2.36183 ,  -1.48676 , -3.13251 ,  -2.41127 ,
               -0.28356  , -0.053949 ,   0.41916 ,  0.87326 , 2.0857 , 0.88241 )

Year <- c(1999, 2003, 2007, 2011, 2015, 2019, 1999, 2003, 2007, 2011, 2015, 2019)

Language <- c("ALL" , "ALL", "ALL", "ALL", "ALL", "ALL" , "GERMAN", "GERMAN", "GERMAN", "GERMAN", "GERMAN", "GERMAN", "FRENCH", "FRENCH", "FRENCH", "FRENCH", "FRENCH", "FRENCH" , "ITALIAN", "ITALIAN","ITALIAN","ITALIAN","ITALIAN","ITALIAN")
df <- data.frame(Year, Share_SVP, Language)


ggplot(df, aes(x = Year, y = Share_SVP, color = Language)) + stat_summary(geom = 'line') +  
  scale_x_continuous(name = "Year",
                     breaks = seq(1999, 2020, 4),
                     limits=c(1999, 2019)) +
  scale_y_continuous(name = "SVP", 
                     breaks = seq(-45, 45, 0.5)) +
  ggtitle("Difference between treated group and control group") +
  geom_vline(xintercept = 2011) + geom_vline(xintercept = 2015) + theme_minimal()


# CTA p-value tables

ddpath <- "C:/..."
wdpath  <- "C:/..."
setwd(wdpath)
wdfS <- read.csv2(file(paste(ddpath, "Data Municipalities Distance, SVP, Covariates.csv", sep=""), 
                       encoding="latin1"), 
                  header=T, sep=",", dec=".", na.strings="NA")

wdfQ <- read.csv2(file(paste(ddpath, "Placebo Test.csv", sep=""), 
                       encoding="latin1"), 
                  header=T, sep=",", dec=".", na.strings="NA")

# Treatment
wdfS$treated = ifelse(wdfS$Diffdistance > 0.0000001, 1, 0) # wdf$Diffdistance < - 0.0000001 | 
wdfQ$treated = ifelse(wdfQ$Diffdistance > 0.0000001, 1, 0) # wdf$Diffdistance < - 0.0000001 | 

# OPTIONS: language
# Switzerland as a whole (), German (de), French (fr) or Italian (it)
wdfS$French = ifelse(wdfS$Language == "it", 1, 0)
wdfS <-subset(wdfS, French ==  1)

# Switzerland as a whole (), German (de), French (fr) or Italian (it)
wdfQ$French = ifelse(wdfQ$Language == "it", 1, 0)
wdfQ <-subset(wdfQ, French ==  1)

# Remake 

# 1999-2003
wdf9903 <-subset(wdfQ, Year=="1999" | Year=="2003")
wdf9903$time = ifelse(wdf9903$Year== 2003, 1,0)
wdf9903$did = wdf9903$time*wdf9903$treated
TT1 <- lm(log(SVP) ~ treated + time + did + log(Population) + log(GDPcap) + Density + Age20.64 + Foreigners, 
          data = wdf9903)

# 2003-2007
wdf0307 <-subset(wdfS, Year=="2003" | Year=="2007")
wdf0307$time = ifelse(wdf0307$Year== 2007, 1,0)
wdf0307$did = wdf0307$time*wdf0307$treated
TT2 <- lm(log(SVP) ~ treated + time + did + log(Population) + log(GDPcap) + Density + Age20.64 + Foreigners, 
          data = wdf0307)

# 2007-2011
wdf0711 <-subset(wdfQ, Year=="2007" | Year=="2011")
wdf0711$time = ifelse(wdf0711$Year== 2011, 1,0)
wdf0711$did = wdf0711$time*wdf0711$treated
TT3 <- lm(log(SVP) ~ treated + time + did + log(Population) + log(GDPcap) + Density + Age20.64 + Foreigners, 
          data = wdf0711)


# Results 
coeftest(TT1,  vcov = vcovHC, type = "HC1", cluster="group")
coeftest(TT2,  vcov = vcovHC, type = "HC1", cluster="group")
coeftest(TT3,  vcov = vcovHC, type = "HC1", cluster="group")
coeftest(TT4,  vcov = vcovHC, type = "HC1", cluster="group")
coeftest(TT5,  vcov = vcovHC, type = "HC1", cluster="group")



# I show the data only for the main figure  

# Table 2: p-value common trend assumption 
p_value  <- matrix(c(0.78137 , 0.580093 , 0.2113388   , 
              0.4101558 , 0.8840131 , 0.9873878 , 
              0.37344 , 0.434539 , 0.3750534 , 
              0.871319 , 0.83295  ,  0.83938 ),ncol=5,byrow=TRUE)
colnames(p_value ) <- c("1999-2003","2003-2007", "2007-2011", "2011-2015", "2015-2019")
rownames(p_value ) <- c("All","German", "French", "Italian")
p_value  <- as.table(p_value)
xtable(p_value)

# Table B2: p-value from logaithmic form of SVP
log_p_value <- matrix(c(0.95369, 0.462580, 0.48769, 
              0.43993, 0.3445, 0.9528355, 
              0.1660569, 0.7228479 , 0.63880 , 
              0.8728402, 0.83933 , 0.759619 ),ncol=5,byrow=TRUE)
colnames(log_p_value) <- c("1999-2003","2003-2007", "2007-2011", "2011-2015", "2015-2019")
rownames(log_p_value) <- c("All","German", "French", "Italian")
log_p_value <- as.table(log_p_value)
xtable(log_p_value)



## 6.2) Main results and ## 6.3) Robustness check 

library(foreign)
library(plm)
library(ggplot2)
library(dplyr)
library(sandwich)
library(lmtest)
library(stargazer)
library(sandwich)
library(clubSandwich)
library(lmtest)
library(car)


# STARGAZER 

ddpath <- "C:/..."
wdpath  <- "C:/..."
setwd(wdpath)
wdf <- read.csv2(file(paste(ddpath, "Data Municipalities Distance, SVP, Covariates.csv", sep=""), 
                       encoding="latin1"), 
                  header=T, sep=",", dec=".", na.strings="NA")

# Treatment
wdf$time = ifelse(wdf$Year== 2015, 1,0)
wdf$treated = ifelse(wdf$Diffdistance > 0.0000001, 1, 0) # wdf$Diffdistance < - 0.0000001 | 
wdf$did = wdf$time*wdf$treated

wdfF <- wdf 

# OPTIONS: Switzerland as a whole (), German (de), French (fr) or Italian (it)
wdf2 <-subset(wdf, Year=="2011" | Year=="2015")
wdf2$German = ifelse(wdf2$Language == "de", 1, 0)
wdf2$French = ifelse(wdf2$Language == "fr", 1, 0)
wdf2$Italian = ifelse(wdf2$Language == "it", 1, 0)
wdfG <-subset(wdf2, German=="1")
wdfF <-subset(wdf2, French=="1")
wdfI <-subset(wdf2, Italian=="1")


# Table 3: Main results

# Switzerland as a whole
TT1 <- lm(SVP ~ treated + time + did + log(Population) + log(GDPcap) + Density + Age20.64 + Foreigners, 
          data = wdf2)

# German-speaking Switzerland
TT2 <- lm(SVP ~ treated + time + did + log(Population) + log(GDPcap) + Density + Age20.64 + Foreigners, 
          data = wdfG)

# French-speaking Switzerland
TT3 <- lm(SVP ~ treated + time + did + log(Population) + log(GDPcap) + Density + Age20.64 + Foreigners, 
          data = wdfF)

# Italian-speaking Switzerland
TT4 <- lm(SVP ~ treated + time + did + log(Population) + log(GDPcap) + Density + Age20.64 + Foreigners, 
          data = wdfI)

# Clustered standard errors 

rob_se_na <-   list(sqrt(diag(vcovHC(TT1, type = "HC1", cluster = "group"))),
                    sqrt(diag(vcovHC(TT2, type = "HC1", cluster = "group"))),
                    sqrt(diag(vcovHC(TT3, type = "HC1", cluster = "group"))),
                    sqrt(diag(vcovHC(TT4, type = "HC1", cluster = "group"))))



stargazer(TT1, TT2, TT3, TT4,  
          dep.var.labels=c("SVP"),
          digits = 3,
          header = FALSE,
          type = "latex",
          se = rob_se_na,
          title = "Main Results - Estimated effect of the change in distance from the nearest asylum centre on SVP's votingshare",
          model.numbers = FALSE,
          omit.stat = c("f", "ser"),
          model.names = FALSE, 
          no.space = NULL,
          initial.zero = TRUE,
          align = FALSE,
          column.sep.width = "5pt",
          star.char = c("*", "**", "***"),
          star.cutoffs = c(.1, .05, .01),
          column.labels = c("(S)", "(g)", "(f)", "(i)"),
          report = ('vc*sp'),
          omit= c("Regionsname", "Year", "Districts", "time", "treated" , "Population", "GDPcap" , "Density" , "Age20.64" , "Foreigners", "Constant"),
          covariate.labels = c("Reduced distance")
)


# Table 4: Robustness check 

# Table 4: Log(SVP)

# Switzerland as a whole
TT1 <- lm(log(SVP) ~ treated + time + did + log(Population) + log(GDPcap) + Density + Age20.64 + Foreigners, 
          data = wdf2)

# German-speaking Switzerland
TT2 <- lm(log(SVP) ~ treated + time + did + log(Population) + log(GDPcap) + Density + Age20.64 + Foreigners, 
          data = wdfG)

# French-speaking Switzerland
TT3 <- lm(log(SVP) ~ treated + time + did + log(Population) + log(GDPcap) + Density + Age20.64 + Foreigners, 
          data = wdfF)
# coeftest(TT3, df = Inf, vcov = vcovHC, type = "HC1", cluster = "group")

# Italian-speaking Switzerland
TT4 <- lm(log(SVP) ~ treated + time + did + log(Population) + log(GDPcap) + Density + Age20.64 + Foreigners, 
          data = wdfI)



# Table 4: Group linear time trend
wdf$German = ifelse(wdf$Language == "de", 1, 0)
wdf$French = ifelse(wdf$Language == "fr", 1, 0)
wdf$Italian = ifelse(wdf$Language == "it", 1, 0)
wdfG1 <-subset(wdf, German=="1")
wdfF1 <-subset(wdf, French=="1")
wdfI1 <-subset(wdf, Italian=="1")

# Switzerland as a whole
TT11 <- lm(SVP ~ factor(treated) + factor(Year) + did + factor(Year)*factor(treated), data=wdf)

# German-speaking Switzerland
TT22 <- lm(SVP ~ factor(treated) + factor(Year) + did + factor(Year)*factor(treated), data=wdfG1)

# French-speaking Switzerland
TT33 <- lm(SVP ~ factor(treated) + factor(Year) + did + factor(Year)*factor(treated), data=wdfF1)

# Italian-speaking Switzerland
TT44 <- lm(SVP ~ factor(treated) + factor(Year) + did + factor(Year)*factor(treated), data=wdfI1)


# Clustered standard errors 

rob_se_na <-   list(sqrt(diag(vcovHC(TT1, type = "HC1", cluster = "group"))),
                    sqrt(diag(vcovHC(TT11, type = "HC1", cluster = "group"))),
                  
                   sqrt(diag(vcovHC(TT2, type = "HC1", cluster = "group"))),
                   sqrt(diag(vcovHC(TT22, type = "HC1", cluster = "group"))),
                   
                    sqrt(diag(vcovHC(TT3, type = "HC1", cluster = "group"))),
                   sqrt(diag(vcovHC(TT33, type = "HC1", cluster = "group"))),
                   
                    sqrt(diag(vcovHC(TT4, type = "HC1", cluster = "group"))),
                    sqrt(diag(vcovHC(TT44, type = "HC1", cluster = "group"))))
                    


# Stargazer output

stargazer(TT1, TT11, TT2, TT22, TT3, TT33, TT4, TT44, 
          dep.var.labels=c("SVP"),
          digits = 3,
          header = FALSE,
          type = "latex",
          se = rob_se_na,
          title = "Robustness check: logarithmic form and group linear time trend",
          model.numbers = FALSE,
          omit.stat = c("f", "ser"),
          model.names = FALSE, 
          no.space = NULL,
          initial.zero = TRUE,
          align = FALSE,
          column.sep.width = "5pt",
          star.char = c("*", "**", "***"),
          star.cutoffs = c(.1, .05, .01),
          column.labels = c("(1)", "(2)", "(3)", "(4)", "(5)", "(6)", "(7)", "(8)"),
          report = ('vc*sp'),
          omit= c("Regionsname", "Year", "Districts", "time", "treated" , "Population", "GDPcap" , "Density" , "Age20.64" , "Foreigners", "Constant"),
          covariate.labels = c("Reduced distance")
)



## 6.4) Table 5: Hetereogeneous effect

# FE: French effect
wdf2$French = ifelse(wdf2$Language == "fr", 1, 0)
wdf2$dtime = wdf2$French*wdfF$time 
wdf2$dtreated = wdf2$French*wdfF$treated 
wdf2$ddd = wdfF$French*wdfF$time*wdfF$treated

DDD1 <- lm(SVP ~ treated + time + did + log(Population) + log(GDPcap) + Density + Age20.64 + Foreigners + French + dtime + dtreated + ddd, 
           data = wdf2)

# LP: Lower Population 

# Operation to avoid unbalanced Panel
wdfp1 <-subset(wdf2, Year == 2015)
quantile(wdfp1$Population, c(.1, .2, .3, .4, .5, .6, .7, .8, .9))
wdfp1$Pop_low_50 = ifelse(wdfp1$Population <  1323.0, 1, 0)
#wdfp1$Pop_low_40 = ifelse(wdfp1$Population <  1004.0, 1, 0)
#wdfp1$Pop_low_30 = ifelse(wdfp1$Population <   759.0, 1, 0)
# wdfp1$Pop_low_20 = ifelse(wdfp1$Population <  519., 1, 0)

wdfp2 <-subset(wdf2, Year== 2011)

wdfp11 <-subset(wdfp1[,c("Regionsname" , "Pop_low_50")])

wdfp2 <- merge(wdfp2, wdfp11,  all = T)

wdfP<- merge(wdfp1, wdfp2,  all = T)

# 
wdfP$dtime = wdfP$Pop_low_50*wdfP$time 
wdfP$dtreated = wdfP$Pop_low_50*wdfP$treated 
wdfP$ddd = wdfP$Pop_low_50*wdfP$time*wdfP$treated


## Only French
wdfP$French = ifelse(wdfP$Language == "fr", 1, 0)
wdfPF <-subset(wdfP, French=="1")

# Regression
DDD_PF1 <- lm(SVP ~ treated + time + did + log(Population) + log(GDPcap) + Density + Age20.64 + Foreigners + Pop_low_50 + dtime + dtreated + ddd, 
              data = wdfPF)

# GP: Greater Proximity

# Operation to avoid unbalanced Panel
wdfd1 <-subset(wdf2, Year == 2015)
quantile(wdfd1$Distance, c(.1, .2, .3, .4, .5, .6, .7, .8, .9))
wdfd1$D_low_50 = ifelse(wdfd1$Distance <  5651.985, 1, 0)
# wdfd1$D_low_40 = ifelse(wdfd1$Distance <  4775.942, 1, 0)
# wdfd1$D_low_30 = ifelse(wdfd1$Distance <  4018.128, 1, 0)
# wdfd1$D_low_20 = ifelse(wdfd1$Distance <  3280.721, 1, 0)

wdfd2 <-subset(wdf2, Year== 2011)

wdfd11 <-subset(wdfd1[,c("Regionsname" , "D_low_50")])

wdfd2 <- merge(wdfd2, wdfd11,  all = T)

wdfD <- merge(wdfd1, wdfd2,  all = T)

# 
wdfD$dtime = wdfD$D_low_50*wdfD$time 
wdfD$dtreated = wdfD$D_low_50*wdfD$treated 
wdfD$ddd = wdfD$D_low_50*wdfD$time*wdfD$treated


## Only French
wdfD$French = ifelse(wdfD$Language == "fr", 1, 0)
wdfDF <-subset(wdfD, French=="1")

# Regression
DDD_DF1 <- lm(SVP ~ treated + time + did + log(Population) + log(GDPcap) + Density + Age20.64 + Foreigners + Distance + D_low_50 + dtime + dtreated + ddd, 
              data = wdfDF)


# GDR: Greater distance reduction
wdfDD <- wdf2 
wdfDD <-subset(wdfDD, Diffdistance < - 0.0000001 | Diffdistance > 0.0000001)
wdfdd1 <-subset(wdfDD, Year == 2015)

## Only French
wdfDD$French = ifelse(wdfDD$Language == "fr", 1, 0)
wdfDDF <-subset(wdfDD, French=="1")

wdfddf1 <-subset(wdfDDF, Year == 2015)
wdfddf1 <-subset(wdfddf1, Diffdistance < - 0.0000001 | Diffdistance > 0.0000001)
quantile(wdfddf1$Diffdistance, c(.1, .2, .3, .4, .5, .6, .7, .8, .9))
#      10%       20%       30%       40%       50%       60%       70%       80%       90% 
# # 368.0324  672.5183 1042.8697 1532.8257 2153.2839 2996.6711 3710.3110 4227.8257 5482.8735

# DD
wdfDDF$treated = ifelse(wdfDDF$Diffdistance >  2153.2839, 1, 0)
wdfDDF$dd = wdfDDF$time*wdfDDF$treated

DDD_DDF1 <- lm(SVP ~ treated + time + dd + log(Population) + log(GDPcap) + Density + Age20.64 + Foreigners, 
               data = wdfDDF)

# Stargazer DDD

# Standar errors 
rob_se_ddd <-   list(sqrt(diag(vcovHC(DDD1, type = "HC1", cluster = "group"))),
                     sqrt(diag(vcovHC(DDD_PF1, type = "HC1", cluster = "group"))),
                     sqrt(diag(vcovHC(DDD_DF1, type = "HC1", cluster = "group"))),
                     sqrt(diag(vcovHC(DDD_DDF1, type = "HC1", cluster = "group"))))


## DDD 
stargazer(DDD1, DDD_PF1, DDD_DF1, DDD_DDF1, 
          dep.var.labels=c("SVP"),
          digits = 3,
          header = FALSE,
          type = "latex",
          se = rob_se_ddd,
          title = "Hetereogeneous effect",
          model.numbers = FALSE,
          omit.stat = c("f", "ser"),
          model.names = FALSE, 
          no.space = NULL,
          initial.zero = TRUE,
          align = FALSE,
          column.sep.width = "0pt",
          star.char = c("*", "**", "***"),
          star.cutoffs = c(.1, .05, .01),
          report = ('vc*sp'),
          column.labels = c("(S)", "(f)", "(f)" , "(f)"),
          omit= c("Regionsname", "Year", "Districts", "time", "treated" , "Population", "GDPcap" , "Density" , "Age20.64" , "Foreigners", "Constant" , "did" , "French" , "dtime" , "dtreated" , "D_low_50" , "Pop_low_50" , "Distance"),
          covariate.labels = c("Reduced distance"))

 
 
