##The Data visualization and measurement of central tendency
## The activity enhance deeply cleaning regarding data and also bolster to understand the Univariate and Multivariate Analysis
##The next phase is to Model the Advanced PLS-PM Models
install.packages("data.table")
dynamic=read.table("C:/Users/bwabo/OneDrive/Desktop/Ph.D. Thesis/Data set.csv",header = T,sep = ",", stringsAsFactors = FALSE )
head(dynamic)
summary(dynamic)
mean(dynamic$KS3,na.rm = TRUE)
tail(dynamic)
str(dynamic)
#Univariate data visualization 
library(ggplot2)
library(cowplot)
library(MASS)
library(JWileymisc)
library(data.table)
#Data Visualization and Normality Testing in a single indicators
ggplot(dynamic, aes(KS1)) +
  geom_dotplot()
#ggplot
ggplot(dynamic, aes(KS1)) +
  geom_histogram()
ggplot(dynamic, aes(KS1)) +
  geom_density()
ggplot(dynamic, aes(sample = KS1)) +
  geom_qq()
#
qplot(
  x = qnorm(
    p = ppoints(length(dynamic$KS1)),
    mean = mean(dynamic$KS1,na.rm=TRUE),
    sd = sd(dynamic$KS1dynamic,na.rm=TRUE)),
  y = sort(dynamic$KS1,na.rm=TRUE),
  xlab = "Theoretical Normal Quantiles",
  ylab = "KS1") +
  geom_abline(slope = 1, intercept = 0)
#
ggplot(dynamic, aes(KS1)) +
  geom_density() +
  stat_function(fun = dnorm,
                args = list(
                  mean = mean(dynamic$KS1),
                  sd = sd(dynamic$KS1
                  )),
                colour = "blue")
#Multivariate data visualization 
library(ggplot2)
library(cowplot)
library(MASS)
library(mvtnorm)
library(mgcv)
library(quantreg)
library(JWileymisc)
library(data.table)
#data table
dy=data.table(dynamic)
class(dynamic)
#
d2[,dynamic$KS1]
p.cut3 <- ggplot(
  data = dy[, .(KS1,xcut = cut(x, quantile(x,
                                           probs = seq(0, 1, by = 1/3)), include.lowest = TRUE))],
  aes(xcut, KS1)) +
  geom_boxplot(width=.25) +
  theme(axis.text.x = element_text(
    angle = 45, hjust = 1, vjust = 1)) +
  xlab("")
#
ggplot(dynamic, aes(KS1, KS2)) +
  geom_point(colour="grey50") +
  stat_smooth(method = "loess", span = .2,
              colour = "black") +
  stat_smooth(method = "loess", span = 2,
              colour = "black", linetype = 2)
#
ggplot(dynamic, aes(KS1, KS2)) +
  geom_point(colour="grey50") +
  stat_smooth(method = "loess", colour = "black") +
  stat_smooth(method = "lm",
              formula = KS1 ~ KS2 + I(x^2),
              colour = "blue", linetype = 2)
#Assesing Homogeneity Variance 
head(dynamic)
dynamic1 = as.data.table(dynamic)
dynamic1[, .(V = var(KS1)), by =Maritual.Stutus]
#Data Visualization
plot_grid(
  ggplot(dynamic1, aes(Maritual.Stutus, KS1)) +
    geom_boxplot() +
    xlab(""),
  ggplot(dynamic1[, .(KS1= KS1 -
                        median(KS1), by = Maritual.Stutus],
         aes(KS1, Maritual.Stutus)) +
           geom_boxplot() +
           xlab(""),
         ncol = 3, labels = c("A", "B"), align = "hv")
#Violin Plot 
  ggplot(dynamic1, aes(Maritual.Stutus, KS1)) +
    geom_violin() +
    geom_boxplot(width = .1) +
    xlab("")
#The Tidyverse 
  install.packages("tidyverse")
  library(tidyverse)
  install.packages("dplyr")
  library(dplyr)
#newdata
  newdata_dy=select(dynamic,Types.of.Firm,Region)
  newdata_dy
#Pipe operator
  dynamic %>%select(Types.of.Firm,Region)%>% Filter(Age.of.Respondents<=30)
  class(dynamic)
  str(dynamic)
#Descriptive summary
s=KS1%>%
filter(Gender=="Female")%>%
summarise(average=mean(KS1),standard_deviation=sd(KS1))
dynamic$Gender=as.factor(dynamic$Gender)
dynamic$Level.of.Occupancy=as.factor(dynamic$Level.of.Occupancy)
dynamic$Education.Level=as.factor(dynamic$Education.Level)
dynamic$Maritual.Status=as.factor(dynamic$Maritual.Status)
dynamic$Size=as.factor(dynamic$Size)
summary(dynamic)
str(dynamic)
#qqplot
install.packages("car")
install.packages("summarytools")
library(summarytools)
library(car)
##Frequency Distribution 
freq(dynamic$Gender)
freq(dynamic$Level.of.Occupancy)
freq(dynamic$Education.Level)
freq(dynamic$Size)
freq(dynamic$Maritual.Status)
##Cross Tabulation
ctable(
  x=dynamic$Gender,
  y=dynamic$Level.of.Occupancy
)
ctable(
  x=dynamic$Gender,
  y=dynamic$Education.Level
)
ctable(
  x=dynamic$Gender,
  y=dynamic$Maritual.Status
)
ctable(
  dynamic$Gender,
  dynamic$Size
)
#Raw-proportions 
ctable(
  x=dynamic$Gender,
  y=dynamic$Level.of.Occupancy,
  prop = "t"
)
ctable(
  x=dynamic$Gender,
  y=dynamic$Education.Level,
  prop = "t"
)
ctable(
  x=dynamic$Gender,
  y=dynamic$Maritual.Status,
  prop = "t"
)
ctable(
  dynamic$Gender,
  dynamic$Size,
  prop = "t"
)
##Test of Independence (Chi-Square)
ctable(
  x=dynamic$Gender,
  y=dynamic$Level.of.Occupancy,
  chisq = TRUE,
  headings = FALSE
)
ctable(
  x=dynamic$Gender,
  y=dynamic$Education.Level,
  chisq = TRUE,
  headings = FALSE
)
ctable(
  x=dynamic$Gender,
  y=dynamic$Maritual.Status,
  chisq = TRUE,
  headings = FALSE
)
ctable(
  dynamic$Gender,
  dynamic$Size,
  chisq = TRUE,
  headings = FALSE
)
#
descr(dynamic,
      headings = FALSE, # remove headings
      stats = "common" # most common descriptive statistics
)
#Dfsummary
dfSummary(dynamic)
#Group analysis
library(psych)
describeBy(
  dynamic,
  dynamic$Gender
)
#Aggregate function 
aggregate(cbind(Gender,Level.of.Occupancy)~Region+Maritual.Status,
          data= dynamic,
          mean)
aggregate(cbind(Gender,Level.of.Occupancy)~Region+Maritual.Status,
          data= dynamic,
          median)
aggregate(cbind(Gender,Level.of.Occupancy)~Region+Maritual.Status,
          data= dynamic,
          sd)
aggregate(cbind(Gender,Level.of.Occupancy)~Region+Maritual.Status,
          data= dynamic,
          chisq.test)
sd(dynamic$Gender)
#Group Analysis
qqPlot(dynamic$Experience, groups = dynamic$Size)
mean(dynamic$Gender)
summarise(dynamic,na.rm=TRUE)
mean(dynamic$KS1)
str(dynamic)
s <- dynamic %>%
filter(Gender == "Female") %>%
  summarize(average = mean(dynamic), standard_deviation = sd(dynamic))
  s
##Descriptive Analysis
  install.packages("readr")
  library(readr)
  install.packages("reshape")
  install.packages("pastecs")
  library(reshape)
  library(pastecs)
  summary(dynamic$Gender)
  summary(dynamic$Region)
  summary(dynamic$Firm)
  summary(dynamic$Education.Level)
  summary(dynamic$Gender)
  summary(dynamic$KS1,mean)
  mean(dynamic$Gender)
  stat.desc(dynamic)
  dim(dynamic)
  dimnames(dynamic)
  dimnames.data.frame(dynamic)
  sapply(dynamic, class)
#Fun function
  summfn = function(x)c(n=sum(!is.na(x)),mean=mean(x),sd=sd(x))
  
  g=apply(dynamic,2,summfn)
#Visualization
  library(ggplot2)
  ggplot(data = dynamic)
  dynamic%>%ggplot()
  d=dynamic%>% ggplot(aes(KS1/10^6,total,label=abb))
  d
#Day 2 Experience 
  mean(dynamic$Experience)
  mean(dynamic$Experience,na.rm = TRUE)
  mean(dynamic$Age)
  structure(dynamic)
  #
  value=dynamic$Gender=="Male"
  x=dynamic$KS1[value]
  m=mean(x)
  x
  index <- heights$sex=="Male"
  x <- heights$height[index]
#ggplot(Female) 
  dynamic %>%
    filter(Gender == "Female") %>%
    ggplot(aes(KS1)) +
    geom_histogram(binwidth = 1)
#adding color 
  dynamic %>%
    filter(Gender == "Female") %>%
    ggplot(aes(KS1)) +
    geom_histogram(binwidth = 1, fill = "blue", col = "black") +
    xlab("Male KS1") +
    ggtitle("Histogram")
#Density Plot 
  dynamic %>%
    filter(Gender == "Female") %>%
    ggplot(aes(KS1)) +
    geom_density()
#fill it with the color 
  dynamic %>%
    filter(Gender == "Female") %>%
    ggplot(aes(KS1)) +
    geom_density(fill="blue")
#QQ-Plots
  dynamic%>%filter(Gender=="Female")%>%
    ggplot(aes(sample=KS1))+
    geom_qq()
#Central Tendency
  dyna=dynamic%>% filter(Gender=="Female")%>%
    summarise(mean=mean(KS1),sd=sd(KS1,na.rm = FALSE))
  dyna
#Inserting the straight line 
  dynamic %>% filter(Gender=="Female") %>%
    ggplot(aes(sample = KS1)) +
    geom_qq(dparams = dyna) +
    geom_abline()
#
  dynamic%>%
    filter(Gender=="Female")%>%
    ggplot(aes(sample=scale(KS1)))+
    geom_abline()
#ggplot(Male)
  str(dynamic)
  View(dynamic)
  dynamic %>%
    filter(Gender == "Male") %>%
    ggplot(aes(KS1)) +
    geom_histogram(binwidth = 1)
#adding color 
  dynamic %>%
    filter(Gender == "Male") %>%
    ggplot(aes(KS1)) +
    geom_histogram(binwidth = 1, fill = "blue", col = "black") +
    xlab("Male KS1") +
    ggtitle("Histogram")
#Density Plot 
  dynamic %>%
    filter(Gender == "Male") %>%
    ggplot(aes(KS1)) +
    geom_density()
#fill it with the color 
  dynamic %>%
    filter(Gender == "Male") %>%
    ggplot(aes(KS1)) +
    geom_density(fill="blue")
#QQ-Plots
  dynamic%>%filter(Gender=="Male")%>%
    ggplot(aes(sample=KS1))+
    geom_qq()
  #Central Tendency
  dyna=dynamic%>% filter(Gender=="Male")%>%
    summarise(mean=mean(KS1),sd=sd(KS1,na.rm = FALSE))
  dyna
#Inserting the straight line 
  dynamic %>% filter(Gender=="Male") %>%
    ggplot(aes(sample = KS1)) +
    geom_qq(dparams = dyna) +
    geom_abline()
#
  dynamic%>%
    filter(Gender=="Male")%>%
    ggplot(aes(sample=scale(KS1)))+
    geom_abline()
##The firm size 
  dynamic %>%
    filter(Size == "Medium") %>%
    ggplot(aes(KS1)) +
    geom_histogram(binwidth = 1)
  dynamic %>%
    filter(Size == "Medium") %>%
    ggplot(aes(KS1)) +
    geom_histogram(binwidth = 1, fill = "blue", col = "black") +
    xlab("Size KS1") +
    ggtitle("Histogram")
#Density Plot 
  dynamic %>%
    filter(Size == "Medium") %>%
    ggplot(aes(KS1)) +
    geom_density()
  #fill it with the color 
  dynamic %>%
    filter(Size == "Medium") %>%
    ggplot(aes(KS1)) +
    geom_density(fill="blue")
#QQ-Plots
  dynamic%>%filter(Size=="Medium")%>%
    ggplot(aes(sample=KS1))+
    geom_qq()
#Central Tendency
  dyna=dynamic%>% filter(Size=="Medium")%>%
    summarise(mean=mean(KS1),sd=sd(KS1,na.rm = FALSE))
  dyna
  #Inserting the straight line 
  dynamic %>% filter(Size=="Medium") %>%
    ggplot(aes(sample = KS1)) +
    geom_qq(dparams = dyna) +
    geom_abline()
 #
  dynamic%>%
    filter(Size=="Medium")%>%
    ggplot(aes(sample=scale(KS1)))+
    geom_abline()
##large
  dynamic %>%
    filter(Size == "Large") %>%
    ggplot(aes(KS1)) +
    geom_histogram(binwidth = 1)
  dynamic %>%
    filter(Size == "Large") %>%
    ggplot(aes(KS1)) +
    geom_histogram(binwidth = 1, fill = "blue", col = "black") +
    xlab("Size KS1") +
    ggtitle("Histogram")
#Density Plot 
  dynamic %>%
    filter(Size == "Large") %>%
    ggplot(aes(KS1)) +
    geom_density()
#fill it with the color 
  dynamic %>%
    filter(Size == "Lager") %>%
    ggplot(aes(KS1)) +
    geom_density(fill="blue")
#QQ-Plots
  dynamic%>%filter(Size=="Large")%>%
    ggplot(aes(sample=KS1))+
    geom_qq()
#Central Tendency
  dyna=dynamic%>% filter(Size=="Large")%>%
    summarise(mean=mean(KS1),sd=sd(KS1,na.rm = FALSE))
  dyna
  #Inserting the straight line 
  dynamic %>% filter(Size=="Large") %>%
    ggplot(aes(sample = KS1)) +
    geom_qq(dparams = dyna) +
    geom_abline()
#
  dynamic%>%
    filter(Size=="Large")%>%
    ggplot(aes(sample=scale(KS1)))+
    geom_abline()
##QUICK PLOT 
  d = dynamic %>%
    filter(Gender=="Female") %>%
    pull(KS1)
  qplot(d)
#qplot through the ggplot
  qplot(sample = scale(x)) + geom_abline()
#Q-Plot with the dot operator 
  dynamic %>% qplot(Gender, KS1, data = .)
#The box plot 
  dynamic %>% qplot(Gender, KS1, data = ., geom = "boxplot")
  #The plot become the noisy density 
  qplot(d, geom = "density")
#Level of Occupancy
  dynamic %>%
    filter(Level.of.Occupancy == "Employee") %>%
    ggplot(aes(KS1)) +
    geom_histogram(binwidth = 1)
  dynamic %>%
    filter(Level.of.Occupancy == "Employee") %>%
    ggplot(aes(KS1)) +
    geom_histogram(binwidth = 1, fill = "blue", col = "black") +
    xlab("KS1") +
    ggtitle("Histogram")
#Density Plot 
  dynamic %>%
    filter(Level.of.Occupancy == "Employee") %>%
    ggplot(aes(KS1)) +
    geom_density()
#fill it with the color 
  dynamic %>%
    filter(Level.of.Occupancy == "Employee") %>%
    ggplot(aes(KS1)) +
    geom_density(fill="blue")
#QQ-Plots
  dynamic%>%filter(Level.of.Occupancy == "Employee")%>%
    ggplot(aes(sample=KS1))+
    geom_qq()
#Central Tendency
  dyna=dynamic%>% filter(Level.of.Occupancy == "Employee")%>%
    summarise(mean=mean(KS1),sd=sd(KS1,na.rm = FALSE))
  dyna
#Inserting the straight line 
  dynamic %>% filter(Level.of.Occupancy == "Employee") %>%
    ggplot(aes(sample = KS1)) +
    geom_qq(dparams = dyna) +
    geom_abline()
#
  dynamic%>%
    filter(Level.of.Occupancy == "Employee")%>%
    ggplot(aes(sample=scale(KS1)))+
    geom_abline()
##Managers 
  dynamic %>%
    filter(Level.of.Occupancy == "Manager") %>%
    ggplot(aes(KS1)) +
    geom_histogram(binwidth = 1)
  dynamic %>%
    filter(Level.of.Occupancy == "Manager") %>%
    ggplot(aes(KS1)) +
    geom_histogram(binwidth = 1, fill = "blue", col = "black") +
    xlab("KS1") +
    ggtitle("Histogram")
#Density Plot 
  dynamic %>%
    filter(Level.of.Occupancy == "Manager") %>%
    ggplot(aes(KS1)) +
    geom_density()
#fill it with the color 
  dynamic %>%
    filter(Level.of.Occupancy == "Manager") %>%
    ggplot(aes(KS1)) +
    geom_density(fill="blue")
#QQ-Plots
  dynamic%>%filter(Level.of.Occupancy == "Manager")%>%
    ggplot(aes(sample=KS1))+
    geom_qq()
#Central Tendency
  dyna=dynamic%>% filter(Level.of.Occupancy == "Manager")%>%
    summarise(mean=mean(KS1),sd=sd(KS1,na.rm = FALSE))
  dyna
#Inserting the straight line 
  dynamic %>% filter(Level.of.Occupancy == "Manager") %>%
    ggplot(aes(sample = KS1)) +
    geom_qq(dparams = dyna) +
    geom_abline()
#
  dynamic%>%
    filter(Level.of.Occupancy == "Manager")%>%
    ggplot(aes(sample=scale(KS1)))+
    geom_abline()
## Tibble data Set 
  dynamic %>% as_tibble()
## Regions analysis
  dynamic %>%
    filter(Gender == "Female" & Region %in% c("Kilimanjaro","Arusha","Tanga")) %>%
    select(Region, Size)
  dynamic %>%
    filter(Size == "Medium" & Region %in% c("Kilimanjaro","Arusha","Tanga")) %>%
    select(Region, Gender,Age)
#Scatter Plot 
  filter(dynamic, Size == "Medium") %>%
    ggplot(aes(Region, Gender,Age)) +
    geom_point()
#
  filter(dynamic, Size == "Medium") %>%
    ggplot( aes( Region, Age, color = Region)) +
    geom_point()
#Faceting
  head(dynamic)
  filter(dynamic, Size %in%c("Small", "Medium", "Large")) %>%
    ggplot(aes( Level.of.Occupancy, Region, col = Region)) +
    geom_point() +
    facet_grid(Region~Size)
#Draw in Pair 
  filter(dynamic, Size%in%c("Small", "Medium", "Large")) %>%
    ggplot(aes(Level.of.Occupancy, Region, col = Region)) +
    geom_point() +
    facet_grid(. ~ Education.Level)
##Facet Wrap
  Regions = c("Arusha", "Kilimanjaro", "Tanga")
  Sizes= c("Small","Medium","Large")
  dynamic %>%
    filter(Region %in% Regions & Size%in% Sizes) %>%
    ggplot( aes( Gender,Level.of.Occupancy, col = Region)) +
    geom_point() +
    facet_wrap(~Education.Level)
#Time Series Plot
  dynamic %>%
    filter(Region== "Large") %>%
    ggplot(aes(Experience, Level.of.Occupancy)) +
    geom_point()
#
  Regions = c("Kilimanjaro","Tanga","Arusha")
  dynamic%>%filter(Region%in% Regions & !is.na(Gender))%>%
    ggplot(aes(Age,KS1,group=Region))+
    geom_line()
#Character Manipulations
#Converting the character into the factor
  dynamic%>%
    ggplot(aes(Gender,KS1)) +
    geom_point()
#Jitter and alpha blending
  dynamic%>%
    ggplot(aes(Gender,KS1)) +
    geom_jitter(width = 0.1, alpha = 0.2)
#Allign plots
  dynamic %>%
    ggplot(aes(KS1, ..density..)) +
    geom_histogram(binwidth = 1, color="black") +
    facet_grid(Gender~.)
#
  dynamic %>%
    ggplot(aes(Gender, KS1)) +
    geom_boxplot(coef=3) +
    geom_jitter(width = 0.1, alpha = 0.2) +
    ylab("sex")
#
  dynamic %>%
    ggplot(aes(KS1, ..density..)) +
    geom_histogram(binwidth = 1, color="Red") +
    facet_grid(Size~.)
##ROBUST SUMMARIES
  dynamic %>%
    group_by(Gender) %>%
    summarize(average = mean(KS1), sd = sd(KS1),
              median = median(KS1), MAD = mad(KS1))
dynamic %>%
    group_by(Gender) %>%
    summarize(average = mean(KS1), sd = sd(KS1),
              median = median(KS1), MAD = mad(KS1))
#
  dynamic %>%
    group_by(Size) %>%
    summarize(average = mean(KS1), sd = sd(KS1),
              median = median(KS1), MAD = mad(KS1))
  
dynamic %>%
    group_by(Education.Level) %>%
    summarize(average = mean(KS1), sd = sd(KS1),
              median = median(KS1), MAD = mad(KS1))

dynamic %>%
  group_by(Maritual.Status) %>%
  summarize(average = mean(KS1), sd = sd(KS1),
            median = median(KS1), MAD = mad(KS1))
#Checking the first ten row
  dynamic%>%
    arrange(desc(KS1))%>%
    top_n(20,KS1)
##Unsupervised Learning Technique(Machine Learning)
##Exploratory Data Analysis
  library(ggplot2)
  library(cowplot)
  library(viridis)
  library(viridisLite)
  library(scales)
  library(data.table)
  install.packages("ape")
  library(ape)
  library(MASS)
  library(matrixStats)
  options(width = 70, digits = 2)
#Install PCA methods
  install.packages("BiocManager")
  library(BiocManager)
#Split Function
  d= split(dynamic, dynamic$Date)
  d 
##Mean Centering for the continuous data(Experience)
## The following codes for mean centering I have borrowed from Prof.
## Gaston Sanchez in his blog.(https://www.gastonsanchez.com/visually-enforced/blog/archive/)
set.seed(212)
Data = matrix(rnorm(15), 5, 3)
Data
### centering with 'scale()'
center_scale = function(x) {
    scale(x, scale = FALSE)
  }
  
# apply it
center_scale(Data)
# center with 'apply()'
center_apply <- function(x) {
apply(x, 2, function(y) y - mean(y))
  }
  
# apply it
  center_apply(Data)  
# center with 'colMeans()'
  center_colmeans <- function(x) {
    xcenter = colMeans(x)
    x - rep(xcenter, rep.int(nrow(x), ncol(x)))
  }
  
# apply it
center_colmeans(Data)  
#mtcars example 
install.packages("dplyr")
#
#Mean centering 
library(tidyverse)
head(dynamic)
str(dynamic)
dynamic %>%
  add_rownames()%>% #if the row names are needed as a column
  group_by(Gender) %>% 
  mutate(cent= Experience-mean(Experience))%>%
  dplyr ::select(cent)
  dynamic$Experience[1:100]-mean(dynamic$Experience)
## This would be useful for multi-group analysis. 