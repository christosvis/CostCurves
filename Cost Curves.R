library(plotly)
library(dplyr)

# choose file
df <- read.csv(file.choose())

# check data
dim(df)
head(df)
summary(df)

# change column names
colnames(df) <- c("AdName", "Spend", "Purchases")

# Create calculated column CPA
df$CPA <- df$Spend/df$Purchases

# drop rows if Purchases or Spend = 0
df<-subset(df, df$Purchases!=0)
df<-subset(df, df$Spend!=0)
dim(df)

# order by CPA
df <- df[order(df$CPA),]

# reset row numbers
rownames(df) = seq(length=nrow(df))


# cumulative spend and Purchases
df$CumulativeSpend <- ave(df$Spend, FUN=cumsum)
df$CumulativePurchases <- ave(df$Purchases, FUN=cumsum)

# Cumulative CPA
df$CumulativeCPA <- df$CumulativeSpend/df$CumulativePurchases

df <- df %>% mutate("MarginalCPA" = 
                      (CumulativeSpend - lag(CumulativeSpend)) /
                      (CumulativePurchases - lag(CumulativePurchases))
                    )

# plotly plot Spend and Purchases
fig1 <- plot_ly(data = df, x = ~CumulativeSpend, 
                y = ~CumulativePurchases, type ='scatter', mode='markers')

fig1


# polynomial model Spend - Purchases. Important: Raw = TRUE
model <- lm(df$CumulativePurchases ~ poly(df$CumulativeSpend,2, raw=TRUE))
summary(model)
model.summary <-summary(model)

# extract intercept
intercept<- model.summary$coefficients[1, 1]  
intercept

# extract x coefficients
coefficient1 <- model.summary$coefficients[2, 1]  
coefficient1

coefficient2 <- model.summary$coefficients[3, 1]  
coefficient2

# Prediction
# change x value. This is your spend
x <- 150000
y <- intercept + coefficient1*x + coefficient2*x^2
# this is your predicted Conversions
y


# multiple lines. Marginal CPA and Cumulative CPA
fig2 <- plot_ly(df, x = ~CumulativeSpend) 
fig2 <- fig2 %>% add_trace(y = ~CumulativeCPA, 
                           name = 'CPA', type = 'scatter', mode = 'lines') 
fig2 <- fig2 %>% add_trace(y = ~MarginalCPA, 
                           name = 'Marginal CPA', type = 'scatter', mode = 'lines') 

fig2






