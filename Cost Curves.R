library(plotly)
library(dplyr)

# choose file
df <- read.csv(file.choose())

# check data
dim(df)
head(df)
summary(df)


# order ad set by country
df <- df[order(df$Ad.Set.Name),]

# drop rows for other countries

df=df[!grepl("au",df$Ad.Set.Name),]
df=df[!grepl("AU",df$Ad.Set.Name),]
df=df[!grepl("eu",df$Ad.Set.Name),]
df=df[!grepl("EU",df$Ad.Set.Name),]
df=df[!grepl("row",df$Ad.Set.Name),]
df=df[!grepl("ROW",df$Ad.Set.Name),]
df=df[!grepl("se",df$Ad.Set.Name),]
df=df[!grepl("SE",df$Ad.Set.Name),]
df=df[!grepl("uk",df$Ad.Set.Name),]
df=df[!grepl("UK",df$Ad.Set.Name),]

dim(df)


# drop columns
df <- df[ -c(1, 4, 6:9) ]

head(df)
dim(df)

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

# Marginal CPA
# df <- df %>% mutate("prev_cumPurchases" = 
#                      CumulativePurchases - lag(CumulativePurchases))

# df <- df %>% mutate("prev_cumSpend" = 
#                    CumulativeSpend - lag(CumulativeSpend))

df <- df %>% mutate("MarginalCPA" = 
                      (CumulativeSpend - lag(CumulativeSpend)) /
                      (CumulativePurchases - lag(CumulativePurchases))
                    )

# plot Spend and Purchase
# plot(df$CumulativeSpend,df$CumulativePurchases)

# plotly plot Spend and Purchases
fig1 <- plot_ly(data = df, x = ~CumulativeSpend, 
                y = ~CumulativePurchases, type ='scatter', mode='markers')

fig1

# 
#fig <- plot_ly(data = df, x = ~CumulativeSpend, y = ~CumulativeCPA, type = 'scatter', mode = 'lines')

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
x<- readline(prompt="Enter your spend: ")
x <- as.integer(x)

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






