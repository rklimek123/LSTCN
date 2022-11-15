# Changes in the Bitcoin transaction graph observed with a daily
# frequency from January 2009 to December 2018.
# Download at: https://bit.ly/3ES71M1
source('main.R')

df <- read.csv('dataset/bitcoin_heist_data/BitcoinHeistData.csv')
df <- df[,c(
  "length",
  "weight",
  "count",
  "looped",
  "neighbors",
  "income"
)]
df <- as.data.frame(lapply(df, as.numeric))

run_test(df, 200)
