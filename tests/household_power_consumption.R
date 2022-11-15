# Electric energy consumption in one house in France
# measured each minute from December 2006 to November 2010.
# Download at: https://bit.ly/3ugv8Pt
source('main.R')

df <- read.csv(
  'dataset/household_power_consumption/household_power_consumption.txt',
  sep = ";"
)
df <- df[,c(
  "Global_active_power",
  "Global_reactive_power",
  "Voltage",
  "Global_intensity"
)]
df <- as.data.frame(lapply(df, as.numeric))

run_test(df, 100)
