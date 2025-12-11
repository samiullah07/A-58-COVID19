# Correlation between Obesity (%) and
# Confirmed COVID-19 cases (% of population)
#Member 1 Abdul Basit Khan Load packages, import data, clean dataset

library(readr)
# 1. Loaded the dataset

diet_data <- read_csv("Food_Supply_Quantity_kg_Data.csv")
df <- diet_data

# Checking the names
names(df)

# We Make sured obesity simple name
names(df)[which(names(df) == "Obesity")]   <- "obesity"
#now we make sure Confirmed cases have simple name
names(df)[which(names(df) == "Confirmed")] <- "confirmed"

# converting to numeric to read as character 
df$obesity   <- as.numeric(df$obesity)
df$confirmed <- as.numeric(df$confirmed)

## 2. Now Cleanning the data

# Removing the unrealistic values and missing values 
df2 <- subset(df,
              !is.na(obesity)   &
              !is.na(confirmed) &
              obesity   > 0  & obesity   < 60 &
              confirmed >= 0 & confirmed < 30)
# below code shows Number of countries
cat("Number of countries used:", nrow(df2), "\n")


## --- Member 2:Zia Ul Mustafa added this code "Histograms to check distributions" 

## 3. Histograms to check distributions

hist(df2$obesity,
     main = "Histogram of Obesity (%)",
     xlab = "Obesity (% of adult population)",
     ylab = "Number of countries",
     col  = "lightblue")

hist(df2$confirmed,
     main = "Histogram of Confirmed COVID-19 Cases (%)",
     xlab = "Confirmed cases (% of population)",
     ylab = "Number of countries",
     col  = "lightgreen")

## --- Member 3: Now we will do Correlation test  ---
# (Member 3 will add their code here later)



## --- Member 4: Scatterplot with regression line  ---
# (Member 4 will add their code here later)

plot(df2$obesity, df2$confirmed,
     xlab = "Obesity (% of adult population)",
     ylab = "Confirmed COVID-19 cases (% of population)",
     main = "Scatterplot of Obesity vs Confirmed COVID-19 Cases")

abline(lm(df2$confirmed ~ df2$obesity), col = "red")

## --- Member 5: Saved plots as PNG ---

#Saved plots as PNG (for the report) 

# Scatterplot
png("scatter_obesity_confirmed.png")
plot(df2$obesity, df2$confirmed,
     xlab = "Obesity (% of adult population)",
     ylab = "Confirmed COVID-19 cases (% of population)",
     main = "Scatterplot of Obesity vs Confirmed COVID-19 Cases")
abline(lm(df2$confirmed ~ df2$obesity), col = "red")
dev.off()

# Histogram of the obesity
png("hist_obesity.png")
hist(df2$obesity,
     main = "Histogram of Obesity (%)",
     xlab = "Obesity (% of adult population)",
     ylab = "Number of countries",
     col  = "lightblue")
dev.off()

# Histogram of the confirmed
png("hist_confirmed.png")
hist(df2$confirmed,
     main = "Histogram of Confirmed COVID-19 Cases (%)",
     xlab = "Confirmed cases (% of population)",
     ylab = "Number of countries",
     col  = "lightgreen")
dev.off()







