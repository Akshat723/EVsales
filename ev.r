library(readr)
library(dplyr)
library(ggplot2)

df <- read_csv("/kaggle/input/evdataset/EVIndia.csv",show_col_types = FALSE)

# Extract numeric values from data
df$Price <- ifelse(grepl("Cr", df$PriceRange), 
                   as.numeric(gsub("₹ ([0-9.]+).*", "\\1", df$PriceRange)) * 100,
                   as.numeric(gsub("₹ ([0-9.]+).*", "\\1", df$PriceRange)))
df$Range <- as.numeric(gsub("([0-9]+).*", "\\1", df$Range))
df$Capacity <- as.numeric(gsub("([0-9]+).*", "\\1", df$Capacity))
df$BootSpace <- as.numeric(gsub("([0-9]+).*", "\\1", df$BootSpace))

# Filter cars with sales data
sales_data <- df[!is.na(df$`Units Sold`), ]

print(sales_data[, c("Car", "Units Sold", "Price", "Range", "Capacity", "BootSpace")])

print("Correlation with Sales:")
print(paste("Price vs Sales:", round(cor(sales_data$Price, sales_data$`Units Sold`, use = "complete.obs"), 3)))
print(paste("Range vs Sales:", round(cor(sales_data$Range, sales_data$`Units Sold`, use = "complete.obs"), 3)))
print(paste("BootSpace vs Sales:", round(cor(sales_data$BootSpace, sales_data$`Units Sold`, use = "complete.obs"), 3)))
print(paste("Capacity vs Sales:", round(cor(sales_data$Capacity, sales_data$`Units Sold`, use = "complete.obs"), 3)))

print("\nHigh vs Low Performers:")
high_sales <- sales_data[sales_data$`Units Sold` > 5000, ]
print(high_sales['Car'])
low_sales <- sales_data[sales_data$`Units Sold` <= 5000, ]
print(low_sales['Car'])

print(paste("High Sales - Avg Price:", round(mean(high_sales$Price, na.rm = TRUE), 1), "L"))
print(paste("Low Sales - Avg Price:", round(mean(low_sales$Price, na.rm = TRUE), 1), "L"))
print(paste("High Sales - Avg Range:", round(mean(high_sales$Range, na.rm = TRUE), 0), "Km"))
print(paste("Low Sales - Avg Range:", round(mean(low_sales$Range, na.rm = TRUE), 0), "Km"))
print(paste("High Sales - Avg BootSpace:", round(mean(high_sales$BootSpace, na.rm = TRUE), 0), "L"))
print(paste("Low Sales - Avg BootSpace:", round(mean(low_sales$BootSpace, na.rm = TRUE), 0), "L"))
print(paste("High Sales - Avg Capacity:", round(mean(high_sales$Capacity, na.rm = TRUE), 0)))
print(paste("Low Sales - Avg Capacity:", round(mean(low_sales$Capacity, na.rm = TRUE), 0)))

ggplot(sales_data, aes(x = Price, y = `Units Sold`)) +
  geom_point(size = 4, color = "blue") +
  geom_text(aes(label = Car), vjust = -0.5, size = 3) +
  labs(title = "Price vs Sales", x = "Price (₹ Lakhs)", y = "Units Sold") +
  theme_minimal()

ggplot(sales_data, aes(x = Range, y = `Units Sold`)) +
  geom_point(size = 4, color = "green") +
  geom_text(aes(label = Car), vjust = -0.5, size = 3) +
  labs(title = "Range vs Sales", x = "Range (Km)", y = "Units Sold") +
  theme_minimal()

ggplot(sales_data, aes(x = BootSpace, y = `Units Sold`)) +
  geom_point(size = 4, color = "red") +
  geom_text(aes(label = Car), vjust = -0.5, size = 3) +
  labs(title = "BootSpace vs Sales", x = "BootSpace (L)", y = "Units Sold") +
  theme_minimal()

ggplot(sales_data, aes(x = Capacity, y = `Units Sold`)) +
  geom_point(size = 4, color = "purple") +
  geom_text(aes(label = Car), vjust = -0.5, size = 3) +
  labs(title = "Capacity vs Sales", x = "Capacity (Seater)", y = "Units Sold") +
  theme_minimal()