#Display_first_five_rows

df <- read.csv("TestingDetails.csv")

df_subset <- df[, c("Date", "Confirmatory.Lab.Percent.Positive..Daily.", "Confirmatory.Lab.7.Day.Percent.Positive", "Presumptive.Lab.Total.Tested..Daily.", "Confirmatory.and.Presumptive.Lab.7.Day.Percent.Positive")]

colnames(df_subset) <- c("DT", "CLPPD", "CL7DPP", "PLTTD", "CPL7DPP")

head(df_subset, 5)


#Display_Graph

# Convert Date column to Date type
df$Date <- as.Date(df$Date, format="%m/%d/%Y")

# Remove rows with missing Date
df <- df[!is.na(df$Date),]

# Extract month-year
df$Month <- format(df$Date, "%Y-%m")

# Convert the target column to numeric (coerce N/A to NA)
df$Confirmatory.and.Presumptive.Lab.Total.Positive..Daily. <- as.numeric(df$Confirmatory.and.Presumptive.Lab.Total.Positive..Daily.)

# Remove rows with NA in the numeric column
df <- df[!is.na(df$Confirmatory.and.Presumptive.Lab.Total.Positive..Daily.),]

# Aggregate total positives by month
monthly_counts <- aggregate(df$Confirmatory.and.Presumptive.Lab.Total.Positive..Daily.,
                            by=list(Month=df$Month), FUN=sum)

#Create barplot & store bar position

bp <- barplot(monthly_counts$x,
              names.arg = monthly_counts$Month,
              las = 2,            # Rotate labels vertically
              cex.names = 0.7,    # Reduce label size
              main = "Ohio's Covid 19 Monthly Total Positive cases",
              xlab = "Month",
              ylab = "Total Positives",
              col = "lightcoral",
              axes = FALSE) 
# Add custom y-axis in thousands

axis(2, at = pretty(monthly_counts$x),
     labels = paste0(pretty(monthly_counts$x) / 1000, "k"))

