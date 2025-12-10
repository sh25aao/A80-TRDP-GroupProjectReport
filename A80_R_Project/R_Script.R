# Load Dataset
df <- read.csv("TestingDetails.csv")

# 1) Load tidyverse

library(tidyverse)


# 2) Read the CSV (mark common NA strings as missing)

df <- readr::read_csv(
  "TestingDetails.csv",
  na = c("N/A", "NA", "", " "),
  show_col_types = FALSE
)

# 3) Set DV column name (short reference)

dv <- "Confirmatory and Presumptive Lab Percent Positive (Daily)"

# 4) Basic cleaning & preparation (tidyverse)
#    - Parse Date
#    - Drop rows with missing Date
#    - Derive Year from Date
#    - Coerce DV to numeric (non-numeric -> NA)

df <- df %>%
  mutate(
    Date = as.Date(Date, format = "%m/%d/%Y")                    # parse Date (mm/dd/yyyy)
  ) %>%
  filter(!is.na(Date))                                           # drop rows where Date is NA
df <- df %>%
  mutate(
    Year = as.integer(format(Date, "%Y")),                       # derive Year (IV)
    `Confirmatory and Presumptive Lab Percent Positive (Daily)` =
      suppressWarnings(as.numeric(`Confirmatory and Presumptive Lab Percent Positive (Daily)`))
  )

# 5) Build analysis frame for 2020–2022
#    - Year as factor
#    - Percent = DV * 100
#    - Remove missing DV values

df_dep <- df %>%
  filter(Year %in% c(2020, 2021, 2022)) %>%
  transmute(
    Year    = factor(Year),                                      # categorical x-axis
    Percent = .data[[dv]] * 100                                  # DV in percent
  ) %>%
  filter(!is.na(Percent))                                        # drop missing DV

# 6) BOXPLOT (ggplot2, tidyverse style)
#    - Includes outliers by default
#    - Mean shown as a crossbar
#    - Clear labels and minimal theme

p_box <- ggplot(df_dep, aes(x = Year, y = Percent, fill = Year)) +
  geom_boxplot(width = 0.65, alpha = 0.25,
               outlier.colour = "black", outlier.size = 1.6) +
  stat_summary(fun = mean, geom = "crossbar",
               width = 0.5, colour = "black", size = 0.5) +
  scale_y_continuous(labels = function(x) paste0(x, "%")) +
  labs(
    title = "Ohio COVID-19: Daily Percent Positive by Year (2020–2022)",
    x     = "Year (independent variable derived from Date)",
    y     = "Daily percent positive (%)\n(Confirmatory + Presumptive tests)"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold"),
    panel.grid.minor = element_blank(),
    legend.position = "none"                                     # legend not needed
  )

# 7) Show and save the boxplot

print(p_box)
