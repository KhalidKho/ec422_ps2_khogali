# Part 3

library(tidyquant)
library(tidyverse)

# Original download code retained for reproducibility:
# unemp_raw <- tq_get(
#   c("UNRATE", "ORUR", "CAUR", "TXUR", "NYUR"),
#   get = "economic.data",
#   from = "1976-01-01"
# )
#
# head(unemp_raw)
# dir.create("data", showWarnings = FALSE)
# write_rds(unemp_raw, "data/unemp_raw.rds")

unemp_raw <- read_rds("data/unemp_raw.rds")

head(unemp_raw)


# Part 4.1

unemp <- unemp_raw |>
  rename(unemployment_rate = price) |>
  mutate(
    region = case_when(
      symbol == "UNRATE" ~ "United States",
      symbol == "ORUR" ~ "Oregon",
      symbol == "CAUR" ~ "California",
      symbol == "TXUR" ~ "Texas",
      symbol == "NYUR" ~ "New York"
    )
  )

head(unemp)

# Replace October 2025 with September 2025

unemp <- unemp |>
  arrange(region, date) |>
  group_by(region) |>
  mutate(
    unemployment_rate = replace(
      unemployment_rate,
      date == as.Date("2025-10-01"),
      unemployment_rate[date == as.Date("2025-09-01")]
    )
  ) |>
  ungroup()

# Calculate month-over-month changes

unemp <- unemp |>
  arrange(region, date) |>
  group_by(region) |>
  mutate(
    unemployment_rate_change =
      unemployment_rate - lag(unemployment_rate)
  ) |>
  ungroup()

# Keep January 2015 onward

unemp_recent <- unemp |>
  filter(date >= as.Date("2015-01-01"))

# Full-sample summary

unemp_summary <- unemp |>
  group_by(region) |>
  summarize(
    mean_rate = mean(unemployment_rate, na.rm = TRUE),
    min_rate = min(unemployment_rate, na.rm = TRUE),
    max_rate = max(unemployment_rate, na.rm = TRUE)
  )

# Summary from January 2015 onward

unemp_recent_summary <- unemp_recent |>
  group_by(region) |>
  summarize(
    mean_rate = mean(unemployment_rate, na.rm = TRUE),
    min_rate = min(unemployment_rate, na.rm = TRUE),
    max_rate = max(unemployment_rate, na.rm = TRUE)
  )

unemp_summary
unemp_recent_summary

# Largest increases, including ties

unemp_top_increases <- unemp |>
  slice_max(
    order_by = unemployment_rate_change,
    n = 5,
    with_ties = TRUE,
    na_rm = TRUE
  )

# Largest decreases, including ties

unemp_top_decreases <- unemp |>
  slice_min(
    order_by = unemployment_rate_change,
    n = 5,
    with_ties = TRUE,
    na_rm = TRUE
  )

unemp_top_increases
unemp_top_decreases

# Largest increases excluding 2020

unemp_top_increases_no_covid <- unemp |>
  filter(
    date < as.Date("2020-01-01") |
      date >= as.Date("2021-01-01")
  ) |>
  slice_max(
    order_by = unemployment_rate_change,
    n = 5,
    with_ties = TRUE,
    na_rm = TRUE
  )

# Largest decreases excluding 2020

unemp_top_decreases_no_covid <- unemp |>
  filter(
    date < as.Date("2020-01-01") |
      date >= as.Date("2021-01-01")
  ) |>
  slice_min(
    order_by = unemployment_rate_change,
    n = 5,
    with_ties = TRUE,
    na_rm = TRUE
  )

unemp_top_increases_no_covid
unemp_top_decreases_no_covid

# Part 5.1: Combined line plot

plot_all <- ggplot(
  unemp,
  aes(x = date, y = unemployment_rate, color = region)
) +
  geom_line(linewidth = 0.5) +
  labs(
    title = "Monthly unemployment rates by region",
    x = "Year",
    y = "Unemployment rate (%)",
    color = "Region"
  ) +
  theme_minimal() +
  theme(legend.position = "bottom")

plot_all

# Part 5.2: Faceted line plot

plot_facets <- ggplot(
  unemp,
  aes(x = date, y = unemployment_rate, color = region)
) +
  geom_line(linewidth = 0.5) +
  facet_wrap(~ region) +
  labs(
    title = "Monthly unemployment rates: individual regions",
    x = "Year",
    y = "Unemployment rate (%)"
  ) +
  theme_minimal() +
  theme(legend.position = "none")

plot_facets

# Part 5.3: Mean unemployment by region

plot_means <- ggplot(
  unemp_summary,
  aes(
    x = fct_reorder(region, mean_rate),
    y = mean_rate
  )
) +
  geom_col(fill = "steelblue") +
  labs(
    title = "Average unemployment rate by region",
    x = "Region",
    y = "Mean unemployment rate (%)"
  ) +
  theme_minimal()

plot_means

# Part 5.4: Oregon versus the national rate

plot_oregon_us <- unemp_recent |>
  filter(region %in% c("Oregon", "United States")) |>
  ggplot(
    aes(x = date, y = unemployment_rate, color = region)
  ) +
  geom_line(linewidth = 0.7) +
  labs(
    title = "Oregon versus U.S. unemployment since 2015",
    x = "Year",
    y = "Unemployment rate (%)",
    color = "Region"
  ) +
  theme_minimal() +
  theme(legend.position = "bottom")

plot_oregon_us

