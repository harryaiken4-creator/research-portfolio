# France–Germany 10-year yield spread | Harry Aiken | 2 Nov 2026
# Download historical daily France and Germany 10Y yield CSVs from Investing.com.
# Change the filenames or choose files interactively. Uses R/tidyverse.
library(tidyverse)

read_yields <- function(path, column) {
  read_csv(path, show_col_types = FALSE) %>%
    transmute(date = as.Date(Date, format = "%m/%d/%Y"),
              yield = parse_number(as.character(Price))) %>%
    rename(!!column := yield) %>%
    distinct(date, .keep_all = TRUE)
}
message("Select France ten-year historical yield CSV")
france <- read_yields(file.choose(), "france_yield")
message("Select Germany ten-year historical yield CSV")
germany <- read_yields(file.choose(), "germany_yield")
data <- inner_join(france, germany, by = "date") %>%
  filter(as.POSIXlt(date)$wday %in% 1:5) %>%
  mutate(spread_bp = (france_yield - germany_yield) * 100) %>%
  arrange(date)
print(nrow(data))
print(data %>% slice_max(spread_bp, n = 1))
write_csv(data, "france_germany_spread_matched.csv")

chart <- ggplot(data, aes(date, spread_bp)) +
  geom_line(linewidth = 0.75, colour = "#213d54") +
  labs(title = "France's sovereign borrowing premium",
       subtitle = "French minus German ten-year yield; matched daily observations",
       x = NULL, y = "Basis points",
       caption = "Source: Investing.com. Calculations: author.") +
  theme_minimal(base_size = 12)
print(chart)
ggsave("france_sovereign_spread_R.png", chart, width = 11,
       height = 6, dpi = 300, bg = "white")
