# AI wealth effect | US household equity ownership, Q2 2026
# Federal Reserve Distributional Financial Accounts (via FRED)
# WFRBST01122, WFRBSN09149, WFRBSN40176, WFRBSB50203

library(ggplot2)

ownership <- data.frame(
  wealth_group = factor(
    c("Top 1%", "Next 9%", "Next 40%", "Bottom 50%"),
    levels = c("Bottom 50%", "Next 40%", "Next 9%", "Top 1%")
  ),
  share_pct = c(50.9, 37.2, 11.4, 0.6)
)

# The rounded figures sum to 100.1%, not exactly 100%.
print(ownership)
cat("Top-decile share:",
    sum(ownership$share_pct[ownership$wealth_group %in%
           c("Top 1%", "Next 9%")]), "%\n")

p <- ggplot(ownership, aes(wealth_group, share_pct,
                            fill = wealth_group)) +
  geom_col(width = 0.65, show.legend = FALSE) +
  geom_text(aes(label = paste0(share_pct, "%")),
            hjust = -0.15, size = 4.2) +
  coord_flip(clip = "off") +
  scale_y_continuous(limits = c(0, 58),
                     breaks = seq(0, 50, 10),
                     expand = c(0, 0)) +
  scale_fill_manual(values = c(
    "Top 1%" = "#c43c30", "Next 9%" = "#344f66",
    "Next 40%" = "#7890a1", "Bottom 50%" = "#a9b6bd"
  )) +
  labs(
    title = "Who owns America's equity wealth?",
    subtitle = "Share of US corporate equities and mutual-fund shares, 2026 Q2",
    x = NULL, y = "Share of aggregate holdings (%)",
    caption = paste(
      "Source: Federal Reserve Distributional Financial Accounts via FRED.",
      "Wealth percentile groups; figures rounded.",
      "Not an AI-specific ownership or consumption measure.",
      sep = "\n"
    )
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold", size = 17),
    plot.subtitle = element_text(margin = margin(b = 12)),
    plot.caption = element_text(size = 9, hjust = 0,
                                margin = margin(t = 12)),
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    axis.text.y = element_text(face = "bold"),
    plot.margin = margin(18, 45, 18, 18)
  )

print(p)
ggsave("ai_equity_ownership_2026Q2.png", p,
       width = 9, height = 5.4, dpi = 300, bg = "white")
