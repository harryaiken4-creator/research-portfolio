# Article 3: Gulf crude export routes before and during the 2026 conflict
# Source: Kpler, 29 September 2026.
# Pre-war: March 2025–February 2026; September: 1–28 September 2026.
# Shares of non-Iranian Gulf crude/condensate exports by route, not volumes.
library(ggplot2)

df <- data.frame(
  period = c("Before the war", "Before the war", "September 2026", "September 2026"),
  route = c("Via Hormuz", "Bypass routes", "Via Hormuz", "Bypass routes"),
  share = c(83,17,60,40)
)
df$period <- factor(df$period, levels = c("Before the war", "September 2026"))
df$route <- factor(df$route, levels = c("Bypass routes", "Via Hormuz"))
p <- ggplot(df, aes(x=period, y=share, fill=route)) +
  geom_col(width=0.55) +
  geom_text(aes(label=paste0(share,"%")), position=position_stack(vjust=0.5),
            colour="white", fontface="bold", size=5) +
  scale_fill_manual(values=c("Bypass routes"="#C65D49", "Via Hormuz"="#213D54")) +
  scale_y_continuous(limits=c(0,100),breaks=seq(0,100,20),expand=c(0,0)) +
  labs(title="How the Gulf's oil routes changed",
       subtitle="Share of non-Iranian Gulf crude exports by route (%)",
       x=NULL,y="Share of crude leaving the region (%)",fill=NULL,
       caption=paste("Source: Kpler, analysis as of 29 September 2026.",
       "Before the war: March 2025–February 2026; September: 1–28 September 2026.",
       "Red Sea and Gulf of Oman loadings bypass Hormuz. Calculations and chart: author.",
       sep="\n")) +
  theme_minimal(base_size=13) +
  theme(legend.position="top",panel.grid.major.x=element_blank(),
        panel.grid.minor=element_blank(),
        plot.title=element_text(face="bold",size=17),
        plot.subtitle=element_text(margin=margin(b=12)),
        plot.caption=element_text(hjust=0,size=9,margin=margin(t=12)))
print(p)
ggsave("adipec_gulf_export_routes.png",plot=p,width=9,height=5.8,dpi=300,bg="white")
