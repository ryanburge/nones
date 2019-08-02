## Calculating the Distribution of Religious Groups in 2018 ####

gss <- gss %>% 
  mutate(trad = frcode(other == 59 | other == 60 | other == 61 | other == 62 | other == 64 ~ "Mormon",
                       relig == 1 ~ "Protestant",
                       relig == 2 ~ "Catholic",
                       relig == 3 ~ "Jewish",
                       relig == 4 ~ "None", 
                       relig == 6 ~ "Buddhist", 
                       relig == 7 ~ "Hindu", 
                       relig == 9 ~ "Muslim",
                       relig == 10 ~ "Orthodox",
                       relig == 5 | relig == 8 | relig == 11 | relig == 12 | relig == 13 ~ "Something Else"))

g1 <- gss %>% 
  filter(year == 2018) %>% 
  ct(trad, show_na = FALSE, wt = wtssall) %>% 
  mutate(group = "GSS")

cces <- cces %>% 
  mutate(trad = frcode(religion == 1 ~ "Protestant",
                       religion == 2 ~ "Catholic",
                       religion == 3 ~ "Mormon",
                       religion == 4 ~ "Orthodox",
                       religion == 5 ~ "Jewish",
                       religion == 6 ~ "Muslim",
                       religion == 7 ~ "Buddhist",
                       religion == 8 ~ "Hindu",
                       religion == 9 | religion == 10 | religion == 11 ~ "None",
                       religion == 12 ~ "Something Else"))

g2 <- cces %>% 
  filter(year == 2018) %>% 
  ct(trad, show_na = FALSE, wt = weight) %>% 
  mutate(group = "CCES")


gg <- bind_rows(g1, g2)

gg$facet <- factor(gg$trad, levels = c("Protestant", "Catholic", "None", "Something Else", "Jewish", "Mormon", "Buddhist", "Muslim", "Hindu", "Orthodox"))

## Figure 3 ####

gg %>% 
  ggplot(., aes(x= reorder(group, -pct), y = pct, fill = group)) +
  geom_col(color = "black") +
  facet_wrap(~ facet) +
  theme_gg("Ubuntu") +
  scale_y_continuous(labels = percent) +
  scale_fill_manual(values = c("darkorchid", "red")) +
  geom_text(aes(y = pct + .05, label = paste0(pct*100, '%')), position = position_dodge(width = .9), size = 4, family = "font") +
  labs(x = "", y = "", title = "The Distribution of Religion in Two Surveys", caption = "Data: GSS + CCES 2018") +
  ggsave("D://nones/images/fig3.png", type = "cairo-png")



