## Calculating Attendance Comparisons ####

gg1 <- cces %>% 
  filter(year == 2018) %>% 
  filter(religion == 1 | religion == 2) %>% 
  mutate(relig = frcode(religion == 1 ~ "Protestant", 
                        religion == 2 ~ "Catholic")) %>% 
  mutate(att = frcode(pew_attendance == 6 ~ "Never",
                      pew_attendance == 5 ~ "Seldom",
                      pew_attendance == 4 ~ "Yearly",
                      pew_attendance == 3 ~ "Monthly",
                      pew_attendance == 2 ~ "Weekly",
                      pew_attendance == 1 ~ "Weekly+")) %>% 
  group_by(relig) %>% 
  ct(att, wt = weight, show_na = FALSE) %>% 
  mutate(group = "CCES")

gg2 <- gss %>% 
  filter(year == 2018) %>% 
  filter(relig == 1 | relig == 2) %>% 
  mutate(relig = frcode(relig == 1 ~ "Protestant", 
                        relig == 2 ~ "Catholic")) %>% 
  mutate(att = frcode(attend == 0 ~ "Never",
                      attend == 1 ~ "Seldom",
                      attend == 2 | attend == 3 ~ "Yearly",
                      attend == 4 | attend == 5 ~ "Monthly",
                      attend == 6 | attend == 7 ~ "Weekly",
                      attend == 8 ~ "Weekly+")) %>% 
  group_by(relig) %>% 
  ct(att, wt = wtssall, show_na = FALSE) %>% 
  mutate(group = "GSS")


graph <- bind_rows(gg1, gg2)

## Figure 4 ####

graph %>% 
  filter(att == "Never" | att == "Weekly+") %>% 
  ggplot(., aes(x = att, y = pct, fill = fct_rev(group))) +
  geom_col(color = "black", position = "dodge") +
  theme_gg("Ubuntu") +
  scale_fill_manual(values = c("red", "darkorchid")) +
  facet_wrap(~ relig) +
  theme(legend.position = "bottom") +
  scale_y_continuous(labels = percent) +
  geom_text(aes(y = pct + .005, label = paste0(pct*100, '%')), position = position_dodge(width = .9), size = 4, family = "font") +
  labs(x = "", y = "", title = "Church Attendance Comparisons", caption = "Data: GSS + CCES 2018") +
  ggsave("D://nones/images/fig4.png", type = "cairo-png")

