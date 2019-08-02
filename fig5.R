
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


gg1 <- gss %>% 
  filter(year == 2018) %>% 
  mutate(pid7 = partyid + 1) %>% 
  filter(pid7 <= 7) %>% 
  group_by(trad) %>% 
  mean_ci(pid7, wt = wtssall) %>% 
  mutate(survey = "GSS")


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

gg2 <- cces %>%
  filter(year == 2018) %>% 
  filter(pid7 <= 7) %>% 
  group_by(trad) %>% 
  mean_ci(pid7, wt = weight) %>% 
  mutate(survey = "CCES")

cces <- cces %>% 
  mutate(trad = frcode(religion == 9 ~ "Atheist",
                       religion == 10 ~ "Agnostic",
                       religion == 11 ~ "Nothing in Particular"))

gg3 <- cces %>%
  filter(year == 2018) %>% 
  filter(pid7 <= 7) %>% 
  group_by(trad) %>% 
  mean_ci(pid7, wt = weight) %>% 
  mutate(survey = "CCES") 

graph <- bind_rows(gg1, gg2, gg3) %>% filter(trad != "NA") %>% filter(trad != "Orthodox")

graph$trad <- factor(graph$trad, levels = c("Atheist", "Agnostic", "Nothing in Particular", "None", "Protestant", "Catholic", "Jewish", "Mormon", "Hindu", "Buddhist", "Muslim", "Something Else")) 

graph %>% 
  ggplot(., aes(y=mean, x= fct_rev(trad), color = survey)) +
  geom_point(position=position_dodge(width=0.5), size =1.5) +
  geom_errorbar(aes(ymin = lower, ymax=upper), position=position_dodge(0.5), size = .25, width = 0.25) +
  coord_flip() +
  theme_gg("Ubuntu") +
  guides(colour = guide_legend(reverse=T)) +
  scale_color_manual(values = c("darkorchid", "red")) +
  theme(legend.position = c(.85,.85)) +
  scale_y_continuous(breaks = c(1,2,3,4,5,6,7), labels = c("Strong\nDemocrat", "Moderate\nDemocrat", "Lean\nDemocrat", "Independent", "Lean\nRepublican", "", "Strong\nRepublican"), limits = c(1.9,5.6)) +
  labs(x = "Religious Tradition", y = "Mean Partisanship", title = "The Mean Party ID of Religious Groups", caption = "Data: GSS + CCES 2018") +
  ggsave("D://nones/images/fig5.png", type = "cairo-png")