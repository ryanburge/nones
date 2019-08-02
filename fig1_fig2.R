over <- gss %>% 
  filter(year == 1982 | year == 1987) %>% 
  group_by(year) %>% 
  ct(reltrad, wt = oversamp)

## This is the weight for the rest of the sample ####
wtss <- gss %>% 
  group_by(year) %>% 
  ct(reltrad, wt = wtssall)

###Removing the two years that used the overweight ####
wtss <- wtss %>% 
  filter(year != 1982) %>% 
  filter(year != 1987)

## Bind them both together ####
graph <- bind_rows(over, wtss)

gss_graph <- graph %>% 
  filter(reltrad == 7) %>% 
  mutate(group = "GSS Nones") %>% 
  select(year, pct, group)

## Calculating CCES None Groups Individually ####

cces_graph <- cces %>% 
  filter(year != 2006) %>% 
  group_by(year) %>% 
  ct(religion, wt = weight) %>% 
  filter(religion == 9 | religion == 10 | religion == 11) %>% 
  mutate(group = frcode(religion == 9  ~ "CCES Atheist",
                        religion == 10 ~ "CCES Agnostic", 
                        religion == 11 ~ "CCES Nothing in Particular")) %>% 
  select(year, pct, group)

## Calculating CCES Nones as a Whole ####

cces_graph1 <- cces %>% 
  filter(year != 2006) %>% 
  mutate(group = frcode(religion == 9 | religion == 10 | religion == 11 ~ "CCES Nones",
                        TRUE ~ "All Others")) %>% 
  group_by(year) %>% 
  ct(group, wt = weight) %>% 
  filter(group != "All Others") %>% 
  select(year, pct, group)

graph <- bind_rows(gss_graph, cces_graph, cces_graph1)

## Figure 1 ####

graph %>% 
  filter(year >= 2008) %>% 
  filter(group == "CCES Nones" | group == "GSS Nones") %>% 
  ggplot(., aes(x = year, y = pct, group = group, color = group)) +
  geom_point(shape = 21, stroke =2) +
  geom_line() +
  geom_point(size =1, color = "white", fill = "white") +
  scale_y_continuous(labels = percent) +
  scale_color_manual(values = c("darkorchid", "red")) +
  theme_gg("Ubuntu") +
  scale_x_continuous(breaks = c(2008, 2010, 2012, 2014, 2016, 2018)) +
  labs(x = "", y = "", title = "Counting the Nones in Two Surveys", caption = "Data: GSS + CCES (2008-2018)") +
  annotate("text", x=2012, y = .26, label = "CCES", size = 5, family = "font") +
  annotate("text", x=2011.75, y = .185, label = "GSS", size = 5, family = "font") +
  geom_segment(aes(x = 2008, y = .171, xend = 2008, yend = .22), color = "black", linetype = "dashed") +
  geom_segment(aes(x = 2014, y = .209, xend = 2014, yend = .274), color = "black", linetype = "dashed") +
  geom_segment(aes(x = 2018, y = .235, xend = 2018, yend = .31), color = "black", linetype = "dashed") +
  annotate("text", x=2008.65, y = .197, label = "5.6%", size = 5, family = "font") +
  annotate("text", x=2014.6, y = .245, label = "7.1%", size = 5, family = "font") +
  annotate("text", x=2017.4, y = .2685, label = "8.2%", size = 5, family = "font") +
  ggsave("D://nones/images/fig1.png", type = "cairo-png")

## Figure 2 ####

graph %>% 
  filter(year >= 2008) %>% 
  filter(group != "CCES Nones") %>% 
  ggplot(., aes(x = year, y = pct, group = group, color = group)) +
  geom_point(shape = 21, stroke =2) +
  geom_line() +
  geom_point(size =1, color = "white", fill = "white") +
  scale_y_continuous(labels = percent) +
  scale_x_continuous(breaks = c(2008, 2010, 2012, 2014, 2016, 2018)) +
  theme_gg("Ubuntu") +
  annotate("text", x=2012, y = .21, label = "GSS\nNones", size = 4, family = "font") +
  annotate("text", x=2012, y = .155, label = "CCES\nNothing in\nParticular", size = 4, family = "font") +
  annotate("text", x=2012, y = .065, label = "CCES\nAgnostic", size = 4, family = "font") +
  annotate("text", x=2014.7, y = .038, label = "CCES\nAtheist", size = 4, family = "font") +
  scale_color_aaas() +
  # scale_color_manual(values = c("blue", "green", "purple", "red")) +
  labs(x = "", y = "", title = "Types of Nones in the GSS and CCES", caption = "Data: GSS + CCES 2008-2018") +
  ggsave("D://nones/images/fig2.png", type = "cairo-png")

