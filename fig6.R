regg <- cces18 %>% 
  filter(pid7 <= 7) %>% 
  mutate(male = case_when(gender == 1 ~ 1, TRUE ~ 0)) %>% 
  mutate(white = case_when(race == 1 ~ 1, TRUE ~ 0)) %>% 
  mutate(atheist = case_when(religpew == 9 ~ 1, TRUE ~ 0)) %>% 
  mutate(agnostic = case_when(religpew == 10 ~ 1, TRUE ~ 0)) %>% 
  mutate(nip = case_when(religpew == 11 ~ 1, TRUE ~ 0)) %>% 
  mutate(att = case_when(pew_churatd == 6 ~ 1,
                         pew_churatd == 5 ~ 2, 
                         pew_churatd == 4 ~ 3, 
                         pew_churatd == 3 ~ 4, 
                         pew_churatd == 2 ~ 5, 
                         pew_churatd == 1 ~ 6)) %>% 
  mutate(age = 2018 - birthyr) %>% 
  mutate(age = age - 18) %>% 
  mutate(age = age/77) %>% 
  mutate(att = att/6) %>% 
  mutate(pid = pid7/7) %>% 
  mutate(ed = educ/6) %>% 
  select(male, white, atheist, agnostic, nip, att, age, pid, ed)

regg1 <- gss %>% 
  filter(year == 2018) %>% 
  filter(partyid <= 6) %>% 
  filter(attend <= 8) %>% 
  mutate(male = case_when(sex == 1 ~ 1, TRUE ~ 0)) %>% 
  mutate(white = case_when(race == 1 ~ 1, TRUE ~ 0)) %>% 
  mutate(att = attend/8) %>% 
  mutate(age = 2018 - birthyr) %>% 
  mutate(age = age - 18) %>% 
  mutate(age = age/71) %>% 
  mutate(pid7 = partyid + 1) %>% 
  mutate(pid = pid7/7) %>% 
  mutate(ed = educ/20) %>% 
  select(male, white, att, age, pid, nofaith, ed)


reg1 <- glm(atheist ~ male + white + att + age + pid + ed, data = regg, family = "binomial")
reg2 <- glm(agnostic ~ male + white + att + age + pid + ed, data = regg, family = "binomial")
reg3 <- glm(nip ~ male + white + att + age + pid + ed, data = regg, family = "binomial")
reg4 <- glm(nofaith ~ male + white + att + age + pid + ed, data = regg1, family = "binomial")


nm <- c("Atheist", "Agnostic", "Nothing in Particular", "GSS None")
coef <- c("Male" = "male", 
          "White" = "white", 
          "Church Attendance" = "att", 
          "Age" = "age", 
          "Republican ID" = "pid",
          "Education" = "ed", 
          "Constant" = "(Intercept)")

plol <- plot_summs(reg1, reg2, reg3, reg4, model.names = nm, coefs = coef)

plol +
  theme_gg("Ubuntu") +
  theme(legend.position = c(.3, .2)) +
  labs(x = "Coefficient Estimate", y = "Independent Variable", title = "Predicting Religious Identity", caption = "Data: CCES + GSS 2018") +
  ggsave("D://nones/images/reg_out.png", type = "cairo-png")