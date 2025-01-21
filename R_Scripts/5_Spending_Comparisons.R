#Download 2018 Data
library(haven)
source("R_Scripts/4_Analysis.R")
on18<-read_sav(file="https://github.com/LISPOP/OPES_2018/blob/master/Data/Ontario%20ES%202018%20LISPOP.sav?raw=true")
lookfor(on18, "spending")
lookfor(on22, "spending")
lookfor(on22, "affordable")
lookfor(on22, "spend")

on22 %>% 
  lookfor(., "How much should") ->spend22
on18 %>% 
  lookfor(., "spending")->spend18
spend22
on22$Q18
spend18
on18 %>% 
  mutate(across(starts_with("spending"), ~Recode(as.numeric(.x), "1='More'; 2='Same'; 3='Less'", levels=c("Less", "Same", "More")), .names="{.col}_x"))->on18
on22 %>% 
  mutate(across(Q16:Q21, ~Recode(as.numeric(.x), "1='Less'; 2='Same'; 3='More'", levels=c("Less", "Same", "More")), .names="{.col}_x"))->on22

str_extract_all(spend18$label, "\\[(.*?)\\]") %>% 
unlist() %>% 
  str_remove_all(., "\\[|\\]")->spend18_labels
str_remove_all(spend22$label, "How much should the provincial government spend on ") %>% 
  str_remove_all(., "the ") %>% 
  str_remove_all(., "\\?")->spend22_labels
spend22_labels
spend18_labels

on18 %>% 
  rename_with(~ spend18_labels, starts_with("spending")&ends_with("_x"))->on18
on22 %>% 
  rename_with(~spend22_labels, Q16_x:Q21_x)->on22
names(on18)
on18 %>% 
  pivot_longer(., cols=`Crime and justice`:`Long-term care facilities for seniors`, names_to=c("Spend"), values_to="Preference") %>% 
  select(Spend, Preference) %>% 
  mutate(Year=rep(2018, nrow(.)))->on18_spending
names(on22)
on22 %>% 
  pivot_longer(., cols=education:`affordable housing`,names_to=c("Spend"), values_to="Preference") %>% 
  select(Spend, Preference) %>% 
  mutate(Year=rep(2022, nrow(.)))->on22_spending
on22_spending %>% 
  bind_rows(on18_spending)->spending
table(spending$Spend)
spending$Spend<-str_to_sentence(spending$Spend)
table(spending$Spend)
spending$Spend<-Recode(spending$Spend, "'The environment'='Environment'; 
       'Fighting crime'='Crime and justice'")
library(wlucolors)

spending %>% 
group_by(Spend, Year, Preference) %>% 
  summarize(n=n()) %>% 
  mutate(Percent=(n/sum(n))*100, 
         interval=map(n, prop.test, n=sum(n)), 
         tidied=map(interval, tidy)) %>% 
  unnest(tidied) %>% 
  select(-interval, -statistic, -p.value, -parameter, -method) %>% 
  mutate(conf.low=conf.low*100,
         conf.high=conf.high*100) %>% 
  #filter(Spend=="Affordable housing")
 filter(Preference=="More") %>% 
  #filter(Year==2022) %>% 
  filter(!is.na(Preference)) %>% 
  filter(Spend!="Welfare") %>% 
  filter(str_detect(Spend, "Long-term", negate=T)) %>%
  filter(Spend!="Social programs") %>% 
 filter(str_detect(Spend, "Services", negate=T)) %>% 
  ggplot(., aes(x=Percent, y=fct_reorder(Spend,desc(Percent)), fill=as.factor(Year)))+
  geom_col(position="dodge")+theme(legend.position = "bottom")+
  geom_vline(xintercept=50)+labs(y="Issue", fill="Year")+
  #scale_fill_manual(values=c("purple"))
  scale_fill_mine(palette="wlu", guide = guide_legend(reverse = TRUE), reverse=T, discrete=T)+
  geom_text(aes(label=round(Percent, 0)), position=position_dodge(0.9), hjust=-1)+
  geom_errorbar(aes(xmin=conf.low, xmax=conf.high), width=0, position=position_dodge(0.9))+xlim(c(0,70))->spending_comparison_2022_2018
spending_comparison_2022_2018
ggsave(spending_comparison_2022_2018, width=6, height=4, filename="Plots/spending_comparison_2018_2022.png")

