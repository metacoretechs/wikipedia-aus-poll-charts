library(ggplot2)
library(dplyr)
setwd("~/Documents/GitHub/wikipedia-aus-poll-charts") # replace with your own working directory
polling1922 <- read.csv("polling1922.csv")
essential <- read.csv("essential_polling1922.csv")
spansize <- 0.4

# Process Essential undecided to allocate on % vote ratio, join to other table
essential <- essential %>%
  mutate(pv_total = pv_lnp_raw + pv_alp_raw + pv_grn_raw + pv_onp_raw + pv_oth_raw,
         tpp_total = tpp_lnp_raw + tpp_alp_raw,
         pv_lnp = pv_lnp_raw+(undec*pv_lnp_raw/pv_total),
         pv_alp = pv_alp_raw+(undec*pv_alp_raw/pv_total),
         pv_grn = pv_grn_raw+(undec*pv_grn_raw/pv_total),
         pv_onp = pv_onp_raw+(undec*pv_onp_raw/pv_total),
         pv_oth = pv_oth_raw+(undec*pv_oth_raw/pv_total),
         tpp_lnp = tpp_lnp_raw+(undec*tpp_lnp_raw/tpp_total),
         tpp_alp = tpp_alp_raw+(undec*tpp_alp_raw/tpp_total)) %>%
  select(Date,last_date,Firm,pv_lnp,pv_alp,pv_grn,pv_onp,pv_oth,tpp_lnp,tpp_alp)

polling1922 <- polling1922 %>%
  bind_rows(essential) %>%
  arrange(desc(as.Date(last_date, '%d %b %Y')))

primary_votes <- ggplot(polling1922, aes(x=as.Date(last_date, '%d %b %Y'))) +
  theme_bw() +
  geom_point(aes(y=pv_lnp), colour="blue4", size=2, alpha = 3/10) +
  geom_smooth(aes(y=pv_lnp, colour="LNP"), span = spansize, se = FALSE) +
  geom_point(aes(y=pv_alp), colour="red3", size=2, alpha = 3/10) +
  geom_smooth(aes(y=pv_alp, colour="ALP"), span = spansize, se = FALSE) +
  geom_point(aes(y=pv_grn), colour="green4", size=2, alpha = 3/10) +
  geom_smooth(aes(y=pv_grn, colour="GRN"), span = spansize, se = FALSE) +
  geom_point(aes(y=pv_onp), colour="yellow3", size=2, alpha = 5/10) +
  geom_smooth(aes(y=pv_onp, colour="ONP"), span = spansize, se = FALSE) +
  geom_point(aes(y=pv_oth), colour="gray60", size=2, alpha = 3/10) +
  geom_smooth(aes(y=pv_oth, colour="OTH"), span = spansize, se = FALSE) +
  scale_y_continuous(limits=c(0, 50), breaks=c(0, 5, 10, 15, 20, 25, 30, 35, 40, 45, 50), minor_breaks = NULL, expand = c(0,0)) +
  scale_x_date(date_breaks = "1 month", date_labels = "%b %Y", minor_breaks = NULL) +
  theme(axis.text.x = element_text(angle=45, vjust=0.5, size=12), axis.text.y = element_text(size=12), axis.title.y = element_text(size=14)) +
  labs(y="Voters (%)", x= NULL) +
  scale_color_manual(name="", 
                     labels = c("ALP", "Greens", "Liberal-National Coalition", "One Nation", "Other"), 
                     values = c("ALP"="red3", "GRN"="green4", "LNP"="blue4", "ONP"="yellow3", "OTH"="gray60"))
primary_votes + theme(legend.position="bottom", legend.box = "horizontal", legend.text = element_text(size=12))

tpp <- ggplot(polling1922, aes(x=as.Date(last_date, '%d %b %Y'))) +
  theme_bw() +
  geom_point(aes(y=tpp_lnp), colour="blue4", size=2, alpha = 3/10) +
  geom_smooth(aes(y=tpp_lnp, colour="LNP"), span = spansize, se = FALSE) +
  geom_point(aes(y=tpp_alp), colour="red3", size=2, alpha = 3/10) +
  geom_smooth(aes(y=tpp_alp, colour="ALP"), span = spansize, se = FALSE) +
  scale_y_continuous(limits=c(40, 60), breaks=c(42, 44, 46, 48, 50, 52, 54, 56, 58, 60), minor_breaks = NULL, expand = c(0,0)) +
  scale_x_date(date_breaks = "1 month", date_labels = "%b %Y", minor_breaks = NULL) +
  theme(axis.text.x = element_text(angle=45, vjust=0.5, size=12), axis.text.y = element_text(size=12), axis.title.y = element_text(size=14)) +
  labs(y="Voters (%)", x= NULL) +
  scale_color_manual(name="", 
                     labels = c("Australian Labor Party", "Liberal-National Coalition"), 
                     values = c("ALP"="red3", "LNP"="blue4"))
tpp + theme(legend.position="bottom", legend.box = "horizontal", legend.text = element_text(size=12))
