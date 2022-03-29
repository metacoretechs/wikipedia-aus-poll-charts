library(ggplot2)
library(dplyr)
setwd("~/Documents/GitHub/wikipedia-aus-poll-charts") # replace with your own working directory
polling1922 <- read.csv("polling1922.csv")
ppm1922 <- read.csv("ppm1922.csv")
albosat <- read.csv("albanese_sat1922.csv")
scomosat <- read.csv("morrison_sat1922.csv")
essential <- read.csv("essential_polling1922.csv")
spansize <- 0.4

# Process Essential undecided to allocate on % vote ratio, join to other table
essential <- essential %>%
  mutate(pv_total = pv_lnp_raw + pv_alp_raw + pv_grn_raw + pv_onp_raw + pv_uap_raw + pv_oth_raw,
         tpp_total = tpp_lnp_raw + tpp_alp_raw,
         pv_lnp = pv_lnp_raw+(undec*pv_lnp_raw/pv_total),
         pv_alp = pv_alp_raw+(undec*pv_alp_raw/pv_total),
         pv_grn = pv_grn_raw+(undec*pv_grn_raw/pv_total),
         pv_onp = pv_onp_raw+(undec*pv_onp_raw/pv_total),
         pv_uap = pv_uap_raw+(undec*pv_uap_raw/pv_total),
         pv_oth = pv_oth_raw+(undec*pv_oth_raw/pv_total),
         tpp_lnp = tpp_lnp_raw+(undec*tpp_lnp_raw/tpp_total),
         tpp_alp = tpp_alp_raw+(undec*tpp_alp_raw/tpp_total)) %>%
  select(Date,last_date,Firm,pv_lnp,pv_alp,pv_grn,pv_onp,pv_uap,pv_oth,tpp_lnp,tpp_alp)

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
  geom_point(aes(y=pv_onp), colour="orange3", size=2, alpha = 5/10) +
  geom_smooth(aes(y=pv_onp, colour="ONP"), span = spansize, se = FALSE) +
  geom_point(aes(y=pv_uap), colour="yellow3", size=2, alpha = 5/10) +
  geom_smooth(aes(y=pv_uap, colour="UAP"), span = spansize, se = FALSE) +
  geom_point(aes(y=pv_oth), colour="gray60", size=2, alpha = 3/10) +
  geom_smooth(aes(y=pv_oth, colour="OTH"), span = spansize, se = FALSE) +
  scale_y_continuous(limits=c(0, 50), breaks=c(0, 5, 10, 15, 20, 25, 30, 35, 40, 45, 50), minor_breaks = NULL, expand = c(0,0)) +
  scale_x_date(date_breaks = "1 month", date_labels = "%b %Y", minor_breaks = NULL) +
  theme(axis.text.x = element_text(angle=45, vjust=0.5, size=12), axis.text.y = element_text(size=12), axis.title.y = element_text(size=14)) +
  labs(y="Voters (%)", x= NULL) +
  scale_colour_manual(name="", 
                     labels = c("ALP", "Greens", "Liberal-National Coalition", "One Nation", "United Australia", "Other"), 
                     values = c("ALP"="red3", "GRN"="green4", "LNP"="blue4", "ONP"="orange3", "UAP"="yellow3", "OTH"="gray60"))
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
  scale_colour_manual(name="", 
                     labels = c("Australian Labor Party", "Liberal-National Coalition"), 
                     values = c("ALP"="red3", "LNP"="blue4"))
tpp + theme(legend.position="bottom", legend.box = "horizontal", legend.text = element_text(size=12))

ppm <- ggplot(ppm1922, aes(x=as.Date(date, '%d-%b-%y'))) +
  theme_bw() +
  geom_point(aes(y=morrison), colour="blue4", size=2, alpha = 3/10) +
  geom_smooth(aes(y=morrison, colour="Morrison"), span = spansize, se = FALSE) +
  geom_point(aes(y=albanese), colour="red3", size=2, alpha = 3/10) +
  geom_smooth(aes(y=albanese, colour="Albanese"), span = spansize, se = FALSE) +
  geom_point(aes(y=unknown), colour="grey20", size=2, alpha = 3/10) +
  geom_smooth(aes(y=unknown, colour="Don't Know"), span = spansize, se = FALSE) +
  scale_y_continuous(limits=c(0, 70), breaks = c(10, 20, 30, 40, 50, 60), minor_breaks = NULL, expand = c(0,0)) +
  scale_x_date(date_breaks = "1 month", date_labels = "%b %Y", minor_breaks = NULL) +
  theme(axis.text.x = element_text(angle=45, vjust=0.5, size=12), axis.text.y = element_text(size=12), axis.title.y = element_text(size=14)) +
  labs(y="Support (%)", x= NULL) +
  scale_colour_manual(name="", 
                     labels = c("Morrison", "Albanese", "Don't Know"), 
                     values = c("Morrison"="blue4", "Albanese"="red3", "Don't Know" = "grey60"))
ppm + theme(legend.position="bottom", legend.box = "horizontal", legend.text = element_text(size=12))

lnp_sat <- ggplot(scomosat, aes(x=as.Date(date, '%d-%b-%y'))) +
  theme_bw() +
  geom_point(aes(y=satisfied), colour="#02e03d", size=2, alpha = 3/10) +
  geom_smooth(aes(y=satisfied, colour="Satisfied"), span = spansize, se = FALSE) +
  geom_point(aes(y=dissatisfied), colour="#f74888", size=2, alpha = 3/10) +
  geom_smooth(aes(y=dissatisfied, colour="Dissatisfied"), span = spansize, se = FALSE) +
  geom_point(aes(y=unknown), colour="#b3b3b3", size=2, alpha = 3/10) +
  geom_smooth(aes(y=unknown, colour="Don't Know"), span = spansize, se = FALSE) +
  scale_y_continuous(limits=c(0, 70), breaks = c(0, 10, 20, 30, 40, 50, 60, 70), minor_breaks = NULL, expand = c(0,0)) +
  scale_x_date(date_breaks = "1 month", date_labels = "%b %Y", minor_breaks = NULL) +
  theme(axis.text.x = element_text(angle=45, vjust=0.5, size=12), axis.text.y = element_text(size=12), axis.title.y = element_text(size=14)) +
  labs(y="% satisfaction", x= NULL, title = "Scott Morrison approval rating") +
  scale_colour_manual(name="", 
                     labels = c("Satisfied", "Dissatisfied", "Don't Know"), 
                     values = c("Satisfied"="#02e03d", "Dissatisfied"="#f74888", "Don't Know" = "#b3b3b3"))
lnp_sat + theme(legend.position="bottom", legend.box = "horizontal", legend.text = element_text(size=12))

alp_sat <- ggplot(albosat, aes(x=as.Date(date, '%d-%b-%y'))) +
  theme_bw() +
  geom_point(aes(y=satisfied), colour="#02e03d", size=2, alpha = 3/10) +
  geom_smooth(aes(y=satisfied, colour="Satisfied"), span = spansize, se = FALSE) +
  geom_point(aes(y=dissatisfied), colour="#f74888", size=2, alpha = 3/10) +
  geom_smooth(aes(y=dissatisfied, colour="Dissatisfied"), span = spansize, se = FALSE) +
  geom_point(aes(y=unknown), colour="#b3b3b3", size=2, alpha = 3/10) +
  geom_smooth(aes(y=unknown, colour="Don't Know"), span = spansize, se = FALSE) +
  scale_y_continuous(limits=c(0, 70), breaks = c(0, 10, 20, 30, 40, 50, 60, 70), minor_breaks = NULL, expand = c(0,0)) +
  scale_x_date(date_breaks = "1 month", date_labels = "%b %Y", minor_breaks = NULL) +
  theme(axis.text.x = element_text(angle=45, vjust=0.5, size=12), axis.text.y = element_text(size=12), axis.title.y = element_text(size=14)) +
  labs(y="% satisfaction", x= NULL, title = "Anthony Albanese approval rating") +
  scale_colour_manual(name="", 
                      labels = c("Satisfied", "Dissatisfied", "Don't Know"), 
                      values = c("Satisfied"="#02e03d", "Dissatisfied"="#f74888", "Don't Know" = "#b3b3b3"))
alp_sat + theme(legend.position="bottom", legend.box = "horizontal", legend.text = element_text(size=12))