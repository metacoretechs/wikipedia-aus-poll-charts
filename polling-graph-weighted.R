library(ggplot2)
library(dplyr)
library(lubridate)
setwd("~/Documents/GitHub/wikipedia-aus-poll-charts") # replace with your own working directory
polling2225 <- read.csv("polling2225.csv")
ppm2225 <- read.csv("ppm2225.csv")
albosat <- read.csv("albanese_sat2225.csv")
duttonsat <- read.csv("dutton_sat2225.csv")
essential_raw <- read.csv("essential_polling2225.csv")
spansize <- 0.2

election22 <- data.frame(c("LNP","ALP","GRN","ONP","UAP","OTH"), c(35.7,32.6,12.2,5.0,4.1,10.4))
election25 <- data.frame(c("LNP","ALP","GRN","ONP","UAP","OTH"), c(32.1,34.8,11.8,6.2,1.8,13.3))
election22tpp <- data.frame(c("LNP","ALP"), c(47.9,52.1))
election25tpp <- data.frame(c("LNP","ALP"), c(45,55))
names(election22) <- c("party", "vote")
names(election25) <- c("party", "vote")
names(election22tpp) <- c("party", "vote")
names(election25tpp) <- c("party", "vote")

# Process Essential undecided to allocate on % vote ratio, join to other table
essential <- essential_raw %>%
  rowwise() %>%
  mutate(pv_total = sum(pv_lnp_raw, pv_alp_raw, pv_grn_raw, pv_onp_raw, pv_uap_raw, pv_oth_raw, na.rm = TRUE),
         tpp_total = tpp_lnp_raw + tpp_alp_raw,
         pv_lnp = pv_lnp_raw+(undec*pv_lnp_raw/pv_total),
         pv_alp = pv_alp_raw+(undec*pv_alp_raw/pv_total),
         pv_grn = pv_grn_raw+(undec*pv_grn_raw/pv_total),
         pv_onp = pv_onp_raw+(undec*pv_onp_raw/pv_total),
         pv_uap = pv_uap_raw+(undec*pv_uap_raw/pv_total),
         pv_oth = pv_oth_raw+(undec*pv_oth_raw/pv_total),
         tpp_lnp = tpp_lnp_raw+(undec*tpp_lnp_raw/tpp_total),
         tpp_alp = tpp_alp_raw+(undec*tpp_alp_raw/tpp_total)) %>%
  select(Date,last_date,Firm,sample_size,pv_lnp,pv_alp,pv_grn,pv_onp,pv_uap,pv_oth,tpp_lnp,tpp_alp)

polling2225$sample_size[is.na(polling2225$sample_size)] <- round(mean(polling2225$sample_size, na.rm=TRUE))
essential$sample_size[is.na(essential$sample_size)] <- round(mean(essential$sample_size, na.rm=TRUE))

polling2225 <- polling2225 %>%
  bind_rows(essential) %>%
  arrange(desc(as.Date(last_date, '%d %b %Y')))

minss <- min(polling2225$sample_size)
#max_date <- max(as.Date(polling2225$last_date, '%d %b %Y')) + days(7)
max_date <- as.Date('2025-05-03')

primary_votes <- ggplot(polling2225, aes(x=as.Date(last_date, '%d %b %Y'))) +
  theme_bw() +
  geom_point(aes(y=pv_alp, size=sample_size), colour="red3", alpha = 3/10) +
  geom_smooth(aes(y=pv_alp, colour="ALP", weight=sqrt(sample_size)), span = spansize, se = FALSE) +
  geom_point(aes(y=pv_lnp, size=sample_size), colour="blue4", alpha = 3/10) +
  geom_smooth(aes(y=pv_lnp, colour="LNP", weight=sqrt(sample_size)), span = spansize, se = FALSE) +
  geom_point(aes(y=pv_grn, size=sample_size), colour="green4", alpha = 3/10) +
  geom_smooth(aes(y=pv_grn, colour="GRN", weight=sqrt(sample_size)), span = spansize, se = FALSE) +
  geom_point(aes(y=pv_onp, size=sample_size), colour="orange3", alpha = 5/10) +
  geom_smooth(aes(y=pv_onp, colour="ONP", weight=sqrt(sample_size)), span = spansize, se = FALSE) +
  geom_point(aes(y=pv_uap, size=sample_size), colour="yellow3", alpha = 5/10) +
  geom_smooth(aes(y=pv_uap, colour="UAP", weight=sqrt(sample_size)), span = spansize, se = FALSE) +
  geom_point(aes(y=pv_oth, size=sample_size), colour="gray60", alpha = 3/10) +
  geom_smooth(aes(y=pv_oth, colour="OTH", weight=sqrt(sample_size)), span = spansize, se = FALSE) +
  geom_point(data = election22, aes(x = as.Date('2022-05-21', '%Y-%m-%d'), y = vote, colour = party), shape=23, stroke=0.5, fill = "#FFFFFF", size=4) +
  geom_point(data = election22, aes(x = as.Date('2022-05-21', '%Y-%m-%d'), y = vote, colour = party), shape=18, size=3) +
  geom_point(data = election25, aes(x = as.Date('2025-05-03', '%Y-%m-%d'), y = vote, colour = party), shape=23, stroke=0.5, fill = "#FFFFFF", size=4) +
  geom_point(data = election25, aes(x = as.Date('2025-05-03', '%Y-%m-%d'), y = vote, colour = party), shape=18, size=3) +
  scale_y_continuous(limits=c(0, 50), breaks=c(0, 5, 10, 15, 20, 25, 30, 35, 40, 45, 50), minor_breaks = NULL, expand = c(0,0)) +
  scale_x_date(limits=c(as.Date('2022-05-21', '%Y-%m-%d'), max_date), date_breaks = "1 month", date_labels = "%b %Y", minor_breaks = "1 month", expand = c(0,0)) +
  guides(colour = guide_legend(override.aes = list(alpha = 1, shape = 16, size = 3))) +
  theme(legend.key = element_rect(colour = NA, fill = NA), legend.text=element_text(size=12), axis.text.y = element_text(size=12), axis.text.x = element_text(angle=90, vjust=0.5, size=12)) +
  labs(y="Voters (%)", x= NULL) +
  scale_colour_manual(name="", 
                     labels = c("Labor", "Greens", "Liberal-National Coalition", "One Nation", "Other", "Trumpet of Patriots"), 
                     values = c("ALP"="red3", "GRN"="green4", "LNP"="blue4", "ONP"="orange3", "OTH"="gray60", "UAP"="yellow3"))

primary_votes + theme(legend.position="bottom", legend.box = "horizontal", legend.text = element_text(size=12)) +
  guides(colour = guide_legend(order=1, override.aes = list(size = 0, shape = 15)), size = guide_legend(order=2)) +
  scale_size_area(name = "Sample size:")

tpp <- ggplot(polling2225, aes(x=as.Date(last_date, '%d %b %Y'))) +
  theme_bw() +
  geom_point(aes(y=tpp_lnp, size=sample_size), colour="blue4", alpha = 3/10) +
  geom_smooth(aes(y=tpp_lnp, colour="LNP", weight=sqrt(sample_size)), span = spansize, se = FALSE) +
  geom_point(aes(y=tpp_alp, size=sample_size), colour="red3", alpha = 3/10) +
  geom_smooth(aes(y=tpp_alp, colour="ALP", weight=sqrt(sample_size)), span = spansize, se = FALSE) +
  geom_point(data = election22tpp, aes(x = as.Date('2022-05-21', '%Y-%m-%d'), y = vote, colour = party), shape=23, stroke=0.5, fill = "#FFFFFF", size=4) +
  geom_point(data = election22tpp, aes(x = as.Date('2022-05-21', '%Y-%m-%d'), y = vote, colour = party), shape=18, size=3) +
  geom_point(data = election25tpp, aes(x = as.Date('2025-05-03', '%Y-%m-%d'), y = vote, colour = party), shape=23, stroke=0.5, fill = "#FFFFFF", size=4) +
  geom_point(data = election25tpp, aes(x = as.Date('2025-05-03', '%Y-%m-%d'), y = vote, colour = party), shape=18, size=3) +
  scale_y_continuous(limits=c(35, 65), breaks=c(40,45,50,55,60), minor_breaks = NULL, expand = c(0,0)) +
  scale_x_date(limits=c(as.Date('2022-05-21', '%Y-%m-%d'), max_date), date_breaks = "1 month", date_labels = "%b %Y", minor_breaks = "1 month", expand = c(0,0)) +
  theme(legend.key = element_rect(colour = NA, fill = NA), legend.text=element_text(size=12), axis.text.y = element_text(size=12), axis.text.x = element_text(angle=90, vjust=0.5, size=12)) +
  labs(y="Voters (%)", x= NULL) +
  scale_colour_manual(name="", 
                     labels = c("Australian Labor Party", "Liberal-National Coalition"), 
                     values = c("ALP"="red3", "LNP"="blue4"))
tpp + theme(legend.position="bottom", legend.box = "horizontal", legend.text = element_text(size=12)) +
  guides(colour = guide_legend(order=1, override.aes = list(size = 0, shape = 15)), size = guide_legend(order=2)) +
  scale_size_area(name = "Sample size:")

ppm <- ggplot(ppm2225, aes(x=as.Date(date, '%d-%b-%y'))) +
  theme_bw() +
  geom_point(aes(y=albanese), colour="red3", size=2.5, alpha = 3/10) +
  geom_smooth(aes(y=albanese, colour="Albanese"), span = spansize, se = FALSE) +
  geom_point(aes(y=dutton), colour="blue4", size=2.5, alpha = 3/10) +
  geom_smooth(aes(y=dutton, colour="Dutton"), span = spansize, se = FALSE) +
  geom_point(aes(y=unknown), colour="grey20", size=2.5, alpha = 3/10) +
  geom_smooth(aes(y=unknown, colour="Unknown"), span = spansize, se = FALSE) +
  scale_y_continuous(limits=c(0, 70), breaks = c(10, 20, 30, 40, 50, 60), minor_breaks = NULL, expand = c(0,0)) +
  scale_x_date(date_breaks = "1 month", date_labels = "%b %Y", minor_breaks = NULL, expand = c(0.01,0.01)) +
  theme(axis.text.x = element_text(angle=90, vjust=0.5, size=12), axis.text.y = element_text(size=12), axis.title.y = element_text(size=14)) +
  labs(y="Support (%)", x= NULL) +
  scale_colour_manual(name="",
                     labels = c("Albanese", "Dutton", "Don't Know"),
                     values = c("Albanese"="red3", "Dutton"="blue4", "Unknown" = "grey60"))
ppm + theme(legend.position="bottom", legend.box = "horizontal", legend.text = element_text(size=12))

alp_sat <- ggplot(albosat, aes(x=as.Date(date, '%d-%b-%y'))) +
  theme_bw() +
  geom_point(aes(y=satisfied), colour="#02e03d", size=2.5, alpha = 3/10) +
  geom_smooth(aes(y=satisfied, colour="Satisfied"), span = spansize, se = FALSE) +
  geom_point(aes(y=dissatisfied), colour="#f74888", size=2.5, alpha = 3/10) +
  geom_smooth(aes(y=dissatisfied, colour="Dissatisfied"), span = spansize, se = FALSE) +
  geom_point(aes(y=unknown), colour="#b3b3b3", size=2.5, alpha = 3/10) +
  geom_smooth(aes(y=unknown, colour="Don't Know"), span = spansize, se = FALSE) +
  scale_y_continuous(limits=c(0, 70), breaks = c(0, 10, 20, 30, 40, 50, 60, 70), minor_breaks = NULL, expand = c(0,0)) +
  scale_x_date(date_breaks = "1 month", date_labels = "%b %Y", minor_breaks = NULL, expand = c(0.01,0.01)) +
  theme(axis.text.x = element_text(angle=90, vjust=0.5, size=12), axis.text.y = element_text(size=12), axis.title.y = element_text(size=14)) +
  labs(y="% satisfaction", x= NULL, title = "Anthony Albanese approval rating") +
  scale_colour_manual(name="",
                      breaks = c("Satisfied", "Dissatisfied", "Don't Know"),
                      labels = c("Satisfied", "Dissatisfied", "Don't Know"),
                      values = c("Satisfied"="#02e03d", "Dissatisfied"="#f74888", "Don't Know" = "#b3b3b3"))
alp_sat + theme(legend.position="bottom", legend.box = "horizontal", legend.text = element_text(size=12))

lnp_sat <- ggplot(duttonsat, aes(x=as.Date(date, '%d-%b-%y'))) +
  theme_bw() +
  geom_point(aes(y=satisfied), colour="#02e03d", size=2.5, alpha = 3/10) +
  geom_smooth(aes(y=satisfied, colour="Satisfied"), span = spansize, se = FALSE) +
  geom_point(aes(y=dissatisfied), colour="#f74888", size=2.5, alpha = 3/10) +
  geom_smooth(aes(y=dissatisfied, colour="Dissatisfied"), span = spansize, se = FALSE) +
  geom_point(aes(y=unknown), colour="#b3b3b3", size=2.5, alpha = 3/10) +
  geom_smooth(aes(y=unknown, colour="Don't Know"), span = spansize, se = FALSE) +
  scale_y_continuous(limits=c(0, 70), breaks = c(0, 10, 20, 30, 40, 50, 60, 70), minor_breaks = NULL, expand = c(0,0)) +
  scale_x_date(date_breaks = "1 month", date_labels = "%b %Y", minor_breaks = NULL, expand = c(0.01,0.01)) +
  theme(axis.text.x = element_text(angle=90, vjust=0.5, size=12), axis.text.y = element_text(size=12), axis.title.y = element_text(size=14)) +
  labs(y="% satisfaction", x= NULL, title = "Peter Dutton approval rating") +
  scale_colour_manual(name="",
                      breaks = c("Satisfied", "Dissatisfied", "Don't Know"),
                     labels = c("Satisfied", "Dissatisfied", "Don't Know"),
                     values = c("Satisfied"="#02e03d", "Dissatisfied"="#f74888", "Don't Know" = "#b3b3b3"))
lnp_sat + theme(legend.position="bottom", legend.box = "horizontal", legend.text = element_text(size=12))
