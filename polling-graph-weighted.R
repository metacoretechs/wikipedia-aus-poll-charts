library(ggplot2)
library(dplyr)
library(lubridate)
setwd("~/Documents/GitHub/wikipedia-aus-poll-charts") # replace with your own working directory
polling2528 <- read.csv("polling2528.csv")
ppm2528 <- read.csv("ppm2528.csv")
albosat <- read.csv("albanese_sat2528.csv")
leysat <- read.csv("ley_sat2528.csv")
# essential_raw <- read.csv("essential_polling2528.csv")
spansize <- 2

election25 <- data.frame(c("ALP","LNP","GRN","ONP","UAP","OTH"), c(31.8,34.6,12.2,6.4,1.9,13.1))
election25tpp <- data.frame(c("LNP","ALP"), c(44.8,55.2))
names(election25) <- c("party", "vote")
names(election25tpp) <- c("party", "vote")

# Process Essential undecided to allocate on % vote ratio, join to other table
# essential <- essential_raw %>%
#   rowwise() %>%
#   mutate(pv_total = sum(pv_lnp_raw, pv_alp_raw, pv_grn_raw, pv_onp_raw, pv_uap_raw, pv_oth_raw, na.rm = TRUE),
#          tpp_total = tpp_lnp_raw + tpp_alp_raw,
#          pv_lnp = pv_lnp_raw+(undec*pv_lnp_raw/pv_total),
#          pv_alp = pv_alp_raw+(undec*pv_alp_raw/pv_total),
#          pv_grn = pv_grn_raw+(undec*pv_grn_raw/pv_total),
#          pv_onp = pv_onp_raw+(undec*pv_onp_raw/pv_total),
#          pv_uap = pv_uap_raw+(undec*pv_uap_raw/pv_total),
#          pv_oth = pv_oth_raw+(undec*pv_oth_raw/pv_total),
#          tpp_lnp = tpp_lnp_raw+(undec*tpp_lnp_raw/tpp_total),
#          tpp_alp = tpp_alp_raw+(undec*tpp_alp_raw/tpp_total)) %>%
#   select(Date,last_date,Firm,sample_size,pv_lnp,pv_alp,pv_grn,pv_onp,pv_uap,pv_oth,tpp_lnp,tpp_alp)

polling2528$sample_size[is.na(polling2528$sample_size)] <- round(mean(polling2528$sample_size, na.rm=TRUE))
#essential$sample_size[is.na(essential$sample_size)] <- round(mean(essential$sample_size, na.rm=TRUE))

#polling2528 <- polling2528 %>%
#  bind_rows(essential) %>%
#  arrange(desc(as.Date(last_date, '%d %b %Y')))

minss <- min(polling2528$sample_size)
max_date <- max(as.Date(polling2528$last_date, '%d %b %Y')) + days(7)
#max_date <- as.Date('2025-05-03')

primary_votes <- ggplot(polling2528, aes(x=as.Date(last_date, '%d %b %Y'))) +
  theme_bw() +
  geom_point(aes(y=pv_alp, size=sample_size), colour="red3", alpha = 3/10) +
  geom_smooth(aes(y=pv_alp, colour="ALP", weight=sqrt(sample_size)), span = spansize, se = FALSE) +
  geom_point(aes(y=pv_lnp, size=sample_size), colour="blue4", alpha = 3/10) +
  geom_smooth(aes(y=pv_lnp, colour="LNP", weight=sqrt(sample_size)), span = spansize, se = FALSE) +
  geom_point(aes(y=pv_grn, size=sample_size), colour="green4", alpha = 3/10) +
  geom_smooth(aes(y=pv_grn, colour="GRN", weight=sqrt(sample_size)), span = spansize, se = FALSE) +
  geom_point(aes(y=pv_onp, size=sample_size), colour="orange3", alpha = 5/10) +
  geom_smooth(aes(y=pv_onp, colour="ONP", weight=sqrt(sample_size)), span = spansize, se = FALSE) +
  #geom_point(aes(y=pv_uap, size=sample_size), colour="yellow3", alpha = 5/10) +
  #geom_smooth(aes(y=pv_uap, colour="UAP", weight=sqrt(sample_size)), span = spansize, se = FALSE) +
  geom_point(aes(y=pv_oth, size=sample_size), colour="gray60", alpha = 3/10) +
  geom_smooth(aes(y=pv_oth, colour="OTH", weight=sqrt(sample_size)), span = spansize, se = FALSE) +
  geom_point(data = election25, aes(x = as.Date('2025-05-03', '%Y-%m-%d'), y = vote, colour = party), shape=23, stroke=0.5, fill = "#FFFFFF", size=4) +
  geom_point(data = election25, aes(x = as.Date('2025-05-03', '%Y-%m-%d'), y = vote, colour = party), shape=18, size=3) +
  scale_y_continuous(limits=c(0, 50), breaks=c(0, 5, 10, 15, 20, 25, 30, 35, 40, 45, 50), minor_breaks = NULL, expand = c(0,0)) +
  scale_x_date(limits=c(as.Date('2025-05-03', '%Y-%m-%d'), max_date), date_breaks = "1 month", date_labels = "%b %Y", minor_breaks = "1 month", expand = c(0,0)) +
  guides(colour = guide_legend(override.aes = list(alpha = 1, shape = 16, size = 3))) +
  theme(legend.key = element_rect(colour = NA, fill = NA), legend.text=element_text(size=12), axis.text.y = element_text(size=12), axis.text.x = element_text(angle=90, vjust=0.5, size=12)) +
  labs(y="Voters (%)", x= NULL) +
  scale_colour_manual(name="", 
                     labels = c("Labor", "Greens", "Liberal-National Coalition", "One Nation", "Other"), 
                     values = c("ALP"="red3", "GRN"="green4", "LNP"="blue4", "ONP"="orange3", "OTH"="gray60"))

pv_plot <- primary_votes + theme(legend.position="bottom", legend.box = "horizontal", legend.text = element_text(size=12)) +
  guides(colour = guide_legend(order=1, override.aes = list(size = 0, shape = 15)), size = guide_legend(order=2)) +
  scale_size_area(name = "Sample size:")

ggsave(file="pv2528.svg", plot=pv_plot, width=10, height=6, units="in")

tpp <- ggplot(polling2528, aes(x=as.Date(last_date, '%d %b %Y'))) +
  theme_bw() +
  geom_point(aes(y=tpp_lnp, size=sample_size), colour="blue4", alpha = 3/10) +
  geom_smooth(aes(y=tpp_lnp, colour="LNP", weight=sqrt(sample_size)), span = spansize, se = FALSE) +
  geom_point(aes(y=tpp_alp, size=sample_size), colour="red3", alpha = 3/10) +
  geom_smooth(aes(y=tpp_alp, colour="ALP", weight=sqrt(sample_size)), span = spansize, se = FALSE) +
  geom_point(data = election25tpp, aes(x = as.Date('2025-05-03', '%Y-%m-%d'), y = vote, colour = party), shape=23, stroke=0.5, fill = "#FFFFFF", size=4) +
  geom_point(data = election25tpp, aes(x = as.Date('2025-05-03', '%Y-%m-%d'), y = vote, colour = party), shape=18, size=3) +
  scale_y_continuous(limits=c(35, 65), breaks=c(40,45,50,55,60), minor_breaks = NULL, expand = c(0,0)) +
  scale_x_date(limits=c(as.Date('2025-05-03', '%Y-%m-%d'), max_date), date_breaks = "1 month", date_labels = "%b %Y", minor_breaks = "1 month", expand = c(0,0)) +
  theme(legend.key = element_rect(colour = NA, fill = NA), legend.text=element_text(size=12), axis.text.y = element_text(size=12), axis.text.x = element_text(angle=90, vjust=0.5, size=12)) +
  labs(y="Voters (%)", x= NULL) +
  scale_colour_manual(name="", 
                     labels = c("Australian Labor Party", "Liberal-National Coalition"), 
                     values = c("ALP"="red3", "LNP"="blue4"))
tpp_plot <- tpp + theme(legend.position="bottom", legend.box = "horizontal", legend.text = element_text(size=12)) +
  guides(colour = guide_legend(order=1, override.aes = list(size = 0, shape = 15)), size = guide_legend(order=2)) +
  scale_size_area(name = "Sample size:")

ggsave(file="tpp2528.svg", plot=tpp_plot, width=10, height=6, units="in")

ppm <- ggplot(ppm2528, aes(x=as.Date(date, '%d-%b-%y'))) +
  theme_bw() +
  geom_point(aes(y=albanese), colour="red3", size=2.5, alpha = 3/10) +
  geom_smooth(aes(y=albanese, colour="Albanese"), span = spansize, se = FALSE) +
  geom_point(aes(y=ley), colour="blue4", size=2.5, alpha = 3/10) +
  geom_smooth(aes(y=ley, colour="ley"), span = spansize, se = FALSE) +
  geom_point(aes(y=unknown), colour="grey20", size=2.5, alpha = 3/10) +
  geom_smooth(aes(y=unknown, colour="Unknown"), span = spansize, se = FALSE) +
  scale_y_continuous(limits=c(0, 70), breaks = c(10, 20, 30, 40, 50, 60), minor_breaks = NULL, expand = c(0,0)) +
  scale_x_date(date_breaks = "1 month", date_labels = "%b %Y", minor_breaks = NULL, expand = c(0.01,0.01)) +
  theme(axis.text.x = element_text(angle=90, vjust=0.5, size=12), axis.text.y = element_text(size=12), axis.title.y = element_text(size=14)) +
  labs(y="Support (%)", x= NULL) +
  scale_colour_manual(name="",
                     labels = c("Albanese", "Ley", "Don't Know"),
                     values = c("Albanese"="red3", "Ley"="blue4", "Unknown" = "grey60"))
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

lnp_sat <- ggplot(leysat, aes(x=as.Date(date, '%d-%b-%y'))) +
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
  labs(y="% satisfaction", x= NULL, title = "Sussan Ley approval rating") +
  scale_colour_manual(name="",
                      breaks = c("Satisfied", "Dissatisfied", "Don't Know"),
                     labels = c("Satisfied", "Dissatisfied", "Don't Know"),
                     values = c("Satisfied"="#02e03d", "Dissatisfied"="#f74888", "Don't Know" = "#b3b3b3"))
lnp_sat + theme(legend.position="bottom", legend.box = "horizontal", legend.text = element_text(size=12))
