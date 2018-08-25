library(ggplot2)
polling1619 <- read.csv("polling1619.csv")
primary_votes <- ggplot(polling1619, aes(x=as.Date(last_date, '%d %b %Y'))) +
  theme_bw() +
  geom_point(aes(y=(pv_lnp)), colour="blue4", size=2, alpha = 3/10) +
  geom_smooth(aes(y=(pv_lnp), colour="LNP"), span = 0.2, se = FALSE) +
  geom_point(aes(y=(pv_alp)), colour="red3", size=2, alpha = 3/10) +
  geom_smooth(aes(y=(pv_alp), colour="ALP"), span = 0.2, se = FALSE) +
  geom_point(aes(y=(pv_grn)), colour="green4", size=2, alpha = 3/10) +
  geom_smooth(aes(y=(pv_grn), colour="GRN"), span = 0.2, se = FALSE) +
  geom_point(aes(y=(pv_onp)), colour="yellow3", size=2, alpha = 5/10) +
  geom_smooth(aes(y=(pv_onp), colour="ONP"), span = 0.2, se = FALSE) +
  geom_point(aes(y=(pv_oth)), colour="gray60", size=2, alpha = 3/10) +
  geom_smooth(aes(y=(pv_oth), colour="OTH"), span = 0.2, se = FALSE) +
  scale_y_continuous(limits=c(0, 50), breaks=c(0, 5, 10, 15, 20, 25, 30, 35, 40, 45, 50), minor_breaks = NULL) +
  scale_x_date(date_breaks = "1 month", date_labels = "%b %Y", minor_breaks = NULL) +
  theme(axis.text.x = element_text(angle=45, vjust=0.5, size=12), axis.text.y = element_text(size=12), axis.title.y = element_text(size=14)) +
  labs(y="Voters (%)", x= NULL) +
  scale_color_manual(name="", 
                     labels = c("ALP", "Greens", "Liberal-National Coalition", "One Nation", "Other"), 
                     values = c("LNP"="blue4", "ALP"="red3", "GRN"="green4", "ONP"="yellow3", "OTH"="gray60"))
primary_votes + theme(legend.position="bottom", legend.box = "horizontal", legend.text = element_text(size=12))

primary_votes <- ggplot(polling1619, aes(x=as.Date(last_date, '%d %b %Y'))) +
  theme_bw() +
  geom_point(aes(y=(tpp_lnp)), colour="blue4", size=2, alpha = 3/10) +
  geom_smooth(aes(y=(tpp_lnp), colour="LNP"), span = 0.2, se = FALSE) +
  geom_point(aes(y=(tpp_alp)), colour="red3", size=2, alpha = 3/10) +
  geom_smooth(aes(y=(tpp_alp), colour="ALP"), span = 0.2, se = FALSE) +
  scale_y_continuous(limits=c(40, 60), breaks=c(42, 44, 46, 48, 50, 52, 54, 56, 58, 60), minor_breaks = NULL) +
  scale_x_date(date_breaks = "1 month", date_labels = "%b %Y", minor_breaks = NULL) +
  theme(axis.text.x = element_text(angle=45, vjust=0.5, size=12), axis.text.y = element_text(size=12), axis.title.y = element_text(size=14)) +
  labs(y="Voters (%)", x= NULL) +
  scale_color_manual(name="", 
                     labels = c("Australian Labor Party", "Liberal-National Coalition"), 
                     values = c("LNP"="blue4", "ALP"="red3"))
primary_votes + theme(legend.position="bottom", legend.box = "horizontal", legend.text = element_text(size=12))
