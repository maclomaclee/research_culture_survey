or_table_label <- read_csv("or table label.csv", 
                           col_names = FALSE)
OR_Outcomes <- cbind(OR_Outcomes, or_table_label)
colnames(OR_Outcomes) <-(c("Question",  "OR", "l95", "u95","X1","X2", "X3"))
OR_Outcomes$Question_f = factor(OR_Outcomes$Question, levels=c("Q41.10.a", "Q41.9.a", "Q41.8.a", "Q41.7.a", "Q41.6.a", "Q41.5.a", "Q41.4.a", "Q41.3.a", "Q41.2.a", "Q41.1.a", "Q40.5.a", "Q40.4.a", "Q40.3.a", "Q40.2.a", "Q40.1.a", "Q38.1", "Q37.15.a", "Q37.14.a", "Q37.13.a", "Q37.12.a", "Q37.11.a", "Q37.10.a", "Q37.9.a", "Q37.8.a", "Q37.7.a", "Q37.6.a", "Q37.5.a", "Q37.4.a", "Q37.3.a", "Q37.2.a", "Q37.1.a", "Q35", "Q34", "Q33", "Q32", "Q31", "Q30", "Q29", "Q28", "Q27.11.a", "Q27.10.a", "Q27.9.a", "Q27.8.a", "Q27.7.a", "Q27.6.a", "Q27.5.a", "Q27.4.a", "Q27.3.a", "Q27.2.a", "Q27.1.a", "Q26.3.a", "Q26.2.a", "Q26.1.a", "Q25.5.a", "Q25.4.a", "Q25.3.a", "Q25.2.a", "Q25.1.a", "Q22", "Q21.11.a", "Q21.10.a", "Q21.9.a", "Q21.8.a", "Q21.7.a", "Q21.6.a", "Q21.5.a", "Q21.4.a", "Q21.3.a", "Q21.2.a", "Q21.1.a", "Q20.7.a", "Q20.6.a", "Q20.5.a", "Q20.4.a", "Q20.3.a", "Q20.2.a", "Q20.1.a", "Q18.13.a", "Q18.12.a", "Q18.11.a", "Q18.10.a", "Q18.9.a", "Q18.8.a", "Q18.7.a", "Q18.6.a", "Q18.5.a", "Q18.4.a", "Q18.3.a", "Q18.2.a", "Q18.1.a", "Q17.4.a", "Q17.3.a", "Q17.2.a", "Q17.1.a", "Q16.4.a", "Q16.3.a", "Q16.2.a", "Q16.1.a", "Q15.4.a", "Q15.3.a", "Q15.2.a", "Q15.1.a", "Q14.3.a", "Q14.2.a", "Q14.1.a", "Q13.5.a", "Q13.4.a", "Q13.3.a", "Q13.2.a", "Q13.1.a"))
WorkenvOR1 <- OR_Outcomes[c(1:20),]
WorkenvOR2 <- OR_Outcomes[c(21:33),]
CareerOR <- OR_Outcomes[c(34:52),]
CarDevOR <- OR_Outcomes[c(53:57),]
PercetOR <- OR_Outcomes[c(58:68),]
ExpOR <- OR_Outcomes[c(69:72),]
ExpOR2 <- OR_Outcomes[c(73:87),]
ExpOR3 <- OR_Outcomes[c(88:102),]

WorkenvOR1$Question_f = factor(WorkenvOR1$Question, levels=c("Q41.10.a", "Q41.9.a", "Q41.8.a", "Q41.7.a", "Q41.6.a", "Q41.5.a", "Q41.4.a", "Q41.3.a", "Q41.2.a", "Q41.1.a", "Q40.5.a", "Q40.4.a", "Q40.3.a", "Q40.2.a", "Q40.1.a", "Q38.1", "Q37.15.a", "Q37.14.a", "Q37.13.a", "Q37.12.a", "Q37.11.a", "Q37.10.a", "Q37.9.a", "Q37.8.a", "Q37.7.a", "Q37.6.a", "Q37.5.a", "Q37.4.a", "Q37.3.a", "Q37.2.a", "Q37.1.a", "Q35", "Q34", "Q33", "Q32", "Q31", "Q30", "Q29", "Q28", "Q27.11.a", "Q27.10.a", "Q27.9.a", "Q27.8.a", "Q27.7.a", "Q27.6.a", "Q27.5.a", "Q27.4.a", "Q27.3.a", "Q27.2.a", "Q27.1.a", "Q26.3.a", "Q26.2.a", "Q26.1.a", "Q25.5.a", "Q25.4.a", "Q25.3.a", "Q25.2.a", "Q25.1.a", "Q22", "Q21.11.a", "Q21.10.a", "Q21.9.a", "Q21.8.a", "Q21.7.a", "Q21.6.a", "Q21.5.a", "Q21.4.a", "Q21.3.a", "Q21.2.a", "Q21.1.a", "Q20.7.a", "Q20.6.a", "Q20.5.a", "Q20.4.a", "Q20.3.a", "Q20.2.a", "Q20.1.a", "Q18.13.a", "Q18.12.a", "Q18.11.a", "Q18.10.a", "Q18.9.a", "Q18.8.a", "Q18.7.a", "Q18.6.a", "Q18.5.a", "Q18.4.a", "Q18.3.a", "Q18.2.a", "Q18.1.a", "Q17.4.a", "Q17.3.a", "Q17.2.a", "Q17.1.a", "Q16.4.a", "Q16.3.a", "Q16.2.a", "Q16.1.a", "Q15.4.a", "Q15.3.a", "Q15.2.a", "Q15.1.a", "Q14.3.a", "Q14.2.a", "Q14.1.a", "Q13.5.a", "Q13.4.a", "Q13.3.a", "Q13.2.a", "Q13.1.a"))
WorkenvOR1 %>%
  ggplot(aes(x = Question_f, y=OR, ymin=l95, ymax=u95))+
  scale_y_log10(name="Odds Ratio")+
  expand_limits(y=c(0.3,3))+
  geom_pointrange()+
  geom_hline(yintercept =1, linetype=2)+
  geom_errorbar(aes(ymin=l95, ymax=u95),width=0.05,cex=0)+
  theme_minimal()+
  facet_wrap(~X3, strip.position="left", nrow=6, scales = "free_y")+
  geom_rect(aes(fill = X3),xmin = -Inf,xmax = Inf,
            ymin = -Inf,ymax = Inf,alpha = 0.1) +
  coord_flip()+
  geom_point(shape=1)+
  labs(title = "Working environment Q13 to Q17")+
  theme(plot.title=element_text(size=16,face="bold"),
        legend.position = "none",
        axis.text.y=element_text(),
        axis.ticks.y=element_blank(),
        axis.text.x=element_text(face="bold", ),
        axis.title.x =element_text(size=10,face="bold"),
        axis.title.y = element_blank(),
        strip.text.y = element_text(hjust=0,vjust = 0.5,angle=180,face="bold", size=10),
        strip.placement = "outside")
plotname <- "Working1"
plotname <- paste(plotname, ".jpg", sep = "")
ggsave(plotname)

WorkenvOR2$Question_f = factor(WorkenvOR2$Question, levels=c("Q41.10.a", "Q41.9.a", "Q41.8.a", "Q41.7.a", "Q41.6.a", "Q41.5.a", "Q41.4.a", "Q41.3.a", "Q41.2.a", "Q41.1.a", "Q40.5.a", "Q40.4.a", "Q40.3.a", "Q40.2.a", "Q40.1.a", "Q38.1", "Q37.15.a", "Q37.14.a", "Q37.13.a", "Q37.12.a", "Q37.11.a", "Q37.10.a", "Q37.9.a", "Q37.8.a", "Q37.7.a", "Q37.6.a", "Q37.5.a", "Q37.4.a", "Q37.3.a", "Q37.2.a", "Q37.1.a", "Q35", "Q34", "Q33", "Q32", "Q31", "Q30", "Q29", "Q28", "Q27.11.a", "Q27.10.a", "Q27.9.a", "Q27.8.a", "Q27.7.a", "Q27.6.a", "Q27.5.a", "Q27.4.a", "Q27.3.a", "Q27.2.a", "Q27.1.a", "Q26.3.a", "Q26.2.a", "Q26.1.a", "Q25.5.a", "Q25.4.a", "Q25.3.a", "Q25.2.a", "Q25.1.a", "Q22", "Q21.11.a", "Q21.10.a", "Q21.9.a", "Q21.8.a", "Q21.7.a", "Q21.6.a", "Q21.5.a", "Q21.4.a", "Q21.3.a", "Q21.2.a", "Q21.1.a", "Q20.7.a", "Q20.6.a", "Q20.5.a", "Q20.4.a", "Q20.3.a", "Q20.2.a", "Q20.1.a", "Q18.13.a", "Q18.12.a", "Q18.11.a", "Q18.10.a", "Q18.9.a", "Q18.8.a", "Q18.7.a", "Q18.6.a", "Q18.5.a", "Q18.4.a", "Q18.3.a", "Q18.2.a", "Q18.1.a", "Q17.4.a", "Q17.3.a", "Q17.2.a", "Q17.1.a", "Q16.4.a", "Q16.3.a", "Q16.2.a", "Q16.1.a", "Q15.4.a", "Q15.3.a", "Q15.2.a", "Q15.1.a", "Q14.3.a", "Q14.2.a", "Q14.1.a", "Q13.5.a", "Q13.4.a", "Q13.3.a", "Q13.2.a", "Q13.1.a"))
WorkenvOR2 %>%
  ggplot(aes(x = Question_f, y=OR, ymin=l95, ymax=u95))+
  scale_y_log10(name="Odds Ratio")+
  expand_limits(y=c(0.3,3))+
  geom_pointrange()+
  geom_hline(yintercept =1, linetype=2)+
  geom_errorbar(aes(ymin=l95, ymax=u95),width=0.05,cex=0)+
  theme_minimal()+
  facet_wrap(~X3, strip.position="left", nrow=6, scales = "free_y")+
  geom_rect(aes(fill = X3),xmin = -Inf,xmax = Inf,
            ymin = -Inf,ymax = Inf,alpha = 0.1) +
  coord_flip()+
  geom_point(shape=1)+
  labs(title = "Working environment Q18")+
  theme(plot.title=element_text(size=16,face="bold"),
        legend.position = "none",
        axis.text.y=element_text(),
        axis.ticks.y=element_blank(),
        axis.text.x=element_text(face="bold", ),
        axis.title.x =element_text(size=10,face="bold"),
        axis.title.y = element_blank(),
        strip.text.y = element_text(hjust=0,vjust = 0.5,angle=180,face="bold", size=10),
        strip.placement = "outside")
plotname <- "Working2"
plotname <- paste(plotname, ".jpg", sep = "")
ggsave(plotname)

CareerOR$Question_f = factor(CareerOR$Question, levels=c("Q41.10.a", "Q41.9.a", "Q41.8.a", "Q41.7.a", "Q41.6.a", "Q41.5.a", "Q41.4.a", "Q41.3.a", "Q41.2.a", "Q41.1.a", "Q40.5.a", "Q40.4.a", "Q40.3.a", "Q40.2.a", "Q40.1.a", "Q38.1", "Q37.15.a", "Q37.14.a", "Q37.13.a", "Q37.12.a", "Q37.11.a", "Q37.10.a", "Q37.9.a", "Q37.8.a", "Q37.7.a", "Q37.6.a", "Q37.5.a", "Q37.4.a", "Q37.3.a", "Q37.2.a", "Q37.1.a", "Q35", "Q34", "Q33", "Q32", "Q31", "Q30", "Q29", "Q28", "Q27.11.a", "Q27.10.a", "Q27.9.a", "Q27.8.a", "Q27.7.a", "Q27.6.a", "Q27.5.a", "Q27.4.a", "Q27.3.a", "Q27.2.a", "Q27.1.a", "Q26.3.a", "Q26.2.a", "Q26.1.a", "Q25.5.a", "Q25.4.a", "Q25.3.a", "Q25.2.a", "Q25.1.a", "Q22", "Q21.11.a", "Q21.10.a", "Q21.9.a", "Q21.8.a", "Q21.7.a", "Q21.6.a", "Q21.5.a", "Q21.4.a", "Q21.3.a", "Q21.2.a", "Q21.1.a", "Q20.7.a", "Q20.6.a", "Q20.5.a", "Q20.4.a", "Q20.3.a", "Q20.2.a", "Q20.1.a", "Q18.13.a", "Q18.12.a", "Q18.11.a", "Q18.10.a", "Q18.9.a", "Q18.8.a", "Q18.7.a", "Q18.6.a", "Q18.5.a", "Q18.4.a", "Q18.3.a", "Q18.2.a", "Q18.1.a", "Q17.4.a", "Q17.3.a", "Q17.2.a", "Q17.1.a", "Q16.4.a", "Q16.3.a", "Q16.2.a", "Q16.1.a", "Q15.4.a", "Q15.3.a", "Q15.2.a", "Q15.1.a", "Q14.3.a", "Q14.2.a", "Q14.1.a", "Q13.5.a", "Q13.4.a", "Q13.3.a", "Q13.2.a", "Q13.1.a"))
CareerOR %>%
  ggplot(aes(x = Question_f, y=OR, ymin=l95, ymax=u95))+
  scale_y_log10(name="Odds Ratio")+
  expand_limits(y=c(0.3,3))+
  geom_pointrange()+
  geom_hline(yintercept =1, linetype=2)+
  geom_errorbar(aes(ymin=l95, ymax=u95),width=0.05,cex=0)+
  theme_minimal()+
  facet_wrap(~X3, strip.position="left", nrow=6, scales = "free_y")+
  geom_rect(aes(fill = X3),xmin = -Inf,xmax = Inf,
            ymin = -Inf,ymax = Inf,alpha = 0.1) +
  coord_flip()+
  geom_point(shape=1)+
  labs(title = "Careers")+
  theme(plot.title=element_text(size=16,face="bold"),
        legend.position = "none",
        axis.text.y=element_text(),
        axis.ticks.y=element_blank(),
        axis.text.x=element_text(face="bold", ),
        axis.title.x =element_text(size=10,face="bold"),
        axis.title.y = element_blank(),
        strip.text.y = element_text(hjust=0,vjust = 0.5,angle=180,face="bold", size=10),
        strip.placement = "outside")
plotname <- "career"
plotname <- paste(plotname, ".jpg", sep = "")
ggsave(plotname)

CarDevOR$Question_f = factor(CarDevOR$Question, levels=c("Q41.10.a", "Q41.9.a", "Q41.8.a", "Q41.7.a", "Q41.6.a", "Q41.5.a", "Q41.4.a", "Q41.3.a", "Q41.2.a", "Q41.1.a", "Q40.5.a", "Q40.4.a", "Q40.3.a", "Q40.2.a", "Q40.1.a", "Q38.1", "Q37.15.a", "Q37.14.a", "Q37.13.a", "Q37.12.a", "Q37.11.a", "Q37.10.a", "Q37.9.a", "Q37.8.a", "Q37.7.a", "Q37.6.a", "Q37.5.a", "Q37.4.a", "Q37.3.a", "Q37.2.a", "Q37.1.a", "Q35", "Q34", "Q33", "Q32", "Q31", "Q30", "Q29", "Q28", "Q27.11.a", "Q27.10.a", "Q27.9.a", "Q27.8.a", "Q27.7.a", "Q27.6.a", "Q27.5.a", "Q27.4.a", "Q27.3.a", "Q27.2.a", "Q27.1.a", "Q26.3.a", "Q26.2.a", "Q26.1.a", "Q25.5.a", "Q25.4.a", "Q25.3.a", "Q25.2.a", "Q25.1.a", "Q22", "Q21.11.a", "Q21.10.a", "Q21.9.a", "Q21.8.a", "Q21.7.a", "Q21.6.a", "Q21.5.a", "Q21.4.a", "Q21.3.a", "Q21.2.a", "Q21.1.a", "Q20.7.a", "Q20.6.a", "Q20.5.a", "Q20.4.a", "Q20.3.a", "Q20.2.a", "Q20.1.a", "Q18.13.a", "Q18.12.a", "Q18.11.a", "Q18.10.a", "Q18.9.a", "Q18.8.a", "Q18.7.a", "Q18.6.a", "Q18.5.a", "Q18.4.a", "Q18.3.a", "Q18.2.a", "Q18.1.a", "Q17.4.a", "Q17.3.a", "Q17.2.a", "Q17.1.a", "Q16.4.a", "Q16.3.a", "Q16.2.a", "Q16.1.a", "Q15.4.a", "Q15.3.a", "Q15.2.a", "Q15.1.a", "Q14.3.a", "Q14.2.a", "Q14.1.a", "Q13.5.a", "Q13.4.a", "Q13.3.a", "Q13.2.a", "Q13.1.a"))
CarDevOR %>%
  ggplot(aes(x = Question_f, y=OR, ymin=l95, ymax=u95))+
  scale_y_log10(name="Odds Ratio")+
  expand_limits(y=c(0.3,3))+
  geom_pointrange()+
  geom_hline(yintercept =1, linetype=2)+
  geom_errorbar(aes(ymin=l95, ymax=u95),width=0.05,cex=0)+
  theme_minimal()+
  facet_wrap(~X3, strip.position="left", nrow=6, scales = "free_y")+
  geom_rect(aes(fill = X3),xmin = -Inf,xmax = Inf,
            ymin = -Inf,ymax = Inf,alpha = 0.1) +
  coord_flip()+
  geom_point(shape=1)+
  labs(title = "Career development")+
  theme(plot.title=element_text(size=16,face="bold"),
        legend.position = "none",
        axis.text.y=element_text(),
        axis.ticks.y=element_blank(),
        axis.text.x=element_text(face="bold", ),
        axis.title.x =element_text(size=10,face="bold"),
        axis.title.y = element_blank(),
        strip.text.y = element_text(hjust=0,vjust = 0.5,angle=180,face="bold", size=10),
        strip.placement = "outside")
plotname <- "cardev"
plotname <- paste(plotname, ".jpg", sep = "")
ggsave(plotname)

PercetOR$Question_f = factor(PercetOR$Question, levels=c("Q41.10.a", "Q41.9.a", "Q41.8.a", "Q41.7.a", "Q41.6.a", "Q41.5.a", "Q41.4.a", "Q41.3.a", "Q41.2.a", "Q41.1.a", "Q40.5.a", "Q40.4.a", "Q40.3.a", "Q40.2.a", "Q40.1.a", "Q38.1", "Q37.15.a", "Q37.14.a", "Q37.13.a", "Q37.12.a", "Q37.11.a", "Q37.10.a", "Q37.9.a", "Q37.8.a", "Q37.7.a", "Q37.6.a", "Q37.5.a", "Q37.4.a", "Q37.3.a", "Q37.2.a", "Q37.1.a", "Q35", "Q34", "Q33", "Q32", "Q31", "Q30", "Q29", "Q28", "Q27.11.a", "Q27.10.a", "Q27.9.a", "Q27.8.a", "Q27.7.a", "Q27.6.a", "Q27.5.a", "Q27.4.a", "Q27.3.a", "Q27.2.a", "Q27.1.a", "Q26.3.a", "Q26.2.a", "Q26.1.a", "Q25.5.a", "Q25.4.a", "Q25.3.a", "Q25.2.a", "Q25.1.a", "Q22", "Q21.11.a", "Q21.10.a", "Q21.9.a", "Q21.8.a", "Q21.7.a", "Q21.6.a", "Q21.5.a", "Q21.4.a", "Q21.3.a", "Q21.2.a", "Q21.1.a", "Q20.7.a", "Q20.6.a", "Q20.5.a", "Q20.4.a", "Q20.3.a", "Q20.2.a", "Q20.1.a", "Q18.13.a", "Q18.12.a", "Q18.11.a", "Q18.10.a", "Q18.9.a", "Q18.8.a", "Q18.7.a", "Q18.6.a", "Q18.5.a", "Q18.4.a", "Q18.3.a", "Q18.2.a", "Q18.1.a", "Q17.4.a", "Q17.3.a", "Q17.2.a", "Q17.1.a", "Q16.4.a", "Q16.3.a", "Q16.2.a", "Q16.1.a", "Q15.4.a", "Q15.3.a", "Q15.2.a", "Q15.1.a", "Q14.3.a", "Q14.2.a", "Q14.1.a", "Q13.5.a", "Q13.4.a", "Q13.3.a", "Q13.2.a", "Q13.1.a"))
PercetOR %>%
  ggplot(aes(x = Question_f, y=OR, ymin=l95, ymax=u95))+
  scale_y_log10(name="Odds Ratio")+
  expand_limits(y=c(0.3,3))+
  geom_pointrange()+
  geom_hline(yintercept =1, linetype=2)+
  geom_errorbar(aes(ymin=l95, ymax=u95),width=0.05,cex=0)+
  theme_minimal()+
  facet_wrap(~X3, strip.position="left", nrow=6, scales = "free_y")+
  geom_rect(aes(fill = X3),xmin = -Inf,xmax = Inf,
            ymin = -Inf,ymax = Inf,alpha = 0.1) +
  coord_flip()+
  geom_point(shape=1)+
  labs(title = "Perceptions")+
  theme(plot.title=element_text(size=16,face="bold"),
        legend.position = "none",
        axis.text.y=element_text(),
        axis.ticks.y=element_blank(),
        axis.text.x=element_text(face="bold", ),
        axis.title.x =element_text(size=10,face="bold"),
        axis.title.y = element_blank(),
        strip.text.y = element_text(hjust=0,vjust = 0.5,angle=180,face="bold", size=10),
        strip.placement = "outside")
plotname <- "Percet"
plotname <- paste(plotname, ".jpg", sep = "")
ggsave(plotname)

ExpOR$Question_f = factor(ExpOR$Question, levels=c("Q41.10.a", "Q41.9.a", "Q41.8.a", "Q41.7.a", "Q41.6.a", "Q41.5.a", "Q41.4.a", "Q41.3.a", "Q41.2.a", "Q41.1.a", "Q40.5.a", "Q40.4.a", "Q40.3.a", "Q40.2.a", "Q40.1.a", "Q38.1", "Q37.15.a", "Q37.14.a", "Q37.13.a", "Q37.12.a", "Q37.11.a", "Q37.10.a", "Q37.9.a", "Q37.8.a", "Q37.7.a", "Q37.6.a", "Q37.5.a", "Q37.4.a", "Q37.3.a", "Q37.2.a", "Q37.1.a", "Q35", "Q34", "Q33", "Q32", "Q31", "Q30", "Q29", "Q28", "Q27.11.a", "Q27.10.a", "Q27.9.a", "Q27.8.a", "Q27.7.a", "Q27.6.a", "Q27.5.a", "Q27.4.a", "Q27.3.a", "Q27.2.a", "Q27.1.a", "Q26.3.a", "Q26.2.a", "Q26.1.a", "Q25.5.a", "Q25.4.a", "Q25.3.a", "Q25.2.a", "Q25.1.a", "Q22", "Q21.11.a", "Q21.10.a", "Q21.9.a", "Q21.8.a", "Q21.7.a", "Q21.6.a", "Q21.5.a", "Q21.4.a", "Q21.3.a", "Q21.2.a", "Q21.1.a", "Q20.7.a", "Q20.6.a", "Q20.5.a", "Q20.4.a", "Q20.3.a", "Q20.2.a", "Q20.1.a", "Q18.13.a", "Q18.12.a", "Q18.11.a", "Q18.10.a", "Q18.9.a", "Q18.8.a", "Q18.7.a", "Q18.6.a", "Q18.5.a", "Q18.4.a", "Q18.3.a", "Q18.2.a", "Q18.1.a", "Q17.4.a", "Q17.3.a", "Q17.2.a", "Q17.1.a", "Q16.4.a", "Q16.3.a", "Q16.2.a", "Q16.1.a", "Q15.4.a", "Q15.3.a", "Q15.2.a", "Q15.1.a", "Q14.3.a", "Q14.2.a", "Q14.1.a", "Q13.5.a", "Q13.4.a", "Q13.3.a", "Q13.2.a", "Q13.1.a"))
ExpOR %>%
  ggplot(aes(x = Question_f, y=OR, ymin=l95, ymax=u95))+
  scale_y_log10(name="Odds Ratio")+
  expand_limits(y=c(0.3,3))+
  geom_pointrange()+
  geom_hline(yintercept =1, linetype=2)+
  geom_errorbar(aes(ymin=l95, ymax=u95),width=0.05,cex=0)+
  theme_minimal()+
  facet_wrap(~X3, strip.position="left", nrow=6, scales = "free_y")+
  geom_rect(aes(fill = X3),xmin = -Inf,xmax = Inf,
            ymin = -Inf,ymax = Inf,alpha = 0.1) +
  coord_flip()+
  geom_point(shape=1)+
  labs(title = "Experiences: Bullying, Harrassment and discrimination")+
  theme(plot.title=element_text(size=16,face="bold"),
        legend.position = "none",
        axis.text.y=element_text(),
        axis.ticks.y=element_blank(),
        axis.text.x=element_text(face="bold", ),
        axis.title.x =element_text(size=10,face="bold"),
        axis.title.y = element_blank(),
        strip.text.y = element_text(hjust=0,vjust = 0.5,angle=180,face="bold", size=10),
        strip.placement = "outside")
plotname <- "Exp"
plotname <- paste(plotname, ".jpg", sep = "")
ggsave(plotname)

ExpOR2$Question_f = factor(ExpOR2$Question, levels=c("Q41.10.a", "Q41.9.a", "Q41.8.a", "Q41.7.a", "Q41.6.a", "Q41.5.a", "Q41.4.a", "Q41.3.a", "Q41.2.a", "Q41.1.a", "Q40.5.a", "Q40.4.a", "Q40.3.a", "Q40.2.a", "Q40.1.a", "Q38.1", "Q37.15.a", "Q37.14.a", "Q37.13.a", "Q37.12.a", "Q37.11.a", "Q37.10.a", "Q37.9.a", "Q37.8.a", "Q37.7.a", "Q37.6.a", "Q37.5.a", "Q37.4.a", "Q37.3.a", "Q37.2.a", "Q37.1.a", "Q35", "Q34", "Q33", "Q32", "Q31", "Q30", "Q29", "Q28", "Q27.11.a", "Q27.10.a", "Q27.9.a", "Q27.8.a", "Q27.7.a", "Q27.6.a", "Q27.5.a", "Q27.4.a", "Q27.3.a", "Q27.2.a", "Q27.1.a", "Q26.3.a", "Q26.2.a", "Q26.1.a", "Q25.5.a", "Q25.4.a", "Q25.3.a", "Q25.2.a", "Q25.1.a", "Q22", "Q21.11.a", "Q21.10.a", "Q21.9.a", "Q21.8.a", "Q21.7.a", "Q21.6.a", "Q21.5.a", "Q21.4.a", "Q21.3.a", "Q21.2.a", "Q21.1.a", "Q20.7.a", "Q20.6.a", "Q20.5.a", "Q20.4.a", "Q20.3.a", "Q20.2.a", "Q20.1.a", "Q18.13.a", "Q18.12.a", "Q18.11.a", "Q18.10.a", "Q18.9.a", "Q18.8.a", "Q18.7.a", "Q18.6.a", "Q18.5.a", "Q18.4.a", "Q18.3.a", "Q18.2.a", "Q18.1.a", "Q17.4.a", "Q17.3.a", "Q17.2.a", "Q17.1.a", "Q16.4.a", "Q16.3.a", "Q16.2.a", "Q16.1.a", "Q15.4.a", "Q15.3.a", "Q15.2.a", "Q15.1.a", "Q14.3.a", "Q14.2.a", "Q14.1.a", "Q13.5.a", "Q13.4.a", "Q13.3.a", "Q13.2.a", "Q13.1.a"))
ExpOR2 %>%
  ggplot(aes(x = Question_f, y=OR, ymin=l95, ymax=u95))+
  scale_y_log10(name="Odds Ratio")+
  expand_limits(y=c(0.3,3))+
  geom_pointrange()+
  geom_hline(yintercept =1, linetype=2)+
  geom_errorbar(aes(ymin=l95, ymax=u95),width=0.05,cex=0)+
  theme_minimal()+
  facet_wrap(~X3, strip.position="left", nrow=6, scales = "free_y")+
  geom_rect(aes(fill = X3),xmin = -Inf,xmax = Inf,
            ymin = -Inf,ymax = Inf,alpha = 0.1) +
  coord_flip()+
  geom_point(shape=1)+
  labs(title = "Experiences Q37")+
  theme(plot.title=element_text(size=16,face="bold"),
        legend.position = "none",
        axis.text.y=element_text(),
        axis.ticks.y=element_blank(),
        axis.text.x=element_text(face="bold", ),
        axis.title.x =element_text(size=10,face="bold"),
        axis.title.y = element_blank(),
        strip.text.y = element_text(hjust=0,vjust = 0.5,angle=180,face="bold", size=10),
        strip.placement = "outside")
plotname <- "Exp2"
plotname <- paste(plotname, ".jpg", sep = "")
ggsave(plotname)

ExpOR3$Question_f = factor(ExpOR3$Question, levels=c("Q41.10.a", "Q41.9.a", "Q41.8.a", "Q41.7.a", "Q41.6.a", "Q41.5.a", "Q41.4.a", "Q41.3.a", "Q41.2.a", "Q41.1.a", "Q40.5.a", "Q40.4.a", "Q40.3.a", "Q40.2.a", "Q40.1.a", "Q38.1", "Q37.15.a", "Q37.14.a", "Q37.13.a", "Q37.12.a", "Q37.11.a", "Q37.10.a", "Q37.9.a", "Q37.8.a", "Q37.7.a", "Q37.6.a", "Q37.5.a", "Q37.4.a", "Q37.3.a", "Q37.2.a", "Q37.1.a", "Q35", "Q34", "Q33", "Q32", "Q31", "Q30", "Q29", "Q28", "Q27.11.a", "Q27.10.a", "Q27.9.a", "Q27.8.a", "Q27.7.a", "Q27.6.a", "Q27.5.a", "Q27.4.a", "Q27.3.a", "Q27.2.a", "Q27.1.a", "Q26.3.a", "Q26.2.a", "Q26.1.a", "Q25.5.a", "Q25.4.a", "Q25.3.a", "Q25.2.a", "Q25.1.a", "Q22", "Q21.11.a", "Q21.10.a", "Q21.9.a", "Q21.8.a", "Q21.7.a", "Q21.6.a", "Q21.5.a", "Q21.4.a", "Q21.3.a", "Q21.2.a", "Q21.1.a", "Q20.7.a", "Q20.6.a", "Q20.5.a", "Q20.4.a", "Q20.3.a", "Q20.2.a", "Q20.1.a", "Q18.13.a", "Q18.12.a", "Q18.11.a", "Q18.10.a", "Q18.9.a", "Q18.8.a", "Q18.7.a", "Q18.6.a", "Q18.5.a", "Q18.4.a", "Q18.3.a", "Q18.2.a", "Q18.1.a", "Q17.4.a", "Q17.3.a", "Q17.2.a", "Q17.1.a", "Q16.4.a", "Q16.3.a", "Q16.2.a", "Q16.1.a", "Q15.4.a", "Q15.3.a", "Q15.2.a", "Q15.1.a", "Q14.3.a", "Q14.2.a", "Q14.1.a", "Q13.5.a", "Q13.4.a", "Q13.3.a", "Q13.2.a", "Q13.1.a"))
ExpOR3 %>%
  ggplot(aes(x = Question_f, y=OR, ymin=l95, ymax=u95))+
  scale_y_log10(name="Odds Ratio")+
  expand_limits(y=c(0.3,3))+
  geom_pointrange()+
  geom_hline(yintercept =1, linetype=2)+
  geom_errorbar(aes(ymin=l95, ymax=u95),width=0.05,cex=0)+
  theme_minimal()+
  facet_wrap(~X3, strip.position="left", nrow=6, scales = "free_y")+
  geom_rect(aes(fill = X3),xmin = -Inf,xmax = Inf,
            ymin = -Inf,ymax = Inf,alpha = 0.1) +
  coord_flip()+
  geom_point(shape=1)+
  labs(title = "Experiences Q40, 41")+
  theme(plot.title=element_text(size=16,face="bold"),
        legend.position = "none",
        axis.text.y=element_text(),
        axis.ticks.y=element_blank(),
        axis.text.x=element_text(face="bold", ),
        axis.title.x =element_text(size=10,face="bold"),
        axis.title.y = element_blank(),
        strip.text.y = element_text(hjust=0,vjust = 0.5,angle=180,face="bold", size=10),
        strip.placement = "outside")
plotname <- "Exp3"
plotname <- paste(plotname, ".jpg", sep = "")
ggsave(plotname)