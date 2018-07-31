# Box plot
anne4<-anne3[!is.na(anne3$logASM00),]
anne5<-anne4[!is.na(anne4$BWGA_CAT),]
c <- ggplot(anne5, aes(factor(BWGA_CAT),logASM00))+
  
  geom_boxplot(size=0.6, width=0.55)+
  theme_bw()+
  
  theme(plot.title = element_text(size=15, face="bold"))+
  
  theme(axis.title.x = element_text(face="bold", size=16))+
  
  theme(axis.title.y = element_text(face="bold", size=16)) +  
  
  theme(legend.title = element_text(size=16, face="bold"))+
  
  ylab("Log-transformed maternal arsenic")+
  
  xlab("Birth weight-for-gestational age category")+
  
  scale_x_discrete(breaks=c("1", "2", "3"), labels=c("SGA", "AGA", "LGA"))+
  
  stat_summary(fun.y=mean, geom="point", shape=5, size=4)+
  
  theme(text = element_text(size=16, face="bold"))+ 
  annotate("text", x = 3.3, y = 0.2, size =6, label = "P=0.009")
c


d<-ggplot(anne5, aes(factor(BWGA_CAT),logASM00))+
  geom_boxplot(mapping = NULL, data = NULL, stat = "boxplot",
               position = "dodge2",  outlier.colour = NULL, outlier.color = NULL,
               outlier.fill = NULL, outlier.shape = 19, outlier.size = 1.5,
               outlier.stroke = 0.5, outlier.alpha = NULL, notch = FALSE,
               notchwidth = 0.5, varwidth = FALSE, na.rm = FALSE, show.legend = NA,
               inherit.aes = TRUE)