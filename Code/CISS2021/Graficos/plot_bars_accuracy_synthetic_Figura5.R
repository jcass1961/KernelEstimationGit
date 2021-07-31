library(extrafont)
loadfonts(device = "win")
library(readxl)
library(ggplot2)

getwd()


accuracy_samples_synt <- as.data.frame(read_excel("../../../Data/CISS2021/accuracy_samples_synt.xlsx"))
accuracy_synt <- as.data.frame(read_excel("../../../Data/CISS2021/accuracy_synt.xlsx"))

# S?lo para L=2

samp <- split(accuracy_samples_synt, accuracy_samples_synt$L)
syn <- split(accuracy_synt, accuracy_synt$L)
samp2 <- as.data.frame(samp[2])
syn2 <- as.data.frame(syn[2])
colnames(samp2) <- colnames(accuracy_samples_synt)
colnames(syn2) <- colnames(accuracy_samples_synt)

#par(mgp=c(2.2,0.45,0), tcl=-0.4, mar=c(0,0,0,0))
ggplot(samp2, aes(x=Size, y=Value, fill=Estimator)) +
  geom_bar(stat='identity', width=15, na.rm=T, position="dodge") +
  facet_grid(L~ Measure, labeller = label_bquote(rows = "")) +
  ylab("") +
  xlab("Sample size") +
  coord_cartesian(ylim = c(0.5, 1)) +
  scale_x_continuous(breaks = c(1,21,41,61,81),
                     labels = c("9", "25", "49", "81", "121")) +
  scale_fill_discrete(name = "Estimator", 
                      labels = c(expression(italic(H)[AO[1]]), 
                                 expression(italic(H)[C]), 
                                 expression(italic(H)[ML]), 
                                 expression(italic(H)["NA"]), 
                                 expression(italic(H)[V]), 
                                 expression(italic(H)[VE]))) + 
  theme(text=element_text(size=12,  family="serif"),
        legend.position = "bottom",
        strip.background =element_rect(fill="white"),
        strip.placement = "outside",
        strip.text = element_text(size=10),
        legend.title = element_text(size = 10),
        legend.text = element_text(size = 10),
        panel.background = element_blank(),
        panel.border = element_rect(colour = "black", fill=NA)) +
  guides(fill = guide_legend(nrow = 1)) -> bars1
#dev.off()
ggsave(plot=bars1, 
       file="../../../Figures/CISS2021/measures_samples_synt2.pdf", 
       width=12, height=9, units="cm")


# pdf("C:/Users/Usuario-PC/Dropbox/Julia-Daiana-Andrea/Figures/measures_synt2.pdf", height = 5, width = 6.5, pointsize=10)
# par(mgp=c(2.2,0.45,0), tcl=-0.4, mar=c(0,0,0,0))
ggplot(syn2, aes(x=Size, y=Value, fill=Estimator)) +
  geom_bar(stat='identity', width=15, na.rm=T, position="dodge") +
  facet_grid(L ~ Measure,
             labeller = label_bquote(rows = "")) +
  ylab("") +
  xlab("Sample size") +
  coord_cartesian(ylim = c(0.5, 1)) +
  scale_x_continuous(breaks = c(1,21,41,61,81),
                     labels = c("9", "25", "49", "81", "121")) +
  scale_fill_discrete(name = "Estimator", 
                      labels = c(expression(italic(H)[AO[1]]), 
                                 expression(italic(H)[C]), 
                                 expression(italic(H)[ML]), 
                                 expression(italic(H)["NA"]), 
                                 expression(italic(H)[V]), 
                                 expression(italic(H)[VE]))) + 
  theme(text=element_text(size=12,  family="serif"),
        legend.position = "bottom",
        strip.background =element_rect(fill="white"),
        strip.placement = "outside",
        strip.text = element_text(size=10),
        legend.title = element_text(size = 10),
        legend.text = element_text(size = 10),
        panel.background = element_blank(),
        panel.border = element_rect(colour = "black", fill=NA)) +
  guides(fill = guide_legend(nrow = 1)) -> bars2
# dev.off()

ggsave(plot=bars2, 
       file="../../../Figures/CISS2021/measures_synt2.pdf", 
       width=12, height=9, units="cm")
