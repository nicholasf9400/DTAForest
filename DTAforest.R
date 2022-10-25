#### ggplot koblet forest plot


setwd('C:/Users/NicholasFitzhugh/OneDrive - Behandlingsraadet/Scripts/DTA forest plot')

#Test data
require(readxl)

d <- read_excel('Systolisk hjertesvigt.xlsx')


#Function requirements
require(ggplot2)
require(epiR)
require(stringr)

#Compute sens and spec plus CI
sesp_temp <- matrix(NA, ncol=6, nrow = nrow(d))
for (i in 1:nrow(d)) {
  temp <- epi.tests(c(d$TP[i], d$FP[i], d$FN[i], d$TN[i]))
  temp <- temp[[1]][c(3,4),]
  sesp_temp[i,] <- as.numeric(cbind(temp[1,2:4], temp[2,2:4]))
}
colnames(sesp_temp) <- c('sens', 'sensLL', 'sensUL',
                         'spec', 'specLL', 'specUL')

d <- cbind(d, sesp_temp)


forest.sens <- ggplot(data=d, aes(x=Reference, y=sens, ymin = sensLL, ymax = sensUL)) + theme_bw() + 
  theme(axis.title.y=element_blank(), axis.text.y = element_blank(), axis.ticks.y=element_blank(),
        axis.line = element_blank(), panel.border = element_blank(), panel.grid.minor = element_blank(), panel.grid.major = element_blank())+ 
  geom_point(color='blue', shape = 'square', size=5) + geom_linerange() +
  coord_flip() + ylab('Sensitivtet (95% KI)') + ylim(0,1)


forest.spec <- ggplot(data=d, aes(x=Reference, y=spec, ymin = specLL, ymax = specUL)) + theme_bw() + 
  theme(axis.title.y=element_blank(), axis.text.y = element_blank(), axis.ticks.y=element_blank(),
        axis.line = element_blank(), panel.border = element_blank(), panel.grid.minor = element_blank(), panel.grid.major = element_blank())+ 
  geom_point(color='blue', shape = 'square', size=5) + geom_linerange() +
  coord_flip() + ylab('Specificitet (95% KI)') + ylim(0,1)


table_base <- ggplot(d, aes(y=Reference)) +
  ylab(NULL) + xlab("  ") + 
  theme(plot.title = element_text(hjust = 0.5, size=12), 
        axis.text.x = element_text(color="white", hjust = -3, size = 25), ## This is used to help with alignment
        axis.line = element_blank(),
        axis.text.y = element_blank(), 
        axis.ticks = element_blank(),
        axis.title.y = element_blank(), 
        legend.position = "none",
        panel.background = element_blank(), 
        panel.border = element_blank(), 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), 
        plot.background = element_blank())

tab1 <- table_base + 
  labs(title = "Reference") +
  geom_text(aes(y = rev(Reference), x = 1, label = Reference)) + ## decimal places
  ggtitle("Studie") + theme(plot.title=element_text(face = 'bold'))

tab2 <-  table_base +
  geom_text(aes(y = Tærskel, x = 1, label = Tærskel)) + ## decimal places
  ggtitle("Tærskel for positivt fund") + theme(plot.title=element_text(face = 'bold'))

tab2

tab3 <-  table_base +
  geom_text(aes(y = `Type HHUSD`, x = 1, label = `Type HHUSD`)) + ## decimal places
  ggtitle("Type HHUSD") + theme(plot.title=element_text(face = 'bold'))

tab3

#### Tab3 applies unique() somehow
