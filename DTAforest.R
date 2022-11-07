#### ggplot koblet forest plot


setwd('C:/Users/NicholasFitzhugh/OneDrive - Behandlingsraadet/Scripts/DTA forest plot')

#Test data
require(readxl)

d <- read_excel('Systolisk hjertesvigt.xlsx')


#Function requirements
require(ggplot2)
require(epiR)
require(stringr)
require(gridExtra)

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

d$ind <- 1:nrow(d)

d$sensitivitet <- paste0(round(d$sens, 3), " [", round(d$sensLL,3), ", ", round(d$sensUL, 3), "]")

d$specificitet <- paste0(round(d$spec, 3), " [", round(d$specLL,3), ", ", round(d$specUL, 3), "]")

forest.sens <- ggplot(data=d, aes(x=Reference, y=sens, ymin = sensLL, ymax = sensUL)) + theme_bw() + 
  theme(axis.title.y=element_blank(), axis.text.y = element_blank(), axis.ticks.y=element_blank(),
        axis.line = element_blank(), panel.border = element_blank(), panel.grid.minor = element_blank(), panel.grid.major = element_blank())+ 
  geom_point(color='blue', shape = 'square', size=3) + geom_linerange() +
  coord_flip() + ylab('Sensitivtet (95% KI)') + ylim(0,1)


forest.spec <- ggplot(data=d, aes(x=Reference, y=spec, ymin = specLL, ymax = specUL)) + theme_bw() + 
  theme(axis.title.y=element_blank(), axis.text.y = element_blank(), axis.ticks.y=element_blank(),
        axis.line = element_blank(), panel.border = element_blank(), panel.grid.minor = element_blank(), panel.grid.major = element_blank())+ 
  geom_point(color='blue', shape = 'square', size=3) + geom_linerange() +
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

ref <- table_base + 
  labs(title = "Reference") +
  geom_text(aes(y = ind, x = 1, label = Reference)) + ## decimal places
  ggtitle("Studie") + theme(plot.title=element_text(face = 'bold'))


SP <- table_base + 
  labs(title = 'SP') + 
  geom_text(aes(y = ind, x = 1, label = TP)) + 
  ggtitle('SP') + theme(plot.title = element_text(face = 'bold'))

FP <- table_base + 
  labs(title = 'FP') + 
  geom_text(aes(y = ind, x = 1, label = FP)) + 
  ggtitle('FP') + theme(plot.title = element_text(face = 'bold'))

FN <- table_base + 
  labs(title = 'FN') + 
  geom_text(aes(y = ind, x = 1, label = FN)) + 
  ggtitle('FN') + theme(plot.title = element_text(face = 'bold'))

SN <- table_base + 
  labs(title = 'SN') + 
  geom_text(aes(y = ind, x = 1, label = TN)) + 
  ggtitle('SN') + theme(plot.title = element_text(face = 'bold'))

sensitivitet <- table_base + 
  labs(title = 'Sensitivitet') + 
  geom_text(aes(y = ind, x=1, label = sensitivitet)) + 
  ggtitle('Sensitivitet (95% KI)') + theme(plot.title = element_text(face = 'bold'))

specificitet <- table_base + 
  labs(title = 'Specificitet') + 
  geom_text(aes(y = ind, x=1, label = specificitet)) + 
  ggtitle('Specificitet (95% KI)') + theme(plot.title = element_text(face = 'bold'))


tab2 <-  table_base +
  geom_text(data = d, aes(y = ind, x = 1, label = Tærskel), size = 3.5,
            position = 'identity') + ## decimal places
  ggtitle("Tærskel for positivt fund") + theme(plot.title=element_text(face = 'bold'))


tab3 <-  table_base +
  geom_text(aes(y = ind, x = 1, label = `Type HHUSD`)) + ## decimal places
  ggtitle("Type HHUSD") + theme(plot.title=element_text(face = 'bold'))


grid.arrange(ref, SP, FP, FN, SN, tab2, tab3, sensitivitet, specificitet, forest.sens, forest.spec, nrow=1, ncol = 11)


