#New attempt of forest plot function

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


plot <- ggplot(d, aes(y = rev(ind))) + theme_classic()

plot


