library(forestplot)


setwd('C:/Users/NicholasFitzhugh/OneDrive - Behandlingsraadet/Scripts/DTA forest plot')

#Test data
require(readxl)

d <- read_excel('Systolisk hjertesvigt.xlsx')

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



colnames(d) <- c('Reference', 'Tærskel', 'TP', 'FP', 'FN', 'TN', 'MetodeH', 'TypeH', 'MetodeU', 'sens', 'sensLL', 'sensUL', 'spec', 'specLL', 'specUL')





d %>% forestplot(labeltext = c(Reference, Tærskel, MetodeH, TypeH, MetodeU, TP, FP, FN, TN, sens, spec),
                 mean = sens,
                 lower = sensLL,
                 upper = sensUL) %>%
  fp_add_header(Reference = c('Studie'),
                Tærskel = c('Tærksel'),
                sens = c('Sensitivitet'),
                MetodeU = c('Metode for vurdering (HHUSD'), 
                MetodeH = c('Metode for vurdering (mid-/high-range ultralyd)'),
                TypeH = c('Type (HHUSD)'),
                TP = 'TP', FP = 'FP', FN = 'FN', TN = 'TN')
