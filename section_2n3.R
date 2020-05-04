rm(list=ls())


#section 2

library(tidyverse)
library(dplyr)

file_17 <- read.delim2("PartD_Prescriber_PUF_NPI_17/PartD_Prescriber_PUF_NPI_17.txt", sep="\t")
file_16 <- read.delim2("PartD_Prescriber_PUF_NPI_16/PartD_Prescriber_PUF_NPI_16.txt", sep="\t")
str(file_17)
summary(file_17)
view(file_17)

#SD for brand name drugs
#brand <- file_17 %>% select(brand_claim_count)
brand = file_17$brand_claim_count
brand
brand <- na.omit(brand)
sd(brand)

b <- file_17$total_day_supply
a <- file_17$total_claim_count
ab <- b-a
ab
length(ab)
summary(ab)
ls(file_17)

#ratio
opoid <- aggregate(x = file_17$opioid_bene_count,                # Specify data column
          by = list(file_17$nppes_provider_state),              # Specify group indicator
          FUN = sum, na.rm=TRUE) 

anti <- aggregate(x = file_17$antibiotic_bene_count,
                  by = list(file_17$nppes_provider_state),
                  FUN = sum, na.rm=TRUE)

ratio <- opoid/anti
summary(ratio)

#pearson
res <- cor.test(file_17$bene_count_ge65, file_17$lis_claim_count, 
                method = "pearson")
res

#inflation


file_17$total_drug_cost <- as.numeric(as.character(file_17$total_drug_cost))
m_17 <- mean(file_17$total_drug_cost, na.rm=TRUE)
file_16$total_drug_cost <- as.numeric(as.character(file_16$total_drug_cost))
m_16 <- mean(file_16$total_drug_cost, na.rm = TRUE)

inflation <- ((m_17-m_16)/m_16)*100

#specialty


spec17 <- summary(file_17$specialty_description)
gr17= subset(spec17, spec17 >=1000)
spec16 <- summary(file_16$specialty_description)
gr16= subset(spec16, spec16 >= 1000)
dif = gr16/gr17
max(dif)

###########
#section 3
i=Inf
n=1:10

a <- sample(n,10 , replace = FALSE)
mean(a)
sd(a)
sum(a)
prob = (sum(a)-45)/sum(a)

n1=1:20
b <- sample(n, 20, replace = FALSE)
mean(b)
sd(b)
prob = (sum(b)-45)/sum(b)
