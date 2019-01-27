QUN <- 669
HANG <- 3898
BA <- 1391

kindergarden <- read_excel("~/desktop/exp/ba.xlsx", sheet = 2)
kindergarden <- kindergarden[, -c(1, 3, 5, 7, 9, 11,13,15,17,19,21,23,25,27,29,31,33)]

# dateframe for the second method
kindergarden2 <- kindergarden

# transform the data 
for (i in 1:nrow(kindergarden)) # every kid 
{
  for (j in 2:ncol(kindergarden)) # every item 
    kindergarden[i,j] <- kindergarden[i, j] / BA
}

# give points from 0 - 3

for (i in 1:nrow(kindergarden)) # every kid 
{
  for (j in 2:ncol(kindergarden)) # every item 
  {
    if (is.na(kindergarden[i,j]))
      kindergarden[i,j] <- NA
    else if (kindergarden[i,j] >= 0.1)
      kindergarden[i, j] <- 0
    else if (kindergarden[i,j] < 0.1 & kindergarden[i,j] >= 0.05)
      kindergarden[i, j] <- 1
    else if (kindergarden[i,j] < 0.05 & kindergarden[i,j] >= 0.02)
      kindergarden[i,j] <- 2
    else 
      kindergarden[i,j] <- 3
  }
}

kindergarden$M <- vector(length=nrow(kindergarden))
for (i in 1:nrow(kindergarden))
  kindergarden$M[i] <- mean(as.numeric(kindergarden[i, 2:17]), na.rm = T)

kindergarden <- kindergarden[, c(1,18)]
write.csv(kindergarden[,2], file = "MyData.csv", row.names = F)

for (i in 1:nrow(kindergarden2)) # every kid 
{
  for (j in 2:ncol(kindergarden2)) # every item 
  {
    if (is.na(kindergarden2[i,j]))
      kindergarden2[i,j] <- NA
    else if (kindergarden2[i,j] > 1)
      kindergarden2[i,j] <- 0
    else 
      kindergarden2[i,j] <- 1
  }
}
kindergarden2$M <- vector(length=nrow(kindergarden2))
for (i in 1:nrow(kindergarden2))
  kindergarden2$M[i] <- mean(as.numeric(kindergarden2[i, 2:17]), na.rm = T)

kindergarden2 <- kindergarden2[, c(1,18)]
write.csv(kindergarden2[,2], file = "MyData.csv", row.names = F)

