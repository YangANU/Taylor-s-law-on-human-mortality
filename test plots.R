par(mfrow = c(4,5))

for(ij in 1:47)
{
  plot(log(temporal_female_mean[,ij], base = 10), type = "l", col = 2)
  lines(log(temporal_male_mean[,ij], base = 10))
}


