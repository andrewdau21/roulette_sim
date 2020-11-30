##Roulette Simulator
require(prob)

#time the code
ptm <- proc.time()

#create a data frame of possible American Roulette outcomes
x <- roulette(european = FALSE, makespace = TRUE)

#specify number of simulation runs
sim=10000

#initate storage of outputs, this is a much faster implementation than rbind
roulette_spins <- data.frame(num = character(sim), color = character(sim), probs = numeric(sim))
martingale_outcome <- data.frame(bet = numeric(sim), result = numeric(sim), running_total=numeric(sim))
martingale_outcome[1,] <- data.frame(5, 0)

#specify the maxium that can be bet at the roulette table.
table_max = 200

#for loop to run the simulation
for(i in 1:sim)
{
set.seed(i)
  y<-x[sample(nrow(x),1), ]
  roulette_spins[i,] <- data.frame(y$num, y$color, y$probs)
  if(y$color == 'Red') 
  {
    martingale_outcome[i,2] <- martingale_outcome[i,1]*2
    if(i == 1)
    {
      martingale_outcome[i,3] <- -martingale_outcome[i,1] + martingale_outcome[i,2] 
    }
    else
    {
      martingale_outcome[i,3] <- martingale_outcome[i-1,3] - martingale_outcome[i,1] + martingale_outcome[i,2] 
    }
    
      martingale_outcome[i+1,1] <- 5  
  }
  if(y$color == 'Black' || y$color == "Green") 
  {
    martingale_outcome[i,2] <- 0
    martingale_outcome[i+1,1] <- martingale_outcome[i,1]*2
    if (martingale_outcome[i+1,1] > table_max)
    {
      martingale_outcome[i+1,1] <- table_max 
    }
    if(i == 1)
    {
      martingale_outcome[i,3] <- -martingale_outcome[i,1] + martingale_outcome[i,2] 
    }
    else
    {
      martingale_outcome[i,3] <- martingale_outcome[i-1,3] - martingale_outcome[i,1] + martingale_outcome[i,2] 
    }
  }
  
}



roulette_spins
#factoring the spins, so we can plot in order
roulette_spins$num <- factor(roulette_spins$num, levels = c('00','0','1','2','3','4','5',
                                                               '6','7','8','9','10','11','12',
                                                               '13','14','15','16','17','18','19',
                                                               '20','21','22','23','24','25','26',
                                                               '27','28','29','30','31','32','33',
    
                                                                                                       '34','35','36'))
#barplot of the spins  
barplot(table(roulette_spins$num),cex.names = .50, horiz = TRUE,las=1)
#time it end
proc.time() - ptm

#calculate net wins
sum(martingale_outcome$bet)
sum(martingale_outcome$result- martingale_outcome$bet, na.rm=TRUE)

options(scipen=999)
#plot cumulative winnings to show timeseries
plot(1,type='n',xlim=c(1,10000),ylim=c(-10000,10000),xlab='Spin #', ylab='Cumulative Winnings')
lines(martingale_outcome$running_total)
abline(h=0, col="red")
