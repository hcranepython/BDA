# Problem 2.13 from BDA (Airline fatalities)
setwd("~/Dropbox/Rutgers/Teaching/STAT 668 - Bayesian Analysis/Homework/solutions/hw1/")
sapply( "../../../GitHub/BDA/Rcode/pause.R", source)
sapply( "../../../GitHub/BDA/Rcode/distributions.R", source)

dat = read.table("airline.txt", header = TRUE)

dat

# plot the data

plot( dat , pch = "*" )

dev.off()

# (a) Assume number of fatal accidents each year are independent Poisson(theta). Specify a prior for theta and determine posterior based on data. Give a 95% predictive interval for number of accidents in 1986.

# The data does not report the number of total flights in each year, so we assume this number is roughly the same for all of the years in the data set.  This allows us to assume that each year has the same level of exposure to fatal accident, and thus we model the number of fatalities in the 10 years as c.i.d. Poisson(theta) for theta ~ prior. A conjugate choice of prior would be the Gamma distribution with parameter a and b.  Based on the data it seems that the number of accidents is pretty consistent over the years, so we expect that the theta is somewhere within a reasonable range of the sample mean.  We choose a prior that puts most of the mass within +- 10 fatalities of this sample mean, by choosing a/b = 23.8 and b = 0.2.  Perhaps a safer choice, since we have no prior information about this problem, is to use the Jeffreys's non-informative prior, which is proportional to sqrt(1/lambda).

# First analysis with informative prior based on empirical Bayes estimate (prior estimate from data)
 
mean_accidents = mean(dat$Accidents)

b = 0.2
a = b*mean_accidents

cat(paste("First analysis with informative Gamma( a , b ) prior with:",
paste("a = ", toString(a), sep = ""),
paste("b = ", toString(b), sep = ""),
"", sep = "\n"))

# Plot prior

split.screen(c(2,2))
screen(1)
xs = seq(5,35,0.1)
plot(xs, dgamma(xs, a, b), type = "l", main = "Informative prior distribution", xlab = "intensity")

#pause("")

# Based on the data, the posterior is Gamma with updated parameters a_post = a + sum_i fatalities, b_post = b + 10.0, where 10.0 = # years of exposure

a_post = a + sum(dat$Accidents)
b_post = b + 10.0

# 95% posterior interval for Poisson intensity  based on 2.5% and 97.5% quantiles 

lower = qgamma(0.025, a_post, b_post)
upper = qgamma(0.975, a_post, b_post)

cat(paste("95% posterior interval for Poisson intensity:", 
paste("Lower: ", toString(round(lower,3)), sep = ""),
paste("Upper: ", toString(round(upper,3)), sep = ""),
"", sep = "\n"))

cord.x <- c(lower,seq(lower,upper,0.01),upper) 
cord.y <- c(0,dgamma(seq(lower,upper,0.01),a_post,b_post),0) 

screen(2)
plot(xs, dgamma(xs, a_post,b_post), type = "l", main = "Posterior interval for intensity", xlab = "intensity")
polygon(cord.x,cord.y,col='skyblue')
lines(xs,dgamma(xs, a, b), lty = 2)

#pause("Compute posterior prediction of 1986")

Poi_Gamma_mix <- function( y , a , b , exposure = 1, pm_sd = 3, dx = 0.025){
	m_gam = a / b
	sd_gam = sqrt( m_gam / b )
	ths =  seq( max( 0 , m_gam - pm_sd * sd_gam ), m_gam + pm_sd*sd_gam, dx*sd_gam)
	probs = rep(0,length(ths))
	probs = dpois(y, exposure*ths) * dgamma( ths, a , b) * dx*sd_gam
	sum(probs)
}

ys = seq(0,100,1)
probs = rep(0,length(ys))

for (i in 1:length(ys)){
	probs[i] = Poi_Gamma_mix(ys[i], a_post, b_post, pm_sd = 5)
}

cumprobs = cumsum(probs)
lower = upper = ys[1]

lower_bd = 0.025
upper_bd = 0.975

for (i in 1:length(ys)){
	if (cumprobs[i] <= lower_bd){
		lower = i
	}
	if (cumprobs[i] <= upper_bd){
		upper = i
	}
#	list(lower = lower, upper = upper)
}


cat(paste("95% prediction interval for number of accidents in 1986:",
"",
paste("Lower: ", toString(ys[lower]), sep = ""),
paste("Upper: ", toString(ys[upper]), sep = ""),
paste("Coverage: ", toString(round(cumprobs[upper] - cumprobs[lower],4)), sep = ""),
"",
sep = "\n"))

lower_inform = ys[lower]
upper_inform = ys[upper]

screen(3)
plot(ys,probs,type="l",main="Posterior predictive",lwd=3)

#pause("Verify that this gives the same result as Negative-Binomial posterior predictive")

neg_binom_density <- function( y , a , p ){
	if(y > 0 ){
	numer = seq(a,a+y-1,1)}else{numer=c(y)}
	prod(numer)/factorial(y)*p^y*(1-p)^a
}

x_new = 1.0
p_post = x_new / (b_post + x_new)

probs_nb = rep(0,length(ys))

for (i in 1:length(ys)){
        probs_nb[i] = neg_binom_density(ys[i], a_post, p_post)
}

lines(ys, probs_nb, col=2, lty=2, lwd=3)

cumprobs = cumsum(probs_nb)
lower_nb = upper_nb = ys[1]

lower_bd = 0.025
upper_bd = 0.975

for (i in 1:length(ys)){
        if (cumprobs[i] <= lower_bd){
                lower_nb = i
        }
        if (cumprobs[i] <= upper_bd){
                upper_nb = i
        }
   #     list(lower = lower, upper = upper)
}

cat(paste("95% prediction interval for number of accidents in 1986 using Negative Binomial:",
"",
paste("Lower: ", toString(ys[lower_nb]), sep = ""),
paste("Upper: ", toString(ys[upper_nb]), sep = ""),
paste("Coverage: ", toString(round(cumprobs[upper_nb] - cumprobs[lower_nb],4)), sep = ""),
"",
sep = "\n"))
#pause("BEWARE USING STANDARD R FUNCTIONS")

lines(ys, dnbinom(ys,a_post,p_post), lwd=3, col=3)

lower_bd = 0.05
upper_bd = 0.95

for (i in 1:length(ys)){
  if (cumprobs[i] <= lower_bd){
    lower_a = i
  }
  if (cumprobs[i] <= upper_bd){
    upper_a = i
  }
  #	list(lower = lower, upper = upper)
}


# store upper and lower bounds for coverage check later
upper_a = rep(upper_a,10)
lower_a = rep(lower_a,10)

#pause("")

dev.off()

split.screen(c(2,2))
screen(1)
xs = seq(5,35,0.1)
plot(xs, dgamma(xs, a, b), type = "l", main = "Informative prior distribution", xlab = "intensity")

a_post = a + sum(dat$Accidents)
b_post = b + 10.0

#cord.x <- c(lower,seq(lower,upper,0.01),upper)
#cord.y <- c(0,dgamma(seq(lower,upper,0.01),a_post,b_post),0)

screen(2)
plot(xs, dgamma(xs, a_post,b_post), type = "l", main = "Posterior interval for intensity", xlab = "intensity")
polygon(cord.x,cord.y,col='skyblue')
lines(xs,dgamma(xs, a, b), lty = 2)


#pause("Next: Show analysis with uninformative prior")

cat(paste("","Under the non-informative prior, the posterior distribution of Poisson intensity",
"is Gamma( ydot + 0.5 , 10.0 ), where ydot is sum of fatal accidents in the 10 year period","",sep = "\n"))

ls = seq(0.1,50,0.1)
ps = 1/sqrt(ls)

screen(3)
plot(ls,ps,type = "l", main = "Jeffreys prior", xlab = "intensity")

#pause("")

a_post = 1/2 + sum(dat$Accidents)
b_post = 10.0 

# 95% posterior interval for Poisson intensity  based on 2.5% and 97.5% quantiles

lower = qgamma(0.025, a_post, b_post)
upper = qgamma(0.975, a_post, b_post)

cat(paste("95% posterior interval for Poisson intensity:",
paste("Lower: ", toString(round(lower,3)), sep = ""),
paste("Upper: ", toString(round(upper,3)), sep = ""),
"", sep = "\n"))

cord.x <- c(lower,seq(lower,upper,0.01),upper)
cord.y <- c(0,dgamma(seq(lower,upper,0.01),a_post,b_post),0)

screen(4)

plot(xs, dgamma(xs, a_post,b_post), type = "l", xlab = "intensity", main = "Posterior interval for intensity")
polygon(cord.x,cord.y,col='skyblue')
lines(xs,.1/sqrt(xs),lty = 2)
#pause("")


ys = seq(0,100,1)
probs = rep(0,length(ys))

for (i in 1:length(ys)){
        probs[i] = Poi_Gamma_mix(ys[i], a_post, b_post, pm_sd = 5)
}

cumprobs = cumsum(probs)
lower = upper = ys[1]

lower_bd = 0.025
upper_bd = 0.975

for (i in 1:length(ys)){
        if (cumprobs[i] <= lower_bd){
                lower = i
        }
        if (cumprobs[i] <= upper_bd){
                upper = i
        }
        list(lower = lower, upper = upper)
}


cat(paste("95% prediction interval for number of accidents in 1986:",
"",
paste("Lower: ", toString(ys[lower]), sep = ""),
paste("Upper: ", toString(ys[upper]), sep = ""),
paste("Coverage: ", toString(round(cumprobs[upper] - cumprobs[lower],4)), sep = ""),
"",
sep = "\n"))


cat(paste("", "Compare predictions for two priors","",
"Informative prior:","",
paste("lower: ", toString(lower_inform),sep = ""),
paste("upper: ", toString(upper_inform),sep = ""),
"",
"Jeffreys prior:","",
paste("lower: ", toString(ys[lower]), sep = ""),
paste("upper: ", toString(ys[upper]), sep = ""),
"",
sep = "\n"))


#pause("Next: Part (b)")

dev.off()
##############
#
#
# (b) Assume that the number of fatal accidents in each year is independent with constant rate and exposure proportional to the number of passenger miles flown.  Set a prior for theta and determine the posterior based on the data.  Given a 95% predictive interval for number of fatal accidents in 1986 under assumption that 8*10^{11} passenger miles will be flown in that year.
#
#
#############

cat(paste("(b) Assume that the number of fatal accidents in each year is independent with constant rate and exposure proportional to the number of passenger miles flown.",
"",
"Perform similar analysis as above","",sep = "\n"))

# The death rate is reported as the number of deaths per 100 million passenger miles

base_rate = 10^{11}

dat$Miles = dat$Deaths/dat$DeathRate

dat$Miles10_11 = dat$Miles / 1000

# We again use the Jeffreys prior for the intensity, which in computing the posterior is equivalent to setting a = 1/2 and b = 0 in a gamma prior
split.screen(c(2,2))
screen(1)
plot(dat$Miles10_11 , dat$Accidents, pch = "*", main = "Plot passenger miles vs accidents")

#pause("")

a = 1/2
b = 0

a_post = a + sum(dat$Accidents)
b_post = b + sum(dat$Miles10_11)

xs = seq(0,10,0.1)

screen(3)
plot(xs, dgamma(xs, a_post,b_post), type = "l", main = "Posterior distribution", xlab = "intensity")
lines(xs,1/sqrt(xs), lty = 2)
#pause("")

ys = seq(0,100,1)
probs = rep(0,length(ys))

for (i in 1:length(ys)){
        probs[i] = Poi_Gamma_mix(ys[i], a_post, b_post, exposure = 8.0, pm_sd = 5)
}

cumprobs = cumsum(probs)
lower = upper = ys[1]

lower_bd = 0.025
upper_bd = 0.975

for (i in 1:length(ys)){
        if (cumprobs[i] <= lower_bd){
                lower = i
        }
        if (cumprobs[i] <= upper_bd){
                upper = i
        }
        list(lower = lower, upper = upper)
}


cat(paste("95% prediction interval for number of accidents in 1986:",
"",
paste("Lower: ", toString(ys[lower]), sep = ""),
paste("Upper: ", toString(ys[upper]), sep = ""),
paste("Coverage: ", toString(round(cumprobs[upper] - cumprobs[lower],4)), sep = ""),
"",
sep = "\n"))

screen(4)
plot( ys , probs , type = "l", main = "Posterior predictive",  xlab = "number of accidents")
#pause("Next: Part (c)")
dev.off()

# Store upper and lower for years 1976-1986 for later coverage check 

lower_bd = 0.05
upper_bd = 0.95

lower_b=upper_b=rep(0,10)

for (j in 1:10) {
probs = rep(0,length(ys))

for (i in 1:length(ys)){
  probs[i] = Poi_Gamma_mix(ys[i], a_post, b_post, exposure = dat$Miles10_11[j], pm_sd = 5)
}

cumprobs = cumsum(probs)
lower = upper = ys[1]

for (i in 1:length(ys)){
  if (cumprobs[i] <= lower_bd){
    lower_b[j] = ys[i]
  }
  if (cumprobs[i] <= upper_bd){
    upper_b[j] = ys[i]
  }
}


}

dev.off()

#############
#
#
# (c) Repeat (a) with "passenger deaths" replacing "fatal accidents"
#
#
#############

cat(paste("","Part (c): Repeat above analysis with passenger deaths instead of fatal accidents","",sep = ""))

# Based on the data, the posterior is Gamma with updated parameters a_post = a + sum_i fatalities, b_post = b + 10.0, where 10.0 = # years of exposure

# prior parameters for Jeffreys prior


xs = seq(600,800,1)
a = 1/2; b = 0

a_post = a + sum(dat$Deaths)
b_post = b + 10.0

# 95% posterior interval based on 2.5% and 97.5% quantiles

lower = qgamma(0.025, a_post, b_post)
upper = qgamma(0.975, a_post, b_post)

cat(paste("95% posterior interval for Poisson intensity:",
paste("Lower: ", toString(round(lower,3)), sep = ""),
paste("Upper: ", toString(round(upper,3)), sep = ""),
"", sep = "\n"))

cord.x <- c(lower,seq(lower,upper,0.01),upper)
cord.y <- c(0,dgamma(seq(lower,upper,0.01),a_post,b_post),0)

plot(xs, dgamma(xs, a_post,b_post), type = "l", main = "Posterior interval for intensity (part (c))", xlab = "intensity")
polygon(cord.x,cord.y,col='skyblue')
 #pause("")
# Compute posterior prediction of 1986 
 
ys = seq(600,1000,1)
probs = rep(0,length(ys))

for (i in 1:length(ys)){
        probs[i] = Poi_Gamma_mix(ys[i], a_post, b_post, pm_sd = 5, dx = 1.0)
}

cumprobs = cumsum(probs)
lower = upper = ys[1]

lower_bd = 0.025
upper_bd = 0.975

for (i in 1:length(ys)){
        if (cumprobs[i] <= lower_bd){
                lower = i
        }
        if (cumprobs[i] <= upper_bd){
                upper = i
        }
        list(lower = lower, upper = upper)
}


cat(paste("95% prediction interval for number of deaths in 1986:",
"",
paste("Lower: ", toString(ys[lower]), sep = ""),
paste("Upper: ", toString(ys[upper]), sep = ""),
paste("Coverage: ", toString(round(cumprobs[upper] - cumprobs[lower],4)), sep = ""),
"",
sep = "\n"))

#Store 90% prediction intervals for later coverage check

lower_c = upper_c = ys[1]

lower_bd = 0.05
upper_bd = 0.95

for (i in 1:length(ys)){
  if (cumprobs[i] <= lower_bd){
    lower_c = ys[i]
  }
  if (cumprobs[i] <= upper_bd){
    upper_c = ys[i]
  }
}

lower_c = rep(lower_c,10)
upper_c = rep(upper_c,10)

##################
#
#
#pause("(d) repeat (b) with deaths instead of accidents")
#
#
#################

dev.off()

split.screen(c(2,2))
screen(1)

plot(dat$Miles10_11, dat$Deaths, pch = "*", main = "Plot passenger miles vs deaths")

#pause("")

base_rate = 10^{11}

a = 1/2; b = 0

a_post = a + sum(dat$Deaths)
b_post = b + sum(dat$Miles10_11)

xs = seq(100,150,1)

screen(3)
plot(xs, dgamma(xs, a_post,b_post), type = "l", main = "Posterior distribution of intensity", xlab = "intensity")

ys = seq(100,1500,1)
probs = rep(0,length(ys))

for (i in 1:length(ys)){
        probs[i] = Poi_Gamma_mix(ys[i], a_post, b_post, exposure = 8.0, pm_sd = 5)
}

cumprobs = cumsum(probs)
lower = upper = ys[1]

lower_bd = 0.025
upper_bd = 0.975

for (i in 1:length(ys)){
        if (cumprobs[i] <= lower_bd){
                lower = i
        }
        if (cumprobs[i] <= upper_bd){
                upper = i
        }
        list(lower = lower, upper = upper)
}

cat(paste("95% prediction interval for number of deaths in 1986:",
"",
paste("Lower: ", toString(ys[lower]), sep = ""),
paste("Upper: ", toString(ys[upper]), sep = ""),
paste("Coverage: ", toString(round(cumprobs[upper] - cumprobs[lower],4)), sep = ""),
"",
sep = "\n"))

screen(4)

plot( ys , probs , type = "l", xlab = "Deaths")

#pause("")

dev.off()

lower_bd = 0.05
upper_bd = 0.95

lower_d=upper_d=rep(0,10)

for (j in 1:10) {
  probs = rep(0,length(ys))
  
  for (i in 1:length(ys)){
    probs[i] = Poi_Gamma_mix(ys[i], a_post, b_post, exposure = dat$Miles10_11[j], pm_sd = 5)
  }
  
  cumprobs = cumsum(probs)
  lower = upper = ys[1]
  
  for (i in 1:length(ys)){
    if (cumprobs[i] <= lower_bd){
      lower_d[j] = ys[i]
    }
    if (cumprobs[i] <= upper_bd){
      upper_d[j] = ys[i]
    }
  }
  
  
}

dev.off()
# Compare (a)-(d) based on predictive coverage

split.screen(c(2,2))
screen(1)
plot(dat$Year, dat$Accidents,pch="*",ylim=c(0,50),main = "(a)")
for(i in 1:10){
  lines(rep(dat$Year[i],2),c(upper_a[i],lower_a[i]))
}
screen(2)
plot(dat$Year, dat$Accidents,pch="*",ylim=c(0,50),main = "(b)")
for(i in 1:10){
  lines(rep(dat$Year[i],2),c(upper_b[i],lower_b[i]))
}
screen(3)
plot(dat$Year, dat$Deaths,pch="*",ylim=c(100,1500),main = "(c)")
for(i in 1:10){
  lines(rep(dat$Year[i],2),c(upper_c[i],lower_c[i]))
}
screen(4)
plot(dat$Year, dat$Deaths,pch="*",ylim=c(100,1500),main = "(d)")
for(i in 1:10){
  lines(rep(dat$Year[i],2),c(upper_d[i],lower_d[i]))
}


################
#
#
# (e) In which circumstance is the Poisson model most appropriate?
#
#
################

# It is not appropriate to assume that the number of deaths is Poisson, as these results are not independent. A single fatal accident is most likely to kill all passengers on board.  Thus, the number of fatal accidents may be reasonably assumed to be Poisson, since each flight is approximately independent of the others.  The rate at which such accidents occur is proportional to the number of total flights, not the number of passenger miles, for we expect that a plane carrying 1 passenger should have just as much chance of crashing as a plane with 100 passengers. Also, while the amount of time in the air suggests greater exposure to failure, this is unlikely to be a linear relationship. Thus, the number of flights may be the best measure of the amount of exposure. 

################
#
#
# Alternative approach to modeling in (c) and (d)
#
#
################

# Model the number of passengers on a flight as independent of the probability of crashing
# Model number of passengers as Gamma( a , b ) with a/b = sample mean and a relatively diffuse variance
dev.off()

cat(paste("Attempt a different model",sep = ""))

deaths_mean = mean(dat$Deaths/dat$Accidents)
deaths_sd = sd(dat$Deaths/dat$Accidents)

b_death = deaths_mean/deaths_sd^2
a_death = deaths_mean*b_death

x_test = seq(0,100,1)
plot(x_test,dgamma(x_test, a_death, b_death), type = "l")

# In this case, the posterior for the Poisson intensity does not depend on the number of fatalities.  We can obtain a posterior prediction of the number of fatalities by building on the model from (a)

a_post = 1/2 + sum(dat$Accidents)
b_post = 10.0

ys = seq(0,100,1)
probs = rep(0,length(ys))

for (i in 1:length(ys)){
        probs[i] = Poi_Gamma_mix(ys[i], a_post, b_post, pm_sd = 5)
}

gamma_total_deaths <- function( t , a_death , b_death , y ){
	dgamma( t , a_death*y, b_death)
}


ts = seq(200,1200,1)
prob_deaths = rep(0,length(ts))
for (i in 1:length(ts)){
	for( j in 1:length(probs) ){
		prob_deaths[i] = prob_deaths[i] + gamma_total_deaths( ts[i], a_death, b_death, ys[j]) * probs[j]
	}
}

cum_death_prob = cumsum(prob_deaths)

lower_bd = 0.025
upper_bd = 0.975

for (i in 1:length(ts)){
        if (cum_death_prob[i] <= lower_bd){
                lower = i
        }
        if (cum_death_prob[i] <= upper_bd){
                upper = i
        }
        list(lower = lower, upper = upper)
}

cat(paste("95% prediction interval for number of accidents in 1986:",
"",
paste("Lower: ", toString(ts[lower]), sep = ""),
paste("Upper: ", toString(ts[upper]), sep = ""),
paste("Coverage: ", toString(round(cum_death_prob[upper] - cum_death_prob[lower],4)), sep = ""),
"",
sep = "\n"))


#pause("Try with normal distribution of death size")

lines( x_test, dnorm(x_test, deaths_mean, deaths_sd), lty = 2)

normal_total_deaths <- function( t , mu_death , sd_death , y ){
	dnorm( t , mu_death*y , sd_death*sqrt(y) )
}


ts = seq(200,1200,1)
prob_deaths = rep(0,length(ts))
for (i in 1:length(ts)){
        for( j in 1:length(probs) ){
                prob_deaths[i] = prob_deaths[i] + normal_total_deaths( ts[i], deaths_mean, deaths_sd, ys[j]) * probs[j]
        }
}

cum_death_prob = cumsum(prob_deaths)

lower_bd = 0.025
upper_bd = 0.975

for (i in 1:length(ts)){
        if (cum_death_prob[i] <= lower_bd){
                lower = i
        }
        if (cum_death_prob[i] <= upper_bd){
                upper = i
        }
        list(lower = lower, upper = upper)
}

cat(paste("95% prediction interval for number of accidents in 1986:",
"",
paste("Lower: ", toString(ts[lower]), sep = ""),
paste("Upper: ", toString(ts[upper]), sep = ""),
paste("Coverage: ", toString(round(cum_death_prob[upper] - cum_death_prob[lower],4)), sep = ""),
"",
sep = "\n"))
 
#pause("")

# Check predictive coverage for this model

lower_bd = 0.05
upper_bd = 0.95

lower_f=upper_f=rep(0,10)
ts = seq(100,1500,1)

for (k in 1:10) {

  prob_deaths = rep(0,length(ts))
  for (i in 1:length(ts)){
    for( j in 1:length(probs) ){
      prob_deaths[i] = prob_deaths[i] + normal_total_deaths( ts[i], deaths_mean, deaths_sd, ys[j]) * probs[j]
    }
  }
  
  cum_death_prob = cumsum(prob_deaths)
  lower_f[k] = upper_f[k] = ts[1]
  
  for (i in 1:length(ts)){
    if (cum_death_prob[i] <= lower_bd){
      lower_f[k] = ts[i]
    }
    if (cum_death_prob[i] <= upper_bd){
      upper_f[k] = ts[i]
    }
  }
  
  
}

dev.off()

plot(dat$Year, dat$Deaths,pch="*",ylim=c(100,1500),main = "Alternative to (a)-(d)")
for(i in 1:10){
  lines(rep(dat$Year[i],2),c(upper_f[i],lower_f[i]))
}

### Why aren't these intervals wide enough?
# Do not account for uncertainty in number of flights per year (changes in overall exposure to accident)
# Does not account for possible non-stationarity in airline safety during this time.
# Other uncertainty in how this data was collected or is recorded?