## Analysis of NFL point spread data
setwd("~/Dropbox/Rutgers/Teaching/STAT 668 - Bayesian Analysis/GitHub/BDA/examples/nfl")
library(rmutil)
source("nfl_data.R")
source("nfl_functions_class.R")

head(dat)

dat_1_5 = dat[which(dat$years %in% years[1:5]),]
dat_6_10 = dat[which(dat$years %in% years[6:10]),]

# First model: 
# p ~ p^{-1/2}*(1-p)^{-1/2}
# Pr(home team wins | p) = p conditionally independently for all games

# Fit model to first 5 years of data
# Test on the remaining 5

home_wins = sum(dat_1_5$home*dat_1_5$favorite_win + (1-dat_1_5$home)*(1-dat_1_5$favorite_win))

# posterior parameters of Beta( a , b ) distribution

a_post = 1/2 + home_wins
b_post = 1/2 + nrow(dat_1_5) - home_wins

# plot posterior

ps = seq(0.01,0.99,0.01)
plot(ps, dbeta(ps, a_post, b_post), type = "l", main = "Posterior", xlab = "p", ylab = "density")

# Posterior predictive is Beta-Binomial with parameters a_post, b_post and N

ys = seq(300,1000,1)
N = nrow(dat_6_10)

# state parameters for dbetabinom function

s = nrow(dat_1_5)
m = (home_wins+1/2)/(nrow(dat_1_5)+1)

plot(ys, dbetabinom( ys, N, m, s), type = "l", main = "posterior prediction", xlab = "home wins", ylab = "probability")


home_win_pct_year = rep(0,length(years))

for(i in 1:length(years)){
	dat_yr = dat[which(dat$years ==  years[i]),]
	home_win_pct_year[i] = sum(dat_yr$home*dat_yr$favorite_win + (1-dat_yr$home)*(1-dat_yr$favorite_win)) / nrow(dat_yr)
}

N = sum(which(dat$years == years[1]))
lower = qbetabinom(0.25, N, m, s) / N
upper = qbetabinom(0.75, N, m, s) / N

plot( years, home_win_pct_year, pch = "*")
for(i in 1:length(years)){
	lines(c(years[i],years[i]), c(lower,upper))
}


# What if we fit to the full dataset?

home_wins = sum(dat$home*dat$favorite_win + (1-dat$home)*(1-dat$favorite_win))

# posterior parameters of Beta( a , b ) distribution

a_post = 1/2 + home_wins
b_post = 1/2 + nrow(dat) - home_wins

# plot posterior

ps = seq(0.01,0.99,0.01)
plot(ps, dbeta(ps, a_post, b_post), type = "l", main = "Posterior", xlab = "p", ylab = "density")

# Posterior predictive is Beta-Binomial with parameters a_post, b_post and N

ys = seq(300,1000,1)
N = nrow(dat_6_10)

# state parameters for dbetabinom function

s = nrow(dat)
m = (home_wins+1/2)/(nrow(dat)+1)

plot(ys, dbetabinom( ys, N, m, s), type = "l", main = "posterior prediction", xlab = "home wins", ylab = "probability")


home_win_pct_year = rep(0,length(years))

for(i in 1:length(years)){
  dat_yr = dat[which(dat$years ==  years[i]),]
  home_win_pct_year[i] = sum(dat_yr$home*dat_yr$favorite_win + (1-dat_yr$home)*(1-dat_yr$favorite_win)) / nrow(dat_yr)
}

N = sum(which(dat$years == years[1]))
lower = qbetabinom(0.25, N, m, s) / N
upper = qbetabinom(0.75, N, m, s) / N

lower80 = qbetabinom(0.1, N, m, s) / N
upper80 = qbetabinom(0.9, N, m, s) / N

plot( years, home_win_pct_year, pch = "*")
for(i in 1:length(years)){
  lines(c(years[i],years[i]), c(lower,upper))
  lines(c(years[i],years[i]), c(lower80,upper80), lty=2)
}


#########
# Second model:
# Assume probability constant for being favorite
# p ~ Jeffreys
# Pr(win | favorite, p) = p

# plot posterior of alpha

favorite_wins = sum(dat_1_5$favorite_win)

a_post = 1/2 + favorite_wins
b_post = 1/2 + nrow(dat_1_5) - favorite_wins


ps = seq(0.01,0.99,0.01)
plot(ps, dbeta(ps, a_post, b_post), type = "l", main = "Posterior", xlab = "p", ylab = "density")

# Posterior predictive is Beta-Binomial with parameters a_post, b_post and N

ys = seq(300,1000,1)
N = nrow(dat_6_10)

# state parameters for dbetabinom function

s = nrow(dat_1_5)
m = (favorite_wins+1/2)/(nrow(dat_1_5)+1)

plot(ys, dbetabinom( ys, N, m, s), type = "l", main = "posterior prediction", xlab = "home wins", ylab = "probability")

# Favorite win percentage per year

fav_win_pct_year = rep(0,length(years))

for(i in 1:length(years)){
        dat_yr = dat[which(dat$years ==  years[i]),]
        fav_win_pct_year[i] = sum(dat_yr$favorite_win) / nrow(dat_yr)
}

N = sum(which(dat$years == years[1]))
lower = qbetabinom(0.25, N, m, s) / N
upper = qbetabinom(0.75, N, m, s) / N

plot( years, fav_win_pct_year, pch = "*")
for(i in 1:length(years)){
        lines(c(years[i],years[i]), c(lower,upper))
}

# Fit to full dataset


favorite_wins = sum(dat$favorite_win)

a_post = 1/2 + favorite_wins
b_post = 1/2 + nrow(dat) - favorite_wins


ps = seq(0.01,0.99,0.01)
plot(ps, dbeta(ps, a_post, b_post), type = "l", main = "Posterior", xlab = "p", ylab = "density")

# Posterior predictive is Beta-Binomial with parameters a_post, b_post and N

ys = seq(300,1000,1)
N = nrow(dat_6_10)

# state parameters for dbetabinom function

s = nrow(dat_1_5)
m = (favorite_wins+1/2)/(nrow(dat)+1)

plot(ys, dbetabinom( ys, N, m, s), type = "l", main = "posterior prediction", xlab = "home wins", ylab = "probability")

# Favorite win percentage per year

fav_win_pct_year = rep(0,length(years))

for(i in 1:length(years)){
  dat_yr = dat[which(dat$years ==  years[i]),]
  fav_win_pct_year[i] = sum(dat_yr$favorite_win) / nrow(dat_yr)
}

N = sum(which(dat$years == years[1]))
lower = qbetabinom(0.25, N, m, s) / N
upper = qbetabinom(0.75, N, m, s) / N
lower80 = qbetabinom(0.1, N, m, s) / N
upper80 = qbetabinom(0.9, N, m, s) / N

plot( years, fav_win_pct_year, pch = "*")
for(i in 1:length(years)){
  lines(c(years[i],years[i]), c(lower,upper))
  lines(c(years[i],years[i]), c(lower80,upper80), lty=2)
}

# Coverage appears to be poor. The model is overstating precision
# Incorporate information about the point spread
#
# logit Pr(team t wins | spread) = a*home(t) + beta*spread(t), where spread(t) is the amount +-
# that team t is assessed prior to the game.
#
# y is the outcomes for the home team, s is the spreads for the home team, a and b are the logit parameters

home_win = dat$home*dat$favorite_win + (1-dat$home)*(1-dat$favorite_win)
home_spread = dat$home*dat$spread - (1-dat$home)*dat$spread
dat$home_win = home_win
dat$home_spread = home_spread

mod = glm(home_win ~ 1 + home_spread, data = dat, family = binomial(link=logit))

a = mod$coefficients[1]
b = mod$coefficients[2]

dat$prob_home_win = exp(a+b*dat$home_spread) / (exp(a+b*dat$home_spread) + 1)

lower_year50 = rep(0,10); upper_year50 = rep(0,10)
lower_year80 = rep(0,10); upper_year80 = rep(0,10)
brier_lower50 = brier_upper50 = rep(0,10)
brier_lower80 = brier_upper80 = rep(0,10)
home_wins_year = rep(0,10)
brier_year = rep(0,10)

iter = 1000

for(i in 1:10){
  inds = which(dat$years == years[i])
  wins = rep(0,iter)
  brier = rep(0,iter)
  for(j in 1:iter){
    for(k in 1:length(inds)){
      outcome = rbinom(1,1,dat$prob_home_win[inds[k]])
      wins[j] = wins[j] + outcome
      brier[j] = brier[j] + (outcome - dat$prob_home_win[inds[k]])^2
    }
  }
  wins = sort(wins); brier = sort(brier/length(inds))
  home_wins_year[i] = sum(dat$home_win[inds])
  brier_year[i] = sum((dat$home_win[inds]-dat$prob_home_win[inds])^2)/length(inds)
  lower_year50[i] = wins[251]; upper_year50[i] = wins[751]
  lower_year80[i] = wins[101]; upper_year80[i] = wins[901]
  brier_lower50[i] = brier[251]; brier_upper50[i] = brier[751]
  brier_lower80[i] = brier[101]; brier_upper80[i] = brier[901]
}

split.screen(c(1,2))
screen(1)
plot(years,home_wins_year,pch="*",main="Posterior Predict")
for(i in 1:10){
  lines(c(years[i],years[i]),c(lower_year50[i],upper_year50[i]))
  lines(c(years[i],years[i]),c(lower_year80[i],upper_year80[i]),lty=2)
}
screen(2)
plot(years,brier_year,pch="*",main="Brier Scores",ylim=c(0.15,0.25))
for(i in 1:10){
  lines(c(years[i],years[i]),c(brier_lower50[i],brier_upper50[i]))
  lines(c(years[i],years[i]),c(brier_lower80[i],brier_upper80[i]),lty=2)
}

#########################
## Model 3: Hierarchical model
## Allow the win probability for home team to vary each year
## Using the setup from Section 5.3 of BDA
## Prior on (a,b) defined by (a,b) ~ (a+b)^{-5/2}

home_win_year = home_win_pct_year * nrow(dat)/10
games_year = rep(nrow(dat)/10,10)

posterior_a_b <- function(a , b, wins_year, games_year){
	post = (a+b)^{-5/2}
	for(i in 1:length(games_year)){
		s = a
		m = a/(a+b)
#		m = (wins_year[i]+a) / (games_year[i] + a + b)
		post = post * dbetabinom( wins_year[i], games_year[i], m, s)
	}
	post
}

posterior_grid <- function(as, bs, wins_year, games_year){
	post_grid = matrix(0,length(as),length(bs))
	for(i in 1:length(as)){for(j in 1:length(bs)){
		post_grid[i,j] = posterior_a_b(as[i], bs[j], wins_year, games_year)
	}}
	post_grid
}


da = 0.5; db = 0.5
a = seq(150,250,da)
b = seq(120,160,db)

posterior = posterior_grid(a,b,home_win_year,games_year)*da*db

marginal_a = rep(0,length(a))

for(i in 1:length(marginal_a)){
	marginal_a[i] = sum(posterior[i,])
}

iter = 1000
b_sim = rep(0,iter)

a_sim = sample(length(a), iter, replace = TRUE, prob = marginal_a)
	for(j in 1:length(a_sim)){
		b_sim[j] = sample(length(b), 1, prob = posterior[a_sim[j],])
}

new_games = 224
y_rep = rep(0,iter)

for(i in 1:iter){
	p = rbeta(1,a[a_sim[i]], b[b_sim[i]])
	y_rep[i] = rbinom(1,new_games,p)
}

y_quant = sort(y_rep)

lower = y_quant[251]; upper = y_quant[751]
lower80 = y_quant[101]; upper80 = y_quant[901]
lower90 = y_quant[51]; upper90 = y_quant[951]

sum((home_win_year >= lower)*(home_win_year<=upper))
sum((home_win_year >= lower80)*(home_win_year<=upper80))
sum((home_win_year >= lower90)*(home_win_year<=upper90))

dev.off()

split.screen(c(1,2))
screen(1)

plot( years, home_win_year, pch = "*", ylim = c(100,160),main="Sampled from posterior")
for(i in 1:length(years)){
  lines(c(years[i],years[i]), c(lower,upper), lwd=2)
  lines(c(years[i],years[i]), c(lower80,upper80))
  lines(c(years[i],years[i]), c(lower90,upper90), lty=2)
}


## Approximate posterior by matching moments to the beta distribution

p = mean(home_win_year/games_year)
v = var(home_win_year/games_year)

# mean of beta = a/(a+b)
# variance of beta = a/(a+b)*b/(a+b)*(1/(a+b+1)) = p*(1-p)/(1+a+b)
# Therefore, 
# a = p*(a+b) = p*(p*(1-p)/v-1)
# b = a/p - a
a = p*((1-p)/v - 1)
b = a/p - a

for(i in 1:iter){
        p = rbeta(1,a, b)
        y_rep[i] = rbinom(1,new_games,p)
}

y_quant = sort(y_rep)

lower = y_quant[251]; upper = y_quant[751]
lower80 = y_quant[101]; upper80 = y_quant[901]
lower90 = y_quant[51]; upper90 = y_quant[951]

screen(2)
plot( years, home_win_year, pch = "*", ylim = c(100,160), main = "using glm estimates")
for(i in 1:length(years)){
	lines(c(years[i],years[i]), c(lower,upper), lwd=2)
	lines(c(years[i],years[i]), c(lower80,upper80))
	lines(c(years[i],years[i]), c(lower90,upper90), lty=2)
}


# Possibly the intervals are too wide, meaning that the model has too much uncertainty in it, even without accounting for the remaining uncertainty in the values of alpha and beta.

# How might this model be improved?
