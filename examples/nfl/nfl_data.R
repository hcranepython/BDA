dat = read.table("../../data/nfl.txt",header=TRUE)

years = c(1981, 1983, 1984, 1985, 1986, 1988, 1989, 1990,1991,1992)

season_length = 224

for(i in 1:length(years)){
        dat$years[((i-1)*season_length+1):(i*season_length)] = years[i]
}

# Put data in format for Bradley-Terry model by assigning each team a label 1,...,28 and then adding columns "winner" and "loser" with the index of the team.

teams = levels(dat$favorite.name)
teams

dat$favorite_win = rep(0,nrow(dat))

for(j in 1:nrow(dat)){
        if( dat$favorite[j] > dat$underdog[j] ){
		dat$favorite_win[j] = 1
                dat$winner[j] = which( dat$favorite.name[j] == teams )
                dat$loser[j] = which( dat$underdog.name[j] == teams )
        }else{
                dat$winner[j] = which( dat$underdog.name[j] == teams )
                dat$loser[j] = which( dat$favorite.name[j] == teams )
}
}

# Also record the winner and loser against the spread. Count ties as win for underdog as of now.

for(j in 1:nrow(dat)){
        if( dat$favorite[j] > dat$underdog[j] + dat$spread[j] ){
                dat$winner_spread[j] = which( dat$favorite.name[j] == teams )
                dat$loser_spread[j] = which( dat$underdog.name[j] == teams )
        }else{
                dat$winner_spread[j] = which( dat$underdog.name[j] == teams )
                dat$loser_spread[j] = which( dat$favorite.name[j] == teams )
}
}

# Tally team wins and losses each year

records = matrix(0,length(teams),2*length(years),dimnames = list(teams,c(paste("W-",years,sep=""), paste("L-",years,sep=""))))
win_pct = matrix(0,length(teams),length(years), dimnames = list(teams,years))

for(i in 1:nrow(dat)){
	yr = dat$years[i]
	yrW = paste("W-",toString(yr),sep="")
	yrL = paste("L-",toString(yr),sep="")
	records[teams[dat$winner[i]],yrW] = records[teams[dat$winner[i]],yrW] + 1	
	records[teams[dat$loser[i]],yrL] = records[teams[dat$loser[i]],yrL] + 1
}

for(j in 1:length(years)){
	yr = years[j]
        yrW = paste("W-",toString(yr),sep="")
        yrL = paste("L-",toString(yr),sep="")
	for(i in 1:length(teams)){
		win_pct[teams[i],toString(years[j])] = records[teams[i],yrW]/(records[teams[i],yrW]+records[teams[i],yrL])
	}
}



