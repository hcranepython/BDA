
beta_binomial_density <- function(y, N, a, b){
	choose(N,y) * beta(y + a, N-y+b)
}


## Proper scoring rules

Brier_score <- function( probs, outcomes){
        N = length(probs)
        observed = sum((probs-outcomes)^2) / N
        expected = sum(probs*(1-probs)) / N
        list(observed = observed, expected = expected)
}

Log_score <- function( probs, outcomes){
        N = length(probs)
        observed = sum(log(probs*outcomes + (1-probs)*(1-outcomes))) / N
        expected = sum(probs*log(probs) + (1-probs)*log(1-probs)) / N
        list(observed = observed, expected = expected)
}

empirical_sample_score <- function( probs , iter = 1000 , rule = "Brier" ){
        tally = matrix(0,length(probs),iter)
        scores = rep(0,iter)
        for(i in 1:length(probs)){
                tally[i,] = sample(2,iter,TRUE,c(1-probs[i],probs[i]))-1
        }
        for(j in 1:iter){
                if( rule == "Brier" ){
                        scores[j] = Brier_score( probs, tally[,j] )$observed
                }else{
                if( rule == "Log" ){
                        scores[j] = Log_score( probs, tally[,j] )$observed
                }}
        }
        sort(scores)
}


