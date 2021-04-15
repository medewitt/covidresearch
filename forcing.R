
start_growth <- 200
end_growth <- 350
amplitude <- .5

force_growth <- function(t, amplitude = .3,
    start_growth = 200, end_growth = 50) {

    if(start_growth < end_growth){
    if (t %% 365 < start_growth | t %% 365 > end_growth) {
        return(1)
    }
    } else {
        if (t %% 365 < start_growth & t %% 365 > end_growth) {
        return(1)
    }
    }
    if ( end_growth < start_growth) {
        theta <- (365 - start_growth + end_growth)/2

    } else {
        theta <- (end_growth - start_growth)/2
    }

    if(end_growth < start_growth & t %% 365 < end_growth) {
        over_year_correction <- 365
    } else {
        over_year_correction <- 0
    }

    1 + amplitude * cos(2 * pi * (t %% 365-start_growth - theta + over_year_correction) / (theta * 4))
}

out <- do.call(rbind, lapply(1:730, force_growth))

plot(out)
start_growth <- 200
end_growth <- 50
theta <- (365 - start_growth + end_growth)/2
1 + amplitude * cos(2 * pi * (1:50 %% 365-200 - theta+365) / (theta * 4))