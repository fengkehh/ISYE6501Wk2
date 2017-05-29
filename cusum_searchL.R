# cusum_L() detects a DECREASING change in x.
# Arguments:
# x: numerical vector of the quantity to undergo detection
# n: critical value factor (integer) to specify detectable shift as 
# 1/2*n*sigma(x)
# T_fac: threshold value factor (integer) to specify mean shift threshold as 
# T_fac*sigma(x)
# 
# mean(x) and sigma(x) is estimated from the first 25 values of x.
# Algorithm details: https://www.mathworks.com/help/signal/ref/cusum.html
# 
# Returns:
# index of when the first change is detected.
# 0 otherwise.
cusum_L <- function(x, n, T_fac = -1) {
    S_t <- 0
    mu <- mean(x[1:25])
    
    sigma <- sd(x[1:25])
    
    C <- 1/2*n*sigma
    
    if (T_fac < 0) {
        T <- 1*sigma
    } else {
        T <- T_fac*sigma
    }
    
    
    for (i in 2:length(x)) {
        
        S_t <- max(0, S_t + mu - x[i] - C)
        
        if (S_t >= T) {
            return(i)
        }
    }
    return(0)
}


# Carries out TWO 2-level nested grid search to find optimal C and T values.
# Arguments:
# data: data frame containing the time stamps and temperature data to be used for 
# training. First column is time stamps, other columns represent data collected 
# in that particular year specified by the column name.
# 
# answer: data frame containing manually chosen starting dates of decreasing 
# temperature of the year specified by column names.
# 
# C_range: range for C factor in the form c(C_start, C_end)
# 
# T_range: range for T factor in the form c(T_start, T_end)
# 
# grid_n: number of grid points. Number of intervals is grid_n - 1.
# 
# level: search level (search stops at 2)
cusum_searchL <- function(data, answer, C_range, T_range, grid_n, level = 1) {
    C_seq <- seq(C_range[1], C_range[2], length.out = grid_n)
    T_seq <- seq(T_range[1], T_range[2], length.out = grid_n)
    
    min_ss <- Inf
    indices <- rep(0, ncol(data) - 1)
    C_ind <- 0
    T_ind <- 0
    
    for (c_ind in 1:length(C_seq)) {
        for (t_ind in 1:length(T_seq)) {
            curr_ss <- 0
            temp_indices <- rep(0, length(indices))
            
            C <- C_seq[c_ind]
            T <- T_seq[t_ind]
            
            for (i in 2:ncol(data)) {
                index = cusum_L(data[,i], C, T)
                
                if (index > 0) {
                    # Found cusum index. Compute squares and add to curr_ss
                    curr_ss <- curr_ss + 
                        as.numeric(data[index, 1] - answer[1,i-1])^2
                    
                    temp_indices[i-1] <- index
                    
                } else {
                    # Failed to find cusum index. Set curr_ss to Inf and break.
                    curr_ss <- Inf
                    break
                }
            }
            
            if (curr_ss < min_ss) {
                min_ss <- curr_ss
                indices <- temp_indices
                C_ind = c_ind
                T_ind = t_ind
            }
        }
    }
    
    if ((C_ind == 0 ) || (T_ind == 0)) {
        return(NA)
        
    } else if (level == 1) {
        if (C_ind == 1) {
            C_range <- c(C_seq[C_ind], C_seq[C_ind + 2])
            
        } else if (C_ind == length(C_seq)) {
            C_range <- c(C_seq[C_ind - 2], C_seq[C_ind])
            
        } else {
            C_range <- c(C_seq[C_ind - 1], C_seq[C_ind + 1])
            
        }
        
        if (T_ind == 1) {
            T_range <- c(T_seq[T_ind], T_seq[T_ind + 2])
            
        } else if (T_ind == length(T_seq)) {
            T_range <- c(T_seq[T_ind - 2], T_seq[T_ind])
            
        } else {
            T_range <- c(T_seq[T_ind - 1], T_seq[T_ind + 1])
            
        }
        
        return(cusum_searchL(data, answer, C_range, T_range, grid_n, level+1))
        
    } else {
        ans = list(C_val = C_seq[C_ind], T_val = T_seq[T_ind], ss = min_ss, 
                   indices = indices)
        
        return(ans)
    }
    
    
}