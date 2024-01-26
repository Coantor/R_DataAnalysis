library(tidyverse)

## 设置参数
pars <- tibble(
  prob = c(0.3,0.7),
  mu = c(-2,4),
  sigma = c(1,4)
)
N = 1000

## 生成样本
points = pars %>% 
  mutate(data = pmap(., ~ rnorm(..1 * N,..2,..3))) %>% 
  unnest(data)

## em算法函数

EM <- function(points,tol = 1e-6,verdose = TRUE,max.iter = 100){
  x = points %>% pull(data)
  n = length(x)
  k = points %>% distinct(mu) %>% nrow()
  
  n_prob = rep(1/k,k)
  n_mu = rnorm(k)
  n_sigma = runif(k)
  
  Gamma_mat = matrix(0,nrow = n,ncol = k)
  i = 1
  Q_vec = rep(0,max.iter)
  while(i < max.iter){
    # e步,在参数 mu,sigma条件下给出最好的估计
    for(j in 1:k){
      Gamma_mat[,j] = dnorm(x,n_mu[j],n_sigma[j])*n_prob[j]
    }
    Gamma_mat = Gamma_mat/rowSums(Gamma_mat)
    
    # 在e步之后计算Q函数
    for(j in 1:k){
      Q_j = sum(Gamma_mat[,j] * (log(1/sqrt(2*pi)) - log(n_sigma[j]) - 1/(2*n_sigma[j]^2)*(x-n_mu[j]) )) + sum(Gamma_mat[,j]*log(n_prob[j]))
      
      Q_vec[i] = Q_vec[i] + Q_j
    }
    
    
    # m步,最大化参数
    pre_prob = n_prob
    pre_mu = n_mu
    pre_sigma = n_sigma
    
    for(j in 1:k){
      sum1  =  sum(Gamma_mat[,j])
      sum2  = sum(x * Gamma_mat[,j])
      n_prob[j] = sum1/n
      n_mu[j] = sum2/sum1
      sum3     <- sum(Gamma_mat[, j]*(x-n_mu[j])^2)
      n_sigma[j] = sqrt(sum3/sum1)
    }
    
    if( sum((n_prob - pre_prob)^2 ) < tol & 
        sum((n_sigma - pre_sigma)^2 ) < tol & 
        sum((n_mu - pre_mu)^2 ) < tol ){
      break
    }
    if(verdose == TRUE){
    cat('step',i, 'alpha', n_prob, 'mu', n_mu, 'sigma', n_sigma, '\n')
    }
  i = i + 1
  }#while
  
  return(Q_vec[1:i])
}

Q_vec = EM(points,verdose = FALSE)



