## -----------------------------------------------------------------------------
prob_point <- function(n, k){
  a <- rep(0,n)
  b <- rep(0,n)
  output <- 0
  for (i in 1:1000){
    for (j in 1:n){
      a[j] <- sample(1:k)
      b[j] <- sum(a)
    }
    if (n %in% b){
      output = output + 1
    }
    a <- rep(0,n)
    b <- rep(0,n)
  }
  print(output / 1000)
}

## -----------------------------------------------------------------------------
knapsack_problem <- function(n, c, w, v){
  value <- matrix(0, nrow = n+1, ncol = c+1)
  w <- c(0,w)
  v <- c(0,v)
  print(nrow(value))
  for (i in 2:nrow(value)){
    for (j in 2:ncol(value)){
      if (j-1 >= w[i]){
        # 考虑价格拿不拿
        value[i,j] <- max(value[i-1,j-w[i]]+v[i],value[i-1,j])
      }else{
        # 不能拿了
        value[i,j] <- value[i-1,j]
      }
    }
  }
  print(value)
  print('最大价值为')
  print(value[n+1,c+1])
  
  final_stuff = c()
  final_value = c()
  j = ncol(value)
  for (i in nrow(value):2){
    if (value[i,j] > value[i-1,j]){
      final_stuff = c(final_stuff, i)
      final_value = c(final_value,v[i])
      j = j - w[i]
    }
  }
  print('拿出来的物品为')
  print(rev(final_stuff-1))
  print('拿出来物品的总价值为')
  print(rev(final_value))
}

