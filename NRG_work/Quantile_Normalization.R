quantile_normalization <- function(df){
  df_rank <- apply(df,2,rank,ties.method="min")
  df_sorted <- data.frame(apply(df, 2, sort))
  df_mean <- apply(df_sorted, 1, mean)
  
  index_to_mean <- function(my_index, my_mean){
    return(my_mean[my_index])
  }
  
  df_final <- apply(df_rank, 2, index_to_mean, my_mean=df_mean)
  rownames(df_final) <- rownames(df)
  return(df_final)
}


#test the function  country_2=c(4,1,4,2,1,5,3,2),
df <- data.frame(country_1=c(5,5,5,1,4,3,4,2),
                 country_2=c(5,3,2,4,1,4,2,1),
                 country_3=c(3,2,2,5,2,4,3,1)
)
df <- data.frame(one=c(5,2,3,4),
                 two=c(2,1,5,4),
                 three=c(3,4,6,8)
)
#rownames(df) <- toupper(letters[1:8])
df

boxplot(df)
summary(df)

dff <- quantile_normalization(df)
dff

key <- unique(as.vector(as.matrix(dff)))
val <- rank(key)

lapply(1:length(dff), FUN = function(i){dff[dff == key[i]] <<- val[i]})
dff

boxplot(dff)
