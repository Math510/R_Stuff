#Create a frquency table using table function
freq_a <- table(var_factor[1])
freq_b <- table(var_factor)
str(freq_a)
str(freq_b)

#Create correlation matrix using cor function
#Specify pearson correlation
corr_a <- cor(var_num, method="pearson")
str(corr_a)
length(corr_a)
length(corr_a[,1])

#how to deal with naming?
?paste

names(corr_a[,1])[[1]] #Name of the first row
names(corr_a[1,])[[2]] #Name of the second column

newname <- paste(names(corr_a[,1])[[1]],names(corr_a[1,])[[2]],sep="-")

#Create some data frames
pairs <- rep("names", 7)
values <- rep(0.0, 7)
mydf <- data.frame(cbind(pairs, values))
names(mydf)[1] <- "Pair"
names(mydf)[2] <- "Value"

#Realize we only need to deal with the upper right triangle of
#the correlation matrix

#Set up a threshold and null vectors before entering loop
threshold <- 0.25
r_square <- NULL
rsq_names <- NULL
corr_list <- NULL
corr_names <- NULL

#Get the length of the one dimension of the square matrix
len <- length(corr_a[1,])

#Only loop through the upper right triangle
for (i in (1:(len-1))) {
  for (j in ((i+1):len)) {
    #Form the name pair and add to the named pair vector
    pair_name <- paste(names(corr_a[,1])[[i]],names(corr_a[1,])[[j]],sep="-")
    rsq_names <- c(rsq_names, pair_name)
    
    #Add the r_square value to the value vector
    r_square <- c(r_square, corr_a[i,j]^2)
    
    #if the threshold is exceeded, add the name and value to the
    #respective correlation vectors
    if (corr_a[i,j] > threshold) {
      corr_names <- c(corr_names, pair_name)
      corr_list <- c(corr_list, corr_a[i,j]) 
    }
  }
}

#create the dataframes and label the columns
rsq_df <- data.frame(cbind(rsq_names, r_square))
names(rsq_df)[1] <- "Pair"
names(rsq_df)[2] <- "Value"
corr_df <- data.frame(cbind(corr_names, corr_list))
names(corr_df)[1] <- "Pair"
names(corr_df)[2] <- "Value"
rsq_corr_list <- list("rsquare"=rsq_df, "correlation"=corr_df)
