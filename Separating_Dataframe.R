data(diamonds)
data(mtcars)

var_logical <- diamonds[which(lapply(diamonds, is.logical) == TRUE)]

#Create a logical vector and add to diamonds
diamonds$vs <- rep((mtcars$vs == 1)[3:22], 2697)


#Separate the columns by mode
var_num <- diamonds[which(lapply(diamonds, is.numeric) == TRUE)]
var_factor <- diamonds[which(lapply(diamonds, is.factor) == TRUE)]
var_logical <- diamonds[which(lapply(diamonds, is.logical) == TRUE)]
