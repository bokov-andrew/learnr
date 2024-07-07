GRID_SIZE <- 4
board <- matrix(0,GRID_SIZE,GRID_SIZE)
init <- sample(c(2,4),1,TRUE,prob=c(75,25))

add_new_tile <- function(n=1,values=c(2,4),prob=1/values){
  print(prob)
  sample(values,n,TRUE,prob=prob)
}

valid_cells <- (1:GRID_SIZE^2) [board[1:GRID_SIZE^2]==0]
xx <- add_new_tile(10,2^(1:11))
xxsorted <- c(xx[xx==0],xx[xx>0])

swiperow <- function(row){
  row <- c(row[row==0],row[row>0])
  for(ii in length(row):2){
    if(row[ii]==row[ii-1]){
      row[c(ii-1,ii)] <- c(0,row[ii]*2)
      return(row)
    }
  }
}

swipeboard <- function(board,direction=c("up","down","left","right")){
  direction <- match.arg(direction)
  transformboard <- if (direction %in% c("left","right")) t else identity
  transformrow <- if (direction %in% c("left","up")) rev else identity
  out <- transformboard(apply(board,1,function(xx){
    xx <- transformrow(xx)
    while(!identical(xx,xx0 <- swiperow(xx))){xx <- xx0};
    transformrow(xx)}))
  out
}

while(!identical(baz,baz0 <- swiperow(baz))){
  baz <- baz0
}
t(apply(foo,1,function(xx){while(!identical(xx,xx0 <- swiperow(xx))){xx <- xx0}; xx}))