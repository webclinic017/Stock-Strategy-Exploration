TripS_Func = function(DF){
  Trip_Starts = which(DF$Counter == 1)
  Trip = 0
  DF$Trip = Trip
  for(i in 1:length(Trip_Starts)){
    Trip = Trip + 1
    if(i == 1 & length(Trip_Starts) == 1){
      DF$Trip = Trip
    }else if(i == length(Trip_Starts)){
      DF[Trip_Starts[i]:nrow(DF),"Trip"] = Trip 
    }else{
      DF[Trip_Starts[i]:Trip_Starts[i+1],"Trip"] = Trip 
    }
  }
  return(DF)
}