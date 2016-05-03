U <- c(1,2,3) #U degreee ;worker
lu <- c(0,0,0) #penalty

V <- c(1,2) #v 1-link only ;task

E <- list( c(1,1)
          ,c(2,1)
          ,c(3,2)
         )
result <- list()

m <- matrix(c(1,1,0,0,0,1,1,0,0), byrow = TRUE, nrow = 3)

#greedy
semi <- function(){
  sapply(V, function(v){
    edge <- 0
    index <- 0
    min <- Inf
    
    sapply(E, function(e){
      if(v == e[2]){
        if(lu[e[1]] < min){
          min <<- lu[e[1]]
          edge <<- e
          index <<- e[1]
        }
      }
    })
    
    result <<- c(result, list(edge)) 
    lu[index] <<- lu[index] + 1
  })
  
  result 
}

sm1 <- function(){
  # M is the semi-matching that is built by this algorithm. Initially M is empty.
  # Q is a queue used for performing breadth-first search. Initially Q is empty.
  M <- c()
  Q <- c()
  
  S <- c()
  # For each root in U
  for(root in V){
    # Perform breadth-first search from root
    # Remove all entries from Q
    # Enqueue(Q, root)
    Q <- c()
    Q <- c(Q,root)
    
    # S is the set of vertices visited during this breadth-first search
    # Let S = { root }
    S <- unique(c(S,root))
    
    # bestV is the V-vertex with least load found so far
    # Let bestV = null
    bestU <- NULL
    
    # While Q is not empty
    while(length(Q) > 0){
      # Let w = Dequeue(Q)
      w <- Q[1]
      Q <- Q[-1]
      
      # If w in U Then
      if(w %in% V){
    
      }
    }
  }
  
 
 
  # Let N = UnmatchedNeighbors(w)
  # Else
  # Let N = MatchedNeighbors(w)
  # If bestV = null Or degM
  # (w) < degM
  # (bestV) Then
  # Let bestV = w
  # End
  # End
  # For each n in N
  # Let Parent(n) = w
  # Enqueue(Q, n)
  # End
  # Let S = S union N
  # End
  # // Switch edges along path from bestV to root, thus increasing the size of M
  # Let v = bestV
  # Let u = Parent(v)
  # Add {u,v} to M
  # While u 6= root
  # Let v = Parent(u)
  # Remove {u,v} from M
  # Let u = Parent(v)
  # Add {u,v} to M
  # End
  # End

  
  
  M
}



