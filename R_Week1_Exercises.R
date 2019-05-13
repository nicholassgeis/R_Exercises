# Exercise 1 (ANSWER: 234168)
# Sum of all numbers that are divisble by 3 or 5 up to n (In this problem: n = 1000)

sumsof3and5 <- function(n){
  s <- 0
  for (i in 1:n){
    if ((i %% 3 == 0) | (i %% 5 == 0)){
      s <- s + i
    }
  }
  s
}

# Exercise 2 (ANSWER: 4613732)
# Sum of all even Fibonacci numbers less than n (In this problem: n = 4000000)

evenfibonaccisums <- function(n){
  
  #These variables will represent the current and previous numbers in the fibonacci sequence
  cnum <- 2
  pnum <- 1
  
  #This variable will keep track of the sum of all even Fibonacci numbers.
  #It will start at 2 so we will start counting after iterating the process
  s <- 0
  
  while (cnum < n + 1){
    
    if (cnum %% 2 == 0){
      s <- s + cnum
    }
    
    # First we assign a temporary variable to remember the value of the current Fibonacci
    # number. Then we update cnum to the next Fibonacci number and finally set pnum to the old
    # cnum value stored in tempnum
    tempnum <- cnum
    cnum <- cnum + pnum
    pnum <- tempnum
    
  }
  s
}

# Exercise 3 (ANSWER: 6857)
# Compute the largest prime factor of a number n (In this problem: n = 600851475143)

# This function finds the first number greater than 1 that divides n, i.e. the smallest
# prime factor of n.
smallestprimefactor <- function(n){
  prime <- 0
  midpoint <- as.integer(sqrt(n))
  
  for (i in 2:midpoint){
    if (n %% i == 0){
      prime <- i
      break
    }
  }
  if (prime == 0){
    prime <- n
  }
  prime
}

# Using the smallest prime factor, we can use an iterative process to compute the largest
# prime. To do this, we will divide n by its smallest prime factor to get a smaller number.
# Notice this smaller number will have one less prime factor than n. In particular, it is
# one less of the smallest prime factors of n. Essentially, we iterate this procedure to 
# remove every prime factor smaller than the largest one.

largestprimefactor <- function(n){
  
  prime <- n
  smallestprime <- smallestprimefactor(n)
  
  while(prime != smallestprime){
    prime <- prime/smallestprime
    smallestprime <- smallestprimefactor(prime)
  }
  prime
}

# Exercise 4 (ANSWER: )
# Find the largest palindromic number that is the product of n digit numbers
# (In this problem: n = 3)

# This function checks if n is a palindrome

# WARNING ERROR OCCURS WITH THIS FUNCTION
numbertovec <- function(n){
  
  # Returns the number of digits of n
  digits <- (as.integer(log10(n)) + 1)
  
  number <- n
  vec <- c()
  i <- 1
  
  while (number > 0){
    vec[i] <- as.integer(number/(10^(digits - 1)))
    
    # This next bit of code checks zeros we may potentially miss
    number <- number - vec[i]*10^(digits - 1)
    i <- i + 1
    digits <- (as.integer(log10(number)) + 1)
  }
  vec
}

ispalindrome <- function(n){
  
  nvec <- numbertovec(n)
  l <- length(nvec)
  m <- as.integer(l/2)
  
  if(m == 0){
    return(TRUE)
  }
  else{
  
    for (i in 1:m) {
    
      if (nvec[i] != nvec[l - i + 1]){
        return(FALSE)
        break
      }
    }
    
    return(TRUE)
  }
}

largestpalindromeproduct <- function(n){
  
  s <- 0
  
  for (i in 10:99){
    
    for (j in 10:99){
      
      if ((i*j> s) & ispalindrome(i*j)){
        s <- i*j
      }
    }
  }
  return(s)
}















