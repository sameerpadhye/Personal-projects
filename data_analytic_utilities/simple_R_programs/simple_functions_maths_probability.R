# Few functions related to simple arithmetic and probability

odd_even<-function (x){
    
    if(x%%2==0){
        print(paste(x,"is an even number"))
    } else {
        print(paste(x,"is an odd number"))
    }
}

tables<-function(x){
    
    for (i in 1:10){
        
        print(i * x)
        
    }
    
}

tables(2)

die_cast<-function(){
    
    die_1_output<-sample(x=1:6,
                         size=1,
                         replace=T)
    
    die_2_output<-sample(x=1:6,
                         size=1,
                         replace=T)
    
    print(paste("The first die rolled",
                die_1_output))
    print(paste("The second die rolled",
                die_2_output))
}

die_cast()


# Factorial of a number (greater than 0)

factorial_number<-function (x) {
    factorial = 1
    for(i in 1:x) {
        factorial = factorial * i
    }
    print(paste("The factorial of",x ,"is",factorial))
}    

factorial_number(9)


## Generating a random sample from a binomial distribution

vec=rbinom(100000,  # number of samples to be drawn
           9, # number of trials per observation
           0.10) # probability of success (per observation)
