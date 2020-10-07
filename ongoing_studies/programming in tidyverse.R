#
library(tidyverse)
library(magrittr)

df <- tibble(
    var_1 = sample(100,20,replace=T),
    var_2 = sample(100,20,replace=T),
    var_3 = runif(20),
    fact_1 = rep(c("X","Y"),each=10),
    fact_2=rep(c("A","B","C","D"),each=5)
)
df


df%>%
    group_by(fact_1)%>%
    summarise(mean_1=mean(var_1))

df%>%
    group_by(fact_2)%>%
    summarise(mean_1=mean(var_1))

desc_stats_func<-function(x,factor){
    factor=enquo(factor)
    x%>%
        group_by(!!factor)%>%
        summarise(mean_1=mean(var_1,na.rm=T),
                  sd_1=sd(var_1,na.rm=T))
}

desc_stats_func(df,fact_1)    


var_4=quo(df$var_1)

summarize(df,mean=mean(!!var_4),sum=sum(!!var_4),n=n())


summary_function<-function(x,var){
    
    var<-enquo(var)
    
    summarize(x,
              mean=mean(!!var,na.rm=T),
              median=median(!!var,na.rm=T),
              stddev=sd(!!var,na.rm=T))
}

summary_function(df,var_3)
summary_function(df,var_1*var_2)


my_mutate <- function(df, expr) {
    expr <- enquo(expr)
    mean_name <- paste0("mean_", quo_name(expr))
    sum_name <- paste0("sum_", quo_name(expr))
    
    mutate(df,
           !! mean_name := mean(!! expr),
           !! sum_name := sum(!! expr)
    )
}

my_mutate(df,var_1)

paste0("mean_",quo_name(x))


x<-c(1:10)
args <- list(na.rm = TRUE, trim = 0.25)
quo(mean(x, !!! args))

function_summary<-function(x,...){
        agruments<-enquos(...)
        
        x%>%
            group_by(!!!agruments)%>%
            summarise(mean_var1=mean(var_1,na.rm=T),
                      mean_var2=mean(var_2,na.rm=TRUE))
    }

function_summary(df,fact_1,fact_2)
str(df)
