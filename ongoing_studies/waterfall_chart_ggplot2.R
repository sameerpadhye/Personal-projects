# Waterfall chart in ggplot


# Libraries

require(tidyverse)
library(plm)
# Data for the plot

data<-data.frame(sample_id=seq(1:12),
                 sample=sprintf("sample%d",
               seq=(1:12)),values=c(50,-22,66,120,150,-98,-45,37,88,-7,56,94))

data<-data%>%
    mutate(cum_sum=cumsum(values),
           lag_val=dplyr::lag(cumsum(values),
                              default = 0))

ifelse()     
View(data)
