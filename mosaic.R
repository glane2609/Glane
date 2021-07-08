library(ggmosaic)
library(ggplot2)
library(dplyr)
library(ggrepel)
data <- read.csv("C:/Users/glane/Downloads/corpora data.csv")
data <- mutate_if(data,is.character,as.factor)

p <- ggplot(data) + 
  geom_mosaic(aes(weight = percentage, 
                  x = product(class, corpora),fill=class))+labs(x="X" , y = "Morphophonological Information")

df2 <- ggplot_build(p)$data[[1]]

df2$pr <- round(100*df2$.wt/sum(df2$.wt), 2)

df2$lab <- paste0(df2$.wt, "%")

df2

p + geom_label_repel(data = df2, 
               aes(x = (xmin+xmax)/2, y = (ymin+ymax)/2, label = lab),nudge_x = 0.05)+guides(fill=guide_legend(title="Morphophonological Information"))

