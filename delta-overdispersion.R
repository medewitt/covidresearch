# Delta Overdispersion

R0 <- seq( 4, 9, .1)

k <- .2

rnbinom(100, size = k, mu = 9)

epi_param <- function(R0, k = .2){
    data.frame(
        R0 = R0,
        hit = 1-1/R0,
        prop_i = 1-exp(-R0),
        q50 = qnbinom(.5,  size = k,  mu = R0),
        q80 = qnbinom(.8,  size = k,  mu = R0)
    )
}

out <- do.call(rbind, lapply(seq(1,9,.1), epi_param))

plot(prop_i ~ R0, ylim = c(0,1),data = out, type = "l")
with(out, lines(R0, hit, lty = 2))

library(ggplot2)
library(ggsci)
library(dplyr)

hit <- function(R0){
    1- 1/R0
}

prop_i <- function(R0){
    1-exp(-R0)
}

out  %>%
dplyr::select(R0, hit, prop_i) %>% 
  tidyr::gather(metric, value, - R0) %>% 
  ggplot()+
  geom_line(aes(R0,value, color = metric))+
  theme_bw()+
  scale_color_manual(labels = c("Critical Proportion (%)",
  "Final Infection Rate (%)"),
                      values = c(pal_lancet()(3)[2], pal_lancet()(3)[1])) +
 theme(legend.position = "top") +
 geom_ribbon(data = subset(out, R0 > 4), 
             aes(ymin = hit, ymax = prop_i, x = R0), 
             fill = "grey40", alpha = .3)+
             labs(
                 title = "Infectious Disease Endpoints",
                 subtitle = "Given Basic Reproduction Number",
                 x = "R0",
                 y = "Proportion of Susceptible Population",
                 color = NULL
             )+
             scale_y_continuous(limits = c(0,1), expand = c(0,0))