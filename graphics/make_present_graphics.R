require(rstan)

all.rates <- c()
contrast <- c()
model.type <- c()

load('../output/present_word_flat.Rdata')

rates <- extract(fit)$beta
N <- nrow(rates)

all.rates <- c(all.rates,rates[,3]-rates[,1])
contrast <- c(contrast,rep('death rate, $+$IC vs.\\ $-$IC',N))
model.type <- c(model.type,rep('word, flat',N))

all.rates <- c(all.rates,rates[,2]-rates[,4])
#contrast <- c(contrast,rep('$-$IC $\\rightarrow$ $+$IC vs.\ $+$IC $\\rightarrow$ $-$IC vs.',N))
contrast <- c(contrast,rep('IC gain vs.\\ loss rate',N))
model.type <- c(model.type,rep('word, flat',N))

load('../output/present_word_hier.Rdata')

rates <- extract(fit)$beta
N <- nrow(rates)

all.rates <- c(all.rates,rates[,3]-rates[,1])
contrast <- c(contrast,rep('death rate, $+$IC vs.\\ $-$IC',N))
model.type <- c(model.type,rep('word, hier',N))

all.rates <- c(all.rates,rates[,2]-rates[,4])
#contrast <- c(contrast,rep('$-$IC $\\rightarrow$ $+$IC vs. $+$IC $\\rightarrow$ $-$IC vs.',N))
contrast <- c(contrast,rep('IC gain vs.\\ loss rate',N))
model.type <- c(model.type,rep('word, hier',N))

load('../output/present_morph_flat.Rdata')

rates <- extract(fit)$beta
N <- nrow(rates)

all.rates <- c(all.rates,rates[,3]-rates[,1])
contrast <- c(contrast,rep('death rate, $+$IC vs.\\ $-$IC',N))
model.type <- c(model.type,rep('morph, flat',N))

all.rates <- c(all.rates,rates[,2]-rates[,4])
#contrast <- c(contrast,rep('$-$IC $\\rightarrow$ $+$IC vs. $+$IC $\\rightarrow$ $-$IC vs.',N))
contrast <- c(contrast,rep('IC gain vs.\\ loss rate',N))
model.type <- c(model.type,rep('morph, flat',N))

load('../output/present_morph_hier.Rdata')

rates <- extract(fit)$beta
N <- nrow(rates)

all.rates <- c(all.rates,rates[,3]-rates[,1])
contrast <- c(contrast,rep('death rate, $+$IC vs.\\ $-$IC',N))
model.type <- c(model.type,rep('morph, hier',N))

all.rates <- c(all.rates,rates[,2]-rates[,4])
#contrast <- c(contrast,rep('$-$IC $\\rightarrow$ $+$IC vs. $+$IC $\\rightarrow$ $-$IC vs.',N))
contrast <- c(contrast,rep('IC gain vs.\\ loss rate',N))
model.type <- c(model.type,rep('morph, hier',N))

rate.df <- data.frame(rate=all.rates,contrast=contrast,model.type=model.type)

library(tikzDevice)
library(HDInterval)
require(ggplot2)
library(ggridges)
require(ggh4x)

#ggplot(rate.df, aes(x = rate, y = 0, fill = stat(quantile))) + 
#  geom_density_ridges_gradient(quantile_lines = TRUE, quantile_fun = hdi, vline_linetype = 2) + 
#  #facet_grid(vars(model.type),vars(contrast),scales = "free_x") + 
#  #facet_grid(vars(model.type),vars(contrast)) + 
#  facet_wrap( ~ model.type + contrast, scales="free") + 
#  scale_fill_manual(values = c("transparent", "lightblue", "transparent"), guide = "none") + ylab('Posterior density') + xlab('')

tikz('present-posterior-rates.tex',width=10,height=5)
ggplot(rate.df, aes(x = rate, y = 0, fill = stat(quantile))) + 
  geom_density_ridges_gradient(quantile_lines = TRUE, quantile_fun = hdi, vline_linetype = 2) + 
  geom_vline(xintercept = 0,linetype="dashed") + 
  #facet_grid(vars(model.type),vars(contrast),scales = "free_x") + 
  #facet_grid(vars(),vars(contrast)) + 
  facet_grid2(vars(contrast),vars(model.type), scales="free", independent=TRUE) + 
  scale_fill_manual(values = c("transparent", "lightblue", "transparent"), guide = "none") + ylab('Posterior density') + xlab('')

dev.off()