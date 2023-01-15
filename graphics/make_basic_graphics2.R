require(rstan)

all.rates <- c()
contrast <- c()
model.type <- c()

load('../output/basic_morph_flat.Rdata')

rates <- extract(fit)$beta
N <- nrow(rates)

#death rate | IC vs noIC

all.rates <- c(all.rates,rates[,4] - rates[,1])
contrast <- c(contrast,rep('death rate, $+$IC vs.\\ $-$IC',N))
model.type <- c(model.type,rep('morph, flat',N))

# noIC > IC | basic vs nonbasic

all.rates <- c(all.rates,rates[,8] - rates[,2])
contrast <- c(contrast,rep('IC gain, basic vs.\\ nonbasic',N))
model.type <- c(model.type,rep('morph, flat',N))

# IC > noIC | basic vs nonbasic

all.rates <- c(all.rates,rates[,10] - rates[,5])
contrast <- c(contrast,rep('IC loss, basic vs.\\ nonbasic',N))
model.type <- c(model.type,rep('morph, flat',N))

# basic > nonbasic | IC vs noIC

all.rates <- c(all.rates,rates[,9] - rates[,7])
contrast <- c(contrast,rep('basic $\\rightarrow$ nonbasic, $+$IC vs.\\ $-$IC',N))
model.type <- c(model.type,rep('morph, flat',N))

# nonbasic > basic | IC vs noIC

all.rates <- c(all.rates,rates[,6] - rates[,3])
contrast <- c(contrast,rep('nonbasic $\\rightarrow$ basic, $+$IC vs.\\ $-$IC',N))
model.type <- c(model.type,rep('morph, flat',N))

load('../output/basic_morph_hier.Rdata')

rates <- extract(fit)$beta
N <- nrow(rates)

#death rate | IC vs noIC

all.rates <- c(all.rates,rates[,4] - rates[,1])
contrast <- c(contrast,rep('death rate, $+$IC vs.\\ $-$IC',N))
model.type <- c(model.type,rep('morph, hier',N))

# noIC > IC | basic vs nonbasic

all.rates <- c(all.rates,rates[,8] - rates[,2])
contrast <- c(contrast,rep('IC gain, basic vs.\\ nonbasic',N))
model.type <- c(model.type,rep('morph, hier',N))

# IC > noIC | basic vs nonbasic

all.rates <- c(all.rates,rates[,10] - rates[,5])
contrast <- c(contrast,rep('IC loss, basic vs.\\ nonbasic',N))
model.type <- c(model.type,rep('morph, hier',N))

# basic > nonbasic | IC vs noIC

all.rates <- c(all.rates,rates[,9] - rates[,7])
contrast <- c(contrast,rep('basic $\\rightarrow$ nonbasic, $+$IC vs.\\ $-$IC',N))
model.type <- c(model.type,rep('morph, hier',N))

# nonbasic > basic | IC vs noIC

all.rates <- c(all.rates,rates[,6] - rates[,3])
contrast <- c(contrast,rep('nonbasic $\\rightarrow$ basic, $+$IC vs.\\ $-$IC',N))
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

s = 7

tikz('basic-posterior-rates.tex',width=s*2,height=s*2)
ggplot(rate.df, aes(x = rate, y = 0, fill = stat(quantile))) + 
  geom_density_ridges_gradient(quantile_lines = TRUE, quantile_fun = hdi, vline_linetype = 2) + 
  geom_vline(xintercept = 0,linetype="dashed") + 
  #facet_grid(vars(model.type),vars(contrast),scales = "free_x") + 
  #facet_grid(vars(),vars(contrast)) + 
  facet_grid2(vars(contrast),vars(model.type), scales="free", independent=TRUE) + 
  scale_fill_manual(values = c("transparent", "lightblue", "transparent"), guide = "none") + ylab('Posterior density') + xlab('')

dev.off()