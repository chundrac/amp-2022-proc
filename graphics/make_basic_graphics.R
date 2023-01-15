require(rstan)

all.rates <- c()
contrast <- c()
model.type <- c()

load('../output/basic_word_flat.Rdata')

rates <- extract(fit)$beta
N <- nrow(rates)

#death rate | IDCC vs noIDCC

all.rates <- c(all.rates,rates[,4] - rates[,1])
contrast <- c(contrast,rep('death rate, $+$IDCC vs.\\ $-$IDCC',N))
model.type <- c(model.type,rep('word, flat',N))

# noIDCC > IDCC | basic vs nonbasic

all.rates <- c(all.rates,rates[,8] - rates[,2])
contrast <- c(contrast,rep('IDCC gain, basic vs.\\ nonbasic',N))
model.type <- c(model.type,rep('word, flat',N))

# IDCC > noIDCC | basic vs nonbasic

all.rates <- c(all.rates,rates[,10] - rates[,5])
contrast <- c(contrast,rep('IDCC loss, basic vs.\\ nonbasic',N))
model.type <- c(model.type,rep('word, flat',N))

# basic > nonbasic | IDCC vs noIDCC

all.rates <- c(all.rates,rates[,9] - rates[,7])
contrast <- c(contrast,rep('basic $\\rightarrow$ nonbasic, $+$IDCC vs.\\ $-$IDCC',N))
model.type <- c(model.type,rep('word, flat',N))

# nonbasic > basic | IDCC vs noIDCC

all.rates <- c(all.rates,rates[,6] - rates[,3])
contrast <- c(contrast,rep('nonbasic $\\rightarrow$ basic, $+$IDCC vs.\\ $-$IDCC',N))
model.type <- c(model.type,rep('word, flat',N))

load('../output/basic_word_hier.Rdata')

rates <- extract(fit)$beta
N <- nrow(rates)

#death rate | IDCC vs noIDCC

all.rates <- c(all.rates,rates[,4] - rates[,1])
contrast <- c(contrast,rep('death rate, $+$IDCC vs.\\ $-$IDCC',N))
model.type <- c(model.type,rep('word, hier',N))

# noIDCC > IDCC | basic vs nonbasic

all.rates <- c(all.rates,rates[,8] - rates[,2])
contrast <- c(contrast,rep('IDCC gain, basic vs.\\ nonbasic',N))
model.type <- c(model.type,rep('word, hier',N))

# IDCC > noIDCC | basic vs nonbasic

all.rates <- c(all.rates,rates[,10] - rates[,5])
contrast <- c(contrast,rep('IDCC loss, basic vs.\\ nonbasic',N))
model.type <- c(model.type,rep('word, hier',N))

# basic > nonbasic | IDCC vs noIDCC

all.rates <- c(all.rates,rates[,9] - rates[,7])
contrast <- c(contrast,rep('basic $\\rightarrow$ nonbasic, $+$IDCC vs.\\ $-$IDCC',N))
model.type <- c(model.type,rep('word, hier',N))

# nonbasic > basic | IDCC vs noIDCC

all.rates <- c(all.rates,rates[,6] - rates[,3])
contrast <- c(contrast,rep('nonbasic $\\rightarrow$ basic, $+$IDCC vs.\\ $-$IDCC',N))
model.type <- c(model.type,rep('word, hier',N))

load('../output/basic_morph_flat.Rdata')

rates <- extract(fit)$beta
N <- nrow(rates)

#death rate | IDCC vs noIDCC

all.rates <- c(all.rates,rates[,4] - rates[,1])
contrast <- c(contrast,rep('death rate, $+$IDCC vs.\\ $-$IDCC',N))
model.type <- c(model.type,rep('morph, flat',N))

# noIDCC > IDCC | basic vs nonbasic

all.rates <- c(all.rates,rates[,8] - rates[,2])
contrast <- c(contrast,rep('IDCC gain, basic vs.\\ nonbasic',N))
model.type <- c(model.type,rep('morph, flat',N))

# IDCC > noIDCC | basic vs nonbasic

all.rates <- c(all.rates,rates[,10] - rates[,5])
contrast <- c(contrast,rep('IDCC loss, basic vs.\\ nonbasic',N))
model.type <- c(model.type,rep('morph, flat',N))

# basic > nonbasic | IDCC vs noIDCC

all.rates <- c(all.rates,rates[,9] - rates[,7])
contrast <- c(contrast,rep('basic $\\rightarrow$ nonbasic, $+$IDCC vs.\\ $-$IDCC',N))
model.type <- c(model.type,rep('morph, flat',N))

# nonbasic > basic | IDCC vs noIDCC

all.rates <- c(all.rates,rates[,6] - rates[,3])
contrast <- c(contrast,rep('nonbasic $\\rightarrow$ basic, $+$IDCC vs.\\ $-$IDCC',N))
model.type <- c(model.type,rep('morph, flat',N))

load('../output/basic_morph_hier.Rdata')

rates <- extract(fit)$beta
N <- nrow(rates)

#death rate | IDCC vs noIDCC

all.rates <- c(all.rates,rates[,4] - rates[,1])
contrast <- c(contrast,rep('death rate, $+$IDCC vs.\\ $-$IDCC',N))
model.type <- c(model.type,rep('morph, hier',N))

# noIDCC > IDCC | basic vs nonbasic

all.rates <- c(all.rates,rates[,8] - rates[,2])
contrast <- c(contrast,rep('IDCC gain, basic vs.\\ nonbasic',N))
model.type <- c(model.type,rep('morph, hier',N))

# IDCC > noIDCC | basic vs nonbasic

all.rates <- c(all.rates,rates[,10] - rates[,5])
contrast <- c(contrast,rep('IDCC loss, basic vs.\\ nonbasic',N))
model.type <- c(model.type,rep('morph, hier',N))

# basic > nonbasic | IDCC vs noIDCC

all.rates <- c(all.rates,rates[,9] - rates[,7])
contrast <- c(contrast,rep('basic $\\rightarrow$ nonbasic, $+$IDCC vs.\\ $-$IDCC',N))
model.type <- c(model.type,rep('morph, hier',N))

# nonbasic > basic | IDCC vs noIDCC

all.rates <- c(all.rates,rates[,6] - rates[,3])
contrast <- c(contrast,rep('nonbasic $\\rightarrow$ basic, $+$IDCC vs.\\ $-$IDCC',N))
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