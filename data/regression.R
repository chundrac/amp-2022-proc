require(reshape2)
require(ggmosaic)
require(ggplot2)
require(tikzDevice)

data.df <- read.csv('basic_morph_character_data.tsv',row.names=1,sep='\t')

long.df <- melt(as.matrix(data.df))

long.df <- long.df[long.df$value == 1,1:2]

feats <- do.call(rbind,strsplit(as.character(long.df$Var2),'\\.'))

IC <- sapply(feats[,2],function(x){substr(x,nchar(x),nchar(x))})

basic <- sapply(feats[,2],function(x){substr(x,1,1)})

new.df <- data.frame(lang=long.df$Var1,etymon=feats[,1],IC=IC,basic=basic)

new.df <- new.df[new.df$IC != 't',]

new.df <- droplevels(new.df)

IC <- ifelse(new.df$IC == 1, '$+$IC', '$-$IC')

vocab.type <- ifelse(new.df$basic == 'T', 'basic', 'non-basic')

model <- glmer(IC ~ basic + (1|lang) + (1|etymon), new.df, family='binomial')

graph.df <- data.frame(IC=IC,vocab.type=vocab.type)

tikz('basic-morph.tex',width=4,height=2)
ggplot(data=graph.df) + geom_mosaic(aes(x=product(vocab.type),fill=IC))+theme(plot.title = element_text(hjust = 0.5))+labs(x='vocabulary type',y='',title='{\\sc basic-morph}')
dev.off()

data.df <- read.csv('basic_word_character_data.tsv',row.names=1,sep='\t')

long.df <- melt(as.matrix(data.df))

long.df <- long.df[long.df$value == 1,1:2]

feats <- do.call(rbind,strsplit(as.character(long.df$Var2),'\\.'))

IC <- sapply(feats[,2],function(x){substr(x,nchar(x),nchar(x))})

basic <- sapply(feats[,2],function(x){substr(x,1,1)})

new.df <- data.frame(lang=long.df$Var1,etymon=feats[,1],IC=IC,basic=basic)

new.df <- new.df[new.df$IC != 't',]

new.df <- droplevels(new.df)

IC <- ifelse(new.df$IC == 1, '$+$IC', '$-$IC')

vocab.type <- ifelse(new.df$basic == 'T', 'basic', 'non-basic')

model <- glmer(IC ~ basic + (1|lang) + (1|etymon), new.df, family='binomial')

graph.df <- data.frame(IC=IC,vocab.type=vocab.type)

tikz('basic-word.tex',width=4,height=2)
ggplot(data=graph.df) + geom_mosaic(aes(x=product(vocab.type),fill=IC))+theme(plot.title = element_text(hjust = 0.5))+labs(x='vocabulary type',y='',title='{\\sc basic-word}')
dev.off()