abvd.languages <- read.csv('../acd_abvd_data/abvd_languages.csv')
abvd.cognates <- read.csv('../acd_abvd_data/abvd_cognates.csv')
abvd.forms <- read.csv('../acd_abvd_data/abvd_forms.csv')
abvd.parameters <- read.csv('../acd_abvd_data/abvd_parameters.csv')

abvd.merged <- merge(abvd.forms,abvd.parameters,by.x='Parameter_ID',by.y='ID')
abvd.merged <- merge(abvd.merged,abvd.languages,by.x='Language_ID',by.y='ID')

abvd.merged <- abvd.merged[,c('Glottocode','Name.x','Form')]

write.table(file='abvd.csv',abvd.merged,quote = F,row.names = F,sep='\t')