#downloaded sept 13 2022

mkdir acd_abvd_data
mkdir output

cd acd_abvd_data

curl https://raw.githubusercontent.com/lexibank/acd/main/cldf/cognates.csv > acd_cognates.csv
curl https://raw.githubusercontent.com/lexibank/acd/main/cldf/cognates.csv > acd_cognates.csv
curl https://raw.githubusercontent.com/lexibank/acd/main/cldf/cognatesets.csv > acd_cognatesets.csv
curl https://raw.githubusercontent.com/lexibank/acd/main/cldf/contributions.csv > acd_contributions.csv
curl https://raw.githubusercontent.com/lexibank/acd/main/cldf/forms.csv > acd_forms.csv
curl https://raw.githubusercontent.com/lexibank/acd/main/cldf/languages.csv > acd_languages.csv
curl https://raw.githubusercontent.com/lexibank/acd/main/cldf/loansets.csv > acd_loansets.csv
curl https://raw.githubusercontent.com/lexibank/acd/main/cldf/merged.csv > acd_merged.csv
curl https://raw.githubusercontent.com/lexibank/acd/main/cldf/parameters.csv > acd_parameters.csv
curl https://raw.githubusercontent.com/lexibank/acd/main/cldf/protoforms.csv > acd_protoforms.csv

curl https://raw.githubusercontent.com/lexibank/abvd/master/cldf/cognates.csv > abvd_cognates.csv
curl https://raw.githubusercontent.com/lexibank/abvd/master/cldf/forms.csv > abvd_forms.csv
curl https://raw.githubusercontent.com/lexibank/abvd/master/cldf/languages.csv > abvd_languages.csv
curl https://raw.githubusercontent.com/lexibank/abvd/master/cldf/parameters.csv > abvd_parameters.csv
