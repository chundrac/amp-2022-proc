cd process_data

Rscript process-acd.R
Rscript process-abvd.R

python3 align_forms.py
python3 gen_ident_cons.py
python3 gen_basic_nonbasic.py

python3 generate_basic_morph_character_data.py
python3 generate_basic_word_character_data.py
python3 generate_present_morph_character_data.py
python3 generate_present_word_character_data.py
