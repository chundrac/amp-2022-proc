from collections import defaultdict
import re

acd_data = [l.strip('\n').split('\t') for l in open('acd_merged_w_IDCC.tsv','r')]

acd_data = [acd_data[0]]+[l for l in acd_data if l[2] != '']

acd_data = [tuple(l[1:]) for l in acd_data]

acd_data = set(acd_data)

acd_data = [list(l) for l in acd_data]

id_cc_word = defaultdict(list)

id_cc_morph = defaultdict(list)

for l in acd_data[1:]:
    id_cc_word[l[0]].append(l[11])
    id_cc_morph[l[0]].append(l[12])

cogs_to_keep = [k for k in id_cc_word.keys() if (len(set(id_cc_word[k]))==2 or len(set(id_cc_morph[k]))==2) and not k.startswith('Noise') and not k.startswith('Near')]

acd_data = [l for l in acd_data if l[0] in cogs_to_keep]

basic_meanings_normalized = """above
all
and
ash
at
back
bad, evil
belly
below
big
bird
black
blood
bone
branch
breast
child
cloud
cold
correct, true
day
dirty
dog
dry
dull, blunt
dust
ear
earth/soil
egg
Eight
eye
far
fat/grease
father
feather
Fifty
fire
fish
Five
flower
fog
Four
fruit
good
grass
green
hair
hand
he/she
head
heavy
house
how?
husband
I
if
in, inside
intestines
lake
leaf
left
leg/foot
lightning
liver
long
louse
man/male
meat/flesh
moon
mosquito
mother
mouth
name
narrow
near
neck
needle
new
night
Nine
no, not
nose
old
One
One Hundred
One Thousand
other
painful, sick
person/human being
rain
rat
red
right
road/path
root
rope
rotten
salt
sand
sea
Seven
sharp
short
shoulder
shy, ashamed
Six
skin
sky
small
smoke
snake
spider
star
stick/wood
stone
tail
Ten
that
thatch/roof
they
thick
thin
this
thou
Three
thunder
to bite
to blow
to breathe
to burn
to buy
to chew
to choose
to climb
to come
to cook
to count
to cry
to cut, hack
to die, be dead
to dig
to dream
to drink
to eat
to fall
to fear
to flow
to fly
to grow
to hear
to hide
to hit
to hold
to hunt
to kill
to know, be knowledgeable
to laugh
to lie down
to live, be alive
to open, uncover
to plant
to pound, beat
to say
to scratch
to see
to sew
to shoot
to sit
to sleep
to sniff, smell
to spit
to split
to squeeze
to stab, pierce
to stand
to steal
to suck
to swell
to swim
to think
to throw
to tie up, fasten
to turn
to vomit
to walk
to work
to yawn
tongue
tooth
Twenty
Two
warm
water
we
wet
what?
when?
where?
white
who?
wide
wife
wind
wing
woman/female
woods/forest
worm (earthworm)
year
yellow
you""".split('\n')

basic_meanings_normalized = [s for w in basic_meanings_normalized for s in re.split(r'\s*\W\s*',w.lower().replace('to ','').strip('?')) if s != '']

basic_meanings_normalized[basic_meanings_normalized.index('i')] = 'I'

basic_meanings_normalized += ['one hundred','one thousand','female breast','right side']

abvd_data = [l.strip('\n').split('\t') for l in open('abvd.csv','r')]

abvd_lang_gloss = [[l[0],l[1]] for l in abvd_data]

abvd_lang_form = [[l[0],l[2]] for l in abvd_data]

abvd_gloss_form = [[l[1],l[2]] for l in abvd_data]

for i,l in enumerate(acd_data):
    print(i/len(acd_data))
    acd_data[i].append('nonbasic')
    if [l[1],l[3],l[2]] in abvd_data:
        acd_data[i][-1] = 'basic'
    elif [l[1],l[3]] in abvd_lang_gloss:
        acd_data[i][-1] = 'basic'
    elif [l[3],l[2]] in abvd_gloss_form:
        acd_data[i][-1] = 'basic'
    #elif [l[1],l[2]] in abvd_lang_form:
    #    acd_data[i][-1] = 'basic?'
    else:
        counter = 0
        gloss = re.sub(r'\ \([^\)]*\)','',l[3]).strip('.?!')
        #if ';' in gloss:
        #    gloss = gloss.split('; ')
        #elif ',' in gloss:
        #    gloss = gloss.split(', ')
        #else:
        #    gloss = [gloss]
        gloss = re.split(r'[;,]\s*',gloss)
        for g in gloss:
            g_ = g.replace('to ','')
            if g_ in basic_meanings_normalized:
                counter += 1
        if counter > 0:
            acd_data[i][-1] = 'basic'

for i,l in enumerate(acd_data):
    if re.match('[1|2|3]\s*(pl)|(sg)|(du)(\.)',l[3]):
        acd_data[i][-1] = 'basic'

acd_data[0][-1] = 'basic/nonbasic'

f = open('acd_merged_w_IDCC_basic.tsv','w')
for l in acd_data:
    print('\t'.join(l),file=f)

f.close()
