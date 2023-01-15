from collections import defaultdict
import numpy as np

taxa = """bunu1267
puyu1239
amis1246
sira1267
paiw1248
sasa1249
bali1279
reja1240
lamp1243
kome1238
bata1289
gayo1244
nucl1460
sund1252
mala1479
indo1316
mina1268
keri1250
cent2053
brun1242
iban1264
banj1239
java1254
emba1238
moke1242
tsat1238
chru1239
east2563
sang1337
sang1336
bant1286
tont1239
maka1311
bugi1244
tora1261
bang1368
woli1241
muna1247
tuka1249
bone1254
mori1268
pamo1252
pala1344
cham1312
nias1242
cent2074
ngad1261
lioo1240
mang1405
kodi1247
lamb1273
weje1237
wanu1241
anak1240
mamb1305
kamb1299
sabu1255
bima1247
sika1262
lama1277
keda1252
biak1248
waro1242
amba1265
wand1267
morm1235
gebe1237
biga1238
asss1237
buli1255
gane1237
seim1238
wuvu1239
kheh1237
liku1243
loni1238
leip1237
louu1245
naun1237
zaza1245
chek1238
koko1269
zaba1237
vari1239
vagh1249
riri1237
baba1268
hoav1238
maro1244
rovi1238
simb1256
teop1238
sapo1253
neha1247
bann1247
siar1238
kuan1248
mini1251
patp1243
tiga1245
tung1290
kara1486
nali1244
tian1237
mand1440
muss1246
gela1263
bugh1239
ghar1239
tali1259
laua1243
kwai1243
toab1237
kwar1239
saaa1240
owaa1237
bauu1243
rotu1241
renn1242
anut1237
samo1305
tong1325
west2516
niue1239
nort2845
mang1401
rapa1244
tuam1242
raro1241
hawa1245
maor1246
tahi1242
aust1304
penr1237
tuva1244
toke1240
nuku1260
kapi1249
taku1257
sika1261
onto1237
tiko1237
emae1237
mele1250
east2447
futu1245
pile1238
urav1235
siee1239
sout2869
lena1238
kwam1252
anei1239
mwat1237
hano1246
mota1237
motl1237
mere1242
arak1252
saka1289
nese1235
sout2857
nati1244
vinm1237
litz1237
mara1399
katb1237
orko1234
paam1238
sout2859
nort2836
sout2856
nama1268
iaai1238
dehu1237
neng1238
xara1244
jawe1237
kuma1276
gilb1244
moki1238
pohn1238
rali1241
rata1243
chuu1238
caro1242
nucl1479
sata1237
mort1237
pulu1242
lamo1243
sons1242
kosr1238
mudu1242
ayiw1239
malo1246
natu1246
nang1262
vano1237
tean1237
tane1237
tani1255
asum1237
amba1266
bilb1241
matu1261
taki1248
geda1237
male1289
kove1237
numb1247
seng1281
mouk1239
naka1262
mana1295
woge1237
kair1263
meke1243
mago1248
motu1246
kili1267
ubir1237
gapa1238
diod1237
bwai1242
moli1248
dobu1241
suau1242
sali1295
west2538
ujir1237
keii1239
paul1238
ambo1250
alun1238
bobo1254
masi1266
watu1247
gese1240
irar1238
kowi1239
seka1247
yamd1240
sela1259
uabm1237
term1237
kuni1263
cent2072
sout2883
east2462
seri1255
empl1237
imro1237
tela1241
dawe1237
daii1240
nort2860
nila1244
seru1245
teun1241
luan1263
leti1246
galo1243
pera1257
aput1237
tugu1245
iliu1237
mamb1306
kema1243
tetu1246
buru1303
tali1262
west2563
moda1244
reja1241
buka1261
laha1253
main1275
kela1258
bela1260
west2564
bint1246
cent2101
timu1262
idaa1241
beka1241
baub1235
meri1243
tunj1244
maan1238
ngaj1237
inab1237
sout2918
yaka1277
cent2092
mapu1244
mara1404
cent2089
west2557
binu1244
ilia1236
west2555
atam1240
mati1250
diba1242
cota1241
sara1327
tbol1240
sara1326
koro1310
taga1270
cent2087
taus1251
kala1388
mans1262
mama1275
cebu1242
suri1273
akla1241
tagb1258
bata1301
cala1258
hanu1241
boto1242
pamp1243
pang1290
kaya1320
ibal1244
ilon1239
kele1259
bata1298
amga1235
bont1247
nort2877
bala1310
lubu1243
bino1237
ilok1237
casi1235
isna1241
pamp1244
iban1267
cent2084
gadd1244
ivat1242
yami1254
ibat1238
goro1259
kaid1239
mong1342
basa1287
kava1241
taro1264
atay1247
sais1237
kulo1237
thao1240
babu1240
tsou1248
kana1286
saar1237
ruka1240
oldc1244
buya1244""".split('\n')

acd_data = [l.strip('\n').split('\t') for l in open('acd_merged_w_IDCC_basic.tsv','r')]

lang_count = defaultdict(int)

for l in acd_data:
    lang_count[l[1]] += 1

langs_to_keep = [k for k in lang_count.keys() if lang_count[k] >= 100 and k in taxa]

acd_data = [l for l in acd_data if l[1] in langs_to_keep]

cogvar = defaultdict(list)

for l in acd_data:
    cogvar[l[6]].append(l[11])

for k in cogvar.keys():
    cogvar[k] = sorted(set(cogvar[k]))

#at the word level
coglangs = defaultdict(list)
for l in acd_data:
    coglangs[l[6]].append(l[1])

cogcount = defaultdict(int)
for k in coglangs.keys():
    cogcount[k] = len(sorted(set(coglangs[k])))

L = len(langs_to_keep)

cogs_to_keep = [k for k in cogvar.keys() if len(cogvar[k])==2 and cogcount[k] > L/5]

acd_data = [l for l in acd_data if l[6] in cogs_to_keep]

cog_values = {}
for l in acd_data:
    if l[6] not in cog_values.keys():
        cog_values[l[6]] = defaultdict(list)
    cog_values[l[6]][l[1]].append(l[11])

print(L,len(cogs_to_keep))

colnames = ['']
charvalues = []

for cog in cogs_to_keep:
    names = ['absent','0','1']
    vals = np.zeros([L,len(names)])
    for lang in langs_to_keep:
        if lang in cog_values[cog].keys():
            for v in cog_values[cog][lang]:
                vals[langs_to_keep.index(lang),names.index(v)] = 1
        else:
            vals[langs_to_keep.index(lang),names.index('absent')] = 1
    colnames += [cog+':'+n for n in names]
    charvalues.append(vals)

charvalues = np.concatenate(charvalues,-1)

f = open('../data/present_word_character_data.tsv','w')
print('\t'.join(colnames),file=f)
for i,lang in enumerate(langs_to_keep):
    print('\t'.join([lang]+[str(s) for s in list(charvalues[i,])]),file=f)

f.close()