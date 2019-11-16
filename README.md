# Tokemon
## main.pl

### dynamic - status(X)
X adalah status dari permainan sekarang.
Status yang ada yaitu idle, roam, dan battle.

### command(X)
X adalah aksi yang dapat dimasukkan oleh pengguna

### processInput(X)
Menjalankan aksi X

### start
Memulai permainan game, dan melakukan inisiasi dasar mekanisme permainan

### quit
Keluar dari permainan

## help.pl
### help
Menampilkan instruksi permainan

## player.pl
### dynamic - inventory(X)
Tokemon X berada di inventory pemain

### dynamic - encounter(X)
Pemain sekarang berhadapan dengan tokemon X

### dynamic - battle(X)
Pemain sekarang memilih tokemon X untuk melawan tokemon lawan

### dynamic - hasHealed(X, Y)
Pemain sudah menggunakan gym di titik (X, Y)

### starterNotSpawned(X)
Tokemon X adalah tokemon starter(yang dipilih untuk menjadi inventory awal) dan belum ada di peta

### initPlayer
Inisiasi posisi player, dan inventory awal player

### writeNabrak, writeNorth, writeSouth, writeEast, writeWest
Menarasikan gerakan player

### w, a, s, d
Predikat translasi gerak player

### sizeInventory(Size)
Size adalah banyaknya inventory player

### isInventoryFull
Memeriksa apakah inventory player penuh

### pick(X)
Player memilih tokemon X untuk melawan tokemon lawan

### addTokemon(X)
Menambahkan tokemon X ke inventory

### drop(X)
Membuang tokemon X dari inventory

### checkEncounter
Memeriksa apakah pemain sekarang bertemu dengan tokemon yang berkeliaran

### fight
Memilih untuk melawan tokemon yang sekarang dihadapi

### run
Mencoba lari dari tokemon yang sekarang dihadapi, kemungkinan untuk berhasil kecil

### printInventory
Mencetak tokemon yang ada di inventory pemain

### heal
Menyembuhkan tokemon yang ada di inventory pemain jika berada di gym

### healList(ListTokemon)
Mengembalikan HP setiap tokemon yang ada di ListTokemon menjadi MaxHP, perhitungan MaxHP dihitung dari health maksimum saat level satu, dikalikan faktor pengali sesuai level

## tokemon.pl
### dynamic - tokemon(Name, X, Y, HP, Owner)
Deskripsi Tokemon yang sedang ada di peta.
Owner = 0 jika Tokemon tidak dimiliki pemain, Owner = 1 jika Tokemon ada di inventory pemain

### dynamic - special(X)
Tokemon dengan nama X telah menggunakan special attack

### dynamic - level(X, Exp)
Tokemon X memiliki experience sebanyak Exp.
Level dari Tokemon adalah floor(Exp).

### legendary(X)
Tokemon X adalah Tokemon Legendary.

### normal(X)
Tokemon X adalah Tokemon normal.

### evolveto(Prev, After)
Tokemon Prev akan evolve menjadi Tokemon After

### maxHealth(X, MaxHP)
Tokemon pada Level 1 memiliki health maksimum MaxHP

### type(X, Type)
Tokemon X memiliki tipe Type

### damage(X, Atk)
Tokemon X memiliki nominal normal attack sebesar Atk

### skill(X, Jurus, Atk)
Tokemon X memiliki special attack Jurus, dengan attack point sebesar Atk

### normalNotSpawned(X)
Tokemon X bertipe normal dan belum ada di peta

### legendaryNotSpawned(X)
Tokemon X bertipe legendary dan belum ada di peta

### legendaryRoaming(X)
Tokemon X bertipe legendary dan ada di peta

### expGain(X, Enemy, ExpGained)
ExpGained adalah Exp yang didapat ketika tokemon X mengalahkan tokemon Enemy
ExpGained = Mult*(1/(1 + 0.5*(LvlX - 1)))

### multiplier(X, Multiplier)
Multiplier adalah faktor pengali atribut tokemon X.
Faktor pengali ini mempengaruhi nilai HP dan Atk dari tokemon X.

### resetLvl
Menghapus semua predikat level, dan mengisi ulang database dengan level(X, 1.0). Dimana X adalah setiap tokemon normal

### resetLvlList(ListTokemon)
Memasukan level(X, 1.0) ke database, dimana X adalah anggota ListTokemon

### initNormal(NNormal), initLegendary(NLegendary)
Menempatkan NNormal tokemon normal dan NLegendary tokemon legendary di peta

### roamAllTokemon
Menggerakan semua tokemon yang berkeliaran di peta

### updateListTokemon(ListTokemon)
Memroses arah gerak tiap tokemon yang ada di ListTokemon

### decide(X, RNG)
Menentukan aksi tokemon X berdasarkan nilai RNG
Predikat ini bergantung pada decideNormal jika tipe tokemon normal, dan pada decideLegendary jika tipe tokemon legendary

### decideNormal(X, RNG), decideLegendary(X, RNG)
Menentukan arah gerak tokemon X berdasarkan nilai RNG, kemungkinan mendekati player untuk tokemon normal lebih besar dibandingkan tokemon legendary

### randomRoam(X)
Mengacak translasi koordinat tokemon X (tidak bergantung pada player)

### moveTowardsPlayer(X)
Tokemon X bergerak menuju player berdasarkan posisi tokemon X dan posisi player

### wTokemon, aTokemon, sTokemon, dTokemon
Predikat translasi koordinat tokemon

### status
Mencetak status tokemon yang ada di inventory player, dan tokemon legendary yang belum dikalahkan

### printStatus(ListTokemon)
Mencetak status dari setiap tokemon yang ada di ListTokemon

### printSpecialAttackMessage(TokemonP, Jurus), printPlayerDamage(AtkAtribut, Enemy), printEnemyDamage(Enemy, AtkAtribut, CurrentPicked), printBattleStatus(TokemonP, Enemy)
Predikat untuk tampilan battle tokemon

### capture
Menangkap tokemon yang sedang dilawan

### ignore
Tidak menangkap tokemon setelah dikalahkan

### attack, specialAttack
Command untuk melakukan serangan terhadap tokemon lawan

### decideEnemyBattle
Predikat untuk membuat musuh memilih tipe serangan, kemungkinan musuh melakukan specialAttack lebih kecil dari attack biasa

### enemyAttack, enemySpecialAttack
Predikat aksi serangan musuh

### kalah
Mengakhiri permainan dan menampilkan pesan kalah

### menang
Mengakhiri permainan dan menampilkan pesan menang

## tools.pl
### take(List, Pos, C)
Mengambil elemen ke-Pos dari List, C adalah elemen tersebut

### reset
Menghapus semua predikat dynamic dari database

## map.pl
### generateFence(NFence)/1
Menempatkan pagar sebanyak NFence ke peta secara acak

### generateGym(NGym)/1
Menempatkan gym sebanyak NGym ke peta secara acak

### isEdgeW(X, Y), isEdgeA(X, Y), isEdgeS(X, Y), isEdgeD(X, Y)
Memeriksa apakah titik (X, Y) merupakan ujung peta

### printPos(X, Y)
Mencetak isi petak (X, Y)

### map/0
Mencetak keseluruhan isi peta

## loadsave.pl

## compass.pl
### tokemonTingle
Compass buat player, untuk menunjukkan Tokemon terdekat ada di arah mata angin mana, kemungkinannya 70% untuk berhasil.

### calcDistance(X1, Y1, X2, Y2, Distance)
Menghitung jarak antar titik, Distance adalah jarak dari kedua titik tersebut

### findNearestTokemon(ListTokemon, XPlayer, YPlayer, NearestTokemon)
Mencari Tokemon terdekat dari titik (XPlayer, YPlayer)

### getDirection(XPlayer, YPlayer, XNearest, YNearest, Dir)
Mendapatkan arah mata angin dari Tokemon terdekat dengan player (1 : North, 2 : East, 3 : South, 4: West).

### writeDirection(Direction)
Mencetak arah mata angin sesuai Direction

### narrateSense(Direction)
Menarasikan arah mata angin yang menunjukkan arah tokemon terdekat

