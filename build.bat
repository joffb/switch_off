del lites.wsc 

..\superfamiconv palette -v -R -P 1 -i tiles.png --mode wsc --out-data tiles.palette
..\superfamiconv palette -v -R -P 1 -i tiles.png --mode wsc --out-data sprites.palette
..\superfamiconv tiles -R -D -B 4 -v -i tiles.png --mode wsc --out-data tiles.raw

..\superfamiconv palette -v -R -P 1 -i title_scroller.png --mode wsc --out-data title_scroller.palette
..\superfamiconv tiles -v -R -D -B 4 -i title_scroller.png --mode wsc --out-data title_scroller.raw

..\superfamiconv tiles -v -R -D -B 4 -i title.png --mode wsc --out-data title.raw

node mml.js song1.mml song1.song
node mml.js song2.mml song2.song
node mml.js winsong.mml winsong.song

nasm -fbin -l lites.lst -o lites.wsc lites.asm