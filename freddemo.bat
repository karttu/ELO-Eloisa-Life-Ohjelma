REM Load the image of shogi piece 'lance' and run 2048 generations with it:
elo -xXNF -Z16 lance.fre -R2048 %1 %2 %3 %4 %5 %6 %7 %8 %9
REM Load the image of shogi piece 'knight' and run with twisted (-t2) toroid:
elo -XF -t2 -Z16 knight.fre,x-200 -R1024 %1 %2 %3 %4 %5 %6 %7 %8 %9
REM Load the image of shogi piece 'silver general' and run 2048 generations:
elo -xXF -Z32 silver.fre -R2048 %1 %2 %3 %4 %5 %6 %7 %8 %9
REM Load the image of an icosaedr together with the shogi piece 'bishop':
elo -xXF -Z32 ikosaedr.fre bishop.fre -R2048 %1 %2 %3 %4 %5 %6 %7 %8 %9
REM Load the image of an ancient oriental horoscope octagon:
elo -xXF -Z32 octagon.fre -R512 %1 %2 %3 %4 %5 %6 %7 %8 %9
REM Load some Escherian fishes:
elo -xXF -Z32 triangl2.fre -R1024 %1 %2 %3 %4 %5 %6 %7 %8 %9
