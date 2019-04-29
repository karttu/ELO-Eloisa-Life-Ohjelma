REM Run the glider gun in one character per cell mode:
elo -p glidergu,y10 %1 %2 %3 %4 %5 %6 %7 %8 %9
REM Load rpentomino:
elo -XxpN rpentomi %1 %2 %3 %4 %5 %6 %7 %8 %9
REM Load some mirrors:
elo -XxpN mirrors:mirrors -c160 -R777
REM Load two "butterflies" (i.e. large spaceships):
elo -X ss:l ss:l,r2,y-5 -x -R3000
REM Load two adjacent spaceship guns, the other shooting down and the other up
elo -xXp ssmgun,x-150 ssmgun,x150,f-1 -R1900 %1 %2 %3 %4 %5 %6 %7 %8 %9
REM Load two spaceship guns, facing each other, and run 2000 generations:
elo -NxXp ssmgun,r-1,y-50,x-200 ssmgun,r-1,f-1,y-50,x200 -R2000 %1 %2 %3 %4 %5 %6 %7 %8 %9
REM Loads a medium weight spaceship puffer train.
elo -xXp mwsspuff -R2700 %1 %2 %3 %4 %5 %6 %7 %8 %9
REM Start with some random soap (with density 1/3):
elo -xXp -ss -R1000 -S %1 %2 %3 %4 %5 %6 %7 %8 %9
