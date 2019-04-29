if a%1 == a  goto virhe
elo -NXp ssmgun,r-1,y-50,x-%1   ssmgun,r-1,f-1,y-50,x%1 %2 %3 %4 %5 %6 %7 %8 %9
goto loppu
:virhe
echo Usage: DOSSMGUN distance (150 for example)
:loppu
