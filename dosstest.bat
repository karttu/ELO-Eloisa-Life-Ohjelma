if a%1 == a  goto virhe
elo -NXp ssmgun,r-1,y-50,x-201  ssmgun,r-1,f-1,y-50,x%1
goto loppu
:virhe
echo Usage: DOSSMGUN distance (150 for example)
:loppu
