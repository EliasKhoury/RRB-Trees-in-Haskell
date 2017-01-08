for SCRIPT in fileaa
do
    echo "[" > temp.txt
    #perl -ne "s/(.)(?=.*.)/'\1',/g; print;" $SCRIPT >> temp.txt
    sed "s/\(.\)/'\1',/g" $SCRIPT >> temp.txt
    truncate -s-2 temp.txt
    echo "]" >> temp.txt
done
