echo "[" > tree1.txt
sed "s/\(.\)/'\1',/g" fileaa >> tree1.txt
truncate -s-2 tree1.txt
echo "]" >> tree1.txt

for SCRIPT in fileab
do
    echo "[" > tree2.txt
    #perl -ne "s/(.)(?=.*.)/'\1',/g; print;" $SCRIPT >> temp.txt
    sed "s/\(.\)/'\1',/g" $SCRIPT >> tree2.txt
    truncate -s-2 tree2.txt
    echo "]" >> temp.txt

    #./concat-trees
done

echo "<html> <head> </head> <body> 
<table border=\"1\" cellpadding=\"5\" cellspacing=\"5\">"
#cat tables.html
echo "<\table><\body><\html>"
