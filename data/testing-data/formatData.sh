#!/bin/sh

INPUT=file*

echo "<html> <head> </head> <body> 
<table border=\"1\" cellpadding=\"5\" cellspacing=\"5\">"
perl -p -i -n -e 's/,(?=[^"]*"(?:[^"]*"[^"]*")*[^"]*$)/ /g; print;' $INPUT 
sed -i 's/,/<\/td><td>/g' $INPUT
sed -i 's/$/<\/td><\/tr>/' $INPUT
sed -i 's/^/<tr><td>/' $INPUT
sed -i "s/'//g" $INPUT
#perl -p -i -e 'if(!eof){s/\n/,/}' $INPUT


#| sed "s/\(.\)/'\1',/g"
echo "<\table><\body><\html>"
