echo "<html> <head> </head> <body> 
<table border=\"1\" cellpadding=\"5\" cellspacing=\"5\">"
perl -ne 's/,(?=[^"]*"(?:[^"]*"[^"]*")*[^"]*$)/ /g; print;' file* |sed 's/,/<\/td><td>/g' | sed 's/$/<\/td><\/tr>/' | sed 's/^/<tr><td>/'
echo "<\table><\body><\html>"
