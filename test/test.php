<?php
echo "Enter your name: ";
$handle = fopen ("php://stdin","r");
$line = fgets($handle);
if(trim($line) != 'yes'){
    echo "Hello ".$line."\n";
}
echo "\n\r";


  print "Hi\n\r";
  for($i = 1; $i < 10; $i++)
    print "10 X ".$i." = ".(10 * $i)."\n\r";
?>