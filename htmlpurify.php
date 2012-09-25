<?php

  require_once 'htmlpurifier-4.4.0/library/HTMLPurifier.auto.php';
  $x = file_get_contents($argv[1]);
  $purifier = new HTMLPurifier();
  $clean_html = $purifier->purify($x); 
  echo($clean_html);
?>

