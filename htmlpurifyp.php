<?php

  require_once 'htmlpurifier-4.4.0/library/HTMLPurifier.auto.php';
  $x = file_get_contents($argv[1]);
  $purifier = new HTMLPurifier();
  $config = HTMLPurifier_Config::createDefault();
  $config->set('Core.CollectErrors', true);
  $clean_html = $purifier->purify($x,$config); 
  // echo($clean_html);
  $errors = $purifier->context->get('ErrorCollector')->getRaw();
  if (!empty($errors)) { echo("Errors"); } 

?>

