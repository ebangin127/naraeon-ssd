<?php
  require './Includes/NSTCommonPart.php';

  function SpecificJob($Row) {
    header(sprintf("Location: %s", $Row['DownPath']));
  }

  $Specific = 'SpecificJob';
  CommonJob(1, $Specific);
?>
