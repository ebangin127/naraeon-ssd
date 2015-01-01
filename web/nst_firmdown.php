<?php
  require './modules/nst_common.php';

  function SpecificJob($Row) {
    header(sprintf("Location: %s", $Row['DownPath']));
  }

  $Specific = 'SpecificJob';
  CommonJob($Specific);
?>
