<?php
  require './modules/nst_common.php';

  function SpecificJob($Row) {
    $VersionCompare =
      (strcmp($_GET['Firmware'], $Row['Firmware']) >= 0) + 1;
    printf(
      "%d\n%s\n%s",
      $VersionCompare,
      $Row['Firmware'],
      $Row['DownName']);
  }

  $Specific = 'SpecificJob';
  CommonJob(0, $Specific);
?>
