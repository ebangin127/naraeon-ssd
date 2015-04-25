<?php
  require './modules/NSTAccount.php';
  require './modules/NSTGetIP.php';

  class Stages {
    var $Conn;
    var $Model;
    var $Firmware;

    function CheckForms() {
      //Check the form
      if((!isset($_GET['Model'])) || (!isset($_GET['Firmware']))) {
        printf("Failed 0 - Fill the form properly");
        return 0;
      }

      $this->Model =
        $this->Conn->real_escape_string($_GET['Model']);
      $this->Firmware =
        $this->Conn->real_escape_string($_GET['Firmware']);
      return 1;
    }

    function Connect($ID, $PASS, $DB) {
      $this->Conn = new mysqli(
        "localhost",
        $ID,
        $PASS,
        $DB);

      if ($this->Conn->connect_error) {
        printf(
          "Failed 1 - MySQL connection Failed<br/>(Error: %d)",
          $this->Conn->connect_errno);
        return 0;
      }
      return 1;
    }

    function WriteLog($IP, $Num) {
      $Query =
        sprintf(
          "INSERT INTO Log
           (ID, Date, IP, Action, Model, Firmware)
           VALUES
           (NULL, CURRENT_TIMESTAMP, '%s', %d, '%s', '%s')",
          $IP, $Num, $this->Model, $this->Firmware);

      if (!$Result = $this->Conn->query($Query)) {
        printf(
          "Failed 2 - Query failed while writing log<br/>(Error: %s)",
          $this->Conn->error);
        return 0;
      }
      return 1;
    }

    function FindProduct($Specific) {
      $Query =
        sprintf(
          "SELECT * FROM FirmwareList
           WHERE Model = '%s'", $this->Model);

      if ($Result = $this->Conn->query($Query)) {
        if ($Result->num_rows) {
          $Row = mysqli_fetch_assoc($Result);
          $Specific($Row);
          $Result->close();
        }
        else {
          printf("0");
          $Result->close();
        }
      }
      else {
        printf(
          "Failed 3 - Query failed while getting firmware<br/>(Error Number: %s)",
          $this->Conn->error);
        return 0;
      }
      return 1;
    }

    function Close() {
      $this->Conn->close();
    }
  }

  function CommonJob($Num, $Specific) {
    $Stage = new Stages();

    //Connect
    if (!$Stage->Connect(LogID(), LogPassword(), LogDB())) {
      return 0;
    }

    if (!$Stage->CheckForms()) {
      $Stage->Close();
      return 0;
    }

    //Write Log
    if (!$Stage->WriteLog(GetIP(), $Num)) {
      $Stage->Close();
      return 0;
    }
    
    //Disconnect
    $Stage->Close();

    //Connect
    if (!$Stage->Connect(FirmwareID(), FirmwarePassword(), FirmwareDB())) {
      return 0;
    }

    //Find Product
    if (!$Stage->FindProduct($Specific)) {
      $Stage->Close();
      return 0;
    }

    //Disconnect
    $Stage->Close();
  }
?>
