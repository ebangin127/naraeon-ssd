<?php
  require './modules/nst_config.php';
  require './modules/nst_function.php';

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
        mysqli_real_escape_string($this->Conn, $_GET['Model']);
      $this->Firmware =
        mysqli_real_escape_string($this->Conn, $_GET['Firmware']);
      return 1;
    }

    function Connect() {
      $this->Conn = new mysqli(
        "localhost",
        db_id(),
        db_pass(),
        "naraeonn_nstools");

      if ($this->Conn->connect_error) {
        printf(
          "Failed 1 - MySQL connection Failed<br/>(Error: %d)",
          $this->Conn->connect_errno);
        return 0;
      }
      return 1;
    }

    function WriteLog($IP) {
      $Query =
        sprintf(
          "INSERT INTO Log
           (ID, Date, IP, Action, Model, Firmware)
           VALUES
           (NULL, CURRENT_TIMESTAMP, '%s', 1, '%s', '%s')",
          $IP, $this->Model, $this->Firmware);

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

  function CommonJob($Specific) {
    $Stage = new Stages();

    //Connect
    if (!$Stage->Connect()) {
      return 0;
    }

    if (!$Stage->CheckForms()) {
      $Stage->Close();
      return 0;
    }

    //Write Log
    if (!$Stage->WriteLog(GetIP())) {
      $Stage->Close();
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
