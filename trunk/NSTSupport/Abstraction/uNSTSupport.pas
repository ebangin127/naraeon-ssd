unit uNSTSupport;

interface

uses
  uSMARTValueList;

type
  TTotalWriteType =
    (WriteNotSupported, WriteSupportedAsCount, WriteSupportedAsValue);

  TSupportStatus = record
    Supported: Boolean;
    FirmwareUpdate: Boolean;
    TotalWriteType: TTotalWriteType;
  end;

  TTotalWrite = record
    TrueHostWriteFalseNANDWrite: Boolean;
    ValueInMiB: UInt64;
  end;

  TSMARTAlert = record
    ReplacedSector: Boolean;
    EraseError: Boolean;
  end;

  TSMARTInterpreted = record
    TotalWrite: TTotalWrite;
    EraseError: UInt64;
    ReplacedSectors: UInt64;
    SMARTAlert: TSMARTAlert;
  end;

  TNSTSupport = class abstract
  private
    FModel, FFirmware: String;
  protected
    property Model: String read FModel;
    property Firmware: String read FFirmware;
  public
    constructor Create(Model, Firmware: String);
    function GetSupportStatus: TSupportStatus; virtual; abstract;
    function GetSMARTInterpreted(SMARTValueList: TSMARTValueList):
      TSMARTInterpreted; virtual; abstract;
  end;

implementation

{ TNSTSupport }

constructor TNSTSupport.Create(Model, Firmware: String);
begin
  FModel := Model;
  FFirmware := Firmware;
end;

end.
