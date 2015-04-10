unit uNSTSupport;

interface

uses
  SysUtils,
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
    UsedHour: UInt64;
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
    constructor Create; overload;
    constructor Create(ModelToCheck, FirmwareToCheck: String); overload;

    procedure SetModelAndFirmware(ModelToCheck, FirmwareToCheck: String);

    function GetSupportStatus: TSupportStatus; virtual; abstract;
    function GetSMARTInterpreted(SMARTValueList: TSMARTValueList):
      TSMARTInterpreted; virtual; abstract;
  end;

implementation

{ TNSTSupport }

constructor TNSTSupport.Create;
begin
end;

constructor TNSTSupport.Create(ModelToCheck, FirmwareToCheck: String);
begin
  SetModelAndFirmware(ModelToCheck, FirmwareToCheck);
end;

procedure TNSTSupport.SetModelAndFirmware(ModelToCheck,
  FirmwareToCheck: String);
begin
  FModel := UpperCase(ModelToCheck);
  FFirmware := UpperCase(FirmwareToCheck);
end;

end.
