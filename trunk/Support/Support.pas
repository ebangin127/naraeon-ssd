unit Support;

interface

uses
  SysUtils,
  Device.SMART.List;

type
  TTotalWriteType =
    (WriteNotSupported, WriteSupportedAsCount, WriteSupportedAsValue);

  TSupportStatus = record
    Supported: Boolean;
    FirmwareUpdate: Boolean;
    TotalWriteType: TTotalWriteType;
  end;

  TTotalWriteInCount = record
    ValueInCount: UInt64
  end;

  TTotalWriteInValue = record
    TrueHostWriteFalseNANDWrite: Boolean;
    ValueInMiB: UInt64;
  end;

  TTotalWrite = record
  case Integer of
    0: (InCount: TTotalWriteInCount);
    1: (InValue: TTotalWriteInValue);
  end;

  TReadEraseError = record
    TrueReadErrorFalseEraseError: Boolean;
    Value: UInt64;
  end;

  TSMARTAlert = record
    ReplacedSector: Boolean;
    ReadEraseError: Boolean;
    CriticalError: Boolean;
  end;

  TSMARTInterpreted = record
    UsedHour: UInt64;
    TotalWrite: TTotalWrite;
    ReadEraseError: TReadEraseError;
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
