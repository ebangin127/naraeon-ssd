unit Support;

interface

uses
  SysUtils,
  Device.SMART.List,
  BufferInterpreter;

type
  TTotalWriteType =
    (WriteNotSupported, WriteSupportedAsCount, WriteSupportedAsValue);

  TSupported =
    (NotSupported, Supported, CDISupported, CDIInsufficient);

  TSupportStatus = record
    Supported: TSupported;
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
    FIdentify: TIdentifyDeviceResult;
    FSMART: TSMARTValueList;
  protected
    property Identify: TIdentifyDeviceResult read FIdentify;
    property SMART: TSMARTValueList read FSMART;
  public
    constructor Create; overload;
    constructor Create(const Identify: TIdentifyDeviceResult;
      const SMART: TSMARTValueList); overload;

    procedure SetModelAndFirmware(const Identify: TIdentifyDeviceResult;
      const SMART: TSMARTValueList);

    function GetSupportStatus: TSupportStatus; virtual; abstract;
    function GetSMARTInterpreted(SMARTValueList: TSMARTValueList):
      TSMARTInterpreted; virtual; abstract;
  end;

implementation

{ TNSTSupport }

constructor TNSTSupport.Create(const Identify: TIdentifyDeviceResult;
  const SMART: TSMARTValueList);
begin
  SetModelAndFirmware(Identify, SMART);
end;

constructor TNSTSupport.Create;
begin
  ;// Intended to empty because of overloading rule
end;

procedure TNSTSupport.SetModelAndFirmware(const Identify: TIdentifyDeviceResult;
  const SMART: TSMARTValueList);
begin
  FIdentify := Identify;
  FSMART := SMART;
  FIdentify.Model := UpperCase(FIdentify.Model);
  FIdentify.Firmware := UpperCase(FIdentify.Firmware);
end;

end.
