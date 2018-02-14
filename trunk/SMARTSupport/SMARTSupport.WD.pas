// Ported CrystalDiskInfo (The MIT License, http://crystalmark.info)
unit SMARTSupport.WD;

interface

uses
  BufferInterpreter, Device.SMART.List, SMARTSupport, Support;

type
  TWDSMARTSupport = class(TSMARTSupport)
  public
    function IsThisStorageMine(
      const IdentifyDevice: TIdentifyDeviceResult;
      const SMARTList: TSMARTValueList): Boolean; override;
    function GetTypeName: String; override;
    function IsSSD: Boolean; override;
    function IsInsufficientSMART: Boolean; override;
    function GetSMARTInterpreted(
      const SMARTList: TSMARTValueList): TSMARTInterpreted; override;
    function IsWriteValueSupported(
      const SMARTList: TSMARTValueList): Boolean; override;
  end;

implementation

{ TWDSMARTSupport }

function TWDSMARTSupport.GetTypeName: String;
begin
  result := 'Smart';
end;

function TWDSMARTSupport.IsInsufficientSMART: Boolean;
begin
  result := false;
end;

function TWDSMARTSupport.IsSSD: Boolean;
begin
  result := false;
end;

function TWDSMARTSupport.IsThisStorageMine(
  const IdentifyDevice: TIdentifyDeviceResult;
  const SMARTList: TSMARTValueList): Boolean;
begin
  result :=
    (FindAtFirst('WDC ', IdentifyDevice.Model)) and
    (not CheckIsSSDInCommonWay(IdentifyDevice));
end;

function TWDSMARTSupport.IsWriteValueSupported(
  const SMARTList: TSMARTValueList): Boolean;
begin
  result := false;
end;

function TWDSMARTSupport.GetSMARTInterpreted(
  const SMARTList: TSMARTValueList): TSMARTInterpreted;
const
  ReadError = true;
  EraseError = false;

  UsedHourID = $09;
  ThisErrorType = ReadError;
  ErrorID = $01;
  ReplacedSectorsID = $05;
begin
  FillChar(result, SizeOf(result), 0);
  result.UsedHour := SMARTList.ExceptionFreeGetRAWByID(UsedHourID);
  result.ReadEraseError.TrueReadErrorFalseEraseError := ReadError;
  result.ReadEraseError.Value := SMARTList.ExceptionFreeGetRAWByID(ErrorID);
  result.ReplacedSectors :=
    SMARTList.ExceptionFreeGetRAWByID(ReplacedSectorsID);
end;

end.

