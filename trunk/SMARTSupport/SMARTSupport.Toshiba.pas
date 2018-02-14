// Ported CrystalDiskInfo (The MIT License, http://crystalmark.info)
unit SMARTSupport.Toshiba;

interface

uses
  BufferInterpreter, Device.SMART.List, SMARTSupport, Support;

type
  TToshibaSMARTSupport = class(TSMARTSupport)
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

{ TToshibaSMARTSupport }

function TToshibaSMARTSupport.GetTypeName: String;
begin
  result := 'SmartToshiba';
end;

function TToshibaSMARTSupport.IsInsufficientSMART: Boolean;
begin
  result := false;
end;

function TToshibaSMARTSupport.IsSSD: Boolean;
begin
  result := true;
end;

function TToshibaSMARTSupport.IsThisStorageMine(
  const IdentifyDevice: TIdentifyDeviceResult;
  const SMARTList: TSMARTValueList): Boolean;
begin
  result :=
    FindAtFirst('TOSHIBA', IdentifyDevice.Model) and
    CheckIsSSDInCommonWay(IdentifyDevice);
end;

function TToshibaSMARTSupport.IsWriteValueSupported(
  const SMARTList: TSMARTValueList): Boolean;
begin
  result := false;
end;

function TToshibaSMARTSupport.GetSMARTInterpreted(
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
