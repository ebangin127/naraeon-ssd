// Ported CrystalDiskInfo (The MIT License, http://crystalmark.info)
unit SMARTSupport.Mtron;

interface

uses
  BufferInterpreter, Device.SMART.List, SMARTSupport,
  Support;

type
  TMtronSMARTSupport = class(TSMARTSupport)
  private
    const
      EntryID = $BB;
  public
    function IsThisStorageMine(
      const IdentifyDevice: TIdentifyDeviceResult;
      const SMARTList: TSMARTValueList): Boolean; override;
    function GetTypeName: String; override;
    function IsSSD: Boolean; override;
    function IsInsufficientSMART: Boolean; override;
    function GetSMARTInterpreted(
      const SMARTList: TSMARTValueList): TSMARTInterpreted; override;
    function IsWriteValueSupported(const SMARTList: TSMARTValueList): Boolean;
      override;
  protected
    function ErrorCheckedGetLife(const SMARTList: TSMARTValueList): Integer;
      override;
    function InnerIsErrorAvailable(const SMARTList: TSMARTValueList):
      Boolean; override; 
    function InnerIsCautionAvailable(const SMARTList: TSMARTValueList): 
      Boolean; override; 
    function InnerIsError(const SMARTList: TSMARTValueList): TSMARTErrorResult;
      override; 
    function InnerIsCaution(const SMARTList: TSMARTValueList):
      TSMARTErrorResult; override; 
  end;

implementation

{ TMtronSMARTSupport }

function TMtronSMARTSupport.GetTypeName: String;
begin
  result := 'SmartMtron';
end;

function TMtronSMARTSupport.IsInsufficientSMART: Boolean;
begin
  result := true;
end;

function TMtronSMARTSupport.IsSSD: Boolean;
begin
  result := true;
end;

function TMtronSMARTSupport.IsThisStorageMine(
  const IdentifyDevice: TIdentifyDeviceResult;
  const SMARTList: TSMARTValueList): Boolean;
begin
  result :=
    (FindAtFirst('MTRON', IdentifyDevice.Model)) and
    (SMARTList.Count = 1) and
    (SMARTList[0].ID = $BB);
end;

function TMtronSMARTSupport.ErrorCheckedGetLife(
  const SMARTList: TSMARTValueList): Integer;
begin
  result := SMARTList[SMARTList.GetIndexByID($BB)].Current;
end;

function TMtronSMARTSupport.InnerIsErrorAvailable(
  const SMARTList: TSMARTValueList): Boolean; 
begin
  result := true;
end;

function TMtronSMARTSupport.InnerIsCautionAvailable(
  const SMARTList: TSMARTValueList): Boolean;
begin
  result := IsEntryAvailable(EntryID, SMARTList);
end;

function TMtronSMARTSupport.InnerIsError(
  const SMARTList: TSMARTValueList): TSMARTErrorResult;
begin
  result.Override := false;
  result.Status :=
    InnerCommonIsError(EntryID, SMARTList).Status;
end;

function TMtronSMARTSupport.InnerIsCaution(
  const SMARTList: TSMARTValueList): TSMARTErrorResult; 
begin
  result := InnerCommonIsCaution(EntryID, SMARTList, CommonLifeThreshold);
end;

function TMtronSMARTSupport.IsWriteValueSupported(
  const SMARTList: TSMARTValueList): Boolean;
begin
  result := false;
end;

function TMtronSMARTSupport.GetSMARTInterpreted(
  const SMARTList: TSMARTValueList): TSMARTInterpreted;
begin
  FillChar(result, SizeOf(result), 0);
end;

end.
