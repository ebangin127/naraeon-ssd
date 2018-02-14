unit SMARTSupport;

interface

uses
  SysUtils,
  BufferInterpreter, Device.SMART.List, Support;

const
  LifeNotSupported = Integer.MaxValue;
  CommonLifeThreshold = 10;

type
  TSMARTStatus = (Unknown, Good, Caution, Bad);
  TSMARTErrorResult = record
    Status: Boolean;
    Override: Boolean;
  end;
  TSMARTSupport = class abstract
  private
    function IsOldSSD(const Model: string): Boolean;
    function IsNewSSD(const RotationRate: TRotationRate): Boolean;
    function IsErrorAvailableForCommonHarddisk(
      const SMARTList: TSMARTValueList): Boolean;
    function IsErrorAvailableForCommonSSD(
      const SMARTList: TSMARTValueList): Boolean;
    function IsCautionAvailableForCommonHarddisk(
      const SMARTList: TSMARTValueList): Boolean;
    function IsCautionAvailableForCommonSSD(
      const SMARTList: TSMARTValueList): Boolean;
    function IsErrorForCommonHarddisk(const SMARTList: TSMARTValueList):
      Boolean;
    function IsErrorForCommonSSD(const SMARTList: TSMARTValueList): Boolean;
    function IsCautionForCommonHarddisk(const SMARTList: TSMARTValueList):
      Boolean;
    function IsCautionForCommonSSD(const SMARTList: TSMARTValueList): Boolean;
    function HarddiskCautionCheck(const SMARTEntry: TSMARTValueEntry;
      const Threshold: Integer): Boolean;
    function IsCaution(const SMARTList: TSMARTValueList): Boolean;
    function IsCautionAvailable(const SMARTList: TSMARTValueList): Boolean;
    function IsError(const SMARTList: TSMARTValueList): Boolean;
    function IsErrorAvailable(const SMARTList: TSMARTValueList): Boolean;
  public
    function IsThisStorageMine(
      const IdentifyDevice: TIdentifyDeviceResult;
      const SMARTList: TSMARTValueList): Boolean; virtual; abstract;
    function GetTypeName: String; virtual; abstract;
    function IsSSD: Boolean; virtual; abstract;
    function GetLife(const SMARTList: TSMARTValueList): Integer;
    function GetDriveStatus(const SMARTList: TSMARTValueList): TSMARTStatus;
    function IsInsufficientSMART: Boolean; virtual; abstract;
    function IsWriteValueSupported(
      const SMARTList: TSMARTValueList): Boolean; virtual; abstract;
    function GetSMARTInterpreted(
      const SMARTList: TSMARTValueList): TSMARTInterpreted; virtual; abstract;
  protected
    function ErrorCheckedGetLife(const SMARTList: TSMARTValueList): Integer;
      virtual;
    function Find(const ToFind, At: String): Boolean;
    function FindAtFirst(const ToFind, At: String): Boolean;
    function CheckIsSSDInCommonWay(const IdentifyDevice: TIdentifyDeviceResult):
      Boolean;
    function IsEntryAvailable(const ID: Byte;
      const SMARTList: TSMARTValueList): Boolean;
    function InnerIsErrorAvailable(const SMARTList: TSMARTValueList):
      Boolean; virtual; 
    function InnerIsCautionAvailable(const SMARTList: TSMARTValueList): 
      Boolean; virtual; 
    function InnerIsError(const SMARTList: TSMARTValueList): TSMARTErrorResult;
      virtual; 
    function InnerIsCaution(const SMARTList: TSMARTValueList):
      TSMARTErrorResult; virtual; 
    function InnerCommonIsError(
      const EntryID: Byte; const SMARTList: TSMARTValueList): TSMARTErrorResult;
    function InnerCommonIsCaution(
      const EntryID: Byte; const SMARTList: TSMARTValueList;
      const Threshold: Integer): TSMARTErrorResult;
  end;

const
  TSMARTStatusInString: Array[TSMARTStatus] of String = (
    'Unknown', 'Good', 'Caution', 'Bad');

implementation

{ TSMARTSupport }

function TSMARTSupport.ErrorCheckedGetLife(const SMARTList: TSMARTValueList):
  Integer;
begin
  result := LifeNotSupported;
end;

function TSMARTSupport.Find(const ToFind, At: String): Boolean;
begin
  result := Pos(ToFind, At) > 0;
end;

function TSMARTSupport.FindAtFirst(const ToFind, At: String): Boolean;
begin
  result := Pos(ToFind, At) = 1;
end;

function TSMARTSupport.GetLife(const SMARTList: TSMARTValueList): Integer;
begin
  try
    result := ErrorCheckedGetLife(SMARTList);
  except
    exit(LifeNotSupported);
  end;
  if result > 100 then
    result := LifeNotSupported;
end;

function TSMARTSupport.CheckIsSSDInCommonWay(
  const IdentifyDevice: TIdentifyDeviceResult): Boolean;
begin
  result :=
    (IsOldSSD(IdentifyDevice.Model)) or
    (IsNewSSD(IdentifyDevice.RotationRate));
end;

// Ported CrystalDiskInfo (The MIT License, http://crystalmark.info)
function TSMARTSupport.IsOldSSD(const Model: string): Boolean;
begin
	result :=
    FindAtFirst('OCZ', Model) or
    FindAtFirst('SPCC', Model) or
    FindAtFirst('PATRIOT', Model) or
    FindAtFirst('PHOTOFAST', Model) or
    FindAtFirst('STT_FTM', Model) or
    FindAtFirst('Super Talent', Model) or
    Find('Solid', Model) or
    Find('SSD', Model) or
    Find('SiliconHardDisk', Model);
end;

function TSMARTSupport.IsNewSSD(const RotationRate: TRotationRate):
  Boolean;
const
  SSDRate = 1;
begin
	result :=
    (RotationRate.Supported) and
    (RotationRate.Value = SSDRate);
end;

function TSMARTSupport.GetDriveStatus(const SMARTList: TSMARTValueList):
  TSMARTStatus;
var
  Error: Boolean;
  Unknown: Boolean;
  Caution: Boolean;
begin
  Error := IsErrorAvailable(SMARTList)  and IsError(SMARTList);
  Unknown := not IsCautionAvailable(SMARTList);
  Caution := IsCautionAvailable(SMARTList) and IsCaution(SMARTList);
  if Error then
    result := TSMARTStatus.Bad
  else if Unknown then
    result := TSMARTStatus.Unknown
  else if Caution then
    result := TSMARTStatus.Caution
  else
    result := TSMARTStatus.Good;
end;

function TSMARTSupport.IsError(const SMARTList: TSMARTValueList):
  Boolean;
var
  InnerResult: TSMARTErrorResult;
begin
  if IsSSD then
    result := IsErrorForCommonSSD(SMARTList)
  else
    result := IsErrorForCommonHarddisk(SMARTList);
  InnerResult := InnerIsError(SMARTList);
  result := result and InnerResult.Status;
  if InnerResult.Override then
    result := InnerResult.Status;
end;

function TSMARTSupport.IsCaution(const SMARTList: TSMARTValueList):
  Boolean;
var
  InnerResult: TSMARTErrorResult;
begin
  if IsSSD then
    result := IsCautionForCommonSSD(SMARTList)
  else
    result := IsCautionForCommonHarddisk(SMARTList);
  InnerResult := InnerIsCaution(SMARTList);
  result := result and InnerResult.Status;
  if InnerResult.Override then
    result := InnerResult.Status;
end;

function TSMARTSupport.IsErrorAvailable(const SMARTList: TSMARTValueList):
  Boolean;
begin
  if IsSSD then
    result := IsErrorAvailableForCommonSSD(SMARTList)
  else
    result := IsErrorAvailableForCommonHarddisk(SMARTList);
  result := result or InnerIsErrorAvailable(SMARTList);
end;

function TSMARTSupport.IsCautionAvailable(const SMARTList: TSMARTValueList):
  Boolean;
begin
  if IsSSD then
    result := IsCautionAvailableForCommonSSD(SMARTList)
  else
    result := IsCautionAvailableForCommonHarddisk(SMARTList);
  result := result or InnerIsCautionAvailable(SMARTList);
end;

function TSMARTSupport.IsErrorAvailableForCommonHarddisk(
  const SMARTList: TSMARTValueList): Boolean;
begin
  result := true;
end;
  
function TSMARTSupport.IsErrorAvailableForCommonSSD(
  const SMARTList: TSMARTValueList): Boolean;
begin
  result := true;
end;

function TSMARTSupport.IsEntryAvailable(const ID: Byte;
  const SMARTList: TSMARTValueList): Boolean;
begin
  try
    SMARTList.GetIndexByID(ID);
    result := true;
  except
    on E: EEntryNotFound do
      result := false;
    else raise;
  end;
end;
  
function TSMARTSupport.IsCautionAvailableForCommonHarddisk(
  const SMARTList: TSMARTValueList): Boolean;
begin
  result :=
    IsEntryAvailable(5, SMARTList) or
    IsEntryAvailable($C5, SMARTList) or
    IsEntryAvailable($C6, SMARTList);
end;
  
function TSMARTSupport.IsCautionAvailableForCommonSSD(
  const SMARTList: TSMARTValueList): Boolean;
var
  CurrentEntry: TSMARTValueEntry;
begin
  result := false;
  for CurrentEntry in SMARTList do
    result := result or (CurrentEntry.Threshold > 0);
end;

function TSMARTSupport.IsErrorForCommonHarddisk(
  const SMARTList: TSMARTValueList): Boolean;
const
  Y = true;
  n = false;
  IsValueToBeChecked: Array[Byte] of Boolean = 
     {0 1 2 3 4 5 6 7 8 9 A B C D E F} 
  {0}(Y,Y,Y,Y,Y,Y,Y,Y,Y,Y,Y,Y,Y,Y,n,n,
  {1} n,n,n,n,n,n,n,n,n,n,n,n,n,n,n,n,
  {2} n,n,n,n,n,n,n,n,n,n,n,n,n,n,n,n,
  {3} n,n,n,n,n,n,n,n,n,n,n,n,n,n,n,n,
  {4} n,n,n,n,n,n,n,n,n,n,n,n,n,n,n,n,
  {5} n,n,n,n,n,n,n,n,n,n,n,n,n,n,n,n,
  {6} n,n,n,n,n,n,n,n,n,n,n,n,n,n,n,n,
  {7} n,n,n,n,n,n,n,n,n,n,n,n,n,n,n,n,
  {8} n,n,n,n,n,n,n,n,n,n,n,n,n,n,n,n,
  {9} n,n,n,n,n,n,n,n,n,n,n,n,n,n,n,n,
  {A} n,n,n,n,n,n,n,n,n,n,n,n,n,n,n,n,
  {B} n,n,n,n,n,n,n,n,n,n,n,Y,Y,Y,n,Y,
  {C} Y,Y,n,Y,Y,Y,Y,Y,Y,Y,Y,Y,Y,Y,Y,Y,
  {D} Y,Y,n,Y,Y,n,n,n,n,n,n,n,Y,Y,Y,Y,
  {E} Y,Y,Y,Y,Y,n,Y,Y,n,n,n,n,n,n,n,n,
  {F} Y,n,n,n,n,n,n,n,n,n,Y,n,n,n,Y,n);
var
  CurrentEntry: TSMARTValueEntry;
begin
  result := false;
  for CurrentEntry in SMARTList do
    if IsValueToBeChecked[CurrentEntry.ID] then
      if CurrentEntry.Threshold > CurrentEntry.Current then
        exit(true);
end;
  
function TSMARTSupport.IsErrorForCommonSSD(
  const SMARTList: TSMARTValueList): Boolean;
var
  CurrentEntry: TSMARTValueEntry;
begin
  result := false;
  for CurrentEntry in SMARTList do
    if CurrentEntry.ID <> $C2 then
      if CurrentEntry.Threshold > CurrentEntry.Current then
        exit(true);
end;

function TSMARTSupport.HarddiskCautionCheck(
  const SMARTEntry: TSMARTValueEntry; const Threshold: Integer): Boolean;
const
  SkipValue = $FFFFFFFF;
begin 
  result :=
    ((SMARTEntry.RAW and SkipValue) <> SkipValue) and
    (SMARTEntry.RAW >= Threshold);
end;

function TSMARTSupport.IsCautionForCommonHarddisk(
  const SMARTList: TSMARTValueList): Boolean;
begin
  result := false;
  if IsEntryAvailable(5, SMARTList) then 
    result := HarddiskCautionCheck(SMARTList[SMARTList.GetIndexByID(5)], 1);
  if IsEntryAvailable($C5, SMARTList) then
    result :=
      result or HarddiskCautionCheck(SMARTList[SMARTList.GetIndexByID($C5)], 1);
  if IsEntryAvailable($C6, SMARTList) then
    result :=
      result or HarddiskCautionCheck(SMARTList[SMARTList.GetIndexByID($C6)], 1);
end;

function TSMARTSupport.IsCautionForCommonSSD(
  const SMARTList: TSMARTValueList): Boolean;
begin
  result := false;
end;

function TSMARTSupport.InnerIsErrorAvailable(const SMARTList: TSMARTValueList):
  Boolean;  
begin
  result := false;
end;
  
function TSMARTSupport.InnerIsCautionAvailable( 
  const SMARTList: TSMARTValueList): Boolean;  
begin
  result := false;
end;

function TSMARTSupport.InnerIsError(const SMARTList: TSMARTValueList):
  TSMARTErrorResult;
begin
  result.Status := false;
  result.Override := false;
end;

function TSMARTSupport.InnerIsCaution(const SMARTList: TSMARTValueList):
  TSMARTErrorResult;  
begin
  result.Status := false;
  result.Override := false;
end;

function TSMARTSupport.InnerCommonIsError(
  const EntryID: Byte; const SMARTList: TSMARTValueList): TSMARTErrorResult;
var
  CurrentEntry: TSMARTValueEntry;
begin
  result.Override := false;
  result.Status := false;
  if IsEntryAvailable(EntryID, SMARTList) then
    CurrentEntry := SMARTList[SMARTList.GetIndexByID(EntryID)];
  result.Status := result.Status or
    (CurrentEntry.Current = 0) or
    (CurrentEntry.Current < CurrentEntry.Threshold);
end;

function TSMARTSupport.InnerCommonIsCaution(
  const EntryID: Byte; const SMARTList: TSMARTValueList;
  const Threshold: Integer): TSMARTErrorResult;
var
  Entry: TSMARTValueEntry;
begin
  result.Override := false;
  if IsEntryAvailable(EntryID, SMARTList) then
    Entry := SMARTList[SMARTList.GetIndexByID(EntryID)];
  if Entry.Current < Threshold then
    result.Status := true;
end;

end.
