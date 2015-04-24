unit uRefresh;

interface

uses
  Classes, SysUtils, Math, Vcl.Controls, Vcl.Graphics, Vcl.StdCtrls, Windows,
  uAlert, uLanguageSettings, ShellApi, Dialogs, Generics.Collections,
  uDiskFunctions, uRegistryHelper,
  uDatasizeUnit, uStrFunctions, uLogSystem, uNSTSupport,
  uPhysicalDriveList, uBufferInterpreter, uFirmwareGetter,
  uPathManager, uPhysicalDrive, uPartitionListGetter, uNCQAvailabilityGetter;

function RefreshTimer(ShowSerial: Boolean;
  FirstiOptLeft: Integer): Boolean;
procedure RefreshDrives(PhysicalDrive: TPhysicalDrive);

type
  TSSDLabel = class(TLabel)
  public
    DeviceInfo: TPhysicalDrive;
  end;

  TSSDLabelList = class(TList<TSSDLabel>)
  public
    destructor Destroy; override;
    procedure Delete(Index: Integer);

    function IndexOf(Entry: TPhysicalDrive): Integer;
  end;

implementation

uses uMain;

function BinaryToDenary(Size: Double): Double;
begin
  exit(Size * (512 / 500));
end;

function LBAtoKB(Size: Double): Double;
begin
  exit(Size / 2);
end;

procedure ApplyBasicInfo(PhysicalDrive: TPhysicalDrive;
  ShowSerial: Boolean);
var
  CurrNum: Integer;
  DenaryUserSizeInKB: Double;
  KBtoMB: TDatasizeUnitChangeSetting;
  DenaryInteger: FormatSizeSetting;
  Query: TFirmwareQuery;
  QueryResult: TFirmwareQueryResult;
  NCQAvailability: TNCQAvailability;
begin
  with fMain do
  begin
    KBtoMB.FNumeralSystem := Denary;
    KBtoMB.FFromUnit := KiloUnit;
    KBtoMB.FToUnit := MegaUnit;

    DenaryUserSizeInKB :=
      ChangeDatasizeUnit(
        BinaryToDenary(PhysicalDrive.IdentifyDeviceResult.UserSizeInKB),
        KBtoMB);

    DenaryInteger.FNumeralSystem := Denary;
    DenaryInteger.FPrecision := 0;

    lName.Caption :=
      PhysicalDrive.IdentifyDeviceResult.Model + ' ' +
      FormatSizeInMB(DenaryUserSizeInKB, DenaryInteger);

    lFirmware.Caption :=
      CapFirmware[CurrLang] + PhysicalDrive.IdentifyDeviceResult.Firmware;

    lConnState.Caption := CapConnState[CurrLang];
    if (PhysicalDrive.IdentifyDeviceResult.SATASpeed <=
        TSATASpeed.UnknownSATASpeed) then
      lConnState.Caption := lConnState.Caption + CapUnknown[CurrLang]
    else
    begin
      lConnState.Caption := lConnState.Caption +
        CapConnSpeed[Integer(PhysicalDrive.IdentifyDeviceResult.SATASpeed) - 2];

      NCQAvailability := PhysicalDrive.NCQAvailability;
      case NCQAvailability of

      TNCQAvailability.Unknown:
        lConnState.Caption := lConnState.Caption +
          CapUnknown[CurrLang];

      TNCQAvailability.Disabled:
        lConnState.Caption := lConnState.Caption +
          CapNonSupNCQ[CurrLang];

      TNCQAvailability.Enabled:
        lConnState.Caption := lConnState.Caption +
          CapSupportNCQ[CurrLang];

      end;
      lConnState.Caption := lConnState.Caption + ')';
    end;

    Query.Model := PhysicalDrive.IdentifyDeviceResult.Model;
    Query.Firmware := PhysicalDrive.IdentifyDeviceResult.Firmware;
    QueryResult := fMain.FirmwareGetter.CheckFirmware(Query);

    lNewFirm.Caption :=
      CapNewFirm[CurrLang] + QueryResult.LatestVersion;
    if QueryResult.CurrentVersion = TFirmwareVersion.OldVersion then
    begin
      lFirmware.Caption := lFirmware.Caption + CapOldVersion[CurrLang];
      lFirmware.Font.Color := clRed;
      lFirmware.Font.Style := [fsBold];
    end;

    if lName.Caption = '' then
    begin
      AlertCreate(fMain, AlrtNoSupport[CurrLang]);
      ShellExecute(Handle, 'open',
        PChar(TPathManager.AppPath + 'SSDTools.exe'),
        PChar('/diag'), nil, SW_SHOW);
    end;

    lSerial.Caption := CapSerial[CurrLang];
    if not ShowSerial then
      for CurrNum := 0 to
        Length(PhysicalDrive.IdentifyDeviceResult.Serial) - 1 do
          lSerial.Caption := lSerial.Caption + 'X'
    else
      lSerial.Caption := lSerial.Caption +
        PhysicalDrive.IdentifyDeviceResult.Serial;
  end;
end;

procedure ApplyHostWrite(PhysicalDrive: TPhysicalDrive);
var
  HostWriteInMiB: UInt64;
  CurrWritLog: TNSTLog;
  AvgDays, CurrAvgDay: Integer;
  BinaryPointOne: FormatSizeSetting;
  DatasizeUnitChangeSetting: TDatasizeUnitChangeSetting;
begin
  //통계 미지원 걸러냄
  if PhysicalDrive.SupportStatus.TotalWriteType =
    TTotalWriteType.WriteNotSupported then
      exit;

  with fMain do
  begin
    if l1Month.Visible = false then
    begin
      lHost.Top := lHost.Top - 25;
      lTodayUsage.Top := lTodayUsage.Top - 15;
      lOntime.Top := lOntime.Top - 10;
      l1Month.Visible := true;
    end;

    case PhysicalDrive.SupportStatus.TotalWriteType of

    //Case 1 : 호스트 쓰기 지원
    TTotalWriteType.WriteSupportedAsValue:
    begin
      HostWriteInMiB :=
        PhysicalDrive.SMARTInterpreted.TotalWrite.InValue.ValueInMiB;
      if PhysicalDrive.SMARTInterpreted.
         TotalWrite.InValue.TrueHostWriteFalseNANDWrite then
          lHost.Caption := CapHostWrite[CurrLang]
      else
        lHost.Caption := CapNandWrite[CurrLang];

      BinaryPointOne.FNumeralSystem := Binary;
      BinaryPointOne.FPrecision := 1;

      lHost.Caption :=
        lHost.Caption
        + FormatSizeInMB(HostWriteInMiB, BinaryPointOne);

      CurrWritLog :=
        TNSTLog.Create(
          TPathManager.AppPath, PhysicalDrive.IdentifyDeviceResult.Serial,
          UIntToStr(MBToLiteONUnit(HostWriteInMiB)), false);

      AvgDays := -1;
      for CurrAvgDay := AvgMax downto 0 do
      begin
        if Length(CurrWritLog.Average[IntToAvg[CurrAvgDay]]) > 0 then
        begin
          AvgDays := CurrAvgDay;
          break;
        end;
      end;

      if AvgDays >= 0 then
      begin
        if Length(CurrWritLog.Average[IntToAvg[AvgDays]]) > 0 then
        begin
          l1Month.Caption :=
            CapAvg[AvgDays][CurrLang] +
            CurrWritLog.Average[IntToAvg[AvgDays]] + 'GB/' + CapDay[CurrLang];
        end;
      end;
      lTodayUsage.Caption := CapToday[CurrLang] +
        CurrWritLog.TodayUsage + 'GB';
      FreeAndNil(CurrWritLog);
    end;

    TTotalWriteType.WriteSupportedAsCount:
    begin
      DatasizeUnitChangeSetting.FNumeralSystem := Binary;
      DatasizeUnitChangeSetting.FFromUnit := KiloUnit;
      DatasizeUnitChangeSetting.FFromUnit := MegaUnit;
      HostWriteInMiB :=
        PhysicalDrive.SMARTInterpreted.TotalWrite.InCount.ValueInCount *
        round(
          ChangeDatasizeUnit(
            PhysicalDrive.IdentifyDeviceResult.UserSizeInKB,
            DatasizeUnitChangeSetting));
      lHost.Caption := CapNandWrite[CurrLang] + UIntToStr(HostWriteInMiB);
      lTodayUsage.Caption :=
        CapWearLevel[CurrLang] +
        UIntToStr(PhysicalDrive.SMARTInterpreted.
          TotalWrite.InCount.ValueInCount);

      l1Month.Visible := false;
      lHost.Top := lHost.Top + 25;
      lTodayUsage.Top := lTodayUsage.Top + 15;
      lOntime.Top := lOntime.Top + 10;
    end;

    end;
  end;
end;

procedure ApplySectLog(PhysicalDrive: TPhysicalDrive);
var
  CurrSectLog: TNSTLog;
  ReplacedSectors: UInt64;
  AvgDays, CurrAvgDay: Integer;
begin
  try
    // 섹터 치환
    ReplacedSectors := PhysicalDrive.SMARTInterpreted.ReplacedSectors;

    CurrSectLog :=
      TNSTLog.Create(
        TPathManager.AppPath, PhysicalDrive.IdentifyDeviceResult.Serial +
        'RSLog', UIntToStr(ReplacedSectors), true);

    with fMain do
    begin
      //섹터 치환만 지원하는 모델들
      if PhysicalDrive.SupportStatus.TotalWriteType =
       TTotalWriteType.WriteNotSupported then
      begin
        lHost.Caption :=
          CapRepSect[CurrLang] + UIntToStr(ReplacedSectors) +
          CapCount[CurrLang];
        lAnalytics.Caption := BtLifeAnaly[CurrLang];
        lAnaly.Caption := CapLifeAnaly[CurrLang];

        AvgDays := -1;
        for CurrAvgDay := AvgMax downto 0 do
        begin
          if Length(CurrSectLog.Average[IntToAvg[CurrAvgDay]]) > 0 then
          begin
            AvgDays := CurrAvgDay;
            break;
          end;
        end;

        if AvgDays >= 0 then
        begin
          if Length(CurrSectLog.Average[IntToAvg[CurrAvgDay]]) > 0 then
          begin
            l1Month.Caption := CaprAvg[AvgDays][CurrLang] +
              CurrSectLog.Average[IntToAvg[CurrAvgDay]] + CapCount[CurrLang];
          end;
        end;
        lTodayUsage.Caption := CaprToday[CurrLang] + CurrSectLog.TodayUsage +
          CapCount[CurrLang];
      end
      else
      begin
        lAnalytics.Caption := BtAnaly[CurrLang];
        lAnaly.Caption := CapAnaly[CurrLang];
      end;
    end;

    with fMain do
    begin
      // 수명 상황 안 좋을때 오류 - 지우기 에러가 더 심각하므로 밑으로 배치.
      if PhysicalDrive.SMARTInterpreted.SMARTAlert.ReplacedSector then
      begin
        lSectors.Font.Color := clRed;
        lNotsafe.Font.Color := clRed;
        lSectors.Caption := CapRepSect[CurrLang] + UIntToStr(ReplacedSectors) +
          CapCount[CurrLang];
        lNotsafe.Caption := CapStatus[CurrLang] + CapNotSafeRepSect[CurrLang];
      end
      else
      begin
        lSectors.Caption := CapRepSect[CurrLang] + UIntToStr(ReplacedSectors) +
          CapCount[CurrLang];
      end;
    end;
  finally
    if CurrSectLog <> nil then
      FreeAndNil(CurrSectLog);
  end;
end;

procedure ApplySMARTInfo(PhysicalDrive: TPhysicalDrive);
var
  CurrDrvPartitions: TPartitionList;
  ReadEraseError: TReadEraseError;
  CurrPartition: Integer;
begin
  with fMain do
  begin
    lOntime.Caption :=
      CapPowerTime[CurrLang] +
      UIntToStr(PhysicalDrive.SMARTInterpreted.UsedHour) +
      CapHour[CurrLang];

    // 읽기/지우기 에러
    ReadEraseError := PhysicalDrive.SMARTInterpreted.ReadEraseError;
    if ReadEraseError.TrueReadErrorFalseEraseError then
      lPError.Caption := CapReadError[CurrLang]
    else
      lPError.Caption := CapWriteError[CurrLang];
    lPError.Caption := lPError.Caption + UIntToStr(ReadEraseError.Value) +
      CapCount[CurrLang];

    if PhysicalDrive.SMARTInterpreted.SMARTAlert.ReadEraseError then
    begin
      lPError.Font.Color := clRed;
      lNotsafe.Font.Color := clRed;
      lNotsafe.Caption := CapNotSafeEraseErrors[CurrLang] +
        CapNotSafeRepSect[CurrLang];
    end;

    // 파티션 정렬
    CurrDrvPartitions := PhysicalDrive.GetPartitionList;
    lPartitionAlign.Caption := CapAlign[CurrLang];
    for CurrPartition := 0 to (CurrDrvPartitions.Count - 1) do
    begin
      if (CurrDrvPartitions[CurrPartition].StartingOffset / 4096) =
          (CurrDrvPartitions[CurrPartition].StartingOffset div 4096) then
        lPartitionAlign.Caption := lPartitionAlign.Caption +
          CurrDrvPartitions[CurrPartition].Letter +
          CapGood[CurrLang]
      else
      begin
        lPartitionAlign.Font.Color := clRed;
        lPartitionAlign.Caption := lPartitionAlign.Caption +
          CurrDrvPartitions[CurrPartition].Letter +
          ' (' +
          IntToStr(
            CurrDrvPartitions[CurrPartition].StartingOffset div 1024) +
          CapBad[CurrLang];
        if lNotsafe.Caption = CapStatus[CurrLang] + CapSafe[CurrLang] then
        begin
          lNotSafe.Font.Color := clRed;
          lNotsafe.Caption := CapStatus[CurrLang] + CapBadPartition[CurrLang];
        end;
      end;
    end;
    FreeAndNil(CurrDrvPartitions);
  end;
end;

procedure ApplyGeneralUISetting(PhysicalDrive: TPhysicalDrive);
begin
  with fMain do
  begin
    lTrim.Visible := false;
    iTrim.Visible := false;
    iFirmUp.Visible := false;
    lFirmUp.Visible := false;

    if PhysicalDrive.IdentifyDeviceResult.IsDataSetManagementSupported then
    begin
      lTrim.Visible := true;
      iTrim.Visible := true;
    end;

    if (PhysicalDrive.SupportStatus.FirmwareUpdate = false) and
       (iTrim.Visible = false) and
       (iOptimize.Left = FirstiOptLeft) then
    begin
      iOptimize.left := iErase.Left;
      lOptimize.left := iOptimize.Left + (iOptimize.Width div 2)
                                        - (lOptimize.Width div 2);

      iErase.left := iFirmUp.Left;
      lErase.left := iErase.Left + (iErase.Width div 2)
                                 - (lErase.Width div 2);
    end
    else
    begin
      iFirmUp.Visible := true;
      lFirmUp.Visible := true;

      if iOptimize.left <> FirstiOptLeft then
      begin
        iErase.left := iOptimize.Left;
        lErase.left := iErase.Left + (iErase.Width div 2)
                                   - (lErase.Width div 2);

        iOptimize.left := FirstiOptLeft;
        lOptimize.left := iOptimize.Left + (iOptimize.Width div 2)
                                         - (lOptimize.Width div 2);
      end;
    end;
  end;
end;

procedure SetLabelPosition;
var
  CurrLabel: Integer;
begin
  with fMain do
  begin
    for CurrLabel := 0 to SSDLabel.Count - 1 do
    begin
      SSDLabel[CurrLabel].Top :=
        (5 * ((CurrLabel + 1) mod 11)) +
        (SSDLabel[CurrLabel].Height * (CurrLabel mod 11));
      SSDLabel[CurrLabel].Left :=
        10 + (((CurrLabel + 1) div 11) * 260);
    end;

    GSSDSel.Height :=
      SSDLabel[SSDLabel.Count - 1].Top +
      SSDLabel[SSDLabel.Count - 1].Height + 5;
  end;
end;

procedure DelDevice(SSDEntry: TPhysicalDrive);
var
  IndexOfEntry: Integer;
begin
  with fMain do
  begin
    IndexOfEntry := SSDLabel.IndexOf(SSDEntry);
    if IndexOfEntry = -1 then
      exit;

    SSDLabel[IndexOfEntry].Free;
    SSDLabel.Delete(IndexOfEntry);
  end;
end;

procedure AddDevice(SSDEntry: TPhysicalDrive);
var
  CurrDrvPartitions: TPartitionList;
  NewLen: Integer;
  CurrPartition: Integer;

  CurrPhysicalDrive: TPhysicalDrive;
  DenaryInteger: FormatSizeSetting;
  DenaryByteToMB: TDatasizeUnitChangeSetting;
  DiskSizeInMB: Double;
begin
  with fMain do
  begin
    NewLen := SSDLabel.Count;
    SSDLabel.Add(TSSDLabel.Create(gSSDSel));
    SSDLabel[NewLen].Parent := gSSDSel;
    SSDLabel[NewLen].DeviceInfo :=
      TPhysicalDrive.Create(
        StrToInt(SSDEntry.GetPathOfFileAccessingWithoutPrefix));

    SSDLabel[NewLen].Font.Name := Font.Name;
    SSDLabel[NewLen].Font.Size := 10;
    SSDLabel[NewLen].Cursor := crHandPoint;
    SSDLabel[NewLen].OnClick := SSDLabelClick;
    SSDLabel[NewLen].OnMouseEnter := SSDSelLblMouseEnter;
    SSDLabel[NewLen].OnMouseLeave := SSDSelLblMouseLeave;

    CurrPhysicalDrive := TPhysicalDrive.Create
      (StrToInt(SSDEntry.GetPathOfFileAccessingWithoutPrefix));

    if NewLen > 9 then
    begin
      GSSDSel.Width := 590;
      GSSDSel.Left := 8;
    end
    else
    begin
      GSSDSel.Width := 335;
      GSSDSel.Left := 260;
    end;

    CurrDrvPartitions := SSDEntry.GetPartitionList;

    DenaryByteToMB.FNumeralSystem := Denary;
    DenaryByteToMB.FFromUnit := ByteUnit;
    DenaryByteToMB.FToUnit := MegaUnit;

    DiskSizeInMB :=
      ChangeDatasizeUnit(SSDEntry.DiskSizeInByte, DenaryByteToMB);

    DenaryInteger.FNumeralSystem := Denary;
    DenaryInteger.FPrecision := 0;

    SSDLabel[NewLen].Caption :=
      SSDLabel[NewLen].Caption +
      CurrPhysicalDrive.IdentifyDeviceResult.Model + ' ' +
      FormatSizeInMB(DiskSizeInMB, DenaryInteger);

    for CurrPartition := 0 to (CurrDrvPartitions.Count - 1) do
    begin
      if CurrPartition = 0 then
        SSDLabel[NewLen].Caption :=
          SSDLabel[NewLen].Caption + '(';

      SSDLabel[NewLen].Caption :=
        SSDLabel[NewLen].Caption
          + CurrDrvPartitions[CurrPartition].Letter;

      if CurrPartition < (CurrDrvPartitions.Count - 1) then
        SSDLabel[NewLen].Caption := SSDLabel[NewLen].Caption
                                          + ' '
      else
        SSDLabel[NewLen].Caption := SSDLabel[NewLen].Caption
                                          + ') ';
    end;

    FreeAndNil(CurrDrvPartitions);
    FreeAndNil(CurrPhysicalDrive);
  end;

  SetLabelPosition;
end;

procedure AddByList(SSDList: TPhysicalDriveList);
var
  CurrEntry: TPhysicalDrive;
begin
  for CurrEntry in SSDList do
    AddDevice(CurrEntry);
end;

procedure DelByList(SSDList: TPhysicalDriveList);
var
  CurrEntry: TPhysicalDrive;
begin
  for CurrEntry in SSDList do
    DelDevice(CurrEntry);
end;

function RefreshTimer(ShowSerial: Boolean;
                      FirstiOptLeft: Integer): Boolean;
begin
  result := true;

  fMain.InitUIToRefresh;

  if Length(fMain.CurrDrive) = 0 then
    exit;

  FreeAndNil(fMain.PhysicalDrive);
  fMain.PhysicalDrive := TPhysicalDrive.Create(StrToInt(fMain.CurrDrive));

  ApplyBasicInfo(fMain.PhysicalDrive, ShowSerial);
  ApplyHostWrite(fMain.PhysicalDrive);
  ApplySectLog(fMain.PhysicalDrive);
  ApplySMARTInfo(fMain.PhysicalDrive);
  ApplyGeneralUISetting(fMain.PhysicalDrive);
end;

procedure RefreshDrives(PhysicalDrive: TPhysicalDrive);
var
  TrvResult: TDiffResult;
begin
  TrvResult := TraverseDevice(true, true, fMain.PhysicalDriveList);

  if fMain.PhysicalDriveList.Count = 0 then
  begin
    AlertCreate(fMain, AlrtNoSupport[CurrLang]);
    ShellExecute(fMain.Handle, 'open',
      PChar(TPathManager.AppPath + 'SSDTools.exe'),
      PChar('/diag'), nil, SW_SHOW);

    FreeAndNil(TrvResult.AddList);
    FreeAndNil(TrvResult.DelList);
    exit;
  end;

  if TrvResult.DelList.Count > 0 then
    DelByList(TrvResult.DelList);

  if TrvResult.AddList.Count > 0 then
    AddByList(TrvResult.AddList);

  if (fMain.PhysicalDriveList.Count > 0) and
     (fMain.CurrDrive = '') then
    fMain.SSDLabel[0].OnClick(fMain.SSDLabel[0]);

  FreeAndNil(TrvResult.AddList);
  FreeAndNil(TrvResult.DelList);
end;

procedure TSSDLabelList.Delete(Index: Integer);
begin
  Self[Index].DeviceInfo.Free;
  Self[Index].DeviceInfo := nil;
  inherited Delete(Index);
end;

destructor TSSDLabelList.Destroy;
var
  CurrentItem: Integer;
begin
  for CurrentItem := 0 to Count - 1 do
    Delete(0);
  inherited;
end;

function TSSDLabelList.IndexOf(Entry: TPhysicalDrive): Integer;
var
  CurrEntry: Integer;
begin
  for CurrEntry := 0 to Count - 1 do
    if self[CurrEntry].DeviceInfo.GetPathOfFileAccessingWithoutPrefix =
       Entry.GetPathOfFileAccessingWithoutPrefix then
      break;

  if CurrEntry < Count then
    exit(CurrEntry)
  else
    exit(-1);
end;

end.

