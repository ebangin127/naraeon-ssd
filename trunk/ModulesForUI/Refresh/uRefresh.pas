unit uRefresh;

interface

uses
  Classes, SysUtils, Math, Vcl.Controls, Vcl.Graphics, Vcl.StdCtrls, Windows,
  uAlert, uLanguageSettings, ShellApi, Dialogs, Generics.Collections,
  uDiskFunctions, uPartitionFunctions, uSSDInfo, uSSDSupport, uRegFunctions,
  uSMARTFunctions, uStrFunctions, uLogSystem, uGetFirm, uSSDList, uPathManager;

function RefreshTimer(SSDInfo: TSSDInfo_NST;
                      ShowSerial: Boolean;
                      FirstiOptLeft: Integer): Boolean;
procedure RefreshDrives(SSDInfo: TSSDInfo_NST);

type
  TSSDLabel = class(TLabel)
  public
    DeviceInfo: TSSDEntry;
  end;

  TSSDLabelList = class(TList<TSSDLabel>)
  public
    function IndexOf(Entry: TSSDEntry): Integer;
  end;

implementation

uses uMain;

procedure ApplyBasicInfo(SSDInfo: TSSDInfo_NST;
  ShowSerial: Boolean);
var
  CurrNum: Integer;
begin
  with fMain do
  begin
    lName.Caption :=
      SSDInfo.Model + ' ' +
      GetTBStr(1000, SSDInfo.UserSize / 2 * (512/500) / 1000, 0);
    lFirmware.Caption :=
      CapFirmware[CurrLang] + SSDInfo.Firmware;

    lConnState.Caption := CapConnState[CurrLang];
    if (SSDInfo.SATASpeed = SPEED_UNKNOWN) or
       (SSDInfo.SATASpeed > SPEED_SATA600) then
      lConnState.Caption := lConnState.Caption + CapUnknown[CurrLang]
    else if SSDInfo.USBMode then
      lConnState.Caption := lConnState.Caption + ConnState[3]
    else
    begin
      lConnState.Caption := lConnState.Caption +
        ConnState[Integer(SSDInfo.SATASpeed) - 1];
      case SSDInfo.NCQSupport of
      0: lConnState.Caption :=
          lConnState.Caption + CapUnknown[CurrLang];
      1: lConnState.Caption :=
          lConnState.Caption + CapNonSupNCQ[CurrLang];
      2: lConnState.Caption :=
          lConnState.Caption + CapSupportNCQ[CurrLang];
      end;
      lConnState.Caption := lConnState.Caption + ')';
    end;

    lNewFirm.Caption := NewFirmCaption(SSDInfo.Model, SSDInfo.Firmware);
    if IsNewVersion(SSDInfo.Model, SSDInfo.Firmware) = OLD_VERSION then
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
      for CurrNum := 0 to Length(SSDInfo.Serial) - 1 do
        lSerial.Caption := lSerial.Caption + 'X'
    else
      lSerial.Caption := lSerial.Caption + SSDInfo.Serial;
  end;
end;

procedure ApplyHostWrite(SSDInfo: TSSDInfo_NST);
var
  HostWrites: UInt64;
  CurrWritLog: TNSTLog;
  AvgDays, CurrAvgDay: Integer;
begin
  SSDInfo.CollectAllSmartData;
  HostWrites := SSDInfo.HostWrites;

  //통계 미지원 걸러냄
  if SSDInfo.SSDSupport.SupportHostWrite = HSUPPORT_NONE then
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

    case SSDInfo.SSDSupport.SupportHostWrite of

    //Case 1 : 호스트 쓰기 지원
    HSUPPORT_FULL:
    begin
      if SSDInfo.IsHostWrite then
        lHost.Caption := CapHostWrite[CurrLang]
      else
        lHost.Caption := CapNandWrite[CurrLang];

      lHost.Caption :=
        lHost.Caption
        + GetTBStr(1024, HostWrites / 10.24 * 0.64 * 1024, 1);

      CurrWritLog :=
        TNSTLog.Create(
          TPathManager.AppPath, SSDInfo.Serial, UIntToStr(HostWrites),
          false, SSDInfo.S10085);

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

    HSUPPORT_COUNT:
      begin
        lHost.Caption := CapSSDLifeLeft[CurrLang]
          + UIntToStr(
              ExtractSMARTPercent(SSDInfo.SMARTData, 1)) + '%';
        lTodayUsage.Caption := CapWearLevel[CurrLang]
          + UIntToStr(ExtractSMART(SSDInfo.SMARTData, 'AD'));

        l1Month.Visible := false;
        lHost.Top := lHost.Top + 25;
        lTodayUsage.Top := lTodayUsage.Top + 15;
        lOntime.Top := lOntime.Top + 10;
      end;
    end;

    if SSDInfo.S10085 then
    begin
      lHost.Caption := lHost.Caption + CapCannotTrust[CurrLang];
      lHost.Font.Color := clRed;
      lHost.Font.Style := [fsBold];
    end
    else
    begin
      lHost.Font.Color := clWindowText;
      lHost.Font.Style := [];
    end;
  end;
end;

procedure ApplySectLog(SSDInfo: TSSDInfo_NST);
var
  CurrSectLog: TNSTLog;
  ReplacedSectors: UInt64;
  AvgDays, CurrAvgDay: Integer;
begin
  try
    // 섹터 치환
    ReplacedSectors := SSDInfo.ReplacedSectors;

    CurrSectLog :=
      TNSTLog.Create(
        TPathManager.AppPath, SSDInfo.Serial + 'RSLog',
        UIntToStr(ReplacedSectors), true, false);

    with fMain do
    begin
      //섹터 치환만 지원하는 모델들
      if SSDInfo.SSDSupport.SupportHostWrite = HSUPPORT_NONE then
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
      if SSDInfo.RepSectorAlert then
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

procedure ApplySMARTInfo(SSDInfo: TSSDInfo_NST);
var
  EraseErrors: UInt64;
  CurrDrvPartitions: TDriveLetters;
  CurrPartition: Integer;
begin
  with fMain do
  begin
    lOntime.Caption :=
      CapPowerTime[CurrLang] +
      UIntToStr(ExtractSMART(SSDInfo.SMARTData, 9) and $FFFFFFFF) +
      CapHour[CurrLang];

    // 지우기 에러
    EraseErrors := SSDInfo.EraseError;
    if SSDInfo.SSDSupport.SupportHostWrite = HSUPPORT_NONE then
    begin
      lPError.Caption := CapReadError[CurrLang] + UIntToStr(EraseErrors) +
        CapCount[CurrLang];
    end
    else
    begin
      lPError.Caption := CapWriteError[CurrLang] + UIntToStr(EraseErrors) +
        CapCount[CurrLang];
    end;

    if SSDInfo.EraseErrorAlert then
    begin
      lPError.Font.Color := clRed;
      lNotsafe.Font.Color := clRed;
      lNotsafe.Caption := CapNotSafeEraseErrors[CurrLang] +
        CapNotSafeRepSect[CurrLang];
    end;

    // 파티션 정렬
    CurrDrvPartitions := GetPartitionList(CurrDrive);
    lPartitionAlign.Caption := CapAlign[CurrLang];
    for CurrPartition := 0 to (CurrDrvPartitions.LetterCount - 1) do
    begin
      if (CurrDrvPartitions.StartOffset[CurrPartition - 1] / 4096) =
          (CurrDrvPartitions.StartOffset[CurrPartition - 1] div 4096) then
        lPartitionAlign.Caption := lPartitionAlign.Caption +
                                    CurrDrvPartitions.Letters[CurrPartition] +
                                    CapGood[CurrLang]
      else
      begin
        lPartitionAlign.Font.Color := clRed;
        lPartitionAlign.Caption := lPartitionAlign.Caption +
          CurrDrvPartitions.Letters[CurrPartition] +
          ' (' +
          IntToStr(CurrDrvPartitions.StartOffset[CurrPartition - 1] div 1024) +
          CapBad[CurrLang];
        if lNotsafe.Caption = CapStatus[CurrLang] + CapSafe[CurrLang] then
        begin
          lNotSafe.Font.Color := clRed;
          lNotsafe.Caption := CapStatus[CurrLang] + CapBadPartition[CurrLang];
        end;
      end;
    end;
  end;
end;

procedure ApplyGeneralUISetting(SSDInfo: TSSDInfo_NST);
var
  CurrDrvPartitions: TDriveLetters;
begin
  with fMain do
  begin
    lTrim.Visible := false;
    iTrim.Visible := false;
    iFirmUp.Visible := false;
    lFirmUp.Visible := false;

    // 파티션 정렬
    CurrDrvPartitions := GetPartitionList(CurrDrive);
    if SSDInfo.ATAorSCSI = MODEL_ATA then
    begin
      if Length(CurrDrvPartitions.Letters) <> 0 then
      begin
        lTrim.Visible := true;
        iTrim.Visible := true;
      end;
    end;

    if (SSDInfo.SSDSupport.SupportFirmUp = false) and
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

procedure DelDevice(SSDEntry: TSSDEntry);
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

procedure AddDevice(SSDEntry: TSSDEntry);
var
  NewLen: Integer;
  CurrDrvPartitions: TDriveLetters;
  CurrPartition: Integer;

  CurrSSDInfo: TSSDInfo;
begin
  with fMain do
  begin
    NewLen := SSDLabel.Count;

    SSDLabel.Add(TSSDLabel.Create(GSSDSel));

    SSDLabel[NewLen].Parent := GSSDSel;
    SSDLabel[NewLen].Font.Name := Font.Name;
    SSDLabel[NewLen].Font.Size := 10;
    SSDLabel[NewLen].DeviceInfo := SSDEntry;
    SSDLabel[NewLen].Cursor := crHandPoint;
    SSDLabel[NewLen].OnClick := SSDLabelClick;
    SSDLabel[NewLen].OnMouseEnter := SSDSelLblMouseEnter;
    SSDLabel[NewLen].OnMouseLeave := SSDSelLblMouseLeave;

    CurrSSDInfo := TSSDInfo.Create;
    CurrSSDInfo.SetDeviceName(StrToInt(SSDEntry.DeviceName));

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

    CurrDrvPartitions :=
      GetPartitionList(SSDEntry.DeviceName);

    SSDLabel[NewLen].Font.Style := [fsBold];
    SSDLabel[NewLen].Font.Style := [];

    SSDLabel[NewLen].Caption :=
      SSDLabel[NewLen].Caption + CurrSSDInfo.Model + ' ' +
      GetTBStr(1000, GetDiskSize(SSDEntry.DeviceName) / 1000 / 1000, 0);

    for CurrPartition := 0 to (CurrDrvPartitions.LetterCount - 1) do
    begin
      if CurrPartition = 0 then
        SSDLabel[NewLen].Caption :=
          SSDLabel[NewLen].Caption + '(';

      SSDLabel[NewLen].Caption :=
        SSDLabel[NewLen].Caption
          + CurrDrvPartitions.Letters[CurrPartition];

      if CurrPartition < (CurrDrvPartitions.LetterCount - 1) then
        SSDLabel[NewLen].Caption := SSDLabel[NewLen].Caption
                                          + ' '
      else
        SSDLabel[NewLen].Caption := SSDLabel[NewLen].Caption
                                          + ') ';
    end;

    FreeAndNil(CurrSSDInfo);
  end;

  SetLabelPosition;
end;

procedure AddByList(SSDList: TSSDList);
var
  CurrEntry: TSSDEntry;
begin
  for CurrEntry in SSDList do
    AddDevice(CurrEntry);
end;

procedure DelByList(SSDList: TSSDList);
var
  CurrEntry: TSSDEntry;
begin
  for CurrEntry in SSDList do
    DelDevice(CurrEntry);
end;

function RefreshTimer(SSDInfo: TSSDInfo_NST;
                      ShowSerial: Boolean;
                      FirstiOptLeft: Integer): Boolean;
begin
  result := true;

  fMain.InitUIToRefresh;

  if Length(fMain.CurrDrive) = 0 then
    exit;

  SSDInfo.SetDeviceName(StrToInt(fMain.CurrDrive));

  ApplyBasicInfo(SSDInfo, ShowSerial);
  ApplyHostWrite(SSDInfo);
  ApplySectLog(SSDInfo);
  ApplySMARTInfo(SSDInfo);
  ApplyGeneralUISetting(SSDInfo);
end;

procedure RefreshDrives(SSDInfo: TSSDInfo_NST);
var
  TrvResult: TDiffResult;
begin
  TrvResult := TraverseDevice(true, true, fMain.SSDList);

  if fMain.SSDList.Count = 0 then
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

  if (fMain.SSDList.Count > 0) and
     (fMain.CurrDrive = '') then
    fMain.SSDLabel[0].OnClick(fMain.SSDLabel[0]);

  FreeAndNil(TrvResult.AddList);
  FreeAndNil(TrvResult.DelList);
end;

function TSSDLabelList.IndexOf(Entry: TSSDEntry): Integer;
var
  CurrEntry: Integer;
begin
  for CurrEntry := 0 to Count - 1 do
    if (self[CurrEntry].DeviceInfo.DeviceName =
        Entry.DeviceName) and
       (self[CurrEntry].DeviceInfo.IsUSBDevice =
        Entry.IsUSBDevice) then
      break;

  if CurrEntry < Count then
    exit(CurrEntry)
  else
    exit(-1);
end;

end.

