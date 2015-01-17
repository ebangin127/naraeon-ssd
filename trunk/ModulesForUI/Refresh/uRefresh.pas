unit uRefresh;

interface

uses
  Classes, SysUtils, Math, Vcl.Controls, Vcl.Graphics, Vcl.StdCtrls, Windows,
  uAlert, uLanguageSettings, ShellApi, Dialogs,
  uDiskFunctions, uPartitionFunctions, uSSDInfo, uSSDSupport, uRegFunctions,
  uSMARTFunctions, uStrFunctions, uLogSystem, uGetFirm;

function RefreshTimer(SSDInfo: TSSDInfo_NST;
                      CurrUSBMode: Boolean;
                      CurrATAorSCSIStatus: TStorInterface;
                      ShowSerial: Boolean;
                      FirstiOptLeft: Integer): Boolean;
function RefreshDrives(SSDInfo: TSSDInfo_NST): Integer;

type
  TSSDLabel = class(TLabel)
  public
    DriveName: String;
    USBMode: Boolean;
    ATAorSCSI: TStorInterface;
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
    else if CurrUSBMode then
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
      ShellExecute(Handle, 'open', PChar(AppPath + 'SSDTools.exe'),
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
          AppPath, SSDInfo.Serial, UIntToStr(HostWrites),
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
    CurrSectLog :=
      TNSTLog.Create(
        AppPath, SSDInfo.Serial + 'RSLog',
        UIntToStr(ReplacedSectors), true, false);

    // 섹터 치환
    ReplacedSectors := SSDInfo.ReplacedSectors;

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
  ReplacedSectors: UInt64;
  CurrDrvPartitions: TDriveLetters;
  CurrPartition, AvgDays, CurrAvgDay: Integer;
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

procedure AddDevice(DeviceName: String; Model: String;
  DriveName: String; ATAorSCSI: TStorInterface; USBMode: Boolean);
var
  NewLen: Integer;
  CurrDrvPartitions: TDriveLetters;
  CurrPartition: Integer;
  Partlen: Integer;
begin
  with fMain do
  begin
    SetLength(SSDLabel, Length(SSDLabel) + 1);
    NewLen := Length(SSDLabel);

    SSDLabel[NewLen - 1] := TSSDLabel.Create(GSSDSel);
    SSDLabel[NewLen - 1].Parent := GSSDSel;
    SSDLabel[NewLen - 1].Font.Name := Font.Name;
    SSDLabel[NewLen - 1].Font.Size := 10;
    SSDLabel[NewLen - 1].DriveName :=  DriveName;
    SSDLabel[NewLen - 1].ATAorSCSI := ATAorSCSI;
    SSDLabel[NewLen - 1].Cursor := crHandPoint;
    SSDLabel[NewLen - 1].OnClick := SSDLabelClick;
    SSDLabel[NewLen - 1].OnMouseEnter := SSDSelLblMouseEnter;
    SSDLabel[NewLen - 1].OnMouseLeave := SSDSelLblMouseLeave;
    SSDLabel[NewLen - 1].Top := (5 * (NewLen mod 11)) +
                                 (SSDLabel[NewLen - 1].Height
                                 * ((NewLen - 1) mod 11));
    SSDLabel[NewLen - 1].Left := 10 + ((NewLen div 11) * 260);

    if NewLen > 10 then
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
      GetPartitionList(ExtractDeviceNum(DeviceName));

    Partlen := 15 * ceil(CurrDrvPartitions.LetterCount / 3);

    SSDLabel[NewLen - 1].Font.Style := [fsBold];
    SSDLabel[NewLen - 1].Font.Style := [];

    SSDLabel[NewLen - 1].USBMode := USBMode;
    SSDLabel[NewLen - 1].Caption :=
      SSDLabel[NewLen - 1].Caption + Model + ' ' +
      GetTBStr(1000, GetDiskSize(DriveName) / 1000 / 1000, 0);

    for CurrPartition := 0 to (CurrDrvPartitions.LetterCount - 1) do
    begin
      if CurrPartition = 0 then
        SSDLabel[NewLen - 1].Caption :=
          SSDLabel[NewLen - 1].Caption + '(';

      SSDLabel[NewLen - 1].Caption :=
        SSDLabel[NewLen - 1].Caption
          + CurrDrvPartitions.Letters[CurrPartition];

      if CurrPartition < (CurrDrvPartitions.LetterCount - 1) then
        SSDLabel[NewLen - 1].Caption := SSDLabel[NewLen - 1].Caption
                                          + ' '
      else
        SSDLabel[NewLen - 1].Caption := SSDLabel[NewLen - 1].Caption
                                          + ') ';
    end;
  end;
end;

function RefreshTimer(SSDInfo: TSSDInfo_NST;
                      CurrUSBMode: Boolean;
                      CurrATAorSCSIStatus: TStorInterface;
                      ShowSerial: Boolean;
                      FirstiOptLeft: Integer): Boolean;
begin
  result := true;

  fMain.InitUIToRefresh;

  if Length(fMain.CurrDrive) = 0 then
    exit;

  SSDInfo.ATAorSCSI := CurrATAorSCSIStatus;
  SSDInfo.USBMode := CurrUSBMode;
  SSDInfo.SetDeviceName(StrToInt(fMain.CurrDrive));

  ApplyBasicInfo(SSDInfo, ShowSerial);
  ApplyHostWrite(SSDInfo);
  ApplySectLog(SSDInfo);
  ApplySMARTInfo(SSDInfo);
  ApplyGeneralUISetting(SSDInfo);
end;

function RefreshDrives(SSDInfo: TSSDInfo_NST): integer;
var
  TempSSDInfo: TSSDInfo_NST;
  AllDrv: TStringList;
  CurrDrv, CurrExistAtApp, SelectedDrv: Integer;
  CurrAvail, RefreshAll: Boolean;
  TempFound: Boolean;
  ATAorSCSI: Boolean;
  RobustMode: Boolean;
  TempUSBMode: Boolean;
  isDriveAccessible: Boolean;
  NewLen: Integer;

  GetSSDResult: TSSDListResult;
  DrvName: String;
begin
  TempSSDInfo := TSSDInfo_NST.Create;
  SelectedDrv := 0;
  if Length(fMain.SSDLabel) > 0 then
    SelectedDrv := StrToInt(ExtractDeviceNum(SSDInfo.DeviceName));

  GetSSDResult := GetSSDList;
  AllDrv := GetSSDResult.ResultList;
  RobustMode := not GetSSDResult.WMIEnabled;

  ATAorSCSI := false;
  RefreshAll := false;

  for CurrDrv := 0 to AllDrv.Count - 1 do
  begin
    if (AllDrv[CurrDrv] = '/') or (AllDrv[CurrDrv] = '') then
    begin
      ATAorSCSI := SCSIMode;
      continue;
    end;

    CurrAvail := false;
    if (AllDrv[CurrDrv][Length(AllDrv[CurrDrv])] <> 'U') and
       (AllDrv[CurrDrv][Length(AllDrv[CurrDrv])] <> 'H') then
      DrvName := AllDrv[CurrDrv]
    else
      DrvName := Copy(AllDrv[CurrDrv], 0, Length(AllDrv[CurrDrv]) - 1);

    with fMain do
    begin
      for CurrExistAtApp := 0 to Length(SSDLabel) - 1 do
        if AllDrv[CurrDrv] = SSDLabel[CurrExistAtApp].DriveName then
          CurrAvail := true;
    end;

    if ATAorSCSI = ATAMode then TempSSDInfo.ATAorSCSI := MODEL_ATA
    else if ATAorSCSI = SCSIMode then TempSSDInfo.ATAorSCSI := MODEL_SCSI;

    if RobustMode then
    begin
      TempSSDInfo.ATAorSCSI := MODEL_DETERMINE;
    end;
    TempSSDInfo.SetDeviceName(StrToInt(DrvName));

    if (TempSSDInfo.SupportedDevice <> SUPPORT_NONE) and
       (CurrAvail = false) then
    begin
      TempUSBMode := false;
      if (AllDrv[CurrDrv][Length(AllDrv[CurrDrv])] = 'U')
          or (AllDrv[CurrDrv][Length(AllDrv[CurrDrv])] = 'H') then
      begin
        if AllDrv[CurrDrv][Length(AllDrv[CurrDrv])] = 'U' then
          TempUSBMode := true;
        AllDrv[CurrDrv] :=
          Copy(AllDrv[CurrDrv], 0, Length(AllDrv[CurrDrv]) - 1);
      end;

      AddDevice(TempSSDInfo.DeviceName, TempSSDInfo.Model, AllDrv[CurrDrv],
        TempSSDInfo.ATAorSCSI, TempUSBMode);
    end;
    with fMain do
    begin
      if lName.Caption = '' then
      begin
        NewLen := Length(SSDLabel);
        SSDLabel[NewLen - 1].OnClick(SSDLabel[NewLen - 1]);
        tRefresh.Enabled := true;
        if ATAorSCSI = ATAMode then CurrATAorSCSIStatus := MODEL_ATA
        else if ATAorSCSI = SCSIMode then CurrATAorSCSIStatus := MODEL_SCSI;
      end;
    end;
  end;

  with fMain do
  begin
    for CurrExistAtApp := 0 to NewLen - 1 do
    begin
      TempFound := false;
      for CurrDrv := 0 to AllDrv.Count - 1 do
      begin
        if (SSDLabel[CurrExistAtApp].DriveName = AllDrv[CurrDrv]) or
           (SSDLabel[CurrExistAtApp].DriveName + 'U' = AllDrv[CurrDrv]) or
           (SSDLabel[CurrExistAtApp].DriveName + 'H' = AllDrv[CurrDrv]) then
          TempFound := true
        else if (CurrDrv = (AllDrv.Count - 1)) and (TempFound = false) then
          RefreshAll := true;
      end;
    end;
  end;

  with fMain do
  begin
    if lName.Caption = '' then
    begin
      AlertCreate(fMain, AlrtNoSupport[CurrLang]);
      ShellExecute(Handle, 'open', PChar(AppPath + 'SSDTools.exe'),
                  PChar('/diag'), nil, SW_SHOW);
    end;
  end;
  FreeAndNil(AllDrv);

  with fMain do
  begin
    if RefreshAll then
    begin
      for CurrExistAtApp := 0 to NewLen - 1 do
      begin
        FreeAndNil(SSDLabel[CurrExistAtApp]);
      end;

      SetLength(SSDLabel, 0);

      lName.Caption := '';
      CurrDrive := '100';
      RefreshDrives(SSDInfo);

      tRefresh.Enabled := true;
    end;

    if Length(SSDLabel) > 0 then
    begin
      GSSDSel.Height := SSDLabel[Length(SSDLabel) - 1].Top
        + SSDLabel[Length(SSDLabel) - 1].Height + 5;
    end;
  end;

  FreeAndNil(TempSSDInfo);
  result := SelectedDrv;
end;
end.

