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
                      firstiOptLeft: Integer): Boolean;
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

function RefreshTimer(SSDInfo: TSSDInfo_NST;
                      CurrUSBMode: Boolean;
                      CurrATAorSCSIStatus: TStorInterface;
                      ShowSerial: Boolean;
                      firstiOptLeft: Integer): Boolean;
var
  HostWrites: UInt64;
  ReplacedSectors: UInt64;
  EraseErrors: UInt64;
  CurrDrvPartitions: TDriveLetters;
  CurrPartition, CurrNum, AvgDays, CurrAvgDay: Integer;
  CurrWritLog: TNSTLog;
  CurrSectLog: TNSTLog;
begin
  result := true;

  with fMain do
  begin
    InitUIToRefresh;

    if Length(CurrDrive) = 0 then
    begin
      exit;
    end;

    SSDInfo.ATAorSCSI := CurrATAorSCSIStatus;
    SSDInfo.USBMode := CurrUSBMode;
    SSDInfo.SetDeviceName(StrToInt(CurrDrive));

    lName.Caption := SSDInfo.Model + ' '
                     + GetTBStr(1000, SSDInfo.UserSize / 2 * (512/500) / 1000,
                                0);
    lFirmware.Caption := CapFirmware[CurrLang] + SSDInfo.Firmware;

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
        0:
        begin
          lConnState.Caption := lConnState.Caption + CapUnknown[CurrLang];
        end;
        1:
        begin
          lConnState.Caption := lConnState.Caption + CapNonSupNCQ[CurrLang];
        end;
        2:
        begin
          lConnState.Caption := lConnState.Caption + CapSupportNCQ[CurrLang];
        end;
      end;
      lConnState.Caption := lConnState.Caption + ')';
    end;

    if IsNewVersion(SSDInfo.Model, SSDInfo.Firmware) = OLD_VERSION then
    begin
      lFirmware.Caption := lFirmware.Caption + CapOldVersion[CurrLang];
      lFirmware.Font.Color := clRed;
      lFirmware.Font.Style := [fsBold];
    end;
    if lName.Caption <> '' then
    begin
      if (cUSB.Items.Count > 0) and (cUSB.ItemIndex = -1) then
        cUSB.ItemIndex := 0;
      if (cUSBErase.Items.Count > 0) and (cUSBErase.ItemIndex = -1) then
        cUSBErase.ItemIndex := 0;
      if (cTrimList.Items.Count > 0) and (cTrimList.ItemIndex = -1) then
        cTrimList.ItemIndex := 0;

      if gFirmware.Visible = false then
        lNewFirm.Caption := NewFirmCaption(SSDInfo.Model, SSDInfo.Firmware);
    end
    else
    begin
      AlertCreate(fMain, AlrtNoSupport[CurrLang]);
      ShellExecute(Handle, 'open', PChar(AppPath + 'SSDTools.exe'),
                  PChar('/diag'), nil, SW_SHOW);
    end;
    if ShowSerial = false then
    begin
      lSerial.Caption := CapSerial[CurrLang];
      for CurrNum := 0 to Length(SSDInfo.Serial) - 1 do
        lSerial.Caption := lSerial.Caption + 'X';
    end
    else
      lSerial.Caption := CapSerial[CurrLang] + SSDInfo.Serial;

    SSDInfo.CollectAllSmartData;
    HostWrites := SSDInfo.HostWrites;

    //통계 미지원 걸러냄
    if SSDInfo.SSDSupport.SupportHostWrite <> HSUPPORT_NONE then
    begin
      if l1Month.Visible = false then
      begin
        lHost.Top := lHost.Top - 25;
        lTodayUsage.Top := lTodayUsage.Top - 15;
        lOntime.Top := lOntime.Top - 10;
        l1Month.Visible := true;
      end;

      //Case 1 : 호스트 쓰기 지원
      if SSDInfo.SSDSupport.SupportHostWrite = HSUPPORT_FULL then
      begin
        if SSDInfo.IsHostWrite then
          lHost.Caption := CapHostWrite[CurrLang]
        else
          lHost.Caption := CapNandWrite[CurrLang];

        lHost.Caption :=
          lHost.Caption
          + GetTBStr(1024, HostWrites / 10.24 * 0.64 * 1024, 1);
      end
      //Case 2 : 호스트 쓰기가 Wear Leveling Count로 제공되는 경우
      else if SSDInfo.SSDSupport.SupportHostWrite = HSUPPORT_COUNT then
      begin
        lHost.Caption := CapSSDLifeLeft[CurrLang]
                          + UIntToStr(ExtractSMARTPercent(SSDInfo.SMARTData,
                                                          1)) + '%';
        lTodayUsage.Caption := CapWearLevel[CurrLang]
                                + UIntToStr(ExtractSMART(SSDInfo.SMARTData,
                                            'AD'));
        lHost.Top := lHost.Top + 25;
        lTodayUsage.Top := lTodayUsage.Top + 15;
        lOntime.Top := lOntime.Top + 10;
        l1Month.Visible := false;
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

      if SSDInfo.SSDSupport.SupportHostWrite = HSUPPORT_FULL then
      begin
        CurrWritLog :=
          TNSTLog.Create(
            ExtractFilePath(
              GetRegStr('LM',
                'Software\Microsoft\Windows\CurrentVersion\' +
                'Uninstall\Naraeon SSD Tools\', 'UninstallString')),
            SSDInfo.Serial, UIntToStr(HostWrites), false, SSDInfo.S10085);

        AvgDays := -1;
        for CurrAvgDay := AvgMax downto 0 do
        begin
          if Length(CurrWritLog.Average[IntToAvg[CurrAvgDay]]) > 0 then
          begin
            AvgDays := CurrAvgDay;
            break;
          end;
        end;
        if AvgDays <> -1 then
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
    end;

    lOntime.Caption :=
      CapPowerTime[CurrLang] +
      UIntToStr(ExtractSMART(SSDInfo.SMARTData, 9) and $FFFFFFFF) +
      CapHour[CurrLang];

    // 섹터 치환
    ReplacedSectors := SSDInfo.ReplacedSectors;
    CurrSectLog :=
      TNSTLog.Create(
        ExtractFilePath(
          GetRegStr('LM', 'Software\Microsoft\Windows\CurrentVersion\' +
            'Uninstall\Naraeon SSD Tools\', 'UninstallString')),
        SSDInfo.Serial + 'RSLog', UIntToStr(ReplacedSectors), true, false);

    //섹터 치환만 지원하는 모델들
    if SSDInfo.SSDSupport.SupportHostWrite = HSUPPORT_NONE then
    begin
      lHost.Caption := CapRepSect[CurrLang] + UIntToStr(ReplacedSectors) +
        CapCount[CurrLang];
      lAnalytics.Caption := BtLifeAnaly[CurrLang];
      lAnaly.Caption := CapLifeAnaly[CurrLang];

      if CurrSectLog <> nil then
      begin
        AvgDays := -1;
        for CurrAvgDay := AvgMax downto 0 do
        begin
          if Length(CurrSectLog.Average[IntToAvg[CurrAvgDay]]) > 0 then
          begin
            AvgDays := CurrAvgDay;
            break;
          end;
        end;
        if AvgDays <> -1 then
        begin
          if Length(CurrSectLog.Average[IntToAvg[CurrAvgDay]]) > 0 then
          begin
            l1Month.Caption := CaprAvg[AvgDays][CurrLang] +
              CurrSectLog.Average[IntToAvg[CurrAvgDay]] + CapCount[CurrLang];
          end;
        end;
        lTodayUsage.Caption := CaprToday[CurrLang] + CurrSectLog.TodayUsage +
          CapCount[CurrLang];
      end;
    end
    else
    begin
      lAnalytics.Caption := BtAnaly[CurrLang];
      lAnaly.Caption := CapAnaly[CurrLang];
    end;
    FreeAndNil(CurrSectLog);

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

    lTrim.Visible := false;
    iTrim.Visible := false;
    iFirmUp.Visible := false;
    lFirmUp.Visible := false;
    if SSDInfo.ATAorSCSI = MODEL_ATA then
    begin
      if Length(CurrDrvPartitions.Letters) <> 0 then
      begin
        lTrim.Visible := true;
        iTrim.Visible := true;
      end;
    end;

    if (SSDInfo.SSDSupport.SupportFirmUp = false) and (iTrim.Visible = false)
        and (iOptimize.Left = firstiOptLeft) then
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

      if iOptimize.left <> firstiOptLeft then
      begin
        iErase.left := iOptimize.Left;
        lErase.left := iErase.Left + (iErase.Width div 2)
                                   - (lErase.Width div 2);

        iOptimize.left := firstiOptLeft;
        lOptimize.left := iOptimize.Left + (iOptimize.Width div 2)
                                         - (lOptimize.Width div 2);
      end;
    end;
  end;
end;

function RefreshDrives(SSDInfo: TSSDInfo_NST): integer;
var
  TempSSDInfo: TSSDInfo_NST;
  AllDrv: TStringList;
  CurrDrv, CurrExistAtApp, SelectedDrv: Integer;
  CurrAvail, RefreshAll: Boolean;
  Partlen: Integer;
  TempFound: Boolean;
  ATAorSCSI: Boolean;
  RobustMode: Boolean;
  TempUSBMode: Boolean;
  CurrDrvPartitions: TDriveLetters;
  CurrPartition: Integer;
  NewLen: Integer;
  isDriveAccessible: Boolean;

  GetSSDResult: TSSDListResult;
  DrvName: String;
begin
  with fMain do
  begin
    TempSSDInfo := TSSDInfo_NST.Create;
    SelectedDrv := 0;
    NewLen := 0;
    if Length(SSDLabel) > 0 then
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
      if (AllDrv[CurrDrv][Length(AllDrv[CurrDrv])] <> 'U')
          and (AllDrv[CurrDrv][Length(AllDrv[CurrDrv])] <> 'H') then
        DrvName := AllDrv[CurrDrv]
      else
        DrvName := Copy(AllDrv[CurrDrv], 0, Length(AllDrv[CurrDrv]) - 1);

      for CurrExistAtApp := 0 to Length(SSDLabel) - 1 do
        if DrvName = SSDLabel[CurrExistAtApp].DriveName then
          CurrAvail := true;

      if ATAorSCSI = ATAMode then TempSSDInfo.ATAorSCSI := MODEL_ATA
      else if ATAorSCSI = SCSIMode then TempSSDInfo.ATAorSCSI := MODEL_SCSI;

      if RobustMode then
      begin
        TempSSDInfo.ATAorSCSI := MODEL_DETERMINE;
      end;
      TempSSDInfo.SetDeviceName(StrToInt(DrvName));

      if (TempSSDInfo.SupportedDevice <> SUPPORT_NONE)
          and (CurrAvail = false) then
      begin
        TempUSBMode := false;
        if (AllDrv[CurrDrv][Length(AllDrv[CurrDrv])] = 'U')
            or (AllDrv[CurrDrv][Length(AllDrv[CurrDrv])] = 'H') then
        begin
          if AllDrv[CurrDrv][Length(AllDrv[CurrDrv])] = 'U' then
            TempUSBMode := true;
          AllDrv[CurrDrv] := Copy(AllDrv[CurrDrv], 0,
                                  Length(AllDrv[CurrDrv]) - 1);
        end;

        SetLength(SSDLabel, Length(SSDLabel) + 1);
        NewLen := Length(SSDLabel);

        SSDLabel[NewLen - 1] := TSSDLabel.Create(GSSDSel);
        SSDLabel[NewLen - 1].Parent := GSSDSel;
        SSDLabel[NewLen - 1].Font.Name := Font.Name;
        SSDLabel[NewLen - 1].Font.Size := 10;
        SSDLabel[NewLen - 1].DriveName :=  AllDrv[CurrDrv];
        SSDLabel[NewLen - 1].ATAorSCSI := TempSSDInfo.ATAorSCSI;
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
          GetPartitionList(ExtractDeviceNum(TempSSDInfo.DeviceName));

        Partlen := 15 * ceil(CurrDrvPartitions.LetterCount / 3);

        SSDLabel[NewLen - 1].Font.Style := [fsBold];
        SSDLabel[NewLen - 1].Font.Style := [];

        if TempUSBMode then
        begin
          SSDLabel[NewLen - 1].USBMode := true;
        end
        else
        begin
          SSDLabel[NewLen - 1].USBMode := false;
        end;

        SSDLabel[NewLen - 1].Caption := SSDLabel[NewLen - 1].Caption
                                        + TempSSDInfo.Model + ' '
                                        + GetTBStr(1000,
                                            GetDiskSize(AllDrv[CurrDrv])
                                                        / 1000 / 1000, 0);
        for CurrPartition := 0 to (CurrDrvPartitions.LetterCount - 1) do
        begin
          if CurrPartition = 0 then
            SSDLabel[NewLen - 1].Caption := SSDLabel[NewLen - 1].Caption
                                              + '(';
          SSDLabel[NewLen - 1].Caption :=
            SSDLabel[NewLen - 1].Caption
              + CurrDrvPartitions.Letters[CurrPartition];
          if CurrPartition = (CurrDrvPartitions.LetterCount - 1) then
            SSDLabel[NewLen - 1].Caption := SSDLabel[NewLen - 1].Caption
                                              + ') '
          else
            SSDLabel[NewLen - 1].Caption := SSDLabel[NewLen - 1].Caption
                                              + ' ';
        end;

        if lName.Caption = '' then
        begin
          SSDLabel[NewLen - 1].OnClick(SSDLabel[NewLen - 1]);
          tRefresh.Enabled := true;
          if ATAorSCSI = ATAMode then CurrATAorSCSIStatus := MODEL_ATA
          else if ATAorSCSI = SCSIMode then  CurrATAorSCSIStatus := MODEL_SCSI;
        end;
      end;
    end;

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
    if lName.Caption = '' then
    begin
      AlertCreate(fMain, AlrtNoSupport[CurrLang]);
      ShellExecute(Handle, 'open', PChar(AppPath + 'SSDTools.exe'),
                  PChar('/diag'), nil, SW_SHOW);
    end;
    FreeAndNil(AllDrv);


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

    FreeAndNil(TempSSDInfo);
    result := SelectedDrv;
  end;
end;
end.
