unit uLegacyOptimizer;

interface

uses SysUtils, Classes, Forms,
    Generics.Collections,
    uNSTRegistry, uIs64Bit, uProcessOpener,
    uLanguageSettings, uAlert, uPathManager, uVersionHelper;

type
  TOptList = TList<Boolean>;
  TNSTOptimizer = class
    Descriptions: TStringList;
    Optimized: TOptList;
    Selective: TOptList;

    //창조자와 파괴자
    constructor Create;
    destructor Destroy; override;

    //최적화 진행
    procedure Optimize(OptList: TOptList);
    procedure OptimizeReturn;

    protected
      //최적화 리스트 초기화
      procedure RefreshOptList;
      procedure CheckOptimized;
  end;

implementation

uses uMain;

constructor TNSTOptimizer.Create;
begin
  Descriptions := TStringList.Create;
  Optimized := TOptList.Create;
  Selective := TOptList.Create;

  RefreshOptList;
  CheckOptimized;
end;

destructor TNSTOptimizer.Destroy;
begin
  if Descriptions <> nil then
    FreeAndNil(Descriptions);

  if Optimized <> nil then
    FreeAndNil(Optimized);

  if Selective <> nil then
    FreeAndNil(Selective);
end;

procedure TNSTOptimizer.Optimize(OptList: TList<Boolean>);
var
  Resultfutil: String;
  CurrItem: Integer;
begin
  CurrItem := 0;
  //--- Essential ---//

  //Hibernation
  if (VersionHelper.MajorVersion < 6) or
     ((VersionHelper.MajorVersion = 6) and (VersionHelper.MinorVersion = 1)) then
     //Exclude above win8
  begin
    if (Optimized[CurrItem] = False) and (OptList[CurrItem]) then
    begin
      ProcessOpener.OpenProcWithOutput(
        PathManager.WinDrive,
        PathManager.WinDir + '\System32\cmd.exe /C powercfg -h off');
    end;
    CurrItem := CurrItem + 1;
  end;

  //LastExcess
  if (Optimized[CurrItem] = False) and (OptList[CurrItem]) then
  begin
    ProcessOpener.OpenProcWithOutput(
      PathManager.WinDrive, 'FSUTIL behavior set disablelastaccess 1');
  end;
  CurrItem := CurrItem + 1;

  //Prefetch
  if (Win32MajorVersion < 6) or
     ((Win32MajorVersion = 6) and (Win32MinorVersion = 1)) then //Exclude above win8
  begin
    if (Optimized[CurrItem] = False) and (OptList[CurrItem]) then
    begin
      if Win32MajorVersion = 6 then
      begin
        NSTRegistry.SetRegInt(NSTRegistry.LegacyPathToNew('LM',
          'SYSTEM\CurrentControlSet\Control\Session Manager' +
          '\Memory Management\PrefetchParameters', 'EnablePrefetcher'), 0);
        NSTRegistry.SetRegInt(NSTRegistry.LegacyPathToNew('LM',
          'SYSTEM\CurrentControlSet\Control\Session Manager' +
          '\Memory Management\PrefetchParameters', 'EnableSuperfetch'), 0);
        NSTRegistry.SetRegInt(NSTRegistry.LegacyPathToNew('LM',
          'SYSTEM\CurrentControlSet\services\SysMain', 'Start'), 4);
        Resultfutil :=
          string(ProcessOpener.OpenProcWithOutput(
            PathManager.WinDir + '\System32', 'net stop SysMain'));
      end
      else if Win32MajorVersion = 5 then
      begin
        NSTRegistry.SetRegInt(NSTRegistry.LegacyPathToNew('LM',
          'SYSTEM\CurrentControlSet\Control\Session Manager' +
          '\Memory Management\PrefetchParameters', 'EnablePrefetcher'), 0);
      end;
    end;
    CurrItem := CurrItem + 1;
  end;

  //Defrag
  if Win32MajorVersion = 6 then
  begin
    if (Optimized[CurrItem] = False) and (OptList[CurrItem]) then
    begin
      NSTRegistry.SetRegStr(NSTRegistry.LegacyPathToNew(
        'LM', 'SOFTWARE\Microsoft\Dfrg\BootOptimizeFunction',
        'Enable'), 'N');
      NSTRegistry.SetRegInt(NSTRegistry.LegacyPathToNew('LM',
        'SOFTWARE\Microsoft\Windows\CurrentVersion' +
        '\OptimalLayout', 'EnableAutoLayout'), 0);
    end;
    CurrItem := CurrItem + 1;
  end;

  //--- Essential ---//

  //--- Selective ---//

  //Indexing
  if (Optimized[CurrItem] = False) and (OptList[CurrItem]) then
  begin
    if Win32MajorVersion = 6 then
    begin
      NSTRegistry.SetRegInt(NSTRegistry.LegacyPathToNew(
        'LM', 'SYSTEM\CurrentControlSet\services\WSearch', 'Start'), 4);
      Resultfutil :=
        string(ProcessOpener.OpenProcWithOutput(PathManager.WinDrive, 'net stop WSearch'));
    end
    else if Win32MajorVersion = 5 then
    begin
      NSTRegistry.SetRegInt(NSTRegistry.LegacyPathToNew(
        'LM', 'SYSTEM\CurrentControlSet\services\CiSvc', 'Start'), 4);
      Resultfutil :=
        string(ProcessOpener.OpenProcWithOutput(PathManager.WinDrive, 'net stop CiSvc'));
    end;
  end;
  CurrItem := CurrItem + 1;

  //System Restore
  if (Optimized[CurrItem] = false) and (OptList[CurrItem]) then
  begin
    NSTRegistry.SetRegInt(
      NSTRegistry.LegacyPathToNew('LM',
        'SYSTEM\CurrentControlSet\services\srservice', 'Start'), 4);
    Resultfutil :=
      string(ProcessOpener.OpenProcWithOutput(PathManager.WinDrive, 'net stop srservice'));
    NSTRegistry.SetRegInt(
      NSTRegistry.LegacyPathToNew('LM',
        'SOFTWARE\Microsoft\Windows NT\CurrentVersion' +
        '\SystemRestore', 'DisableSR'), 1);
    if Is64Bit then
    begin
      Resultfutil :=
        string(ProcessOpener.OpenProcWithOutput(PathManager.WinDrive,
          PathManager.WinDir + '\System32\cmd.exe /C ' +
          '"%windir%\sysnative\vssadmin.exe" delete shadows /all /quiet'));
    end
    else
    begin
      Resultfutil :=
        string(ProcessOpener.OpenProcWithOutput(PathManager.WinDrive,
          PathManager.WinDir + '\System32\cmd.exe /C ' +
          '"%windir%\system32\vssadmin.exe" delete shadows /all /quiet'));
    end;
  end;

  //--- Selective ---//

  CheckOptimized;
end;


procedure TNSTOptimizer.OptimizeReturn;
var
  Resultfutil: String;
  CurrItem: Integer;
begin
  CurrItem := 0;
  //--- Essential ---//

  //Hibernation
  if (Win32MajorVersion < 6) or
     ((Win32MajorVersion = 6) and (Win32MinorVersion = 1)) then
     //Exclude above win8
  begin
    if Optimized[CurrItem] then
    begin
      ProcessOpener.OpenProcWithOutput(
        PathManager.WinDrive,
        PathManager.WinDir + '\System32\cmd.exe /C powercfg -h on');
    end;
    CurrItem := CurrItem + 1;
  end;

  //LastExcess
  if Optimized[CurrItem] then
  begin
    ProcessOpener.OpenProcWithOutput(PathManager.WinDrive,
      'FSUTIL behavior set disablelastaccess 0');
  end;
  CurrItem := CurrItem + 1;

  //Prefetch
  if (Win32MajorVersion < 6) or
     ((Win32MajorVersion = 6) and (Win32MinorVersion = 1)) then
     //Exclude above win8
  begin
    if Optimized[CurrItem] then
    begin
      if Win32MajorVersion = 6 then
      begin
        NSTRegistry.SetRegInt(NSTRegistry.LegacyPathToNew('LM',
          'SYSTEM\CurrentControlSet\Control\Session Manager' +
          '\Memory Management\PrefetchParameters', 'EnablePrefetcher'), 3);
        NSTRegistry.SetRegInt(NSTRegistry.LegacyPathToNew('LM',
          'SYSTEM\CurrentControlSet\Control\Session Manager' +
          '\Memory Management\PrefetchParameters', 'EnableSuperfetch'), 3);
        NSTRegistry.SetRegInt(NSTRegistry.LegacyPathToNew('LM',
          'SYSTEM\CurrentControlSet\services\SysMain',
          'Start'), 2);
        Resultfutil := string(ProcessOpener.OpenProcWithOutput(PathManager.WinDir +
          '\System32',
          'net start SysMain'));
      end
      else if Win32MajorVersion = 5 then
      begin
        NSTRegistry.SetRegInt(NSTRegistry.LegacyPathToNew('LM',
          'SYSTEM\CurrentControlSet\Control\Session Manager\' +
          'Memory Management\PrefetchParameters', 'EnablePrefetcher'), 3);
      end;
    end;
    CurrItem := CurrItem + 1;
  end;

  //Defrag
  if Win32MajorVersion = 6 then
  begin
    if Optimized[CurrItem] then
    begin
      NSTRegistry.SetRegStr(NSTRegistry.LegacyPathToNew(
        'LM', 'SOFTWARE\Microsoft\Dfrg\BootOptimizeFunction', 'Enable'), 'Y');
      NSTRegistry.SetRegInt(NSTRegistry.LegacyPathToNew(
        'LM', 'SOFTWARE\Microsoft\Windows\CurrentVersion' +
        '\OptimalLayout', 'EnableAutoLayout'), 1);
    end;
    CurrItem := CurrItem + 1;
  end;

  //--- Essential ---//

  //--- Selective ---//

  //Indexing
  if Optimized[CurrItem] then
  begin
    if Win32MajorVersion = 6 then
    begin
      NSTRegistry.SetRegInt(NSTRegistry.LegacyPathToNew(
        'LM', 'SYSTEM\CurrentControlSet\services\WSearch', 'Start'), 2);
      Resultfutil :=
        string(ProcessOpener.OpenProcWithOutput(PathManager.WinDrive, 'net start WSearch'));
    end
    else if Win32MajorVersion = 5 then
    begin
      NSTRegistry.SetRegInt(NSTRegistry.LegacyPathToNew(
        'LM', 'SYSTEM\CurrentControlSet\services\CiSvc', 'Start'), 2);
      Resultfutil :=
        string(ProcessOpener.OpenProcWithOutput(PathManager.WinDrive, 'net start CiSvc'));
    end;
  end;
  CurrItem := CurrItem + 1;

  //System Restore
  if Optimized[CurrItem] then
  begin
    NSTRegistry.SetRegInt(NSTRegistry.LegacyPathToNew(
      'LM', 'SYSTEM\CurrentControlSet\services\srservice', 'Start'), 2);
    Resultfutil :=
      string(ProcessOpener.OpenProcWithOutput(PathManager.WinDrive, 'net start srservice'));
    NSTRegistry.SetRegInt(NSTRegistry.LegacyPathToNew(
      'LM', 'SOFTWARE\Microsoft\Windows NT\CurrentVersion\' +
      'SystemRestore', 'DisableSR'), 0);
  end;

  //--- Selective ---//

  CheckOptimized;
end;

procedure TNSTOptimizer.CheckOptimized;
var
  Resultfutil: String;
  CurrItem: Integer;
begin
  CurrItem := 0;
  RefreshOptList;

  //--- Essential ---//

  //Hibernation
  if (Win32MajorVersion < 6) or
     ((Win32MajorVersion = 6) and (Win32MinorVersion = 1)) then //Exclude above win8
  begin
    Optimized[CurrItem] :=
      not FileExists(PathManager.WinDrive + '\hiberfil.sys');
    CurrItem := CurrItem + 1;
  end;

  //LastAccess
  Resultfutil :=
    string(ProcessOpener.OpenProcWithOutput(PathManager.WinDrive,
      'FSUTIL behavior query disablelastaccess'));
  Optimized[CurrItem] := not (Pos('= 0', Resultfutil) > 0);
  CurrItem := CurrItem + 1;

  //Prefetch
  if (Win32MajorVersion < 6) or
     ((Win32MajorVersion = 6) and (Win32MinorVersion = 1)) then //Exclude above win8
  begin
    Optimized[CurrItem] :=
      not
        (NSTRegistry.GetRegInt(NSTRegistry.LegacyPathToNew('LM',
          'SYSTEM\CurrentControlSet\Control\Session Manager' +
          '\Memory Management\PrefetchParameters',
          'EnablePrefetcher')) > 0);
    CurrItem := CurrItem + 1;
  end;

  //Defrag
  if Win32MajorVersion = 6 then
  begin
    Optimized[CurrItem] :=
      not
        ((NSTRegistry.GetRegStr(NSTRegistry.LegacyPathToNew('LM',
          'SOFTWARE\Microsoft\Dfrg\BootOptimizeFunction', 'Enable')) <> 'N')
        and
        (NSTRegistry.GetRegInt(NSTRegistry.LegacyPathToNew('LM',
          'SOFTWARE\Microsoft\Windows\CurrentVersion\OptimalLayout',
          'EnableAutoLayout')) <> 0));
    CurrItem := CurrItem + 1;
  end;

  //--- Essential ---//

  //--- Selective ---//

  //Indexing
  if Win32MajorVersion = 6 then
  begin
    Optimized[CurrItem] :=
      (NSTRegistry.GetRegInt(NSTRegistry.LegacyPathToNew(
        'LM', 'SYSTEM\CurrentControlSet\services\WSearch',
        'Start')) = 4);
  end
  else if Win32MajorVersion = 5 then
  begin
    Optimized[CurrItem] :=
      (NSTRegistry.GetRegInt(NSTRegistry.LegacyPathToNew(
        'LM', 'SYSTEM\CurrentControlSet\services\CiSvc',
        'Start')) = 4);
  end;
  CurrItem := CurrItem + 1;

  //시스템 보호 끄기
  Optimized[CurrItem] :=
    ((NSTRegistry.GetRegInt(NSTRegistry.LegacyPathToNew('LM',
                'SYSTEM\CurrentControlSet\services\srservice',
                'Start')) = 4) and
     (NSTRegistry.GetRegInt(NSTRegistry.LegacyPathToNew('LM',
                'SOFTWARE\Microsoft\Windows NT\CurrentVersion\SystemRestore',
                'DisableSR')) = 1));
end;

procedure TNSTOptimizer.RefreshOptList;
begin
  Descriptions.Clear;
  Selective.Clear;
  Optimized.Clear;

  Descriptions.Add(CapOptHiber[CurrLang]);
  Descriptions.Add(CapOptFilerec[CurrLang]);
  if Win32MajorVersion < 6 then
    Descriptions.Add(CapOptPrefetch[CurrLang])
  else
    Descriptions.Add(CapOptSupFetch[CurrLang]);
  if Win32MajorVersion = 6 then
    Descriptions.Add(CapOptDfrg[CurrLang]);
  Descriptions.Add(CapOptIndex[CurrLang]);
  Descriptions.Add(CapOptRes[CurrLang]);

  Selective.Add(False);
  Selective.Add(False);
  Selective.Add(False);
  if Win32MajorVersion = 6 then
    Selective.Add(False);
  Selective.Add(True);
  Selective.Add(True);

  Optimized.Add(False);
  Optimized.Add(False);
  Optimized.Add(False);
  if Win32MajorVersion = 6 then
    Optimized.Add(False);
  Optimized.Add(False);
  Optimized.Add(False);

  if Win32MajorVersion = 6 then
  begin
    if Win32MinorVersion > 1 then
    begin
      Descriptions.Delete(2);
      Selective.Delete(2);
      Optimized.Delete(2);

      Descriptions.Delete(0);
      Selective.Delete(0);
      Optimized.Delete(0);
    end;
  end;
end;
end.
