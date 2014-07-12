unit uUSBDrive;

interface

uses
  Windows, Classes,
  uLanguageSettings,
  uDiskFunctions, uPartitionFunctions;


//자식 드라이브 가져오기
procedure GetUSBDrives(USBDrives: TStrings);
procedure GetChildDrives(DiskNumber: String; ChildDrives: TStrings);

implementation

uses uMain;

procedure GetChildDrives(DiskNumber: String; ChildDrives: TStrings);
var
  CurrDrv, DriveCount: Integer;
  DrvNames: TDriveLetters;
begin
  ChildDrives.Clear;
  DrvNames := GetPartitionList(DiskNumber);
  DriveCount := DrvNames.LetterCount;
  for CurrDrv := 0 to DriveCount - 1 do
    ChildDrives.Add(GetVolumeLabel(CapLocalDisk[CurrLang],
                                   DrvNames.Letters[CurrDrv] + '\'));
end;

procedure GetUSBDrives(USBDrives: TStrings);
var
  CurrDrv, DriveCount: Integer;
  Drives: Array[0..255] of char;
  DrvName: String;
begin
  USBDrives.Clear;
  FillChar(Drives, 256, #0 );
  DriveCount := GetLogicalDriveStrings(256, Drives);
  for CurrDrv := 0 to DriveCount - 1 do
  begin
    if Drives[CurrDrv] = #0  then
    begin
      if GetDriveType(PChar(DrvName)) = DRIVE_REMOVABLE then
      begin
        USBDrives.Add(GetVolumeLabel(CapRemvDisk[CurrLang], DrvName));
      end;
      DrvName := '';
    end
    else
      DrvName := DrvName + Drives[CurrDrv];
  end;
end;
end.
