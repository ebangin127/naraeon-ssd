unit uWMIPhysicalDriveListGetter;

interface

uses
  Windows, ActiveX, ComObj, Variants, SysUtils,
  uOSFile, uPhysicalDriveList;

type
  TWMIPhysicalDriveListGetter = class abstract(TOSFile)
  public
    function GetPhysicalDriveList: TPhysicalDriveList; override;
  end;

implementation

{ TBruteForcePhysicalDriveGetter }

function TWMIPhysicalDriveListGetter.GetPhysicalDriveList:
  TPhysicalDriveList;
var
  wsFileObj: WideString;
  OleDrives: OleVariant;
  Dispatch: IDispatch;
  OleDrivesVar: OleVariant;
  OleEnum: IEnumvariant;
  OleCtx: IBindCtx;
  OleMoniker: IMoniker;

  i: Integer;
  iValue: LongWord;

  CurrEntry: TPhysicalDriveEntry;

  CurrDrv: Integer;
  hdrive: THandle;
begin
  result := TPhysicalDriveList.Create;
  wsFileObj := 'winmgmts:\\localhost\root\cimv2';
  try
    OleCheck(CreateBindCtx(0, OleCtx));
    OleCheck(MkParseDisplayName(OleCtx, PWideChar(wsFileObj), i, OleMoniker));
    OleCheck(OleMoniker.BindToObject(OleCtx, nil, IUnknown, Dispatch));

    OleDrivesVar :=
      OleVariant(Dispatch).ExecQuery('Select * from Win32_DiskDrive');
    OleEnum := IUnknown(OleDrivesVar._NewEnum) as IEnumVariant;

    while OleEnum.Next(1, OleDrives, iValue) = 0 do
    begin
      if (not VarIsNull(OleDrives.DeviceID <> '')) and
         (OleDrives.MediaLoaded) and
         (not VarIsNull(OleDrives.MediaType)) then
      begin
        CurrEntry.DeviceName := ExtractDeviceNum(OleDrives.DeviceID);

        if Pos('hard', LowerCase(OleDrives.MediaType)) >= 0 then
        begin
          CurrEntry.IsUSBDevice := OleDrives.InterfaceType = 'USB';

          if (OleDrives.InterfaceType = 'IDE') or
             (OleDrives.InterfaceType = 'SCSI') or
             (OleDrives.InterfaceType = 'USB') then
            result.Add(CurrEntry);
        end;
      end;
      OleDrives := Unassigned;
    end;
    OleDrivesVar := Unassigned;
  except
  end;
end;

end.
