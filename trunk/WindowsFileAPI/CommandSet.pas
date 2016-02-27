unit CommandSet;

interface

uses
  SysUtils,
  Device.SMART.List, BufferInterpreter,
  {$IfDef UNITTEST}
  Mock.OSFile.IoControl;
  {$Else}
  OSFile.Handle, OSFile.IoControl;
  {$EndIf}

type
  TCommandSet = class abstract(TIoControlFile)
  public
    constructor Create(const FileToGetAccess: String);
    function IdentifyDevice: TIdentifyDeviceResult; virtual; abstract;
    function SMARTReadData: TSMARTValueList; virtual; abstract;
    function IsDataSetManagementSupported: Boolean; virtual; abstract;
    function DataSetManagement(StartLBA, LBACount: Int64): Cardinal;
      virtual; abstract;
    function IsExternal: Boolean; virtual; abstract;
  protected
    function GetMinimumPrivilege: TCreateFileDesiredAccess; override;
  end;

implementation

{ TCommandSet }

constructor TCommandSet.Create(const FileToGetAccess: String);
begin
  CreateHandle(FileToGetAccess, GetMinimumPrivilege);
end;

function TCommandSet.GetMinimumPrivilege: TCreateFileDesiredAccess;
begin
  exit(DesiredReadWrite);
end;

end.
