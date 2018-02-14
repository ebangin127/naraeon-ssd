unit CommandSet;

interface

uses
  SysUtils,
  Device.SMART.List, BufferInterpreter,
  {$IfDef UNITTEST}
  Mock.OSFile.IoControl;
  {$Else}
  OSFile.Handle, OSFile.IoControl, OS.Handle;
  {$EndIf}

type
  TCommandSet = class abstract(TIoControlFile)
  public
    constructor Create(const FileToGetAccess: String); override;
    function IdentifyDevice: TIdentifyDeviceResult; virtual; abstract;
    function SMARTReadData: TSMARTValueList; virtual; abstract;
    function RAWIdentifyDevice: String; virtual; abstract;
    function RAWSMARTReadData: String; virtual; abstract;
    function IsDataSetManagementSupported: Boolean; virtual; abstract;
    function DataSetManagement(StartLBA, LBACount: Int64): Cardinal;
      virtual; abstract;
    function IsExternal: Boolean; virtual; abstract;
    procedure Flush; virtual; abstract;
  protected
    const
      IdentifyDevicePrefix = 'IdentifyDevice';
      SMARTPrefix = 'SMART';
    function GetMinimumPrivilege: TCreateFileDesiredAccess; override;
  end;

implementation

{ TCommandSet }

constructor TCommandSet.Create(const FileToGetAccess: String);
begin
  inherited;
  CreateHandle(FileToGetAccess, GetMinimumPrivilege);
end;

function TCommandSet.GetMinimumPrivilege: TCreateFileDesiredAccess;
begin
  exit(DesiredReadWrite);
end;

end.
