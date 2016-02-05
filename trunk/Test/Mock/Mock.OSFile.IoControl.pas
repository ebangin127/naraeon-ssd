unit Mock.OSFile.IoControl;

interface

uses
  OSFile;

type
  TCreateFileDesiredAccess =
    (DesiredNone, DesiredReadOnly, DesiredReadWrite);
  TIoControlFile = class(TOSFile)
  protected
    procedure CreateHandle(const FileToGetAccess: String;
      const DesiredAccess: TCreateFileDesiredAccess); virtual;
    function GetMinimumPrivilege: TCreateFileDesiredAccess; virtual; abstract;
  end;

implementation

{ TIoControlFile }

procedure TIoControlFile.CreateHandle(const FileToGetAccess: String;
  const DesiredAccess: TCreateFileDesiredAccess);
begin

end;

end.
