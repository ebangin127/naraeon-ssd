unit Getter.SCSIAddress;

interface

uses
  Windows, SysUtils, Generics.Collections,
  OSFile.Handle, OSFile.IoControl;

type
  SCSI_ADDRESS = record
    Length: ULONG;
    PortNumber: UCHAR;
    PathId: UCHAR;
    TargetId: UCHAR;
    Lun: UCHAR;
  end;

  TSCSIAddressGetter = class sealed(TIoControlFile)
  public
    constructor Create(FileToGetAccess: String); override;
    function GetSCSIAddress: SCSI_ADDRESS;
  protected
    function GetMinimumPrivilege: TCreateFileDesiredAccess; override;
  end;

implementation

{ TSCSIAddressGetter }

constructor TSCSIAddressGetter.Create(FileToGetAccess: String);
begin
  CreateHandle(FileToGetAccess, DesiredReadWrite);
end;

function TSCSIAddressGetter.GetMinimumPrivilege: TCreateFileDesiredAccess;
begin
  exit(DesiredReadWrite);
end;

function TSCSIAddressGetter.GetSCSIAddress: SCSI_ADDRESS;
var
  resultBuffer: SCSI_ADDRESS;
begin
  IoControl(TIoControlCode.GetScsiAddress,
    BuildOSBufferByOutput<SCSI_ADDRESS>(resultBuffer));
  exit(resultBuffer);
end;

end.
