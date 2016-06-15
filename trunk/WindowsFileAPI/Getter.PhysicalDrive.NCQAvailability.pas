unit Getter.PhysicalDrive.NCQAvailability;

interface

uses
  Windows,
  OSFile.Handle, OSFile.IoControl, OS.Handle;

type
  TNCQAvailability =
    (Unknown, Disabled, Enabled);

  TNCQAvailabilityGetter = class sealed(TIoControlFile)
  public
    constructor Create(const FileToGetAccess: String); override;
    function GetNCQStatus: TNCQAvailability;
  protected
    function GetMinimumPrivilege: TCreateFileDesiredAccess; override;
  private
    type
      STORAGE_PROPERTY_QUERY = record
        PropertyId: DWORD;
        QueryType: DWORD;
        AdditionalParameters: array[0..3] of Byte;
      end;
      TBusType = (UnknownBus = 0, SCSI, ATAPI, ATA, IEEE1394, SSA,
        Fibre, USB, RAID, iSCSI, SAS, SATA, BusTypeMaxReserved = $7F);
      STORAGE_ADAPTOR_DESCRIPTOR = record
        Version: DWORD;
        Size: DWORD;
        MaximumTransferLength: DWORD;
        MaximumPhysicalPages: DWORD;
        AlignmentMask: DWORD;
        AdaptorUsesPio: Boolean;
        AdaptorScansDown: Boolean;
        CommandQueuing: Boolean;
        AccelatedTransfer: Boolean;
        BusType: TBusType;
        BusMajorVersion: WORD;
        BusMinorVersion: WORD;
      end;
      TStorageAdaptorDescriptorWithBuffer = record
        StorageAdaptorDescriptor: STORAGE_ADAPTOR_DESCRIPTOR;
        Buffer: Array[0..1023] of Byte;
      end;
  private
    InnerQueryBuffer: STORAGE_PROPERTY_QUERY;
    InnerOutputBuffer: STORAGE_ADAPTOR_DESCRIPTOR;
    procedure GetAdaptorDescriptorAndIfNotReturnedRaiseException(
      const IOBuffer: TIoControlIOBuffer);
    procedure SetAdaptorDescriptor;
    function GetIOBufferToGetAdaptorDescriptor: TIoControlIOBuffer;
    procedure SetQueryBuffer;
    function DetermineNCQStatus: TNCQAvailability;
    function IsSCSIwithCommandQueuing: Boolean;
    function IsATAwithCommandQueuing: Boolean;
    function IsSATA: Boolean;
    function IsNCQ: Boolean;
    function IsUnknown: Boolean;
    function IsRAIDwithCommandQueuing: Boolean;
  end;


implementation

{ TDiskGeometry }

constructor TNCQAvailabilityGetter.Create(const FileToGetAccess: String);
begin
  inherited;
  CreateHandle(FileToGetAccess, GetMinimumPrivilege);
end;

function TNCQAvailabilityGetter.GetIOBufferToGetAdaptorDescriptor:
  TIoControlIOBuffer;
const
  NullInputBuffer = nil;
  NullInputBufferSize = 0;
begin
  result.InputBuffer.Buffer := @InnerQueryBuffer;
  result.InputBuffer.Size := SizeOf(InnerQueryBuffer);

  result.OutputBuffer.Buffer := @InnerOutputBuffer;
  result.OutputBuffer.Size := SizeOf(InnerOutputBuffer);
end;

procedure TNCQAvailabilityGetter.
  GetAdaptorDescriptorAndIfNotReturnedRaiseException(
    const IOBuffer: TIoControlIOBuffer);
var
  ReturnedBytes: Cardinal;
begin
  ReturnedBytes := IoControl(TIoControlCode.StorageQueryProperty, IOBuffer);
  if ReturnedBytes = 0 then
    ENoDataReturnedFromIO.Create
      ('NoDataReturnedFromIO: No data returned from StorageQueryProperty');
end;

procedure TNCQAvailabilityGetter.SetQueryBuffer;
type
  STORAGE_QUERY_TYPE = (PropertyStandardQuery = 0, PropertyExistsQuery,
                        PropertyMaskQuery, PropertyQueryMaxDefined);
  TStorageQueryType = STORAGE_QUERY_TYPE;

  STORAGE_PROPERTY_ID = (StorageDeviceProperty = 0, StorageAdapterProperty);
  TStoragePropertyID = STORAGE_PROPERTY_ID;
begin
  InnerOutputBuffer.Size := SizeOf(InnerOutputBuffer);
  InnerQueryBuffer.PropertyId := Cardinal(StorageAdapterProperty);
  InnerQueryBuffer.QueryType := Cardinal(PropertyStandardQuery);
end;

procedure TNCQAvailabilityGetter.SetAdaptorDescriptor;
var
  IOBuffer: TIoControlIOBuffer;
begin
  SetQueryBuffer;
  IOBuffer := GetIOBufferToGetAdaptorDescriptor;
  GetAdaptorDescriptorAndIfNotReturnedRaiseException(IOBuffer);
end;

function TNCQAvailabilityGetter.IsSCSIwithCommandQueuing: Boolean;
begin
  result :=
    (InnerOutputBuffer.CommandQueuing) and
    (InnerOutputBuffer.BusType = TBusType.SCSI);
end;

function TNCQAvailabilityGetter.IsATAwithCommandQueuing: Boolean;
begin
  result :=
    (InnerOutputBuffer.CommandQueuing) and
    (InnerOutputBuffer.BusType = TBusType.ATA);
end;

function TNCQAvailabilityGetter.IsRAIDwithCommandQueuing: Boolean;
begin
  result :=
    (InnerOutputBuffer.CommandQueuing) and
    (InnerOutputBuffer.BusType = TBusType.RAID);
end;

function TNCQAvailabilityGetter.IsSATA: Boolean;
begin
  result :=
    InnerOutputBuffer.BusType = TBusType.SATA;
end;

function TNCQAvailabilityGetter.IsUnknown: Boolean;
begin
  result :=
    (InnerOutputBuffer.BusType <> TBusType.SATA) and
    (InnerOutputBuffer.BusType <> TBusType.RAID) and
    (InnerOutputBuffer.BusType <> TBusType.ATA) and
    (InnerOutputBuffer.BusType <> TBusType.SCSI);
end;

function TNCQAvailabilityGetter.IsNCQ: Boolean;
begin
  result :=
    IsSCSIwithCommandQueuing or
    IsATAwithCommandQueuing or
    IsRAIDwithCommandQueuing or
    IsSATA;
end;

function TNCQAvailabilityGetter.DetermineNCQStatus: TNCQAvailability;
begin
  if IsUnknown then
    result := TNCQAvailability.Unknown
  else if IsNCQ then
    result := TNCQAvailability.Enabled
  else
    result := TNCQAvailability.Disabled;
end;

function TNCQAvailabilityGetter.GetNCQStatus: TNCQAvailability;
begin
  SetAdaptorDescriptor;
  result := DetermineNCQStatus;
end;

function TNCQAvailabilityGetter.GetMinimumPrivilege:
  TCreateFileDesiredAccess;
begin
  exit(DesiredReadOnly);
end;

end.
