unit uDiskFunctions;

interface

uses Windows, SysUtils, Dialogs, Math, Classes,
     ComObj, ShellAPI, Variants, ActiveX,
     uRegFunctions, uPartitionFunctions, uStrFunctions, uDatasizeUnit,
     uPhysicalDriveList;

type
  //---Trim Command--//
  PSTARTING_LCN_INPUT_BUFFER = ^STARTING_LCN_INPUT_BUFFER;
  {$EXTERNALSYM PSTARTING_LCN_INPUT_BUFFER}
  STARTING_LCN_INPUT_BUFFER = record
    StartingLcn: LARGE_INTEGER;
  end;

  PVOLUME_BITMAP_BUFFER = ^VOLUME_BITMAP_BUFFER;
  {$EXTERNALSYM PVOLUME_BITMAP_BUFFER}
  VOLUME_BITMAP_BUFFER = record
    StartingLcn: LARGE_INTEGER;
    BitmapSize: LARGE_INTEGER;
    Buffer: array [0..4095] of Byte;
  end;
  //---Trim Command--//

  //---NCQ---//
  STORAGE_QUERY_TYPE = (PropertyStandardQuery = 0, PropertyExistsQuery,
                        PropertyMaskQuery, PropertyQueryMaxDefined);
  TStorageQueryType = STORAGE_QUERY_TYPE;

  STORAGE_PROPERTY_ID = (StorageDeviceProperty = 0, StorageAdapterProperty);
  TStoragePropertyID = STORAGE_PROPERTY_ID;

  STORAGE_PROPERTY_QUERY = packed record
    PropertyId: DWORD;
    QueryType: DWORD;
    AdditionalParameters: array[0..3] of Byte;
  end;

  STORAGE_BUS_TYPE = (BusTypeUnknown = 0, BusTypeScsi, BusTypeAtapi,
    BusTypeAta, BusType1394, BusTypeSsa, BusTypeFibre,
    BusTypeUsb, BusTypeRAID, BusTypeiScsi, BusTypeSas,
    BusTypeSata, BusTypeMaxReserved = $7F);

  STORAGE_DEVICE_DESCRIPTOR = packed record
    Version: DWORD;
    Size: DWORD;
    DeviceType: Byte;
    DeviceTypeModifier: Byte;
    RemovableMedia: Boolean;
    CommandQueueing: Boolean;
    VendorIdOffset: DWORD;
    ProductIdOffset: DWORD;
    ProductRevisionOffset: DWORD;
    SerialNumberOffset: DWORD;
    BusType: STORAGE_BUS_TYPE;
    RawPropertiesLength: DWORD;
    RawDeviceProperties: PChar;
  end;

  STORAGE_ADAPTOR_DESCRIPTOR = packed record
    Version: DWORD;
    Size: DWORD;
    MaximumTransferLength: DWORD;
    MaximumPhysicalPages: DWORD;
    AlignmentMask: DWORD;
    AdaptorUsesPio: Boolean;
    AdaptorScansDown: Boolean;
    CommandQueueing: Boolean;
    AccelatedTransfer: Boolean;
    BusType: STORAGE_BUS_TYPE;
    BusMajorVersion: WORD;
    BusMinorVersion: WORD;
  end;
  //---NCQ---//

//�뷮, ���� �̸� �� ���� ���� ��������
function GetVolumeLabel(AltName: String; DriveName: String): string;

const
  SMART_READ_ATTRIBUTE_VALUES = $D0;
  SMART_CYL_LOW = $4F;
  SMART_CYL_HI = $C2;
  SMART_CMD = $B0;

  ATAMode = false;
  SCSIMode = true;

  VolumeNames = 'ABCDEFGHIJKLMNOPQRSTUVWXYZ';

implementation

function GetSizeOfDiskInMB(DriveName: String): Double;
var
  SizeOfDiskInByte: Int64;
  ByteToMega: DatasizeUnitChangeSetting;
begin
  SizeOfDiskInByte := DiskSize(Pos(DriveName[1], VolumeNames));

  ByteToMega.FNumeralSystem := Denary;
  ByteToMega.FFromUnit := ByteUnit;
  ByteToMega.FToUnit := MegaUnit;

  exit(ChangeDatasizeUnit(SizeOfDiskInByte, ByteToMega));
end;

function GetVolumeLabel(AltName: String; DriveName: String): string;
const
  VolumeLabelSetting: FormatSizeSetting =
    (FNumeralSystem: Denary; FPrecision: 0);
var
  NotUsed: DWORD;
  VolumeFlags: DWORD;
  VolumeSerialNumber: DWORD;
  Buf: array [0..MAX_PATH] of Char;
  SizeOfDiskInMB: Double;
begin
  FillMemory(@Buf, Length(Buf) * SizeOf(Char), 0);
  GetVolumeInformation(PChar(DriveName), Buf, SizeOf(Buf), @VolumeSerialNumber,
                        NotUsed, VolumeFlags, nil, 0);

  if Buf[0] = #0 then
    CopyMemory(@Buf, @AltName[1],
               Length(AltName) * SizeOf(Char));

  SizeOfDiskInMB := GetSizeOfDiskInMB(DriveName);
  Result :=
    DriveName + ' (' + Buf + ' - ' +
      FormatSizeInMB(SizeOfDiskInMB, VolumeLabelSetting) + ')';
end;

end.

