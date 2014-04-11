unit uLanguageSettings;

interface

var
  EnglishMode: Boolean;

const
  CapTotal: Array[0..1] of String = ('전체 용량 : ', 'Disk Size : ');
  CapFirmware: Array[0..1] of String = ('펌웨어 버전 : ', 'Firmware : ');
  CapSerial: Array[0..1] of String = ('시리얼 번호 : ', 'Serial Number : ');
  CapPError: Array[0..1] of String = ('지우기 에러(중요) : ', 'Erase Error(Important) : ');
  CapSectors: Array[0..1] of String = ('치환된 섹터(중요) : ', 'Reallocated Sectors(Important) : ');
  CapAlign: Array[0..1] of String = ('파티션 배열 : ', 'Partition Align : ');
  CapNotSafe: Array[0..1] of String = ('상태 : ', 'Status : ');


implementation

end.
