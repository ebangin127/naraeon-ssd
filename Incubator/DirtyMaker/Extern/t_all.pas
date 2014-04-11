{-Test prog for CRC/HASH, we 30.08.03}

program t_all;

{$i STD.INC}

{$ifdef APPCONS}
  {$apptype console}
{$endif}


uses
  {$ifdef WINCRT} WinCRT, {$endif}
  {$ifdef USEDLL}
    {$ifdef VirtualPascal}
      ch_intv,
    {$else}
      ch_intf,
    {$endif}
  {$else}
    hash,
    crc64,
    rmd160,
    sha224,
    sha256,
    sha384,
    sha512,
    sha5_224,
    sha5_256,
    whirl512,
    sha1,
    ed2k,
    md4,
    md5,
    adler32,
    fcrc32,
    crc32,
    crc24,
    crc16,
    crc_sick,
  {$endif}
  bcrc32,
  bcrc64;

begin
  writeln('CRC/Hash test program    (c) 2002-2012 W.Ehrhardt');
  {$ifdef USEDLL}
    writeln('CH_DLL version: ', CH_DLL_Version);
  {$endif}
  writeln('CRC-Sick   self test passed: ', CRC_Sick_SelfTest);
  writeln('CRC16      self test passed: ', CRC16SelfTest);
  writeln('CRC24      self test passed: ', CRC24SelfTest);
  writeln('CRC32      self test passed: ', CRC32SelfTest);
  writeln('FCRC32     self test passed: ', FCRC32SelfTest);
  writeln('bCRC32     self test passed: ', bCRC32SelfTest);
  writeln('Adler32    self test passed: ', Adler32SelfTest);
  writeln('CRC64      self test passed: ', CRC64SelfTest);
  writeln('bCRC64     self test passed: ', bCRC64SelfTest);
  writeln('eDonkey    self test passed: ', ED2K_SelfTest);
  writeln('MD4        self test passed: ', MD4SelfTest);
  writeln('MD5        self test passed: ', MD5SelfTest);
  writeln('RIPEMD160  self test passed: ', RMD160SelfTest);
  writeln('SHA1       self test passed: ', SHA1SelfTest);
  writeln('SHA224     self test passed: ', SHA224SelfTest);
  writeln('SHA256     self test passed: ', SHA256SelfTest);
  writeln('SHA384     self test passed: ', SHA384SelfTest);
  writeln('SHA512     self test passed: ', SHA512SelfTest);
  writeln('SHA512/224 self test passed: ', SHA5_224SelfTest);
  writeln('SHA512/256 self test passed: ', SHA5_256SelfTest);
  writeln('Whirlpool  self test passed: ', Whirl_SelfTest);
end.
