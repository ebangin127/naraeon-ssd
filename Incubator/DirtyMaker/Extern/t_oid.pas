{-Test prog for hash OID vectors, we 22.02.2007}

program t_pbkdf;

{$i STD.INC}

{$ifdef APPCONS}
  {$apptype console}
{$endif}


uses
  {$ifdef WINCRT}
    wincrt,
  {$endif}
  {$ifdef USEDLL}
    {$ifdef VirtualPascal}
      ch_intv,
    {$else}
      ch_intf,
    {$endif}
  {$else}
    Hash,
    {include hash units in order to register the hash descriptors}
    MD4, MD5, RMD160, SHA1, SHA224, SHA256, SHA384,
    SHA512, SHA5_224, SHA5_256, Whirl512,
  {$endif}
  Mem_Util;


{---------------------------------------------------------------------------}
procedure OID_Check;
  {-Simple check OID1 .. OIDLen >=0, remaing < 0}
var
  algo : THashAlgorithm;
  phash: PHashDesc;
  i,ne: word;
begin
  for algo := C_MinHash to C_MaxHash do begin
    phash := findhash_by_id(algo);
    if phash=nil then writeln('Hash #',ord(algo),' not registered/found [FALSE]')
    else with phash^ do begin
      write(HName:10);
      ne := 0;
      for i:=1 to HLenOID do begin
        if HPtrOID^[i]<0 then begin
          write(i:3,'! ');
          inc(ne);
        end;
      end;
      for i:=HLenOID+1 to MaxOIDLen do begin
        if HPtrOID^[i]>=0 then begin
          write(i:3,'* ');
          inc(ne);
        end;
      end;
      writeln(' - ',ne=0);
    end;
  end;
end;

begin
  writeln('==== sanitiy check OID vectors ======');
  {$ifdef USEDLL}
    writeln('CH_DLL version: ', CH_DLL_Version);
  {$endif}
  OID_Check;
end.
