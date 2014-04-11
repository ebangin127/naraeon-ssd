{-Test prog for crc_sick, we 17.12.10}

program t_sick;

{$i STD.INC}

{$ifdef APPCONS}
  {$apptype console}
{$endif}


uses
  {$ifdef WINCRT} WinCRT, {$endif}
  {$ifdef USEDLL}
    BTypes,
    {$ifdef VirtualPascal}
      ch_intv;
    {$else}
      ch_intf;
    {$endif}
  {$else}
    crc_sick;
  {$endif}


{$ifdef USEDLL}
{---------------------------------------------------------------------------}
function CRC_Sick_SelfTest2: boolean;
  {-Same as CRC_Sick_SelfTest, used to test the separate DLL functions}
const
  s1: array[0..8] of char8 = '123456789';
  s2: string[26] = 'abcdefghijklmnopqrstuvwxyz';
var
  i,CRC,CRCF: word;
  ctx: TSickCTX;
begin
  {$ifndef BIT16}
    CRC_Sick_FullXL(CRCF, @s1, sizeof(s1));
  {$else}
    CRC_Sick_Full(CRCF, @s1, sizeof(s1));
  {$endif}
  CRC_Sick_Init(ctx);
  for i:=1 to length(s2) do CRC_Sick_Update(ctx, @s2[i], 1);
  CRC := CRC_Sick_Final(ctx);
  CRC_Sick_SelfTest2 := (CRC=$42FE) and (CRCF=$56A6);
end;
{$endif}


begin
  writeln('CRC/Hash test program [CRC-16/Sick]   (c) 2002-2012 W.Ehrhardt');
  {$ifdef USEDLL}
    writeln('CH_DLL version: ', CH_DLL_Version);
    writeln('CRC-16/Sick selftest1: ',crc_sick_selftest);
    writeln('CRC-16/Sick selftest2: ',crc_sick_selftest2);
  {$else}
    writeln('CRC-16/Sick selftest: ',crc_sick_selftest);
  {$endif}
end.
