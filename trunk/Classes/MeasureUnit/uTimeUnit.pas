unit uTimeUnit;

interface

uses
  uLanguageSettings;

type
  TimeUnit = record
    FPowerOfSUnit: Integer;
  end;

const
  UnittimePerHigherUnittime = 60;
  HourUnit: TimeUnit = (FPowerOfSUnit: 2);
  MinuteUnit: TimeUnit = (FPowerOfSUnit: 1);
  SecondUnit: TimeUnit = (FPowerOfSUnit: 0);

implementation

end.
