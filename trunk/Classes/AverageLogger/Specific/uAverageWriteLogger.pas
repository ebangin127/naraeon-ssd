unit uAverageWriteLogger;

interface

uses uAverageLogger;

type
  TAverageWriteLogger = class(TAverageLogger)
  protected
    function GetUnit: Double; virtual; override; 
  end;

implementation

function TAverageWriteLogger.GetUnit: Double; 
begin
  result := 0.064;
end;
end.
