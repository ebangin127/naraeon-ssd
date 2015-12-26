unit AverageLogger.Count;

interface

uses AverageLogger;

type
  TAverageCountLogger = class(TAverageLogger)
  protected
    function GetUnit: Double; override;
  end;

implementation

function TAverageCountLogger.GetUnit: Double; 
begin
  result := 1;
end;
end.
