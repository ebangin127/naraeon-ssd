unit Support.Sandforce.MachXtreme;

interface

uses
  SysUtils,
  Support, Support.Sandforce;

type
  TMachXtremeSandforceNSTSupport = class sealed(TSandforceNSTSupport)
  private
    function IsMachXtremeSandforceProduct: Boolean;

  public
    function GetSupportStatus: TSupportStatus; override;
  end;

implementation

{ TMachXtremeSandforceNSTSupport }

function TMachXtremeSandforceNSTSupport.GetSupportStatus: TSupportStatus;
begin
  FillChar(result, SizeOf(result), #0);
  if IsMachXtremeSandforceProduct then
    result := GetSemiSupport;
end;

function TMachXtremeSandforceNSTSupport.IsMachXtremeSandforceProduct: Boolean;
begin
  result :=
    (Pos('MXSSD', UpperCase(Model)) > 0) and
    (Pos('SF', UpperCase(Model)) > 0);
end;

end.
