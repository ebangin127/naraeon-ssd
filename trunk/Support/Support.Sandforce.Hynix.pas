unit Support.Sandforce.Hynix;

interface

uses
  SysUtils,
  Support, Support.Sandforce;

type
  THynixSandforceNSTSupport = class sealed(TSandforceNSTSupport)
  private
    function IsHynixSandforceProduct: Boolean;

  public
    function GetSupportStatus: TSupportStatus; override;
  end;

implementation

{ THynixSandforceNSTSupport }

function THynixSandforceNSTSupport.GetSupportStatus: TSupportStatus;
begin
  if IsHynixSandforceProduct then
    result := GetSemiSupport;
end;

function THynixSandforceNSTSupport.IsHynixSandforceProduct: Boolean;
begin
  result := (Pos('MNM', Model) > 0) and (Pos('HFS', Model) > 0);
end;

end.
