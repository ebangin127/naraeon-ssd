unit Support.Sandforce.OCZ;

interface

uses
  SysUtils,
  Support, Support.Sandforce;

type
  TOCZSandforceNSTSupport = class sealed(TSandforceNSTSupport)
  private
    function IsOCZSandforceProduct: Boolean;
    function IsAgility3: Boolean;

    function IsVertex3: Boolean;
  public
    function GetSupportStatus: TSupportStatus; override;
  end;

implementation

{ TOCZSandforceNSTSupport }

function TOCZSandforceNSTSupport.GetSupportStatus: TSupportStatus;
begin
  FillChar(result, SizeOf(result), #0);
  if IsOCZSandforceProduct then
    result := GetSemiSupport;
end;

function TOCZSandforceNSTSupport.IsVertex3: Boolean;
begin
  result := (Identify.Model = 'OCZ-VERTEX3') or (Identify.Model = 'OCZ-VERTEX3 MI');
end;

function TOCZSandforceNSTSupport.IsAgility3: Boolean;
begin
  result := Identify.Model = 'OCZ-AGILITY3';
end;

function TOCZSandforceNSTSupport.IsOCZSandforceProduct: Boolean;
begin
  result := IsVertex3 or IsAgility3;
end;

end.
