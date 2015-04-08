unit uWeb;

interface

uses
  Classes;

type
  TWeb = class abstract
  public
    function Get(PathToGet: String): TStringList; virtual; abstract;
  protected
    const
      UserAgent = 'Naraeon SSD Tools';
      CharacterSet = 'Unicode';
  end;

implementation

end.
