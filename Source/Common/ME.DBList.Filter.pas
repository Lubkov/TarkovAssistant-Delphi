unit ME.DBList.Filter;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants, 
  FMX.Types, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls,
  ME.List.Filter, System.Actions, FMX.ActnList, System.ImageList, FMX.ImgList,
  FMX.ListBox, FMX.Controls.Presentation, FMX.Layouts, Data.DB, MemDS, DBAccess,
  Uni;

type
  TDBListFilter = class(TListFilter)
  private
  protected
    function GetCommandText: string; virtual; abstract;
    function GetKeyField: string; virtual; abstract;
    function GetCaptionField: string; virtual; abstract;
  public
    procedure Init; override;
  end;

implementation

uses
  App.Service;

{$R *.fmx}

{ TDBListFilter }

procedure TDBListFilter.Init;
var
  Query: TUniQuery;
begin
  inherited Init;

  Query := TUniQuery.Create(Self);
  try
    Query.Connection := AppService.DBConnection.Connection;
    Query.SQL.Text := GetCommandText;
    Query.Open;

    while not Query.Eof do begin
      AddItem(Query.FieldByName(GetKeyField).Value, Query.FieldByName(GetCaptionField).AsString);
      Query.Next;
    end;
  finally
    Query.Free;
  end;
end;

end.
