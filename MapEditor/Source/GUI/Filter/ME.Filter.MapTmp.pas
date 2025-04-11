unit ME.Filter.MapTmp;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants, 
  FMX.Types, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls,
  ME.Form.Filter, System.Actions, FMX.ActnList, System.ImageList, FMX.ImgList,
  FMX.Controls.Presentation, FMX.Layouts, FMX.ListBox, Data.Bind.EngExt,
  Fmx.Bind.DBEngExt, System.Rtti, System.Bindings.Outputs, Fmx.Bind.Editors,
  Data.Bind.Components, Data.Bind.DBScope, Data.DB, MemDS, DBAccess, Uni,
  ME.List.Filter, ME.DBList.Filter, ME.DB.Map;

type
  TMapFilterTmp = class(TDBListFilter)
  private
    procedure OnEditItem(Sender: TObject);
  protected
    function GetCommandText: string; override;
    function GetKeyField: string; override;
    function GetCaptionField: string; override;
  public
    constructor Create(AOwner: TComponent); override;
  end;

implementation

uses
  App.Service, ME.Service.Map, ME.Presenter.Map, ME.Edit.Map;

{$R *.fmx}

{ TMapFilterTmp }

constructor TMapFilterTmp.Create(AOwner: TComponent);
begin
  inherited;

  EditItem.OnExecute := OnEditItem;
  EditItem.Visible := True;
end;

procedure TMapFilterTmp.OnEditItem(Sender: TObject);
var
  Presenter: TEditMapPresenter;
  Dialog: TedMap;
  Item: TFilterItem;
  Map: TDBMap;
begin
  if Index = -1 then
    Exit;

  Item := Items[Index];

  Dialog := TedMap.Create(Self);
  try
    Map := TDBMap.Create;
    try
      if not MapService.GetAt(Item.Key, Map) then
        Exit;

      Presenter := TEditMapPresenter.Create(Dialog, Map);
      try
        if Presenter.Edit then begin
          Item.Key := Map.ID;
          Item.Caption := Map.Caption;

          LookupReload;
        end;
      finally
        Presenter.Free;
      end;
    finally
      Map.Free;
    end;
  finally
    Dialog.Free;
  end;
end;

function TMapFilterTmp.GetCommandText: string;
begin
  Result := 'SELECT ID, Caption FROM Map';
end;

function TMapFilterTmp.GetKeyField: string;
begin
  Result := 'ID';
end;

function TMapFilterTmp.GetCaptionField: string;
begin
  Result := 'Caption';
end;

end.
