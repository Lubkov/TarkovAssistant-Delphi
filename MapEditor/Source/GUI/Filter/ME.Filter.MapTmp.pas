unit ME.Filter.MapTmp;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants, 
  FMX.Types, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls,
  ME.Form.Filter, System.Actions, FMX.ActnList, System.ImageList, FMX.ImgList,
  FMX.Controls.Presentation, FMX.Layouts, FMX.ListBox, Data.Bind.EngExt,
  Fmx.Bind.DBEngExt, System.Rtti, System.Bindings.Outputs, Fmx.Bind.Editors,
  Data.Bind.Components, Data.Bind.DBScope, Data.DB, MemDS, DBAccess, Uni,
  ME.List.Filter, ME.DB.Map;

type
  TMapFilterTmp = class(TListFilter)
    F: TUniQuery;
    FID: TIntegerField;
    FCaption: TWideStringField;
    BindSourceDB1: TBindSourceDB;
    BindingsList1: TBindingsList;
    LinkListControlToField1: TLinkListControlToField;
    procedure BindSourceDB1SubDataSourceDataChange(Sender: TObject; Field: TField);
  private
    procedure OnEditItem(Sender: TObject);
  protected
//    procedure SetKeyValue(const Value: Variant); override;
    procedure SetDisplayValue(const Value: Variant); override;
  public
    constructor Create(AOwner: TComponent); override;

    procedure Init; override;
  end;

implementation

uses
  App.Service, ME.Service.Map, ME.Presenter.Map, ME.Edit.Map;

{$R *.fmx}

{ TMapFilterTmp }

constructor TMapFilterTmp.Create(AOwner: TComponent);
begin
  inherited;

  edFilterValues.OnChange := nil;

  EditItem.OnExecute := OnEditItem;
  EditItem.Visible := True;
end;

procedure TMapFilterTmp.BindSourceDB1SubDataSourceDataChange(Sender: TObject; Field: TField);
begin
  FIsGUIChanged := True;
  KeyValue := FID.Value;
end;

//procedure TMapFilterTmp.SetKeyValue(const Value: Variant);
//begin
//  if KeyValue = Value then
//    Exit;
//
//  if F.Active or VarIsEmpty(Value) or VarIsNull(Value) then
//    edFilterValues.ItemIndex := -1
//  else
//    F.Locate('ID', Value, []);
//
//  inherited;
//end;

procedure TMapFilterTmp.OnEditItem(Sender: TObject);
var
  Presenter: TEditMapPresenter;
  Dialog: TedMap;
  Map: TDBMap;
begin
  Dialog := TedMap.Create(Self);
  try
    Map := TDBMap.Create;
    try
      if not MapService.GetAt(FID.Value, Map) then
        Exit;

      Presenter := TEditMapPresenter.Create(Dialog, Map);
      try
        if Presenter.Edit then
          F.RefreshRecord;
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

procedure TMapFilterTmp.SetDisplayValue(const Value: Variant);
begin
  F.Locate('ID', Value, []);
end;

procedure TMapFilterTmp.Init;
begin
  inherited;

  F.Close;
  F.Connection := AppService.DBConnection.Connection;
  F.SQL.Text := 'SELECT ID, Caption FROM Map';
  F.Open;
end;

end.
