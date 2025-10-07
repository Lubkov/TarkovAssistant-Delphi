unit ME.Frame.Layer;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls,
  System.Actions, FMX.ActnList, FMX.Controls.Presentation, System.ImageList,
  FMX.ImgList, FMX.Objects, System.Rtti, FMX.Grid.Style, FMX.Grid, FMX.ScrollBox,
  Data.DB, MemDS, DBAccess, Uni, ME.DB.Layer, ME.DB.Map, Data.Bind.Components,
  Data.Bind.DBScope, Fmx.Bind.Grid, System.Bindings.Outputs, Fmx.Bind.Editors,
  Data.Bind.EngExt, Fmx.Bind.DBEngExt, Data.Bind.Grid, ME.Grid.Helper;

type
  TfrLayerList = class(TFrame)
    ImageList1: TImageList;
    paTopPanel: TPanel;
    edAddLayer: TSpeedButton;
    edEditLayer: TSpeedButton;
    edDeleteLayer: TSpeedButton;
    laTitle: TLabel;
    ActionList1: TActionList;
    acAddLayer: TAction;
    acEditLayer: TAction;
    acDeleteLayer: TAction;
    paPicture: TPanel;
    imMapPicture: TImage;
    F: TUniQuery;
    FID: TIntegerField;
    FLevel: TIntegerField;
    BindSourceDB1: TBindSourceDB;
    Grid: TStringGrid;
    FName: TWideMemoField;
    procedure ActionList1Update(Action: TBasicAction; var Handled: Boolean);
    procedure acAddLayerExecute(Sender: TObject);
    procedure acEditLayerExecute(Sender: TObject);
    procedure acDeleteLayerExecute(Sender: TObject);
    procedure BindSourceDB1SubDataSourceDataChange(Sender: TObject; Field: TField);
  private
    FMapID: Variant;
    FLayerID: Variant;
    FGridHelper: TGridHelper;

    procedure InitColumns;
    procedure LoadPicture;
    function InternalLayerEdit(const Layer: TDBLayer): Boolean;
    procedure LayerEdit(const LayerID: Variant);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure Init(const Map: TDBMap);
  end;

implementation

{$R *.fmx}

uses
  App.Service, ME.DB.Utils, ME.Dialog.Presenter, ME.Presenter.Layer, ME.Edit.Layer,
  ME.Dialog.Message, ME.Service.Layer;

{ TfrLayerList }

constructor TfrLayerList.Create(AOwner: TComponent);
begin
  inherited;

  FMapID := Null;
  FLayerID := Null;
  Grid.RowCount := 0;

  FGridHelper := TGridHelper.Create(Grid);
  InitColumns;
end;

destructor TfrLayerList.Destroy;
begin
  FGridHelper.Free;

  inherited;
end;

procedure TfrLayerList.InitColumns;
var
  Column: TGridColumn;
begin
  Column := TGridColumn.Create;
  try
    Column.Caption := 'ID';
    Column.FieldName := 'ID';
    Column.Alignment := TAlignment.taRightJustify;
    Column.HeaderAlignment := TAlignment.taCenter;
    Column.Width := 60;
  finally
    FGridHelper.AddColumn(Column);
  end;

  Column := TGridColumn.Create;
  try
    Column.Caption := 'Level';
    Column.FieldName := 'Level';
    Column.Alignment := TAlignment.taCenter;
    Column.HeaderAlignment := TAlignment.taCenter;
    Column.Width := 80;
  finally
    FGridHelper.AddColumn(Column);
  end;

  Column := TGridColumn.Create;
  try
    Column.Caption := 'Name';
    Column.FieldName := 'Name';
    Column.Alignment := TAlignment.taLeftJustify;
    Column.HeaderAlignment := TAlignment.taCenter;
    Column.AutoWidth := True;
  finally
    FGridHelper.AddColumn(Column);
  end;
end;

procedure TfrLayerList.Init(const Map: TDBMap);
const
  TitleFmt = 'Уровни карты "%s"';
begin
  if Map <> nil then begin
    FMapID := Map.ID;
    laTitle.Text := Format(TitleFmt, [map.Caption]);
  end
  else begin
    FMapID := Null;
    laTitle.Text := '';
  end;

  FGridHelper.Binding(BindSourceDB1);

  F.Close;
  F.Connection := AppService.DBConnection.Connection;
  F.SQL.Text :=
    ' SELECT ' + TDBLayer.FieldList +
    ' FROM ' + TDBLayer.EntityName +
    ' WHERE MapID = :MapID';
  F.ParamByName('MapID').Value := FMapID;
  F.Open;

  FGridHelper.InitColumns;
end;

procedure TfrLayerList.BindSourceDB1SubDataSourceDataChange(Sender: TObject; Field: TField);
begin
  if FID.Value = FLayerID then
    Exit;

  FLayerID := FID.Value;
  LoadPicture;
end;

procedure TfrLayerList.LoadPicture;
begin
  if IsNullID(FID.Value) then
    imMapPicture.Bitmap.Assign(nil)
  else
    LayerService.LoadPicture(FID.Value, imMapPicture.Bitmap);
end;

function TfrLayerList.InternalLayerEdit(const Layer: TDBLayer): Boolean;
var
  Presenter: TEditLayerPresenter;
  Dialog: TedLayer;
begin
  Dialog := TedLayer.Create(Self);
  try
    Presenter := TEditLayerPresenter.Create(Dialog, Layer);
    try
      Result := Presenter.Edit;
    finally
      Presenter.Free;
    end;
  finally
    Dialog.Free;
  end;
end;

procedure TfrLayerList.LayerEdit(const LayerID: Variant);
var
  Layer: TDBLayer;
begin
  Layer := TDBLayer.Create;
  try
    if not LayerService.GetAt(FID.Value, Layer) then
      Exit;

    Layer.Picture := imMapPicture.Bitmap;
    if InternalLayerEdit(Layer) then begin
      F.RefreshRecord;
      LoadPicture;
    end;
  finally
    Layer.Free;
  end;
end;

procedure TfrLayerList.ActionList1Update(Action: TBasicAction; var Handled: Boolean);
begin
  acAddLayer.Enabled := not IsNullID(FMapID);
  acEditLayer.Enabled := acAddLayer.Enabled and not IsNullID(FID.Value);
  acDeleteLayer.Enabled := acAddLayer.Enabled and not IsNullID(FID.Value);
end;

procedure TfrLayerList.acAddLayerExecute(Sender: TObject);
var
  Layer: TDBLayer;
begin
  Layer := TDBLayer.Create;
  try
    Layer.MapID := FMapID;
    if not InternalLayerEdit(Layer) then
      Exit;

    F.DisableControls;
    try
      F.Refresh;
      F.Last;
    finally
      F.EnableControls;
    end;
  finally
    Layer.Free;
  end;
end;

procedure TfrLayerList.acEditLayerExecute(Sender: TObject);
begin
  LayerEdit(FID.Value);
end;

procedure TfrLayerList.acDeleteLayerExecute(Sender: TObject);
var
  Layer: TDBLayer;
  Presenter: TDelLayerPresenter;
  Dialog: TedMessage;
begin
  if IsNullID(FID.Value) then
    Exit;

  Layer := TDBLayer.Create;
  try
    Layer.ID := FID.Value;
    Layer.MapID := FMapID;
    Layer.Name := FName.AsString;

    Dialog := TedMessage.Create(Self);
    try
      Presenter := TDelLayerPresenter.Create(Dialog, Layer);
      try
        if not Presenter.Delete then
          Exit;

        F.DisableControls;
        try
          F.Refresh;
        finally
          F.EnableControls;
        end;

        LoadPicture;
      finally
        Presenter.Free;
      end;
    finally
      Dialog.Free;
    end;
  finally
    Layer.Free;
  end;
end;

end.
