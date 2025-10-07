unit ME.Grid.Resources;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants, 
  FMX.Types, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls,
  System.Rtti, FMX.Grid.Style, System.Actions, FMX.ActnList, System.ImageList,
  FMX.ImgList, FMX.Grid, FMX.ScrollBox, FMX.Controls.Presentation, Data.DB,
  MemDS, DBAccess, Uni, Fmx.Bind.Grid, System.Bindings.Outputs, FMX.ExtCtrls,
  Fmx.Bind.Editors, Data.Bind.EngExt, Fmx.Bind.DBEngExt, Data.Bind.Components,
  Data.Bind.Grid, Data.Bind.DBScope, ME.DB.Resource, ME.DB.Marker,
  ME.Frame.Picture, ME.DB.Presenter.Resource, FMX.Edit, ME.Grid.Helper;

type
  TResourcesDBGrid = class(TFrame)
    paTopPanel: TPanel;
    edAddResource: TSpeedButton;
    edEditResource: TSpeedButton;
    edDeleteResource: TSpeedButton;
    laTitle: TLabel;
    ImageList1: TImageList;
    ActionList1: TActionList;
    acAddResource: TAction;
    acEditResource: TAction;
    acDeleteResource: TAction;
    F: TUniQuery;
    FID: TIntegerField;
    FKind: TIntegerField;
    FDescription: TWideMemoField;
    BindSourceDB1: TBindSourceDB;
    Grid: TStringGrid;
    paPicture: TPanel;
    edFilterText: TEdit;

    procedure acEditResourceExecute(Sender: TObject);
    procedure acAddResourceExecute(Sender: TObject);
    procedure acDeleteResourceExecute(Sender: TObject);
    procedure GridCellDblClick(const Column: TColumn; const Row: Integer);
    procedure ActionList1Update(Action: TBasicAction; var Handled: Boolean);
    procedure BindSourceDB1SubDataSourceDataChange(Sender: TObject; Field: TField);
    procedure edFilterTextChangeTracking(Sender: TObject);
  private
    FMarker: TDBMarker;
    FResourceID: Variant;
    FPicturePanel: TfrPicture;
    FSorted: Boolean;
    FGridHelper: TGridHelper;

    function GetMarkerID: Variant;
    function GetShowFilter: Boolean;
    procedure SetShowFilter(const Value: Boolean);
  protected
    function GetResourceKind: TResourceKind; virtual; abstract;
    function GetCommandSQLText: string; virtual; abstract;
    function GetRefreshSQLText: string; virtual; abstract;
    function GetResourceID: Variant; virtual; abstract;
    function GetEditPresenterClass: TEditResourcePresenterClass; virtual; abstract;
    function GetDelPresenterClass: TDelResourcePresenterClass; virtual; abstract;
    procedure InitColumns;
//    function InternalResourceAdd(const Resource: TDBResource): Boolean;

    property PicturePanel: TfrPicture read FPicturePanel;
    property EditPresenterClass: TEditResourcePresenterClass read GetEditPresenterClass;
    property DelPresenterClass: TDelResourcePresenterClass read GetDelPresenterClass;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure Init(const Marker: TDBMarker); virtual;
    procedure AddRecord; virtual; abstract;
    procedure EditRecord; virtual; abstract;
    procedure DeleteRecord; virtual; abstract;
    procedure SetPosition(const ResourceID: Variant);

    property ResourceID: Variant read FResourceID;
    property MarkerID: Variant read GetMarkerID;
    property Marker: TDBMarker read FMarker;
    property ResourceKind: TResourceKind read GetResourceKind;
    property ShowFilter: Boolean read GetShowFilter write SetShowFilter;
    property Sorted: Boolean read FSorted write FSorted;
  end;

implementation

uses
  App.Service, ME.DB.Utils, ME.Service.Resource, ME.DB.Edit.Resource,
  ME.Dialog.Message;

{$R *.fmx}

{ TDBResourcesGrid }

constructor TResourcesDBGrid.Create(AOwner: TComponent);
begin
  inherited;

  FMarker := nil;
  FResourceID := Null;
  Grid.RowCount := 0;
  FSorted := False;
  F.FilterOptions := F.FilterOptions + [TFilterOption.foCaseInsensitive];

  FPicturePanel := TfrPicture.Create(Self);
  FPicturePanel.Parent := paPicture;
  FPicturePanel.Align := TAlignLayout.Client;
  FPicturePanel.Readonly := True;
  edFilterText.Visible := False;
  edFilterText.Position.X := 0;

  FGridHelper := TGridHelper.Create(Grid);
  InitColumns;
end;

destructor TResourcesDBGrid.Destroy;
begin
  FGridHelper.Free;

  inherited;
end;

function TResourcesDBGrid.GetMarkerID: Variant;
begin
  if Marker = nil then
    Result := Null
  else
    Result := Marker.ID;
end;

function TResourcesDBGrid.GetShowFilter: Boolean;
begin
  Result := edFilterText.Visible;
end;

procedure TResourcesDBGrid.SetShowFilter(const Value: Boolean);
begin
  edFilterText.Visible := Value;
end;

procedure TResourcesDBGrid.InitColumns;
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
    Column.Caption := 'Description';
    Column.FieldName := 'Description';
    Column.Alignment := TAlignment.taLeftJustify;
    Column.HeaderAlignment := TAlignment.taCenter;
    Column.AutoWidth := True;
  finally
    FGridHelper.AddColumn(Column);
  end;

  Column := TGridColumn.Create;
  try
    Column.Caption := 'Kind';
    Column.FieldName := 'Kind';
    Column.Alignment := TAlignment.taCenter;
    Column.HeaderAlignment := TAlignment.taCenter;
    Column.Width := 80;
  finally
    FGridHelper.AddColumn(Column);
  end;
end;

procedure TResourcesDBGrid.Init(const Marker: TDBMarker);
begin
  FMarker := Marker;

  FGridHelper.Binding(BindSourceDB1);

  F.Close;
  F.Connection := AppService.DBConnection.Connection;
  F.CachedUpdates := (FMarker <> nil) and FMarker.IsNewInstance;
  F.SQL.Text := GetCommandSQLText;
  F.SQLRefresh.Text := GetRefreshSQLText;
  F.ParamByName('Kind').AsInteger := TDBResource.KindToInt(ResourceKind);
  if F.FindParam('MarkerID') <> nil then
    F.ParamByName('MarkerID').Value := FMarker.ID;

  F.Open;

  FGridHelper.InitColumns;
end;

procedure TResourcesDBGrid.acAddResourceExecute(Sender: TObject);
begin
  AddRecord;
end;

procedure TResourcesDBGrid.acEditResourceExecute(Sender: TObject);
begin
  EditRecord;
end;

procedure TResourcesDBGrid.acDeleteResourceExecute(Sender: TObject);
begin
  DeleteRecord;
end;

procedure TResourcesDBGrid.SetPosition(const ResourceID: Variant);
begin
  if F.Active then
    F.Locate('ID', ResourceID, []);
end;

procedure TResourcesDBGrid.GridCellDblClick(const Column: TColumn; const Row: Integer);
begin
  EditRecord;
end;

procedure TResourcesDBGrid.ActionList1Update(Action: TBasicAction;  var Handled: Boolean);
begin
  acAddResource.Enabled := True;
  acEditResource.Enabled := Grid.RowCount > 0;
  acDeleteResource.Enabled := Grid.RowCount > 0;
end;

procedure TResourcesDBGrid.BindSourceDB1SubDataSourceDataChange(Sender: TObject; Field: TField);
begin
  FResourceID := GetResourceID;
  ResourceService.LoadPicture(ResourceID, ResourceKind, FPicturePanel.Picture);
  FPicturePanel.ResizePicture;
end;

procedure TResourcesDBGrid.edFilterTextChangeTracking(Sender: TObject);
var
  Filter: string;
begin
  if not F.Active then
    Exit;

  Filter := Trim(edFilterText.Text);
  if Filter <> '' then
    F.Filter := 'Description like ' + QuotedStr('%' + edFilterText.Text + '%')
  else
    F.Filter := '';

  F.Filtered := F.Filter <> '';
end;

end.
