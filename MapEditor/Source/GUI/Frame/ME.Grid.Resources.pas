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
  ME.Frame.Picture, ME.DB.Presenter.Resource;

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
    BindSourceDB1: TBindSourceDB;
    Grid: TStringGrid;
    LinkGridToDataSourceBindSourceDB1: TLinkGridToDataSource;
    GridBindings: TBindingsList;
    FDescription: TWideStringField;
    paPicture: TPanel;
    procedure acEditResourceExecute(Sender: TObject);
    procedure acAddResourceExecute(Sender: TObject);
    procedure acDeleteResourceExecute(Sender: TObject);
    procedure GridCellDblClick(const Column: TColumn; const Row: Integer);
    procedure ActionList1Update(Action: TBasicAction; var Handled: Boolean);
    procedure BindSourceDB1SubDataSourceDataChange(Sender: TObject; Field: TField);
  private
    FMarker: TDBMarker;
    FResourceID: Variant;
    FPicturePanel: TfrPicture;

    function GetMarkerID: Variant;
  protected
    function GetResourceKind: TResourceKind; virtual; abstract;
    function GetCommandSQLText: string; virtual; abstract;
    function GetRefreshSQLText: string; virtual; abstract;
    function GetResourceID: Variant; virtual; abstract;
    function GetEditPresenterClass: TEditResourcePresenterClass; virtual; abstract;
    function GetDelPresenterClass: TDelResourcePresenterClass; virtual; abstract;
//    function InternalResourceAdd(const Resource: TDBResource): Boolean;
  protected
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

  FPicturePanel := TfrPicture.Create(Self);
  FPicturePanel.Parent := paPicture;
  FPicturePanel.Align := TAlignLayout.Client;
  FPicturePanel.Readonly := True;
end;

destructor TResourcesDBGrid.Destroy;
begin

  inherited;
end;

//function TResourcesDBGrid.GetCommandSQLText: string;
//begin
//  Result :=
//    ' SELECT r.ID as ID, ' +
//    '        r.Kind as Kind, ' +
//    '        r.Description as Description ' +
//    ' FROM Resource r ';
//
////  Result := 'SELECT r.ID as ID, r.Kind as Kind, r.Description as Description FROM Resource r ';
////  if FMarker = nil then
////    Result := Result + ' WHERE (1 = 1)'
////  else
////    case ResourceKind of
////      TResourceKind.Screenshot:
////        Result := Result + ' WHERE (r.MarkerID = :MarkerID)';
////      TResourceKind.QuestItem:
////        Result := Result +
////          ' INNER JOIN QuestItem qi ON (qi.ResourceID = r.ID) AND (qi.MarkerID = :MarkerID)';
////    end;
////  Result := Result + ' AND (r.Kind = :Kind)';
//end;

function TResourcesDBGrid.GetMarkerID: Variant;
begin
  if Marker = nil then
    Result := Null
  else
    Result := Marker.ID;
end;

procedure TResourcesDBGrid.Init(const Marker: TDBMarker);
begin
  FMarker := Marker;

  F.Close;
  F.Connection := AppService.DBConnection.Connection;
  F.CachedUpdates := (FMarker <> nil) and FMarker.IsNewInstance;
  F.SQL.Text := GetCommandSQLText;
  F.SQLRefresh.Text := GetRefreshSQLText;
  F.ParamByName('Kind').AsInteger := Ord(ResourceKind);
  if F.FindParam('MarkerID') <> nil then
    F.ParamByName('MarkerID').Value := FMarker.ID;

  F.Open;
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

end.
