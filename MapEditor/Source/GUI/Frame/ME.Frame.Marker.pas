unit ME.Frame.Marker;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants, 
  Generics.Collections, FMX.Types, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs,
  FMX.StdCtrls, System.ImageList, FMX.ImgList, System.Actions, FMX.ActnList,
  FMX.Controls.Presentation, System.Rtti, FMX.Grid.Style, FMX.Grid, FMX.ScrollBox,
  ME.DB.Map, ME.DB.Marker, Data.DB, MemDS, DBAccess, Uni, Fmx.Bind.Grid,
  System.Bindings.Outputs, Fmx.Bind.Editors, Data.Bind.EngExt, Fmx.Bind.DBEngExt,
  Data.Bind.Components, Data.Bind.Grid, Data.Bind.DBScope, ME.Grid.Helper;

type
  TfrMarkerGrid = class(TFrame)
    ActionList1: TActionList;
    acAddExtraction: TAction;
    acEditExtraction: TAction;
    acDeleteExtraction: TAction;
    paTopPanel: TPanel;
    edAddExtraction: TSpeedButton;
    edEditExtraction: TSpeedButton;
    edDeleteExtraction: TSpeedButton;
    laTitle: TLabel;
    ImageList1: TImageList;
    F: TUniQuery;
    FID: TIntegerField;
    FKind: TIntegerField;
    FLeft: TIntegerField;
    FTop: TIntegerField;
    BindSourceDB1: TBindSourceDB;
    Grid: TStringGrid;
    FDescription: TWideMemoField;
    FKindName: TWideStringField;

    procedure acAddExtractionExecute(Sender: TObject);
    procedure acEditExtractionExecute(Sender: TObject);
    procedure ActionList1Update(Action: TBasicAction; var Handled: Boolean);
    procedure acDeleteExtractionExecute(Sender: TObject);
    procedure FCalcFields(DataSet: TDataSet);
    procedure GridCellDblClick(const Column: TColumn; const Row: Integer);
  private
    FMapID: Variant;
    FGridHelper: TGridHelper;

    function InternalExtractionEdit(const Marker: TDBMarker): Boolean;
    procedure ExtractionEdit(const Index: Integer);
    procedure InitColumns;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure Init(const Map: TDBMap);
  end;

implementation

uses
  App.Service, ME.DB.Utils, ME.Service.Marker, ME.Presenter.Marker, ME.Edit.Marker,
  ME.Dialog.Message;

{$R *.fmx}

constructor TfrMarkerGrid.Create(AOwner: TComponent);
begin
  inherited;

  FMapID := Null;

  FGridHelper := TGridHelper.Create(Grid);
  InitColumns;
end;

destructor TfrMarkerGrid.Destroy;
begin
  FGridHelper.Free;

  inherited;
end;

procedure TfrMarkerGrid.InitColumns;
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
    Column.Visible := False;
    Column.Width := 80;
  finally
    FGridHelper.AddColumn(Column);
  end;

  Column := TGridColumn.Create;
  try
    Column.Caption := 'Left';
    Column.FieldName := 'Left';
    Column.Alignment := TAlignment.taCenter;
    Column.HeaderAlignment := TAlignment.taCenter;
    Column.Width := 80;
  finally
    FGridHelper.AddColumn(Column);
  end;

  Column := TGridColumn.Create;
  try
    Column.Caption := 'Top';
    Column.FieldName := 'Top';
    Column.Alignment := TAlignment.taCenter;
    Column.HeaderAlignment := TAlignment.taCenter;
    Column.Width := 80;
  finally
    FGridHelper.AddColumn(Column);
  end;

  Column := TGridColumn.Create;
  try
    Column.Caption := 'KindName';
    Column.FieldName := 'KindName';
    Column.Alignment := TAlignment.taCenter;
    Column.HeaderAlignment := TAlignment.taCenter;
    Column.Width := 120;
  finally
    FGridHelper.AddColumn(Column);
  end;
end;

procedure TfrMarkerGrid.Init(const Map: TDBMap);
const
  TitleFmt = 'Выходы с карты "%s"';
var
  i: Integer;
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
    ' SELECT ' + TDBMarker.FieldList +
    ' FROM ' + TDBMarker.EntityName +
    ' WHERE (MapID = :MapID)' +
    '      AND (Kind in (' + IntToStr(TDBMarker.KindToInt(TMarkerKind.PMCExtraction)) + ', ' +
                             IntToStr(TDBMarker.KindToInt(TMarkerKind.ScavExtraction)) + ', ' +
                             IntToStr(TDBMarker.KindToInt(TMarkerKind.CoopExtraction)) + ', ' +
                             IntToStr(TDBMarker.KindToInt(TMarkerKind.TransitExtraction)) + '))' +
    ' ORDER BY Kind, Description';
  F.ParamByName('MapID').Value := FMapID;
  F.Open;

  FGridHelper.InitColumns;
end;

procedure TfrMarkerGrid.FCalcFields(DataSet: TDataSet);
begin
  FKindName.AsString := TDBMarker.KindToStr(TDBMarker.IntToKind(FKind.AsInteger));
end;

procedure TfrMarkerGrid.GridCellDblClick(const Column: TColumn; const Row: Integer);
begin
  if not IsNullID(FID.Value) then
    ExtractionEdit(FID.Value);
end;

function TfrMarkerGrid.InternalExtractionEdit(const Marker: TDBMarker): Boolean;
var
  Presenter: TEditMarkerPresenter;
  Dialog: TedMarker;
begin
  Dialog := TedMarker.Create(Self);
  try
    Presenter := TEditMarkerPresenter.Create(Dialog, Marker);
    try
      Result := Presenter.Edit;
    finally
      Presenter.Free;
    end;
  finally
    Dialog.Free;
  end;
end;

procedure TfrMarkerGrid.ExtractionEdit(const Index: Integer);
var
  Marker: TDBMarker;
begin
  Marker := TDBMarker.Create;
  try
    if not MarkerService.GetAt(FID.Value, Marker) then
      Exit;

    if InternalExtractionEdit(Marker) then
      F.RefreshRecord;
  finally
    Marker.Free;
  end;
end;

procedure TfrMarkerGrid.acAddExtractionExecute(Sender: TObject);
var
  Marker: TDBMarker;
  Res: Boolean;
begin
  Marker := TDBMarker.Create;
  try
    Marker.MapID := FMapID;

//    MarkerService.Insert(Marker);
    Res := InternalExtractionEdit(Marker);
    if not Res then
      Exit;

    F.DisableControls;
    try
      F.Refresh;
      F.Last;
    finally
      F.EnableControls;
    end;
  finally
  //  if not Res then
//      MarkerService.Remove(Marker.ID);
    Marker.Free;
  end;
end;

procedure TfrMarkerGrid.acEditExtractionExecute(Sender: TObject);
begin
  ExtractionEdit(Grid.Selected);
end;

procedure TfrMarkerGrid.acDeleteExtractionExecute(Sender: TObject);
var
  Marker: TDBMarker;
  Presenter: TDelMarkerPresenter;
  Dialog: TedMessage;
begin
  if IsNullID(FID.Value) then
    Exit;

  Marker := TDBMarker.Create;
  try
    Marker.ID := FID.Value;
    Marker.MapID := FMapID;
    Marker.Description := FDescription.AsString;
    Marker.Kind := TDBMarker.IntToKind(FKind.AsInteger);

    Dialog := TedMessage.Create(Self);
    try
      Presenter := TDelMarkerPresenter.Create(Dialog, Marker);
      try
        if not Presenter.Delete then
          Exit;

        F.DisableControls;
        try
          F.Refresh;
        finally
          F.EnableControls;
        end;
      finally
        Presenter.Free;
      end;
    finally
      Dialog.Free;
    end;
  finally
    Marker.Free;
  end;
end;

procedure TfrMarkerGrid.ActionList1Update(Action: TBasicAction; var Handled: Boolean);
begin
  acAddExtraction.Enabled := not IsNullID(FMapID);
  acEditExtraction.Enabled := acAddExtraction.Enabled and not IsNullID(FID.Value);
  acDeleteExtraction.Enabled := acAddExtraction.Enabled and not IsNullID(FID.Value);
end;

end.
