unit ME.Frame.Marker;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants, 
  Generics.Collections, FMX.Types, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs,
  FMX.StdCtrls, System.ImageList, FMX.ImgList, System.Actions, FMX.ActnList,
  FMX.Controls.Presentation, System.Rtti, FMX.Grid.Style, FMX.Grid,
  FMX.ScrollBox, ME.DB.Marker, Data.DB, MemDS, DBAccess, Uni, Fmx.Bind.Grid,
  System.Bindings.Outputs, Fmx.Bind.Editors, Data.Bind.EngExt,
  Fmx.Bind.DBEngExt, Data.Bind.Components, Data.Bind.Grid, Data.Bind.DBScope;

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
    FCaption: TWideStringField;
    FKind: TIntegerField;
    FLeft: TIntegerField;
    FTop: TIntegerField;
    FKindName: TWideStringField;
    BindSourceDB1: TBindSourceDB;
    Grid: TStringGrid;
    LinkGridToDataSourceBindSourceDB1: TLinkGridToDataSource;
    BindingsList1: TBindingsList;
    procedure acAddExtractionExecute(Sender: TObject);
    procedure acEditExtractionExecute(Sender: TObject);
    procedure ActionList1Update(Action: TBasicAction; var Handled: Boolean);
    procedure acDeleteExtractionExecute(Sender: TObject);
    procedure FCalcFields(DataSet: TDataSet);
  private
    FMapID: Variant;

    function InternalExtractionEdit(const Marker: TDBMarker): Boolean;
    procedure ExtractionEdit(const Index: Integer);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure Init(const MapID: Variant);
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
//  Grid.RowCount := 0;
end;

destructor TfrMarkerGrid.Destroy;
begin

  inherited;
end;

procedure TfrMarkerGrid.Init(const MapID: Variant);
begin
  FMapID := MapID;

  F.Close;
  F.Connection := AppService.DBConnection.Connection;
  F.SQL.Text :=
    ' SELECT ' + TDBMarker.FieldList +
    ' FROM ' + TDBMarker.EntityName +
    ' WHERE (MapID = :MapID)' +
    '      AND (Kind in (0, 1, 2))' +
    ' ORDER BY Kind, Caption';
  F.ParamByName('MapID').Value := MapID;
  F.Open;
end;

procedure TfrMarkerGrid.FCalcFields(DataSet: TDataSet);
begin
  FKindName.AsString := TDBMarker.KindToStr(TMarkerKind(FKind.AsInteger));
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
//    if not Res then
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
    Marker.Caption := FCaption.AsString;
    Marker.Kind := TMarkerKind(FKind.AsInteger);

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
