unit ME.Frame.QuestPart;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants, 
  FMX.Types, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls,
  System.Rtti, FMX.Grid.Style, System.ImageList, FMX.ImgList, System.Actions,
  FMX.ActnList, FMX.Grid, FMX.ScrollBox, FMX.Controls.Presentation,
  ME.DB.Marker, Data.DB, MemDS, DBAccess, Uni, Fmx.Bind.Grid,
  System.Bindings.Outputs, Fmx.Bind.Editors, Data.Bind.EngExt,
  Fmx.Bind.DBEngExt, Data.Bind.Components, Data.Bind.Grid, Data.Bind.DBScope;

type
  TfrQuestPartGrid = class(TFrame)
    paTopPanel: TPanel;
    edAddMarker: TSpeedButton;
    edEditMarker: TSpeedButton;
    edDeleteMarker: TSpeedButton;
    laTitle: TLabel;
    ActionList1: TActionList;
    acAddMarker: TAction;
    acEditMarker: TAction;
    acDeleteMarker: TAction;
    ImageList1: TImageList;
    F: TUniQuery;
    FID: TIntegerField;
    FCaption: TWideStringField;
    FLeft: TIntegerField;
    FTop: TIntegerField;
    BindSourceDB1: TBindSourceDB;
    Grid: TStringGrid;
    LinkGridToDataSourceBindSourceDB1: TLinkGridToDataSource;
    BindingsList1: TBindingsList;
    procedure ActionList1Update(Action: TBasicAction; var Handled: Boolean);
    procedure acAddMarkerExecute(Sender: TObject);
    procedure acEditMarkerExecute(Sender: TObject);
    procedure acDeleteMarkerExecute(Sender: TObject);
    procedure GridCellDblClick(const Column: TColumn; const Row: Integer);
  private
    FMapID: Variant;
    FQuestID: Variant;
//    FFocusedIndex: Integer;

    function InternalMarkerEdit(const Point: TDBMarker): Boolean;
    procedure MarkerEdit(const Index: Integer);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure Init(const MapID, QuestID: Variant);
  end;

implementation

{$R *.fmx}

uses
  App.Service, ME.DB.Utils, ME.Service.Marker, ME.Edit.QuestPart, ME.Presenter.QuestPart,
  ME.Dialog.Message;

{ frQuestPartGrid }

constructor TfrQuestPartGrid.Create(AOwner: TComponent);
begin
  inherited;

  Grid.RowCount := 0;
end;

destructor TfrQuestPartGrid.Destroy;
begin

  inherited;
end;

procedure TfrQuestPartGrid.Init(const MapID, QuestID: Variant);
begin
  FMapID := MapID;
  FQuestID := QuestID;

  F.Close;
  F.Connection := AppService.DBConnection.Connection;
  F.SQL.Text :=
    ' SELECT ' + TDBMarker.FieldList +
    ' FROM ' + TDBMarker.EntityName +
    ' WHERE (MapID = :MapID) AND (QuestID = :QuestID)';
  F.ParamByName('MapID').Value := MapID;
  F.ParamByName('QuestID').Value := QuestID;
  F.Open;
end;

function TfrQuestPartGrid.InternalMarkerEdit(const Point: TDBMarker): Boolean;
var
  Presenter: TEditQuestPartPresenter;
  Dialog: TedQuestPart;
begin
  Dialog := TedQuestPart.Create(Self);
  try
    Presenter := TEditQuestPartPresenter.Create(Dialog, Point);
    try
      Result := Presenter.Edit;
    finally
      Presenter.Free;
    end;
  finally
    Dialog.Free;
  end;
end;

procedure TfrQuestPartGrid.MarkerEdit(const Index: Integer);
var
  Marker: TDBMarker;
begin
  Marker := TDBMarker.Create;
  try
    if not MarkerService.GetAt(FID.Value, Marker) then
      Exit;

    if InternalMarkerEdit(Marker) then
      F.RefreshRecord;
  finally
    Marker.Free;
  end;
end;

procedure TfrQuestPartGrid.acAddMarkerExecute(Sender: TObject);
var
  Marker: TDBMarker;
begin
  Marker := TDBMarker.Create;
  try
    Marker.MapID := FMapID;
    Marker.QuestID := FQuestID;
    if not InternalMarkerEdit(Marker) then
      Exit;

    F.DisableControls;
    try
      F.Refresh;
      F.Last;
    finally
      F.EnableControls;
    end;
  finally
    Marker.Free;
  end;
end;

procedure TfrQuestPartGrid.acEditMarkerExecute(Sender: TObject);
begin
  MarkerEdit(FID.Value);
end;

procedure TfrQuestPartGrid.acDeleteMarkerExecute(Sender: TObject);
var
  Marker: TDBMarker;
  Presenter: TDelQuestPartPresenter;
  Dialog: TedMessage;
//  Res: Boolean;
begin
  if IsNullID(FID.Value) then
    Exit;

  Marker := TDBMarker.Create;
  try
    Marker.ID := FID.Value;
    Marker.MapID := FMapID;
    Marker.Caption := FCaption.AsString;

    Dialog := TedMessage.Create(Self);
    try
      Presenter := TDelQuestPartPresenter.Create(Dialog, Marker);
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

procedure TfrQuestPartGrid.GridCellDblClick(const Column: TColumn; const Row: Integer);
begin
  if not IsNullID(FID.Value) then
    MarkerEdit(FID.Value);
end;

procedure TfrQuestPartGrid.ActionList1Update(Action: TBasicAction; var Handled: Boolean);
begin
  acAddMarker.Enabled := not IsNullID(FQuestID);
  acEditMarker.Enabled := acAddMarker.Enabled and not IsNullID(FID.Value);
  acDeleteMarker.Enabled := acAddMarker.Enabled and not IsNullID(FID.Value);
end;

end.
