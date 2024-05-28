unit ME.Frame.Extraction;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants, 
  Generics.Collections, FMX.Types, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs,
  FMX.StdCtrls, System.ImageList, FMX.ImgList, System.Actions, FMX.ActnList,
  FMX.Controls.Presentation, System.Rtti, FMX.Grid.Style, FMX.Grid,
  FMX.ScrollBox, ME.DB.Entity, ME.LocalMap, ME.MapTag;

type
  TfrExtraction = class(TFrame)
    ActionList1: TActionList;
    acAddExtraction: TAction;
    acEditExtraction: TAction;
    acDeleteExtraction: TAction;
    ImageList1: TImageList;
    paTopPanel: TPanel;
    edAddMap: TSpeedButton;
    edEditMap: TSpeedButton;
    edDeleteMap: TSpeedButton;
    laTitle: TLabel;
    Grid: TGrid;
    Column1: TColumn;
    StringColumn1: TStringColumn;
    IntegerColumn1: TIntegerColumn;
    StringColumn2: TStringColumn;
    IntegerColumn2: TIntegerColumn;

    procedure GridGetValue(Sender: TObject; const ACol, ARow: Integer; var Value: TValue);
    procedure acAddExtractionExecute(Sender: TObject);
  private
    FLocalMap: TLocalMap;

    function GetCount: Integer;
    function GetItem(Index: Integer): TMapTag;
    function InternalExtractionEdit(const MapTag: TMapTag): Boolean;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure Init(const LocalMap: TLocalMap);

    property Count: Integer read GetCount;
    property Items[Index: Integer]: TMapTag read GetItem;
  end;

implementation

uses
  ME.MapTagService, ME.Presenter.Extraction, ME.Edit.Extraction;

{$R *.fmx}

constructor TfrExtraction.Create(AOwner: TComponent);
begin
  inherited;

  Grid.RowCount := 0;
end;

destructor TfrExtraction.Destroy;
begin

  inherited;
end;

function TfrExtraction.GetCount: Integer;
begin
  Result := FLocalMap.Tags.Count;
end;

function TfrExtraction.GetItem(Index: Integer): TMapTag;
begin
  Result := FLocalMap.Tags[Index];
end;

function TfrExtraction.InternalExtractionEdit(const MapTag: TMapTag): Boolean;
var
  Presenter: TEditExtractionPresenter;
  Dialog: TedExtraction;
begin
  Dialog := TedExtraction.Create(Self);
  try
    Presenter := TEditExtractionPresenter.Create(Dialog, MapTag);
    try
      Result := Presenter.Edit;
    finally
      Presenter.Free;
    end;
  finally
    Dialog.Free;
  end;
end;

procedure TfrExtraction.GridGetValue(Sender: TObject; const ACol, ARow: Integer; var Value: TValue);
const
  ColumnKeyIdx = 0;
  ColumnNameIdx = 1;
  ColumnKindIdx = 2;
  ColumnPositionXIdx = 3;
  ColumnPositionYIdx = 4;
begin
  if Count <= ARow then
    Exit;

  case ACol of
    ColumnKeyIdx:
      Value := VarToStr(Items[ARow].ID);
    ColumnNameIdx:
      Value := VarToStr(Items[ARow].Name);
    ColumnKindIdx:
      Value := TMapTag.KindToStr(Items[ARow].Kind);
    ColumnPositionXIdx:
      Value := Items[ARow].Position.X;
    ColumnPositionYIdx:
      Value := Items[ARow].Position.Y;
  end;
end;

procedure TfrExtraction.Init(const LocalMap: TLocalMap);
begin
  FLocalMap := LocalMap;

  Grid.BeginUpdate;
  try
    Grid.RowCount := Count;
  finally
    Grid.EndUpdate;
  end;

  if Count > 0 then
    Grid.Selected := 0;
end;

procedure TfrExtraction.acAddExtractionExecute(Sender: TObject);
var
  MapTag: TMapTag;
  Res: Boolean;
begin
  Res := False;
  MapTag := TMapTag.Create;
  try
    MapTag.MapID := FLocalMap.ID;

    Res := InternalExtractionEdit(MapTag);
    if Res then begin
      FLocalMap.Tags.Add(MapTag);

      Grid.BeginUpdate;
      try
        Grid.RowCount := Count;
      finally
        Grid.EndUpdate;
      end;
    end;
  finally
    if not Res then
      MapTag.Free;
  end;
end;

end.
