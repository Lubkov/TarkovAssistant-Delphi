unit ME.Frame.Extraction;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants, 
  Generics.Collections, FMX.Types, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs,
  FMX.StdCtrls, System.ImageList, FMX.ImgList, System.Actions, FMX.ActnList,
  FMX.Controls.Presentation, System.Rtti, FMX.Grid.Style, FMX.Grid,
  FMX.ScrollBox, ME.DB.Entity, ME.MapTag;

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
  private
    FItems: TList<TEntity>;

    function GetCount: Integer;
    function GetItem(Index: Integer): TMapTag;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure Init;
    procedure Clear;

    property Count: Integer read GetCount;
    property Items[Index: Integer]: TMapTag read GetItem;
  end;

implementation

uses
  ME.MapTagService;

{$R *.fmx}

constructor TfrExtraction.Create(AOwner: TComponent);
begin
  inherited;

  FItems := TList<TEntity>.Create;
  Grid.RowCount := 0;
end;

destructor TfrExtraction.Destroy;
begin
  Clear;
  FItems.Free;

  inherited;
end;

function TfrExtraction.GetCount: Integer;
begin
  Result := FItems.Count;
end;

function TfrExtraction.GetItem(Index: Integer): TMapTag;
begin
  Result := TMapTag(FItems[Index]);
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

procedure TfrExtraction.Init;
begin
  MapTagService.GetAll(FItems);

  Grid.BeginUpdate;
  try
    Grid.RowCount := Count;
  finally
    Grid.EndUpdate;
  end;

  if Count > 0 then
    Grid.Selected := 0;
end;

procedure TfrExtraction.Clear;
var
  Item: TEntity;
begin
  for Item in FItems do
    Item.Free;

  FItems.Clear;
end;

end.
