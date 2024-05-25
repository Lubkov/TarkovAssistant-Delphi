unit ME.Frame.MapLevel;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants, 
  FMX.Types, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls,
  System.Actions, FMX.ActnList, FMX.Controls.Presentation, System.ImageList,
  FMX.ImgList, FMX.Objects, System.Rtti, FMX.Grid.Style, FMX.Grid, FMX.ScrollBox,
  ME.DB.Entity, ME.LocalMap, ME.MapLevel;

type
  TfrMapLevel = class(TFrame)
    ImageList1: TImageList;
    paTopPanel: TPanel;
    edAddMap: TSpeedButton;
    edEditMap: TSpeedButton;
    edDeleteMap: TSpeedButton;
    laTitle: TLabel;
    ActionList1: TActionList;
    acAddMapLevel: TAction;
    acEditMapLevel: TAction;
    acDeleteMapLevel: TAction;
    paPicture: TPanel;
    imMapPicture: TImage;
    Grid: TGrid;
    Column1: TColumn;
    Column2: TColumn;
    Column3: TColumn;
    Column4: TColumn;

    procedure GridGetValue(Sender: TObject; const ACol, ARow: Integer; var Value: TValue);
    procedure GridSelChanged(Sender: TObject);
    procedure ActionList1Update(Action: TBasicAction; var Handled: Boolean);
    procedure acAddMapLevelExecute(Sender: TObject);
    procedure acEditMapLevelExecute(Sender: TObject);
    procedure acDeleteMapLevelExecute(Sender: TObject);
    procedure GridCellDblClick(const Column: TColumn; const Row: Integer);
  private
    FLocalMap: TLocalMap;
    FFocusedIndex: Integer;

    function GetCount: Integer;
    function GetItem(Index: Integer): TMapLevel;
    function GetFocusedIndex: Integer;
    procedure SetFocusedIndex(const Value: Integer);
    function InternalMapLevelEdit(const MapLevel: TMapLevel): Boolean;
    procedure MapLevelEdit(const Index: Integer);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure Init(const LocalMap: TLocalMap);

    property Count: Integer read GetCount;
    property Items[Index: Integer]: TMapLevel read GetItem;
    property FocusedIndex: Integer read GetFocusedIndex write SetFocusedIndex;
  end;

implementation

{$R *.fmx}

uses
  ME.DB.Utils, ME.Dialog.Presenter, ME.Presenter.MapLevel, ME.Edit.MapLevel,
  ME.Dialog.Message;

{ TfrMapLevel }

constructor TfrMapLevel.Create(AOwner: TComponent);
begin
  inherited;

  Grid.RowCount := 0;
end;

destructor TfrMapLevel.Destroy;
begin

  inherited;
end;

procedure TfrMapLevel.Init(const LocalMap: TLocalMap);
begin
  FLocalMap := LocalMap;

  Grid.BeginUpdate;
  try
    Grid.RowCount := Count;
  finally
    Grid.EndUpdate;
  end;

  if Count > 0 then begin
    Grid.Selected := -1;
    Grid.Selected := 0;
  end
  else
    imMapPicture.Bitmap.Assign(nil);
end;

function TfrMapLevel.GetCount: Integer;
begin
  if FLocalMap <> nil then
    Result := FLocalMap.Levels.Count
  else
    Result := 0;
end;

function TfrMapLevel.GetItem(Index: Integer): TMapLevel;
begin
  Result := FLocalMap.Levels[Index];
end;

function TfrMapLevel.GetFocusedIndex: Integer;
begin
  if (FLocalMap = nil) or (Grid.Selected < 0) or (Grid.Selected >= Count) then
    Result := -1
  else
    Result := Grid.Selected;
end;

procedure TfrMapLevel.SetFocusedIndex(const Value: Integer);
begin
  if FFocusedIndex = Value then
    Exit;

  FFocusedIndex := Value;
  if FocusedIndex >= 0 then
    imMapPicture.Bitmap.Assign(Items[FocusedIndex].Picture)
  else
    imMapPicture.Bitmap.Assign(nil);
end;

function TfrMapLevel.InternalMapLevelEdit(const MapLevel: TMapLevel): Boolean;
var
  Presenter: TEditMapLevelPresenter;
  Dialog: TedMapLevel;
begin
  Dialog := TedMapLevel.Create(Self);
  try
    Presenter := TEditMapLevelPresenter.Create(Dialog, MapLevel);
    try
      Result := Presenter.Edit;
    finally
      Presenter.Free;
    end;
  finally
    Dialog.Free;
  end;
end;

procedure TfrMapLevel.MapLevelEdit(const Index: Integer);
var
  MapLevel: TMapLevel;
begin
  if (Index < 0) or (Index >= Count) then
    Exit;

  MapLevel := Items[Index];
  Grid.BeginUpdate;
  try
    InternalMapLevelEdit(MapLevel);
    imMapPicture.Bitmap.Assign(MapLevel.Picture);
  finally
    Grid.EndUpdate;
  end;
end;

procedure TfrMapLevel.GridGetValue(Sender: TObject; const ACol, ARow: Integer; var Value: TValue);
const
  ColumnKeyIdx = 0;
  ColumnMapIDIdx = 1;
  ColumnLevelIdx = 2;
  ColumnNameIdx = 3;
begin
  if Count <= ARow then
    Exit;

  case ACol of
    ColumnKeyIdx:
      Value := VarToStr(Items[ARow].ID);
    ColumnMapIDIdx:
      Value := VarToStr(Items[ARow].MapID);
    ColumnLevelIdx:
      Value := Items[ARow].Level;
    ColumnNameIdx:
      Value := Items[ARow].Name;
  end;
end;

procedure TfrMapLevel.GridSelChanged(Sender: TObject);
begin
  FocusedIndex := Grid.Selected;
end;

procedure TfrMapLevel.ActionList1Update(Action: TBasicAction; var Handled: Boolean);
begin
  acAddMapLevel.Enabled := FLocalMap <> nil;
  acEditMapLevel.Enabled := (FLocalMap <> nil) and (FocusedIndex >= 0);
  acDeleteMapLevel.Enabled := (FLocalMap <> nil) and (FocusedIndex >= 0);
end;

procedure TfrMapLevel.acAddMapLevelExecute(Sender: TObject);
var
  MapLevel: TMapLevel;
  Res: Boolean;
begin
  Res := False;
  MapLevel := TMapLevel.Create;
  try
    MapLevel.MapID := FLocalMap.ID;

    Res := InternalMapLevelEdit(MapLevel);
    if Res then begin
      FLocalMap.Levels.Add(MapLevel);

      Grid.BeginUpdate;
      try
        Grid.RowCount := Count;
      finally
        Grid.EndUpdate;
      end;
    end;
  finally
    if not Res then
      MapLevel.Free;
  end;
end;

procedure TfrMapLevel.acEditMapLevelExecute(Sender: TObject);
begin
  MapLevelEdit(Grid.Selected);
end;

procedure TfrMapLevel.acDeleteMapLevelExecute(Sender: TObject);
var
  MapLevel: TMapLevel;
  Presenter: TDelMapLevelPresenter;
  Dialog: TedMessage;
  Res: Boolean;
begin
  if (Grid.Selected < 0) or (Grid.Selected >= Count) then
    Exit;

  Res := False;
  MapLevel := Items[Grid.Selected];
  try
    Dialog := TedMessage.Create(Self);
    try
      Presenter := TDelMapLevelPresenter.Create(Dialog, MapLevel);
      try
        Res := Presenter.Delete;
        if Res then begin
          Grid.BeginUpdate;
          try
            FLocalMap.Levels.Delete(Grid.Selected);
            Grid.RowCount := Count;
          finally
            Grid.EndUpdate;
          end;
        end;
      finally
        Presenter.Free;
      end;
    finally
      Dialog.Free;
    end;
  finally
    if Res then
      MapLevel.Free;
  end;
end;

procedure TfrMapLevel.GridCellDblClick(const Column: TColumn; const Row: Integer);
begin
  MapLevelEdit(Row);
end;

end.
