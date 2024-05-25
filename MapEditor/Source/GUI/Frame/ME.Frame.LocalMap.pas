unit ME.Frame.LocalMap;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  Generics.Collections, FMX.Types, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs,
  FMX.StdCtrls, System.ImageList, FMX.ImgList, FMX.Controls.Presentation, FMX.Styles.Objects,
  FMX.Objects, FMX.ListView.Types, FMX.ListView.Appearances, FMX.ListView.Adapters.Base,
  FMX.ListView, FMX.Layouts, System.Rtti, FMX.Grid.Style, FMX.Grid, FMX.ScrollBox,
  ME.DB.Entity, ME.LocalMap, System.Actions, FMX.ActnList;

type
  TOnChangeEvent = procedure (const LocalMap: TLocalMap) of object;

  TfrLocalMap = class(TFrame)
    paTopPanel: TPanel;
    edAddMap: TSpeedButton;
    ImageList1: TImageList;
    edEditMap: TSpeedButton;
    edDeleteMap: TSpeedButton;
    laTitle: TLabel;
    Grid: TGrid;
    Column1: TColumn;
    Column2: TColumn;
    Column3: TColumn;
    Column4: TColumn;
    Column5: TColumn;
    Column6: TColumn;
    ActionList1: TActionList;
    acAddMap: TAction;
    acEditMap: TAction;
    acDeleteMap: TAction;
    procedure GridGetValue(Sender: TObject; const ACol, ARow: Integer; var Value: TValue);
    procedure GridCellDblClick(const Column: TColumn; const Row: Integer);
    procedure acAddMapExecute(Sender: TObject);
    procedure acEditMapExecute(Sender: TObject);
    procedure acDeleteMapExecute(Sender: TObject);
    procedure GridSelChanged(Sender: TObject);
  private
    FItems: TList<TEntity>;
    FOnChange: TOnChangeEvent;

    function GetCount: Integer;
    function GetItem(Index: Integer): TLocalMap;
    function InternalMapEdit(const LocalMap: TLocalMap): Boolean;
    procedure MapEdit(const Index: Integer);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure Init;
    procedure Clear;

    property Count: Integer read GetCount;
    property Items[Index: Integer]: TLocalMap read GetItem;
    property OnChange: TOnChangeEvent read FOnChange write FOnChange;
  end;

implementation

uses
  ME.DB.Utils, ME.Dialog.Presenter, ME.Presenter.LocalMap, ME.Edit.LocalMap,
  ME.LocalMapService, ME.Dialog.Message;

{$R *.fmx}

{ TfrLocalMap }

constructor TfrLocalMap.Create(AOwner: TComponent);
begin
  inherited;

  FItems := TList<TEntity>.Create;
  Grid.RowCount := 0;
  FOnChange := nil;
end;

destructor TfrLocalMap.Destroy;
begin
  FOnChange := nil;
  Clear;
  FItems.Free;

  inherited;
end;

function TfrLocalMap.GetCount: Integer;
begin
  Result := FItems.Count;
end;

function TfrLocalMap.GetItem(Index: Integer): TLocalMap;
begin
  Result := TLocalMap(FItems[Index]);
end;

function TfrLocalMap.InternalMapEdit(const LocalMap: TLocalMap): Boolean;
var
  Presenter: TEditMapPresenter;
  Dialog: TedLocalMap;
begin
  LocalMapService.LoadMapLevels(LocalMap, True);

  Dialog := TedLocalMap.Create(Self);
  try
    Presenter := TEditMapPresenter.Create(Dialog, LocalMap);
    try
      Result := Presenter.Edit;
    finally
      Presenter.Free;
    end;
  finally
    Dialog.Free;
  end;
end;

procedure TfrLocalMap.MapEdit(const Index: Integer);
var
  LocalMap: TLocalMap;
begin
  if (Index < 0) or (Index >= Count) then
    Exit;

  LocalMap := Items[Index];
  Grid.BeginUpdate;
  try
    InternalMapEdit(LocalMap);
  finally
    Grid.EndUpdate;
  end;
end;

procedure TfrLocalMap.GridGetValue(Sender: TObject; const ACol, ARow: Integer; var Value: TValue);
const
  ColumnKeyIdx = 0;
  ColumnNameIdx = 1;
  ColumnLeftIdx = 2;
  ColumnTopIdx = 3;
  ColumnRightIdx = 4;
  ColumnBottomIdx = 5;
begin
  if FItems.Count <= ARow then
    Exit;

  case ACol of
    ColumnKeyIdx:
      Value := VarToStr(Items[ARow].ID);
    ColumnNameIdx:
      Value := Items[ARow].Name;
    ColumnLeftIdx:
      Value := Items[ARow].Left.X;
    ColumnTopIdx:
      Value := Items[ARow].Left.Y;
    ColumnRightIdx:
      Value := Items[ARow].Right.X;
    ColumnBottomIdx:
      Value := Items[ARow].Right.Y;
  end;
end;

procedure TfrLocalMap.GridSelChanged(Sender: TObject);
var
  Item: TLocalMap;
begin
  if not Assigned(FOnChange) then
    Exit;

  if (Grid.Selected < 0) or (Grid.Selected >= Count) then
    Item := nil
  else
    Item := Items[Grid.Selected];

  FOnChange(Item);
end;

procedure TfrLocalMap.GridCellDblClick(const Column: TColumn; const Row: Integer);
begin
  MapEdit(Row);
end;

procedure TfrLocalMap.Init;
begin
  LocalMapService.GetAll(FItems);

  Grid.BeginUpdate;
  try
    Grid.RowCount := Count;
  finally
    Grid.EndUpdate;
  end;

  if Count > 0 then
    Grid.Selected := 0;
end;

procedure TfrLocalMap.Clear;
var
  Item: TEntity;
begin
  for Item in FItems do
    Item.Free;

  FItems.Clear;
end;

procedure TfrLocalMap.acAddMapExecute(Sender: TObject);
var
  LocalMap: TLocalMap;
  Res: Boolean;
begin
  Res := False;
  LocalMap := TLocalMap.Create;
  try
    Res := InternalMapEdit(LocalMap);
    if Res then begin
      FItems.Add(LocalMap);

      Grid.BeginUpdate;
      try
        Grid.RowCount := Count;
      finally
        Grid.EndUpdate;
      end;
    end;
  finally
    if not Res then
      LocalMap.Free;
  end;
end;

procedure TfrLocalMap.acEditMapExecute(Sender: TObject);
begin
  MapEdit(Grid.Selected);
end;

procedure TfrLocalMap.acDeleteMapExecute(Sender: TObject);
var
  LocalMap: TLocalMap;
  Presenter: TDelMapPresenter;
  Dialog: TedMessage;
  Res: Boolean;
begin
  if (Grid.Selected < 0) or (Grid.Selected >= Count) then
    Exit;

  Res := False;
  LocalMap := Items[Grid.Selected];
  try
    Dialog := TedMessage.Create(Self);
    try
      Presenter := TDelMapPresenter.Create(Dialog, LocalMap);
      try
        Res := Presenter.Delete;
        if Res then begin
          Grid.BeginUpdate;
          try
            FItems.Delete(Grid.Selected);
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
      LocalMap.Free;
  end;
end;

end.
