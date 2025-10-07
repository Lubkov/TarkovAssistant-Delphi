unit ME.Grid.Helper;

interface

uses
  System.SysUtils, System.Types, System.Classes, Generics.Collections,
  FMX.Types, FMX.Grid, Data.Bind.Grid, Data.Bind.Components;

type
  TGridColumn = class
  private
    FAlignment: TAlignment;
    FAutoWidth: Boolean;
    FCaption: string;
    FFieldName: string;
    FHeaderAlignment: TAlignment;
    FVisible: Boolean;
    FWidth: Integer;
  public
    constructor Create;

    property Alignment: TAlignment read FAlignment write FAlignment;
    property AutoWidth: Boolean read FAutoWidth write FAutoWidth;
    property Caption: string read FCaption write FCaption;
    property FieldName: string read FFieldName write FFieldName;
    property HeaderAlignment: TAlignment read FHeaderAlignment write FHeaderAlignment;
    property Visible: Boolean read FVisible write FVisible;
    property Width: Integer read FWidth write FWidth;
  end;

  TGridHelper = class
  private
    FGrid: TCustomGrid;
    FColumns: TList<TGridColumn>;
    FBindingsList: TBindingsList;
    FLink: TLinkGridToDataSource;
  public
    constructor Create(const Grid: TCustomGrid);
    destructor Destroy; override;

    procedure AddColumn(const Value: TGridColumn); overload;
    procedure Clear;

    procedure Binding(const DataSource: TBaseLinkingBindSource);
    procedure InitColumns;

    property Columns: TList<TGridColumn> read FColumns;
  end;

implementation

{ TGridColumn }

constructor TGridColumn.Create;
begin
  inherited;

  FVisible := True;
  FWidth := 100;
end;

{ TGridHelper }

constructor TGridHelper.Create(const Grid: TCustomGrid);
begin
  inherited Create;

  FGrid := Grid;
  FBindingsList := TBindingsList.Create(Grid);
  FLink := TLinkGridToDataSource.Create(FBindingsList);
  FLink.GridControl := FGrid;

  FColumns := TObjectList<TGridColumn>.Create;
end;

destructor TGridHelper.Destroy;
begin
  FColumns.Clear;
  FColumns.Free;

  inherited;
end;

procedure TGridHelper.AddColumn(const Value: TGridColumn);
begin
  FColumns.Add(Value);
end;

procedure TGridHelper.Clear;
begin
  FColumns.Clear;
  FLink.Columns.Clear;
  FGrid.ClearColumns;
end;

procedure TGridHelper.Binding(const DataSource: TBaseLinkingBindSource);
const
  DefScrollBarWidth = 24;
var
  Column: TGridColumn;
  DSColumn: TLinkGridToDataSourceColumn;
  Items: TList<TLinkGridToDataSourceColumn>;
  Width: Integer;
begin
  if FLink.Columns.Count > 0 then
    Exit;

  FGrid.ClearColumns;
  FLink.DataSource := DataSource;

  Width := 0;
  Items := TList<TLinkGridToDataSourceColumn>.Create;
  try
    for Column in FColumns do begin
      DSColumn := FLink.Columns.Add;
      DSColumn.MemberName := Column.FieldName;
      DSColumn.Alignment := Column.Alignment;
      DSColumn.Header := Column.Caption;
      DSColumn.Visible := Column.Visible;

      if Column.AutoWidth then
        Items.Add(DSColumn)
      else begin
        DSColumn.Width := Column.Width;
        if Column.Visible then
          Inc(Width, Column.Width);
      end;
    end;

    if Items.Count = 0 then
      Exit;

    Width := Trunc((FGrid.Width - Width - DefScrollBarWidth - 2) / Items.Count);
    for DSColumn in Items do
      DSColumn.Width := Width;
  finally
    Items.Free;
  end;
end;

procedure TGridHelper.InitColumns;
var
  i: Integer;
  Column: TGridColumn;
  TextAlign: TTextAlign;
begin
  if FColumns.Count <> FGrid.ColumnCount then
    Exit;

  for i := 0 to FColumns.Count - 1 do begin
    Column := FColumns[i];

    case Column.HeaderAlignment of
      TAlignment.taRightJustify:
        TextAlign := TTextAlign.Trailing;
      TAlignment.taCenter:
        TextAlign := TTextAlign.Center;
    else
      TextAlign := TTextAlign.Leading;
    end;
    FGrid.Columns[i].HeaderSettings.TextSettings.HorzAlign := TextAlign;
  end;
end;

end.
