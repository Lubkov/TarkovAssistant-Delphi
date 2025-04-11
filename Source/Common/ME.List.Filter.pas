unit ME.List.Filter;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  Generics.Collections, FMX.Types, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs,
  FMX.StdCtrls, ME.Form.Filter, System.Actions, FMX.ActnList, System.ImageList,
  FMX.ImgList, FMX.Controls.Presentation, FMX.Layouts, FMX.ListBox;

type
  TFilterItem = class
  private
    FKey: Variant;
    FCaption: string;
  public
    constructor Create; overload;
    constructor Create(const Key: Variant; const Caption: string); overload;

    property Key: Variant read FKey write FKey;
    property Caption: string read FCaption write FCaption;
  end;

  TListFilter = class(TFormFilter)
    edFilterValues: TComboBox;
    laFilterName: TLabel;

    procedure edFilterValuesChange(Sender: TObject);
  private
    FItems: TList<TFilterItem>;

    function GetIndex: Integer;
    procedure SetIndex(const Value: Integer);
  protected
    procedure SetKeyValue(const Value: Variant); override;
    function GetDisplayValue: string; override;
    procedure SetDisplayValue(const Value: string); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure AddItem(const Key: Variant; const Caption: string);
    function GetItem(const Key: Variant): TFilterItem;
    function IndexOf(const Key: Variant): Integer; overload;
    procedure Init; override;
    procedure LookupReload;

    property Index: Integer read GetIndex write SetIndex;
    property Items: TList<TFilterItem> read FItems;
  end;

implementation

{$R *.fmx}

{ TFilterItem }

constructor TFilterItem.Create;
begin
  inherited;

  FKey := Null;
  FCaption := '';
end;

constructor TFilterItem.Create(const Key: Variant; const Caption: string);
begin
  inherited Create;

  FKey := Key;
  FCaption := Caption;
end;

{ TListFilter }

constructor TListFilter.Create(AOwner: TComponent);
begin
  inherited;

  FItems := TObjectList<TFilterItem>.Create;
end;

destructor TListFilter.Destroy;
begin
  FItems.Free;

  inherited;
end;

function TListFilter.GetIndex: Integer;
begin
  Result := edFilterValues.ItemIndex;
end;

procedure TListFilter.SetIndex(const Value: Integer);
begin
  edFilterValues.ItemIndex := Value;
end;

procedure TListFilter.edFilterValuesChange(Sender: TObject);
begin
  if edFilterValues.ItemIndex >= 0 then
    KeyValue := FItems[edFilterValues.ItemIndex].Key
  else
    KeyValue := Null;
end;

procedure TListFilter.SetKeyValue(const Value: Variant);
var
  Index: Integer;
begin
  if (KeyValue = Value) or edFilterValues.IsUpdating then
    Exit;

  Index := IndexOf(Value);

  inherited;

  edFilterValues.ItemIndex := Index;
end;

function TListFilter.GetDisplayValue: string;
begin
  Result := edFilterValues.Text;
end;

procedure TListFilter.SetDisplayValue(const Value: string);
var
  Index: Integer;
begin
  Index := edFilterValues.ItemIndex;
  if Index <> -1 then begin
    FItems[Index].Caption := Value;
    edFilterValues.Items[Index] := Value;
  end;
end;

procedure TListFilter.AddItem(const Key: Variant; const Caption: string);
var
  Item: TFilterItem;
begin
  Item := TFilterItem.Create(Key, Caption);
  try
    edFilterValues.Items.Add(Caption);
  finally
    FItems.Add(Item);
  end;
end;

function TListFilter.GetItem(const Key: Variant): TFilterItem;
var
  Index: Integer;
begin
  Index := IndexOf(Key);
  if Index = -1 then
    Result := nil
  else
    Result := FItems[Index];
end;

function TListFilter.IndexOf(const Key: Variant): Integer;
var
  i: Integer;
  Item: TFilterItem;
begin
  Result := -1;

  if not (VarIsEmpty(Key) and VarIsNull(Key)) then
    for i := 0 to FItems.Count - 1 do begin
      Item := FItems[i];

      if VarSameValue(Item.Key, Key) then begin
        Result := i;
        Exit;
      end;
    end;
end;

procedure TListFilter.Init;
begin
  inherited;

  edFilterValues.Clear;
  FItems.Clear;
end;

procedure TListFilter.LookupReload;
var
  Item: TFilterItem;
  Position: Integer;
begin
  edFilterValues.BeginUpdate;
  try
    Position := Index;
    edFilterValues.Items.Clear;

    for Item in FItems do
      edFilterValues.Items.Add(Item.Caption);

    Index := Position;
  finally
    edFilterValues.EndUpdate;
  end;
end;

end.
