unit ME.Form.Filter;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants, 
  FMX.Types, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls,
  System.ImageList, FMX.ImgList, FMX.Controls.Presentation, FMX.Layouts,
  System.Actions, FMX.ActnList;

type
  TFormFilter = class(TFrame)
    ImageList1: TImageList;
    MainLayout: TLayout;
    ToolLayout: TLayout;
    edClearFilter: TSpeedButton;
    ActionList1: TActionList;
    edEditItem: TSpeedButton;
    edAddItem: TSpeedButton;
    edDeleteItem: TSpeedButton;
    Action1: TAction;
  private
    FKeyValue: Variant;
    FOnChange: TNotifyEvent;
    FAddItem: TAction;
    FEditItem: TAction;
    FDeleteItem: TAction;
    FClearFilter: TAction;
  protected
    function GetKeyValue: Variant; virtual;
    procedure SetKeyValue(const Value: Variant); virtual;
    function GetDisplayValue: string; virtual;
    procedure SetDisplayValue(const Value: string); virtual;
    procedure DoChange;
    procedure UpdateButton(const Button: TSpeedButton; var Offset: Single);
    procedure UpdateActions(Sender: TObject);
    procedure ClearFilterExecute(Sender: TObject); virtual;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure Init; virtual;

    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    property KeyValue: Variant read GetKeyValue write SetKeyValue;
    property DisplayValue: string read GetDisplayValue write SetDisplayValue;
    property AddItem: TAction read FAddItem;
    property EditItem: TAction read FEditItem;
    property DeleteItem: TAction read FDeleteItem;
    property ClearFilter: TAction read FClearFilter;
  end;

implementation

const
  ClearFilterImageIndex = 0;
  AddItemImageIndex = 1;
  EditItemImageIndex = 2;
  DelItemImageIndex = 3;

  ButtonPositionTop = 14;
  ButtonMarginLeft = 4;
  ButtonWidth = 30;

{$R *.fmx}

{ TFormFilter }

constructor TFormFilter.Create(AOwner: TComponent);
begin
  inherited;

  FKeyValue := Null;
  FOnChange := nil;

  FClearFilter := TAction.Create(Self);
  FClearFilter.Caption := '';
  FClearFilter.ImageIndex := ClearFilterImageIndex;
  FClearFilter.OnExecute := ClearFilterExecute;
  FClearFilter.ActionList := ActionList1;
  FClearFilter.OnUpdate := UpdateActions;
  FClearFilter.Visible := True;

  FAddItem := TAction.Create(Self);
  FAddItem.Caption := '';
  FAddItem.ImageIndex := AddItemImageIndex;
  FAddItem.OnExecute := nil;
  FAddItem.ActionList := ActionList1;
  FAddItem.OnUpdate := UpdateActions;
  FAddItem.Visible := False;

  FEditItem := TAction.Create(Self);
  FEditItem.Caption := '';
  FEditItem.ImageIndex := EditItemImageIndex;
  FEditItem.OnExecute := nil;
  FEditItem.ActionList := ActionList1;
  FEditItem.OnUpdate := UpdateActions;
  FEditItem.Visible := False;

  FDeleteItem := TAction.Create(Self);
  FDeleteItem.Caption := '';
  FDeleteItem.ImageIndex := DelItemImageIndex;
  FDeleteItem.OnExecute := nil;
  FDeleteItem.ActionList := ActionList1;
  FDeleteItem.OnUpdate := UpdateActions;
  FDeleteItem.Visible := False;

  edAddItem.Action := AddItem;
  edEditItem.Action := EditItem;
  edDeleteItem.Action := DeleteItem;
  edClearFilter.Action := ClearFilter;
end;

destructor TFormFilter.Destroy;
begin
  FOnChange := nil;

  inherited;
end;

function TFormFilter.GetKeyValue: Variant;
begin
  Result := FKeyValue;
end;

procedure TFormFilter.SetKeyValue(const Value: Variant);
begin
  if FKeyValue <> Value then begin
    FKeyValue := Value;

    DoChange;
  end;
end;

function TFormFilter.GetDisplayValue: string;
begin
  Result := '';
end;

procedure TFormFilter.SetDisplayValue(const Value: string);
begin

end;

procedure TFormFilter.DoChange;
begin
  if Assigned(FOnChange) then
    FOnChange(Self);
end;

procedure TFormFilter.UpdateButton(const Button: TSpeedButton; var Offset: Single);
begin
  if not Button.Visible then
    Exit;

  Button.Position.X := Offset;
  Button.Position.Y := ButtonPositionTop;
  Offset := Offset + Button.Width + ButtonMarginLeft;
end;

procedure TFormFilter.UpdateActions(Sender: TObject);
var
  Offset: Single;
begin
  ToolLayout.Visible := FAddItem.Visible or FEditItem.Visible or FDeleteItem.Visible or ClearFilter.Visible;

  Offset := ButtonMarginLeft;

  UpdateButton(edAddItem, Offset);
  UpdateButton(edEditItem, Offset);
  UpdateButton(edDeleteItem, Offset);
  UpdateButton(edClearFilter, Offset);

  ToolLayout.Width := Offset;
end;

procedure TFormFilter.ClearFilterExecute(Sender: TObject);
begin
  KeyValue := Null;
end;

procedure TFormFilter.Init;
begin
  UpdateActions(Self);
end;

end.
