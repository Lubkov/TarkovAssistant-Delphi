unit ME.Filter.Trader;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  System.Actions, System.ImageList, FMX.Types, FMX.Graphics, FMX.Controls, FMX.Forms,
  FMX.Dialogs, FMX.StdCtrls, FMX.ListBox, FMX.Controls.Presentation, FMX.Layouts,
  FMX.ImgList, FMX.ActnList, ME.DB.Quest, ME.Trader;

type
  TTraderFilter = class(TFrame)
    laTraderName: TLabel;
    edTraderName: TComboBox;
    MainLayout: TLayout;
    ToolLayout: TLayout;
    ButtonLayout: TLayout;
    edEditMap: TSpeedButton;
    ImageList1: TImageList;
    ActionList1: TActionList;
    acClearFilter: TAction;
    procedure acClearFilterExecute(Sender: TObject);
    procedure edTraderNameChange(Sender: TObject);
  private
    FOnChange: TNotifyEvent;

    function GetTraderName: TTrader;
    procedure SetTraderName(const Value: TTrader);
    function GetClearActionVisible: Boolean;
    procedure SetClearActionVisible(const Value: Boolean);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    property TraderName: TTrader read GetTraderName write SetTraderName;
    property ClearActionVisible: Boolean read GetClearActionVisible write SetClearActionVisible;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;

implementation

{$R *.fmx}

{ TTraderFilter }

constructor TTraderFilter.Create(AOwner: TComponent);
var
  Trader: TTrader;
begin
  inherited;

  FOnChange := nil;

  edTraderName.Clear;
  for Trader := TTrader.Prapor to TTrader.Lightkeeper do
    edTraderName.Items.Add(TraderToStr(Trader));
end;

destructor TTraderFilter.Destroy;
begin
  FOnChange := nil;

  inherited;
end;

function TTraderFilter.GetTraderName: TTrader;
begin
  if edTraderName.ItemIndex >= 0 then
    Result := TTrader(edTraderName.ItemIndex + 1)
  else
    Result := TTrader.None;
end;

procedure TTraderFilter.SetTraderName(const Value: TTrader);
begin
  edTraderName.ItemIndex := Ord(Value) - 1;
end;

function TTraderFilter.GetClearActionVisible: Boolean;
begin
  Result := acClearFilter.Visible;
end;

procedure TTraderFilter.edTraderNameChange(Sender: TObject);
begin
  if Assigned(FOnChange) then
    FOnChange(Self);
end;

procedure TTraderFilter.SetClearActionVisible(const Value: Boolean);
begin
  acClearFilter.Visible := Value;
  ToolLayout.Visible := Value;
end;

procedure TTraderFilter.acClearFilterExecute(Sender: TObject);
begin
  edTraderName.ItemIndex := -1;
end;

end.
