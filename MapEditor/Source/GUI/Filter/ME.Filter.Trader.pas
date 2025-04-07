unit ME.Filter.Trader;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants, 
  FMX.Types, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls,
  FMX.ListBox, FMX.Controls.Presentation, ME.DB.Quest, FMX.Layouts,
  System.ImageList, FMX.ImgList, System.Actions, FMX.ActnList;

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
  private
    function GetTraderName: TTrader;
    procedure SetTraderName(const Value: TTrader);
    function GetClearActionVisible: Boolean;
    procedure SetClearActionVisible(const Value: Boolean);
  public
    constructor Create(AOwner: TComponent); override;

    property TraderName: TTrader read GetTraderName write SetTraderName;
    property ClearActionVisible: Boolean read GetClearActionVisible write SetClearActionVisible;
  end;

implementation

{$R *.fmx}

{ TTraderFilter }

constructor TTraderFilter.Create(AOwner: TComponent);
var
  Trader: TTrader;
begin
  inherited;

  edTraderName.Clear;
  for Trader := TTrader.Prapor to TTrader.Lightkeeper do
    edTraderName.Items.Add(TDBQuest.TraderToStr(Trader));
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
