unit ME.Filter.Trader;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants, 
  FMX.Types, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls,
  ME.Form.Filter, System.Actions, FMX.ActnList, System.ImageList, FMX.ImgList,
  FMX.Controls.Presentation, FMX.Layouts, FMX.ListBox, ME.Trader;

type
  TTraderFilter = class(TFormFilter)
    laTraderName: TLabel;
    edTraderName: TComboBox;

    procedure edTraderNameChange(Sender: TObject);
  private
    function GetTrader: TTrader;
    procedure SetTrader(const Value: TTrader);
  protected
    function GetKeyValue: Variant; override;
    procedure SetKeyValue(const Value: Variant); override;
  public
    procedure Init; override;

    property Trader: TTrader read GetTrader write SetTrader;
  end;

implementation

{$R *.fmx}

{ TTraderFilter }

procedure TTraderFilter.edTraderNameChange(Sender: TObject);
begin
  DoChange;
end;

function TTraderFilter.GetTrader: TTrader;
begin
  if KeyValue = Null then
    Result := TTrader.None
  else
    Result := TTrader(KeyValue + 1);
end;

procedure TTraderFilter.SetTrader(const Value: TTrader);
begin
  KeyValue := Ord(Value) - 1;
end;

function TTraderFilter.GetKeyValue: Variant;
begin
  if edTraderName.ItemIndex >= 0 then
    Result := edTraderName.ItemIndex + 1
  else
    Result := Null;
end;

procedure TTraderFilter.SetKeyValue(const Value: Variant);
begin
  if VarIsEmpty(Value) or VarIsNull(Value) then
    edTraderName.ItemIndex := -1
  else
    edTraderName.ItemIndex := Ord(Integer(Value)) - 1;
end;

procedure TTraderFilter.Init;
var
  Trader: TTrader;
begin
  inherited;

  edTraderName.Clear;
  for Trader := TTrader.Prapor to TTrader.Lightkeeper do
    edTraderName.Items.Add(TraderToStr(Trader));
end;

end.
