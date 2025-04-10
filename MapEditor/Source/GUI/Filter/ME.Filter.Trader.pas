unit ME.Filter.Trader;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants, 
  FMX.Types, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls,
  ME.Form.Filter, System.Actions, FMX.ActnList, System.ImageList, FMX.ImgList,
  FMX.Controls.Presentation, FMX.Layouts, FMX.ListBox,
  ME.List.Filter, ME.Trader;

type
  TTraderFilter = class(TListFilter)
  private
    function GetTrader: TTrader;
    procedure SetTrader(const Value: TTrader);
  protected
    function GetDisplayValue: Variant; override;
    procedure SetDisplayValue(const Value: Variant); override;
  public
    procedure Init; override;

    property Trader: TTrader read GetTrader write SetTrader;
  end;

implementation

{$R *.fmx}

{ TTraderFilter }

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

function TTraderFilter.GetDisplayValue: Variant;
begin
  Result := edFilterValues.ItemIndex + 1;
end;

procedure TTraderFilter.SetDisplayValue(const Value: Variant);
begin
  edFilterValues.ItemIndex := Value - 1;
end;

procedure TTraderFilter.Init;
var
  Trader: TTrader;
begin
  inherited;

  edFilterValues.Clear;
  for Trader := TTrader.Prapor to TTrader.Lightkeeper do
    edFilterValues.Items.Add(TraderToStr(Trader));
end;

end.
