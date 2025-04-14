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
  public
    constructor Create(AOwner: TComponent); override;

    procedure Init; override;

    property Trader: TTrader read GetTrader write SetTrader;
  end;

implementation

{$R *.fmx}

{ TTraderFilter }

constructor TTraderFilter.Create(AOwner: TComponent);
begin
  inherited;

  laFilterName.Text := 'Торговец:';
end;

function TTraderFilter.GetTrader: TTrader;
begin
  if KeyValue = Null then
    Result := TTrader.None
  else
    Result := TTrader(KeyValue);
end;

procedure TTraderFilter.SetTrader(const Value: TTrader);
begin
  KeyValue := Ord(Value);
end;

procedure TTraderFilter.Init;
var
  Trader: TTrader;
begin
  inherited;

  for Trader := TTrader.Prapor to TTrader.Lightkeeper do
    AddItem(Ord(Trader), TraderToStr(Trader));
end;

end.
