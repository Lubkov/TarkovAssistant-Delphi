unit fruLocation;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, cxGraphics, cxControls,
  cxLookAndFeels, cxLookAndFeelPainters, cxContainer, cxEdit, dxSkinsCore,
  dxSkinOffice2016Dark, dxGDIPlusClasses, cxImage, LocalMap, Vcl.ExtCtrls,
  cxLabel;

type
  TLocationChangedEvent = procedure (Value: TLocalMap) of object;

  TfrLocation = class(TFrame)
    im—ustoms: TcxImage;
    pa—ustoms: TPanel;
    la—ustoms: TcxLabel;
    paWoods: TPanel;
    laWoods: TcxLabel;
    imWoods: TcxImage;
    paInterchange: TPanel;
    laInterchange: TcxLabel;
    imInterchange: TcxImage;
    paShoreline: TPanel;
    laShoreline: TcxLabel;
    imShoreline: TcxImage;
    paFirstLine: TPanel;
    paSecondLine: TPanel;
    paReserve: TPanel;
    imReserve: TcxImage;
    laReserve: TcxLabel;
  private
    FOnLocationChanged: TLocationChangedEvent;

    procedure DoLocationChanged(Value: TLocalMap);
    procedure OnCustomsClick(Sender: TObject);
    procedure OnWoodsClick(Sender: TObject);
    procedure OnInterchangeClick(Sender: TObject);
    procedure OnShorelineClick(Sender: TObject);
    procedure OnReserveClick(Sender: TObject);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    property OnLocationChanged: TLocationChangedEvent read FOnLocationChanged write FOnLocationChanged;
  end;

implementation

{$R *.dfm}

{ TfrLocation }

constructor TfrLocation.Create(AOwner: TComponent);
begin
  inherited;

  FOnLocationChanged := nil;

  im—ustoms.OnClick := OnCustomsClick;
  la—ustoms.OnClick := OnCustomsClick;

  imWoods.OnClick := OnWoodsClick;
  laWoods.OnClick := OnWoodsClick;

  imInterchange.OnClick := OnInterchangeClick;
  laInterchange.OnClick := OnInterchangeClick;

  imShoreline.OnClick := OnShorelineClick;
  laShoreline.OnClick := OnShorelineClick;

  imReserve.OnClick := OnReserveClick;
  laReserve.OnClick := OnReserveClick;
end;

destructor TfrLocation.Destroy;
begin
  FOnLocationChanged := nil;

  inherited;
end;

procedure TfrLocation.DoLocationChanged(Value: TLocalMap);
begin
  if Assigned(FOnLocationChanged) then
    FOnLocationChanged(Value);
end;

procedure TfrLocation.OnCustomsClick(Sender: TObject);
begin
  DoLocationChanged(CustomsMap);
end;

procedure TfrLocation.OnWoodsClick(Sender: TObject);
begin
  DoLocationChanged(WoodsMap);
end;

procedure TfrLocation.OnInterchangeClick(Sender: TObject);
begin
  DoLocationChanged(InterchangeMap);
end;

procedure TfrLocation.OnShorelineClick(Sender: TObject);
begin
  DoLocationChanged(ShorelineMap);
end;

procedure TfrLocation.OnReserveClick(Sender: TObject);
begin
  DoLocationChanged(ReserveMap);
end;

end.
