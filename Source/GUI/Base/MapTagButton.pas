unit MapTagButton;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes,
  System.UITypes, Vcl.ExtCtrls, Vcl.Controls, Vcl.StdCtrls, Vcl.Graphics;

type
  TMapTagButton = class(TPanel)
  private
    FGlyph: TImage;
    FLabel: TLabel;
    FImages: TImageList;
    FImageIndex: Integer;
    FDisableImageIndex: Integer;
    FEnabled: Boolean;

    procedure SetImages(const Value: TImageList);
    procedure SetImageIndex(const Value: Integer);
    function GetDrawImageIndex: Integer;
    function GetText: string;
    procedure SetText(const Value: string);
    function GetFont: TFont;
    procedure SetFont(const Value: TFont);

    procedure DrawImage(const Index: Integer);
    procedure InternalOnClick(Sender: TObject);
  protected
    function GetEnabled: Boolean; override;
    procedure SetEnabled(Value: Boolean); override;

    property DrawImageIndex: Integer read GetDrawImageIndex;
  public
    constructor Create(AOwner: TComponent); override;

    property Images: TImageList read FImages write SetImages;
    property ImageIndex: Integer read FImageIndex write SetImageIndex;
    property DisableImageIndex: Integer read FDisableImageIndex write FDisableImageIndex;
    property Caption: string read GetText write SetText;
    property Font: TFont read GetFont write SetFont;
  end;


implementation

{ TMapTagButton }

constructor TMapTagButton.Create(AOwner: TComponent);
begin
  inherited;

  FEnabled := True;
  Width := 100;
  Height := 32;
  Caption := '';
  BevelOuter := bvNone;
//  BevelKind := bkTile;
  Cursor := crHandPoint;

  FGlyph := TImage.Create(Self);
  FGlyph.Parent := Self;
  FGlyph.Align := alLeft;
  FGlyph.OnClick := InternalOnClick;

  FLabel := TLabel.Create(Self);
  FLabel.Parent := Self;
  FLabel.Align := alClient;
  FLabel.AlignWithMargins := True;
  FLabel.Margins.SetControlBounds(5, 0, 5, 0);
  FLabel.Layout := tlCenter;
  FLabel.OnClick := InternalOnClick;
end;

procedure TMapTagButton.SetImages(const Value: TImageList);
begin
  FImages := Value;
  Self.Height := Images.Height;
  FGlyph.Height := Images.Height;
  FGlyph.Width := Images.Width;
  DrawImage(DrawImageIndex);
end;

procedure TMapTagButton.SetImageIndex(const Value: Integer);
begin
  FImageIndex := Value;
  DrawImage(DrawImageIndex);
end;

function TMapTagButton.GetDrawImageIndex: Integer;
begin
  if FEnabled then
    Result := ImageIndex
  else
    Result := DisableImageIndex;
end;

procedure TMapTagButton.SetText(const Value: string);
begin
  FLabel.Caption := Value;
end;

function TMapTagButton.GetText: string;
begin
  Result := FLabel.Caption;
end;

function TMapTagButton.GetFont: TFont;
begin
  Result := FLabel.Font;
end;

procedure TMapTagButton.SetFont(const Value: TFont);
begin
  FLabel.Font := Value;
end;

procedure TMapTagButton.DrawImage(const Index: Integer);
begin
  if not Assigned(Parent) or (FImages = nil) or (Index < 0) then
    Exit;

  FGlyph.Picture.Assign(nil);
  FImages.GetBitmap(Index, FGlyph.Picture.Bitmap);
  FGlyph.Picture.Bitmap.TransparentColor:= FGlyph.Canvas.Pixels[0, 0];
  FGlyph.Transparent:= True;
end;

procedure TMapTagButton.InternalOnClick(Sender: TObject);
begin
  if Assigned(OnClick) then
    OnClick(Self);
end;

function TMapTagButton.GetEnabled: Boolean;
begin
  Result := FEnabled;
end;

procedure TMapTagButton.SetEnabled(Value: Boolean);
begin
//  inherited SetEnabled(True);

  if FEnabled = Value then
    Exit;

  FEnabled := Value;
  if Enabled then
    FLabel.Font.Style := FLabel.Font.Style - [TFontStyle.fsStrikeOut]
  else
    FLabel.Font.Style := FLabel.Font.Style + [TFontStyle.fsStrikeOut];

  DrawImage(DrawImageIndex);
end;

end.
