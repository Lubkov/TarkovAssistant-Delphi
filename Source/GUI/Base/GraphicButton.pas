unit GraphicButton;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes,
  Vcl.ExtCtrls, Vcl.Controls;

type
  TGraphicButton = class(TImage)
  private
    FImages: TImageList;
    FImageIndex: Integer;
    FHotImageIndex: Integer;
    FPressedImageIndex: Integer;
    FFocused: Boolean;

    procedure SetImages(const Value: TImageList);
    procedure DrawImage(const Index: Integer);
    procedure SetImageIndex(const Value: Integer);
  public
    constructor Create(AOwner: TComponent); override;

    procedure OnImageMouseEnter(Sender: TObject); virtual;
    procedure OnImageMouseLeave(Sender: TObject); virtual;
    procedure OnImageMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer); virtual;
    procedure OnImageMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer); virtual;

    property Images: TImageList read FImages write SetImages;
    property ImageIndex: Integer read FImageIndex write SetImageIndex;
    property HotImageIndex: Integer read FHotImageIndex write FHotImageIndex;
    property PressedImageIndex: Integer read FPressedImageIndex write FPressedImageIndex;
  end;

implementation

{ TGraphicButton }

constructor TGraphicButton.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FImages := nil;
  FImageIndex := -1;
  FHotImageIndex := -1;
  FPressedImageIndex := -1;
  Height := 32;
  Width := 32;
  FFocused := False;
  Cursor := crHandPoint;

  Self.OnMouseEnter := OnImageMouseEnter;
  Self.OnMouseLeave := OnImageMouseLeave;
  Self.OnMouseDown := OnImageMouseDown;
  Self.OnMouseUp := OnImageMouseUp;
end;

procedure TGraphicButton.SetImages(const Value: TImageList);
begin
  FImages := Value;
  Height := Images.Height;
  Width := Images.Width;
  DrawImage(FImageIndex);
end;

procedure TGraphicButton.SetImageIndex(const Value: Integer);
begin
  FImageIndex := Value;
  DrawImage(FImageIndex);
end;

procedure TGraphicButton.DrawImage(const Index: Integer);
begin
  if not Assigned(Parent) or (FImages = nil) or (Index < 0) then
    Exit;

  Self.Picture.Assign(nil);
  FImages.GetBitmap(Index, Self.Picture.Bitmap);
  Self.Picture.Bitmap.TransparentColor:= Self.Canvas.Pixels[0, 0];
  Self.Transparent:= True;
end;

procedure TGraphicButton.OnImageMouseEnter(Sender: TObject);
begin
  FFocused := True;
  DrawImage(HotImageIndex);
end;

procedure TGraphicButton.OnImageMouseLeave(Sender: TObject);
begin
  FFocused := False;
  DrawImage(ImageIndex);
end;

procedure TGraphicButton.OnImageMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  DrawImage(PressedImageIndex);
end;

procedure TGraphicButton.OnImageMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if FFocused then
    DrawImage(HotImageIndex)
  else
    DrawImage(ImageIndex);
end;

end.
