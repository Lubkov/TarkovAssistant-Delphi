unit ME.Frame.Picture;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants, 
  FMX.Types, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls,
  FMX.Objects, FMX.Controls.Presentation, System.Actions, FMX.ActnList,
  System.ImageList, FMX.ImgList, FMX.Menus;

type
  TfrPicture = class(TFrame)
    ImageList1: TImageList;
    ActionList1: TActionList;
    acOpenPicture: TAction;
    acDeletePicture: TAction;
    OpenDialog: TOpenDialog;
    edPicture: TImage;
    paToolbar: TPanel;
    edAddMap: TSpeedButton;
    edDeleteMap: TSpeedButton;
    ImageMenu: TPopupMenu;
    miOpenPicture: TMenuItem;
    miDeletePicture: TMenuItem;
    laTitle: TLabel;

    procedure acOpenPictureExecute(Sender: TObject);
    procedure acDeletePictureExecute(Sender: TObject);
    procedure ActionList1Update(Action: TBasicAction; var Handled: Boolean);
  private
    FChanged: Boolean;
    FFileName: string;
    FResize: Boolean;

    function GetPicture: TBitmap;
    procedure SetPicture(const Value: TBitmap);
    function GetTitle: string;
    procedure SetTitle(const Value: string);
    procedure SetReadonly(const Value: Boolean);
    function GetReadonly: Boolean;
    procedure SetResize(const Value: Boolean);
  public
    constructor Create(AOwner: TComponent); override;

    procedure ResizePicture;

    property Picture: TBitmap read GetPicture write SetPicture;
    property Title: string read GetTitle write SetTitle;
    property Changed: Boolean read FChanged write FChanged;
    property FileName: string read FFileName write FFileName;
    property Readonly: Boolean read GetReadonly write SetReadonly;
    property Resize: Boolean read FResize write SetResize;
  end;

implementation

{$R *.fmx}

{ TfrPicture }

constructor TfrPicture.Create(AOwner: TComponent);
begin
  inherited;

  Title := '';
  Picture := nil;
  FChanged := False;
  FFileName := '';
end;

function TfrPicture.GetPicture: TBitmap;
begin
  Result := edPicture.Bitmap;
end;

procedure TfrPicture.SetPicture(const Value: TBitmap);
begin
  edPicture.Bitmap.Assign(Value);
  ResizePicture;
end;

function TfrPicture.GetTitle: string;
begin
  Result := laTitle.Text;
end;

procedure TfrPicture.SetTitle(const Value: string);
begin
  laTitle.Text := Value;
end;

function TfrPicture.GetReadonly: Boolean;
begin
  Result := not paToolbar.Visible;
end;

procedure TfrPicture.SetReadonly(const Value: Boolean);
begin
  paToolbar.Visible := not Value;
end;

procedure TfrPicture.SetResize(const Value: Boolean);
begin
  if FResize = Value  then
    Exit;

  FResize := Value;
  ResizePicture;
end;

procedure TfrPicture.acOpenPictureExecute(Sender: TObject);
begin
  if OpenDialog.Execute then begin
    FFileName := OpenDialog.FileName;
    Picture.LoadFromFile(FFileName);
    FChanged := True;
  end;
end;

procedure TfrPicture.acDeletePictureExecute(Sender: TObject);
begin
  Picture.Assign(nil);
  FChanged := True;
end;

procedure TfrPicture.ActionList1Update(Action: TBasicAction; var Handled: Boolean);
begin
  acOpenPicture.Enabled := not Readonly;
  acDeletePicture.Enabled := not (Readonly or Picture.IsEmpty);
end;

procedure TfrPicture.ResizePicture;
var
  bmp: TBitmap;
begin
  bmp := edPicture.Bitmap;
  if (bmp <> nil) and not bmp.IsEmpty then
    if Resize and (bmp.Height < edPicture.Height) and (bmp.Width < edPicture.Width) then
      edPicture.WrapMode := TImageWrapMode.Center
    else
      edPicture.WrapMode := TImageWrapMode.Fit;
end;

end.
