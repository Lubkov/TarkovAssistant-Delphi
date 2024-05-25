unit ME.Frame.Picture;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants, 
  FMX.Types, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls,
  FMX.Objects, FMX.Controls.Presentation, System.Actions, FMX.ActnList,
  System.ImageList, FMX.ImgList;

type
  TfrPicture = class(TFrame)
    ImageList1: TImageList;
    ActionList1: TActionList;
    acOpenPicture: TAction;
    acDeletePicture: TAction;
    OpenDialog: TOpenDialog;
    paMain: TPanel;
    edPicture: TImage;
    paToolbar: TPanel;
    edAddMap: TSpeedButton;
    edDeleteMap: TSpeedButton;
    procedure acOpenPictureExecute(Sender: TObject);
    procedure acDeletePictureExecute(Sender: TObject);
    procedure ActionList1Update(Action: TBasicAction; var Handled: Boolean);
  private
    function GetPicture: TBitmap;
    procedure SetPicture(const Value: TBitmap);
  public
    property Picture: TBitmap read GetPicture write SetPicture;
  end;

implementation

{$R *.fmx}

{ TfrPicture }

function TfrPicture.GetPicture: TBitmap;
begin
  Result := edPicture.Bitmap;
end;

procedure TfrPicture.SetPicture(const Value: TBitmap);
begin
  edPicture.Bitmap.Assign(Value);
end;

procedure TfrPicture.acOpenPictureExecute(Sender: TObject);
begin
  if OpenDialog.Execute then
    Picture.LoadFromFile(OpenDialog.FileName);
end;

procedure TfrPicture.acDeletePictureExecute(Sender: TObject);
begin
  Picture.Assign(nil);
end;

procedure TfrPicture.ActionList1Update(Action: TBasicAction; var Handled: Boolean);
begin
  acOpenPicture.Enabled := True;
  acDeletePicture.Enabled := not Picture.IsEmpty;
end;

end.
