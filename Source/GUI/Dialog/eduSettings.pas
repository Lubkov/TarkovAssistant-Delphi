unit eduSettings;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, cxGraphics, cxLookAndFeels,
  cxLookAndFeelPainters, Vcl.Menus, dxSkinsCore, dxSkinOffice2016Dark,
  Vcl.StdCtrls, cxButtons;

type
  TedSettings = class(TForm)
    buSave: TcxButton;
    buCancel: TcxButton;
    edSreenshotPath: TEdit;
    cxButton1: TcxButton;
    edTrackLocation: TCheckBox;
  private
  public
    function Open: Boolean;
  end;

var
  edSettings: TedSettings;

implementation

uses
  App.Constants;

{$R *.dfm}

{ TForm1 }

function TedSettings.Open: Boolean;
begin
  edSreenshotPath.Text := AppParams.SreenshotPath;
  edTrackLocation.Checked := AppParams.TrackLocation;

  Result := ShowModal = mrOk;
  if Result then begin
    AppParams.SreenshotPath := edSreenshotPath.Text;
    AppParams.TrackLocation := edTrackLocation.Checked;
  end;
end;

end.
