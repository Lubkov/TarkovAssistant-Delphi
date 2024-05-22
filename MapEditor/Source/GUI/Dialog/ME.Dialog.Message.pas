unit ME.Dialog.Message;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ExtCtrls, Vcl.StdCtrls,
  Vcl.ImgList, System.ImageList, ME.LocalMap, ME.Dialog.Presenter;

type
  TedMessage = class(TForm, IMessageDialog)
    btnOk: TButton;
    btnCancel: TButton;
    paMain: TPanel;
    lblMessage: TLabel;
    Image: TImage;
    ImageList: TImageList;

    procedure btnOkClick(Sender: TObject);
  private
  public
//    function Open(const Text: string): Boolean;
    function GetModalResult: TModalResult;
    procedure SetModalResult(Value: TModalResult);
    procedure SetMessage(const Value: string);
  end;

implementation

{$R *.dfm}

procedure TedMessage.btnOkClick(Sender: TObject);
begin
  //Close;
end;

//function TedMessage.Open(const Text: string): Boolean;
//begin
//  lblMessage.Caption := Text;
//  Self.ShowModal;
//  Result := Self.ModalResult = mrOk;
//end;

function TedMessage.GetModalResult: TModalResult;
begin
  Result := ModalResult;
end;

procedure TedMessage.SetModalResult(Value: TModalResult);
begin
  ModalResult := Value;
end;

procedure TedMessage.SetMessage(const Value: string);
begin
  lblMessage.Caption := Value;
end;

end.
