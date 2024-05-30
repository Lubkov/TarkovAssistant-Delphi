unit ME.Dialog.Message;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.StdCtrls,
  FMX.Objects, FMX.Controls.Presentation, ME.DB.Map, ME.Dialog.Presenter;

type
  TedMessage = class(TForm, IMessageDialog)
    btnSuccess: TButton;
    btnCancel: TButton;
    pMain: TPanel;
    Image1: TImage;
    lblMessage: TLabel;
    procedure btnSuccessClick(Sender: TObject);
  private
  public
    function GetModalResult: TModalResult;
    procedure SetModalResult(Value: TModalResult);
    procedure SetMessage(const Value: string);
  end;

var
  edMessage: TedMessage;

implementation

{$R *.fmx}

procedure TedMessage.btnSuccessClick(Sender: TObject);
begin
  //
end;

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
  lblMessage.Text := Value;
end;

end.
