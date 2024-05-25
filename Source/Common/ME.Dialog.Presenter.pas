unit ME.Dialog.Presenter;

interface

uses
  System.SysUtils, System.Variants, System.Classes, System.UITypes,
  FMX.Controls, FMX.Forms;

type
  TErrorEvent = procedure(const ErrorMessage: string) of object;

  ICLDialog = interface
    ['{06620A0C-459E-4811-A144-A0ADF2CCA6EE}']
    function ShowModal: TModalResult;
    function GetModalResult: TModalResult;
    procedure SetModalResult(Value: TModalResult);
  end;

  IMessageDialog = interface(ICLDialog)
    ['{74037A32-6D98-4632-9A78-936988125D64}']

    procedure SetMessage(const Value: string);
  end;

  TDialogPresenter = class
  private
    FOnError: TErrorEvent;
  protected
    FEditDialog: ICLDialog;
  public
    constructor Create(Dialog: ICLDialog); virtual;
    destructor Destroy; override;

    function ShowModal: Boolean;

    property OnError: TErrorEvent read FOnError write FOnError;
  end;

implementation

{ TDialogPresenter }

constructor TDialogPresenter.Create(Dialog: ICLDialog);
begin
  inherited Create;

  FEditDialog := Dialog;
end;

destructor TDialogPresenter.Destroy;
begin
  FEditDialog := nil;

  inherited;
end;

function TDialogPresenter.ShowModal: Boolean;
begin
  Result := FEditDialog.ShowModal = mrOK;
end;

end.
