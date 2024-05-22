unit FormWrapper;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes,
  Vcl.Controls, Vcl.Forms;

type
  TFormWrapper = class
  private
    FForm: TForm;

    FLeft: Integer;
    FTop: Integer;
    FWidth: Integer;
    FHeight: Integer;
    FBorderStyle: TFormBorderStyle;
    FFormStyle: TFormStyle;
    FFullScreen: Boolean;

    function GetMainMonitor: TMonitor;
    procedure SetFullScreen(const Value: Boolean);
  public
    constructor Create(const Form: TForm); virtual;
    destructor Destroy; override;

    procedure SaveFormState;
    procedure RestoreFormState;

    procedure EnableFullScreenMode;
    procedure DisableFullScreenMode;

    property FullScreen: Boolean read FFullScreen write SetFullScreen;
  end;

implementation

{ TFormWrapper }

constructor TFormWrapper.Create(const Form: TForm);
begin
  inherited Create;

  FForm := Form;
  SaveFormState;
  FFullScreen := False;
end;

destructor TFormWrapper.Destroy;
begin
  FForm := nil;

  inherited;
end;

function TFormWrapper.GetMainMonitor: TMonitor;
var
  i: Integer;
begin
  for i := 0 to Screen.MonitorCount - 1 do
    if Screen.Monitors[i].Primary then
      Exit(Screen.Monitors[i]);

  Result := Screen.Monitors[0];
end;

procedure TFormWrapper.SetFullScreen(const Value: Boolean);
begin
  if FFullScreen = Value then
    Exit;

  if Value then
    EnableFullScreenMode
  else
    DisableFullScreenMode;
end;

procedure TFormWrapper.SaveFormState;
begin
  FLeft := FForm.Left;
  FTop := FForm.Top;
  FWidth := FForm.Width;
  FHeight := FForm.Height;
  FBorderStyle := FForm.BorderStyle;
  FFormStyle := FForm.FormStyle;
end;

procedure TFormWrapper.RestoreFormState;
begin
  FForm.Left := FLeft;
  FForm.Top := FTop;
  FForm.Width := FWidth;
  FForm.Height := FHeight;
  FForm.BorderStyle := FBorderStyle;
  FForm.FormStyle := FFormStyle;
end;

procedure TFormWrapper.EnableFullScreenMode;
var
  Monitor: TMonitor;
begin
  SaveFormState;

  Monitor := GetMainMonitor;
  FForm.Left := Monitor.Left;
  FForm.Top := Monitor.Top;
  FForm.Width := Monitor.Width;
  FForm.Height := Monitor.Height;
  FForm.BorderStyle := bsNone;
  FForm.FormStyle := fsStayOnTop;

  FFullScreen := True;
end;

procedure TFormWrapper.DisableFullScreenMode;
begin
  RestoreFormState;
  FFullScreen := False;
end;

end.
