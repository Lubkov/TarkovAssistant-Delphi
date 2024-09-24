unit ME.Edit.Options;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants, 
  FMX.Types, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls,
  ME.Edit.Form, System.Actions, FMX.ActnList, FMX.Controls.Presentation,
  ME.Edit.Form.Presenter, ME.DB.Options, FMX.Edit;

type
  TedOptions = class(TEditForm, IEditDialog<TOptions>)
    laDataPath: TLabel;
    edDataPath: TEdit;
    laSreenshotPath: TLabel;
    edSreenshotPath: TEdit;
    edTrackLocation: TCheckBox;
  private
    FOptions: TOptions;

    function GetDataPath: string;
    procedure SetDataPath(const Value: string);
    function GetSreenshotPath: string;
    procedure SetSreenshotPath(const Value: string);
    function GetTrackLocation: Boolean;
    procedure SetTrackLocation(const Value: Boolean);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure SetInstance(const Value: TOptions);
    procedure PostValues(const Value: TOptions);

    property DataPath: string read GetDataPath write SetDataPath;
    property SreenshotPath: string read GetSreenshotPath write SetSreenshotPath;
    property TrackLocation: Boolean read GetTrackLocation write SetTrackLocation;
  end;

var
  edOptions: TedOptions;

implementation

{$R *.fmx}

{ TedOptions }

constructor TedOptions.Create(AOwner: TComponent);
begin
  inherited;

  FOptions := nil;
end;

destructor TedOptions.Destroy;
begin
  FOptions := nil;

  inherited;
end;

function TedOptions.GetDataPath: string;
begin
  Result := edDataPath.Text;
end;

procedure TedOptions.SetDataPath(const Value: string);
begin
  edDataPath.Text := Value;
end;

function TedOptions.GetSreenshotPath: string;
begin
  Result := edSreenshotPath.Text;
end;

procedure TedOptions.SetSreenshotPath(const Value: string);
begin
  edSreenshotPath.Text := Value;
end;

function TedOptions.GetTrackLocation: Boolean;
begin
  Result := edTrackLocation.IsChecked;
end;

procedure TedOptions.SetTrackLocation(const Value: Boolean);
begin
  edTrackLocation.IsChecked := Value;
end;

procedure TedOptions.SetInstance(const Value: TOptions);
begin
  FOptions := Value;
  Caption := 'Редактирование квеста';

  DataPath := FOptions.DataPath;
  SreenshotPath := FOptions.SreenshotPath;
  TrackLocation := FOptions.TrackLocation;
end;

procedure TedOptions.PostValues(const Value: TOptions);
begin
  Value.DataPath := DataPath;
  Value.SreenshotPath := SreenshotPath;
  Value.TrackLocation := TrackLocation;
end;

end.
