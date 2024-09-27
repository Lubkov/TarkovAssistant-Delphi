unit ME.Edit.Options;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants, 
  FMX.Types, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls,
  ME.Edit.Form, System.Actions, FMX.ActnList, FMX.Controls.Presentation,
  ME.Edit.Form.Presenter, ME.DB.Options, FMX.Edit, System.ImageList, FMX.ImgList,
  FMX.Layouts, FMX.Objects, ME.Filter.Profile;

type
  TedOptions = class(TEditForm, IEditDialog<TOptions>)
    laDataPath: TLabel;
    laSreenshotPath: TLabel;
    edTrackLocation: TCheckBox;
    OpenDialog: TOpenDialog;
    ImageList1: TImageList;
    paDataPath: TLayout;
    edDataPath: TEdit;
    buDataPath: TSpeedButton;
    paSreenshotPath: TLayout;
    edSreenshotPath: TEdit;
    buSreenshotPath: TSpeedButton;
    Background: TRectangle;
    paProfileFilter: TLayout;
  private
    FOptions: TOptions;
    FProfileFilter: TProfileFilter;

    function GetDataPath: string;
    procedure SetDataPath(const Value: string);
    function GetSreenshotPath: string;
    procedure SetSreenshotPath(const Value: string);
    function GetTrackLocation: Boolean;
    procedure SetTrackLocation(const Value: Boolean);

    function InternalOpenFolder(const FileName: string): string;
    procedure OpenFolderButtonClick(Sender: TObject);
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
  Background.Fill.Color := $FF252525; // FFB97A57
//  laDataPath.TextSettings.FontColor := TColorRec.White;

  buDataPath.OnClick := OpenFolderButtonClick;
  buSreenshotPath.OnClick := OpenFolderButtonClick;

  FProfileFilter := TProfileFilter.Create(Self);
  FProfileFilter.Parent := paProfileFilter;
  FProfileFilter.Align := TAlignLayout.Top;
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

function TedOptions.InternalOpenFolder(const FileName: string): string;
var
  Directory: string;
begin
  if SelectDirectory('Выбор директории' {Caption}, FileName {Root}, Directory {out}) then
    Result := Directory
  else
    Result := FileName;
end;

procedure TedOptions.OpenFolderButtonClick(Sender: TObject);
var
  Edit: TEdit;
begin
  if Sender = buDataPath then
    Edit := edDataPath
  else
    Edit := edSreenshotPath;

  Edit.Text := InternalOpenFolder(Edit.Text);
end;

procedure TedOptions.SetInstance(const Value: TOptions);
begin
  FOptions := Value;
  Caption := 'Параметры приложения';

  DataPath := FOptions.DataPath;
  SreenshotPath := FOptions.SreenshotPath;
  TrackLocation := FOptions.TrackLocation;

  FProfileFilter.Init;
  FProfileFilter.ProfileName := FOptions.Profile;
end;

procedure TedOptions.PostValues(const Value: TOptions);
begin
  Value.DataPath := DataPath;
  Value.SreenshotPath := SreenshotPath;
  Value.TrackLocation := TrackLocation;
  Value.Profile := FProfileFilter.ProfileName;
end;

end.
