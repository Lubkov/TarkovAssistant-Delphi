unit TM.Frame.Options;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants, 
  FMX.Types, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls,
  System.ImageList, FMX.ImgList, FMX.Controls.Presentation, FMX.Edit,
  FMX.Layouts, ME.DB.Options, ME.Filter.Profile;

type
  TOptionsFrame = class(TFrame)
    ImageList24: TImageList;
    OpenDialog: TOpenDialog;
    laDataPath: TLabel;
    paDataPath: TLayout;
    edDataPath: TEdit;
    buDataPath: TSpeedButton;
    laSreenshotPath: TLabel;
    paSreenshotPath: TLayout;
    edSreenshotPath: TEdit;
    buSreenshotPath: TSpeedButton;
    edTrackLocation: TCheckBox;
    paProfileFilter: TLayout;
    Layout1: TLayout;
    buClose: TSpeedButton;
    procedure buCloseClick(Sender: TObject);
  private
    FOptions: TOptions;
    FProfileFilter: TProfileFilter;
    FOnClose: TNotifyEvent;
    FMaxHeight: Integer;
    FMaxWidth: Integer;

    function GetDataPath: string;
    function GetSreenshotPath: string;
    function GetTrackLocation: Boolean;
    procedure SetDataPath(const Value: string);
    procedure SetSreenshotPath(const Value: string);
    procedure SetTrackLocation(const Value: Boolean);

    function InternalOpenFolder(const FileName: string): string;
    procedure OpenFolderButtonClick(Sender: TObject);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure Init(const Options: TOptions);
    procedure Save;

    property MaxHeight: Integer read FMaxHeight;
    property MaxWidth: Integer read FMaxWidth;
    property DataPath: string read GetDataPath write SetDataPath;
    property SreenshotPath: string read GetSreenshotPath write SetSreenshotPath;
    property TrackLocation: Boolean read GetTrackLocation write SetTrackLocation;
    property OnClose: TNotifyEvent read FOnClose write FOnClose;
  end;

implementation

{$R *.fmx}

uses
  App.Service;

{ TOptionsPanel }

constructor TOptionsFrame.Create(AOwner: TComponent);
begin
  inherited;

  FOptions := TOptions.Create;
  FOnClose := nil;
  FProfileFilter := TProfileFilter.Create(Self);
  FProfileFilter.Parent := paProfileFilter;
  FProfileFilter.Align := TAlignLayout.Top;

  FMaxHeight := 260;
  FMaxWidth := 408;

  buDataPath.OnClick := OpenFolderButtonClick;
  buSreenshotPath.OnClick := OpenFolderButtonClick;
end;

destructor TOptionsFrame.Destroy;
begin
  FOptions.Free;
  FOnClose := nil;
  FProfileFilter.Free;

  inherited;
end;

procedure TOptionsFrame.buCloseClick(Sender: TObject);
begin
  if Assigned(FOnClose) then
    FOnClose(Self);
end;

function TOptionsFrame.GetDataPath: string;
begin
  Result := edDataPath.Text;
end;

procedure TOptionsFrame.SetDataPath(const Value: string);
begin
  edDataPath.Text := Value;
end;

function TOptionsFrame.GetSreenshotPath: string;
begin
  Result := edSreenshotPath.Text;
end;

procedure TOptionsFrame.SetSreenshotPath(const Value: string);
begin
  edSreenshotPath.Text := Value;
end;

function TOptionsFrame.GetTrackLocation: Boolean;
begin
  Result := edTrackLocation.IsChecked;
end;

procedure TOptionsFrame.SetTrackLocation(const Value: Boolean);
begin
  edTrackLocation.IsChecked := Value;
end;

function TOptionsFrame.InternalOpenFolder(const FileName: string): string;
var
  Directory: string;
begin
  if SelectDirectory('Выбор директории' {Caption}, FileName {Root}, Directory {out}) then
    Result := Directory
  else
    Result := FileName;
end;

procedure TOptionsFrame.OpenFolderButtonClick(Sender: TObject);
var
  Edit: TEdit;
begin
  if Sender = buDataPath then
    Edit := edDataPath
  else
    Edit := edSreenshotPath;

  Edit.Text := InternalOpenFolder(Edit.Text);
end;

procedure TOptionsFrame.Init(const Options: TOptions);
begin
  FOptions.Assign(Options);

  DataPath := FOptions.DataPath;
  SreenshotPath := FOptions.SreenshotPath;
  TrackLocation := FOptions.TrackLocation;

  FProfileFilter.Init;
  FProfileFilter.ProfileName := FOptions.Profile;
end;

procedure TOptionsFrame.Save;
begin
  FOptions.DataPath:= DataPath;
  FOptions.SreenshotPath:= SreenshotPath;
  FOptions.TrackLocation:= TrackLocation;
  FOptions.Profile:= FProfileFilter.ProfileName;

  if FOptions.IsEqual(AppService.Options) then
    Exit;

  AppService.Options.Assign(FOptions);
  AppService.SaveParams;
end;

end.
