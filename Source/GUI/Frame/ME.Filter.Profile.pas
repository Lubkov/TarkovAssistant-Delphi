unit ME.Filter.Profile;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants, 
  System.ImageList, System.Actions, FMX.Types, FMX.Graphics, FMX.Controls, FMX.Forms,
  FMX.Dialogs, FMX.StdCtrls, FMX.Edit, FMX.Controls.Presentation, FMX.ListBox,
  FMX.Layouts, FMX.ImgList, FMX.ActnList, ME.Profile;

type
  TProfileFilter = class(TFrame)
    laProfileName: TLabel;
    edProfileName: TComboBox;
    ImageList1: TImageList;
    ActionList1: TActionList;
    acAddProfile: TAction;
    acEditProfile: TAction;
    acDeleteProfile: TAction;
    MainLayout: TLayout;
    ToolLayout: TLayout;
    edEditProfile: TSpeedButton;
    ProfileNameLayout: TLayout;
    ButtonLayout: TLayout;
    edAddProfile: TSpeedButton;
    edDelProfile: TSpeedButton;
    SpeedButton1: TSpeedButton;
    acClearProfile: TAction;

    procedure acAddProfileExecute(Sender: TObject);
    procedure acDeleteProfileExecute(Sender: TObject);
    procedure acEditProfileExecute(Sender: TObject);
    procedure acClearProfileExecute(Sender: TObject);
    procedure ActionList1Update(Action: TBasicAction; var Handled: Boolean);
  private
    function GetProfileIndex: Integer;
    procedure SetProfileIndex(const Value: Integer);
    function GetProfileName: string;
    procedure SetProfileName(const Value: string);

    function InternalProfileEdit(const Profile: TProfile): Boolean;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure Init;

    property ProfileIndex: Integer read GetProfileIndex write SetProfileIndex;
    property ProfileName: string read GetProfileName write SetProfileName;
  end;

implementation

uses
  App.Service, ME.DB.Utils, ME.Service.Profile, ME.Presenter.Profile, ME.Edit.Profile,
  ME.Dialog.Message;

{$R *.fmx}

{ TProfileFilter }

constructor TProfileFilter.Create(AOwner: TComponent);
begin
  inherited;

end;

destructor TProfileFilter.Destroy;
begin

  inherited;
end;

function TProfileFilter.GetProfileIndex: Integer;
begin
  Result := edProfileName.ItemIndex;
end;

procedure TProfileFilter.SetProfileIndex(const Value: Integer);
begin
  edProfileName.ItemIndex := Value;
end;

function TProfileFilter.GetProfileName: string;
begin
  Result := edProfileName.Text;
end;

procedure TProfileFilter.SetProfileName(const Value: string);
begin
  ProfileIndex := edProfileName.Items.IndexOf(Value);
end;

function TProfileFilter.InternalProfileEdit(const Profile: TProfile): Boolean;
var
  Presenter: TEditProfilePresenter;
  Dialog: TedProfile;
begin
  Dialog := TedProfile.Create(Self);
  try
    Presenter := TEditProfilePresenter.Create(Dialog, Profile);
    try
      Result := Presenter.Edit;
    finally
      Presenter.Free;
    end;
  finally
    Dialog.Free;
  end;
end;

procedure TProfileFilter.Init;
begin
  ProfileService.GetAll(edProfileName.Items);
  ProfileIndex := -1;
end;

procedure TProfileFilter.acAddProfileExecute(Sender: TObject);
var
  Profile: TProfile;
  Res: Boolean;
begin
  Profile := TProfile.Create;
  try
    Res := InternalProfileEdit(Profile);
    if not Res then
      Exit;

    edProfileName.Items.Add(Profile.Name);
    ProfileIndex := edProfileName.Count - 1;
  finally
    Profile.Free;
  end;
end;

procedure TProfileFilter.acEditProfileExecute(Sender: TObject);
var
  Profile: TProfile;
begin
  if ProfileIndex = -1 then
    Exit;

  Profile := TProfile.Create;
  try
    if not ProfileService.Load(ProfileName, Profile) then
      Exit;

    InternalProfileEdit(Profile);
  finally
    Profile.Free;
  end;
end;

procedure TProfileFilter.acDeleteProfileExecute(Sender: TObject);
var
  Profile: TProfile;
  Presenter: TDelProfilePresenter;
  Dialog: TedMessage;
begin
  if Trim(ProfileName) = '' then
    Exit;

  Profile := TProfile.Create;
  try
    Profile.Name := ProfileName;

    Dialog := TedMessage.Create(Self);
    try
      Presenter := TDelProfilePresenter.Create(Dialog, Profile);
      try
        if not Presenter.Delete then
          Exit;

        edProfileName.Items.Delete(ProfileIndex);
      finally
        Presenter.Free;
      end;
    finally
      Dialog.Free;
    end;
  finally
    Profile.Free;
  end;
end;

procedure TProfileFilter.acClearProfileExecute(Sender: TObject);
begin
  ProfileIndex := -1;
end;

procedure TProfileFilter.ActionList1Update(Action: TBasicAction; var Handled: Boolean);
begin
  acAddProfile.Enabled := True;
  acEditProfile.Enabled := ProfileIndex >= 0;
  acDeleteProfile.Enabled := ProfileIndex >= 0;
  acClearProfile.Enabled := ProfileIndex >= 0;
end;

end.
