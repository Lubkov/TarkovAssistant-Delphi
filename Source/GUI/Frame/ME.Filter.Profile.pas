unit ME.Filter.Profile;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants, 
  System.ImageList, System.Actions, FMX.Types, FMX.Graphics, FMX.Controls, FMX.Forms,
  FMX.Dialogs, FMX.StdCtrls, FMX.Edit, FMX.Controls.Presentation, FMX.ListBox, FMX.Layouts,
  FMX.ImgList, FMX.ActnList, ME.DB.Profile, Data.DB, MemDS, DBAccess, Uni,
  System.Rtti, System.Bindings.Outputs, Fmx.Bind.Editors, Data.Bind.EngExt,
  Fmx.Bind.DBEngExt, Data.Bind.Components, Data.Bind.DBScope;

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
    F: TUniQuery;
    FID: TIntegerField;
    FProfileName: TWideStringField;
    BindSourceDB1: TBindSourceDB;
    BindingsList1: TBindingsList;
    LinkListControlToField1: TLinkListControlToField;
    FKind: TIntegerField;
    edAddProfile: TSpeedButton;
    SpeedButton1: TSpeedButton;
    procedure BindSourceDB1SubDataSourceDataChange(Sender: TObject; Field: TField);
    procedure edEditProfileClick(Sender: TObject);
    procedure acAddProfileExecute(Sender: TObject);
    procedure acDeleteProfileExecute(Sender: TObject);
  private
//    FOnMapChanged: TDBMapChangedEvent;

    function GetProfileID: Variant;
    procedure SetProfileID(const Value: Variant);
    function GetProfileName: string;
    procedure SetProfileName(const Value: string);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure Init;

    property ProfileID: Variant read GetProfileID write SetProfileID;
    property ProfileName: string read GetProfileName write SetProfileName;
//    property OnMapChanged: TDBMapChangedEvent read FOnMapChanged write FOnMapChanged;
  end;

implementation

uses
  App.Service, ME.Service.Profile; // ME.Presenter.Profile;

{$R *.fmx}

{ TProfileFilter }

constructor TProfileFilter.Create(AOwner: TComponent);
begin
  inherited;

//  FOnMapChanged := nil;
end;

destructor TProfileFilter.Destroy;
begin
//  FOnMapChanged := nil;

  inherited;
end;

procedure TProfileFilter.BindSourceDB1SubDataSourceDataChange(Sender: TObject; Field: TField);
begin
//  if Assigned(FOnMapChanged) then
//    FOnMapChanged(FID.Value);
end;

function TProfileFilter.GetProfileID: Variant;
begin
  if edProfileName.ItemIndex >= 0 then
    Result := FID.Value
  else
    Result := Null;
end;

procedure TProfileFilter.SetProfileID(const Value: Variant);
begin
  if F.Active then
    F.Locate('ID', Value, []);
end;

function TProfileFilter.GetProfileName: string;
begin
  if edProfileName.ItemIndex >= 0 then
    Result := FProfileName.AsString
  else
    Result := '';
end;

procedure TProfileFilter.SetProfileName(const Value: string);
begin
  if F.Active then
    F.Locate('ProfileName', Value, [loCaseInsensitive]);
end;

procedure TProfileFilter.Init;
begin
  F.Close;
  F.Connection := AppService.DBConnection.Connection;
  F.SQL.Text := 'SELECT ID, Name as ProfileName, Kind FROM Profile';
  F.Open;

  edProfileName.ItemIndex := -1;
end;

procedure TProfileFilter.acAddProfileExecute(Sender: TObject);
begin
//
end;

procedure TProfileFilter.edEditProfileClick(Sender: TObject);
//var
//  Presenter: TEditMapPresenter;
//  Dialog: TedMap;
//  Map: TDBMap;
begin
//  Dialog := TedMap.Create(Self);
//  try
//    Map := TDBMap.Create;
//    try
//      if not MapService.GetAt(FID.Value, Map) then
//        Exit;
//
//      Presenter := TEditMapPresenter.Create(Dialog, Map);
//      try
//        if Presenter.Edit then
//          F.RefreshRecord;
//      finally
//        Presenter.Free;
//      end;
//    finally
//      Map.Free;
//    end;
//  finally
//    Dialog.Free;
//  end;
end;

procedure TProfileFilter.acDeleteProfileExecute(Sender: TObject);
begin
//
end;

end.
