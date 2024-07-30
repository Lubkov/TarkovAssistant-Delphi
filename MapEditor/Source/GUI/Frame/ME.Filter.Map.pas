unit ME.Filter.Map;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants, 
  System.ImageList, System.Actions, FMX.Types, FMX.Graphics, FMX.Controls, FMX.Forms,
  FMX.Dialogs, FMX.StdCtrls, FMX.Edit, FMX.Controls.Presentation, FMX.ListBox, FMX.Layouts,
  FMX.ImgList, FMX.ActnList, ME.DB.Map, Data.DB, MemDS, DBAccess, Uni,
  System.Rtti, System.Bindings.Outputs, Fmx.Bind.Editors, Data.Bind.EngExt,
  Fmx.Bind.DBEngExt, Data.Bind.Components, Data.Bind.DBScope;

type
  TMapFilter = class(TFrame)
    laMapName: TLabel;
    edMapName: TComboBox;
    ImageList1: TImageList;
    ActionList1: TActionList;
    acAddMap: TAction;
    acEditMap: TAction;
    acDeleteMap: TAction;
    MainLayout: TLayout;
    ToolLayout: TLayout;
    edEditMap: TSpeedButton;
    MapNameLayout: TLayout;
    ButtonLayout: TLayout;
    F: TUniQuery;
    FID: TIntegerField;
    FCaption: TWideStringField;
    BindSourceDB1: TBindSourceDB;
    BindingsList1: TBindingsList;
    LinkListControlToField1: TLinkListControlToField;
    procedure acEditMapExecute(Sender: TObject);
    procedure BindSourceDB1SubDataSourceDataChange(Sender: TObject; Field: TField);
  private
    FOnMapChanged: TDBMapChangedEvent;

    function GetMapID: Variant;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure Init;

    property MapID: Variant read GetMapID;
    property OnMapChanged: TDBMapChangedEvent read FOnMapChanged write FOnMapChanged;
  end;

implementation

uses
  App.Service, ME.Service.Map, ME.Presenter.Map, ME.Edit.Map;

{$R *.fmx}

{ TMapFilter }

procedure TMapFilter.BindSourceDB1SubDataSourceDataChange(Sender: TObject; Field: TField);
begin
  if Assigned(FOnMapChanged) then
    FOnMapChanged(FID.Value);
end;

constructor TMapFilter.Create(AOwner: TComponent);
begin
  inherited;

  FOnMapChanged := nil;
end;

destructor TMapFilter.Destroy;
begin
  FOnMapChanged := nil;

  inherited;
end;

function TMapFilter.GetMapID: Variant;
begin
  Result := FID.Value;
end;

procedure TMapFilter.Init;
begin
  F.Close;
  F.Connection := AppService.DBConnection.Connection;
  F.SQL.Text := 'SELECT ID, Caption FROM Map';
  F.Open;
end;

procedure TMapFilter.acEditMapExecute(Sender: TObject);
var
  Presenter: TEditMapPresenter;
  Dialog: TedMap;
  Map: TDBMap;
begin
  Dialog := TedMap.Create(Self);
  try
    Map := TDBMap.Create;
    try
      if not MapService.GetAt(FID.Value, Map) then
        Exit;

      Presenter := TEditMapPresenter.Create(Dialog, Map);
      try
        if Presenter.Edit then
          F.RefreshRecord;
      finally
        Presenter.Free;
      end;
    finally
      Map.Free;
    end;
  finally
    Dialog.Free;
  end;
end;


end.
