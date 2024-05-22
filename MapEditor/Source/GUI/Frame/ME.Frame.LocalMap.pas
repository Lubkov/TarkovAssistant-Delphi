unit ME.Frame.LocalMap;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Data.DB, Vcl.ExtCtrls,
  MemDS, DBAccess, Uni, Vcl.Grids, Vcl.DBGrids, cxGraphics, cxLookAndFeels,
  cxLookAndFeelPainters, Vcl.Menus, dxSkinsCore, dxSkinOffice2013White,
  Vcl.StdCtrls, cxButtons, System.ImageList, Vcl.ImgList, cxImageList,
  System.Actions, Vcl.ActnList, ME.LocalMap, ME.Dialog.Message;

type
  TLocalMapChangeEvent = procedure(const LocalMap: TLocalMap) of object;

  TfrLocalMap = class(TFrame)
    Grid: TDBGrid;
    DS: TDataSource;
    F: TUniQuery;
    edTopPanel: TPanel;
    FID: TIntegerField;
    FName: TStringField;
    FLeftX: TIntegerField;
    FLeftY: TIntegerField;
    FRightX: TIntegerField;
    FRightY: TIntegerField;
    ActionList1: TActionList;
    acAddMap: TAction;
    acEditMap: TAction;
    acDeleteMap: TAction;
    cxImageList1: TcxImageList;
    edAddMap: TcxButton;
    edEditMap: TcxButton;
    edDeleteMap: TcxButton;
    PopupMenu1: TPopupMenu;
    N1: TMenuItem;
    N2: TMenuItem;
    N3: TMenuItem;
    FLeftID: TIntegerField;
    FRightID: TIntegerField;
    procedure acAddMapExecute(Sender: TObject);
    procedure acEditMapExecute(Sender: TObject);
    procedure acDeleteMapExecute(Sender: TObject);
  private
    FLocalMap: TLocalMap;
    FOnLocalMapChange: TLocalMapChangeEvent;

    function InternalMapEdit(const LocalMap: TLocalMap): Boolean;
    procedure UpdateCurrentItem;

    procedure OnDataChange(Sender: TObject; Field: TField);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure Init(const Connection: TUniConnection);

    property OnLocalMapChange: TLocalMapChangeEvent read FOnLocalMapChange write FOnLocalMapChange;
  end;

implementation

uses
  ME.DB.Utils, ME.Dialog.Presenter, ME.Presenter.LocalMap, ME.Edit.LocalMap,
  ME.LocalMapService;

{$R *.dfm}

{ TfrLocalMap }

constructor TfrLocalMap.Create(AOwner: TComponent);
begin
  inherited;

  FLocalMap := TLocalMap.Create;
  FOnLocalMapChange := nil;
end;

destructor TfrLocalMap.Destroy;
begin
  FOnLocalMapChange := nil;
  FLocalMap.Free;

  inherited;
end;

function TfrLocalMap.InternalMapEdit(const LocalMap: TLocalMap): Boolean;
var
  Presenter: TEditMapPresenter;
  Dialog: TedLocalMap;
begin
  Dialog := TedLocalMap.Create(Self);
  try
    Presenter := TEditMapPresenter.Create(Dialog, LocalMap);
    try
      Result := Presenter.Edit;
    finally
      Presenter.Free;
    end;
  finally
    Dialog.Free;
  end;
end;

procedure TfrLocalMap.UpdateCurrentItem;
begin
  FLocalMap.ID := FID.Value;
  FLocalMap.Name := FName.AsString;
  FLocalMap.Left.ID := FLeftID.Value;
  FLocalMap.Left.SetBounds(FLeftX.AsInteger, FLeftY.AsInteger);
  FLocalMap.Right.ID := FRightID.Value;
  FLocalMap.Right.SetBounds(FRightX.AsInteger, FRightY.AsInteger);
  LocalMapService.LoadMapLevels(FLocalMap, True);

  if Assigned(FOnLocalMapChange) then
    FOnLocalMapChange(FLocalMap);  
end;

procedure TfrLocalMap.OnDataChange(Sender: TObject; Field: TField);
begin
  UpdateCurrentItem;
end;

procedure TfrLocalMap.Init(const Connection: TUniConnection);
begin
  F.Connection := Connection;
  F.SQL.Text :=
    'SELECT ' +
    '    m.ID as ID, ' +
    '    m.Name as Name, ' +
    '    p1.ID as LeftID, ' +
    '    p1.X as LeftX, ' +
    '    p1.Y as LeftY, ' +
    '    p2.ID as RightID, ' +
    '    p2.X as RightX, ' +
    '    p2.Y as RightY ' +
    'FROM LocalMap m ' +
    '  INNER JOIN Point p1 ON p1.ID = m.LeftID ' +
    '  INNER JOIN Point p2 ON p2.ID = m.RightID ';
  F.Open;

  DS.OnDataChange := OnDataChange;
  UpdateCurrentItem;
end;

procedure TfrLocalMap.acAddMapExecute(Sender: TObject);
var
  LocalMap: TLocalMap;
begin
  LocalMap := TLocalMap.Create;
  try
    if InternalMapEdit(LocalMap) then
      F.RefreshRecord;
  finally
    LocalMap.Free;
  end;
end;

procedure TfrLocalMap.acEditMapExecute(Sender: TObject);
//var
//  LocalMap: TLocalMap;
begin
//  LocalMap := TLocalMap.Create;
  try
    UpdateCurrentItem;
    if not IsNullID(FLocalMap.ID) and InternalMapEdit(FLocalMap) then
      F.RefreshRecord;
  finally
//    LocalMap.Free;
  end;
end;

procedure TfrLocalMap.acDeleteMapExecute(Sender: TObject);
var
  LocalMap: TLocalMap;
  Presenter: TDelMapPresenter;
  Dialog: TedMessage;
begin
//  LocalMap := TLocalMap.Create;
  try
    UpdateCurrentItem;

    Dialog := TedMessage.Create(Self);
    try
      Presenter := TDelMapPresenter.Create(Dialog, FLocalMap);
      try
        if Presenter.Delete then
          F.RefreshRecord;
      finally
        Presenter.Free;
      end;
    finally
      Dialog.Free;
    end;
//    if not IsNullID(LocalMap.ID) and InternalMapEdit(LocalMap) then
//      F.Refresh;
  finally
//    LocalMap.Free;
  end;
end;

end.
