unit ME.Frame.MapLevel;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, cxGraphics,
  cxLookAndFeels, cxLookAndFeelPainters, Vcl.Menus, dxSkinsCore,
  dxSkinOffice2013White, Data.DB, Vcl.Grids, Vcl.DBGrids, System.ImageList,
  Vcl.ImgList, cxImageList, System.Actions, Vcl.ActnList, MemDS, DBAccess, Uni,
  Vcl.StdCtrls, cxButtons, Vcl.ExtCtrls, VirtualTable, ME.LocalMap, ME.MapLevel,
  cxControls, cxContainer, cxEdit, cxImage;

type
  TfrMapLevel = class(TFrame)
    edTopPanel: TPanel;
    edAddMapLevel: TcxButton;
    edEditMapLevel: TcxButton;
    edDeleteMapLevel: TcxButton;
    DS: TDataSource;
    ActionList1: TActionList;
    acAddMapLevel: TAction;
    acEditMapLevel: TAction;
    acDeleteMapLevel: TAction;
    ImageList: TcxImageList;
    Grid: TDBGrid;
    GridMenu: TPopupMenu;
    N1: TMenuItem;
    N2: TMenuItem;
    N3: TMenuItem;
    F: TVirtualTable;
    FID: TIntegerField;
    FMapID: TIntegerField;
    FLevel: TIntegerField;
    FPicture: TBlobField;
    imMapPicture: TImage;
    paPicture: TPanel;
    imMapPicture2: TcxImage;
  private
    FLocalMap: TLocalMap;

    procedure OnDataChange(Sender: TObject; Field: TField);
  public
    constructor Create(AOwner: TComponent); override;

    procedure Init(const LocalMap: TLocalMap);
  end;

implementation

{$R *.dfm}

{ TfrMapLevel }

constructor TfrMapLevel.Create(AOwner: TComponent);
begin
  inherited;

//  VirtualTable1.AddField('ID', ftInteger);
//  VirtualTable1.AddField('FMapID', ftInteger);
//  VirtualTable1.AddField('FLevel', ftInteger);
//  VirtualTable1.AddField('JOB', ftString, 9);
end;

procedure TfrMapLevel.OnDataChange(Sender: TObject; Field: TField);
//var
//  Stream: TMemoryStream;
begin
  imMapPicture2.Picture.Assign(FLocalMap.Levels[F.RecNo - 1].Picture);

//  if FPicture.IsNull then
//    imMapPicture.Picture.Assign(nil)
//  else begin
//    Stream := TMemoryStream.Create;
//    try
//      TBlobField(Field).SaveToStream(Stream);
//      Stream.Position := 0;
//      Picture.LoadFromStream(Stream);
//    finally
//      Stream.Free;
//    end;
//  end;
end;

procedure TfrMapLevel.Init(const LocalMap: TLocalMap);
var
  MapLevel: TMapLevel;
begin
  FLocalMap := LocalMap;

  DS.OnDataChange := nil;
  F.DisableControls;
  try
   F.Open;
   F.Clear;

    for MapLevel in LocalMap.Levels do begin
      F.Append;
      FID.Value := MapLevel.ID;
      FMapID.Value := MapLevel.MapID;
      FLevel.AsInteger := MapLevel.Level;
      MapLevel.AssignPictureTo(FPicture);
      F.Post;
    end;

    F.First;
  finally
    F.EnableControls;
    DS.OnDataChange := OnDataChange;
    OnDataChange(F, nil);
  end;
end;

end.
