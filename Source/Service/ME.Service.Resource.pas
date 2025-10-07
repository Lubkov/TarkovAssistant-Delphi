unit ME.Service.Resource;

interface

uses
  System.SysUtils, System.Classes, System.IOUtils, System.Variants, FMX.Graphics,
  Generics.Collections, Data.DB, ME.DB.Entity, ME.DB.DAO, ME.DB.Service,
  ME.DB.Resource, ME.DAO.Resource;

type
  TResourceService = class(TServiceCommon)
  private
    function GetPictureFileName(const ID: Variant; const Kind: TResourceKind): string; overload;
    function GetPictureFileName(const Source: TDBResource): string; overload;
    function GetResourceDAO: TResourceDAO;
  protected
    function GetDAOClass: TDAOClass; override;
  public
    procedure Insert(const Entity: TDBEntity); override;
    procedure Update(const Entity: TDBEntity); override;
    procedure Remove(const ID: Variant); overload; override;
    procedure Remove(const ID: Variant; Kind: TResourceKind); overload;

    procedure LoadPicture(const ID: Variant; const Kind: TResourceKind; const Dest: TBitmap); overload;
    procedure LoadPicture(const Source: TDBResource; const Dest: TBitmap); overload;
    procedure LoadPicture(const Source: TDBResource); overload;
    procedure SavePicture(const Source: TDBResource);
    procedure DeletePicture(const ID: Variant; const Kind: TResourceKind); overload;
    procedure DeletePicture(const Source: TDBResource); overload;

    procedure LoadMarkerPictures(const MarkerID: Variant; const Items: TList<TDBResource>);
    procedure LoadMarkerQuestItems(const MarkerID: Variant; const Items: TList<TDBResource>);

    procedure ExportFromDB;

    property ResourceDAO: TResourceDAO read GetResourceDAO;
  end;

var
  ResourceService: TResourceService;

implementation

uses
  App.Service, ME.Service.QuestItem;

{ TResourceService }

function TResourceService.GetPictureFileName(const ID: Variant; const Kind: TResourceKind): string;
const
  ScreenshotFileFmt = 'Markers\Resource_%s.jpg';
  QuestItemFileFmt = 'Items\Resource_%s.png';
begin
  Result := '';
  case Kind of
    TResourceKind.Screenshot:
      Result := Format(ScreenshotFileFmt, [VarToStr(ID)]);
    TResourceKind.QuestItem:
      Result := Format(QuestItemFileFmt, [VarToStr(ID)]);
  end;
  Result := TPath.Combine(AppService.Options.DataPath, Result);
end;

function TResourceService.GetPictureFileName(const Source: TDBResource): string;
begin
  Result := GetPictureFileName(Source.ID, Source.Kind);
end;

function TResourceService.GetResourceDAO: TResourceDAO;
begin
  Result := TResourceDAO(DAO);
end;

function TResourceService.GetDAOClass: TDAOClass;
begin
  Result := TResourceDAO;
end;

procedure TResourceService.Insert(const Entity: TDBEntity);
var
  Resource: TDBResource;
begin
  Resource := TDBResource(Entity);

//  case Resource.Kind of
//    TResourceKind.Screenshot:
//      DAO.Insert(Resource);
//    TResourceKind.QuestItem:
//      ;
//  end;
  DAO.Insert(Resource);
  SavePicture(Resource);
end;

procedure TResourceService.Update(const Entity: TDBEntity);
begin
  inherited;

  SavePicture(TDBResource(Entity));
end;

procedure TResourceService.Remove(const ID: Variant);
begin
  inherited;

  DeletePicture(ID, TResourceKind.Screenshot);
  DeletePicture(ID, TResourceKind.QuestItem);
end;

procedure TResourceService.Remove(const ID: Variant; Kind: TResourceKind);
begin
  inherited Remove(ID);

  DeletePicture(ID, Kind);
end;

procedure TResourceService.LoadPicture(const ID: Variant; const Kind: TResourceKind; const Dest: TBitmap);
var
  FileName: string;
begin
  FileName := GetPictureFileName(ID, Kind);
  if FileExists(FileName) then
    Dest.LoadFromFile(FileName)
  else
    Dest.Assign(nil);
end;

procedure TResourceService.LoadPicture(const Source: TDBResource; const Dest: TBitmap);
begin
  LoadPicture(Source.ID, Source.Kind, Dest);
end;

procedure TResourceService.LoadPicture(const Source: TDBResource);
begin
  LoadPicture(Source, Source.Picture);
end;

procedure TResourceService.SavePicture(const Source: TDBResource);
var
  FileName: string;
begin
  if Source.Picture.IsEmpty then
    DeletePicture(Source)
  else begin
    FileName := GetPictureFileName(Source);
    Source.Picture.SaveToFile(FileName);
  end;
end;

procedure TResourceService.DeletePicture(const ID: Variant; const Kind: TResourceKind);
var
  FileName: string;
begin
  FileName := GetPictureFileName(ID, Kind);

  if FileExists(FileName) then
    TFile.Delete(FileName);
end;

procedure TResourceService.DeletePicture(const Source: TDBResource);
begin
  DeletePicture(Source.ID, Source.Kind);
end;

procedure TResourceService.ExportFromDB;
var
  Items: TObjectList<TDBEntity>;
  Resource: TDBResource;
  i: Integer;
begin
  Items := TObjectList<TDBEntity>.Create;
  try
    ResourceDAO.GetAll(Items);

     for i := 0 to Items.Count - 1 do begin
       Resource := TDBResource(Items[i]);
       SavePicture(Resource);
     end;
  finally
    Items.Free;
  end;
end;

procedure TResourceService.LoadMarkerPictures(const MarkerID: Variant; const Items: TList<TDBResource>);
begin
  ResourceDAO.GetResources(MarkerID, TResourceKind.Screenshot, Items);
end;

procedure TResourceService.LoadMarkerQuestItems(const MarkerID: Variant; const Items: TList<TDBResource>);
begin
  ResourceDAO.GetResources(MarkerID, TResourceKind.QuestItem, Items);
end;

end.
