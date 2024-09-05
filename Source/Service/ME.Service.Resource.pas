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
  protected
    function GetDAOClass: TDAOClass; override;
  public
    procedure Insert(const Entity: TEntity); override;
    procedure Update(const Entity: TEntity); override;
    procedure Remove(const ID: Variant); override;

    procedure LoadPicture(const ID: Variant; const Kind: TResourceKind; const Dest: TBitmap); overload;
    procedure LoadPicture(const Source: TDBResource; const Dest: TBitmap); overload;
    procedure LoadPicture(const Source: TDBResource); overload;
    procedure SavePicture(const Source: TDBResource);
    procedure DeletePicture(const ID: Variant; const Kind: TResourceKind); overload;
    procedure DeletePicture(const Source: TDBResource); overload;

    procedure LoadMarkerPictures(const MarkerID: Variant; const Items: TList<TDBResource>);

    procedure ExportFromDB;
  end;

var
  ResourceService: TResourceService;

implementation

uses
  App.Constants;

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
  Result := TPath.Combine(AppParams.DataPath, Result);
end;

function TResourceService.GetPictureFileName(const Source: TDBResource): string;
begin
  Result := GetPictureFileName(Source.ID, Source.Kind);
end;

function TResourceService.GetDAOClass: TDAOClass;
begin
  Result := TResourceDAO;
end;

procedure TResourceService.Insert(const Entity: TEntity);
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

procedure TResourceService.Update(const Entity: TEntity);
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
  Items: TObjectList<TEntity>;
  Resource: TDBResource;
  i: Integer;
begin
  Items := TObjectList<TEntity>.Create;
  try
    TResourceDAO(DAO).GetAll(Items);

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
  TResourceDAO(DAO).GetPictures(MarkerID, Items);
end;

end.
