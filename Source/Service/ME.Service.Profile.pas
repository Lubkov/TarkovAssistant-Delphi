unit ME.Service.Profile;

interface

uses
  System.SysUtils, System.Classes, Data.DB, ME.DB.Entity, ME.DB.DAO, ME.DB.Service,
  ME.DB.Profile, ME.DAO.Profile;

type
  TProfileService = class(TServiceCommon)
  protected
    function GetDAOClass: TDAOClass; override;
  public
    function GetByName(const Name: string; const Entity: TEntity): Boolean;
  end;

var
  ProfileService: TProfileService;

implementation

{ TProfileService }

function TProfileService.GetDAOClass: TDAOClass;
begin
  Result := TProfileDAO;
end;

function TProfileService.GetByName(const Name: string; const Entity: TEntity): Boolean;
begin
  Result := TProfileDAO(DAO).GetAtName(Name, Entity);
end;

end.
