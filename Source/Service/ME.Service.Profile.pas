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
  end;

var
  ProfileService: TProfileService;

implementation

{ TProfileService }

function TProfileService.GetDAOClass: TDAOClass;
begin
  Result := TProfileDAO;
end;

end.
