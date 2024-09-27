unit ME.DB.Profile;

interface

uses
  System.SysUtils, System.Classes, System.Variants, Data.DB, ME.DB.Entity;

type
  TPMCType = (pmcBear, pmcUsec);

  TProfile = class(TEntity)
  private
    FName: string;
    FKind: TPMCType;
  public
    constructor Create; override;
    destructor Destroy; override;

    procedure Assign(const Source: TEntity); overload; override;
    procedure Assign(const DataSet: TDataSet); overload; override;

    class function EntityName: string; override;
    class function FieldList: string; override;

    property Name: string read FName write FName;
    property Kind: TPMCType read FKind write FKind;
  end;

implementation

{ TProfile }

constructor TProfile.Create;
begin
  inherited;

  FName := '';
  FKind := TPMCType.pmcBear;
end;

destructor TProfile.Destroy;
begin

  inherited;
end;

procedure TProfile.Assign(const Source: TEntity);
begin
  inherited;

  FName := TProfile(Source).Name;
  FKind := TProfile(Source).Kind;
end;

procedure TProfile.Assign(const DataSet: TDataSet);
begin
  inherited;

  FName := DataSet.FieldByName('Name').AsString;
  FKind := TPMCType(DataSet.FieldByName('Kind').AsInteger);
end;

class function TProfile.EntityName: string;
begin
  Result := 'Profile';
end;

class function TProfile.FieldList: string;
begin
  Result := 'ID, Name, Kind';
end;

end.
