unit ME.DB.Profile;

interface

uses
  System.SysUtils, System.Classes, System.Variants, Data.DB, ME.DB.Entity;

type
  TProfile = class(TEntity)
  private
    FName: string;
  public
    constructor Create; override;
    destructor Destroy; override;

    procedure Assign(const Source: TEntity); overload; override;
    procedure Assign(const DataSet: TDataSet); overload; override;

    class function EntityName: string; override;
    class function FieldList: string; override;

    property Name: string read FName write FName;
  end;

implementation

{ TProfile }

constructor TProfile.Create;
begin
  inherited;

  FName := '';
end;

destructor TProfile.Destroy;
begin

  inherited;
end;

procedure TProfile.Assign(const Source: TEntity);
begin
  inherited;

  FName := TProfile(Source).Name;
end;

procedure TProfile.Assign(const DataSet: TDataSet);
begin
  inherited;

  FName := DataSet.FieldByName('Name').AsString;
end;

class function TProfile.EntityName: string;
begin
  Result := 'Profile';
end;

class function TProfile.FieldList: string;
begin
  Result := 'ID, Name';
end;

end.
