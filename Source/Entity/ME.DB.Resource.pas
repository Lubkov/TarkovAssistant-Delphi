unit ME.DB.Resource;

interface

uses
  System.SysUtils, System.Classes, System.Variants, FMX.Graphics,
  Data.DB, App.Entity, ME.DB.Entity;

type
  TResourceKind = (Screenshot, QuestItem);

  TDBResource = class(TDBEntity)
  private
    FKind: TResourceKind;
    FDescription: string;
    FPicture: TBitmap;

    procedure SetPicture(const Value: TBitmap);
  public
    constructor Create; override;
    destructor Destroy; override;

    procedure Assign(const Source: TEntity); overload; override;
    procedure Assign(const DataSet: TDataSet); overload; override;

    class function EntityName: string; override;
    class function FieldList: string; override;

    class function KindToInt(const Value: TResourceKind): Integer;
    class function IntToKind(const Value: Integer): TResourceKind;

    property Kind: TResourceKind read FKind write FKind;
    property Description: string read FDescription write FDescription;
    property Picture: TBitmap read FPicture write SetPicture;
  end;

implementation

{ TDBResource }

constructor TDBResource.Create;
begin
  inherited;

  Kind := TResourceKind.Screenshot;
  FDescription := '';
  FPicture := TBitmap.Create;
end;

destructor TDBResource.Destroy;
begin
  FreeAndNil(FPicture);

  inherited;
end;

procedure TDBResource.SetPicture(const Value: TBitmap);
begin
  FPicture.Assign(Value);
end;

procedure TDBResource.Assign(const Source: TEntity);
var
  Resource: TDBResource;
begin
  inherited;

  Resource := TDBResource(Source);

  Kind := Resource.Kind;
  Description := Resource.Description;
  Picture := Resource.Picture;
end;

procedure TDBResource.Assign(const DataSet: TDataSet);
begin
  inherited;

  Kind := TDBResource.IntToKind(DataSet.FieldByName('Kind').AsInteger);
  Description := DataSet.FieldByName('Description').AsString;

//  if DataSet.FindField('Picture') <> nil then
//    AssignPicture(DataSet.FieldByName('Picture'), Picture);
end;

class function TDBResource.EntityName: string;
begin
  Result := 'Resource';
end;

class function TDBResource.FieldList: string;
begin
  Result := 'ID, Kind, Description'; //, Picture';
end;

class function TDBResource.KindToInt(const Value: TResourceKind): Integer;
begin
  Result := Ord(Value) + 1;
end;

class function TDBResource.IntToKind(const Value: Integer): TResourceKind;
begin
  Result := TResourceKind(Value - 1);
end;

end.
