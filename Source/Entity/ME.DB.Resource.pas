unit ME.DB.Resource;

interface

uses
  System.SysUtils, System.Classes, System.Variants, FMX.Graphics,
  Data.DB, ME.DB.Entity;

type
  TResourceKind = (Screenshot, QuestItem);

  TDBResource = class(TEntity)
  private
    FMarkerID: Variant;
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

    property MarkerID: Variant read FMarkerID write FMarkerID;
    property Kind: TResourceKind read FKind write FKind;
    property Description: string read FDescription write FDescription;
    property Picture: TBitmap read FPicture write SetPicture;
  end;

implementation

{ TDBResource }

constructor TDBResource.Create;
begin
  inherited;

  MarkerID := Null;
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
begin
  inherited;

  MarkerID := TDBResource(Source).MarkerID;
  Kind := TDBResource(Source).Kind;
  Description := TDBResource(Source).Description;
  Picture := TDBResource(Source).Picture;
end;

procedure TDBResource.Assign(const DataSet: TDataSet);
begin
  inherited;

  MarkerID := DataSet.FieldByName('MarkerID').Value;
  Kind := TResourceKind(DataSet.FieldByName('Kind').AsInteger);
  Description := DataSet.FieldByName('Description').AsString;

  if DataSet.FindField('Picture') <> nil then
    AssignPicture(DataSet.FieldByName('Picture'), Picture);
end;

class function TDBResource.EntityName: string;
begin
  Result := 'Resource';
end;

class function TDBResource.FieldList: string;
begin
  Result := 'ID, MarkerID, Kind, Description, Picture';
end;

end.
