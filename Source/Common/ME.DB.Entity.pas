unit ME.DB.Entity;

interface

uses
  System.Classes, System.SysUtils, System.Variants, Data.DB;

type
  TEntityClass = class of TEntity;

  TEntity = class(TObject)
  private
    function GetIsNewInstance: Boolean;
  protected
    FID: Variant;
  public
    constructor Create; virtual;
    destructor Destroy; override;

    procedure Assign(const Source: TEntity); overload; virtual;
    procedure Assign(const DataSet: TDataSet); overload; virtual;

    class function EntityName: string; virtual; abstract;
    class function FieldList: string; virtual; abstract;

    property ID: Variant read FID write FID;
    property IsNewInstance: Boolean read GetIsNewInstance;
  end;

implementation

{ TEntity }

constructor TEntity.Create;
begin
  inherited;

  FID := Null;
end;

destructor TEntity.Destroy;
begin

  inherited;
end;

function TEntity.GetIsNewInstance: Boolean;
begin
  Result := VarIsNull(ID) or VarIsEmpty(ID);
end;

procedure TEntity.Assign(const Source: TEntity);
begin
  FID := Source.ID;
end;

procedure TEntity.Assign(const DataSet: TDataSet);
begin
  FID := DataSet.FieldByName('ID').Value;
end;

end.
