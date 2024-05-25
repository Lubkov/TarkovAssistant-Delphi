unit ME.DB.Entity;

interface

uses
  System.Classes, System.SysUtils, System.Variants, FMX.Graphics, Data.DB,
  Uni;

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
    class procedure AssignPicture(const Field: TField; Bitmap: TBitmap);
    class procedure AssignPictureTo(const Source: TBitmap; const Field: TField); overload;
    class procedure AssignPictureTo(const Source: TBitmap; Param: TParam); overload;

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

class procedure TEntity.AssignPicture(const Field: TField; Bitmap: TBitmap);
var
  Stream: TMemoryStream;
  EmptyPicture: TBitmap;
begin
  if Field.IsNull then begin
    EmptyPicture := TBitmap.Create;
    try
      Bitmap. Assign(EmptyPicture);
    finally
      EmptyPicture.Free;
    end;

    Exit;
  end;

  Stream := TMemoryStream.Create;
  try
    TBlobField(Field).SaveToStream(Stream);
    Stream.Position := 0;
    if Stream.Size > 0 then
      Bitmap.LoadFromStream(Stream)
    else
      Bitmap.Assign(nil);
  finally
    Stream.Free;
  end;
end;

class procedure TEntity.AssignPictureTo(const Source: TBitmap; const Field: TField);
var
  Stream: TMemoryStream;
begin
  Stream := TMemoryStream.Create;
  try
    Source.SaveToStream(Stream);
    Stream.Position := 0;
    TBlobField(Field).LoadFromStream(Stream);
  finally
    Stream.Free;
  end;
end;

class procedure TEntity.AssignPictureTo(const Source: TBitmap; Param: TParam);
var
  Stream: TMemoryStream;
begin
 if Source.IsEmpty then begin
   Param.Value := Null;
   Exit;
 end;

  Stream := TMemoryStream.Create;
  try
    Source.SaveToStream(Stream);
    Stream.Position := 0;

    TUniParam(Param).LoadFromStream(Stream, ftBlob);
  finally
    Stream.Free;
  end;
end;

end.
