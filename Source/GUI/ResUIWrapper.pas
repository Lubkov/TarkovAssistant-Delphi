unit ResUIWrapper;

interface

uses
  Vcl.Imaging.PNGImage;

type
  TResIUWrapper = class
  public
    class procedure LoadPNGImage(const ResourceName: string; Image: TPNGImage);
  end;

implementation

  procedure LoadPNGImageFromRes(ResourceFile: PChar; Image: TPNGImage); stdcall; export;
  external 'eft_uires.dll' name 'LoadPNGImageFromRes';

{ TResIUWrapper }

class procedure TResIUWrapper.LoadPNGImage(const ResourceName: string; Image: TPNGImage);
begin
  LoadPNGImageFromRes(PChar(ResourceName), Image);
end;

end.
