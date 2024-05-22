library eft_uires;

{$R *.dres}

uses
  System.SysUtils,
  System.Classes,
  Winapi.Windows,
  Vcl.Imaging.PNGImage,
  EFT.ResourceConsts in 'Source\EFT.ResourceConsts.pas';

{$R *.res}

procedure LoadPNGImageFromRes(ResourceFile: PChar; Image: TPNGImage); stdcall; export;
var
  Stream: TResourceStream;
begin
  Stream := TResourceStream.Create(HInstance, ResourceFile, RT_RCDATA);
  try
    Stream.Position := 0;
    Image.LoadFromStream(Stream);
  finally
    Stream.Free;
  end;
end;

exports
  LoadPNGImageFromRes;

begin
end.
