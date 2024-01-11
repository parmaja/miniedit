unit mneResources;
{$mode objfpc}{$H+}
{**
 * Mini Edit
 *
 * @license    GPL 2 (http://www.gnu.org/licenses/gpl.html)
 * @author    Zaher Dirkey 
 *}

interface

uses
  SysUtils, Classes, ImgList, Controls, contnrs, StrUtils,
  LCLType,
  EditorEngine;

type
  TThemeStyle = (thsLight, thsDark);

  { TEditorResource }

  TEditorResource = class(TDataModule)
    FileImages: TImageList;
    BookmarkImages: TImageList;
    DebugImages: TImageList;
    PanelImages: TImageList;
    PanelImages1: TImageList;
    ToolbarImageList: TImageList;
    procedure DataModuleCreate(Sender: TObject);
    procedure DataModuleDestroy(Sender: TObject);
  private
  public
    Extensions: TEditorExtensions;
    function GetFileImageIndex(FileName: string; DefaultImage: Integer = -1): integer;
    function GetImageIndex(AName: string; DefaultImage: Integer = -1): integer;
    procedure Invert(Style: TThemeStyle);
  end;

const
  DEBUG_IMAGE_EXECUTE = 0;
  DEBUG_IMAGE_BREAKPOINT = 1;
  DEBUG_IMAGE_MARGINES = 0;

  cDatabaseImage = 13;

var
  EditorResource: TEditorResource = nil;

implementation

uses
  Graphics, GraphType;

procedure TEditorResource.Invert(Style: TThemeStyle);
var
  img: TRawImage;
  Bmp: TBitmap;
  p: PRGBAQuad;
  c, i: integer;
  new: Byte;
begin
//  exit;//TODO there is bug in it
  PanelImages.BeginUpdate;
  try
    if Style = thsLight then
      new := 0
    else
      new := $FF;
    Bmp := TBitmap.Create;
    try
      PanelImages.GetFullBitmap(Bmp);
      PanelImages.Clear;
      Bmp.TransparentColor := clFuchsia;
      Bmp.Transparent := True;
      Bmp.BeginUpdate;
      Img := Bmp.RawImage;
      p := PRGBAQuad(img.Data);
      c := img.DataSize div SizeOf(p^);
      i := 0;
      while i < c do
      begin
        p^.Red := new;
        p^.Green := new;
        p^.Blue := new;
        inc(p);
        inc(i);
      end;
      Bmp.EndUpdate;
      PanelImages.AddSliced(Bmp, Bmp.Width div PanelImages.Width, Bmp.Height div PanelImages.Height);
    finally
      Bmp.Free;
    end;
  finally
    PanelImages.EndUpdate;
  end;
end;

{$R *.lfm}

procedure TEditorResource.DataModuleDestroy(Sender: TObject);
begin
  FreeAndNil(Extensions);
end;

procedure TEditorResource.DataModuleCreate(Sender: TObject);
begin
  Extensions := TEditorExtensions.Create(true);

  //Group or Category name not extension
  Extensions.Add('txt', 1);
  Extensions.Add('md', 1);
  Extensions.Add('php', 3);
  Extensions.Add('pas', 4);
  Extensions.Add('d', 5);
  Extensions.Add('py', 6);
  Extensions.Add('lua', 7);
  Extensions.Add('mne-project', 8);
  Extensions.Add('Image', 9);
  Extensions.Add('cmd', 10);
  Extensions.Add('sh', 10);
  Extensions.Add('bat', 10);
  Extensions.Add('go', 11);
  Extensions.Add('sql', 12);
  Extensions.Add('db', cDatabaseImage);
  Extensions.Add('postgresql', 14);
  Extensions.Add('sqlite', 15);
  Extensions.Add('firebird', 16);
  Extensions.Add('mysql', 17);
  Extensions.Add('csv', 18);
  Extensions.Add('nim', 19);
end;

function TEditorResource.GetFileImageIndex(FileName: string; DefaultImage: Integer): integer;
var
  Extension: TEditorExtension;
  s: string;
  g: TFileGroup;
begin
  s := ExtractFileName(FileName);
  g := Engine.Groups.FindGroup(s);
  if g = nil then
    Result := DefaultImage //have no group
  else
  begin
    Extension := Extensions.Find(g.Name);
    if Extension <> nil then
      Result := Extension.ImageIndex
    else
    begin
      if g.Category <> nil then
      begin
        Extension := Extensions.Find(g.Category.ImageName);
        if Extension <> nil then
          Extension := Extensions.Find(g.Category.Name);
      end;

      if Extension <> nil then
        Result := Extension.ImageIndex
      else
        Result := 1; //our default beause it have a group
    end;
  end;
end;

function TEditorResource.GetImageIndex(AName: string; DefaultImage: Integer): integer;
var
  Extension: TEditorExtension;
begin
  if LeftStr(AName, 1) = '.' then
    AName := MidStr(AName, 2, MaxInt);
  Extension := Extensions.Find(LowerCase(AName));
  if Extension <> nil then
    Result := Extension.ImageIndex
  else
    Result := DefaultImage;
end;

end.
