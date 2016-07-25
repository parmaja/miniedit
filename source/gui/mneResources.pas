unit mneResources;
{$mode objfpc}{$H+}
{**
 * Mini Edit
 *
 * @license    GPL 2 (http://www.gnu.org/licenses/gpl.html)
 * @author    Zaher Dirkey <zaher at parmaja dot com>
 *}

interface

uses
  SysUtils, Classes, ImgList, Controls, contnrs, EditorEngine;

type

  { TEditorResource }

  TEditorResource = class(TDataModule)
    FileImages: TImageList;
    BookmarkImages: TImageList;
    DebugImages: TImageList;
    PanelImages: TImageList;
    ToolbarImageList: TImageList;
    procedure DataModuleCreate(Sender: TObject);
    procedure DataModuleDestroy(Sender: TObject);
  private
  public
    Extensions: TEditorExtensions;
    function GetFileImageIndex(const FileName: string): integer;
  end;

const
  DEBUG_IMAGE_EXECUTE = 0;
  DEBUG_IMAGE_BREAKPOINT = 1;
  DEBUG_IMAGE_MARGINES = 0;

var
  EditorResource: TEditorResource = nil;

implementation

function TEditorResource.GetFileImageIndex(const FileName: string): integer;
var
  Extension: TEditorExtension;
  s: string;
begin
  s := ExtractFileExt(FileName);
  if LeftStr(s, 1) = '.' then
    s := Copy(s, 2, MaxInt);

  Extension := Extensions.Find(s);
  if Extension <> nil then
    Result := Extension.ImageIndex
  else
    Result := 1;//any file
end;


{$R *.lfm}

procedure TEditorResource.DataModuleCreate(Sender: TObject);
begin
  Extensions := TEditorExtensions.Create(true);
  Extensions.Add('php', 3);
  Extensions.Add('pas', 4);
  Extensions.Add('d', 5);
  Extensions.Add('py', 6);
end;

procedure TEditorResource.DataModuleDestroy(Sender: TObject);
begin
  FreeAndNil(Extensions);
end;

end.
