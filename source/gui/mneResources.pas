unit mneResources;
{$mode delphi}
{**
 * Mini Edit
 *
 * @license    GPL 2 (http://www.gnu.org/licenses/gpl.html)
 * @author    Zaher Dirkey <zaher at parmaja dot com>
 *}

interface

uses
  SysUtils, Classes, ImgList, Controls;

type

  { TEditorResource }

  TEditorResource = class(TDataModule)
    SmallImages: TImageList;
    BookmarkImages: TImageList;
    ToolbarImageList: TImageList;
    procedure DataModuleCreate(Sender: TObject);
  private
  public
  end;

var
  EditorResource: TEditorResource = nil;

implementation

{$R *.lfm}

procedure TEditorResource.DataModuleCreate(Sender: TObject);
begin
end;

end.
