unit EditorAddons;
{$mode delphi}
{**
 * Mini Edit
 *
 * @license    GPL 2 (http://www.gnu.org/licenses/gpl.html)
 * @author    Zaher Dirkey <zaher at parmaja dot com>
 *}

{
  Link to SCM manager like TSVN and TGIT
}
interface

uses
  SysUtils, Forms, StrUtils, Variants, Classes, Controls, Graphics, Contnrs, SynEdit;

type
  TEnumMenuCallback = procedure(Caption, Name: string; OnClick: TNotifyEvent) of object;

  { TEditorAddon }

  TEditorAddon = class(TObject)
  private
  protected
  public
    procedure EnumMenu(Callback: TEnumMenuCallback); virtual;
  end;

implementation

{ TEditorAddon }

procedure TEditorAddon.EnumMenu(Callback: TEnumMenuCallback);
begin
end;

end.

