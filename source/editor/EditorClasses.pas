unit EditorClasses;
{$mode objfpc}{$H+}
{$INTERFACES CORBA} //Needed for interfaces without guid, see below
{**
 * Mini Edit
 *
 * @license    GPL 2 (http://www.gnu.org/licenses/gpl.html)
 * @author    Zaher Dirkey <zaher at parmaja dot com>
 *}

interface

uses
  Classes, SysUtils, contnrs;

  type
    IFileEditor = interface
    end;

    IControlEditor = interface(IFileEditor)
    end;

    IClipboardEditor = interface(IFileEditor)
      function CanPaste: Boolean;
      procedure Paste;
      procedure Cut;
      function CanCopy: Boolean;
      procedure Copy;
      procedure SelectAll;
    end;

    ITextEditor = interface(IClipboardEditor)
    end;

    IExecuteEditor = interface
    end;

    IWatchEditor = interface
    end;

    IFormEditor = interface
    end;

    { TmneObjectList }

    TmneObjectList = class(TObjectList)
    protected
      procedure Created; virtual;
    public
      procedure AfterConstruction; override;
    end;

    { GmneObjects }

    generic GmneObjects<_Object_> = class(TmneObjectList)
    protected
      function GetItem(Index: Integer): _Object_;
    public
      function Add(AItem: _Object_): Integer;
      property Items[Index: Integer]: _Object_ read GetItem; default;
    end;

implementation

{ GmneObjects }

function GmneObjects.GetItem(Index: Integer): _Object_;
begin
  Result := _Object_(inherited Items[Index]);
end;

function GmneObjects.Add(AItem: _Object_): Integer;
begin
  Result := inherited Add(AItem);
end;

{ TmneObjectList }

procedure TmneObjectList.Created;
begin
  inherited AfterConstruction;
  Created;
end;

procedure TmneObjectList.AfterConstruction;
begin
  inherited AfterConstruction;
end;

end.

