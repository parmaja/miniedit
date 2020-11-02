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
  Classes, SysUtils, Contnrs, mnClasses, SynEdit;

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

    { TEditorElement }

    TEditorElement = class(TPersistent)
    private
    protected
      FName: string;
      FTitle: string;
      FDescription: string;
      FImageIndex: integer;
      function GetDescription: string; virtual;
    public
      constructor Create; virtual;

      property Name: string read FName write FName;
      property Title: string read FTitle write FTitle;
      property Description: string read GetDescription write FDescription;
      property ImageIndex: integer read FImageIndex write FImageIndex;
    end;

    { TEditorElements }

    TEditorElements = class(specialize TmnNamedObjectList<TEditorElement>)
    private
    public
    end;

    TEditorChangeStates = set of (
      ecsChanged,
      ecsState,
      ecsRefresh,
      ecsRecents, //Recent files, folders, projects changes
      ecsOptions,
      ecsDebug,
      ecsShow,
      ecsEdit,
      ecsFolder,
      ecsProject,
      ecsProjectLoaded
    ); //ecsShow bring to front

    TNotifyMessageType = (msgtStatus, msgtEndStatus, msgtLog, msgtOutput, msgtError);
    TEditorAction = (eaShowFolders, eaShowDatabases, eaShowProject, eaClearOutput, eaClearLog, eaEnd);

    TErrorInfo = record
      ID: Integer;
      Line: Integer;
      Column: Integer;
      Name: string;
      FileName: string;
      Message: string;
    end;

    { INotifyEngine }

    INotifyEngine = interface(IInterface)
      ['{1BFF59EA-1B50-483D-A34C-9C925ABCD88C}']
      procedure ChangeState(State: TEditorChangeStates);
    end;

    ISettingNotifyEngine = interface(INotifyEngine)
      ['{371F2173-3169-4C93-9EDC-206BDDA9903F}']
      procedure SaveOptions;
      procedure LoadOptions;
    end;

    IEditorNotifyEngine = interface(INotifyEngine)
      ['{B3ACF9DF-2A37-4693-A7A5-FBF47DCAFC92}']
      procedure EngineAction(EngineAction: TEditorAction);
      procedure EngineMessage(S: string; vMessageType: TNotifyMessageType; vError: TErrorInfo);
      procedure EngineReplaceText(Sender: TObject; const ASearch, AReplace: string; Line, Column: integer; var ReplaceAction: TSynReplaceAction);
    end;

implementation

{ TEditorElement }

function TEditorElement.GetDescription: string;
begin
  Result := FDescription;
end;

constructor TEditorElement.Create;
begin
  inherited Create;
  FImageIndex := -1;
end;

end.

