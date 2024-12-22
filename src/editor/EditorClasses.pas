unit EditorClasses;
{$mode objfpc}{$H+}
{$INTERFACES CORBA} //Needed for interfaces without guid, see below
{**
 * Mini Edit
 *
 * @license    GPL 2 (http://www.gnu.org/licenses/gpl.html)
 * @author    Zaher Dirkey 
 *}

interface

uses
  Classes, SysUtils, Contnrs, mnClasses, SynEdit, ntvUtils;

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

    TEditorElement = class(TInterfacedPersistent, INamedObject)
    private
      function GetName: string;
      procedure SetName(const AValue: string);
    protected
      FName: string;
      FTitle: string;
      FDescription: string;
      FImageIndex: integer;
      function GetDescription: string; virtual;
    public
      constructor Create; virtual;

      property Name: string read GetName write SetName;
      property Title: string read FTitle write FTitle;
      property Description: string read GetDescription write FDescription;
      property ImageIndex: integer read FImageIndex write FImageIndex;
    end;

    { TEditorElements }

    TEditorElements = class(specialize TINamedObjects<TEditorElement>)
    private
    public
    end;

    TEditorChangeState = (
      ecsChanged,
      ecsState,
      ecsRefresh,
      ecsRecents, //Recent files, folders, projects changes
      ecsOptions,
      ecsDebug,
      ecsShow,
      ecsEdit,
      ecsFolder,
      ecsMenu,
      ecsProject,
      ecsProjectLoaded,
      ecsTransaction

    ); //ecsShow bring to front

    TEditorChangeStates = set of TEditorChangeState;

    TNotifyMessageType = (
      msgtStatus,
      msgtEndStatus,
      msgtLog,
      msgtOutput,
      msgtInteractive //like msgtOutput but should parsed
    );
    TEditorAction = (eaShowFolders, eaShowDatabases, eaShowProject, eaClearOutput, eaClearLog, eaEnd);

    TMessageKind = (mskInfo, mskHint, mskWarning, mskError);

    TMessageInfo = record
      Processed: Boolean;
      Kind: TMessageKind;
      MessageType: TNotifyMessageType;
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
    end;

    INotifyEngineState = interface(INotifyEngine)
      ['{F5088D63-9DA3-4957-B6DA-A79577665B7A}']
      procedure ChangeState(var State: TEditorChangeStates);
    end;

    INotifyEngineSetting = interface(INotifyEngine)
      ['{371F2173-3169-4C93-9EDC-206BDDA9903F}']
      procedure SaveOptions;
      procedure LoadOptions;
    end;

    INotifyEngineEditor = interface(INotifyEngine)
      ['{B3ACF9DF-2A37-4693-A7A5-FBF47DCAFC92}']
      procedure EngineAction(EngineAction: TEditorAction);
      procedure EngineMessage(S: string; vMessageInfo: TMessageInfo);
      procedure EngineReplaceText(Sender: TObject; const ASearch, AReplace: string; Line, Column: integer; var ReplaceAction: TSynReplaceAction);
    end;

implementation

{ TEditorElement }

function TEditorElement.GetName: string;
begin
    Result := FName;
end;

procedure TEditorElement.SetName(const AValue: string);
begin
  FName := AValue;
end;

function TEditorElement.GetDescription: string;
begin
  Result := FDescription;
end;

constructor TEditorElement.Create;
begin
  FImageIndex := -1;
  inherited Create;
end;

end.

