{ nyx
  Copyright (c) 2020 mr-highball

  Permission is hereby granted, free of charge, to any person obtaining a copy
  of this software and associated documentation files (the "Software"), to deal
  in the Software without restriction, including without limitation the rights
  to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
  copies of the Software, and to permit persons to whom the Software is
  furnished to do so, subject to the following conditions:

  The above copyright notice and this permission notice shall be included in all
  copies or substantial portions of the Software.

  THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
  IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
  FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
  AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
  LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
  OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
  SOFTWARE.
}

unit nyx.studio.sourcefiles;

{$mode delphi}{$H+}{$codepage utf8}

interface

uses nyx.text, nyx.types, nyx.model, nyx.behavior, nyx.pascal.files.editor,
  nyx.studio.session, nyx.studio.sourcejobs;

const
  NyxStudioSourceFilesID = 'studio-source-files';
  NyxStudioSourceFilesTabID = 'action-source-files-tab';

type
  { Field input retains the mounted control. Review/queued effects request a
    new presentation; only the source-command controller may publish a pair. }
  TNyxStudioSourceFilesEffect = (ssfeNone, ssfeField, ssfePresentation, ssfeQueued);

{ Shared ordinary-controller route. Borrows only the current source-pane tree
  during this call; stale node identities cannot reuse another form's actions.
  Validation stays visible in the public form and preserves its exact fields. }
function RouteNyxStudioSourceFiles(ASession: TNyxStudioSession;
  ACommands: TNyxSourceCommands; ARoot, ANode: TNyxNode;
  const AEvent: TNyxEventInfo; var AState: TNyxPascalFilesEditorState;
  var AReview: TNyxStudioFileRemovalReview): TNyxStudioSourceFilesEffect;

implementation

uses SysUtils, nyx.schema;

function RouteNyxStudioSourceFiles(ASession: TNyxStudioSession;
  ACommands: TNyxSourceCommands; ARoot, ANode: TNyxNode;
  const AEvent: TNyxEventInfo; var AState: TNyxPascalFilesEditorState;
  var AReview: TNyxStudioFileRemovalReview): TNyxStudioSourceFilesEffect;
var
  LAction: TNyxPascalFilesEditorAction;
  LRequest: TNyxStudioSourceRequest;
begin
  Result := ssfeNone;

  if (ARoot = nil) or (ANode = nil) or AEvent.DefaultPrevented or
    (ARoot.Find(ANode.ID) <> ANode) then
  begin
    Exit;
  end;
  try

    if (AEvent.Trigger = ntChange) and
      ((ANode.ID = NyxStudioSourceFilesID + '-name') or
      (ANode.ID = NyxStudioSourceFilesID + '-kind')) then
    begin
      AState.Capture(NyxStudioSourceFilesID, ARoot);
      Exit(ssfeField);
    end;
    LAction := NyxPascalFilesEditorAction(NyxStudioSourceFilesID, ANode, AEvent.Trigger);

    if LAction <> npfeaNone then
    begin
      { Each new action owns its feedback. An earlier field/review refusal must
        not mask the current compiler status or a later confirmation warning. }
      AState.Report('');
    end;
    case LAction of
      npfeaCreate:
        begin
          AState.Capture(NyxStudioSourceFilesID, ARoot);
          ACommands.ChangeFile(AState.Creation);
          Result := ssfeQueued;
        end;
      npfeaRemove:
        begin
          AReview := ASession.ReviewFileRemoval(ASession.ActiveSourceFile);
          Result := ssfePresentation;
        end;
      npfeaConfirmRemove:
        begin
          LRequest := ASession.PrepareReviewedFileRemoval(AReview, NyxSchemaRevision);
          ACommands.ChangeFile(LRequest);
          AReview := Default(TNyxStudioFileRemovalReview);
          Result := ssfeQueued;
        end;
      npfeaCancelRemove:
        begin
          AReview := Default(TNyxStudioFileRemovalReview);
          Result := ssfePresentation;
        end;
      npfeaNone:
        begin
          { Other shell/source actions keep their original owner. }
        end;
    end;
  except
    on LException: Exception do
    begin
      { pas2js messages already use Unicode text. Native RTL exception strings
        cross explicitly into the portable UTF-8 text contract. }
      {$ifdef PAS2JS}
      AState.Report(LException.Message);
      {$else}
      AState.Report(UTF8Encode(UnicodeString(LException.Message)));
      {$endif}
      Result := ssfePresentation;
    end;
  end;
end;

end.
