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
{ Shared target attachment. Only adoption feedback and its button state are
  synchronized; the actual event-route fields, caret, canvas and connector faces
  retain their existing owners while the source compiler works asynchronously. }

procedure TToolView.AdoptionChanged;
var
  LLease: INyxToolCanvasView;
  LBlueprint: INyxCard;
  LSource: TNyxNode;
  LTarget: TNyxNode;
begin

  if not FConnected or FUpdating then
  begin
    Exit;
  end;
  LLease := Self;
  LBlueprint := FInspector.Root;

  if FRenderer.Root <> nil then
  begin
    LSource := LBlueprint.Node.Find(LBlueprint.ID + '-save-connections-feedback');

    if LSource <> nil then
    begin
      LTarget := FRenderer.Root.Find(LSource.ID);

      if LTarget <> nil then
      begin
        LTarget.Configure.Text(LSource.Prop(NyxAttributeName(atText))).Done;
      end;
    end;
    LSource := LBlueprint.Node.Find(LBlueprint.ID + '-save-connections');

    if LSource <> nil then
    begin
      LTarget := FRenderer.Root.Find(LSource.ID);

      if LTarget <> nil then
      begin
        LTarget.Configure.Enabled(LSource.Prop(NyxAttributeName(atEnabled)) <> 'false').Done;
      end;
    end;
  end;

  if Ord(FQueuedUpdate) < Ord(ntcuSynchronize) then
  begin
    FQueuedUpdate := ntcuSynchronize;
  end;
  QueueRefresh;
end;
