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
unit nyx.flow.route.model;

{$mode delphi}{$H+}{$codepage utf8}

interface

uses SysUtils, nyx.text, nyx.data, nyx.model, nyx.flow.routes;

{ Independent typed facade. Missing local metadata inherits, while an explicitly
  empty set masks a template. Returned values own no node or document. }
function NyxControlFlowRoutes(ANode: TNyxNode): INyxFlowRoutes;
procedure SetNyxFlowRoutes(const AControl: INyxNode; const ARoutes: INyxFlowRoutes); overload;
procedure SetNyxFlowRoutes(ANode: TNyxNode; const ARoutes: INyxFlowRoutes); overload;
procedure InheritNyxFlowRoutes(const AControl: INyxNode); overload;
procedure InheritNyxFlowRoutes(ANode: TNyxNode); overload;

implementation

uses nyx.controls;

function NyxControlFlowRoutes(ANode: TNyxNode): INyxFlowRoutes;
begin

  if ANode = nil then
  begin
    raise ENyxModel.Create('Flow routes require an exact control');
  end;
  Result := NewNyxFlowRoutes;

  if ANode.Extensions.Has(NyxExtension(NyxFlowRoutesKey)) then
  begin
    Result := NyxFlowRoutesFromData(ANode.Extensions.Value(NyxExtension(NyxFlowRoutesKey)));
  end;
end;

procedure SetNyxFlowRoutes(const AControl: INyxNode; const ARoutes: INyxFlowRoutes);
var
  LControl: INyxNode;
  LRoutes: INyxFlowRoutes;
  LData: TNyxDataValue;
begin
  LControl := AControl;
  LRoutes := ARoutes;

  if (LControl = nil) or (LRoutes = nil) then
  begin
    raise ENyxModel.Create('Flow route configuration requires a retained control and typed set');
  end;
  { Creator getters may retire caller variables; acquire both interface leases
    and admit independent data before resolving the retained control again. }
  LData := NyxFlowRoutesFromData(LRoutes.ToData).ToData;
  LControl.Node.Extensions.SetValue(NyxExtension(NyxFlowRoutesKey), LData);
end;

procedure SetNyxFlowRoutes(ANode: TNyxNode; const ARoutes: INyxFlowRoutes);
begin

  if ANode = nil then
  begin
    raise ENyxModel.Create('Flow routes require a control');
  end;
  SetNyxFlowRoutes(RetainNyxControl(ANode), ARoutes);
end;

procedure InheritNyxFlowRoutes(const AControl: INyxNode);
var
  LControl: INyxNode;
begin
  LControl := AControl;

  if LControl = nil then
  begin
    raise ENyxModel.Create('Flow routes require a retained control');
  end;
  LControl.Node.Extensions.Remove(NyxExtension(NyxFlowRoutesKey));
end;

procedure InheritNyxFlowRoutes(ANode: TNyxNode);
begin

  if ANode = nil then
  begin
    raise ENyxModel.Create('Flow routes require a control');
  end;
  InheritNyxFlowRoutes(RetainNyxControl(ANode));
end;

end.
