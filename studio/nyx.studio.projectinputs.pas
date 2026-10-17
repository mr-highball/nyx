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
unit nyx.studio.projectinputs;

{$mode delphi}{$H+}{$codepage utf8}

interface

uses nyx.pascal.project, nyx.studio.sourceprojection;

type
  { Detached projection attachment for exact supporting-file admission. This is
    local producer data, never a wire execution flag. The trusted compiler host
    binds it to its result; editor admission compares the entire inventory as
    well as the ordinary exact source/design/state checks. }
  INyxPascalProjectProjection = interface(INyxSourceProjection)
    ['{68FE918B-457C-45BE-B2E0-CA2300000005}']
    function GetFiles: TNyxPascalProjectFiles;
    property Files: TNyxPascalProjectFiles read GetFiles;
  end;

  { In-process compiler receipt retaining the exact frozen main/unit/include
    snapshot. The ordinary projection still distinguishes compilation/execution.
    This facet has no wire representation and grants no editor publication,
    import/recovery or application-launch authority. Retaining it borrows no
    executor, mutable provider, project, process or native filesystem handle. }
  INyxPascalProjectBuild = interface(INyxSourceProjectionBuild)
    ['{68FE918B-457C-45BE-B2E0-CA2300000003}']
    function GetInputs: INyxPascalProject;
    property Inputs: INyxPascalProject read GetInputs;
  end;

implementation

end.
