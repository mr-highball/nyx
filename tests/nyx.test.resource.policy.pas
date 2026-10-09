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

unit nyx.test.resource.policy;

{$mode delphi}{$H+}{$codepage utf8}

interface

uses nyx.text, nyx.resource.sources, nyx.resources.loader,
  nyx.test.resource.stream;

type
  { The same immutable plan is consumed by the real application and its HTTP
    producer driver. Expected describes the second load after a healthy seed
    and an unavailable host; it never substitutes a response or a clock. }
  TNyxTestPolicyCase = (npcFresh, npcBypass, npcNoStore, npcOverrideNoStore,
    npcValidate, npcRevalidate, npcStale, npcExpired, npcOverrideExpired,
    npcCallerTTL, npcConflict);
  TNyxTestPolicyPlan = record
    Title: TNyxText;
    Policy: TNyxResourceCachePolicy;
    Reply: TNyxTestResourceReply;
    Expected: TNyxResourceLoadOrigin;
    Writes: Boolean;
  end;

{ Independent typed choices; fresh/stale timing uses real target clocks and
  fixed Age headers. Override deliberately ignores the server's constraints. }
function NyxTestPolicyPlan(ACase: TNyxTestPolicyCase): TNyxTestPolicyPlan;

implementation

function NyxTestPolicyPlan(ACase: TNyxTestPolicyCase): TNyxTestPolicyPlan;
begin
  Result := Default(TNyxTestPolicyPlan);
  Result.Policy := NyxResourceCache.Memory.FreshFor(600).StaleFor(30);
  Result.Reply := ntrFresh;
  Result.Expected := rloFallback;
  Result.Writes := True;
  case ACase of
    npcFresh:
      begin
        Result.Title := 'Fresh reuse';
        Result.Expected := rloFreshCache;
      end;
    npcBypass:
      begin
        Result.Title := 'Explicit bypass';
        Result.Policy := Result.Policy.Bypass;
        Result.Writes := False;
      end;
    npcNoStore:
      begin
        Result.Title := 'Respect no-store';
        Result.Reply := ntrNoStore;
        Result.Writes := False;
      end;
    npcOverrideNoStore:
      begin
        Result.Title := 'Override no-store';
        Result.Reply := ntrNoStore;
        Result.Policy := Result.Policy.ServerPolicy(rcspOverride);
        Result.Expected := rloFreshCache;
      end;
    npcValidate:
      begin
        Result.Title := 'Respect no-cache';
        Result.Reply := ntrValidate;
      end;
    npcRevalidate:
      begin
        Result.Title := 'Respect must-revalidate';
        Result.Reply := ntrRevalidate;
      end;
    npcStale:
      begin
        Result.Title := 'Within stale allowance';
        Result.Reply := ntrStale;
        Result.Expected := rloStaleCache;
      end;
    npcExpired:
      begin
        Result.Title := 'Exhausted stale allowance';
        Result.Reply := ntrExpired;
      end;
    npcOverrideExpired:
      begin
        Result.Title := 'Override response age';
        Result.Reply := ntrExpired;
        Result.Policy := Result.Policy.ServerPolicy(rcspOverride);
        Result.Expected := rloFreshCache;
      end;
    npcCallerTTL:
      begin
        Result.Title := 'Caller requires refresh';
        Result.Policy := Result.Policy.FreshFor(0).StaleFor(0);
      end;
    npcConflict:
      begin
        Result.Title := 'Conflicting freshness';
        Result.Reply := ntrConflict;
      end;
  end;
end;

end.
