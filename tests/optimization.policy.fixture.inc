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
{ Whole original FPC selectors, each independently enabled and disabled. The
  compiler owns availability/effect; these are not a synthesized optimizer preset. }
{$optimization LEVEL1}{$optimization NOLEVEL1}
{$optimization LEVEL2}{$optimization NOLEVEL2}
{$optimization LEVEL3}{$optimization NOLEVEL3}
{$optimization LEVEL4}{$optimization NOLEVEL4}
{$optimization REGVAR}{$optimization NOREGVAR}
{$optimization UNCERTAIN}{$optimization NOUNCERTAIN}
{$optimization SIZE}{$optimization NOSIZE}
{$optimization STACKFRAME}{$optimization NOSTACKFRAME}
{$optimization PEEPHOLE}{$optimization NOPEEPHOLE}
{$optimization LOOPUNROLL}{$optimization NOLOOPUNROLL}
{$optimization TAILREC}{$optimization NOTAILREC}
{$optimization CSE}{$optimization NOCSE}
{$optimization DFA}{$optimization NODFA}
{$optimization STRENGTH}{$optimization NOSTRENGTH}
{$optimization SCHEDULE}{$optimization NOSCHEDULE}
{$optimization AUTOINLINE}{$optimization NOAUTOINLINE}
{$optimization USEEBP}{$optimization NOUSEEBP}
{$optimization USERBP}{$optimization NOUSERBP}
{$optimization ORDERFIELDS}{$optimization NOORDERFIELDS}
{$optimization FASTMATH}{$optimization NOFASTMATH}
{$optimization DEADVALUES}{$optimization NODEADVALUES}
{$optimization REMOVEEMPTYPROCS}{$optimization NOREMOVEEMPTYPROCS}
{$optimization CONSTPROP}{$optimization NOCONSTPROP}
{$optimization DEADSTORE}{$optimization NODEADSTORE}
{$optimization FORCENOSTACKFRAME}{$optimization NOFORCENOSTACKFRAME}
{$optimization USELOADMODIFYSTORE}{$optimization NOUSELOADMODIFYSTORE}
{$optimization UNUSEDPARA}{$optimization NOUNUSEDPARA}
{$optimization CONSTS}{$optimization NOCONSTS}
{$optimization FORLOOP}{$optimization NOFORLOOP}
