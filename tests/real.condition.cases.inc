{ Original directive expressions, compiled by FPC and then read unchanged by
  Nyx's ordered conditional owner. Both branches keep distinct Boolean tokens. }
{$if 0.1+0.2=0.3}Emit('fraction',True);{$else}Emit('fraction',False);{$endif}
{$if 16777217=16777216.0}Emit('no-minimum-round',True);{$else}Emit('no-minimum-round',False);{$endif}
{$if (16777216.0+1.0)-16777216.0=1.0}Emit('host-intermediate',True);{$else}Emit('host-intermediate',False);{$endif}
{$if SingleValue=0.1}Emit('stored-single',True);{$else}Emit('stored-single',False);{$endif}
{$if SumOfSingles=0.2}Emit('stored-sum',True);{$else}Emit('stored-sum',False);{$endif}
{$if SingleAlias=SingleValue}Emit('stored-alias',True);{$else}Emit('stored-alias',False);{$endif}
{$if PInf>0.0}Emit('positive-reference',True);{$else}Emit('positive-reference',False);{$endif}
{$if NInf<0.0}Emit('negative-reference',True);{$else}Emit('negative-reference',False);{$endif}
{$if QNaN<>QNaN}Emit('nan-not-equal',True);{$else}Emit('nan-not-equal',False);{$endif}
{$if QNaN=QNaN}Emit('nan-equal',True);{$else}Emit('nan-equal',False);{$endif}
{$if QNaN<0.0}Emit('nan-less',True);{$else}Emit('nan-less',False);{$endif}
{$if QNaN<=0.0}Emit('nan-less-equal',True);{$else}Emit('nan-less-equal',False);{$endif}
{$if QNaN>0.0}Emit('nan-greater',True);{$else}Emit('nan-greater',False);{$endif}
{$if QNaN>=0.0}Emit('nan-greater-equal',True);{$else}Emit('nan-greater-equal',False);{$endif}
{$if 1.0/0.0>0.0}Emit('real-zero-division',True);{$else}Emit('real-zero-division',False);{$endif}
{$if 0.0/0.0<>0.0/0.0}Emit('invalid-relation',True);{$else}Emit('invalid-relation',False);{$endif}
{$if 1.0/2.0=0.5}Emit('half',True);{$else}Emit('half',False);{$endif}
{$if 18446744073709551615/3=6148914691236517205.0}Emit('unsigned-divide',True);{$else}Emit('unsigned-divide',False);{$endif}
{$if 1/0>0}Emit('integer-zero-division',True);{$else}Emit('integer-zero-division',False);{$endif}
{$if True+1.5=2.5}Emit('boolean-real',True);{$else}Emit('boolean-real',False);{$endif}
{$if True or Missing}Emit('short-true',True);{$else}Emit('short-true',False);{$endif}
{$if False and Missing}Emit('short-false',True);{$else}Emit('short-false',False);{$endif}
{$if -1.5<0.0}Emit('negative',True);{$else}Emit('negative',False);{$endif}
{$if 1.0e-9999=0.0}Emit('underflow',True);{$else}Emit('underflow',False);{$endif}
{$if 1.0e9999>0.0}Emit('parser-overflow',True);{$else}Emit('parser-overflow',False);{$endif}
{$if 18446744073709551616>0.0}Emit('decimal-beyond64',True);{$else}Emit('decimal-beyond64',False);{$endif}
{$if PositiveFlag}Emit('source-flag',True);{$else}Emit('source-flag',False);{$endif}
{$if SingleValue=0.5}Emit('global-shadow',True);{$else}Emit('global-shadow',False);{$endif}
